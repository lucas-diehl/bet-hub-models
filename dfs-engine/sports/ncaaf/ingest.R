# ==============================================================================
# NCAAF (CFB) plugin — ingestion (CollegeFootballData.com free API -> training store)
# CFBD's /games/players endpoint gives per-game player box scores (passing/rushing/
# receiving/fumbles), nested by team -> category -> stat-type -> athlete. We flatten
# to one row per player-game and compute DK points from components so scoring matches
# DraftKings exactly (DK CFB Classic uses the SAME scoring table as DK NFL — no DST).
# Free tier key required: CFB_API_KEY (config/.Renviron or env).
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# DK CFB Classic scoring == DK NFL scoring exactly (no DST slot in CFB). Kept here (not
# just roster.R) so ingest can compute dk_pts standalone. box: named stat columns.
cfb_dk_scoring <- function(box) {
  g <- function(col) if (col %in% names(box)) as.numeric(box[[col]]) else 0
  0.04 * g("pass_yds") + 4 * g("pass_td") - 1 * g("interceptions") +
  0.1  * g("rush_yds") + 6 * g("rush_td") +
  1    * g("receptions") + 0.1 * g("rec_yds") + 6 * g("rec_td") +
  2 * g("two_pt") - 1 * g("fumbles_lost") + 6 * g("fumble_td") +
  3 * (g("pass_yds") >= 300) + 3 * (g("rush_yds") >= 100) + 3 * (g("rec_yds") >= 100)
}

CFBD_BASE <- function() Sys.getenv("CFBD_BASE", "https://api.collegefootballdata.com")
cfb_weekly_path <- function() dfs_path("data", "raw", "ncaaf_weekly.rds")
cfb_src_dir     <- function() dfs_path("data", "raw", "ncaaf_src")
cfb_src_json    <- function(year, week) file.path(cfb_src_dir(), sprintf("games_players_%d_wk%d.json", year, week))

.cfb_key <- function() {
  k <- Sys.getenv("CFB_API_KEY", "")
  if (!nzchar(k)) stop("CFB_API_KEY not set (config/.Renviron or env) — required for CollegeFootballData.com")
  k
}

# fetch one week of game-player box scores (FBS regular season), cached to disk.
.cfb_fetch_week <- function(year, week, prefer_cache = TRUE) {
  local <- cfb_src_json(year, week)
  if (prefer_cache && file.exists(local)) return(jsonlite::fromJSON(local, simplifyVector = FALSE))
  dir.create(cfb_src_dir(), recursive = TRUE, showWarnings = FALSE)
  url <- sprintf("%s/games/players?year=%d&week=%d&seasonType=regular&classification=fbs", CFBD_BASE(), year, week)
  resp <- tryCatch(httr2::request(url) |>
    httr2::req_headers(Authorization = paste("Bearer", .cfb_key())) |>
    httr2::req_user_agent("DFS-ENGINE/1.0") |> httr2::req_timeout(60) |> httr2::req_perform(),
    error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) != 200) return(NULL)
  txt <- httr2::resp_body_string(resp); writeLines(txt, local)
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

# CFBD stat-type -> our canonical column, keyed on "category:type" (type names COLLIDE
# across categories — YDS and TD each appear in passing/rushing/receiving — so category
# must disambiguate; a flat type-only lookup silently collapses all three into one).
.CFB_COLMAP <- c(
  "passing:YDS" = "pass_yds", "passing:TD" = "pass_td", "passing:INT" = "interceptions",
  "rushing:YDS" = "rush_yds", "rushing:TD" = "rush_td", "rushing:CAR" = "carries",
  "receiving:YDS" = "rec_yds", "receiving:TD" = "rec_td", "receiving:REC" = "receptions",
  "fumbles:LOST" = "fumbles_lost")

# flatten one week's nested games -> long (game_id, team, athlete_id, player, category, stat_name, value)
.cfb_flatten_week <- function(gj, year, week) {
  if (is.null(gj) || !length(gj)) return(NULL)
  rows <- rbindlist(lapply(gj, function(g) {
    rbindlist(lapply(g$teams %||% list(), function(tm) {
      rbindlist(lapply(tm$categories %||% list(), function(cat) {
        rbindlist(lapply(cat$types %||% list(), function(ty) {
          rbindlist(lapply(ty$athletes %||% list(), function(ath) {
            data.table(game_id = g$id %||% NA_integer_, team = tm$team %||% NA_character_,
                       athlete_id = ath$id %||% NA_character_, player = ath$name %||% NA_character_,
                       category = cat$name %||% NA_character_, stat_name = ty$name %||% NA_character_,
                       value = suppressWarnings(as.numeric(ath$stat %||% NA)))
          }), fill = TRUE)
        }), fill = TRUE)
      }), fill = TRUE)
    }), fill = TRUE)
  }), fill = TRUE)
  if (is.null(rows) || !nrow(rows)) return(NULL)
  rows[, `:=`(season = year, wk = week)]
  rows[!is.na(athlete_id) & !is.na(value)]
}

# pivot long -> one row per player-game with canonical NFL-style component columns,
# so nfl_dk_scoring-equivalent (cfb_dk_scoring, sports/ncaaf/roster.R) applies unchanged.
.cfb_pivot_game <- function(L) {
  if (is.null(L) || !nrow(L)) return(NULL)
  L <- copy(L)[, key := paste(category, stat_name, sep = ":")]
  L <- L[key %in% names(.CFB_COLMAP)]
  if (!nrow(L)) return(NULL)
  L[, col := .CFB_COLMAP[key]]
  L <- L[, .(value = sum(value, na.rm = TRUE)), by = .(season, wk, game_id, team, athlete_id, player, col)]
  W <- dcast(L, season + wk + game_id + team + athlete_id + player ~ col, value.var = "value", fill = 0)
  for (c1 in unique(.CFB_COLMAP)) if (!c1 %in% names(W)) W[, (c1) := 0]
  # TRAINING-ONLY position heuristic (no roster/position endpoint call, to save API+tokens):
  # dominant stat category this game. At SERVE time project.R uses DK's own live position
  # field instead (authoritative) — this only segments the regression during training.
  W[, pos := fifelse(pass_yds > 0, "QB", fifelse(rush_yds >= rec_yds, "RB", "WR"))]
  W[]
}

# Pull + cache multiple seasons of weekly player-game stats -> ncaaf_weekly.rds.
# weeks: FBS regular season is weeks 1-15 (championship week varies; safe default 1:15).
cfb_ingest <- function(years = (as.integer(format(Sys.Date(), "%Y")) - 2):as.integer(format(Sys.Date(), "%Y")),
                       weeks = 1:15, refresh = FALSE) {
  cached <- if (!refresh && file.exists(cfb_weekly_path())) as.data.table(readRDS(cfb_weekly_path())) else NULL
  have <- if (!is.null(cached)) unique(cached[, .(season, wk)]) else data.table(season = integer(0), wk = integer(0))
  out <- list(); n_new <- 0L
  for (y in years) for (w in weeks) {
    if (!refresh && nrow(have[season == y & wk == w])) next
    gj <- tryCatch(.cfb_fetch_week(y, w), error = function(e) NULL)
    piv <- tryCatch(.cfb_pivot_game(.cfb_flatten_week(gj, y, w)), error = function(e) NULL)
    if (!is.null(piv) && nrow(piv)) { out[[length(out) + 1L]] <- piv; n_new <- n_new + nrow(piv) }
  }
  D <- rbindlist(c(list(cached), out), fill = TRUE)
  if (!nrow(D)) stop("cfb_ingest: no data pulled — check CFB_API_KEY / connectivity")
  D <- unique(D, by = c("season", "wk", "game_id", "athlete_id"))
  D[, dk_pts := cfb_dk_scoring(.SD)]
  dir.create(dirname(cfb_weekly_path()), recursive = TRUE, showWarnings = FALSE)
  saveRDS(D, cfb_weekly_path())
  msg(sprintf("  ncaaf ingest: %d player-games cached (%d new) -> %s", nrow(D), n_new, cfb_weekly_path()))
  invisible(D)
}
