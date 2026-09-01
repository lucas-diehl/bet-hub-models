# ==============================================================================
# NFL plugin — ingestion (nflverse weekly player stats -> training store)
# nflverse-data GitHub releases are free, no key, historical (2016+ weekly; pbp to
# 1999). We pull weekly player_stats and keep the OPPORTUNITY signals that actually
# predict — target_share, air_yards_share, wopr, carries, targets — because volume is
# stickier and less priced-in by the field than last week's fantasy points. DK points
# are recomputed from components (nfl_dk_scoring) so scoring matches DraftKings exactly.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# nflverse migrated weekly stats to the `stats_player` release (has 2016-current; the
# old `player_stats` release stopped at 2024). Schema matches except interceptions ->
# passing_interceptions (normalized in nfl_ingest).
NFL_SRC <- function() Sys.getenv("NFL_NFLVERSE_BASE",
  "https://github.com/nflverse/nflverse-data/releases/download/stats_player")
nfl_weekly_path <- function() dfs_path("data", "raw", "nfl_weekly.rds")
nfl_src_dir     <- function() dfs_path("data", "raw", "nfl_src")
nfl_src_csv     <- function(year) file.path(nfl_src_dir(), sprintf("stats_player_week_%d.csv", year))

.nfl_fetch_year <- function(year, prefer_cache = TRUE) {
  local <- nfl_src_csv(year)
  if (prefer_cache && file.exists(local)) return(fread(local, showProgress = FALSE))
  url <- sprintf("%s/stats_player_week_%d.csv", NFL_SRC(), year)
  resp <- tryCatch(httr2::request(url) |> httr2::req_user_agent("DFS-ENGINE/1.0") |>
                     httr2::req_timeout(120) |> httr2::req_retry(max_tries = 3) |>
                     httr2::req_perform(), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) != 200) {
    if (file.exists(local)) return(fread(local, showProgress = FALSE))
    msg("  skip NFL", year, "(unreachable)"); return(NULL)
  }
  txt <- httr2::resp_body_string(resp)
  dir.create(nfl_src_dir(), recursive = TRUE, showWarnings = FALSE)
  writeLines(txt, local); fread(text = txt, showProgress = FALSE)
}

# DK points from nflverse components (reuses the tested nfl_dk_scoring contract).
.nfl_dk_points <- function(D) {
  z <- function(col) { v <- if (col %in% names(D)) as.numeric(D[[col]]) else 0; fifelse(is.na(v), 0, v) }
  box <- data.table(
    pass_yds = z("passing_yards"), pass_td = z("passing_tds"), interceptions = z("interceptions"),
    rush_yds = z("rushing_yards"), rush_td = z("rushing_tds"),
    receptions = z("receptions"), rec_yds = z("receiving_yards"), rec_td = z("receiving_tds"),
    fumbles_lost = z("sack_fumbles_lost") + z("rushing_fumbles_lost") + z("receiving_fumbles_lost"),
    two_pt = z("passing_2pt_conversions") + z("rushing_2pt_conversions") + z("receiving_2pt_conversions"))
  nfl_dk_scoring(box)
}

# Download + store weekly skill-position player games (QB/RB/WR/TE) with DK points +
# usage signals. years default last 6; current season re-fetched, past cached.
nfl_ingest <- function(years = NULL) {
  yr <- as.integer(format(Sys.Date(), "%Y"))
  if (is.null(years)) years <- (yr - 5):yr
  got <- rbindlist(lapply(years, function(y) {
    d <- .nfl_fetch_year(y, prefer_cache = (y < yr)); if (is.null(d) || !nrow(d)) return(NULL)
    as.data.table(d)[, season := y]
  }), fill = TRUE)
  if (is.null(got) || !nrow(got)) { msg("No NFL data (network blocked? drop CSVs in ", nfl_src_dir(), ")"); return(invisible(NULL)) }
  got <- got[position %in% c("QB", "RB", "WR", "TE") & !is.na(week)]
  keep_reg <- if ("season_type" %in% names(got)) got$season_type %in% c("REG", "POST") else TRUE
  got <- got[keep_reg]
  if (!"interceptions" %in% names(got) && "passing_interceptions" %in% names(got))
    got[, interceptions := passing_interceptions]                # nflverse release rename
  got[, dk_pts := .nfl_dk_points(got)]
  dir.create(dirname(nfl_weekly_path()), recursive = TRUE, showWarnings = FALSE)
  saveRDS(got, nfl_weekly_path())
  msg(sprintf("Stored %d NFL player-games (%d seasons) -> %s", nrow(got), uniqueN(got$season), nfl_weekly_path()))
  invisible(got)
}
