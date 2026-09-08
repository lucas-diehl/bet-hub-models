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

# nflverse SCHEDULE/RESULTS (separate release from player stats -- has real final scores
# + the historical closing Vegas spread_line, both absent from stats_player). Free, no
# key. Used for game-script correlation calibration (tests/validate_nfl_game_script_correlation.R)
# and, live, as the pregame exp_margin source (real spread when available).
NFL_SCHED_URL <- function() Sys.getenv("NFL_SCHEDULES_URL",
  "https://github.com/nflverse/nflverse-data/releases/download/schedules/games.csv")
nfl_games_path <- function() dfs_path("data", "raw", "nfl_games.rds")

nfl_game_scores <- function(seasons = NULL, refresh = FALSE) {
  if (!refresh && file.exists(nfl_games_path())) {
    G <- as.data.table(readRDS(nfl_games_path()))
  } else {
    resp <- tryCatch(httr2::request(NFL_SCHED_URL()) |> httr2::req_user_agent("DFS-ENGINE/1.0") |>
                       httr2::req_timeout(60) |> httr2::req_retry(max_tries = 3) |> httr2::req_perform(),
                     error = function(e) NULL)
    if (is.null(resp) || httr2::resp_status(resp) != 200) {
      if (file.exists(nfl_games_path())) G <- as.data.table(readRDS(nfl_games_path()))
      else stop("nfl_game_scores: schedule fetch failed and no cache present")
    } else {
      G <- fread(text = httr2::resp_body_string(resp), showProgress = FALSE)
      dir.create(dirname(nfl_games_path()), recursive = TRUE, showWarnings = FALSE)
      saveRDS(G, nfl_games_path())
    }
  }
  if (!is.null(seasons)) G <- G[season %in% seasons]
  G[game_type == "REG" & is.finite(home_score) & is.finite(away_score)]
}

# nflverse DEPTH CHARTS (free, no key, updated ~daily from team-reported charts) — the
# only reliable "will this QB actually play" signal for a HEALTHY backup. A benched
# 3rd-stringer (e.g. a real case: Carson Wentz, MIN, pos_rank 3) is not "injured" so the
# ESPN-injury-based apply_inactives() safeguard can't catch him — he'd otherwise get a
# small nonzero salary-baseline projection (his DK price is very low, and the baseline
# formula scales proj ~linearly with salary) that a min-salary-hungry optimizer will
# happily plug in as "cheap value" despite his real expected output being ~0.
# QB only (per user request) — it's a clean 1-starter-per-team position, unlike
# RB/WR/TE where committee backfields make "backup" fuzzy and often still relevant.
NFL_DEPTH_URL <- function() Sys.getenv("NFL_DEPTH_CHARTS_URL",
  sprintf("https://github.com/nflverse/nflverse-data/releases/download/depth_charts/depth_charts_%d.csv",
          as.integer(format(Sys.Date(), "%Y"))))
nfl_depth_path <- function() dfs_path("data", "raw", "nfl_depth_charts.rds")

# normalized set of CURRENT starting-QB names (one per team, latest snapshot only).
# Cached ~12h (the file is ~50MB; "who's starting" doesn't change within a day). Falls
# back to a stale cache on fetch failure, and to NULL (caller skips the filter) if
# there's no cache at all — never blocks the pipeline on a missing/failed external pull.
nfl_qb_starters <- function(max_age_hours = 12) {
  p <- nfl_depth_path()
  fresh <- file.exists(p) && difftime(Sys.time(), file.info(p)$mtime, units = "hours") < max_age_hours
  if (!fresh) {
    resp <- tryCatch(httr2::request(NFL_DEPTH_URL()) |> httr2::req_user_agent("DFS-ENGINE/1.0") |>
                       httr2::req_timeout(90) |> httr2::req_perform(), error = function(e) NULL)
    if (!is.null(resp) && httr2::resp_status(resp) == 200) {
      DC <- tryCatch(fread(text = httr2::resp_body_string(resp), showProgress = FALSE), error = function(e) NULL)
      if (!is.null(DC) && nrow(DC)) { dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); saveRDS(DC, p) }
    }
  }
  if (!file.exists(p)) return(NULL)
  DC <- tryCatch(as.data.table(readRDS(p)), error = function(e) NULL)
  if (is.null(DC) || !nrow(DC) || !all(c("pos_abb", "pos_rank", "team", "player_name", "dt") %in% names(DC))) return(NULL)
  Q <- DC[pos_abb == "QB"]; Q <- Q[dt == max(dt)]                # latest snapshot only
  starters <- Q[pos_rank == 1, unique(norm_name(player_name))]
  if (!length(starters)) return(NULL)
  starters
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
