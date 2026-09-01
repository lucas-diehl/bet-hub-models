# ============================================================================
# weekly_feed.R  —  hands-off weekly driver for the Bet Hub feed (nfl-modeling)
# ----------------------------------------------------------------------------
# The NFL feed WRITER already exists and is complete: scripts/42_build_dashboard_feed.R
# (CLI: --slate=YYYY-MM-DD [--results] [--mode=PAPER|LIVE]). It emits ONE game day
# at a time and requires an explicit --slate. This driver is the missing automation:
# it reads the season schedule, decides which game days to POST (upcoming) and which
# to GRADE (already-final), and calls the writer for each — the NFL analogue of
# cfb-modeling/weekly_update.R.
#
# Flow, per run (scheduled Tue + Mon; a mid-week run is a cheap no-op off-season):
#   1. IDLE GATE  — if no NFL game within +/-10 days, exit fast (off-season no-op).
#   2. POST       — for each upcoming game day (today..+9d, not yet all-final):
#                     Rscript scripts/42_build_dashboard_feed.R --slate=<day>
#   3. GRADE      — for each recent game day (last 16d) whose games are ALL final:
#                     Rscript scripts/42_build_dashboard_feed.R --slate=<day> --results
#
# Env: NFL_SEASON (force season), FEED_DIR (passed through to the writer),
#      GRADE_LOOKBACK_DAYS (16), FORCE_ACTIVE (ignore the idle gate, for testing),
#      NFL_FEED_MODE (PAPER).  Never throws — a failed slate is logged and skipped.
# ============================================================================

PROJ <- "C:/Users/ljdie/OneDrive/Documents/NFL"
if (dir.exists(PROJ)) setwd(PROJ)
suppressWarnings(suppressMessages(library(dplyr)))

log <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", ..., "\n")
RSCRIPT     <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
DRIVER      <- "scripts/42_build_dashboard_feed.R"
MODE        <- Sys.getenv("NFL_FEED_MODE", "PAPER")
LOOKBACK    <- as.integer(Sys.getenv("GRADE_LOOKBACK_DAYS", "16"))
STEP_TIMEOUT<- 1500L    # seconds per writer call (kills a runaway; well under a 2h task)

season <- suppressWarnings(as.integer(Sys.getenv("NFL_SEASON", "")))
if (is.na(season)) { td <- Sys.Date(); yr <- as.integer(format(td, "%Y")); mo <- as.integer(format(td, "%m"))
  season <- if (mo >= 3) yr else yr - 1 }   # NFL league year rolls in March
log("=== weekly_feed start | season", season, "| mode", MODE, "===")

# ---- load the season schedule ----------------------------------------------
# 42 reads data/raw/schedules_2026.rds; use the same source so days/finals agree.
sched_path <- c(sprintf("data/raw/schedules_%d.rds", season), "data/raw/schedules.rds")
sched_path <- sched_path[file.exists(sched_path)]
if (!length(sched_path)) { log("no schedule rds found — nothing to do."); quit(save = "no", status = 0) }

sched <- purrr::map_dfr(sched_path, readRDS) |>
  dplyr::filter(.data$game_type == "REG", .data$season == !!season) |>
  dplyr::mutate(
    gameday = as.Date(.data$gameday),
    final   = !is.na(suppressWarnings(as.numeric(.data$home_score))) &
              !is.na(suppressWarnings(as.numeric(.data$away_score)))
  ) |>
  dplyr::filter(!is.na(.data$gameday), !is.na(.data$week))
if (!nrow(sched)) { log("schedule has no", season, "REG games — nothing to do."); quit(save = "no", status = 0) }

today <- Sys.Date()

# ---- 1. idle gate ----------------------------------------------------------
if (!any(abs(as.integer(sched$gameday - today)) <= 10) && !nzchar(Sys.getenv("FORCE_ACTIVE"))) {
  log("idle (no game within +/-10 days of", format(today), ") — fast exit."); quit(save = "no", status = 0)
}

# ---- capture fresh game-line snapshots (spreads/totals) --------------------
# Spends Odds API credits: the bulk historical /odds call is 20 credits per
# snapshot (10 x 2 markets x 1 region), ~4 snapshots/week = ~80/week. 35 is
# idempotent (skips already-cached snapshots) and only fetches snapshots whose
# anchored time has PASSED — future weeks error out without a charge, so this is
# a no-op until the season is live. --quota-floor stops before the reserve is hit.
# Disable with SKIP_ODDS_CAPTURE=1; raise/lower the reserve with ODDS_QUOTA_FLOOR.
if (!nzchar(Sys.getenv("SKIP_ODDS_CAPTURE"))) {
  floor <- Sys.getenv("ODDS_QUOTA_FLOOR", "300")
  log("odds capture: 35_download_line_movement.R --season", season, "--quota-floor", floor)
  cap <- tryCatch(
    system2(RSCRIPT, shQuote(c("scripts/35_download_line_movement.R",
      paste0("--season=", season), "--execute", paste0("--quota-floor=", floor))),
      stdout = TRUE, stderr = TRUE, timeout = STEP_TIMEOUT),
    error = function(e) paste("ERROR", conditionMessage(e)))
  st <- attr(cap, "status"); if (is.null(st)) st <- 0
  log("  capture exit", st, "-", paste(tail(cap[nzchar(cap)], 2), collapse = " ; "))
}

# ---- one game day per row: week, counts, all-final --------------------------
days <- sched |>
  dplyr::group_by(.data$gameday) |>
  dplyr::summarise(week = min(.data$week), n = dplyr::n(), all_final = all(.data$final), .groups = "drop") |>
  dplyr::arrange(.data$gameday)

post_days  <- days |> dplyr::filter(.data$gameday >= today, .data$gameday <= today + 9, !.data$all_final) |> dplyr::pull(.data$gameday)
grade_days <- days |> dplyr::filter(.data$all_final, .data$gameday >= today - LOOKBACK, .data$gameday < today) |> dplyr::pull(.data$gameday)

log("post days:",  if (length(post_days))  paste(format(post_days),  collapse = ", ") else "none")
log("grade days:", if (length(grade_days)) paste(format(grade_days), collapse = ", ") else "none")

run_writer <- function(day, grade) {
  a <- c(DRIVER, paste0("--slate=", format(day)), paste0("--mode=", MODE))
  if (grade) a <- c(a, "--results")
  out <- tryCatch(
    system2(RSCRIPT, shQuote(a), stdout = TRUE, stderr = TRUE, timeout = STEP_TIMEOUT),
    error = function(e) { log("  ", format(day), if (grade) "[grade]" else "[post]", "ERROR", conditionMessage(e)); character() })
  st <- attr(out, "status"); if (is.null(st)) st <- 0
  log("  ", format(day), if (grade) "[grade]" else "[post]", "exit", st, "-",
      paste(tail(out[nzchar(out)], 2), collapse = " ; "))
}

for (d in as.list(post_days))  run_writer(d, grade = FALSE)
for (d in as.list(grade_days)) run_writer(d, grade = TRUE)

log("=== weekly_feed done ===")
