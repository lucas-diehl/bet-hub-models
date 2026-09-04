# ============================================================================
# weekly_update.R  —  hands-off weekly driver for the dashboard feed
# ----------------------------------------------------------------------------
# Scheduled (Windows Task Scheduler, Mon+Thu). Hardened so an off-season or
# no-new-data run is a fast no-op, heavy steps are isolated + time-limited (no
# runaway/orphan Rscript), and the feed writer is ALWAYS reached.
#
# Flow:
#   1. light refresh (schedule/lines/SP+/coaches — cheap, non-fatal)
#   2. IDLE GATE: if no current-season game within a +/-9d window -> exit fast
#      (this is what makes off-season runs cheap; mirrors the manual fast path)
#   3. rebuild ONLY if new completed games since last rebuild:
#        heavy pbp refresh, then 01 -> 02 -> 05_build_features, each as an
#        isolated subprocess with a timeout (memory-safe, kills any runaway)
#   4. write/grade the feed (07 per week, subprocess, current season only)
#
# Env: PPP_SEASON (force), SKIP_REFRESH, SKIP_REBUILD, GRADE_LOOKBACK (2)
# ============================================================================

PROJ <- "C:/Users/ljdie/OneDrive/Documents/cfb-modeling"
if (dir.exists(PROJ)) setwd(PROJ)
if (file.exists(".Renviron")) readRenviron(".Renviron")
suppressWarnings(suppressMessages(library(dplyr)))
# no parallel workers anywhere (cfbfastR/furrr can spawn multisession R procs that
# orphan if a run is killed) — force sequential + single-thread, belt and suspenders
options(future.fork.enable = FALSE); Sys.setenv(R_FUTURE_PLAN = "sequential")
try(if (requireNamespace("future", quietly = TRUE)) future::plan("sequential"), silent = TRUE)
try(data.table::setDTthreads(1), silent = TRUE)

log <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", ..., "\n")
SKIP_REFRESH   <- nzchar(Sys.getenv("SKIP_REFRESH"))
SKIP_REBUILD   <- nzchar(Sys.getenv("SKIP_REBUILD"))
GRADE_LOOKBACK <- as.integer(Sys.getenv("GRADE_LOOKBACK", "2"))
STATE_FILE     <- ".state_rebuild.txt"
RSCRIPT        <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
STEP_TIMEOUT   <- 1500   # seconds per heavy subprocess (kills runaways; 3 steps < task's 2h)

season <- suppressWarnings(as.integer(Sys.getenv("PPP_SEASON", "")))
if (is.na(season)) { td <- Sys.Date(); yr <- as.integer(format(td,"%Y")); mo <- as.integer(format(td,"%m"))
  season <- if (mo >= 8) yr else yr - 1 }
log("=== weekly_update start | season", season, "===")

run_step <- function(script, setenv = NULL) {   # isolated subprocess + timeout; never throws
  if (!is.null(setenv)) do.call(Sys.setenv, setenv)   # child inherits (system2 env= is ignored on Windows)
  tryCatch({
    out <- system2(RSCRIPT, shQuote(script), stdout = TRUE, stderr = TRUE, timeout = STEP_TIMEOUT)
    st  <- attr(out, "status"); if (is.null(st)) st <- 0
    log("  ", script, "exit", st, "-", paste(tail(out, 2), collapse = " ; ")); st
  }, error = function(e) { log("  ", script, "ERROR", conditionMessage(e)); 1L })
}

# ---- helpers to (re)load season data ---------------------------------------
refresh <- function(yr, heavy) {
  suppressWarnings(suppressMessages(library(cfbfastR)))
  key <- Sys.getenv("CFB_API_KEY"); if (key == "") key <- Sys.getenv("CFBD_API_KEY")
  if (key != "") Sys.setenv(CFBD_API_KEY = key) else { log("WARN no CFBD key; using cache"); return(invisible()) }
  add <- function(cache, col, loader) {
    new <- tryCatch(loader(yr), error = function(e) { log("  refresh FAIL", basename(cache), conditionMessage(e)); NULL })
    if (is.null(new) || nrow(new) == 0) return(invisible())
    old <- if (file.exists(cache)) readRDS(cache) else NULL
    if (!is.null(old) && col %in% names(old)) old <- dplyr::filter(old, .data[[col]] != yr)
    saveRDS(dplyr::bind_rows(old, new), cache); log("  refreshed", basename(cache), nrow(new), "rows")
  }
  add("data_cache/game_info.rds",     "season", function(y) cfbfastR::cfbd_game_info(year = y, season_type = "regular"))
  add("data_cache/betting_lines.rds", "season", function(y) cfbfastR::cfbd_betting_lines(year = y, season_type = "regular"))
  add("data_cache/sp_ratings.rds",    "year",   function(y) cfbfastR::cfbd_ratings_sp(year = y))
  add("data_cache/coaches.rds",       "year",   function(y) cfbfastR::cfbd_coaches(year = y))
  if (heavy) add("data_cache/pbp_data.rds", "season", function(y) cfbfastR::load_cfb_pbp(seasons = y))
}

# ---- 1. light refresh (schedule + lines) -----------------------------------
if (!SKIP_REFRESH) tryCatch(refresh(season, heavy = FALSE), error = function(e) log("light refresh error:", conditionMessage(e))) else log("SKIP_REFRESH")

if (!file.exists("data_cache/game_info.rds")) { log("no game_info cache; nothing to do."); quit(save="no", status=0) }
gi <- readRDS("data_cache/game_info.rds")
gs <- gi %>% mutate(season = if ("season" %in% names(.)) season else year,
                    start  = as.POSIXct(sub("\\..*$","", start_date), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
                    final  = !is.na(as.numeric(home_points))) %>%
  filter(season == !!season, !is.na(week))

# ---- 2. IDLE GATE ----------------------------------------------------------
now <- Sys.time()
have_soon   <- any(gs$start >= now - 9*86400 & gs$start <= now + 9*86400, na.rm = TRUE)
have_recent <- any(gs$final & gs$start >= now - 16*86400, na.rm = TRUE)
idle <- nrow(gs) == 0 || (!have_soon && !have_recent)
if (idle && !nzchar(Sys.getenv("FORCE_ACTIVE"))) {
  log("idle (no games within window for season", season, ") — fast exit, no refresh/rebuild."); quit(save="no", status=0)
}
if (idle) log("idle but FORCE_ACTIVE set — continuing (test)")

# ---- 3. rebuild only if new completed games --------------------------------
n_final <- sum(gs$final, na.rm = TRUE)
key_now <- paste(season, n_final)
prev    <- if (file.exists(STATE_FILE)) readLines(STATE_FILE, warn = FALSE)[1] else ""
need_rebuild <- !SKIP_REBUILD && !identical(prev, key_now)
if (need_rebuild) {
  log("new data (", n_final, "final games; prev '", prev, "') -> refresh pbp + rebuild")
  if (!SKIP_REFRESH) tryCatch(refresh(season, heavy = TRUE), error = function(e) log("pbp refresh error:", conditionMessage(e)))
  ok <- TRUE
  for (s in c("01_build_possessions.R","02_build_asof_ratings.R","05_build_features.R"))
    if (run_step(s) != 0) ok <- FALSE
  if (ok) writeLines(key_now, STATE_FILE) else log("rebuild had failures; state not advanced (will retry)")
} else log(if (SKIP_REBUILD) "SKIP_REBUILD" else paste("no new completed games (", n_final, ") — skip rebuild"))

# ---- 4. write/grade the feed (current season only) -------------------------
future <- gs %>% filter(start >= now - 12*3600)
pick_week <- if (nrow(future) > 0) min(future$week, na.rm = TRUE) else max(gs$week, na.rm = TRUE)
grade_weeks <- gs %>% group_by(week) %>% summarise(all_final = all(final), .groups = "drop") %>%
  filter(all_final, week >= pick_week - GRADE_LOOKBACK, week < pick_week) %>% pull(week)
weeks <- sort(unique(c(grade_weeks, pick_week))); weeks <- weeks[is.finite(weeks)]
log("pick week", pick_week, "| grade weeks", if (length(grade_weeks)) paste(grade_weeks, collapse=",") else "none")

for (w in weeks)
  run_step("07_write_feed.R", setenv = list(PPP_SEASON = as.character(season), PPP_WEEK = as.character(w), PPP_MODE = "PAPER"))

# full-slate model board for the Extras tab (every game + model numbers), current week only
run_step("08_write_slate.R", setenv = list(PPP_SEASON = as.character(season), PPP_WEEK = as.character(pick_week), PPP_MODE = "PAPER"))

log("=== weekly_update done ===")
