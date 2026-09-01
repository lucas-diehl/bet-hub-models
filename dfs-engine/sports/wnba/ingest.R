# ==============================================================================
# WNBA plugin — ingestion (wehoop history -> training store)
# Pulls player box scores via wehoop (sportsdataverse, free), computes DK fantasy
# points, and stores them for the minutes/usage projection model + the contest
# backtest. Run periodically; not at import.
#
# DATA GAP (documented): wehoop has stats but NOT historical DK salaries/ownership.
# The contest backtest (validate.R L3) needs those per slate — which is exactly why
# the ownership logger (jobs/log_ownership.R) and saved DKSalaries.csv exports must
# accumulate from day one. Until enough slates are logged, WNBA L3 is limited.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

wnba_box_path <- function() dfs_path("data", "raw", "wnba_player_box.rds")

# Keep the box current: if its latest game is more than `max_lag` days before the
# slate date AND we're in WNBA season (May-Oct), re-pull recent games. Projections
# read entering-form from the box, so this keeps daily picks current WITHOUT a
# retrain (the trained model is stable). Cheap (~5s) and defensive.
wnba_ensure_fresh_box <- function(as_of = Sys.Date(), max_lag = 2L) {
  as_of <- as.Date(as_of); mo <- as.integer(format(as_of, "%m"))
  bp <- wnba_box_path(); stale <- TRUE
  if (file.exists(bp)) {
    last <- suppressWarnings(max(as.Date(as.data.table(readRDS(bp))$game_date), na.rm = TRUE))
    stale <- !is.finite(last) || as.numeric(as_of - last) > max_lag
  }
  if (stale && mo >= 5 && mo <= 10) {
    yr <- as.integer(format(as_of, "%Y"))
    tryCatch(wnba_ingest((yr - 3):yr), error = function(e) msg("  box refresh failed:", conditionMessage(e)))
  }
  invisible()
}

# Download + store player box scores with DK points. seasons defaults to last 4 yrs.
wnba_ingest <- function(seasons = NULL) {
  if (!requireNamespace("wehoop", quietly = TRUE)) stop("install.packages('wehoop')")
  yr <- as.integer(format(Sys.Date(), "%Y"))
  if (is.null(seasons)) seasons <- (yr - 3):yr
  msg("wehoop: loading WNBA player box for", paste(range(seasons), collapse = "-"))
  box <- tryCatch(as.data.table(wehoop::load_wnba_player_box(seasons = seasons)),
                  error = function(e) { msg("  wehoop load failed:", conditionMessage(e)); NULL })
  if (is.null(box) || !nrow(box)) stop("No WNBA box data returned.")

  # wehoop player-box columns -> our scoring inputs (verified names, 2025 schema)
  setnames(box, "points", "pts", skip_absent = TRUE)
  setnames(box, "three_point_field_goals_made", "fg3m", skip_absent = TRUE)
  setnames(box, "rebounds", "reb", skip_absent = TRUE)
  setnames(box, "assists", "ast", skip_absent = TRUE)
  setnames(box, "steals", "stl", skip_absent = TRUE)
  setnames(box, "blocks", "blk", skip_absent = TRUE)
  setnames(box, "turnovers", "tov", skip_absent = TRUE)
  setnames(box, "minutes", "min", skip_absent = TRUE)
  # DNP rows have NA minutes -> treat as 0 (survivorship: keep zeros, like golf)
  for (c in c("min","pts","fg3m","reb","ast","stl","blk","tov"))
    if (c %in% names(box)) box[[c]] <- fcoalesce(as.numeric(box[[c]]), 0)
  box[, dk_pts := wnba_dk_scoring(box)]

  dir.create(dirname(wnba_box_path()), recursive = TRUE, showWarnings = FALSE)
  saveRDS(box, wnba_box_path())
  msg("Stored", nrow(box), "WNBA player-games ->", wnba_box_path())
  invisible(box)
}
