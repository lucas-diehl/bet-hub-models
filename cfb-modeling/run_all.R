# ============================================================================
# run_all.R  —  end-to-end PPP pipeline orchestrator
# ----------------------------------------------------------------------------
# Runs the whole thing from the cached data:
#   01_build_possessions.R   plays -> drives -> team-game PPP        (touches 519MB pbp; ~2-3 min)
#   02_build_asof_ratings.R  leak-free opponent-adjusted as-of ratings
#   03_models_backtest.R     A/B/C bake-off + benchmark + metrics
#   04_weekly_picks.R        weekly projections + recommended bets
#
# Usage (from the project dir):
#   Rscript run_all.R              # full rebuild incl. possessions
#   Rscript run_all.R --fast       # skip 01/02 (reuse cached ratings), just 03/04
#
# In-season weekly update: source 04's refresh_2026(), then run without --fast.
# ============================================================================

args <- commandArgs(trailingOnly = TRUE)
fast <- "--fast" %in% args
t0 <- Sys.time()

step <- function(f) { cat("\n", strrep("#", 78), "\n## ", f, "\n", strrep("#", 78), "\n", sep=""); source(f, local = new.env()) }

if (!fast) {
  step("01_build_possessions.R")
  step("02_build_asof_ratings.R")
  step("05_build_features.R")        # heavy pbp pass -> team_game_features.rds
} else {
  cat("[--fast] skipping 01/02/05_build_features, reusing cached rds\n")
}
step("03_models_backtest.R")
step("05_bet_slip.R")       # UNDER totals bet slip
step("06_early_ats.R")      # early-season 4th-down aggressiveness ATS bet slip
step("04_weekly_picks.R")
# dashboard feed (set PPP_SEASON/PPP_WEEK for a live slate; defaults to a demo week)
if (nzchar(Sys.getenv("WRITE_FEED", ""))) step("07_write_feed.R")

cat(sprintf("\n✓ pipeline complete in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
