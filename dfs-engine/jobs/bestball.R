#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — best-ball draft tool.
#   Rscript jobs/bestball.R                         # value board + forward projections -> CSVs
#   Rscript jobs/bestball.R --roster "Name1,Name2,...,Name18"   # season advance/win rate
# Advance rate = P(finish top-2 of a 12-team pool over weeks 1-14) vs an ADP-drafted field.
# It is INDICATIVE (best ball is high-variance); use it to compare roster-construction
# decisions, not as a precise probability.
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine(); dfs_load_sport("nfl")

parse_args <- function(args = commandArgs(TRUE)) { out <- list(); i <- 1L
  while (i <= length(args)) { if (startsWith(args[i], "--")) { out[[sub("^--", "", args[i])]] <- args[i + 1L]; i <- i + 2L } else i <- i + 1L }; out }

if (!interactive()) {
  a <- parse_args()
  if (!is.null(a$roster)) {
    players <- trimws(strsplit(a$roster, ",")[[1]])
    r <- bb_season_sim(players)
    if (!is.null(r)) {
      cat(sprintf("\n=== BEST-BALL SEASON SIM (%d players) ===\n", r$n_players))
      cat(sprintf("ADVANCE %.1f%%  (baseline %.1f%%)  |  WIN %.1f%%  |  season pts: mean %d, ceiling %d, floor %d\n",
          r$advance_pct, r$baseline_advance_pct, r$win_pct, r$mean_pts, r$ceiling_pts, r$floor_pts))
      if (length(r$unmatched)) cat("  unmatched (ignored):", paste(r$unmatched, collapse = ", "), "\n")
      print(r$roster)
    }
  } else {
    bb_export()                                                     # value board CSV
    fp <- bb_forward_projection()
    if (!is.null(fp)) {
      p <- dfs_path("data", "reports", sprintf("bestball_projection_%s.csv", Sys.Date()))
      data.table::fwrite(fp, p); msg("Forward projections ->", p)
      cat("\ntop-20 forward projections (pts/game):\n")
      print(head(fp[order(-proj_pg), .(player, pos, adp, proj_pg, boom, p_play, bye)], 20))
    }
  }
}
