#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — jobs/my_contests.R
# "What am I actually entered in, and is the structure any good?" Fetches YOUR live
# DK entries (authenticated /mycontests) and scores their rake / field / entry-cap,
# then shows the best-structured ALTERNATIVES available for the same sports. Real-world
# math (no sim) — this is the highest-leverage, sim-independent DFS decision.
# Sport-agnostic: golf today, NFL when the season starts (same call).
#   Rscript jobs/my_contests.R
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine()
suppressWarnings(suppressPackageStartupMessages(library(data.table)))

r <- my_contest_report()
print_my_contests(r)

if (!is.null(r) && nrow(r)) {
  cat("\n================ BETTER-STRUCTURED ALTERNATIVES (per sport you're playing) ================\n")
  for (sp in sort(unique(r$sport))) {
    if (!sp %in% c("golf","wnba","tennis","nfl","nba","ncaaf")) next
    sc <- tryCatch(score_contests(sp, fetch_topheavy = FALSE), error = function(e) NULL)
    if (!is.null(sc) && nrow(sc)) print_contest_board(sc, top = 6L)
  }
  cat("\nRule of thumb: prefer rake <14%, single-/small-max fields (softer), guaranteed pools\n",
      "filling slowly (overlay). The 150-max mega-GPPs are the highest-rake, shark-heaviest format.\n")
}
