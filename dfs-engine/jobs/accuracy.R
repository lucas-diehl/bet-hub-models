#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — projection accuracy scorecard. How close our projections are to
# actual DK points, per sport (the metric that decides if we can win). Add a sport
# to also print the biggest individual misses.
#   Rscript jobs/accuracy.R [wnba|golf|tennis|...]
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine()

if (!interactive()) {
  projection_accuracy()
  for (sp in commandArgs(TRUE)) projection_misses(sp)
}
