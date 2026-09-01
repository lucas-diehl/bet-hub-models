#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — fast daily projection logging (no sim). Persists today's projections
# + projected ownership + salaries for each sport with a live slate, so the accuracy
# scorecard can grade them later. Runs automatically inside run_all.R; this is the
# standalone version.
#   Rscript jobs/log_projections.R [--date 2026-07-21] [--sports wnba,tennis,golf,golf_opp]
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine()

parse_args <- function(args = commandArgs(TRUE)) { out <- list(); i <- 1L
  while (i <= length(args)) { if (startsWith(args[i], "--")) { out[[sub("^--", "", args[i])]] <- args[i + 1L]; i <- i + 2L } else i <- i + 1L }; out }

if (!interactive()) {
  a <- parse_args()
  sports <- if (!is.null(a$sports)) strsplit(a$sports, ",")[[1]] else c("wnba", "tennis", "golf", "golf_opp")
  log_projections(sports = sports, date = a$date %||% Sys.Date())
}
