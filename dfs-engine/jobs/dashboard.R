#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — build the interactive dashboard (one self-contained HTML file).
#   Rscript jobs/dashboard.R [--date 2026-06-23] [--bankroll 300]
#                            [--sports wnba,tennis,golf,nfl,ncaaf] [--nlineups 8]
# Open the resulting data/reports/dashboard_<date>.html in any browser.
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine()

parse_args <- function(args = commandArgs(TRUE)) {
  out <- list(); i <- 1L
  while (i <= length(args)) { if (startsWith(args[i], "--")) { out[[sub("^--", "", args[i])]] <- args[i + 1L]; i <- i + 2L } else i <- i + 1L }
  out
}

if (!interactive()) {
  a <- parse_args()
  sports <- if (!is.null(a$sports)) strsplit(a$sports, ",")[[1]] else c("wnba","tennis","golf","golf_m80","golf_opp","golf_opp_m80","nfl","ncaaf")
  contests <- if (!is.null(a$contests)) strsplit(a$contests, ",")[[1]] else NULL
  build_dashboard(sports = sports, contests = contests, date = a$date %||% Sys.Date(),
                  bankroll = a$bankroll, n_lineups = as.integer(a$nlineups %||% 8))
}
