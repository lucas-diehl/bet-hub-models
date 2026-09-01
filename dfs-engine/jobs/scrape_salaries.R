#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — daily DK salary scrape (free, repeatable, backtestable)
# Run every day per sport to pull the slate's player pool + salaries from DK's
# public JSON API and snapshot it (this accumulates the historical salary record
# the backtest needs). Schedule via Windows Task Scheduler.
#
#   Rscript jobs/scrape_salaries.R --sport wnba [--date 2026-06-23] [--slate main]
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
  if (is.null(a$sport)) { cat("Usage: Rscript jobs/scrape_salaries.R --sport SPORT [--date YYYY-MM-DD] [--slate main]\n"); quit(status = 1) }
  scrape_dk_salaries(sport = a$sport, date = a$date %||% Sys.Date(), slate = a$slate %||% "main")
}
