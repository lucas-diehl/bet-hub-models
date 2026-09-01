#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — DAILY REPORT  ("what do I enter today, how much, what lineups")
# Runs the full pipeline for a sport/slate and writes:
#   - an HTML one-pager (open in a browser)  -> data/reports/plan_<sport>_<date>.html
#   - an xlsx with Lineups + Rankings        -> data/lineups/<sport>/
#   - a console card
# Assumes salaries are already scraped (jobs/scrape_salaries.R).
#
#   Rscript jobs/daily_report.R --sport wnba [--date 2026-06-23] [--bankroll 300]
#                               [--nlineups 8] [--surface Hard] [--best_of 3]
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine()
suppressPackageStartupMessages({ library(data.table) })

parse_args <- function(args = commandArgs(TRUE)) {
  out <- list(); i <- 1L
  while (i <= length(args)) { if (startsWith(args[i], "--")) { out[[sub("^--", "", args[i])]] <- args[i + 1L]; i <- i + 2L } else i <- i + 1L }
  out
}

daily_report <- function(sport, date = Sys.Date(), slate = "main", n_lineups = 8L,
                         bankroll = NULL, extra = list()) {
  if (sport != "golf" && !ensure_dk_slate(sport, date, slate))
    stop(sprintf("No live %s slate on DraftKings for %s yet (DK posts a few hours before lock).", sport, date))
  r <- run_slate(sport, date = date, slate = slate, n_lineups = n_lineups, extra = extra)
  opts <- load_bankroll_opts(); if (!is.null(bankroll)) opts$bankroll <- as.numeric(bankroll)
  plan <- recommend_plan(r$picks, r$gates, opts)
  write_report(r$picks, r$pool, sport, r$slate_id, r$gates, r$rr)            # xlsx + DK csv
  write_daily_report(sport, r$slate_id, r$pool, r$picks, r$res, r$gates, plan = plan, opts = opts)
  invisible(r)
}

if (!interactive()) {
  a <- parse_args()
  if (is.null(a$sport)) { cat("Usage: Rscript jobs/daily_report.R --sport SPORT [--date YYYY-MM-DD] [--bankroll N] [--nlineups N] [--surface Hard] [--best_of 3]\n"); quit(status = 1) }
  extra <- list()
  if (!is.null(a$surface)) extra$surface <- a$surface
  if (!is.null(a$best_of)) extra$best_of <- as.integer(a$best_of)
  if (!is.null(a$tour))    extra$tour <- a$tour
  daily_report(sport = a$sport, date = a$date %||% Sys.Date(), slate = a$slate %||% "main",
               n_lineups = as.integer(a$nlineups %||% 8), bankroll = a$bankroll, extra = extra)
}
