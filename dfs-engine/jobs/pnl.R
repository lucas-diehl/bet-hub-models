#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — P&L ledger. Ingests any newly-settled DK standings into the
# results ledger, then prints your tracked win/loss + ROI (overall, by sport,
# by contest type). Add --backfill to (re)ingest everything already downloaded.
#   Rscript jobs/pnl.R [--backfill]
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine()

if (!interactive()) {
  force <- "--backfill" %in% commandArgs(TRUE)
  ingest_all_results(force = force)
  pnl_summary()
}
