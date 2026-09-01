#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — auto-import ownership. Drop DK "Contest Standings" CSVs into
# data/ownership_inbox/ and run this (or let the scheduler run it). It derives
# sport/date/fee/field from the contest id in each filename via the contest API,
# logs actual ownership, and moves the file to processed/. No flags to type.
#   Rscript jobs/import_ownership.R
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine()
if (!interactive()) auto_import_ownership()
