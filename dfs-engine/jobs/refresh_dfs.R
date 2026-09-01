#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — scoped DFS refresh (scrape + project + values) for the given sports.
# Purpose: catch LATE-POSTING daily slates (tennis/WNBA) that the main 10/13/16/19
# run misses, so the bet site's /dfs values feed doesn't sit on yesterday's board.
# Scrapes the fresh DK slate (via build_dashboard) then rewrites the values feed.
#   Rscript jobs/refresh_dfs.R [--sports tennis,wnba]
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine()
a <- commandArgs(trailingOnly = TRUE); i <- which(a == "--sports")
sports <- if (length(i)) strsplit(a[i + 1L], ",")[[1]] else c("tennis", "wnba")
msg("=== DFS refresh:", paste(sports, collapse = ","), "===")
# rebuild the FULL dashboard (all tabs; golf uses its cached model -> fast) so the published
# SIMULATOR refreshes at this cadence too and never lags the values feed. tennis/wnba scrape fresh.
Sys.setenv(GOLF_MODEL_AUTORUN = "0")
f <- tryCatch(build_dashboard(sports = c("wnba", "tennis", "golf", "golf_round", "golf_captain")),
              error = function(e) { msg("refresh build error:", conditionMessage(e)); NULL })
tryCatch(publish_simulator(f), error = function(e) msg("simulator publish error:", conditionMessage(e)))
# rewrite the /dfs values feed from the fresh scrape
tryCatch({
  Sys.setenv(DFS_VALUES_SOURCE_ONLY = "1")
  source(dfs_path("jobs", "dfs_values.R")); write_dfs_values(sports)
}, error = function(e) msg("refresh values error:", conditionMessage(e)))
# golf ROUND-specific feed: catch late-locking single-round slates (R2-R4 lock on later
# days) at the 3/8/11pm refreshes. No-op when no DK single-round slate is live.
tryCatch({
  rj <- dfs_path("jobs", "round_dfs.R")
  if (file.exists(rj)) {
    rs2 <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
    out <- system2(rs2, shQuote(rj), stdout = TRUE, stderr = TRUE)
    msg("round DFS feed:", if (length(out)) tail(out, 1) else "(done)")
  }
}, error = function(e) msg("round DFS feed error:", conditionMessage(e)))
