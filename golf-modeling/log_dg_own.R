#!/usr/bin/env Rscript
# ==============================================================================
# golf-modeling — log_dg_own.R
# Snapshot DataGolf's PROJECTED ownership for the live slate(s) each day, appending
# to golf_picks/dg_ownership_log.csv. This builds the A/B record so we can later
# grade DataGolf's ownership vs OUR model (v2_own_model, cor .81) vs actual DK
# ownership. Run daily (wired into DFS ENGINE jobs/run_all.R). No-op if no slate.
#   Rscript log_dg_own.R
# ==============================================================================
Sys.setenv(OWNERSHIP_SOURCE_ONLY = "1")
local({
  win <- "c:/Users/ljdie/OneDrive/Documents/golf-modeling"
  if (dir.exists(win)) setwd(win) else {  # CI/Linux: find the golf root from --file
    m <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(m)) { d <- dirname(normalizePath(sub("^--file=", "", m[1])))
      while (!dir.exists(file.path(d, "golf_picks")) && dirname(d) != d) d <- dirname(d)
      if (dir.exists(file.path(d, "golf_picks"))) setwd(d) } }
})
for (p in c(".Renviron", "../DFS ENGINE/.env")) if (file.exists(p)) try(readRenviron(p), silent = TRUE)
suppressWarnings(suppressPackageStartupMessages(source("engine/ownership.R")))  # -> log_dg_ownership(), .dg
emsg <- get0("emsg", ifnotfound = function(...) cat(..., "\n"))

if (!nzchar(Sys.getenv("DATAGOLF_API_KEY"))) {
  emsg("dg-own log: DATAGOLF_API_KEY not set — skipping"); quit(save = "no")
}
for (tour in c("pga", "opp"))            # main tour + opposite-field event when present
  tryCatch(log_dg_ownership(tour), error = function(e) emsg("dg-own log", tour, "error:", conditionMessage(e)))
