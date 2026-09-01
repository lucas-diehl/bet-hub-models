#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — jobs/round_dfs.R  (golf ROUND-specific DFS exposure feed)
# Auto-detects the live DK single-round golf slate (R1..R4) and writes the round feed
# the bet site's round simulator reads — the same top-10-by-exposure view as the
# tournament DFS feed, in a SEPARATE round_values_*.json with clear round headers.
#   1) export the v2-round projection (per-tee-time wind + FRL) from golf-modeling
#   2) run the spine single-round exposure build -> dashboard_feed/.../golf/round_values_*.json
#
#   Rscript jobs/round_dfs.R [--site dk] [--round N]     # N forces a round; else auto-detect
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine(); dfs_load_sport("golf")
suppressWarnings(suppressPackageStartupMessages(library(data.table)))

parse_args <- function(args = commandArgs(TRUE)) { out <- list(); i <- 1L
  while (i <= length(args)) { if (startsWith(args[i], "--")) { out[[sub("^--", "", args[i])]] <- args[i + 1L]; i <- i + 2L } else i <- i + 1L }; out }

a <- parse_args()
site <- a$site %||% "dk"

lr <- tryCatch(golf_live_round(), error = function(e) NULL)
if (is.null(lr)) { msg("round DFS: no live DK single-round golf slate — nothing to do."); quit(save = "no", status = 0) }
rnd <- if (!is.null(a$round)) as.integer(a$round) else lr$round
msg(sprintf("round DFS: live single-round slate = Round %d (%s)", rnd, lr$name %||% ""))

# 1) export the v2-round projection (per-tee-time wind + FRL) from golf-modeling
dir <- golf_model_dir()
if (!is.na(dir) && file.exists(file.path(dir, "engine", "round_sim.R"))) {
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  old <- getwd(); on.exit(setwd(old), add = TRUE); setwd(dir)
  msg("round DFS: exporting v2-round projection (round ", rnd, ") ...")
  out <- tryCatch(system2(rscript, c(shQuote(file.path(dir, "engine", "round_sim.R")), "--round", rnd, "--export"),
                          stdout = TRUE, stderr = TRUE), error = function(e) conditionMessage(e))
  setwd(old); msg("round DFS export:", if (length(out)) tail(out, 1) else "(done)")
} else msg("round DFS: golf-modeling round_sim.R not found -> adapter will use DataGolf fallback")

# 2) write the round exposure feed (adapter prefers the export just written)
Sys.setenv(DFS_VALUES_SOURCE_ONLY = "1")
source(dfs_path("jobs", "dfs_values.R"))
f <- write_dfs_round_values(rnd, site)
if (!is.null(f)) msg("round DFS: wrote ", f) else msg("round DFS: no feed written")
