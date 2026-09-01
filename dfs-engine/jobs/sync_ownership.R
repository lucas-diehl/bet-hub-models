#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — sync actual ownership from your entered DK contests.
# Downloads standings (with %Drafted) for your entered+settled contests using your
# DK session cookie (config/dk_session.txt), then logs them. Runs inside run_all.R
# too; this is for an on-demand sync.
#   Rscript jobs/sync_ownership.R [--contests id,id]
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
  ids <- if (!is.null(a$contests)) vapply(strsplit(a$contests, ",")[[1]], dk_parse_contest_id, character(1)) else NULL
  sync_ownership(extra_ids = ids)
  auto_import_ownership()
}
