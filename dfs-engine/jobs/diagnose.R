#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — loss-attribution diagnostic ("why did we lose?" autopsy).
# Run after settled slates (weekly is plenty) to see which lever is leaking:
# projection accuracy, ownership accuracy, chalk-duplication, or ceiling.
#   Rscript jobs/diagnose.R [--sports golf,wnba,tennis]
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
a <- parse_args()
sports <- if (!is.null(a$sports)) strsplit(a$sports, ",")[[1]] else c("golf", "wnba", "tennis")
loss_autopsy(sports)
