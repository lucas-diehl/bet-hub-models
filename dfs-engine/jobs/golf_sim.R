#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — golf pre-contest simulator (maximize your entries)
#   Rscript jobs/golf_sim.R [--tournament main|opp] [--variant default|m80] [--n 20]
# Prints + writes (data/reports/) a portfolio of optimized lineups with simulated
# cash/boom probabilities + GPP EV, and a player-exposure table.
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine(); dfs_load_sport("golf")

parse_args <- function(args = commandArgs(TRUE)) {
  out <- list(); i <- 1L
  while (i <= length(args)) { if (startsWith(args[i], "--")) { out[[sub("^--", "", args[i])]] <- args[i + 1L]; i <- i + 2L } else i <- i + 1L }
  out
}

if (!interactive()) {
  a <- parse_args()
  golf_presim(tournament = a$tournament %||% "main",
              variant    = a$variant %||% "default",
              n          = as.integer(a$n %||% 20))
}
