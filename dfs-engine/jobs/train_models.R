#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — (re)train a sport's projection model + cache it.
# Run periodically (e.g. weekly) so the model reflects recent games.
#   Rscript jobs/train_models.R --sport wnba [--seasons 2021:2025] [--backtest]
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
  sport <- a$sport %||% NULL
  if (is.null(sport)) { cat("Usage: Rscript jobs/train_models.R --sport wnba [--seasons 2021:2025] [--backtest]\n"); quit(status = 1) }
  dfs_load_sport(sport)
  trainer <- get0(paste0(sport, "_train"))
  if (is.null(trainer)) { cat("Sport '", sport, "' has no <sport>_train() model yet.\n", sep = ""); quit(status = 1) }
  seasons <- if (!is.null(a$seasons)) eval(parse(text = a$seasons)) else NULL
  trainer(seasons = seasons, refresh = TRUE)
  # also (re)train the trained OWNERSHIP model as logged actual-ownership accrues (no-op /
  # heuristic fallback until the sport has enough slates). Cheap + idempotent.
  tryCatch(train_ownership_model(sport), error = function(e) msg("ownership train skipped:", conditionMessage(e)))
  if (!is.null(a$backtest)) {
    bt <- get0(paste0(sport, "_accuracy_backtest")); if (!is.null(bt)) invisible(bt())
  }
}
