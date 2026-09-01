#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — live pipeline orchestrator (sport-agnostic)
# Runs the full spine for one sport/slate: project -> correlate -> slate sim ->
# field sim -> candidate pool -> EV grade -> portfolio -> export. The ONLY
# sport-specific calls go through the plugin contract (get_sport).
#
#   Rscript run_sport.R --sport wnba [--slate main] [--date 2026-06-22] \
#       [--nsims 10000] [--ncand 400] [--nlineups 8] [--field 1500] [--contest gpp]
# ==============================================================================

local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(normalizePath(sub("^--file=", "", m[1]))) else getwd()
  source(file.path(root, "bootstrap.R"))
})
dfs_load_spine()

suppressPackageStartupMessages({ library(data.table) })

parse_args <- function(args = commandArgs(TRUE)) {
  out <- list(); i <- 1L
  while (i <= length(args)) { if (startsWith(args[i], "--")) { out[[sub("^--", "", args[i])]] <- args[i + 1L]; i <- i + 2L } else i <- i + 1L }
  out
}

run_sport <- function(sport, slate = "main", date = Sys.Date(),
                      n_sims = 10000L, n_cand = 400L, n_lineups = 8L, field_n = 1500L,
                      site = "dk", seed = 2026L) {
  r <- run_slate(sport, date = date, slate = slate, site = site, n_sims = n_sims,
                 n_cand = n_cand, n_lineups = n_lineups, field_n = field_n, seed = seed)
  write_report(r$picks, r$pool, sport, r$slate_id, r$gates, r$rr)
  invisible(r$picks)
}

if (!interactive()) {
  a <- parse_args()
  if (is.null(a$sport)) { cat("Usage: Rscript run_sport.R --sport SPORT [--slate main] [--date YYYY-MM-DD]\n"); quit(status = 1) }
  run_sport(sport = a$sport, slate = a$slate %||% "main", date = a$date %||% Sys.Date(),
            n_sims = as.integer(a$nsims %||% 10000), n_cand = as.integer(a$ncand %||% 400),
            n_lineups = as.integer(a$nlineups %||% 8), field_n = as.integer(a$field %||% 1500))
}
