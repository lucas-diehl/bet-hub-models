#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — log one DK "Contest Standings" CSV -> actual ownership.
# (For hands-off logging, prefer jobs/import_ownership.R — just drop CSVs in
#  data/ownership_inbox/ and it derives everything from the contest id.)
#
#   Rscript jobs/log_ownership.R --csv "contest-standings-123.csv" --sport wnba \
#       --date 2026-06-23 --contest 123 [--slate main] [--type gpp] [--fee 20] [--field 11764]
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
  if (is.null(a$csv) || is.null(a$sport) || is.null(a$date)) {
    cat("Usage: Rscript jobs/log_ownership.R --csv FILE --sport SPORT --date YYYY-MM-DD [--contest ID] [--slate main] [--type gpp] [--fee N] [--field N]\n")
    quit(status = 1)
  }
  log_ownership_csv(csv = a$csv, sport = a$sport, date = a$date, slate = a$slate %||% "main",
                    contest = a$contest, type = a$type %||% NA, fee = a$fee %||% NA, field = a$field %||% NA)
}
