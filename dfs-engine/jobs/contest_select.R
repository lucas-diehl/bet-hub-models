#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — jobs/contest_select.R  (P3: which GPPs to enter)
# Ranks today's live DK contests per sport by structural +EV (rake / entry-cap
# softness / field size / overlay / top-heaviness). Projection-free — pure contest
# structure. Prints a board you can act on before locking lineups.
#
#   Rscript jobs/contest_select.R --sports wnba,golf,nfl
#   Rscript jobs/contest_select.R --sports golf --cap-min 100        # your multi-entry format only
#   Rscript jobs/contest_select.R --sports wnba --cap-max 1 --fee-max 25   # softest single-entry
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine()
suppressWarnings(suppressPackageStartupMessages(library(data.table)))

parse_args <- function(args = commandArgs(TRUE)) { out <- list(); i <- 1L
  while (i <= length(args)) { if (startsWith(args[i], "--")) { out[[sub("^--", "", args[i])]] <- args[i + 1L]; i <- i + 2L } else i <- i + 1L }; out }

a <- parse_args()
sports  <- if (!is.null(a$sports)) strsplit(a$sports, ",")[[1]] else c("wnba", "golf", "nfl", "tennis")
cap_min <- if (!is.null(a$`cap-min`)) as.integer(a$`cap-min`) else 1L
cap_max <- if (!is.null(a$`cap-max`)) as.integer(a$`cap-max`) else Inf
fee_max <- if (!is.null(a$`fee-max`)) as.numeric(a$`fee-max`) else Inf

for (sp in sports) {
  sc <- tryCatch(score_contests(sp, cap_min = cap_min, cap_max = cap_max, fee_max = fee_max),
                 error = function(e) { msg("contest-select", sp, "error:", conditionMessage(e)); NULL })
  print_contest_board(sc, top = 12L)
}
cat("\nRule of thumb: prefer rake <14%, single-/small-max fields (softer), and guaranteed pools\n",
    "filling slowly (overlay). 150-max mega-GPPs are the highest-rake, shark-heaviest format.\n")
