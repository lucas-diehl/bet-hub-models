#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — regression test for dk_slate_options() (spine/R/dk_scrape.R). Real bug,
# user-reported: only ONE live single-game Showdown slate was ever built (single[1]),
# even when DK runs several simultaneously (one per marquee matchup) — a real Rams/49ers
# Showdown was silently dropped in favor of an earlier-kickoff one. Fixed to enumerate
# ALL live showdowns, each with a distinct slate_tag, while excluding formats the
# roster/scoring logic doesn't support (Snake draft, in-game/live contests) — confirmed
# real during development: DK was also running a "Snake Showdown" and a "2nd Half"
# in-game contest for the same matchup, which are NOT the CPT+FLEX format this pipeline
# builds for.
# Network-dependent (live DK lobby pull) — skips gracefully if unreachable.
# Run: Rscript tests/test_dk_slate_options.R
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine()
suppressPackageStartupMessages(library(data.table))

.pass <- 0L; .fail <- 0L
ok <- function(desc, cond) {
  if (isTRUE(cond)) { .pass <<- .pass + 1L; cat("  ok   - ", desc, "\n", sep = "") }
  else              { .fail <<- .fail + 1L; cat("  FAIL - ", desc, "\n", sep = "") }
}

opts <- tryCatch(dk_slate_options("nfl"), error = function(e) NULL)
if (is.null(opts) || !nrow(opts)) {
  cat("dk_slate_options('nfl') unavailable (no network / no live slates) — skipping.\n")
  quit(status = 0L)
}
ok("every row has a unique slate_tag (no showdowns silently collapsed onto one tag)",
   uniqueN(opts$slate_tag) == nrow(opts))
sd_rows <- opts[is_showdown == TRUE]
ok("no Snake-draft contest leaked into the showdown rows",
   !any(grepl("snake", sd_rows$top_name, ignore.case = TRUE)))
ok("no in-game/live contest leaked into the showdown rows",
   !any(grepl("in-game|2nd half|1st half", sd_rows$top_name, ignore.case = TRUE)))
if (nrow(sd_rows) > 1) {
  ok("multiple live showdowns each get a DISTINCT matchup in their layout_label",
     uniqueN(sd_rows$layout_label) == nrow(sd_rows))
  cat(sprintf("  (found %d simultaneous live showdowns: %s)\n", nrow(sd_rows),
              paste(sd_rows$layout_label, collapse = ", ")))
} else cat("  (only 0-1 showdown currently live -- multi-showdown case not exercised this run)\n")

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
