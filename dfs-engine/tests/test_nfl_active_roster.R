#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — regression test for the roster-status filter (sports/nfl/ingest.R::
# nfl_inactive_roster(), wired into sports/nfl/project.R). Closes a real blind spot in
# apply_inactives(): the ESPN injury feed is a "status for THIS WEEK's game" report, so
# a long-term IR player (season-ending, placed weeks ago) can fall off it entirely once
# he's no longer "in question" — confirmed case: Ricky Pearsall (SF, on IR), absent from
# injury_report("nfl") though DK's salary feed still listed him.
#
# Also regression-guards a REAL bug caught during development: matching by name ALONE
# collides across teams — "DeVonta Smith" (PHI WR, active) vs "Devonta Smith" (CAR
# practice-squad DB) share a normalized name, so a name-only filter wrongly excluded the
# active Eagles starter too. Fixed by keying on (name, team); this test asserts that
# fix specifically, not just the original Pearsall case.
#
# Network-dependent (live nflverse roster pull) — skips gracefully if unreachable.
# Run: Rscript tests/test_nfl_active_roster.R
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine(); dfs_load_sport("nfl")
suppressPackageStartupMessages(library(data.table))

.pass <- 0L; .fail <- 0L
ok <- function(desc, cond) {
  if (isTRUE(cond)) { .pass <<- .pass + 1L; cat("  ok   - ", desc, "\n", sep = "") }
  else              { .fail <<- .fail + 1L; cat("  FAIL - ", desc, "\n", sep = "") }
}

inactive <- tryCatch(nfl_inactive_roster(), error = function(e) NULL)
if (is.null(inactive)) {
  cat("nfl_inactive_roster() unavailable (no network / no cache) — skipping.\n")
  quit(status = 0L)
}
ok("returns a data.table keyed on (norm, team), not a flat name vector",
   is.data.table(inactive) && all(c("norm", "team") %in% names(inactive)))
ok("Ricky Pearsall (SF, season-ending IR) IS flagged inactive",
   nrow(inactive[norm == norm_name("Ricky Pearsall") & team == "SF"]) > 0)
ok("DeVonta Smith (PHI, active) is NOT flagged inactive despite a same-normalized-name\n         practice-squad player on a different team (CAR)",
   nrow(inactive[norm == norm_name("DeVonta Smith") & team == "PHI"]) == 0)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
