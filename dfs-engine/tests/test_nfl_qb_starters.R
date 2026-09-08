#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — regression test for the backup-QB filter (sports/nfl/ingest.R::
# nfl_qb_starters(), wired into sports/nfl/project.R). A healthy benched backup (e.g.
# Carson Wentz, MIN, depth-chart rank 3) is NOT "injured" so apply_inactives()'s
# ESPN-injury safeguard can't catch him — without this filter he'd get a small nonzero
# salary-baseline projection that a min-salary-hungry optimizer will happily plug in as
# "cheap value" despite his real expected output being ~0.
# Network-dependent (live nflverse depth-chart pull) — skips gracefully if unreachable,
# same as any other external-data test in this suite.
# Run: Rscript tests/test_nfl_qb_starters.R
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine(); dfs_load_sport("nfl")

.pass <- 0L; .fail <- 0L
ok <- function(desc, cond) {
  if (isTRUE(cond)) { .pass <<- .pass + 1L; cat("  ok   - ", desc, "\n", sep = "") }
  else              { .fail <<- .fail + 1L; cat("  FAIL - ", desc, "\n", sep = "") }
}

starters <- tryCatch(nfl_qb_starters(), error = function(e) NULL)
if (is.null(starters)) {
  cat("nfl_qb_starters() unavailable (no network / no cache) — skipping.\n")
  quit(status = 0L)
}
ok("returns a nonempty set of starters", length(starters) > 0)
ok("roughly one starter per team (28-34, allows for bye-week/injury edge cases)",
   length(starters) >= 28 && length(starters) <= 34)
ok("Carson Wentz (MIN, depth rank 3) is NOT flagged a starter", !("carson wentz" %in% starters))
ok("all entries are normalized (lowercase, no punctuation)", all(starters == norm_name(starters)))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
