# Unit tests: NFL salary stack (sports/nfl/project.R .nfl_salary_stack) + built-in history guard.
# Run: Rscript tests/test_nfl_salary_stack.R  (from dfs-engine/)
suppressPackageStartupMessages(library(data.table))
source("bootstrap.R"); dfs_load_spine(); dfs_load_sport("nfl")
msg <- function(...) invisible(NULL)
n_ok <- 0L; n_bad <- 0L
ok <- function(cond, label) { if (isTRUE(cond)) { n_ok <<- n_ok + 1L; cat("ok   -", label, "\n") } else { n_bad <<- n_bad + 1L; cat("FAIL -", label, "\n") } }
P <- data.table(team = c("AAA", "BBB", "CCC", "AAA", "BBB", "CCC"), position = c("QB", "RB", "WR", "TE", "DST", "WR"),
                salary = c(7000, 8000, 3000, 4500, 3500, 6000), proj = c(18, 12, 9, 7, 6, 14), ceil = c(25, 20, 15, 12, 10, 22), floor = c(10, 5, 2, 2, 1, 6))
S <- .nfl_salary_stack(copy(P))
b <- NFL_SALARY_STACK$QB; s <- 7
ok(abs(S$proj[1] - (b[1] + b[2] * 18 + b[3] * s + b[4] * s^2)) < 1e-9, "QB row follows the fitted formula")
ok(S[position == "DST", proj] == 6, "DST untouched (no stack for DST)")
ok(all(S$proj >= 0), "no negative projections")
d <- S$proj - P$proj; ok(all(abs((S$ceil - P$ceil) - d)[S$ceil > S$proj] < 1e-9), "ceil shifted by the same amount as proj")
ok(S$proj[3] > P$proj[3] - 9 && S$proj[3] < P$proj[3] + 9, "cheap WR stays in a sane range")
# cheap player we over-rate gets pulled toward price; expensive player we under-rate gets pulled up
ok(S$proj[6] > S$proj[3], "higher-priced WR with higher proj stays above cheap WR")
M <- .nfl_salary_stack(copy(P), model_rows = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE))
ok(M$proj[2] == P$proj[2], "salary-baseline (non-model) rows are not stacked")
# showdown pricing (2 teams) is mapped back to the classic scale before stacking
SD <- data.table(team = c("AAA", "BBB"), position = c("WR", "WR"), salary = c(-5828 + 2.533 * 6000, -5828 + 2.533 * 6000), proj = c(14, 14))
C6 <- .nfl_salary_stack(data.table(team = c("AAA", "BBB", "CCC"), position = "WR", salary = 6000, proj = 14))
ok(identical(.nfl_salary_stack(copy(SD))$proj, SD$proj), "showdown pools skipped by default (classic-validated only)")
Sys.setenv(DFS_NFL_SALARY_STACK_SD = "1")
ok(abs(.nfl_salary_stack(copy(SD))$proj[1] - C6$proj[1]) < 1e-6, "opt-in: showdown salary mapped to classic scale")
Sys.unsetenv("DFS_NFL_SALARY_STACK_SD")
ok(exists("nfl_weekly_ensure"), "built-in history guard exists (nfl_weekly_ensure)")
cat(sprintf("\n%d passed, %d failed\n", n_ok, n_bad)); if (n_bad) quit(status = 1) else cat("ALL PASS\n")
