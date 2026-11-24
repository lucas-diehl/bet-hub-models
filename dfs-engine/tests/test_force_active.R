# Unit tests: "<sport>_force_active" overrides (spine/R/injuries.R load_force_active + apply_inactives exemption).
# Run: Rscript tests/test_force_active.R  (from dfs-engine/)
suppressPackageStartupMessages(library(data.table))
source("bootstrap.R"); dfs_load_spine()
msg <- function(...) invisible(NULL)
n_ok <- 0L; n_bad <- 0L
ok <- function(cond, label) { if (isTRUE(cond)) { n_ok <<- n_ok + 1L; cat("ok   -", label, "\n") } else { n_bad <<- n_bad + 1L; cat("FAIL -", label, "\n") } }
tf <- tempfile(fileext = ".json"); dfs_path_orig <- dfs_path; dfs_path <- function(...) tf
writeLines('{"nfl_force_active": ["Jacory Croskey-Merritt"]}', tf)
ok(identical(load_force_active("nfl"), norm_name("Jacory Croskey-Merritt")), "force-active list loads (normalized)")
ok(length(load_force_active("wnba")) == 0, "other sports unaffected")
injury_report_orig <- injury_report
injury_report <- function(sport) data.table(norm = norm_name(c("Jacory Croskey-Merritt", "Other Guy")), cat = c("out", "out"))
P <- data.table(player_name = c("Jacory Croskey-Merritt", "Other Guy", "Healthy Guy"), proj = c(9, 8, 7))
A <- apply_inactives(copy(P), "nfl")
ok("Jacory Croskey-Merritt" %in% A$player_name, "force-active player survives an Out listing")
ok(!"Other Guy" %in% A$player_name, "other Out players still dropped")
writeLines('{}', tf); A2 <- apply_inactives(copy(P), "nfl")
ok(!"Jacory Croskey-Merritt" %in% A2$player_name, "without the override he is dropped as before")
injury_report <- injury_report_orig; dfs_path <- dfs_path_orig
cat(sprintf("\n%d passed, %d failed\n", n_ok, n_bad)); if (n_bad) quit(status = 1) else cat("ALL PASS\n")
