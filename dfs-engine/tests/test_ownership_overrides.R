# Unit tests: ownership overrides (spine/R/ownership_model.R apply_ownership_overrides).
# Run: Rscript tests/test_ownership_overrides.R  (from dfs-engine/)
suppressPackageStartupMessages(library(data.table))
source("bootstrap.R"); dfs_load_spine()
msg <- function(...) invisible(NULL)
n_ok <- 0L; n_bad <- 0L
ok <- function(cond, label) { if (isTRUE(cond)) { n_ok <<- n_ok + 1L; cat("ok   -", label, "\n") } else { n_bad <<- n_bad + 1L; cat("FAIL -", label, "\n") } }
pool <- data.table(player_name = c("Emmett Johnson", "Back Two", "Back Three", "Back Four", "Some Wr"),
                   position = c("RB", "RB", "RB", "RB", "WR"), own = c(0.06, 0.90, 0.80, 0.74, 0.30))   # RB total ~2.5, as on DK
tf <- tempfile(fileext = ".json"); dfs_path_orig <- dfs_path; dfs_path <- function(...) tf
today <- format(Sys.time(), tz = "America/New_York", "%Y-%m-%d")
writeLines(sprintf('{"nfl_own": {"Emmett Johnson": {"own": 0.55, "until": "%s"}}}', today), tf)
P <- apply_ownership_overrides(copy(pool), "nfl")
ok(abs(P[player_name == "Emmett Johnson", own] - 0.55) < 1e-9, "override sets Johnson to 55%")
ok(abs(sum(P[position == "RB", own]) - sum(pool[position == "RB", own])) < 1e-9, "RB total ownership unchanged (others rescaled)")
ok(all(P[position == "RB" & player_name != "Emmett Johnson", own] < pool[position == "RB" & player_name != "Emmett Johnson", own]), "other RBs scaled down")
ok(P[position == "WR", own] == 0.30, "other positions untouched")
writeLines('{"nfl_own": {"Emmett Johnson": {"own": 0.55, "until": "2020-01-01"}}}', tf)
ok(identical(apply_ownership_overrides(copy(pool), "nfl")$own, pool$own), "expired override is ignored")
writeLines('{"nfl_own": {"Johnson, Emmett": 0.40}}', tf)
ok(abs(apply_ownership_overrides(copy(pool), "nfl")[1, own] - 0.40) < 1e-9, "plain number + 'Last, First' name both work")
# "eat the good chalk": an active >= 30% ownership override implies a minimum exposure
writeLines(sprintf('{"nfl_own": {"Emmett Johnson": {"own": 0.55, "until": "%s"}, "Faded Guy": {"own": 0.40, "fade": true}, "Small Guy": 0.20},
                    "nfl": {"Other Guy": {"min": 0.25}}}', today), tf)
mn <- load_exposure_minimums("nfl")
ok(abs(mn[["Emmett Johnson"]] - 0.44) < 1e-9, "55% chalk override -> min exposure 44% (80% of field)")
ok(!"Faded Guy" %in% names(mn) && !"Small Guy" %in% names(mn), "fade:true and <30% overrides add no minimum")
ok(abs(mn[["Other Guy"]] - 0.25) < 1e-9, "explicit {min} still honoured")
dfs_path <- dfs_path_orig
cat(sprintf("\n%d passed, %d failed\n", n_ok, n_bad)); if (n_bad) quit(status = 1) else cat("ALL PASS\n")
