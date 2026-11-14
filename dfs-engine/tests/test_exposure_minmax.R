# Unit tests: per-player MIN and MAX exposures in build_gpp20 (spine/R/portfolio.R), on a
# synthetic 6-team NFL classic slate WITH the NFL stacking rules active.
# Run: Rscript tests/test_exposure_minmax.R  (from dfs-engine/)
suppressPackageStartupMessages(library(data.table))
source("bootstrap.R"); dfs_load_spine(); dfs_load_sport("nfl")
msg <- function(...) invisible(NULL)
n_ok <- 0L; n_bad <- 0L
ok <- function(cond, label) { if (isTRUE(cond)) { n_ok <<- n_ok + 1L; cat("ok   -", label, "\n") } else { n_bad <<- n_bad + 1L; cat("FAIL -", label, "\n") } }

set.seed(42)
teams <- c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF"); games <- c("AAA@BBB", "CCC@DDD", "EEE@FFF")
gid <- setNames(rep(games, each = 2), teams)
pos_n <- c(QB = 1, RB = 3, WR = 5, TE = 2, DST = 1)
pool <- rbindlist(lapply(teams, function(t) rbindlist(lapply(names(pos_n), function(p) data.table(
  team = t, position = p, k = seq_len(pos_n[[p]]))))))
pool[, `:=`(game_id = gid[team], player_id = paste(team, position, k), player_name = paste(team, position, k))]
base <- c(QB = 18, RB = 12, WR = 11, TE = 7, DST = 7)
pool[, proj := pmax(1, base[position] / k^0.7 + rnorm(.N, 0, 1.5))]
pool[, salary := as.integer(pmax(2500, pmin(9000, round(proj * 400 / 100) * 100 + 1500)))]
pool[, `:=`(sim_sd = pmax(0.42 * proj, 4), ceil = proj * 1.4, floor = proj * 0.4, p_zero = 0.03, own = 0.08)]
rr <- get_sport("nfl")$roster_rules
cands <- make_candidates(pool, rr, 300L)
res <- data.table(idx = lapply(cands, `[[`, "idx"))
res[, `:=`(proj = vapply(idx, function(i) sum(pool$proj[i]), 0), avg_own = 0.08)]

# a mid-projection WR nobody would pick much, forced to >= 60%; the top RB capped at 10%
target <- pool[position == "WR"][order(-proj)][6, player_name]
toprb  <- pool[position == "RB"][order(-proj)][1, player_name]
picks0 <- build_gpp20(res, n = 20L, exp_cap = 0.5, pool = pool, rr = rr)
cnt <- function(pk, nm) sum(vapply(pk, function(p) nm %in% pool$player_name[p$idx[[1]]], TRUE))
cat(sprintf("baseline: %s in %d/20, %s in %d/20\n", target, cnt(picks0, target), toprb, cnt(picks0, toprb)))
picks <- build_gpp20(res, n = 20L, exp_cap = 0.5, pool = pool, rr = rr,
                     caps = setNames(c(0.10, 0.80), c(toprb, target)), min_caps = setNames(0.60, target))
ok(length(picks) == 20L, "still builds the full 20 lineups")
ok(cnt(picks, target) >= 12L, sprintf("MIN honoured: %s in %d/20 (>= 12)", target, cnt(picks, target)))
ok(cnt(picks, target) <= 16L, "MIN does not break its own MAX (<= 16)")
ok(cnt(picks, toprb) <= 2L, sprintf("MAX honoured: %s in %d/20 (<= 2)", toprb, cnt(picks, toprb)))
others <- table(unlist(lapply(picks, function(p) p$idx[[1]])))
others <- others[!pool$player_name[as.integer(names(others))] %in% c(target, toprb)]
ok(max(others) <= 10L, sprintf("everyone else under the 50%% cap (max %d/20)", max(others)))
naked <- vapply(picks, function(p) { i <- p$idx[[1]]; q <- i[pool$position[i] == "QB"]
  sum(pool$position[i] %in% c("WR", "TE") & pool$team[i] == pool$team[q]) == 0 }, TRUE)
ok(!any(naked), "stacking rules still hold on every lineup (no naked QB)")
# min larger than any feasible use -> reported, not a crash
picks2 <- build_gpp20(res, n = 20L, exp_cap = 0.5, pool = pool, rr = rr, min_caps = setNames(1.0, target),
                      caps = setNames(0.5, target))
ok(length(picks2) == 20L && cnt(picks2, target) == 20L, "min of 100% raises that player's max and fills all 20")
# JSON loader: number = max, object = min/max
tf <- tempfile(fileext = ".json"); writeLines('{"nfl": {"A B": 0.3, "C D": {"min": 0.4, "max": 0.8}, "E F": {"min": 0.2}}}', tf)
dfs_path_orig <- dfs_path; dfs_path <- function(...) tf
mx <- load_exposure_overrides("nfl"); mn <- load_exposure_minimums("nfl"); dfs_path <- dfs_path_orig
ok(isTRUE(all.equal(unname(mx[c("A B", "C D")]), c(0.3, 0.8))) && !"E F" %in% names(mx), "loader: max from number or {max}")
ok(isTRUE(all.equal(unname(mn[c("C D", "E F")]), c(0.4, 0.2))) && !"A B" %in% names(mn), "loader: min from {min}")
cat(sprintf("\n%d passed, %d failed\n", n_ok, n_bad)); if (n_bad) quit(status = 1) else cat("ALL PASS\n")
