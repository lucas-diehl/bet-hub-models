# Unit tests for the 2026-11-14 MobyWhaler-derived rules: salary floor (M3), QB+2 cap (M9),
# absolute QB/DST conviction ceilings (R1), exclude via max 0 (M8), stack-only players (M6),
# user-listed naked QBs (M7), sim-optimal candidates (R2) and ceiling rate (M1).
# Run: Rscript tests/test_nfl_rules_round3.R  (from dfs-engine/)
suppressPackageStartupMessages(library(data.table))
source("bootstrap.R"); dfs_load_spine(); dfs_load_sport("nfl")
msg <- function(...) invisible(NULL)
n_ok <- 0L; n_bad <- 0L
ok <- function(cond, label) { if (isTRUE(cond)) { n_ok <<- n_ok + 1L; cat("ok   -", label, "\n") } else { n_bad <<- n_bad + 1L; cat("FAIL -", label, "\n") } }

set.seed(7)
teams <- c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF"); gid <- setNames(rep(c("AAA@BBB", "CCC@DDD", "EEE@FFF"), each = 2), teams)
pos_n <- c(QB = 1, RB = 3, WR = 5, TE = 2, DST = 1)
pool <- rbindlist(lapply(teams, function(t) rbindlist(lapply(names(pos_n), function(p) data.table(team = t, position = p, k = seq_len(pos_n[[p]]))))))
pool[, `:=`(game_id = gid[team], player_id = paste(team, position, k), player_name = paste(team, position, k))]
pool[, norm := norm_name(player_name)]
base <- c(QB = 18, RB = 12, WR = 12, TE = 7, DST = 7)
pool[, proj := pmax(1, base[position] / k^0.6 + rnorm(.N, 0, 1.5))]
pool[, salary := as.integer(pmax(3000, pmin(9000, round(proj * 420 / 100) * 100 + 1200)))]
pool[, `:=`(sim_sd = pmax(0.42 * proj, 4), ceil = proj * 1.4, floor = proj * 0.4, p_zero = 0.03, own = 0.08)]
rr <- get_sport("nfl")$roster_rules

# M3 salary floor
ok(identical(rr$floor, 49000L), "NFL DK roster rules carry a $49,000 salary floor")
cands <- make_candidates(pool, rr, 150L)
sal <- vapply(cands, function(c) sum(pool$salary[c$idx]), 0)
ok(length(cands) > 50 && all(sal >= 49000 & sal <= 50000), sprintf("every candidate uses $49,000-$50,000 (min %d)", min(sal)))

# M9 QB+2 cap: a pool where stacking 3 pass-catchers is very attractive
pool3 <- copy(pool)[team == "AAA" & position %in% c("WR", "TE"), proj := proj + 15]
c3 <- make_candidates(pool3, rr, 120L)
mates <- vapply(c3, function(c) { i <- c$idx; q <- i[pool3$position[i] == "QB"]
  sum(pool3$position[i] %in% c("WR", "TE") & pool3$team[i] == pool3$team[q]) }, 0)
ok(max(mates) <= 2, sprintf("QB+2 cap holds even when a 3-man stack is juicy (max %d)", max(mates)))

# R1 absolute ceilings
nat <- rep(0.5, nrow(pool))
cc <- conviction_caps(pool, nat, 0.5)
ok(max(cc[pool$position == "QB"]) <= 0.25 + 1e-9 && max(cc[pool$position == "DST"]) <= 0.20 + 1e-9,
   "QB ceiling 25% and DST ceiling 20% hold at a 50% base cap")
ok(max(cc[pool$position %in% c("RB", "WR")]) <= 0.70 + 1e-9, "skill ceiling 70%")

# M8 exclude (max 0) + M6 stack-only + M7 naked-ok, via a temporary overrides file
tf <- tempfile(fileext = ".json")
writeLines(sprintf('{"nfl": {"%s": 0}, "nfl_stack_only": ["%s"], "nfl_naked_ok": ["%s"]}',
                   "AAA WR 1", "BBB WR 1", "CCC QB 1"), tf)
dfs_path_orig <- dfs_path; dfs_path <- function(...) tf
p8 <- drop_excluded_players(copy(pool), "nfl")
ok(!"AAA WR 1" %in% p8$player_name && nrow(p8) == nrow(pool) - 1, "max-0 override removes the player from the pool")
c6 <- make_candidates(pool, rr, 200L)
with_bbbwr <- Filter(function(c) "BBB WR 1" %in% pool$player_name[c$idx], c6)
ok(length(with_bbbwr) > 0 && all(vapply(with_bbbwr, function(c) "BBB QB 1" %in% pool$player_name[c$idx], TRUE)),
   sprintf("stack-only WR appears only alongside his own QB (%d lineups checked)", length(with_bbbwr)))
H <- nfl_stack_hard(pool)
qb_c <- which(pool$player_name == "CCC QB 1"); qb_a <- which(pool$player_name == "AAA QB 1")
rule1_rows <- function(q) sum(H$mat[, q] == -1 & H$dir == ">=")
ok(rule1_rows(qb_c) == 0 && rule1_rows(qb_a) >= 1, "naked-ok QB has no hard stack requirement; others still do")
dfs_path <- dfs_path_orig

# R2 sim-optimal candidates + M1 ceiling rate
sim <- slate_sim(pool, get_loadings(pool, "nfl"), team_loadings = get_team_loadings(pool, "nfl"), n_sims = 500L, seed = 3)
cs <- make_candidates_sim(pool, sim, rr, 120L, seed = 3)
keys <- vapply(cs, function(c) paste(sort(c$idx), collapse = "-"), "")
ok(length(cs) >= 100 && !anyDuplicated(keys), sprintf("sim-optimal generator: %d distinct lineups from 120 draws", length(cs)))
legal <- vapply(cs, function(c) lineup_legal(c$idx, build_constraints(pool, rr)), TRUE)
ok(all(legal), "every sim-optimal lineup satisfies all roster + stacking + floor rules")
cr <- cand_ceiling_rate(lapply(cs, `[[`, "idx"), sim)
ok(all(is.finite(cr)) && all(cr >= 0 & cr <= 1) && abs(mean(cr) - 0.02) < 0.01, sprintf("ceiling rate in [0,1], mean %.3f ~ 2%%", mean(cr)))

cat(sprintf("\n%d passed, %d failed\n", n_ok, n_bad)); if (n_bad) quit(status = 1) else cat("ALL PASS\n")
