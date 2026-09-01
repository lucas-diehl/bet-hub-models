#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — regression tests: DK scoring + roster legality (WNBA / tennis)
#   Rscript tests/test_scoring_roster.R
# Hand-calculated examples pin the DK point values and bonus logic so a stray edit
# to a scoring weight or roster rule fails loudly. Exits non-zero on any failure.
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine(); dfs_load_sport("wnba"); dfs_load_sport("tennis"); dfs_load_sport("nfl")
suppressPackageStartupMessages(library(data.table))

.fails <- 0L; .tests <- 0L
ok <- function(cond, label) {
  .tests <<- .tests + 1L
  if (isTRUE(cond)) cat(sprintf("  PASS  %s\n", label))
  else { .fails <<- .fails + 1L; cat(sprintf("  FAIL  %s\n", label)) }
}
eq <- function(actual, expected, label, tol = 1e-6) ok(is.finite(actual) && abs(actual - expected) < tol,
  sprintf("%s (got %s, want %s)", label, round(actual, 4), expected))

cat("== WNBA DK scoring ==\n")
# base = pts + .5*3pm + 1.25*reb + 1.5*ast + 2*stl + 2*blk - .5*tov; DD +1.5, TD +3
b1 <- data.frame(pts=20, fg3m=2, reb=10, ast=5, stl=2, blk=1, tov=3)   # double-double (pts,reb)
eq(wnba_dk_scoring(b1), 47.0, "double-double = 47.0")
b2 <- data.frame(pts=25, fg3m=3, reb=12, ast=11, stl=1, blk=0, tov=4)  # triple-double (pts,reb,ast)
eq(wnba_dk_scoring(b2), 61.0, "triple-double = 61.0 (+3 not +4.5)")
b3 <- data.frame(pts=8, fg3m=1, reb=4, ast=2, stl=0, blk=0, tov=1)     # no bonus
eq(wnba_dk_scoring(b3), 8 + 0.5 + 5 + 3 - 0.5, "no-bonus line = 16.0")

cat("== Tennis DK scoring (best-of-3) ==\n")
# 30 + 2.5*gw - 2*gl + 6*sw - 3*sl + 6*won + .4*ace - 1*df + .75*brk + 2.5*clean + 2.5*no_df + 6*straight
wbox <- data.frame(match_played=1, games_won=12, games_lost=8, sets_won=2, sets_lost=0,
                   match_won=1, aces=5, dfs=1, breaks=3, clean_set=0, no_df=0, straight_sets=1)
eq(tennis_dk_scoring(wbox), 71.25, "winner 6-4 6-4 (5 ace,1 df,3 brk) = 71.25")
lbox <- data.frame(match_played=1, games_won=8, games_lost=12, sets_won=0, sets_lost=2,
                   match_won=0, aces=2, dfs=0, breaks=0, clean_set=0, no_df=1, straight_sets=0)
eq(tennis_dk_scoring(lbox), 23.3, "loser 4-6 4-6 (2 ace,0 df,no_df bonus) = 23.3")

cat("== Tennis score parsing + ManTennisData conversion ==\n")
p <- parse_tennis_score("6-0 6-0")
ok(!is.null(p) && p$g_for==12 && p$g_against==0 && p$s_for==2 && p$bag_for==2, "parse 6-0 6-0 -> double bagel")
ok(.mtd_convert_score("60 60") == "6-0 6-0", "convert '60 60' -> '6-0 6-0'")
ok(.mtd_convert_score("76(4) 36 63") == "7-6(4) 3-6 6-3", "convert tiebreak+3set")
ok(is.na(.mtd_convert_score("")), "convert empty (walkover) -> NA")

cat("== Roster legality (ILP enforces DK rules) ==\n")
rr <- get_sport("wnba")$roster_rules
# pool: 4 G + 4 F, all $8000 -> a legal 6-man lineup costs 48000 (<= 50000, >= floor 49000? no)
set.seed(1)
pool <- data.table(
  player_id = 1:8,
  player_name = paste0("P", 1:8),
  position = c(rep("G",4), rep("F",4)),
  salary = rep(8000L, 8),
  proj = c(40, 38, 36, 34, 39, 37, 35, 33))
# floor 49000 makes 6*8000=48000 infeasible -> drop floor for this legality fixture
rr_nofloor <- rr; rr_nofloor$floor <- NULL
idx <- ilp_solve(pool$proj, pool, rr_nofloor)
ok(!is.null(idx), "feasible pool solves")
ok(length(idx) == rr$n, sprintf("lineup has exactly %d players", rr$n))
ok(sum(pool$salary[idx]) <= rr$cap, "lineup under salary cap")
ok(sum(pool$position[idx]=="G") >= rr$slots[["G"]], "meets G minimum (>=2)")
ok(sum(pool$position[idx]=="F") >= rr$slots[["F"]], "meets F minimum (>=2)")
# negative: cap too low for any 6 players -> infeasible
rr_tight <- rr_nofloor; rr_tight$cap <- 1000L
ok(is.null(ilp_solve(pool$proj, pool, rr_tight)), "impossible cap -> infeasible (NULL)")
# negative: not enough forwards to fill F slots
pool_g <- copy(pool); pool_g[, position := "G"]
ok(is.null(ilp_solve(pool_g$proj, pool_g, rr_nofloor)), "all-guard pool can't fill F slots -> NULL")

cat("== L3 data-quality gate (gates must not flip on thin/synthetic data) ==\n")
mk_H <- function(n_slates, own_na = FALSE, with_prov = TRUE) {
  rbindlist(lapply(seq_len(n_slates), function(s) {
    np <- rr$n * 4
    dt <- data.table(slate_id = paste0("s", s), year = 2025L, player_id = seq_len(np),
                     salary = rep(8000L, np), position = rep(c("G","F"), each = np/2),
                     proj = runif(np, 10, 40), sim_sd = 8, ceil = runif(np, 20, 60),
                     own_actual = if (own_na) NA_real_ else runif(np, 0.02, 0.4),
                     total_pts = runif(np, 5, 50))
    if (with_prov) dt[, `:=`(own_source = "actual", payout_source = "table")]
    dt
  }))
}
dq_thin <- validate_data_quality(mk_H(5), rr, min_slates = 30L)
ok(!dq_thin$pass && any(grepl("slates", dq_thin$reasons)), "5 slates -> refused (insufficient_data)")
dq_noprov <- validate_data_quality(mk_H(30, with_prov = FALSE), rr, min_slates = 30L)
ok(!dq_noprov$pass && any(grepl("provenance|payout", dq_noprov$reasons)), "no provenance -> refused")
dq_na <- validate_data_quality(mk_H(30, own_na = TRUE), rr, min_slates = 30L)
ok(!dq_na$pass && any(grepl("ownership", dq_na$reasons)), "NA actual ownership -> refused")
dq_ok <- validate_data_quality(mk_H(30), rr, min_slates = 30L)
ok(dq_ok$pass, "30 complete slates w/ real own + payout table -> passes")
# strict_own path never fabricates: a slate with missing ownership is skipped
sl_bad <- mk_H(1)[, own_actual := NA_real_]
ok(is.null(grade_history_slate(sl_bad, rr, 6, make_gpp(), make_double_up())),
   "grade_history_slate skips slate with missing ownership (no 0.1 fallback)")

cat("== NFL DK scoring (foundation) ==\n")
qb <- data.frame(pass_yds=300, pass_td=3, interceptions=1, rush_yds=20)      # 12+12-1+2 +3(300)
eq(nfl_dk_scoring(qb), 28.0, "QB 300yd/3TD/1INT/20rush = 28.0")
wr <- data.frame(receptions=8, rec_yds=120, rec_td=1)                        # 8+12+6 +3(100)
eq(nfl_dk_scoring(wr), 29.0, "WR 8rec/120yd/1TD = 29.0")
rb <- data.frame(rush_yds=100, rush_td=1, receptions=4, rec_yds=30, fumbles_lost=1)  # 10+6+3+4+3-1
eq(nfl_dk_scoring(rb), 25.0, "RB 100yd/1TD/4-30rec/1fum = 25.0")
dst <- data.frame(sacks=3, def_int=1, fumble_rec=1, def_td=1, points_allowed=10)     # 3+2+2+6 +4(7-13)
eq(nfl_dk_scoring(dst), 17.0, "DST 3sk/1int/1fr/1td/10pa = 17.0")
rr_nfl <- get_sport("nfl")$roster_rules
ok(rr_nfl$n == 9L && rr_nfl$cap == 50000L && sum(rr_nfl$slots) + rr_nfl$flex$count == 9L,
   "NFL roster is a valid 9-man $50k template")

cat("== NFL two-factor correlation (stacking) ==\n")
np <- data.table(player_id = 1:4, player_name = paste0("p", 1:4), position = c("QB","WR","RB","WR"),
                 team = c("KC","KC","KC","BUF"), game_id = "KC@BUF", salary = 8000L,
                 proj = 15, sim_sd = 8, p_zero = 0)
tl2 <- get_team_loadings(np, "nfl")
ok(tl2[1] > 0 && tl2[3] < 0, "NFL team-load: QB stack positive, RB game-script negative")
Cm <- cor(t(slate_sim(np, get_loadings(np, "nfl"), team_loadings = tl2, n_sims = 20000L, seed = 3)$scores))
ok(Cm[1, 2] > Cm[1, 4] + 0.1, "QB-WR same-team stack correlates MORE than bring-back")
ok(Cm[1, 3] < Cm[1, 2] - 0.1, "RB decoupled from its own QB (game script)")
ok(all(get_team_loadings(data.table(player_id = 1:2, position = "G", team = "X", game_id = "Y"), "wnba") == 0),
   "non-stacking sports keep single-factor (team_load = 0)")

cat("== Realistic field simulation (competitive optimizer field) ==\n")
set.seed(7)
np <- 40L
fp <- data.table(player_id = 1:np, player_name = paste0("P", 1:np), position = "G",
                 salary = as.integer(round(runif(np, 4000, 11000), -2)), proj = runif(np, 10, 40),
                 sim_sd = runif(np, 6, 12), p_zero = 0.02)
fp[, own := { r <- frank(proj) / np; pmax(0.01, pmin(0.6, 0.02 + 0.5 * r^2)) }]   # skewed chalk
frr <- list(n = 6L, cap = 50000L, floor = NULL, slots = NULL)
fld_new <- simulate_field(fp, frr, field_n = 1500L, own = fp$own)
fld_hi  <- simulate_field(fp, frr, field_n = 1500L, own = fp$own, tiers = list(list(w = 1, noise = 0.6, pull = 0)))
ok(length(fld_new$idx) > 1400, "field builds ~field_n valid lineups")
ok(all(vapply(fld_new$idx, function(ix) length(ix) == 6L && sum(fp$salary[ix]) <= frr$cap, logical(1))),
   "all field lineups valid (size + cap)")
ok(cor(field_ownership(fld_new, np), fp$own) > 0.5, "field ownership tracks projected ownership (corr>0.5)")
# the field must reach near-optimal (sharks compete): best field proj ~ the true optimum
opt_proj <- { o <- order(-fp$proj); s <- 0L; pick <- integer(0)
  for (i in o) if (s + fp$salary[i] <= frr$cap && length(pick) < 6) { pick <- c(pick, i); s <- s + fp$salary[i] }
  sum(fp$proj[pick]) }
ok(max(vapply(fld_new$idx, function(ix) sum(fp$proj[ix]), 0)) >= 0.95 * opt_proj,
   "competitive field reaches >=95% of the optimal projection")
dr <- function(fl) { k <- vapply(fl$idx, paste, character(1), collapse = "-"); mean(table(k)[k] > 1) }
ok(dr(fld_new) >= dr(fld_hi), "competitive field duplicates >= a high-noise field")
# EV-REALISM regression (the fix): the chalk-max lineup is NOT absurdly +EV in a GPP —
# it is heavily duplicated, so it should grade near-breakeven/negative, never the old +900%.
sim7  <- slate_sim(fp, n_sims = 2000L, seed = 7)
chalk <- sort(order(-fp$proj / pmax(fp$salary/1000, 1))[1:6])
gc    <- grade_candidates(list(list(idx = chalk, family = "c")), sim7, fld_new)
ok(is.finite(gc$gpp_ev[1]) && gc$gpp_ev[1] < 2,
   "chalk-max GPP EV is realistic (<+200%, not the old +900% fiction)")

cat(sprintf("\n%d/%d checks passed.\n", .tests - .fails, .tests))
if (.fails > 0L) { cat("TESTS FAILED\n"); quit(status = 1) } else cat("ALL TESTS PASSED\n")
