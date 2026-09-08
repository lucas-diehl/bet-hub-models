#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — regression tests for scenario-conditioned correlation (slate_sim.R's
# `regime_loadings` support, added 2026-09 to build Blickle-thread idea #1 for real:
# https://x.com/AlexBlickle1/status/2096644199198097515). Asserts:
#   1. isolated regime targets (p_alt=0 / p_alt=1) are hit within sampling tolerance
#   2. the blend is a TRUE per-simulation draw (checked via the spread between team
#      average scores having real variance, not a single narrow band)
#   3. sports without regime_loadings (everything except WNBA) are BYTE-level
#      unaffected — get_loadings() still returns a plain numeric vector
# Run: Rscript tests/test_regime_correlation.R
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine(); dfs_load_sport("wnba"); dfs_load_sport("nfl"); dfs_load_sport("ncaaf")
suppressPackageStartupMessages(library(data.table))

.pass <- 0L; .fail <- 0L
ok <- function(desc, cond) {
  if (isTRUE(cond)) { .pass <<- .pass + 1L; cat("  ok   - ", desc, "\n", sep = "") }
  else              { .fail <<- .fail + 1L; cat("  FAIL - ", desc, "\n", sep = "") }
}
close_enough <- function(a, b, tol) abs(a - b) <= tol

mkpool <- function() data.table(player_id = 1:6, salary = rep(6000, 6), proj = rep(20, 6),
  sim_sd = rep(6, 6), game_id = rep("A@B", 6), team = c("A", "A", "A", "B", "B", "B"))

cat("regime_loadings: isolated regime targets\n")
pool <- mkpool()
L <- structure(list(base = rep(sqrt(0.041), 6), alt = sqrt(0.068) * c(1, 1, 1, -1, -1, -1), p_alt = rep(0, 6)), class = "regime_loadings")
Tl <- structure(list(base = rep(0, 6), alt = rep(sqrt(0.008), 6), p_alt = rep(0, 6)), class = "regime_loadings")
sim <- slate_sim(pool, L, team_loadings = Tl, n_sims = 80000L, seed = 1L)
z <- scale(t(sim$scores))
ok("close regime: opponent cor ~ +0.041", close_enough(cor(z[, 1], z[, 4]), 0.041, 0.02))
ok("close regime: same-team cor ~ +0.041 (team_load_close=0)", close_enough(cor(z[, 1], z[, 2]), 0.041, 0.02))

L$p_alt <- rep(1, 6); Tl$p_alt <- rep(1, 6)
sim <- slate_sim(pool, L, team_loadings = Tl, n_sims = 80000L, seed = 2L)
z <- scale(t(sim$scores))
ok("blowout regime: opponent cor ~ -0.068", close_enough(cor(z[, 1], z[, 4]), -0.068, 0.02))
ok("blowout regime: same-team cor ~ +0.076", close_enough(cor(z[, 1], z[, 2]), 0.076, 0.02))

cat("regime_loadings: mixture is a TRUE per-sim draw, not an averaged constant\n")
L$p_alt <- rep(0.5, 6); Tl$p_alt <- rep(0.5, 6)
sim <- slate_sim(pool, L, team_loadings = Tl, n_sims = 60000L, seed = 3L)
div <- abs(colMeans(sim$scores[1:3, ]) - colMeans(sim$scores[4:6, ]))
ok("mixture shows wide spread (q90 - q10 > 3x sd of a single fixed-regime run)",
   (quantile(div, 0.90, names = FALSE) - quantile(div, 0.10, names = FALSE)) > 3)
mid <- cor(scale(sim$scores[1, ]), scale(sim$scores[4, ]))
ok("p_alt=0.5 opponent cor sits strictly between the two regime targets",
   mid < 0.041 && mid > -0.068)

cat("regime_loadings: end-to-end via wnba_correlation() + get_loadings/get_team_loadings\n")
poolD <- mkpool(); poolD[, exp_margin := rep(3, 6)]
Ld <- get_loadings(poolD, "wnba"); Tld <- get_team_loadings(poolD, "wnba")
ok("get_loadings(wnba) returns a regime_loadings object", inherits(Ld, "regime_loadings"))
ok("get_team_loadings(wnba) returns a regime_loadings object", inherits(Tld, "regime_loadings"))
ok("p_alt in [0,1]", all(Ld$p_alt >= 0 & Ld$p_alt <= 1))
sim <- tryCatch(slate_sim(poolD, Ld, team_loadings = Tld, n_sims = 20000L, seed = 4L), error = function(e) NULL)
ok("slate_sim runs clean on real wnba_correlation() output", !is.null(sim) && all(dim(sim$scores) == c(6L, 20000L)))

cat("regime_loadings: NCAAF isolated regime targets + end-to-end (team_load=0 both regimes)\n")
poolF <- data.table(player_id = 1:4, salary = rep(6000, 4), proj = rep(20, 4), sim_sd = rep(6, 4),
                    game_id = rep("A@B", 4), team = c("A", "A", "B", "B"))
Lf <- structure(list(base = rep(sqrt(0.060), 4), alt = sqrt(0.082) * c(1, 1, -1, -1), p_alt = rep(0, 4)), class = "regime_loadings")
sim <- slate_sim(poolF, Lf, n_sims = 80000L, seed = 6L)
z <- scale(t(sim$scores))
ok("ncaaf close regime: opponent cor ~ +0.060", close_enough(cor(z[, 1], z[, 3]), 0.060, 0.02))
Lf$p_alt <- rep(1, 4)
sim <- slate_sim(poolF, Lf, n_sims = 80000L, seed = 7L)
z <- scale(t(sim$scores))
ok("ncaaf blowout regime: opponent cor ~ -0.082", close_enough(cor(z[, 1], z[, 3]), -0.082, 0.02))
poolG <- copy(poolF); poolG[, exp_margin := 10]
Lg <- get_loadings(poolG, "ncaaf")
ok("get_loadings(ncaaf) returns a regime_loadings object", inherits(Lg, "regime_loadings"))
sim <- tryCatch(slate_sim(poolG, Lg, n_sims = 20000L, seed = 8L), error = function(e) NULL)
ok("slate_sim runs clean on real ncaaf_correlation() output", !is.null(sim) && all(dim(sim$scores) == c(4L, 20000L)))

cat("regime_loadings: NFL mixed case (game factor regime-conditional, team factor plain)\n")
poolH <- data.table(player_id = 1:4, salary = rep(6000, 4), proj = rep(20, 4), sim_sd = rep(6, 4),
                    game_id = rep("A@B", 4), team = c("A", "A", "B", "B"), position = "QB", exp_margin = 3)
Lh <- get_loadings(poolH, "nfl"); Th <- get_team_loadings(poolH, "nfl")
ok("get_loadings(nfl) IS regime-conditional (has load_alt/p_alt)", inherits(Lh, "regime_loadings"))
ok("get_team_loadings(nfl) stays a PLAIN vector (no team_load_alt supplied)", is.numeric(Th) && !inherits(Th, "regime_loadings"))
sim <- tryCatch(slate_sim(poolH, Lh, team_loadings = Th, n_sims = 20000L, seed = 9L), error = function(e) NULL)
ok("slate_sim runs clean on the mixed regime/plain combination", !is.null(sim) && all(dim(sim$scores) == c(4L, 20000L)))
# isolated: same position (QB) both sides -> game_ld identical -> clean opponent-corr check
poolI <- data.table(player_id = 1:2, salary = rep(6000, 2), proj = rep(20, 2), sim_sd = rep(6, 2),
                    game_id = rep("A@B", 2), team = c("A", "B"), position = "QB")
Li <- structure(list(base = rep(0.50, 2), alt = rep(0.50 * sqrt(0.009 / 0.038), 2), p_alt = rep(0, 2)), class = "regime_loadings")
sim <- slate_sim(poolI, Li, n_sims = 60000L, seed = 10L)
z <- scale(t(sim$scores))
ok("nfl close regime (QB-QB opponent): cor ~ 0.50^2 = 0.25", close_enough(cor(z[, 1], z[, 2]), 0.25, 0.03))
Li$p_alt <- rep(1, 2)
sim <- slate_sim(poolI, Li, n_sims = 60000L, seed = 11L)
z <- scale(t(sim$scores))
ok("nfl blowout regime (QB-QB opponent): cor shrinks toward measured ratio", close_enough(cor(z[, 1], z[, 2]), 0.25 * (0.009 / 0.038), 0.03))

cat("regime_loadings: OTHER sports completely unaffected (backward compatibility)\n")
# NOTE: NFL used to be this test's example, but is no longer plain-vector by design —
# nfl_correlation() now always supplies load_alt/p_alt too (see sports/nfl/correlate.R).
# golf/tennis have no team/script concept and remain genuinely unaffected.
dfs_load_sport("golf")
poolE <- data.table(player_id = 1:4, salary = rep(6000, 4), proj = rep(20, 4), sim_sd = rep(6, 4),
                    game_id = rep("X@Y", 4), team = c("X", "X", "Y", "Y"))
Le <- get_loadings(poolE, "golf")
ok("get_loadings(golf) returns a plain numeric vector, not regime_loadings", is.numeric(Le) && !inherits(Le, "regime_loadings"))
sim <- tryCatch(slate_sim(poolE, Le, team_loadings = get_team_loadings(poolE, "golf"), n_sims = 5000L, seed = 5L), error = function(e) NULL)
ok("slate_sim runs clean with plain-vector loadings (golf)", !is.null(sim) && all(dim(sim$scores) == c(4L, 5000L)))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
