#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — does realized CFB correlation depend on ACTUAL game script?
# NCAAF analog of tests/validate_game_script_correlation.R (WNBA). Uses team-level
# `points` already sitting in the cached raw CFBD JSON (data/raw/ncaaf_src/*.json,
# pulled for the CFB DFS build) — no new API calls.
#
# FINDING (2026-09, 2024-2025 CFBD data, 1,172 games with usable rolling residuals):
#   opponent pairs:  close (<=7 margin)  cor ~ +0.060  |  blowout (>=28) cor ~ -0.082
#   same-team pairs: close                cor ~ +0.036  |  blowout           cor ~ +0.059
# The correlation SIGN FLIPS for opponent pairs, same as WNBA. UNLIKE WNBA, same-team
# correlation sits BELOW the opponent-level magnitude in BOTH regimes here — real CFB
# data does not support a positive same-team increment, so sports/ncaaf/correlate.R
# uses team_load=0 in both regimes (a single sign-flipping game factor), simpler than
# WNBA's model, chosen because that's what THIS sport's data actually supports.
#
# Run: Rscript tests/validate_ncaaf_game_script_correlation.R
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine()
if (!exists("cfb_game_scores")) source(dfs_path("sports", "ncaaf", "ingest.R"))
suppressPackageStartupMessages(library(data.table))

wk_path <- dfs_path("data", "raw", "ncaaf_weekly.rds")
if (!file.exists(wk_path)) { cat("No cached ncaaf_weekly.rds -> run cfb_ingest() first.\n"); quit(status = 0) }
G <- cfb_game_scores()
if (is.null(G) || !nrow(G)) { cat("No cached raw CFBD week JSONs (data/raw/ncaaf_src) -> nothing to measure.\n"); quit(status = 0) }
G[, margin := abs(points - opp_points)]

D <- as.data.table(readRDS(wk_path))
setorder(D, athlete_id, season, wk)
D[, `:=`(m1 = shift(frollmean(dk_pts, 8)), m2 = shift(frollmean(dk_pts^2, 8))), by = athlete_id]
D[, `:=`(mu = m1, sd = sqrt(pmax(m2 - m1^2, 0)))]
D <- D[is.finite(mu) & is.finite(sd) & sd > 1]
D[, resid := (dk_pts - mu) / sd]
D <- merge(D, G[, .(game_id, team, margin)], by = c("game_id", "team"))

pairs <- D[D, on = "game_id", allow.cartesian = TRUE][athlete_id < i.athlete_id]
pairs[, rel := fifelse(team == i.team, "same_team", "opponent")]
pairs[, script := fifelse(margin <= 7, "close", fifelse(margin >= 28, "blowout", "mid"))]

cat("=== REALIZED same-game correlation split by ACTUAL game script ===\n")
print(pairs[script != "mid", .(n_pairs = .N, realized_cor = round(cor(resid, i.resid), 3)),
            by = .(rel, script)][order(rel, script)])
cat(sprintf("\nplayer-games: %d | games: %d\n", nrow(D), uniqueN(D$game_id)))
cat(sprintf("games close(<=7): %d | blowout(>=28): %d\n",
            uniqueN(pairs[script == "close"]$game_id), uniqueN(pairs[script == "blowout"]$game_id)))
cat(sprintf("\nsd(final margin): %.2f | mean |margin|: %.2f\n", sd(G$points - G$opp_points), mean(G$margin)))
