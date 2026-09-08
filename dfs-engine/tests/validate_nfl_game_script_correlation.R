#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — does realized NFL correlation depend on ACTUAL game script?
# NFL analog of tests/validate_game_script_correlation.R (WNBA) and
# tests/validate_ncaaf_game_script_correlation.R (CFB). Uses nflverse's free schedule/
# results dataset (sports/nfl/ingest.R::nfl_game_scores(), separate from the player-stats
# release already ingested — has real final scores + the historical closing spread).
#
# FINDING (2026-09, 2021-2025, 815 games with usable rolling residuals):
#   opponent pairs:  close (<=6 margin)  cor ~ +0.038  |  blowout (>=21) cor ~ +0.009
#   same-team pairs: close                cor ~ +0.023  |  blowout           cor ~ +0.075
# UNLIKE WNBA/NCAAF, opponent correlation does NOT flip sign in a blowout — it shrinks
# toward independence (NFL's well-known garbage-time dynamic: trailing teams pass more
# to catch up, inflating BOTH sides' skill-position stats, unlike basketball where a
# blown-out team's rotation gets pulled). sports/nfl/correlate.R therefore only makes
# the GAME factor regime-conditional (shrinks in blowouts, no sign flip); the existing
# QB-stack/RB-decouple TEAM factor is left unchanged — no position x script evidence
# exists yet to safely recalibrate it.
#
# Run: Rscript tests/validate_nfl_game_script_correlation.R
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine()
if (!exists("nfl_game_scores")) source(dfs_path("sports", "nfl", "ingest.R"))
suppressPackageStartupMessages(library(data.table))

wk_path <- nfl_weekly_path()
if (!file.exists(wk_path)) { cat("No cached nfl_weekly.rds -> run nfl_train(refresh=TRUE) first.\n"); quit(status = 0) }
G <- tryCatch(nfl_game_scores(seasons = 2021:2025), error = function(e) { cat("schedule fetch failed:", conditionMessage(e), "\n"); NULL })
if (is.null(G) || !nrow(G)) quit(status = 0)
Gh <- G[, .(game_id, team = home_team, margin = abs(home_score - away_score))]
Ga <- G[, .(game_id, team = away_team, margin = abs(home_score - away_score))]
GT <- rbind(Gh, Ga)

D <- as.data.table(readRDS(wk_path))
setorder(D, player_id, season, week)
D[, `:=`(m1 = shift(frollmean(dk_pts, 8)), m2 = shift(frollmean(dk_pts^2, 8))), by = player_id]
D[, `:=`(mu = m1, sd = sqrt(pmax(m2 - m1^2, 0)))]
D <- D[is.finite(mu) & is.finite(sd) & sd > 1]
D[, resid := (dk_pts - mu) / sd]
D <- merge(D, GT, by = c("game_id", "team"))

pairs <- D[D, on = "game_id", allow.cartesian = TRUE][player_id < i.player_id]
pairs[, rel := fifelse(team == i.team, "same_team", "opponent")]
pairs[, script := fifelse(margin <= 6, "close", fifelse(margin >= 21, "blowout", "mid"))]

cat("=== REALIZED same-game correlation split by ACTUAL game script (NFL) ===\n")
print(pairs[script != "mid", .(n_pairs = .N, realized_cor = round(cor(resid, i.resid), 3)),
            by = .(rel, script)][order(rel, script)])
cat(sprintf("\nplayer-games: %d | games: %d\n", nrow(D), uniqueN(D$game_id)))
cat(sprintf("games close(<=6): %d | blowout(>=21): %d\n",
            uniqueN(pairs[script == "close"]$game_id), uniqueN(pairs[script == "blowout"]$game_id)))
cat(sprintf("sd(final margin): %.2f\n", sd(G$home_score - G$away_score)))
