#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — does realized same-game correlation depend on ACTUAL game script?
# A tractable, data-driven approximation of Blickle idea #1 (scenario-conditioned
# correlation, https://x.com/AlexBlickle1/status/2096644199198097515) using data we
# already have (WNBA box scores + final margins) instead of a full synthetic
# scenario-injected play-by-play simulator, which would need play-by-play data we
# don't currently ingest.
#
# FINDING (2026-09, 2023-2026 box scores, high-minute players >=24 min):
#   opponent pairs:  close (<=8 margin) cor ~ +0.041  |  blowout (>=18 margin) cor ~ -0.068
#   same-team pairs: close                cor ~ -0.001 |  blowout                cor ~ +0.076
# The correlation SIGN FLIPS by game script in both cases. A flat single factor (as
# used in sports/wnba/correlate.R, and in this validate_wnba_correlation.R's own
# baseline) CANNOT represent a sign flip -- this is real, measured evidence that
# script-conditional ("hidden") correlation exists and is currently unmodeled.
#
# NOT YET BUILT: an actual script-conditional simulator, which would need (1) a
# pre-game script-probability estimate (e.g. from Vegas spread -> P(blowout)), and
# (2) a slate_sim.R architecture change to draw a discrete script regime PER SIM and
# switch loadings conditionally, rather than a single fixed load per player passed in
# once. That is a properly-scoped follow-up project, not attempted here.
#
# Run: Rscript tests/validate_game_script_correlation.R
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine()
suppressPackageStartupMessages(library(data.table))

box_path <- dfs_path("data", "raw", "wnba_player_box.rds")
if (!file.exists(box_path)) {
  cat("No cached WNBA box scores at", box_path, "-> run wnba_ingest() first (needs the 'wehoop' package).\n")
  quit(status = 0)
}
D <- as.data.table(readRDS(box_path))
D <- D[did_not_play == FALSE & min >= 10]
setorder(D, athlete_id, game_date)
D[, `:=`(m1 = shift(frollmean(dk_pts, 10)), m2 = shift(frollmean(dk_pts^2, 10))), by = athlete_id]
D[, `:=`(mu = m1, sd = sqrt(pmax(m2 - m1^2, 0)))]
D <- D[is.finite(mu) & is.finite(sd) & sd > 1]
D[, resid := (dk_pts - mu) / sd]
D[, margin := abs(team_score - opponent_team_score)]

pairs <- D[D, on = "game_id", allow.cartesian = TRUE][athlete_id < i.athlete_id & min >= 24 & i.min >= 24]
pairs[, rel := fifelse(team_abbreviation == i.team_abbreviation, "same_team", "opponent")]
pairs[, script := fifelse(margin <= 8, "close", fifelse(margin >= 18, "blowout", "mid"))]

cat("=== REALIZED correlation split by ACTUAL game script (final margin), high-minute players ===\n")
print(pairs[script != "mid", .(n_pairs = .N, realized_cor = round(cor(resid, i.resid), 3)),
            by = .(rel, script)][order(rel, script)])
cat(sprintf("\ngames: close(<=8) %d | blowout(>=18) %d\n",
            uniqueN(pairs[script == "close"]$game_id), uniqueN(pairs[script == "blowout"]$game_id)))
