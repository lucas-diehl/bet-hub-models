#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — validate WNBA's assumed same-game correlation against REALITY.
# Per https://x.com/AlexBlickle1/status/2096644199198097515: a contest sim's reported
# ROI is only as trustworthy as its correlation assumptions. sports/wnba/correlate.R's
# rho was an untested guess (0.20-0.45) until this script measured it against real
# box scores. Re-run this whenever wnba_player_box.rds grows meaningfully (a full
# season or two of new data) to confirm the calibration still holds.
#
# Method: standardize each player's dk_pts as a z-score vs their OWN trailing-10-game
# mean/sd (isolates game-level over/under-performance from skill level), then correlate
# those residuals for same-team vs opponent pairs within the same real game.
#
# Run: Rscript tests/validate_wnba_correlation.R
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

pairs <- D[D, on = "game_id", allow.cartesian = TRUE][athlete_id < i.athlete_id]
pairs[, rel := fifelse(team_abbreviation == i.team_abbreviation, "same_team", "opponent")]

cat("=== REALIZED same-game correlation (all minutes >= 10) ===\n")
print(pairs[, .(n_pairs = .N, realized_cor = round(cor(resid, i.resid), 3)), by = rel])

cat("\n=== ROBUSTNESS: restricted to players with >= 24 min both games (removes garbage-time noise) ===\n")
pairs2 <- pairs[min >= 24 & i.min >= 24]
print(pairs2[, .(n_pairs = .N, realized_cor = round(cor(resid, i.resid), 3)), by = rel])

cat(sprintf("\nplayer-games: %d | games: %d | seasons: %s\n",
            nrow(D), uniqueN(D$game_id), paste(range(D$season), collapse = "-")))
cat("\nCurrent sports/wnba/correlate.R calibration (as of 2026-09): rho_game ~0.016-0.05,\n",
    "rho_team_incr = 0.02 -- fit directly from the high-minute robustness numbers above.\n",
    "If these numbers drift materially, recalibrate correlate.R to match.\n", sep = "")
