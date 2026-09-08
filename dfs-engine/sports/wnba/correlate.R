# ==============================================================================
# WNBA plugin — correlation
# Players in the same game share a positive environment factor (pace / total): a
# fast, high-total game lifts everyone together, scaled by the game's expected total
# (game_total_z from team_env.R). Teammates get an ADDITIONAL small same-team factor
# on top (get_team_loadings), so pairwise corr = load^2 (opponents) or load^2+tload^2
# (teammates) per slate_sim.R's two-factor model.
#
# CALIBRATED 2026-09 against REALIZED co-movement in real box scores (wehoop, 2023-2026,
# 1,076 games, 15,290 player-games; standardized each player's dk_pts as a z-score vs
# their own trailing-10-game mean/sd, then correlated same-game residuals). Restricting
# to starters/high-minute players (>=24 min, robust to garbage-time noise) measured:
#   opponent pairs:   cor ~ 0.016
#   same-team pairs:  cor ~ 0.036
# The PRIOR assumption (rho = 0.20-0.45) overstated same-game co-movement by roughly an
# order of magnitude — exactly the failure mode that makes a sim's reported GPP EV
# reflect the sim's own flaws rather than lineup quality (see the field-sim EV-trust
# caveat in DFS_MODEL_HANDOFF.md §10, and the "obvious vs hidden correlation" framing in
# https://x.com/AlexBlickle1/status/2096644199198097515). Re-derivation script:
# tests/validate_wnba_correlation.R — rerun if wnba_player_box.rds grows meaningfully.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

wnba_correlation <- function(pool) {
  pool <- as.data.table(pool)
  if (!"game_id" %in% names(pool)) return(NULL)
  gtz <- if ("game_total_z" %in% names(pool)) pool$game_total_z else rep(0, nrow(pool))
  gtz[!is.finite(gtz)] <- 0
  rho_game <- pmin(pmax(0.016 + 0.01 * gtz, 0.005), 0.05)   # opponent-level, calibrated
  rho_team_incr <- 0.02                                     # same-team increment, calibrated
  L <- loadings_from_rho(pool$player_id, pool$game_id, rho = rho_game)
  L[, team_load := sqrt(rho_team_incr)]
  L[]
}
