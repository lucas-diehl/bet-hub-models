# ==============================================================================
# WNBA plugin — correlation
# Players in the same game share a positive environment factor (pace / total): a
# fast, high-total game lifts everyone together. The base loading is scaled by the
# game's expected total (game_total_z from team_env.R) — high-total games get
# stronger same-game correlation (fatter STACKED ceilings in the slate sim), low
# totals weaker. This is the Vegas-free game-environment effect on the JOINT
# distribution. (Within-team usage trade-offs are a later refinement.)
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

wnba_correlation <- function(pool) {
  pool <- as.data.table(pool)
  if (!"game_id" %in% names(pool)) return(NULL)
  gtz <- if ("game_total_z" %in% names(pool)) pool$game_total_z else rep(0, nrow(pool))
  gtz[!is.finite(gtz)] <- 0
  rho <- pmin(pmax(0.20 + 0.05 * gtz, 0.05), 0.45)   # per-player rho, scaled by total
  loadings_from_rho(pool$player_id, pool$game_id, rho = rho)
}
