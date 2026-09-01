# ==============================================================================
# DFS ENGINE — correlation (game-environment factor model)
# Independent player sims systematically misprice stacks. We use a one-factor
# model per game: each game has a latent environment factor Z_game (pace / total /
# script). A player's standardized score loads on it with `load` in [-1, 1]:
#
#   z_p = load_p * Z_game(p) + sqrt(1 - load_p^2) * eps_p
#
# Then within-game pairwise correlation = load_i * load_j. Same-sign loads ->
# positive (QB+WR, NBA teammates, MLB-style stacks); OPPOSITE signs -> negative
# (tennis opponents: one advances). load = 0 -> independent (golf).
#
# This is more stable and far faster than an N×N matrix, and is the abstraction
# every sport's `correlation(slate)` plugs into.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# Helper for plugins: turn a target within-game correlation `rho` into per-player
# loadings. Pass `signs` (named by player_id, +1/-1) to encode anti-correlation;
# default all +1. Returns data.table(player_id, game_id, load).
loadings_from_rho <- function(player_id, game_id, rho = 0.4, signs = NULL) {
  rho <- pmin(pmax(rho, 0), 0.95)
  base <- sqrt(rho)
  s <- if (is.null(signs)) rep(1, length(player_id)) else signs[as.character(player_id)]
  s[is.na(s)] <- 1
  data.table(player_id = player_id, game_id = game_id, load = base * s)
}

# Resolve loadings for a pool: call the sport's correlation() plugin, align to the
# pool's player_id order, clamp, and fill missing with 0 (independent). Returns a
# numeric vector `load` parallel to pool rows.
get_loadings <- function(pool, sport) {
  spec <- get_sport(sport)
  load_vec <- rep(0, nrow(pool))
  if (!is.null(spec$correlation)) {
    cl <- tryCatch(spec$correlation(pool), error = function(e) { msg("  correlation() failed:", conditionMessage(e)); NULL })
    if (!is.null(cl) && nrow(cl)) {
      cl <- as.data.table(cl)
      m <- match(pool$player_id, cl$player_id)
      load_vec <- ifelse(is.na(m), 0, cl$load[m])
    }
  }
  pmin(pmax(load_vec, -0.99), 0.99)
}

# Optional SECOND-factor (team) loadings for stacking sports (NFL/NBA). Reads a
# `team_load` column from the sport's correlation() output; 0 if absent (single-factor).
get_team_loadings <- function(pool, sport) {
  spec <- get_sport(sport); tv <- rep(0, nrow(pool))
  if (!is.null(spec$correlation)) {
    cl <- tryCatch(spec$correlation(pool), error = function(e) NULL)
    if (!is.null(cl) && "team_load" %in% names(cl) && nrow(cl)) {
      cl <- as.data.table(cl); m <- match(pool$player_id, cl$player_id)
      tv <- ifelse(is.na(m), 0, cl$team_load[m])
    }
  }
  pmin(pmax(tv, -0.99), 0.99)
}
