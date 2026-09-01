# ==============================================================================
# DFS ENGINE — slate simulation (joint Monte Carlo)
# Sample game environments and player outcomes JOINTLY using per-player
# distributions (proj, sim_sd, p_zero) and the one-factor correlation (loadings).
# Output: a P×N_sims matrix of fantasy outcomes. Everything downstream (field
# grading, candidate EV) reads from this cached matrix.
#
# Vectorized (matrix ops, no per-sim loops) — generalizes simulate_slate() in
# Golf/dfs_pipeline_v2.R, adding correlated game factors and DNP (p_zero) risk.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# pool: data.table with player_id, proj, sim_sd, game_id (NA allowed = independent),
#       optional p_zero. loadings: numeric vector parallel to pool rows (from get_loadings).
# Returns list(scores = P×n_sims matrix, pool = pool) so callers keep alignment.
# team_loadings (optional): a SECOND factor shared by same-`team` players, on top of the
# game factor. Lets NFL express the QB->pass-catcher STACK (same-team corr > bring-back)
# and RB game-script decoupling (negative team load). NULL -> single-factor (all other
# sports unchanged): z = load*Z_game + sqrt(1-load^2)*eps exactly as before.
slate_sim <- function(pool, loadings = NULL, team_loadings = NULL, n_sims = 10000L,
                      floor0 = TRUE, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  pool <- validate_projection(as.data.table(pool))
  P <- nrow(pool)
  if (is.null(loadings)) loadings <- rep(0, P)
  load  <- pmin(pmax(loadings, -0.99), 0.99)
  tload <- if (is.null(team_loadings)) rep(0, P) else pmin(pmax(team_loadings, -0.99), 0.99)
  # keep total shared variance < 1 (game^2 + team^2), rescaling both if they'd exceed it
  tot <- load^2 + tload^2; sc <- ifelse(tot > 0.98, sqrt(0.98 / tot), 1)
  load <- load * sc; tload <- tload * sc

  # shared per-game environment factor
  games <- if ("game_id" %in% names(pool)) pool$game_id else rep(NA_character_, P)
  ug <- unique(games[!is.na(games)])
  gi <- match(games, ug)
  Zg <- if (length(ug)) matrix(rnorm(length(ug) * n_sims), nrow = length(ug)) else NULL

  eps <- matrix(rnorm(P * n_sims), nrow = P)
  Zp_game <- matrix(0, P, n_sims)
  has_game <- !is.na(gi)
  if (any(has_game)) Zp_game[has_game, ] <- Zg[gi[has_game], , drop = FALSE]

  # shared per-team factor (only if any team loadings are non-zero)
  Zp_team <- matrix(0, P, n_sims)
  if (any(tload != 0) && "team" %in% names(pool)) {
    teams <- pool$team; ut <- unique(teams[!is.na(teams)]); ti <- match(teams, ut)
    if (length(ut)) { Zt <- matrix(rnorm(length(ut) * n_sims), nrow = length(ut))
      has_team <- !is.na(ti); Zp_team[has_team, ] <- Zt[ti[has_team], , drop = FALSE] }
  }

  resid <- sqrt(pmax(1 - load^2 - tload^2, 0))
  z <- load * Zp_game + tload * Zp_team + resid * eps  # standardized, game+team correlated
  scores <- pool$proj + pool$sim_sd * z               # vectorized recycle down columns

  # DNP / bust risk: with prob p_zero the player produces ~0 this sim
  if ("p_zero" %in% names(pool)) {
    pz <- pool$p_zero; pz[!is.finite(pz)] <- 0
    if (any(pz > 0)) {
      mask <- matrix(runif(P * n_sims), P) < pz
      scores[mask] <- 0
    }
  }
  if (floor0) scores[scores < 0] <- 0

  list(scores = scores, pool = pool, n_sims = n_sims)
}

# Quick marginal summary from a sim (for the Rankings tab / sanity checks).
sim_summary <- function(sim) {
  s <- sim$scores
  data.table(
    player_id = sim$pool$player_id,
    sim_mean  = rowMeans(s),
    sim_p10   = apply(s, 1, quantile, 0.10, names = FALSE),
    sim_p90   = apply(s, 1, quantile, 0.90, names = FALSE))
}
