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
#       optional p_zero. loadings: numeric vector parallel to pool rows (from get_loadings),
#       OR a "regime_loadings" object (list(base, alt, p_alt), from get_loadings() when a
#       sport plugin supplies a SECOND, alternate-scenario loading set — see below.
# Returns list(scores = P×n_sims matrix, pool = pool) so callers keep alignment.
# team_loadings (optional): a SECOND factor shared by same-`team` players, on top of the
# game factor. Lets NFL express the QB->pass-catcher STACK (same-team corr > bring-back)
# and RB game-script decoupling (negative team load). NULL -> single-factor (all other
# sports unchanged): z = load*Z_game + sqrt(1-load^2)*eps exactly as before.
#
# SCENARIO-CONDITIONED CORRELATION (regime_loadings): per
# https://x.com/AlexBlickle1/status/2096644199198097515 idea #1, and confirmed for real
# in WNBA (tests/validate_game_script_correlation.R) — realized correlation SIGN FLIPS by
# game script (close vs blowout); a single fixed loading cannot represent that. When
# `loadings` (and optionally `team_loadings`) is a `regime_loadings` object — list(base,
# alt, p_alt) — we draw a per-game, per-SIMULATION Bernoulli regime (p_alt = P(the "alt"
# scenario, e.g. blowout)) and blend base/alt loadings PER COLUMN accordingly, so
# different simulated draws of the SAME slate can land in different correlation regimes.
# p_alt lives on the `loadings` object (shared with team_loadings' blend, since script is
# one game-level draw, not two independent ones). Ordinary numeric-vector loadings behave
# EXACTLY as before — this is fully backward-compatible / zero-risk for every other sport.
slate_sim <- function(pool, loadings = NULL, team_loadings = NULL, n_sims = 10000L,
                      floor0 = TRUE, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  pool <- validate_projection(as.data.table(pool))
  P <- nrow(pool)
  if (is.null(loadings)) loadings <- rep(0, P)

  is_regime <- inherits(loadings, "regime_loadings")
  base_load <- if (is_regime) loadings$base else loadings
  base_load <- pmin(pmax(base_load, -0.99), 0.99)
  is_regime_team <- inherits(team_loadings, "regime_loadings")
  base_tload <- if (is.null(team_loadings)) rep(0, P) else if (is_regime_team) team_loadings$base else team_loadings
  base_tload <- pmin(pmax(base_tload, -0.99), 0.99)

  # shared per-game environment factor
  games <- if ("game_id" %in% names(pool)) pool$game_id else rep(NA_character_, P)
  ug <- unique(games[!is.na(games)])
  gi <- match(games, ug)
  Zg <- if (length(ug)) matrix(rnorm(length(ug) * n_sims), nrow = length(ug)) else NULL

  eps <- matrix(rnorm(P * n_sims), nrow = P)
  Zp_game <- matrix(0, P, n_sims)
  has_game <- !is.na(gi)
  if (any(has_game)) Zp_game[has_game, ] <- Zg[gi[has_game], , drop = FALSE]

  if (is_regime || is_regime_team) {
    # per-game, per-sim regime draw (shared by the game- and team-factor blend below):
    # p_alt comes from whichever of loadings/team_loadings is regime-conditional.
    p_alt_src <- if (is_regime) loadings$p_alt else team_loadings$p_alt
    p_alt <- pmin(pmax(p_alt_src, 0), 1)
    Rg <- if (length(ug)) { pg <- p_alt[match(ug, games)]; pg[is.na(pg)] <- 0
      matrix(runif(length(ug) * n_sims), nrow = length(ug)) < pg } else NULL
    Rp <- matrix(FALSE, P, n_sims)          # per-player-row regime flag, per sim column
    if (any(has_game)) Rp[has_game, ] <- Rg[gi[has_game], , drop = FALSE]

    alt_load  <- if (is_regime)      pmin(pmax(loadings$alt, -0.99), 0.99)      else base_load
    alt_tload <- if (is_regime_team) pmin(pmax(team_loadings$alt, -0.99), 0.99) else base_tload
    load  <- matrix(base_load,  P, n_sims); load[Rp]  <- matrix(alt_load,  P, n_sims)[Rp]
    tload <- matrix(base_tload, P, n_sims); tload[Rp] <- matrix(alt_tload, P, n_sims)[Rp]
  } else {
    load <- base_load; tload <- base_tload
  }
  # keep total shared variance < 1 (game^2 + team^2), rescaling both if they'd exceed it
  tot <- load^2 + tload^2; sc <- ifelse(tot > 0.98, sqrt(0.98 / tot), 1)
  load <- load * sc; tload <- tload * sc

  # shared per-team factor (only if any team loadings are non-zero)
  Zp_team <- matrix(0, P, n_sims)
  if (any(base_tload != 0, if (is_regime_team) team_loadings$alt != 0 else FALSE) && "team" %in% names(pool)) {
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
