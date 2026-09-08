# ==============================================================================
# WNBA plugin — correlation (SCENARIO-CONDITIONED — close game vs blowout)
#
# Superseded the single-flat-factor model (rho constant regardless of game script) once
# tests/validate_game_script_correlation.R found the realized correlation SIGN FLIPS by
# actual game script — a flat factor structurally cannot represent that. Per
# https://x.com/AlexBlickle1/status/2096644199198097515 idea #1 (a contest sim needs
# "hidden," scenario-conditioned correlation, not just a fixed pairwise coefficient),
# this supplies TWO regimes to slate_sim.R's regime-conditional blending:
#
#   CLOSE  (final margin <=8):  opponent cor ~ +0.041 | same-team cor ~ -0.001 (~= noise)
#   BLOWOUT(final margin >=18): opponent cor ~ -0.068 | same-team cor ~ +0.076
#
# (2023-2026 box scores, 1,076 games, high-minute players >= 24 min, standardized
# trailing-10-game residuals — see tests/validate_game_script_correlation.R.)
#
# CLOSE regime: a single same-sign factor for both teams (pace lifts everyone a little);
# no separate team increment — the measured same-team value (-0.001) is statistically
# indistinguishable from zero, and this additive 2-factor model can't go BELOW the
# opponent-level correlation anyway (team_load^2 only ever adds), so 0 is the honest
# choice rather than fabricating precision on a noise-level number.
# BLOWOUT regime: the two teams get OPPOSITE-signed loadings on the SAME shared game
# factor (deterministic split — see .wnba_team_sign — not per-sim-random: which team
# "wins" a given simulated blowout falls out naturally from the sign of that sim's game
# factor draw, since it's symmetric). Teammates share the loading's sign, so same-team
# pairs get the FULL +0.068 from the game factor alone, plus a small team_load top-up
# (sqrt(0.076-0.068)) to reach the measured +0.076.
#
# P(blowout) per game: closed-form P(|Margin| >= 18) with Margin ~ Normal(exp_margin,
# sigma), sigma = 14.13 = the REAL empirical sd of final WNBA margins (2023-2026, 2,268
# games) — not an assumption. exp_margin (team_env.R) is the pregame-expected |margin|
# (Vegas spread when available, else box-derived team net rating).
#
# Re-derivation: tests/validate_game_script_correlation.R (correlation targets) +
# margin sigma from data/raw/wnba_player_box.rds directly. Rerun periodically.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

.WNBA_MARGIN_SIGMA <- 14.13
.wnba_p_blowout <- function(exp_margin, sigma = .WNBA_MARGIN_SIGMA, thresh = 18) {
  m <- pmax(exp_margin, 0)
  pnorm(-thresh, m, sigma) + (1 - pnorm(thresh, m, sigma))
}

# deterministic +1/-1 split between the two teams in a game (arbitrary which is which —
# the blowout direction is resolved by the sign of the simulated game factor, not by
# this assignment; it only needs to be CONSISTENT within a game, which alphabetical
# team-string comparison guarantees for both sides).
.wnba_team_sign <- function(team, game_id) {
  opp <- vapply(seq_along(team), function(i) {
    g <- strsplit(as.character(game_id[i]), "@", fixed = TRUE)[[1]]
    o <- setdiff(g, team[i]); if (length(o)) o[1] else NA_character_
  }, character(1))
  ifelse(is.na(opp), 1, ifelse(team < opp, 1, -1))
}

wnba_correlation <- function(pool) {
  pool <- as.data.table(pool)
  if (!"game_id" %in% names(pool)) return(NULL)
  em <- if ("exp_margin" %in% names(pool)) pool$exp_margin else rep(0, nrow(pool))
  em[!is.finite(em)] <- 0

  rho_close_game   <- 0.041; rho_close_team_incr   <- 0
  rho_blow_game    <- 0.068; rho_blow_team_incr    <- 0.076 - 0.068   # = 0.008

  sgn <- if ("team" %in% names(pool)) .wnba_team_sign(pool$team, pool$game_id) else rep(1, nrow(pool))
  L <- loadings_from_rho(pool$player_id, pool$game_id, rho = rho_close_game)   # base = close regime
  L[, `:=`(team_load = sqrt(rho_close_team_incr),
           load_alt = sqrt(rho_blow_game) * sgn,
           team_load_alt = sqrt(rho_blow_team_incr),
           p_alt = .wnba_p_blowout(em))]
  L[]
}
