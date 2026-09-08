# ==============================================================================
# NCAAF (CFB) plugin — correlation (SCENARIO-CONDITIONED — close game vs blowout)
#
# Same architecture as sports/wnba/correlate.R (see that file + DFS_MODEL_HANDOFF.md
# section 15 for the full design rationale — https://x.com/AlexBlickle1/status/2096644199198097515
# idea #1). Built after confirming the SAME sign-flip phenomenon holds in CFB using
# REAL box scores already cached from the CFB DFS build (cfb_game_scores() reads the
# team-level `points` already sitting in data/raw/ncaaf_src/*.json — no new API calls):
#
#   CLOSE  (final margin <=7):  opponent cor ~ +0.060 | same-team cor ~ +0.036
#   BLOWOUT(final margin >=28): opponent cor ~ -0.082 | same-team cor ~ +0.059
#
# (2024-2025 CFBD data, 1,172 games with usable rolling residuals, 368 close / 284
# blowout — see tests/validate_ncaaf_game_script_correlation.R.)
#
# UNLIKE WNBA: same-team correlation in BOTH regimes measures BELOW the opponent-level
# magnitude (0.036 < 0.060 close; 0.059 < 0.082 blowout) — i.e. real CFB data does not
# support a positive same-team increment here (the additive team_load^2 term can only
# ADD, never subtract, so a genuine below-opponent same-team value can't be represented
# either way). Rather than force an unsupported team factor, CFB uses team_load = 0 in
# both regimes — a single sign-flipping game factor, simpler than WNBA's model, chosen
# because that's what the real data actually supports. same-team correlation will read
# as ~= the opponent-level magnitude (slightly overstating it vs the measured values
# above) — the same honest simplification WNBA's close-regime already makes.
#
# P(blowout) per game: closed-form P(|Margin| >= 28) with Margin ~ Normal(exp_margin,
# sigma), sigma = 23.98 = the REAL empirical sd of final CFB margins (2024-2025, 1,759
# games) — not assumed. exp_margin (sports/ncaaf/project.R) prefers the real Vegas
# spread (vegas_games("ncaaf", date), already free/available via the same ESPN-lines
# infra WNBA uses) and falls back to the empirical mean |margin| (~18.8) when no line
# matches, rather than defaulting to 0 (which would wrongly imply "always close").
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

.NCAAF_MARGIN_SIGMA <- 23.98
.ncaaf_p_blowout <- function(exp_margin, sigma = .NCAAF_MARGIN_SIGMA, thresh = 28) {
  m <- pmax(exp_margin, 0)
  pnorm(-thresh, m, sigma) + (1 - pnorm(thresh, m, sigma))
}

# deterministic +1/-1 split between the two teams in a game (same convention as
# sports/wnba/correlate.R's .wnba_team_sign — arbitrary which team is which; the
# blowout direction is resolved by the sign of the simulated game factor, not this
# assignment, so it only needs to be CONSISTENT within a game).
.ncaaf_team_sign <- function(team, game_id) {
  opp <- vapply(seq_along(team), function(i) {
    g <- strsplit(as.character(game_id[i]), "@", fixed = TRUE)[[1]]
    o <- setdiff(g, team[i]); if (length(o)) o[1] else NA_character_
  }, character(1))
  ifelse(is.na(opp), 1, ifelse(team < opp, 1, -1))
}

ncaaf_correlation <- function(pool) {
  pool <- as.data.table(pool)
  if (!"game_id" %in% names(pool)) return(NULL)
  em <- if ("exp_margin" %in% names(pool)) pool$exp_margin else rep(18.8, nrow(pool))
  em[!is.finite(em)] <- 18.8

  rho_close_game <- 0.060
  rho_blow_game  <- 0.082

  sgn <- if ("team" %in% names(pool)) .ncaaf_team_sign(pool$team, pool$game_id) else rep(1, nrow(pool))
  L <- loadings_from_rho(pool$player_id, pool$game_id, rho = rho_close_game)   # base = close regime
  L[, `:=`(team_load = 0, load_alt = sqrt(rho_blow_game) * sgn,
           team_load_alt = 0, p_alt = .ncaaf_p_blowout(em))]
  L[]
}
