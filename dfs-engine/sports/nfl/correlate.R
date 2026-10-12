# ==============================================================================
# NFL plugin — correlation (the GPP edge: stacking)
# Two factors (uses the extended slate_sim):
#   GAME factor  — shootout/pace: a high game total lifts everyone in the game
#                  (positive load for all -> QB + bring-back correlate).
#   TEAM factor  — the passing STACK: QB + WR/TE on the SAME team share it strongly
#                  (positive), so a same-team stack correlates MORE than a bring-back;
#                  RB gets a NEGATIVE team load (game script — teams run when ahead,
#                  pass when behind), so it decouples from its own QB.
# Net same-team corr = load_i*load_j + team_load_i*team_load_j (> cross-team = game only).
# Requires the pool to carry `team` + `game_id` (from the DK slate).
#
# NOTE (2026-09, honest disclosure, not fixed here — out of today's scope): these
# [SUPERSEDED 2026-09-15] game_ld/team_ld are now calibrated from measured
# residual correlations - see the block above them. The note below described the
# pre-calibration state and is kept for context on what changed.
# game_ld/team_ld values are hand-set DFS-community assumptions, never calibrated
# against real data. Checking them against real nflverse box scores (same method as
# below) shows the EXISTING game_ld implies opponent correlation of 0.12-0.25, while
# the REAL measured close-game value is ~0.038 — likely overstated, same pattern found
# and fixed for WNBA (DFS_MODEL_HANDOFF.md section 13). Left untouched today since the
# ask was specifically the scenario-conditioning below, not a full stacking recalibration
# — a good next step, but a separate one (would also need position x script splits this
# aggregate measurement can't support with the current sample).
#
# SCENARIO-CONDITIONED extension (2026-09): per
# https://x.com/AlexBlickle1/status/2096644199198097515 idea #1 — see WNBA/NCAAF's
# correlate.R for the full design. Measured real correlation split by game script
# (tests/validate_nfl_game_script_correlation.R, 2021-2025, 815 games):
#   opponent pairs:  close (<=6 margin) cor ~ +0.038 | blowout (>=21) cor ~ +0.009
#   same-team pairs: close                cor ~ +0.023 | blowout          cor ~ +0.075
# UNLIKE WNBA/NCAAF, opponent correlation does NOT flip sign in a blowout — it shrinks
# toward independence instead (NFL's well-known garbage-time dynamic: trailing teams
# pass more to catch up, inflating BOTH sides' skill-position stats, unlike basketball
# where a blown-out team's rotation gets pulled). So the GAME factor here shrinks in
# magnitude (no team-sign flip needed) rather than reversing sign.
# Only the GAME factor is made regime-conditional; TEAM (stacking) stays UNCHANGED
# between regimes — there's no position x script evidence to safely recalibrate the
# QB-stack/RB-decouple structure by script, and it shouldn't be touched without it.
# (get_team_loadings() naturally stays a plain, non-regime vector since no
# team_load_alt column is supplied — slate_sim.R supports this mixed case directly:
# only the factor that HAS alt/p_alt columns becomes per-simulation conditional.)
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

.NFL_MARGIN_SIGMA <- 14.19
.nfl_p_blowout <- function(exp_margin, sigma = .NFL_MARGIN_SIGMA, thresh = 21) {
  m <- pmax(exp_margin, 0)
  pnorm(-thresh, m, sigma) + (1 - pnorm(thresh, m, sigma))
}

nfl_correlation <- function(pool) {
  pool <- as.data.table(pool)
  if (!"game_id" %in% names(pool)) return(NULL)
  pos <- toupper(as.character(pool$position))
  # CALIBRATED 2026-09-15 against 2023-2025 residuals (NFL repo
  # scripts/85_calibrate_correlations.R). These replace hand-set DFS-community
  # values that this header used to flag as "never calibrated". Fitted by
  # weighted least squares to measured within-game correlations of standardised
  # residuals, restricted to ROSTERABLE players (projection >= 5 pts) - the full
  # roster universe dilutes a real stack with QB-to-WR5 pairs and understates it.
  #
  # Weighted mean abs error vs measured correlations: 0.185 -> 0.028.
  #
  # What the old numbers got wrong, measured vs implied:
  #   QB-WR same team   0.265 measured, 0.515 implied   (overstated ~2x)
  #   QB-TE same team   0.234 measured, 0.480 implied   (overstated ~2x)
  #   WR-WR same team   0.020 measured, 0.452 implied   (overstated ~23x)
  #   QB-QB opposing    0.123 measured, 0.250 implied
  #
  # WR-WR is the consequential one: two receivers on a team COMPETE for the same
  # targets, so their residuals are essentially uncorrelated. Modelling them at
  # 0.45 told the optimizer a same-team WR pair was nearly one doubled-up bet,
  # distorting both stacking and diversification.
  #
  # RB team load is now 0.000, not -0.28. The old negative encoded a game-script
  # assumption (teams run when ahead, so RB decouples from QB); measured
  # same-team QB-RB residual correlation is mildly POSITIVE (+0.042), so the
  # data does not support it. Left at zero rather than fitted slightly positive
  # because the constraint boundary is where the fit put it.
  #
  # DST/K CALIBRATED 2026-10-12 against 2021-2025 real DK scoring (sacks, INTs,
  # fumble recoveries, safeties, blocked kicks, def TDs, points-allowed tiers for
  # DST; FG-distance tiers for K, computed from nflreadr team_stats - no model
  # exists for either position, so there's no "residual"; z-scored DK points
  # within-season stands in for it, same role the fitted projection plays for
  # QB/RB/WR/TE). QB/RB/WR/TE loadings held FIXED at their already-calibrated
  # values; only DST/K's 4 parameters were fit, by the same weighted-least-squares
  # method as 85_calibrate_correlations.R (see
  # NFL/scripts/85b_calibrate_dst_k_correlations.R).
  #
  # The PRIOR hand-set DST value (game=0.35, team=0.20) was never calibrated and
  # had the WRONG SIGN on the game factor. Measured DST-QB cross-team correlation
  # is -0.356 (n=1,864), not positive: DST scoring comes from sacks/INTs/points-
  # allowed, i.e. the OPPOSING offense doing BADLY - a "shootout" (high shared
  # game factor, which is what lifts QB/WR/TE together) is the worst case for
  # BOTH teams' defenses simultaneously, not a boost. The old +0.35 told the
  # simulator a kicker/DST-heavy lineup was a legitimate correlated-upside GPP
  # play; it was backwards, and it visibly surfaced as the "too safe" lineups
  # (kicker + DST loaded for floor) the optimizer kept producing.
  #
  # K comes back close to zero on both factors (game -0.046, team 0.109) -
  # essentially uncorrelated with game script either way, which matches the
  # mechanism (FG attempts come from stalled drives, not scores either team banks).
  game_ld <- fifelse(pos == "QB", 0.408,
             fifelse(pos == "WR", 0.175,
             fifelse(pos == "TE", 0.228,
             fifelse(pos == "RB", 0.062,
             fifelse(pos == "DST", -0.663,
             fifelse(pos == "K", -0.046, 0.15))))))                # other: uncalibrated
  team_ld <- fifelse(pos == "QB", 0.619,                           # stack anchor
             fifelse(pos == "WR", 0.229,
             fifelse(pos == "TE", 0.180,
             fifelse(pos == "RB", -0.011,                          # measured: no decoupling
             fifelse(pos == "DST", 0.331,
             fifelse(pos == "K", 0.109, 0.0))))))                  # other: uncalibrated
  em <- if ("exp_margin" %in% names(pool)) pool$exp_margin else rep(11.1, nrow(pool))
  em[!is.finite(em)] <- 11.1
  blowout_shrink <- sqrt(0.009 / 0.038)                            # measured blowout/close ratio, on the LOAD
  data.table(player_id = pool$player_id, game_id = pool$game_id,
             load = game_ld, team_load = team_ld,
             load_alt = game_ld * blowout_shrink, p_alt = .nfl_p_blowout(em))
}
