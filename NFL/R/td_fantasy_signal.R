## ---------------------------------------------------------------------------
## R/td_fantasy_signal.R
##
## The fantasy projection model's own touchdown expectation, exposed as a
## probability the touchdown model can consume.
##
## WHY. The fantasy model's TD components add a small but sign-consistent
## amount of touchdown information on top of market + fundamental. "Small" is
## the operative word, and the history of this comment block is itself the
## cautionary tale - see WHAT THIS BLOCK USED TO CLAIM below.
##
## MEASURED 2026-10-08, walk-forward (each fit trained strictly on seasons <
## test season; 2023 is not testable because it has no prior season on the
## board, so its model_probability is the raw unblended fundamental):
##
##   anytime_td ~ td_fundamental + market + fantasy_td
##     train < 2024 (n =  5,276)   fund +0.0731 (p = 0.550)
##                                 market +0.9888 (p < 1e-16)
##                                 fantasy +0.0804 (p = 0.549)
##     train < 2025 (n = 10,466)   fund +0.0920 (p = 0.258)
##                                 market +0.8821 (p < 1e-16)
##                                 fantasy +0.1421 (p = 0.147)
##
##   The MARKET is the only input that is statistically significant. Neither
##   the fundamental nor the fantasy term clears p = 0.05 in either fold, and
##   the fundamental is POSITIVE, not negative.
##
##   Brier, with vs without the fantasy term (walk-forward, out of sample):
##     2024   0.144242  vs  0.144283   gain +0.000040
##     2025   0.144075  vs  0.144146   gain +0.000070
##
## So the term earns its place, but barely: the gain is ~0.00005, positive in
## sign in both folds. Dropping it costs that much and no more. For scale, the
## blend as a whole beats a market-only calibrator by +0.000201 (2024) and
## +0.000321 (2025), i.e. the fundamental and fantasy terms together are worth
## several times what the fantasy term alone contributes.
##
## WHAT THIS BLOCK USED TO CLAIM, AND WHY IT WAS WRONG. Until 2026-10-08 this
## comment recorded:
##
##   td_fundamental  -0.1457  (p = 0.017)   <- NEGATIVE once fantasy is present
##   market          +0.6480
##   fantasy_td      +0.6579  (p < 1e-16)
##   2025 holdout Brier gain +0.00142, "~22x the TE-specific blend and ~24x the
##   TD model's entire honest edge over the market"
##
## plus a positional claim that at TE the fantasy signal "nearly replaces the
## market price (fantasy +0.996 vs market +0.074)".
##
## None of those numbers reproduce. They came from a SINGLE in-sample fit on
## one fantasy-model generation, and the reason that was never evidence is
## collinearity. The three input logits correlate 0.86-0.90 with each other
## (fund~market 0.856, fund~fantasy 0.905, market~fantasy 0.876) and carry VIFs
## of 6.09 / 4.75 / 7.02. High VIF means the COEFFICIENT ESTIMATES have inflated
## variance: the data pins down the blended prediction far more tightly than it
## pins down how credit is divided among three near-duplicate inputs, so small
## changes in those inputs move the fitted weights a lot. The fantasy term's own
## standard error (0.098-0.134 across folds) is comparable to the coefficient
## itself, which is the same fact stated directly.
##
## Be precise about what this does and does not mean. The old coefficients are
## NOT merely a tied alternative that the optimiser happened to report instead:
## evaluated on the current data they are decisively rejected (log-likelihood
## -6972.52 vs -6952.08 for the MLE, LR = 40.87 on 3 df, p < 1e-4 - worse even
## than a plain market-only blend at -6962.22). They were fitted to a DIFFERENT
## fantasy generation and do not transfer to this one. That is the hazard:
## collinear weights are not stable across input revisions, so a coefficient
## recorded from one generation silently stops describing the next.
##
## The cleanest demonstration is a perturbation test. Between the 2026-09-29 and
## 2026-10-01 fantasy walk-forward generations the fantasy INPUT barely moved -
## row-level correlation 0.959, mean absolute difference 0.022, ~4%. Refitting
## the pooled deployment calibrator on it moved the COEFFICIENTS this far:
##
##     fantasy    +0.658  ->  +0.134     (5x smaller)
##     market     +0.648  ->  +0.855
##     fundamental -0.146  ->  +0.089    (sign flip)
##
##   and at TE:  fantasy +0.996 -> +0.221, market +0.074 -> +0.398
##
## A coefficient that swings 5x and a sign that flips on a 4% input change were
## never measuring information content. That is the GENERAL lesson and not a
## quirk of this signal: in a collinear blend, judge a term by its
## out-of-sample Brier contribution, never by the size, sign or p-value of its
## fitted coefficient. The +0.00005 figures above are that honest judgment.
##
## NOT STABLY IDENTIFIED - OPEN QUESTION. Because of the collinearity above,
## the three-input calibrator's coefficients are not stably identified, and the
## weight the live path applies to the fantasy signal depends on which fantasy
## generation the calibrator happened to be fitted against. A regularized
## (ridge) or market-anchored reformulation would be the principled fix, but it
## is a real modeling decision with its own validation burden and has NOT been
## attempted - it is deliberately left open rather than slipped in. Note before
## reaching for it that removing the fantasy term outright is a small LOSS
## (-0.000040 / -0.000070), not a gain, so "just drop it" is not the answer
## either. See the same note at .fit_one_market_calibrator() in
## R/touchdown_model.R.
##
## ALSO WORTH KNOWING: the live and historical paths read DIFFERENT sources
## (td_fantasy_signal_live() the current projection CSV,
## td_fantasy_signal_history() the fantasy walk-forward RDS). Keep the
## deployment calibrator refitted on the same fantasy generation that produces
## the live CSV it will score - a stale calibrator applied to a newer
## generation over-weighted the fantasy term ~5x until 2026-10-08.
##
## CONVERSION. The fantasy model predicts EXPECTED rushing and receiving TDs.
## Anytime-TD is P(at least one), so the two expectations are summed and pushed
## through a Poisson tail: P = 1 - exp(-lambda). Poisson is the right family
## here - it is what the fantasy model's own TD targets are fit with
## (count:poisson, see fantasy_target_specifications()).
##
## ---------------------------------------------------------------------------
## DRIVE-CONCENTRATION RECALIBRATION, added 2026-10-08.
##
## WHY. A touchdown-opportunity "lambda" built by summing independent
## per-touch/per-opportunity probabilities and pushing it through a Poisson
## tail implicitly treats every opportunity as an independent draw. It is not:
## several of a player's zone touches (yardline_100 <= 10) in the same game
## often come from the SAME DRIVE, where a later attempt only happens because
## an earlier one in that drive already failed - not a fresh, independent
## chance. Research (scripts/111-116, a six-round investigation of this TD
## model, all prior candidates in that arc failed their own mechanism tests)
## found this measurably miscalibrates the naive Poisson-tail probability:
## walk-forward, out of sample, actual TD rate is far above the naive implied
## rate for players whose zone touches are SPREAD across several distinct
## drives (gap +0.091) and roughly correctly calibrated for players whose
## touches cluster into repeat attempts on one drive (gap -0.014) - a 5-season-
## consistent, statistically confident split (pooled diff +0.1048, 95% CI
## [0.0777, 0.1315], positive in 5 of 5 seasons; see
## scripts/115_td_round6_candidates.R's Direction 1).
##
## A first correction attempt (a flat two-bucket additive probability shift)
## improved Brier but narrowed the output distribution - a collapse signature,
## not a fix, so it was NOT shipped. The corrected version below instead uses
## a CONTINUOUS, lagged (r5, strictly prior-game, season-bounded)
## `distinct_drive_fraction_r5` covariate and recalibrates in LOGIT space
## (exactly the method fit_platt_calibrator()/apply_platt_calibrator() in
## R/touchdown_model.R already uses for this project's other calibrators):
##
##   corrected_p = plogis(intercept + coef_naive_logit * qlogis(clip(naive_p))
##                         + coef_concentration * distinct_drive_fraction_r5)
##
## Validated in scripts/116_td_direction1_logit_recalibration.R: pooled Brier
## gain +0.00132 (positive in all 4 testable seasons 2022-2025), output spread
## WIDENS not narrows (pooled sd 109.8% of naive, no ceiling-clipping), and
## calibration-band max |actual - predicted| gap roughly halves (0.0904 ->
## 0.0422) with no band made meaningfully worse. Full specification and
## fitted coefficients by season in that script's output / the handback
## report dated 2026-10-08.
##
## Coefficients are fit WALK-FORWARD (per-season, trained strictly on seasons
## before the test season) by scripts/117_build_td_drive_concentration_
## calibrator.R and stored in data/processed/td_drive_concentration_
## calibrator.rds - built once, applied cheaply here, same separation as
## fit_market_calibrator()/apply_market_calibrator(). The historical path
## (td_fantasy_signal_history()) uses the per-season fit matching each row's
## season; the live path (td_fantasy_signal_live()) uses the "deployment" fit
## trained on all available history, same convention fit_td_deployment_model()
## already uses for the fundamental/market calibrators. If the calibrator
## artifact is missing, both paths fall back to the original, unmodified
## naive Poisson-tail probability - this change cannot hard-fail the pipeline.
## ---------------------------------------------------------------------------

td_fantasy_prob_from_lambda <- function(rush_td, rec_td,
                                        distinct_drive_fraction_r5 = NA_real_,
                                        calibrator = NULL) {
  lambda <- pmax(
    dplyr::coalesce(as.numeric(rush_td), 0) +
      dplyr::coalesce(as.numeric(rec_td), 0),
    1e-6
  )
  naive_p <- pmin(pmax(1 - exp(-lambda), 1e-6), 1 - 1e-6)
  if (is.null(calibrator)) return(naive_p)

  concentration <- dplyr::coalesce(
    as.numeric(distinct_drive_fraction_r5), as.numeric(calibrator$impute_val)
  )
  clipped_logit <- stats::qlogis(pmin(pmax(naive_p, 0.005), 0.995))
  corrected <- stats::plogis(
    as.numeric(calibrator$intercept) +
      as.numeric(calibrator$coef_naive_logit) * clipped_logit +
      as.numeric(calibrator$coef_concentration) * concentration
  )
  pmin(pmax(corrected, 1e-6), 1 - 1e-6)
}

## HISTORICAL: from the fantasy walk-forward predictions. Those are genuinely
## out-of-sample per season (each fold trains on season < test_season), and the
## `prediction` column is PRE salary-stack, so no market information leaks in
## through DK pricing.
td_fantasy_signal_history <- function(
  path = "data/processed/fantasy_prop_walk_forward.rds",
  calibrator_path = "data/processed/td_drive_concentration_calibrator.rds"
) {
  if (!file.exists(path)) return(NULL)
  fw <- tryCatch(readRDS(path)$predictions, error = function(e) NULL)
  if (is.null(fw) || !nrow(fw)) return(NULL)
  need <- c("game_id", "season", "player_id", "target", "prediction")
  if (!all(need %in% names(fw))) return(NULL)

  wide <- fw |>
    dplyr::filter(.data$target %in% c("rushing_tds", "receiving_tds")) |>
    dplyr::select("game_id", "season", "player_id", "target", "prediction") |>
    tidyr::pivot_wider(
      names_from = "target", values_from = "prediction",
      values_fn = mean, values_fill = 0
    )
  ## A fold that produced only one of the two targets would otherwise drop the
  ## missing column and error below.
  for (cc in c("rushing_tds", "receiving_tds")) {
    if (!cc %in% names(wide)) wide[[cc]] <- 0
  }

  ## Drive-concentration recalibration (see the dated comment block above
  ## td_fantasy_prob_from_lambda()). Per-season walk-forward fit, joined by
  ## `season`; a season with no fit (none exist before 2022, which needs at
  ## least one prior season of pbp - moot in practice since this file only
  ## ever covers 2023-2025) falls back to the unmodified naive probability.
  calibrator <- tryCatch(readRDS(calibrator_path), error = function(e) NULL)
  if (is.null(calibrator) || is.null(calibrator$historical_concentration) ||
        is.null(calibrator$per_season_fits)) {
    wide$fantasy_p_td <- td_fantasy_prob_from_lambda(
      wide$rushing_tds, wide$receiving_tds
    )
    return(wide |> dplyr::select("game_id", "player_id", "fantasy_p_td"))
  }

  n_before <- nrow(wide)
  wide <- wide |>
    dplyr::left_join(
      calibrator$historical_concentration |>
        dplyr::select("game_id", "player_id", "distinct_drive_fraction_r5"),
      by = c("game_id", "player_id")
    ) |>
    dplyr::left_join(calibrator$per_season_fits, by = "season")
  if (nrow(wide) != n_before) {
    stop("Drive-concentration join changed row count in td_fantasy_signal_history(): ",
         n_before, " -> ", nrow(wide), ". Duplicate keys upstream.", call. = FALSE)
  }

  wide$fantasy_p_td <- NA_real_
  has_fit <- !is.na(wide$coef_intercept)
  if (any(has_fit)) {
    wide$fantasy_p_td[has_fit] <- td_fantasy_prob_from_lambda(
      wide$rushing_tds[has_fit], wide$receiving_tds[has_fit],
      distinct_drive_fraction_r5 = wide$distinct_drive_fraction_r5[has_fit],
      calibrator = list(
        intercept = wide$coef_intercept[has_fit],
        coef_naive_logit = wide$coef_naive_logit[has_fit],
        coef_concentration = wide$coef_concentration[has_fit],
        impute_val = wide$impute_val[has_fit]
      )
    )
  }
  if (any(!has_fit)) {
    wide$fantasy_p_td[!has_fit] <- td_fantasy_prob_from_lambda(
      wide$rushing_tds[!has_fit], wide$receiving_tds[!has_fit]
    )
  }

  wide |>
    dplyr::select("game_id", "player_id", "fantasy_p_td")
}

## LIVE: from the current week's projection board, which carries the same
## components as `projected_rushing_tds` / `projected_receiving_tds`.
##
## STALENESS. On the daily job scripts/15 runs BEFORE scripts/17, because
## scripts/17 needs the box scores that scripts/15 --refresh-stats pulls. So the
## board this reads is normally the PREVIOUS run's - at most one day old on a
## daily cadence. That is acceptable for a projection whose inputs move weekly,
## but the age is logged on every call rather than hidden, and a board older
## than `max_age_days` is refused outright so a dead scheduler cannot quietly
## feed week-old numbers into live prices.
## Live-only helper for the drive-concentration recalibration: each player's
## CURRENT trailing (up to 5 completed games this season) distinct_drive_
## fraction, from a FRESH play-by-play pull. Deliberately never a long-lived
## cache (unlike the historical path's precomputed table) - an in-progress
## season's plays change week to week, and a stale cache would silently stop
## updating. Self-contained (no dependency on the research files under
## scripts/111-117) so this production path does not depend on scaffolding
## that lives outside R/td_fantasy_signal.R.
td_fantasy_live_drive_concentration <- function(seasons) {
  pbp <- nflreadr::load_pbp(seasons)
  plays <- pbp |>
    dplyr::filter(
      .data$season_type == "REG",
      !is.na(.data$yardline_100), !is.na(.data$posteam),
      is.na(.data$two_point_attempt) | .data$two_point_attempt == 0,
      .data$play_type %in% c("run", "pass"),
      is.na(.data$penalty) | .data$penalty == 0,
      .data$yardline_100 <= 10
    )
  touches <- dplyr::bind_rows(
    plays |>
      dplyr::filter(.data$play_type == "run", !is.na(.data$rusher_player_id)) |>
      dplyr::transmute(.data$season, .data$week, .data$drive,
                       player_id = .data$rusher_player_id),
    plays |>
      dplyr::filter(.data$play_type == "pass", !is.na(.data$receiver_player_id)) |>
      dplyr::transmute(.data$season, .data$week, .data$drive,
                       player_id = .data$receiver_player_id)
  )
  if (!nrow(touches)) {
    return(tibble::tibble(player_id = character(), distinct_drive_fraction_r5 = double()))
  }
  per_game <- touches |>
    dplyr::group_by(.data$season, .data$week, .data$player_id) |>
    dplyr::summarise(
      n_zone_touches = dplyr::n(),
      n_zone_drives = dplyr::n_distinct(.data$drive),
      .groups = "drop"
    ) |>
    dplyr::mutate(distinct_drive_fraction = .data$n_zone_drives / .data$n_zone_touches)

  per_game |>
    dplyr::arrange(.data$player_id, .data$season, .data$week) |>
    dplyr::group_by(.data$player_id) |>
    dplyr::summarise(
      distinct_drive_fraction_r5 = safe_mean(utils::tail(.data$distinct_drive_fraction, 5)),
      .groups = "drop"
    )
}

td_fantasy_signal_live <- function(
  path = "outputs/fantasy_prop_2026_latest_projections.csv",
  max_age_days = 8,
  calibrator_path = "data/processed/td_drive_concentration_calibrator.rds"
) {
  if (!file.exists(path)) {
    message("TD fantasy signal: no ", path, "; falling back to market+fundamental only.")
    return(NULL)
  }
  age <- as.numeric(difftime(Sys.time(), file.mtime(path), units = "days"))
  if (age > max_age_days) {
    message(sprintf(
      "TD fantasy signal: %s is %.1f days old (limit %d) - REFUSED, using market+fundamental only.",
      path, age, max_age_days))
    return(NULL)
  }
  d <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE),
    error = function(e) NULL
  )
  if (is.null(d) || !nrow(d)) return(NULL)
  need <- c("player_id", "projected_rushing_tds", "projected_receiving_tds")
  if (!all(need %in% names(d))) {
    message("TD fantasy signal: ", path, " lacks projected TD components; skipped.")
    return(NULL)
  }
  message(sprintf("TD fantasy signal: using %s (%.1f days old, %d players).",
                  basename(path), age, nrow(d)))

  base <- d |>
    dplyr::transmute(
      player_id = as.character(.data$player_id),
      rush_td = .data$projected_rushing_tds,
      rec_td = .data$projected_receiving_tds
    ) |>
    dplyr::filter(!is.na(.data$player_id)) |>
    dplyr::distinct(.data$player_id, .keep_all = TRUE)

  ## Drive-concentration recalibration (see the dated comment block above
  ## td_fantasy_prob_from_lambda()). Live path uses the DEPLOYMENT fit
  ## (trained on all available history) and a fresh current-season pbp pull
  ## for each player's own concentration tendency. Any failure here (no
  ## calibrator artifact, pbp pull error, no season column on the live board)
  ## degrades to the unmodified naive probability rather than erroring the
  ## whole board.
  calibrator <- tryCatch(readRDS(calibrator_path), error = function(e) NULL)
  if (is.null(calibrator) || is.null(calibrator$deployment) || !"season" %in% names(d)) {
    base$fantasy_p_td <- td_fantasy_prob_from_lambda(base$rush_td, base$rec_td)
    return(base |> dplyr::select("player_id", "fantasy_p_td"))
  }

  concentration <- tryCatch(
    td_fantasy_live_drive_concentration(sort(unique(d$season))),
    error = function(e) {
      message("TD fantasy signal: live drive-concentration pull failed (",
              conditionMessage(e), "); using the calibrator default for every player.")
      NULL
    }
  )
  dcf <- rep(NA_real_, nrow(base))
  if (!is.null(concentration) && nrow(concentration)) {
    dcf <- concentration$distinct_drive_fraction_r5[
      match(base$player_id, concentration$player_id)
    ]
  }
  base$fantasy_p_td <- td_fantasy_prob_from_lambda(
    base$rush_td, base$rec_td,
    distinct_drive_fraction_r5 = dcf,
    calibrator = calibrator$deployment
  )
  message(sprintf(
    "TD fantasy signal: drive-concentration matched %.1f%% of %d live players (rest use the %.3f default).",
    100 * mean(!is.na(dcf)), nrow(base), calibrator$deployment$impute_val
  ))

  base |> dplyr::select("player_id", "fantasy_p_td")
}
