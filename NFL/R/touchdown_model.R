# fit_td_fundamental() calls assert_td_schema(), so this file needs it in
# scope regardless of which script sources it - scripts/13 loads
# td_player_features.rds straight from disk and never sources
# touchdown_features.R (the only other place touchdown_registry.R gets
# pulled in), so relying on source order left assert_td_schema undefined here.
if (!exists("assert_td_schema")) source("R/touchdown_registry.R")

clip_probability <- function(x, lower = 0.005, upper = 0.995) {
  pmin(pmax(as.numeric(x), lower), upper)
}

td_model_feature_names <- function(data) {
  rolling <- names(data)[stringr::str_detect(
    names(data),
    "(_r3|_r5)$"
  )]
  context <- c(
    "week", "prior_games", "career_td_rate",
    "total_line", "team_spread", "implied_team_total", "is_home",
    "temperature", "wind_speed", "precip_probability",
    "is_dome", "grass_flag", "turf_flag", "rain_flag", "snow_flag",
    "cold_index", "high_wind_index",
    "position_qb", "position_rb", "position_wr", "position_te",
    "wind_receiving_role", "precip_rushing_role",
    "qb_rush_weather", "favorite_rush_role",
    # Not a rolled _r3/_r5 feature - a raw scalar computed directly in
    # add_td_carryover_flags(), so it needs an explicit entry here the same
    # way prior_games/career_td_rate do. Missing on the first schema-lock run
    # (caught by assert_td_schema() exactly as intended - see
    # outputs/touchdown_scoring_model_handoff.md and the message this fix
    # responds to).
    "days_since_prior_game"
  )
  intersect(unique(c(rolling, context)), names(data))
}

td_candidate_universe <- function(data) {
  data |>
    dplyr::filter(
      .data$prior_games >= 1,
      dplyr::coalesce(.data$touches_r3, 0) >= 0.5 |
        .data$position_qb == 1
    )
}

td_matrix_pair <- function(train, test, features) {
  train_x <- train[, features, drop = FALSE]
  test_x <- test[, features, drop = FALSE]
  medians <- vapply(
    train_x,
    function(x) stats::median(as.numeric(x), na.rm = TRUE),
    numeric(1)
  )
  medians[!is.finite(medians)] <- 0
  for (feature in features) {
    train_x[[feature]] <- as.numeric(train_x[[feature]])
    test_x[[feature]] <- as.numeric(test_x[[feature]])
    train_x[[feature]][!is.finite(train_x[[feature]])] <- medians[[feature]]
    test_x[[feature]][!is.finite(test_x[[feature]])] <- medians[[feature]]
  }
  list(
    train = as.matrix(train_x),
    test = as.matrix(test_x),
    medians = medians
  )
}

fit_td_fundamental <- function(train, test, seed = 20260727L) {
  features <- td_model_feature_names(train)
  # Every fit - walk-forward fold or deployment - goes through here, so this
  # is the single choke point to guard rather than scripts/13's top level.
  # allow_additions covers exactly the redzone/carryover columns; anything
  # else added or removed is scope drift and stops the run.
  assert_td_schema(
    features,
    allow_additions = c(td_redzone_expected_features(),
                        td_snap_expected_features())
  )
  matrices <- td_matrix_pair(train, test, features)
  # The R package ignores params$seed and takes its RNG state from R, so
  # subsample/colsample draws are only reproducible via set.seed().
  set.seed(seed)
  fit <- xgboost::xgb.train(
    params = list(
      objective = "binary:logistic",
      eval_metric = "logloss",
      learning_rate = 0.035,
      max_depth = 3,
      min_child_weight = 12,
      subsample = 0.8,
      colsample_bytree = 0.8,
      reg_lambda = 2,
      reg_alpha = 0.1,
      nthread = 1,
      verbosity = 0
    ),
    data = xgboost::xgb.DMatrix(
      matrices$train,
      label = train$anytime_td
    ),
    nrounds = 250
  )
  list(
    fit = portable_booster(fit),
    prediction = clip_probability(
      stats::predict(fit, matrices$test)
    ),
    features = features,
    medians = matrices$medians
  )
}

fit_platt_calibrator <- function(probability, outcome) {
  probability <- clip_probability(probability)
  frame <- data.frame(
    outcome = as.integer(outcome),
    model_logit = stats::qlogis(probability)
  )
  fit <- tryCatch(
    stats::glm(
      outcome ~ model_logit,
      family = stats::binomial(),
      data = frame
    ),
    error = function(e) NULL
  )
  if (is.null(fit) || any(!is.finite(stats::coef(fit)))) return(NULL)
  fit
}

apply_platt_calibrator <- function(fit, probability) {
  if (is.null(fit)) return(clip_probability(probability))
  clip_probability(stats::predict(
    fit,
    newdata = data.frame(
      model_logit = stats::qlogis(clip_probability(probability))
    ),
    type = "response"
  ))
}

## Positions that get their own blend weight instead of the global one.
##
## TE only, and it is earned rather than assumed. Measured on walk-forward
## predictions, the fundamental model carries materially more information at
## tight end than anywhere else:
##
##   global blend    fundamental 0.155 | market 0.945
##   TE-specific     fundamental 0.506 | market 0.566
##
## i.e. the global weight throws away roughly two thirds of the model's TE
## signal. Confirmed on a 2025 holdout that was untouched during the search:
## TE Brier 0.134963 vs 0.135612 global, gain +0.00065, week-block CI
## [+0.000008, +0.001276], P(gain<=0) = 0.023.
##
## Nothing else qualified. A 525-cell scan over position x player archetype x
## team style x opponent-defense flavor (including RB-vs-pass-defense, WR on
## short-throwing offenses, TE on run-heavy teams) produced ZERO survivors
## after BH correction - the nominal winners were the ~26 false positives you
## expect from 525 tests. QB is actively worse than useless (beta_fundamental
## -0.036), so it stays on the global weight where the market dominates it.
td_positional_calibration <- function() "TE"

## The calibrator gains a third input when the fantasy model's TD projection is
## available (R/td_fantasy_signal.R). Measured on 2023-2025 it is the strongest
## of the three - the TD fundamental's own coefficient goes NEGATIVE once it is
## included - so it is used whenever present and simply omitted when not, which
## keeps every historical artifact and any caller without fantasy data working.
## Returns a PAIR: the preferred fit (three-input where the fantasy projection
## is available) and a two-input fallback fit on the same rows.
##
## Both are needed because a glm fitted with `fantasy_logit` returns NA for any
## row missing it, and you cannot simply drop the term from a fitted model - the
## intercept and the other coefficients were estimated assuming it was there.
## Players the fantasy model does not cover therefore get scored by a genuine
## two-input model rather than a mutilated three-input one.
.fit_one_market_calibrator <- function(frame) {
  if (nrow(frame) < 500 || length(unique(frame$outcome)) < 2) return(NULL)

  fit_form <- function(form, d) {
    f <- tryCatch(stats::glm(form, family = stats::binomial(), data = d),
                  error = function(e) NULL)
    if (is.null(f) || any(!is.finite(stats::coef(f)))) return(NULL)
    f
  }
  base <- fit_form(outcome ~ fundamental_logit + market_logit, frame)
  if (is.null(base)) return(NULL)

  full <- NULL
  if ("fantasy_logit" %in% names(frame) &&
      sum(is.finite(frame$fantasy_logit)) >= 500) {
    full <- fit_form(
      outcome ~ fundamental_logit + market_logit + fantasy_logit, frame
    )
  }
  list(full = full, base = base, uses_fantasy = !is.null(full))
}

fit_market_calibrator <- function(data) {
  base <- data |>
    dplyr::filter(
      !is.na(.data$anytime_td),
      is.finite(.data$fundamental_probability),
      is.finite(.data$consensus_probability)
    )
  frame <- base |>
    dplyr::transmute(
      outcome = as.integer(.data$anytime_td),
      fundamental_logit = stats::qlogis(
        clip_probability(.data$fundamental_probability)
      ),
      market_logit = stats::qlogis(
        clip_probability(.data$consensus_probability)
      )
    )
  ## `frame` MUST stay row-aligned with `base`, because the per-position loop
  ## below subsets it using indices computed against `base`. So the fantasy
  ## column is added with NA where the fantasy model has no projection, and
  ## glm's default na.omit drops those rows at fit time - filtering here instead
  ## would silently shift every positional subset.
  if ("fantasy_p_td" %in% names(base)) {
    fl <- rep(NA_real_, nrow(base))
    ok <- is.finite(base$fantasy_p_td)
    fl[ok] <- stats::qlogis(clip_probability(base$fantasy_p_td[ok]))
    frame$fantasy_logit <- fl
  }
  global <- .fit_one_market_calibrator(frame)
  if (is.null(global)) return(NULL)

  ## Per-position calibrators, where the data supports one. Falls back to the
  ## global fit for any position without enough rows, so a thin season can
  ## never silently produce a wild positional weight.
  by_pos <- list()
  if ("position" %in% names(base)) {
    for (po in td_positional_calibration()) {
      idx <- which(base$position == po)
      if (length(idx) >= 500) {
        f <- .fit_one_market_calibrator(frame[idx, , drop = FALSE])
        if (!is.null(f)) by_pos[[po]] <- f
      }
    }
  }
  structure(list(global = global, by_position = by_pos), class = "td_market_cal")
}

## `position` is optional so every existing caller keeps working, and a fit
## saved before positional calibration existed (a bare glm rather than a
## td_market_cal) is still honoured - otherwise reloading an older
## td_deployment_model.rds would error instead of degrading to the global blend.
apply_market_calibrator <- function(fit, fundamental, market, position = NULL,
                                    fantasy = NULL) {
  if (is.null(fit)) return(clip_probability(fundamental))

  newdata <- data.frame(
    fundamental_logit = stats::qlogis(clip_probability(fundamental)),
    market_logit = stats::qlogis(clip_probability(market))
  )
  ## A calibrator fitted WITH the fantasy term cannot score rows that lack it.
  ## Rather than error or silently drop those players, they fall back to a
  ## two-input prediction built from the same fit's coefficients - see
  ## .predict_cal() below.
  if (!is.null(fantasy) && length(fantasy) == nrow(newdata)) {
    f <- rep(NA_real_, nrow(newdata))
    okf <- is.finite(fantasy)
    f[okf] <- stats::qlogis(clip_probability(fantasy[okf]))
    newdata$fantasy_logit <- f
  }
  ## Scores with the three-input fit where the fantasy projection exists and
  ## with the paired two-input fit everywhere else.
  predict_with <- function(f, idx = NULL) {
    nd <- if (is.null(idx)) newdata else newdata[idx, , drop = FALSE]
    if (!is.list(f) || inherits(f, "glm")) {
      return(as.numeric(stats::predict(f, newdata = nd, type = "response")))
    }
    out <- as.numeric(stats::predict(f$base, newdata = nd, type = "response"))
    if (isTRUE(f$uses_fantasy) && "fantasy_logit" %in% names(nd)) {
      ok <- is.finite(nd$fantasy_logit)
      if (any(ok)) {
        out[ok] <- as.numeric(stats::predict(
          f$full, newdata = nd[ok, , drop = FALSE], type = "response"
        ))
      }
    }
    out
  }

  ## Legacy shape: a plain glm, no positional component.
  if (!inherits(fit, "td_market_cal")) return(clip_probability(predict_with(fit)))

  out <- predict_with(fit$global)
  if (length(fit$by_position) && !is.null(position) &&
      length(position) == nrow(newdata)) {
    for (po in names(fit$by_position)) {
      idx <- which(as.character(position) == po)
      if (length(idx)) out[idx] <- predict_with(fit$by_position[[po]], idx)
    }
  }
  clip_probability(out)
}

walk_forward_td_predictions <- function(
  player_features,
  prop_board,
  test_seasons = 2023:2025,
  seed = 20260727L
) {
  universe <- td_candidate_universe(player_features)
  season_boards <- list()
  artifacts <- list()

  for (test_season in test_seasons) {
    calibration_season <- test_season - 1L
    train <- universe |>
      dplyr::filter(.data$season < calibration_season)
    calibration <- universe |>
      dplyr::filter(.data$season == calibration_season)
    test <- universe |>
      dplyr::filter(.data$season == test_season)
    if (!nrow(train) || !nrow(calibration) || !nrow(test)) next

    combined <- dplyr::bind_rows(
      dplyr::mutate(calibration, prediction_set = "calibration"),
      dplyr::mutate(test, prediction_set = "test")
    )
    fundamental <- fit_td_fundamental(
      train,
      combined,
      seed = seed + test_season
    )
    calibration_raw <- fundamental$prediction[
      combined$prediction_set == "calibration"
    ]
    test_raw <- fundamental$prediction[
      combined$prediction_set == "test"
    ]
    platt <- fit_platt_calibrator(
      calibration_raw,
      calibration$anytime_td
    )
    test_probability <- apply_platt_calibrator(platt, test_raw)

    player_predictions <- test |>
      dplyr::transmute(
        .data$game_id,
        .data$player_id,
        fundamental_probability = test_probability
      )
    season_board <- prop_board |>
      dplyr::filter(
        .data$season == test_season,
        !is.na(.data$anytime_td),
        .data$prior_games >= 1
      ) |>
      dplyr::left_join(
        player_predictions,
        by = c("game_id", "player_id"),
        relationship = "many-to-one"
      ) |>
      dplyr::filter(!is.na(.data$fundamental_probability))

    season_boards[[as.character(test_season)]] <- season_board
    artifacts[[as.character(test_season)]] <- list(
      fundamental_fit = fundamental$fit,
      features = fundamental$features,
      medians = fundamental$medians,
      platt_fit = platt
    )
  }

  predictions <- dplyr::bind_rows(season_boards)

  ## Fantasy-model TD projection as a calibrator input. See
  ## R/td_fantasy_signal.R for the measurement that justifies it: on the 2025
  ## holdout it is worth +0.00142 Brier, ~22x the TE blend, and it drives the
  ## TD fundamental's own coefficient negative.
  if (!exists("td_fantasy_signal_history")) source("R/td_fantasy_signal.R")
  fsig <- td_fantasy_signal_history()
  if (!is.null(fsig) && nrow(fsig)) {
    n_before <- nrow(predictions)
    predictions <- predictions |>
      dplyr::left_join(fsig, by = c("game_id", "player_id"))
    if (nrow(predictions) != n_before) {
      stop("Fantasy TD signal join changed row count: ", n_before, " -> ",
           nrow(predictions), call. = FALSE)
    }
    message(sprintf("TD fantasy signal: matched %.1f%% of %s walk-forward rows.",
                    100 * mean(is.finite(predictions$fantasy_p_td)),
                    format(nrow(predictions), big.mark = ",")))
  } else {
    message("TD fantasy signal unavailable; calibrator uses market + fundamental only.")
  }

  predictions$model_probability <- NA_real_
  market_fits <- list()

  for (test_season in test_seasons) {
    current <- predictions$season == test_season
    prior <- predictions$season < test_season
    market_fit <- fit_market_calibrator(predictions[prior, ])
    predictions$model_probability[current] <- apply_market_calibrator(
      market_fit,
      predictions$fundamental_probability[current],
      predictions$consensus_probability[current],
      position = predictions$position[current],
      fantasy = if ("fantasy_p_td" %in% names(predictions)) {
        predictions$fantasy_p_td[current]
      } else {
        NULL
      }
    )
    market_fits[[as.character(test_season)]] <- market_fit
  }

  predictions <- predictions |>
    dplyr::mutate(
      decimal_odds = dplyr::if_else(
        .data$best_american_odds > 0,
        1 + .data$best_american_odds / 100,
        1 + 100 / abs(.data$best_american_odds)
      ),
      probability_edge = .data$model_probability -
        .data$best_implied_probability,
      relative_edge = .data$probability_edge /
        .data$best_implied_probability,
      expected_roi = .data$model_probability * .data$decimal_odds - 1,
      won = as.integer(.data$anytime_td == 1),
      flat_profit = dplyr::if_else(
        .data$won == 1,
        .data$decimal_odds - 1,
        -1
      )
    )

  list(
    predictions = predictions,
    artifacts = artifacts,
    market_fits = market_fits
  )
}

td_probability_metrics <- function(data) {
  data |>
    dplyr::group_by(.data$season) |>
    dplyr::summarise(
      player_games = dplyr::n(),
      actual_td_rate = mean(.data$anytime_td),
      predicted_td_rate = mean(.data$model_probability),
      brier = mean((.data$model_probability - .data$anytime_td)^2),
      log_loss = -mean(
        .data$anytime_td * log(clip_probability(.data$model_probability)) +
          (1 - .data$anytime_td) *
            log(1 - clip_probability(.data$model_probability))
      ),
      market_brier = mean(
        (.data$consensus_probability - .data$anytime_td)^2
      ),
      .groups = "drop"
    )
}

fit_td_deployment_model <- function(
  player_features,
  walk_forward_predictions,
  calibration_season = 2025L,
  seed = 20260727L
) {
  universe <- td_candidate_universe(player_features)
  train <- universe |>
    dplyr::filter(.data$season < calibration_season)
  calibration <- universe |>
    dplyr::filter(.data$season == calibration_season)
  fundamental <- fit_td_fundamental(
    train,
    calibration,
    seed = seed
  )
  platt <- fit_platt_calibrator(
    fundamental$prediction,
    calibration$anytime_td
  )
  market <- fit_market_calibrator(walk_forward_predictions)
  importance <- xgboost::xgb.importance(
    feature_names = fundamental$features,
    model = fundamental$fit
  )

  list(
    fundamental_fit = fundamental$fit,
    features = fundamental$features,
    medians = fundamental$medians,
    platt_fit = platt,
    market_fit = market,
    calibration_season = calibration_season,
    feature_importance = importance
  )
}
