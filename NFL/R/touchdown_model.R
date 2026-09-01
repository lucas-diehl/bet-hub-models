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
    "qb_rush_weather", "favorite_rush_role"
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

fit_market_calibrator <- function(data) {
  frame <- data |>
    dplyr::filter(
      !is.na(.data$anytime_td),
      is.finite(.data$fundamental_probability),
      is.finite(.data$consensus_probability)
    ) |>
    dplyr::transmute(
      outcome = as.integer(.data$anytime_td),
      fundamental_logit = stats::qlogis(
        clip_probability(.data$fundamental_probability)
      ),
      market_logit = stats::qlogis(
        clip_probability(.data$consensus_probability)
      )
    )
  if (nrow(frame) < 500 || length(unique(frame$outcome)) < 2) return(NULL)
  fit <- tryCatch(
    stats::glm(
      outcome ~ fundamental_logit + market_logit,
      family = stats::binomial(),
      data = frame
    ),
    error = function(e) NULL
  )
  if (is.null(fit) || any(!is.finite(stats::coef(fit)))) return(NULL)
  fit
}

apply_market_calibrator <- function(fit, fundamental, market) {
  if (is.null(fit)) return(clip_probability(fundamental))
  clip_probability(stats::predict(
    fit,
    newdata = data.frame(
      fundamental_logit = stats::qlogis(clip_probability(fundamental)),
      market_logit = stats::qlogis(clip_probability(market))
    ),
    type = "response"
  ))
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
  predictions$model_probability <- NA_real_
  market_fits <- list()

  for (test_season in test_seasons) {
    current <- predictions$season == test_season
    prior <- predictions$season < test_season
    market_fit <- fit_market_calibrator(predictions[prior, ])
    predictions$model_probability[current] <- apply_market_calibrator(
      market_fit,
      predictions$fundamental_probability[current],
      predictions$consensus_probability[current]
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
