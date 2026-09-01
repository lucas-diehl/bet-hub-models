challenger_numeric_features <- function(data) {
  excluded <- c(
    "season", "home_score", "away_score", "home_score_rw", "away_score_rw",
    "home_margin", "game_total", "home_line", "total_line"
  )
  candidates <- setdiff(names(data), excluded)
  candidates <- candidates[
    vapply(data[candidates], is.numeric, logical(1))
  ]
  forbidden <- candidates[
    stringr::str_detect(
      candidates,
      "^matchup_(diff|sum)_score(_rw)?$"
    )
  ]
  if (length(forbidden)) {
    stop(
      "Outcome leakage detected in challenger features: ",
      paste(forbidden, collapse = ", ")
    )
  }
  candidates
}

add_challenger_features <- function(data) {
  out <- data |>
    dplyr::mutate(
      abs_market_margin = abs(.data$market_margin),
      late_season = as.numeric(.data$week >= 15),
      early_season = as.numeric(.data$week <= 4),
      is_dome = as.numeric(tolower(dplyr::coalesce(.data$surface, "")) == "dome"),
      cold_game = as.numeric(.data$neutral_temperature <= 35),
      high_wind = as.numeric(.data$neutral_wind >= 15),
      wind_total_interaction = .data$neutral_wind * .data$market_total,
      implied_home_points = (.data$market_total + .data$market_margin) / 2,
      implied_away_points = (.data$market_total - .data$market_margin) / 2,
      distance_to_key_3 = abs(.data$abs_market_margin - 3),
      distance_to_key_7 = abs(.data$abs_market_margin - 7)
    )

  home_names <- names(out)[stringr::str_starts(names(out), "home_")]
  for (home_name in home_names) {
    stem <- stringr::str_remove(home_name, "^home_")
    if (stem %in% c("score", "score_rw")) next
    away_name <- paste0("away_", stem)
    if (
      away_name %in% names(out) &&
        is.numeric(out[[home_name]]) &&
        is.numeric(out[[away_name]])
    ) {
      out[[paste0("matchup_diff_", stem)]] <-
        out[[home_name]] - out[[away_name]]
      out[[paste0("matchup_sum_", stem)]] <-
        out[[home_name]] + out[[away_name]]
    }
  }
  out
}

challenger_target_columns <- function(target) {
  if (target == "home_margin") {
    list(truth = "home_margin", market = "market_margin")
  } else if (target == "game_total") {
    list(truth = "game_total", market = "market_total")
  } else {
    stop("Unknown target: ", target)
  }
}

recency_case_weights <- function(season, reference_season, half_life) {
  if (!is.finite(half_life)) return(rep(1, length(season)))
  weights <- 0.5^((reference_season - season) / half_life)
  weights / mean(weights)
}

prepare_challenger_xy <- function(train, test, features) {
  train_x <- train[, features, drop = FALSE]
  test_x <- test[, features, drop = FALSE]
  medians <- vapply(
    train_x,
    function(x) stats::median(x, na.rm = TRUE),
    numeric(1)
  )
  medians[!is.finite(medians)] <- 0
  for (feature in features) {
    train_x[[feature]][!is.finite(train_x[[feature]])] <- medians[[feature]]
    test_x[[feature]][!is.finite(test_x[[feature]])] <- medians[[feature]]
  }
  list(train = train_x, test = test_x)
}

weighted_ridge_predict <- function(
    train_x,
    train_y,
    test_x,
    weights,
    lambda) {
  x <- as.matrix(train_x)
  new_x <- as.matrix(test_x)
  weight_sum <- sum(weights)
  center <- colSums(x * weights) / weight_sum
  centered <- sweep(x, 2, center, "-")
  scale <- sqrt(colSums(centered^2 * weights) / weight_sum)
  scale[!is.finite(scale) | scale < 1e-8] <- 1
  x_scaled <- sweep(centered, 2, scale, "/")
  new_scaled <- sweep(sweep(new_x, 2, center, "-"), 2, scale, "/")
  y_center <- sum(train_y * weights) / weight_sum
  y_centered <- train_y - y_center
  root_weight <- sqrt(weights)
  weighted_x <- x_scaled * root_weight
  weighted_y <- y_centered * root_weight
  penalty <- diag(lambda, ncol(weighted_x))
  beta <- tryCatch(
    solve(
      crossprod(weighted_x) + penalty,
      crossprod(weighted_x, weighted_y)
    ),
    error = function(e) qr.solve(
      crossprod(weighted_x) + penalty,
      crossprod(weighted_x, weighted_y)
    )
  )
  as.numeric(y_center + new_scaled %*% beta)
}

challenger_parameter_grid <- function(model) {
  if (model == "residual_ridge") {
    return(tidyr::crossing(
      lambda = c(10, 100, 500),
      half_life = c(5, Inf)
    ))
  }
  if (model == "residual_rf") {
    return(tibble::tibble(
      mtry_fraction = 0.35,
      min_node_size = 25L,
      half_life = c(5, Inf)
    ))
  }
  if (model == "residual_xgb") {
    return(tibble::tibble(
      max_depth = 2L,
      eta = 0.05,
      half_life = c(5, Inf)
    ))
  }
  if (model == "residual_gam") {
    return(tibble::tibble(half_life = c(5, Inf)))
  }
  stop("Unknown challenger: ", model)
}

fit_predict_residual_model <- function(
    model,
    train,
    test,
    target,
    parameters,
    seed = 1L) {
  columns <- challenger_target_columns(target)
  train_y <- train[[columns$truth]] - train[[columns$market]]
  features <- challenger_numeric_features(train)
  xy <- prepare_challenger_xy(train, test, features)
  half_life <- as.numeric(parameters$half_life[[1]])
  weights <- recency_case_weights(
    train$season,
    max(train$season),
    half_life
  )

  if (model == "residual_ridge") {
    return(weighted_ridge_predict(
      xy$train,
      train_y,
      xy$test,
      weights,
      as.numeric(parameters$lambda[[1]])
    ))
  }

  if (model == "residual_rf") {
    fit_data <- cbind(residual_target = train_y, xy$train)
    fit <- ranger::ranger(
      residual_target ~ .,
      data = fit_data,
      num.trees = 150,
      mtry = max(
        1L,
        floor(
          ncol(xy$train) *
            as.numeric(parameters$mtry_fraction[[1]])
        )
      ),
      min.node.size = as.integer(parameters$min_node_size[[1]]),
      case.weights = weights,
      seed = seed,
      num.threads = 1
    )
    return(as.numeric(
      stats::predict(fit, data = xy$test, num.threads = 1)$predictions
    ))
  }

  if (model == "residual_xgb") {
    # The R package ignores params$seed; set.seed() is what actually controls
    # the subsample/colsample draws.
    set.seed(seed)
    fit <- xgboost::xgb.train(
      params = list(
        objective = "reg:squarederror",
        eta = as.numeric(parameters$eta[[1]]),
        max_depth = as.integer(parameters$max_depth[[1]]),
        min_child_weight = 15,
        subsample = 0.8,
        colsample_bytree = 0.7,
        nthread = 1
      ),
      data = xgboost::xgb.DMatrix(
        as.matrix(xy$train),
        label = train_y,
        weight = weights
      ),
      nrounds = 150,
      verbose = 0
    )
    return(as.numeric(stats::predict(
      fit,
      xgboost::xgb.DMatrix(as.matrix(xy$test))
    )))
  }

  if (model == "residual_gam") {
    gam_features <- intersect(
      c(
        "market_total", "abs_market_margin", "neutral_wind",
        "neutral_temperature", "matchup_diff_off_epa_r4",
        "matchup_sum_off_plays_r4", "matchup_diff_pass_epa_r4",
        "matchup_diff_def_epa_r4", "matchup_sum_explosive_rate_r4",
        "matchup_sum_giveaway_rate_r4", "late_season"
      ),
      features
    )
    gam_train <- xy$train[, gam_features, drop = FALSE]
    gam_test <- xy$test[, gam_features, drop = FALSE]
    gam_train$residual_target <- train_y
    smooth_features <- intersect(
      c(
        "market_total", "abs_market_margin",
        "neutral_wind", "neutral_temperature"
      ),
      gam_features
    )
    linear_features <- setdiff(gam_features, smooth_features)
    terms <- c(
      paste0("s(", smooth_features, ", k = 5)"),
      linear_features
    )
    formula <- stats::as.formula(
      paste("residual_target ~", paste(terms, collapse = " + "))
    )
    fit <- mgcv::gam(
      formula,
      data = gam_train,
      weights = weights,
      method = "REML",
      select = TRUE
    )
    return(as.numeric(stats::predict(fit, newdata = gam_test)))
  }

  stop("Unknown residual model: ", model)
}

select_and_predict_challenger <- function(
    data,
    test_season,
    target,
    model,
    validation_seasons = 2L,
    seed = 1L) {
  prior_seasons <- sort(unique(data$season[data$season < test_season]))
  validation_years <- tail(prior_seasons, validation_seasons)
  base_train <- data |>
    dplyr::filter(.data$season < min(validation_years))
  validation <- data |>
    dplyr::filter(.data$season %in% validation_years)
  full_train <- data |>
    dplyr::filter(.data$season < test_season)
  test <- data |>
    dplyr::filter(.data$season == test_season)
  if (
    length(validation_years) < validation_seasons ||
      nrow(base_train) < 500 ||
      !nrow(validation) ||
      !nrow(test)
  ) {
    return(NULL)
  }

  columns <- challenger_target_columns(target)
  alpha_grid <- c(0.25, 0.5, 0.75, 1)
  grid <- challenger_parameter_grid(model)
  validation_results <- vector("list", nrow(grid))

  for (row_index in seq_len(nrow(grid))) {
    parameters <- grid[row_index, , drop = FALSE]
    raw_prediction <- fit_predict_residual_model(
      model,
      base_train,
      validation,
      target,
      parameters,
      seed + row_index
    )
    alpha_mae <- vapply(alpha_grid, function(alpha) {
      prediction <- validation[[columns$market]] + alpha * raw_prediction
      mae_vec(validation[[columns$truth]], prediction)
    }, numeric(1))
    best_alpha_index <- which.min(alpha_mae)
    validation_results[[row_index]] <- dplyr::bind_cols(
      parameters,
      tibble::tibble(
        alpha = alpha_grid[[best_alpha_index]],
        validation_mae = alpha_mae[[best_alpha_index]]
      )
    )
  }

  tournament <- dplyr::bind_rows(validation_results) |>
    dplyr::arrange(.data$validation_mae)
  selected <- tournament[1, , drop = FALSE]
  raw_test <- fit_predict_residual_model(
    model,
    full_train,
    test,
    target,
    selected,
    seed + test_season
  )
  prediction <- test[[columns$market]] + selected$alpha[[1]] * raw_test

  prediction_output <- test |>
    dplyr::transmute(
      .data$game_id, .data$season, .data$week, .data$game_date,
      .data$home_team, .data$away_team,
      target = target,
      model = model,
      truth = .data[[columns$truth]],
      prediction = prediction,
      raw_residual_prediction = raw_test,
      shrinkage_alpha = selected$alpha[[1]],
      market_prediction = .data[[columns$market]],
      .data$home_line, .data$total_line,
      .data$market_margin, .data$market_total,
      .data$neutral_temperature, .data$neutral_wind,
      .data$surface
    )
  specification <- selected |>
    dplyr::mutate(
      test_season = test_season,
      target = target,
      model = model,
      validation_start = min(validation_years),
      validation_end = max(validation_years),
      .before = 1
    )
  list(
    predictions = prediction_output,
    specification = specification,
    tournament = tournament
  )
}

walk_forward_challengers <- function(data, cfg) {
  enhanced <- add_challenger_features(data)
  cache_directory <- "data/processed/challenger_cache_v2"
  dir.create(cache_directory, recursive = TRUE, showWarnings = FALSE)
  seasons <- seq(cfg$backtest$start_season, cfg$backtest$end_season)
  targets <- c("home_margin", "game_total")
  models <- c(
    "residual_ridge", "residual_rf",
    "residual_xgb", "residual_gam"
  )
  predictions <- list()
  specifications <- list()
  prediction_index <- 1L
  specification_index <- 1L

  for (test_season in seasons) {
    for (target in targets) {
      for (model in models) {
        message(
          "Challenger season ", test_season,
          "; ", target, "; ", model
        )
        cache_path <- file.path(
          cache_directory,
          paste(test_season, target, model, "rds", sep = ".")
        )
        if (file.exists(cache_path)) {
          result <- readRDS(cache_path)
        } else {
          result <- select_and_predict_challenger(
            enhanced,
            test_season,
            target,
            model,
            cfg$backtest$validation_seasons,
            cfg$backtest$seed
          )
          saveRDS(result, cache_path)
        }
        if (is.null(result)) next
        predictions[[prediction_index]] <- result$predictions
        specifications[[specification_index]] <- result$specification
        prediction_index <- prediction_index + 1L
        specification_index <- specification_index + 1L
      }
    }
  }
  list(
    predictions = dplyr::bind_rows(predictions),
    specifications = dplyr::bind_rows(specifications)
  )
}

add_sequential_cover_probabilities <- function(predictions) {
  predictions |>
    dplyr::group_by(.data$target, .data$model) |>
    dplyr::group_modify(function(.x, .y) {
      seasons <- sort(unique(.x$season))
      purrr::map_dfr(seasons, function(test_season) {
        current <- dplyr::filter(.x, .data$season == test_season)
        history <- dplyr::filter(.x, .data$season < test_season) |>
          dplyr::mutate(
            edge = .data$prediction - .data$market_prediction,
            result_vs_market = .data$truth - .data$market_prediction,
            selected_win = as.numeric(
              .data$edge * .data$result_vs_market > 0
            ),
            selected_side_positive = as.numeric(.data$edge > 0),
            absolute_edge = abs(.data$edge)
          ) |>
          dplyr::filter(
            abs(.data$result_vs_market) > 1e-9,
            .data$absolute_edge > 1e-9
          )
        current <- current |>
          dplyr::mutate(
            edge = .data$prediction - .data$market_prediction,
            selected_side_positive = as.numeric(.data$edge > 0),
            absolute_edge = abs(.data$edge)
          )
        if (nrow(history) < 300 || length(unique(history$selected_win)) < 2) {
          current$cover_probability <- NA_real_
          return(current)
        }
        calibration <- stats::glm(
          selected_win ~ absolute_edge + selected_side_positive,
          data = history,
          family = stats::binomial()
        )
        probability <- as.numeric(stats::predict(
          calibration,
          newdata = current,
          type = "response"
        ))
        current$cover_probability <- pmin(pmax(probability, 0.40), 0.75)
        current
      })
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      break_even_probability = 110 / 210,
      expected_value = .data$cover_probability * (100 / 110) -
        (1 - .data$cover_probability)
    )
}

grade_probability_bets <- function(predictions, probability_threshold) {
  predictions |>
    dplyr::filter(
      is.finite(.data$cover_probability),
      .data$cover_probability >= probability_threshold,
      abs(.data$edge) > 1e-9
    ) |>
    dplyr::mutate(
      bet_side = dplyr::case_when(
        .data$target == "home_margin" & .data$edge > 0 ~ "home",
        .data$target == "home_margin" ~ "away",
        .data$edge > 0 ~ "over",
        TRUE ~ "under"
      ),
      raw_result = .data$truth - .data$market_prediction,
      bet_result = dplyr::if_else(
        .data$edge > 0,
        signed_result(.data$raw_result),
        -signed_result(.data$raw_result)
      ),
      profit = american_profit(.data$bet_result, -110)
    )
}

select_probability_bets_walk_forward <- function(
    predictions,
    validation_seasons = 2L,
    minimum_bets = 40L) {
  cutoffs <- c(0.525, 0.54, 0.55, 0.56, 0.58, 0.60)
  seasons <- sort(unique(predictions$season))
  purrr::map_dfr(seasons, function(test_season) {
    validation_years <- tail(
      seasons[seasons < test_season],
      validation_seasons
    )
    if (length(validation_years) < validation_seasons) {
      return(dplyr::tibble())
    }
    purrr::map_dfr(unique(predictions$target), function(target_name) {
      purrr::map_dfr(unique(predictions$model), function(model_name) {
        history <- predictions |>
          dplyr::filter(
            .data$season %in% validation_years,
            .data$target == target_name,
            .data$model == model_name
          )
        candidate_rows <- purrr::map_dfr(cutoffs, function(cutoff) {
          graded <- grade_probability_bets(history, cutoff)
          if (!nrow(graded)) return(tibble::tibble())
          graded |>
            dplyr::summarise(
              probability_threshold = cutoff,
              bets = dplyr::n(),
              validation_seasons_available =
                dplyr::n_distinct(.data$season),
              profit = sum(.data$profit),
              roi = mean(.data$profit)
            )
        })
        if (!nrow(candidate_rows) || !"bets" %in% names(candidate_rows)) {
          return(dplyr::tibble())
        }
        candidates <- candidate_rows |>
          dplyr::filter(
            .data$bets >= minimum_bets,
            .data$validation_seasons_available == validation_seasons
          ) |>
          dplyr::arrange(dplyr::desc(.data$roi), dplyr::desc(.data$bets))
        if (!nrow(candidates)) return(dplyr::tibble())
        selected <- candidates[1, ]
        predictions |>
          dplyr::filter(
            .data$season == test_season,
            .data$target == target_name,
            .data$model == model_name
          ) |>
          grade_probability_bets(selected$probability_threshold) |>
          dplyr::mutate(
            selected_probability_threshold =
              selected$probability_threshold,
            validation_start = min(validation_years),
            validation_end = max(validation_years),
            validation_bets = selected$bets,
            validation_roi = selected$roi
          )
      })
    })
  })
}

add_locked_interaction_tags <- function(bets) {
  bets |>
    dplyr::mutate(
      favorite_strength = dplyr::case_when(
        abs(.data$home_line) >= 7 ~ "heavy_7_plus",
        abs(.data$home_line) >= 3 ~ "medium_3_to_6.5",
        TRUE ~ "small_0_to_2.5"
      ),
      interaction_over_heavy_favorite =
        .data$target == "game_total" &
        .data$bet_side == "over" &
        abs(.data$home_line) >= 7,
      interaction_under_small_favorite =
        .data$target == "game_total" &
        .data$bet_side == "under" &
        abs(.data$home_line) <= 2.5,
      interaction_under_high_wind =
        .data$target == "game_total" &
        .data$bet_side == "under" &
        .data$neutral_wind >= 15,
      interaction_late_season_under =
        .data$target == "game_total" &
        .data$bet_side == "under" &
        .data$week >= 15,
      interaction_home_spread =
        .data$target == "home_margin" &
        .data$bet_side == "home",
      prediction_margin = dplyr::if_else(
        .data$target == "home_margin",
        .data$prediction,
        NA_real_
      ),
      market_margin = dplyr::if_else(
        .data$target == "home_margin",
        -.data$home_line,
        .data$market_margin
      ),
      interaction_crosses_key_3 =
        .data$target == "home_margin" &
        (
          (.data$market_margin < 3 & .data$prediction_margin >= 3) |
          (.data$market_margin > 3 & .data$prediction_margin <= 3) |
          (.data$market_margin < -3 & .data$prediction_margin >= -3) |
          (.data$market_margin > -3 & .data$prediction_margin <= -3)
        ),
      interaction_crosses_key_7 =
        .data$target == "home_margin" &
        (
          (.data$market_margin < 7 & .data$prediction_margin >= 7) |
          (.data$market_margin > 7 & .data$prediction_margin <= 7) |
          (.data$market_margin < -7 & .data$prediction_margin >= -7) |
          (.data$market_margin > -7 & .data$prediction_margin <= -7)
        )
    )
}

summarize_locked_interactions <- function(bets) {
  tagged <- add_locked_interaction_tags(bets) |>
    dplyr::mutate(
      evaluation = dplyr::if_else(
        .data$season <= 2022,
        "discovery_2020_2022",
        "validation_2023_2025"
      )
    )
  tag_names <- names(tagged)[
    stringr::str_starts(names(tagged), "interaction_")
  ]
  purrr::map_dfr(tag_names, function(tag_name) {
    tagged |>
      dplyr::filter(.data[[tag_name]]) |>
      dplyr::group_by(
        .data$evaluation, .data$target, .data$model
      ) |>
      dplyr::summarise(
        interaction = stringr::str_remove(tag_name, "^interaction_"),
        bets = dplyr::n(),
        wins = sum(.data$bet_result > 0),
        losses = sum(.data$bet_result < 0),
        pushes = sum(.data$bet_result == 0),
        win_rate = .data$wins / (.data$wins + .data$losses),
        profit_units = sum(.data$profit),
        roi = mean(.data$profit),
        .groups = "drop"
      )
  })
}
