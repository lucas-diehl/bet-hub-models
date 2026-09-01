feature_names <- function(data) {
  excluded <- c(
    "game_id", "season", "week", "game_date", "home_team", "away_team",
    "home_score", "away_score", "home_score_rw", "away_score_rw",
    "home_margin", "game_total", "home_line", "total_line",
    "market_margin", "market_total", "surface", "weather", "precip_type"
  )
  candidates <- setdiff(names(data), excluded)
  candidates[vapply(data[candidates], is.numeric, logical(1))]
}

compact_feature_names <- function(data) {
  patterns <- c(
    "pass_yards_play_r", "rush_yards_play_r",
    "giveaway_rate_r", "takeaways_rate_r",
    "rest_days", "pythagorean_win_pct"
  )
  keep <- Reduce(`|`, lapply(patterns, function(pattern) {
    stringr::str_detect(names(data), pattern)
  }))
  names(data)[keep]
}

make_xy <- function(train, test, target, features) {
  train_x <- train[, features, drop = FALSE]
  test_x <- test[, features, drop = FALSE]

  medians <- vapply(train_x, function(x) stats::median(x, na.rm = TRUE), numeric(1))
  medians[!is.finite(medians)] <- 0
  for (nm in features) {
    train_x[[nm]][is.na(train_x[[nm]])] <- medians[[nm]]
    test_x[[nm]][is.na(test_x[[nm]])] <- medians[[nm]]
  }

  list(
    train_x = train_x,
    test_x = test_x,
    train_y = train[[target]],
    center = vapply(train_x, mean, numeric(1)),
    scale = vapply(train_x, stats::sd, numeric(1))
  )
}

fit_predict_model <- function(model_name, train, test, target, cfg, features = NULL) {
  if (is.null(features)) features <- feature_names(train)
  xy <- make_xy(train, test, target, features)

  if (model_name == "linear") {
    fit <- stats::lm(xy$train_y ~ ., data = xy$train_x)
    return(as.numeric(stats::predict(fit, newdata = xy$test_x)))
  }

  if (model_name == "forward_linear") {
    dat <- cbind(model_target = xy$train_y, xy$train_x)
    full_formula <- stats::reformulate(features, response = "model_target")
    null_formula <- model_target ~ 1
    null_fit <- stats::lm(null_formula, data = dat)
    fit <- stats::step(
      null_fit,
      scope = list(lower = stats::formula(null_fit), upper = full_formula),
      direction = "forward",
      trace = 0
    )
    return(as.numeric(stats::predict(fit, newdata = xy$test_x)))
  }

  if (model_name == "random_forest") {
    dat <- cbind(target = xy$train_y, xy$train_x)
    fit <- ranger::ranger(
      target ~ .,
      data = dat,
      num.trees = cfg$models$random_forest_trees,
      mtry = max(1L, floor(sqrt(length(features)))),
      min.node.size = 10,
      importance = "permutation",
      seed = cfg$backtest$seed
    )
    return(as.numeric(stats::predict(fit, data = xy$test_x)$predictions))
  }

  if (model_name == "xgboost") {
    # Without this the subsample/colsample draws vary between identical runs,
    # so a "frozen" backtest would not reproduce.
    set.seed(cfg$backtest$seed)
    fit <- xgboost::xgboost(
      data = as.matrix(xy$train_x),
      label = xy$train_y,
      objective = "reg:squarederror",
      nrounds = cfg$models$xgboost_rounds,
      eta = 0.03,
      max_depth = 4,
      min_child_weight = 8,
      subsample = 0.8,
      colsample_bytree = 0.8,
      nthread = 1,
      verbose = 0
    )
    return(as.numeric(stats::predict(fit, as.matrix(xy$test_x))))
  }

  if (model_name == "neural_net") {
    scales <- xy$scale
    scales[!is.finite(scales) | scales == 0] <- 1
    x_train <- scale(xy$train_x, center = xy$center, scale = scales)
    x_test <- scale(xy$test_x, center = xy$center, scale = scales)
    y_center <- mean(xy$train_y)
    y_scale <- stats::sd(xy$train_y)
    fit <- nnet::nnet(
      x = x_train,
      y = (xy$train_y - y_center) / y_scale,
      size = cfg$models$neural_net_hidden,
      linout = TRUE,
      decay = 0.01,
      maxit = 500,
      MaxNWts = 10000,
      trace = FALSE
    )
    return(as.numeric(stats::predict(fit, x_test)) * y_scale + y_center)
  }

  stop("Unknown model: ", model_name)
}

simulate_outcomes <- function(prediction, training_residuals, simulations = 10000L) {
  # Samford-style repeated simulation, using the empirical training residual
  # distribution instead of assuming normally distributed box-score inputs.
  vapply(prediction, function(mu) {
    draws <- mu + sample(training_residuals, simulations, replace = TRUE)
    mean(draws > 0)
  }, numeric(1))
}
