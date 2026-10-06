# Scripts source this file directly, so pull the registry in rather than
# requiring every caller to remember it.
if (!exists("registered_game_features")) source("R/feature_registry.R")

# Returns the registered game-model schema, and fails if the data has drifted
# from it in either direction. This used to be "every numeric column except a
# blocklist", which meant any new column joined every model silently.
feature_names <- function(data) {
  assert_registered_features(
    data,
    registered = registered_game_features(),
    ignore = game_non_feature_columns(),
    label = "game model"
  )
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

# `seed` overrides cfg$backtest$seed for this fit only. Callers that leave it
# NULL keep the frozen config seed, so every existing backtest reproduces
# exactly. It exists so the seed-noise study can actually vary the seed: passing
# a fixed value straight to ranger meant an outer set.seed() was inert, and a
# noise floor measured that way would have read as identically zero.
fit_predict_model <- function(model_name, train, test, target, cfg,
                              features = NULL, seed = NULL) {
  if (is.null(features)) features <- feature_names(train)
  if (is.null(seed)) seed <- cfg$backtest$seed
  seed <- as.integer(seed)
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
      seed = seed
    )
    return(as.numeric(stats::predict(fit, data = xy$test_x)$predictions))
  }

  if (model_name == "xgboost") {
    # Without this the subsample/colsample draws vary between identical runs,
    # so a "frozen" backtest would not reproduce.
    set.seed(seed)
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
    # nnet draws random starting weights, so it needs the seed too.
    set.seed(seed)
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
