fantasy_lagged_roll <- function(x, window, statistic = c("mean", "sd")) {
  statistic <- match.arg(statistic)
  slider::slide_dbl(
    dplyr::lag(as.numeric(x)),
    function(values) {
      if (!length(values) || all(is.na(values))) return(NA_real_)
      if (statistic == "mean") return(mean(values, na.rm = TRUE))
      if (sum(is.finite(values)) < 2) return(0)
      stats::sd(values, na.rm = TRUE)
    },
    .before = window - 1L,
    .complete = FALSE
  )
}

fantasy_target_specifications <- function() {
  list(
    receptions = list(
      outcome = "receptions",
      family = "receiving",
      objective = "count:poisson",
      label = "Receptions"
    ),
    receiving_yards = list(
      outcome = "receiving_yards",
      family = "receiving",
      objective = "reg:squarederror",
      label = "Receiving yards"
    ),
    rushing_yards = list(
      outcome = "rushing_yards",
      family = "rushing",
      objective = "reg:squarederror",
      label = "Rushing yards"
    ),
    passing_yards = list(
      outcome = "passing_yards",
      family = "passing",
      objective = "reg:squarederror",
      label = "Passing yards"
    ),
    passing_tds = list(
      outcome = "passing_tds",
      family = "passing",
      objective = "count:poisson",
      label = "Passing touchdowns"
    ),
    interceptions = list(
      outcome = "passing_interceptions",
      family = "passing",
      objective = "count:poisson",
      label = "Interceptions thrown"
    ),
    rushing_tds = list(
      outcome = "rushing_tds",
      family = "rushing",
      objective = "count:poisson",
      label = "Rushing touchdowns"
    ),
    receiving_tds = list(
      outcome = "receiving_tds",
      family = "receiving",
      objective = "count:poisson",
      label = "Receiving touchdowns"
    ),
    fumbles_lost = list(
      outcome = "offensive_fumbles_lost",
      family = "all",
      objective = "count:poisson",
      label = "Fumbles lost"
    )
  )
}

prepare_fantasy_player_features <- function(player_stats, game_context) {
  measures <- c(
    "completions", "attempts", "passing_yards", "passing_tds",
    "passing_interceptions", "passing_first_downs", "passing_air_yards",
    "carries", "rushing_yards", "rushing_tds", "rushing_first_downs",
    "receptions", "targets", "receiving_yards", "receiving_tds",
    "receiving_first_downs", "receiving_air_yards",
    "target_share", "air_yards_share", "wopr",
    "passing_2pt_conversions", "rushing_2pt_conversions",
    "receiving_2pt_conversions", "fumbles_lost_total",
    "fantasy_points_ppr"
  )
  measures <- intersect(measures, names(player_stats))

  players <- player_stats |>
    dplyr::filter(
      .data$season_type == "REG",
      .data$position %in% c("QB", "RB", "FB", "WR", "TE")
    ) |>
    dplyr::mutate(
      position = dplyr::if_else(.data$position == "FB", "RB", .data$position),
      team = normalize_team(.data$team),
      opponent_team = normalize_team(.data$opponent_team),
      dplyr::across(
        dplyr::all_of(measures),
        ~ dplyr::coalesce(as.numeric(.x), 0)
      ),
      offensive_fumbles_lost = dplyr::coalesce(
        as.numeric(.data$fumbles_lost_total),
        0
      ),
      two_point_conversions =
        dplyr::coalesce(as.numeric(.data$passing_2pt_conversions), 0) +
        dplyr::coalesce(as.numeric(.data$rushing_2pt_conversions), 0) +
        dplyr::coalesce(as.numeric(.data$receiving_2pt_conversions), 0),
      opportunity = .data$attempts + .data$carries + .data$targets,
      ppr_points_actual =
        0.04 * .data$passing_yards +
        4 * .data$passing_tds -
        2 * .data$passing_interceptions +
        0.1 * .data$rushing_yards +
        6 * .data$rushing_tds +
        .data$receptions +
        0.1 * .data$receiving_yards +
        6 * .data$receiving_tds -
        2 * .data$offensive_fumbles_lost +
        2 * .data$two_point_conversions
    ) |>
    dplyr::left_join(
      game_context,
      by = c("game_id", "season", "week")
    ) |>
    dplyr::mutate(
      game_date = dplyr::coalesce(
        as.Date(.data$game_date),
        as.Date(NA)
      ),
      is_home = as.integer(.data$team == .data$home_team),
      team_spread = dplyr::if_else(
        .data$is_home == 1L,
        .data$home_line,
        -.data$home_line
      ),
      implied_team_total = dplyr::if_else(
        .data$is_home == 1L,
        .data$total_line / 2 - .data$home_line / 2,
        .data$total_line / 2 + .data$home_line / 2
      ),
      position_qb = as.integer(.data$position == "QB"),
      position_rb = as.integer(.data$position == "RB"),
      position_wr = as.integer(.data$position == "WR"),
      position_te = as.integer(.data$position == "TE")
    )

  rolling_measures <- c(
    "completions", "attempts", "passing_yards", "passing_tds",
    "passing_interceptions", "passing_first_downs", "passing_air_yards",
    "carries", "rushing_yards", "rushing_tds", "rushing_first_downs",
    "receptions", "targets", "receiving_yards", "receiving_tds",
    "receiving_first_downs", "receiving_air_yards",
    "target_share", "air_yards_share", "wopr",
    "offensive_fumbles_lost", "two_point_conversions",
    "opportunity", "fantasy_points_ppr", "ppr_points_actual"
  )
  rolling_measures <- intersect(rolling_measures, names(players))

  players <- players |>
    dplyr::arrange(.data$player_id, .data$game_date, .data$game_id) |>
    dplyr::group_by(.data$player_id) |>
    dplyr::mutate(prior_games = dplyr::row_number() - 1L)

  for (window in c(3L, 5L, 8L)) {
    players <- players |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(rolling_measures),
          ~ fantasy_lagged_roll(.x, window, "mean"),
          .names = "{.col}_r{window}"
        )
      )
  }
  volatility_measures <- intersect(
    c(
      "attempts", "passing_yards", "carries", "rushing_yards",
      "targets", "receptions", "receiving_yards", "fantasy_points_ppr"
    ),
    names(players)
  )
  players <- players |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(volatility_measures),
        ~ fantasy_lagged_roll(.x, 5L, "sd"),
        .names = "{.col}_sd5"
      )
    ) |>
    dplyr::ungroup()

  add_fantasy_opponent_features(players)
}

build_fantasy_2026_features <- function(
  player_stats,
  rosters,
  historical_game_context,
  upcoming_game_context
) {
  current_roster <- rosters |>
    dplyr::filter(
      .data$season == 2026,
      .data$status == "ACT",
      .data$position %in% c("QB", "RB", "FB", "WR", "TE"),
      !is.na(.data$gsis_id)
    ) |>
    dplyr::arrange(.data$gsis_id, dplyr::desc(.data$week)) |>
    dplyr::group_by(.data$gsis_id) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      player_id = .data$gsis_id,
      player_display_name = .data$full_name,
      position = .data$position,
      team = normalize_team(.data$team),
      entry_year = as.numeric(.data$entry_year),
      draft_number = as.numeric(.data$draft_number),
      years_exp = as.numeric(.data$years_exp)
    )

  game_teams <- dplyr::bind_rows(
    upcoming_game_context |>
      dplyr::transmute(
        .data$game_id, .data$season, .data$week,
        team = .data$home_team,
        opponent_team = .data$away_team
      ),
    upcoming_game_context |>
      dplyr::transmute(
        .data$game_id, .data$season, .data$week,
        team = .data$away_team,
        opponent_team = .data$home_team
      )
  )
  synthetic <- current_roster |>
    dplyr::inner_join(game_teams, by = "team") |>
    dplyr::mutate(season = 2026L, season_type = "REG")

  prepared <- prepare_fantasy_player_features(
    dplyr::bind_rows(player_stats, synthetic),
    dplyr::bind_rows(historical_game_context, upcoming_game_context)
  ) |>
    dplyr::filter(
      .data$season == 2026,
      .data$game_id %in% upcoming_game_context$game_id
    )

  prepared |>
    dplyr::mutate(
      recent_opportunity = dplyr::coalesce(.data$opportunity_r5, 0),
      draft_priority = dplyr::if_else(
        .data$prior_games == 0 & is.finite(.data$draft_number),
        pmax(0, 300 - .data$draft_number) / 100,
        0
      ),
      role_score = .data$recent_opportunity + .data$draft_priority
    ) |>
    dplyr::arrange(
      .data$team, .data$position,
      dplyr::desc(.data$role_score),
      .data$draft_number
    ) |>
    dplyr::group_by(.data$game_id, .data$team, .data$position) |>
    dplyr::mutate(preseason_role_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::filter(
      (.data$position == "QB" & .data$preseason_role_rank <= 1) |
        (.data$position == "RB" & .data$preseason_role_rank <= 4) |
        (.data$position == "WR" & .data$preseason_role_rank <= 5) |
        (.data$position == "TE" & .data$preseason_role_rank <= 3)
    )
}

add_fantasy_opponent_features <- function(players) {
  allowed <- players |>
    dplyr::group_by(
      .data$game_id, .data$game_date,
      defense = .data$opponent_team,
      .data$position
    ) |>
    dplyr::summarise(
      allowed_attempts = sum(.data$attempts, na.rm = TRUE),
      allowed_pass_yards = sum(.data$passing_yards, na.rm = TRUE),
      allowed_pass_tds = sum(.data$passing_tds, na.rm = TRUE),
      allowed_interceptions = sum(.data$passing_interceptions, na.rm = TRUE),
      allowed_carries = sum(.data$carries, na.rm = TRUE),
      allowed_rush_yards = sum(.data$rushing_yards, na.rm = TRUE),
      allowed_rush_tds = sum(.data$rushing_tds, na.rm = TRUE),
      allowed_targets = sum(.data$targets, na.rm = TRUE),
      allowed_receptions = sum(.data$receptions, na.rm = TRUE),
      allowed_rec_yards = sum(.data$receiving_yards, na.rm = TRUE),
      allowed_rec_tds = sum(.data$receiving_tds, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      .data$defense, .data$position, .data$game_date, .data$game_id
    ) |>
    dplyr::group_by(.data$defense, .data$position) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("allowed_"),
        ~ fantasy_lagged_roll(.x, 5L, "mean"),
        .names = "{.col}_r5"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      "game_id", "defense", "position", dplyr::ends_with("_r5")
    )

  players |>
    dplyr::left_join(
      allowed,
      by = c(
        "game_id",
        "opponent_team" = "defense",
        "position"
      )
    )
}

fantasy_model_feature_names <- function(data) {
  rolling <- names(data)[stringr::str_detect(
    names(data),
    "(_r3|_r5|_r8|_sd5)$"
  )]
  context <- c(
    "week", "prior_games", "total_line", "team_spread",
    "implied_team_total", "is_home",
    "temperature", "wind_speed", "precip_probability",
    "is_dome", "grass_flag", "turf_flag", "rain_flag", "snow_flag",
    "cold_index", "high_wind_index",
    "position_qb", "position_rb", "position_wr", "position_te"
  )
  intersect(unique(c(rolling, context)), names(data))
}

fantasy_target_candidates <- function(data, family, deployment = FALSE) {
  recent_pass <- dplyr::coalesce(data$attempts_r3, 0)
  recent_rush <- dplyr::coalesce(data$carries_r3, 0)
  recent_receive <- dplyr::coalesce(data$targets_r3, 0)
  is_future <- deployment | data$season >= 2026

  keep <- switch(
    family,
    passing = data$position == "QB" & (recent_pass >= 3 | is_future),
    rushing = data$position %in% c("QB", "RB", "WR", "TE") &
      (recent_rush >= 0.5 | is_future),
    receiving = data$position %in% c("RB", "WR", "TE") &
      (recent_receive >= 0.5 | is_future),
    all = recent_pass + recent_rush + recent_receive >= 0.5 | is_future
  )
  data[which(keep & is.finite(data$prior_games)), , drop = FALSE]
}

fantasy_matrix_pair <- function(train, test, features, medians = NULL) {
  train_x <- train[, features, drop = FALSE]
  test_x <- test[, features, drop = FALSE]
  if (is.null(medians)) {
    medians <- vapply(
      train_x,
      function(x) stats::median(as.numeric(x), na.rm = TRUE),
      numeric(1)
    )
    medians[!is.finite(medians)] <- 0
  }
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

fit_fantasy_stat_model <- function(
  train,
  test,
  outcome,
  objective,
  seed = 20260728L
) {
  features <- fantasy_model_feature_names(train)
  matrices <- fantasy_matrix_pair(train, test, features)
  params <- list(
    objective = objective,
    eval_metric = if (objective == "count:poisson") "poisson-nloglik" else "rmse",
    learning_rate = 0.035,
    max_depth = 3,
    min_child_weight = 15,
    subsample = 0.8,
    colsample_bytree = 0.8,
    reg_lambda = 3,
    reg_alpha = 0.15,
    nthread = 1,
    verbosity = 0
  )
  if (objective == "count:poisson") params$max_delta_step <- 0.7
  # The R package ignores params$seed and takes its RNG state from R, so
  # subsample/colsample draws are only reproducible via set.seed().
  set.seed(seed)
  fit <- xgboost::xgb.train(
    params = params,
    data = xgboost::xgb.DMatrix(
      matrices$train,
      label = as.numeric(train[[outcome]])
    ),
    nrounds = 180
  )
  prediction <- if (nrow(test)) {
    pmax(0, as.numeric(stats::predict(fit, matrices$test)))
  } else {
    numeric()
  }
  list(
    fit = portable_booster(fit),
    prediction = prediction,
    features = features,
    medians = matrices$medians
  )
}

fantasy_regression_metrics <- function(data) {
  if (!nrow(data)) return(tibble::tibble())
  truth <- data$actual
  prediction <- data$prediction
  baseline <- data$baseline
  denominator <- sum((truth - mean(truth))^2)
  tibble::tibble(
    observations = nrow(data),
    actual_mean = mean(truth),
    predicted_mean = mean(prediction),
    mae = mean(abs(prediction - truth)),
    rmse = sqrt(mean((prediction - truth)^2)),
    r_squared = dplyr::if_else(
      denominator > 0,
      1 - sum((prediction - truth)^2) / denominator,
      NA_real_
    ),
    bias = mean(prediction - truth),
    baseline_mae = mean(abs(baseline - truth)),
    mae_improvement = .data$baseline_mae - .data$mae
  )
}

walk_forward_fantasy_models <- function(
  features,
  test_seasons = 2023:2025,
  seed = 20260728L
) {
  specifications <- fantasy_target_specifications()
  predictions <- list()
  artifacts <- list()
  metrics <- list()

  for (target_name in names(specifications)) {
    specification <- specifications[[target_name]]
    target_data <- fantasy_target_candidates(
      features,
      specification$family
    )
    target_predictions <- list()
    target_artifacts <- list()
    for (test_season in test_seasons) {
      train <- target_data |>
        dplyr::filter(.data$season < test_season, .data$prior_games >= 1)
      test <- target_data |>
        dplyr::filter(.data$season == test_season, .data$prior_games >= 1)
      if (!nrow(train) || !nrow(test)) next
      fitted <- fit_fantasy_stat_model(
        train,
        test,
        specification$outcome,
        specification$objective,
        seed + test_season + match(target_name, names(specifications))
      )
      baseline_column <- paste0(specification$outcome, "_r5")
      baseline <- dplyr::coalesce(
        as.numeric(test[[baseline_column]]),
        mean(train[[specification$outcome]], na.rm = TRUE)
      )
      board <- test |>
        dplyr::transmute(
          .data$game_id,
          .data$season,
          .data$week,
          .data$game_date,
          .data$player_id,
          .data$player_display_name,
          .data$position,
          .data$team,
          .data$opponent_team,
          target = target_name,
          actual = as.numeric(.data[[specification$outcome]]),
          prediction = fitted$prediction,
          baseline = baseline
        )
      target_predictions[[as.character(test_season)]] <- board
      target_artifacts[[as.character(test_season)]] <- fitted
      metrics[[paste(target_name, test_season)]] <-
        fantasy_regression_metrics(board) |>
        dplyr::mutate(
          target = target_name,
          season = test_season,
          .before = 1
        )
    }
    predictions[[target_name]] <- dplyr::bind_rows(target_predictions)
    artifacts[[target_name]] <- target_artifacts
  }

  list(
    predictions = dplyr::bind_rows(predictions),
    metrics = dplyr::bind_rows(metrics),
    artifacts = artifacts
  )
}

fit_fantasy_deployment_models <- function(features, seed = 20260728L) {
  specifications <- fantasy_target_specifications()
  purrr::imap(specifications, function(specification, target_name) {
    train <- fantasy_target_candidates(features, specification$family) |>
      dplyr::filter(.data$season <= 2025, .data$prior_games >= 1)
    fitted <- fit_fantasy_stat_model(
      train,
      train[0, , drop = FALSE],
      specification$outcome,
      specification$objective,
      seed + match(target_name, names(specifications))
    )
    fitted$prediction <- NULL
    fitted$outcome <- specification$outcome
    fitted$outcome_mean <- mean(train[[specification$outcome]], na.rm = TRUE)
    fitted$family <- specification$family
    fitted$label <- specification$label
    fitted
  })
}

fantasy_deployment_blend_weights <- function(walk_forward_predictions) {
  walk_forward_predictions |>
    dplyr::group_by(.data$target) |>
    dplyr::summarise(
      numerator = sum(
        (.data$prediction - .data$baseline) *
          (.data$actual - .data$baseline)
      ),
      denominator = sum((.data$prediction - .data$baseline)^2),
      model_weight = dplyr::if_else(
        .data$denominator > 0,
        pmin(pmax(.data$numerator / .data$denominator, 0), 1),
        1
      ),
      .groups = "drop"
    ) |>
    dplyr::select("target", "model_weight")
}

predict_fantasy_deployment <- function(
  models,
  future_features,
  blend_weights = NULL
) {
  predictions <- purrr::imap_dfr(models, function(model, target_name) {
    candidates <- fantasy_target_candidates(
      future_features,
      model$family,
      deployment = TRUE
    )
    if (!nrow(candidates)) return(tibble::tibble())
    matrices <- fantasy_matrix_pair(
      candidates[0, , drop = FALSE],
      candidates,
      model$features,
      model$medians
    )
    model_prediction <- pmax(
      0,
      as.numeric(stats::predict(
        revive_booster(model$fit, target_name),
        matrices$test
      ))
    )
    baseline_column <- paste0(model$outcome, "_r5")
    baseline_prediction <- dplyr::coalesce(
      as.numeric(candidates[[baseline_column]]),
      model$outcome_mean
    )
    candidates |>
      dplyr::transmute(
        .data$game_id,
        .data$season,
        .data$week,
        .data$game_date,
        .data$player_id,
        player = .data$player_display_name,
        .data$position,
        .data$prior_games,
        .data$preseason_role_rank,
        .data$team,
        .data$opponent_team,
        .data$total_line,
        .data$team_spread,
        .data$implied_team_total,
        .data$temperature,
        .data$wind_speed,
        .data$precip_probability,
        .data$is_dome,
        weather_status = dplyr::coalesce(
          as.character(.data$weather_status),
          "PENDING"
        ),
        target = target_name,
        model_prediction = model_prediction,
        baseline_prediction = baseline_prediction
      )
  })
  if (is.null(blend_weights)) {
    predictions$model_weight <- 1
  } else {
    predictions <- predictions |>
      dplyr::left_join(blend_weights, by = "target") |>
      dplyr::mutate(model_weight = dplyr::coalesce(.data$model_weight, 1))
  }
  predictions |>
    dplyr::mutate(
      prediction = pmax(
        0,
        .data$model_weight * .data$model_prediction +
          (1 - .data$model_weight) * .data$baseline_prediction
      )
    )
}

fantasy_residual_intervals <- function(walk_forward_predictions) {
  walk_forward_predictions |>
    dplyr::mutate(residual = .data$actual - .data$prediction) |>
    dplyr::group_by(.data$target) |>
    dplyr::summarise(
      residual_p10 = stats::quantile(.data$residual, 0.10, na.rm = TRUE),
      residual_p90 = stats::quantile(.data$residual, 0.90, na.rm = TRUE),
      .groups = "drop"
    )
}

build_fantasy_projection_board <- function(
  long_predictions,
  residual_intervals,
  historical_features
) {
  projected <- long_predictions |>
    dplyr::left_join(residual_intervals, by = "target") |>
    dplyr::mutate(
      projection_low = pmax(0, .data$prediction + .data$residual_p10),
      projection_high = pmax(0, .data$prediction + .data$residual_p90)
    )

  id_columns <- c(
    "game_id", "season", "week", "game_date", "player_id", "player",
    "position", "prior_games", "preseason_role_rank",
    "team", "opponent_team", "total_line", "team_spread",
    "implied_team_total", "temperature", "wind_speed",
    "precip_probability", "is_dome", "weather_status"
  )
  point <- projected |>
    dplyr::select(dplyr::all_of(id_columns), "target", "prediction") |>
    tidyr::pivot_wider(
      names_from = "target",
      values_from = "prediction",
      names_prefix = "projected_",
      values_fill = 0
    )
  low <- projected |>
    dplyr::select("game_id", "player_id", "target", "projection_low") |>
    tidyr::pivot_wider(
      names_from = "target",
      values_from = "projection_low",
      names_prefix = "low_",
      values_fill = 0
    )
  high <- projected |>
    dplyr::select("game_id", "player_id", "target", "projection_high") |>
    tidyr::pivot_wider(
      names_from = "target",
      values_from = "projection_high",
      names_prefix = "high_",
      values_fill = 0
    )

  two_point_rates <- historical_features |>
    dplyr::filter(.data$season >= 2023, .data$season <= 2025) |>
    dplyr::group_by(.data$position) |>
    dplyr::summarise(
      projected_two_point_conversions =
        (sum(.data$two_point_conversions) + 0.5) /
        (dplyr::n() + 50),
      .groups = "drop"
    )

  point |>
    dplyr::left_join(low, by = c("game_id", "player_id")) |>
    dplyr::left_join(high, by = c("game_id", "player_id")) |>
    dplyr::left_join(two_point_rates, by = "position") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with(c("projected_", "low_", "high_")),
        ~ dplyr::coalesce(.x, 0)
      ),
      projected_ppr =
        0.04 * .data$projected_passing_yards +
        4 * .data$projected_passing_tds -
        2 * .data$projected_interceptions +
        0.1 * .data$projected_rushing_yards +
        6 * .data$projected_rushing_tds +
        .data$projected_receptions +
        0.1 * .data$projected_receiving_yards +
        6 * .data$projected_receiving_tds -
        2 * .data$projected_fumbles_lost +
        2 * .data$projected_two_point_conversions,
      ppr_low = pmax(
        0,
        0.04 * .data$low_passing_yards +
          4 * .data$low_passing_tds -
          2 * .data$high_interceptions +
          0.1 * .data$low_rushing_yards +
          6 * .data$low_rushing_tds +
          .data$low_receptions +
          0.1 * .data$low_receiving_yards +
          6 * .data$low_receiving_tds -
          2 * .data$high_fumbles_lost
      ),
      ppr_high =
        0.04 * .data$high_passing_yards +
        4 * .data$high_passing_tds -
        2 * .data$low_interceptions +
        0.1 * .data$high_rushing_yards +
        6 * .data$high_rushing_tds +
        .data$high_receptions +
        0.1 * .data$high_receiving_yards +
        6 * .data$high_receiving_tds -
        2 * .data$low_fumbles_lost +
        2 * .data$projected_two_point_conversions,
      projection_status = dplyr::case_when(
        .data$weather_status == "PENDING" ~ "PRELIMINARY_WEATHER_PENDING",
        TRUE ~ "PRELIMINARY_DEPTH_CHART_PENDING"
      ),
      role_status = dplyr::case_when(
        .data$prior_games == 0 ~ "ROOKIE_PRIOR",
        TRUE ~ "PRESEASON_ROLE_ESTIMATE"
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$projected_ppr))
}
