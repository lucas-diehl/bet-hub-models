dk_component_names <- function() {
  names(fantasy_target_specifications())
}

composite_ppr_from_components <- function(data, prefix) {
  get_component <- function(target) {
    column <- paste0(prefix, "_", target)
    if (!column %in% names(data)) return(rep(0, nrow(data)))
    dplyr::coalesce(as.numeric(data[[column]]), 0)
  }
  0.04 * get_component("passing_yards") +
    4 * get_component("passing_tds") -
    2 * get_component("interceptions") +
    0.1 * get_component("rushing_yards") +
    6 * get_component("rushing_tds") +
    get_component("receptions") +
    0.1 * get_component("receiving_yards") +
    6 * get_component("receiving_tds") -
    2 * get_component("fumbles_lost")
}

build_historical_ppr_predictions <- function(walk_forward, features) {
  wide <- walk_forward$predictions |>
    dplyr::select(
      "game_id", "season", "week", "game_date", "player_id",
      "player_display_name", "position", "team", "opponent_team",
      "target", "actual", "prediction", "baseline"
    ) |>
    tidyr::pivot_wider(
      names_from = "target",
      values_from = c("actual", "prediction", "baseline"),
      values_fill = 0
    )

  actual <- features |>
    dplyr::select(
      "game_id", "player_id", "ppr_points_actual",
      "total_line", "team_spread", "implied_team_total",
      "temperature", "wind_speed", "precip_probability", "is_dome"
    ) |>
    dplyr::distinct(.data$game_id, .data$player_id, .keep_all = TRUE)

  wide |>
    dplyr::left_join(actual, by = c("game_id", "player_id")) |>
    dplyr::mutate(
      projected_ppr = composite_ppr_from_components(wide, "prediction"),
      baseline_ppr = composite_ppr_from_components(wide, "baseline")
    )
}

dfs_player_name_key <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x <- gsub("\\b(jr|sr|ii|iii|iv|v)\\.?\\b", "", x)
  x <- gsub("\\b(mitchell)\\b", "mitch", x)
  x <- gsub("\\b(gabriel)\\b", "gabe", x)
  x <- gsub("\\b(robert)\\b", "rob", x)
  x <- gsub("\\b(christopher)\\b", "chris", x)
  x <- gsub("\\b(matthew)\\b", "matt", x)
  x <- gsub("\\b(joshua)\\b", "josh", x)
  gsub("[^a-z0-9]", "", x)
}

dk_main_slate_games <- function(schedules, seasons = 2017:2021) {
  schedules |>
    dplyr::filter(
      .data$season %in% seasons,
      .data$game_type == "REG",
      .data$weekday == "Sunday",
      .data$gametime %in% c("13:00", "16:05", "16:25")
    ) |>
    dplyr::transmute(
      .data$game_id,
      season = as.integer(.data$season),
      week = as.integer(.data$week),
      game_date = as.Date(.data$gameday),
      game_time = .data$gametime,
      home_team = normalize_team(.data$home_team),
      away_team = normalize_team(.data$away_team)
    )
}

prepare_dk_salary_pool <- function(salaries, main_games) {
  team_games <- dplyr::bind_rows(
    main_games |>
      dplyr::transmute(
        .data$game_id, .data$season, .data$week,
        team = .data$home_team, opponent_team = .data$away_team
      ),
    main_games |>
      dplyr::transmute(
        .data$game_id, .data$season, .data$week,
        team = .data$away_team, opponent_team = .data$home_team
      )
  )
  salaries |>
    dplyr::filter(
      .data$site == "DK",
      .data$position %in% c("QB", "RB", "WR", "TE", "DEF")
    ) |>
    dplyr::mutate(
      team = normalize_team(.data$team),
      position = dplyr::if_else(.data$position == "DEF", "DST", .data$position),
      player_name_key = dfs_player_name_key(.data$player_name)
    ) |>
    dplyr::inner_join(team_games, by = c("season", "week", "team")) |>
    dplyr::arrange(
      .data$season, .data$week, .data$team, .data$position,
      dplyr::desc(.data$salary)
    ) |>
    dplyr::distinct(
      .data$season, .data$week, .data$team,
      .data$position, .data$player_name_key,
      .keep_all = TRUE
    )
}

fuzzy_match_salary_index <- function(
    player_key,
    candidate_keys,
    maximum_distance = 2L) {
  if (!length(candidate_keys) || is.na(player_key) || !nzchar(player_key)) {
    return(NA_integer_)
  }
  distances <- as.numeric(utils::adist(player_key, candidate_keys))
  minimum <- min(distances)
  relative_limit <- max(1L, floor(nchar(player_key) * 0.15))
  limit <- min(maximum_distance, relative_limit)
  if (minimum > limit || sum(distances == minimum) != 1L) return(NA_integer_)
  which.min(distances)
}

join_predictions_to_dk_salaries <- function(predictions, salary_pool) {
  player_predictions <- predictions |>
    dplyr::filter(.data$position %in% c("QB", "RB", "WR", "TE")) |>
    dplyr::mutate(
      team = normalize_team(.data$team),
      player_name_key = dfs_player_name_key(.data$player_display_name)
    )
  salary_offense <- salary_pool |>
    dplyr::filter(.data$position != "DST") |>
    dplyr::mutate(salary_row_id = dplyr::row_number())

  exact <- player_predictions |>
    dplyr::left_join(
      dplyr::select(
        salary_offense,
        "salary_row_id", "season", "week", "team", "position",
        "player_name_key", "salary", "fantasy_points",
        salary_player_name = "player_name",
        salary_game_id = "game_id"
      ),
      by = c(
        "season", "week", "team", "position", "player_name_key"
      )
    ) |>
    dplyr::mutate(match_method = dplyr::if_else(
      !is.na(.data$salary),
      "EXACT_NAME_TEAM_POSITION",
      NA_character_
    ))

  unmatched <- which(is.na(exact$salary))
  if (length(unmatched)) {
    for (row_index in unmatched) {
      candidates <- which(
        salary_offense$season == exact$season[[row_index]] &
          salary_offense$week == exact$week[[row_index]] &
          salary_offense$team == exact$team[[row_index]] &
          salary_offense$position == exact$position[[row_index]]
      )
      local_index <- fuzzy_match_salary_index(
        exact$player_name_key[[row_index]],
        salary_offense$player_name_key[candidates]
      )
      if (is.na(local_index)) next
      matched <- salary_offense[candidates[[local_index]], ]
      exact$salary_row_id[[row_index]] <- matched$salary_row_id
      exact$salary[[row_index]] <- matched$salary
      exact$fantasy_points[[row_index]] <- matched$fantasy_points
      exact$salary_player_name[[row_index]] <- matched$player_name
      exact$salary_game_id[[row_index]] <- matched$game_id
      exact$match_method[[row_index]] <- "FUZZY_NAME_TEAM_POSITION"
    }
  }

  exact |>
    dplyr::mutate(
      match_method = dplyr::coalesce(.data$match_method, "UNMATCHED"),
      salary_match = !is.na(.data$salary)
    )
}

add_market_salary_expectation <- function(
    matched,
    salary_pool,
    test_seasons = 2017:2021) {
  output <- list()
  historical_offense <- salary_pool |>
    dplyr::filter(
      .data$position %in% c("QB", "RB", "WR", "TE"),
      is.finite(.data$salary),
      is.finite(.data$fantasy_points)
    ) |>
    dplyr::mutate(salary_k = .data$salary / 1000)

  for (test_season in test_seasons) {
    test <- matched |>
      dplyr::filter(.data$season == test_season, .data$salary_match) |>
      dplyr::mutate(salary_k = .data$salary / 1000)
    train <- historical_offense |>
      dplyr::filter(.data$season < test_season)
    expected <- rep(NA_real_, nrow(test))
    for (position_name in c("QB", "RB", "WR", "TE")) {
      train_position <- train |>
        dplyr::filter(.data$position == position_name)
      test_indices <- which(test$position == position_name)
      if (!length(test_indices) || nrow(train_position) < 100) next
      fit <- stats::lm(
        fantasy_points ~ salary_k + I(salary_k^2),
        data = train_position
      )
      expected[test_indices] <- pmax(
        0,
        as.numeric(stats::predict(fit, newdata = test[test_indices, ]))
      )
    }
    test$salary_expected_points <- expected
    output[[as.character(test_season)]] <- test
  }

  dplyr::bind_rows(output) |>
    dplyr::mutate(
      model_points_per_1k = .data$projected_ppr / .data$salary_k,
      baseline_points_per_1k = .data$baseline_ppr / .data$salary_k,
      actual_points_per_1k = .data$fantasy_points / .data$salary_k,
      model_salary_edge = .data$projected_ppr - .data$salary_expected_points,
      baseline_salary_edge = .data$baseline_ppr - .data$salary_expected_points,
      actual_salary_edge = .data$fantasy_points - .data$salary_expected_points,
      model_advantage = .data$projected_ppr - .data$baseline_ppr,
      hit_3x = .data$actual_points_per_1k >= 3,
      hit_4x = .data$actual_points_per_1k >= 4,
      hit_5x = .data$actual_points_per_1k >= 5,
      salary_tier = cut(
        .data$salary,
        breaks = c(-Inf, 3999, 5499, 6999, Inf),
        labels = c("<$4K", "$4K-$5.4K", "$5.5K-$6.9K", "$7K+")
      ),
      favorite_status = dplyr::case_when(
        .data$team_spread <= -3 ~ "Favorite 3+",
        .data$team_spread < 0 ~ "Small favorite",
        .data$team_spread >= 3 ~ "Underdog 3+",
        TRUE ~ "Near pick'em"
      ),
      total_tier = cut(
        .data$total_line,
        breaks = c(-Inf, 42, 46, 50, Inf),
        labels = c("<=42", "42.5-46", "46.5-50", "50+")
      ),
      weather_tier = dplyr::case_when(
        .data$is_dome == 1 ~ "Dome",
        .data$wind_speed >= 15 ~ "High wind",
        .data$temperature <= 35 ~ "Cold",
        TRUE ~ "Other outdoor"
      )
    ) |>
    dplyr::group_by(.data$season, .data$week, .data$position) |>
    dplyr::mutate(
      model_value_decile = dplyr::ntile(
        dplyr::desc(.data$model_salary_edge),
        10
      ),
      baseline_value_decile = dplyr::ntile(
        dplyr::desc(.data$baseline_salary_edge),
        10
      ),
      model_position_rank = dplyr::min_rank(
        dplyr::desc(.data$model_salary_edge)
      ),
      baseline_position_rank = dplyr::min_rank(
        dplyr::desc(.data$baseline_salary_edge)
      )
    ) |>
    dplyr::ungroup()
}

summarise_value_group <- function(data, ...) {
  data |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      players = dplyr::n(),
      weeks = dplyr::n_distinct(paste(.data$season, .data$week)),
      average_salary = mean(.data$salary),
      projected_points = mean(.data$projected_ppr),
      actual_points = mean(.data$fantasy_points),
      actual_points_per_1k = mean(.data$actual_points_per_1k),
      actual_salary_edge = mean(.data$actual_salary_edge),
      hit_3x = mean(.data$hit_3x),
      hit_4x = mean(.data$hit_4x),
      hit_5x = mean(.data$hit_5x),
      projection_mae = mean(abs(.data$projected_ppr - .data$fantasy_points)),
      .groups = "drop"
    )
}

build_value_summaries <- function(value_board) {
  deciles <- summarise_value_group(
    value_board,
    .data$model_value_decile
  )
  by_season <- summarise_value_group(
    dplyr::filter(value_board, .data$model_value_decile == 1),
    .data$season
  )
  by_position <- summarise_value_group(
    value_board,
    .data$model_value_decile,
    .data$position
  )
  interactions <- dplyr::bind_rows(
    summarise_value_group(
      dplyr::filter(value_board, .data$model_value_decile <= 2),
      .data$position,
      .data$salary_tier
    ) |>
      dplyr::mutate(interaction = "Position x salary", .before = 1),
    summarise_value_group(
      dplyr::filter(value_board, .data$model_value_decile <= 2),
      .data$position,
      .data$favorite_status
    ) |>
      dplyr::mutate(interaction = "Position x favorite", .before = 1),
    summarise_value_group(
      dplyr::filter(value_board, .data$model_value_decile <= 2),
      .data$position,
      .data$total_tier
    ) |>
      dplyr::mutate(interaction = "Position x total", .before = 1),
    summarise_value_group(
      dplyr::filter(value_board, .data$model_value_decile <= 2),
      .data$position,
      .data$weather_tier
    ) |>
      dplyr::mutate(interaction = "Position x weather", .before = 1)
  )
  selection_comparison <- dplyr::bind_rows(
    value_board |>
      dplyr::filter(.data$model_position_rank <= 3) |>
      dplyr::mutate(selection = "Model top 3"),
    value_board |>
      dplyr::filter(.data$baseline_position_rank <= 3) |>
      dplyr::mutate(selection = "Baseline top 3")
  ) |>
    summarise_value_group(.data$selection, .data$season)
  model_advantage <- value_board |>
    dplyr::group_by(.data$season, .data$week, .data$position) |>
    dplyr::mutate(
      model_advantage_decile = dplyr::ntile(
        dplyr::desc(.data$model_advantage),
        10
      )
    ) |>
    dplyr::ungroup() |>
    summarise_value_group(.data$model_advantage_decile)

  list(
    deciles = deciles,
    by_season = by_season,
    by_position = by_position,
    interactions = interactions,
    selection_comparison = selection_comparison,
    model_advantage = model_advantage
  )
}

position_combinations <- function(
    pool,
    position_name,
    count,
    score_column,
    candidate_limit = 24L) {
  candidates <- pool |>
    dplyr::filter(
      .data$position == position_name,
      is.finite(.data[[score_column]]),
      is.finite(.data$salary)
    ) |>
    dplyr::mutate(
      selection_value = .data[[score_column]] / pmax(.data$salary, 1)
    )
  candidates <- dplyr::bind_rows(
    candidates |>
      dplyr::slice_max(.data[[score_column]], n = candidate_limit),
    candidates |>
      dplyr::slice_max(.data$selection_value, n = 10L)
  ) |>
    dplyr::distinct(.data$row_id, .keep_all = TRUE)
  if (nrow(candidates) < count) return(tibble::tibble())

  combinations <- utils::combn(seq_len(nrow(candidates)), count)
  result <- tibble::tibble(
    salary = colSums(matrix(
      candidates$salary[combinations],
      nrow = count
    )),
    score = colSums(matrix(
      candidates[[score_column]][combinations],
      nrow = count
    )),
    ids = apply(combinations, 2, function(indices) {
      paste(candidates$row_id[indices], collapse = "|")
    })
  ) |>
    dplyr::filter(.data$salary <= 50000) |>
    dplyr::group_by(.data$salary) |>
    dplyr::slice_max(.data$score, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  result
}

combine_lineup_parts <- function(first, second, maximum_salary = 50000) {
  if (!nrow(first) || !nrow(second)) return(tibble::tibble())
  grid <- expand.grid(
    first_index = seq_len(nrow(first)),
    second_index = seq_len(nrow(second))
  )
  tibble::tibble(
    salary =
      first$salary[grid$first_index] +
      second$salary[grid$second_index],
    score =
      first$score[grid$first_index] +
      second$score[grid$second_index],
    ids = paste(
      first$ids[grid$first_index],
      second$ids[grid$second_index],
      sep = "|"
    )
  ) |>
    dplyr::filter(.data$salary <= maximum_salary) |>
    dplyr::group_by(.data$salary) |>
    dplyr::slice_max(.data$score, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
}

optimize_dk_lineup <- function(pool, score_column) {
  patterns <- list(
    c(RB = 2L, WR = 3L, TE = 2L),
    c(RB = 2L, WR = 4L, TE = 1L),
    c(RB = 3L, WR = 3L, TE = 1L)
  )
  quarterback <- position_combinations(pool, "QB", 1L, score_column)
  defense <- position_combinations(pool, "DST", 1L, score_column)
  solutions <- lapply(patterns, function(pattern) {
    running_back <- position_combinations(
      pool, "RB", pattern[["RB"]], score_column
    )
    wide_receiver <- position_combinations(
      pool, "WR", pattern[["WR"]], score_column
    )
    tight_end <- position_combinations(
      pool, "TE", pattern[["TE"]], score_column
    )
    combine_lineup_parts(
      combine_lineup_parts(
        combine_lineup_parts(
          combine_lineup_parts(quarterback, running_back),
          wide_receiver
        ),
        tight_end
      ),
      defense
    )
  })
  all_solutions <- dplyr::bind_rows(solutions)
  if (!nrow(all_solutions)) return(NULL)
  best <- all_solutions |>
    dplyr::slice_max(.data$score, n = 1, with_ties = FALSE)
  ids <- as.integer(strsplit(best$ids[[1]], "|", fixed = TRUE)[[1]])
  lineup <- pool |>
    dplyr::filter(.data$row_id %in% ids)
  list(
    summary = tibble::tibble(
      salary = sum(lineup$salary),
      projected_score = sum(lineup[[score_column]]),
      actual_score = sum(lineup$actual_dk_points),
      players = nrow(lineup)
    ),
    lineup = lineup
  )
}

build_dst_projection_pool <- function(salary_pool) {
  salary_pool |>
    dplyr::filter(.data$position == "DST") |>
    dplyr::arrange(.data$team, .data$season, .data$week) |>
    dplyr::group_by(.data$team) |>
    dplyr::mutate(
      dst_projection = fantasy_lagged_roll(
        .data$fantasy_points,
        5L,
        "mean"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      dst_projection = dplyr::coalesce(.data$dst_projection, 7),
      baseline_ppr = .data$dst_projection,
      projected_ppr = .data$dst_projection,
      actual_dk_points = .data$fantasy_points
    )
}

run_weekly_lineup_backtest <- function(value_board, salary_pool) {
  offense <- value_board |>
    dplyr::transmute(
      .data$season, .data$week,
      player = .data$player_display_name,
      .data$position, .data$team, .data$opponent_team,
      .data$salary,
      .data$projected_ppr,
      .data$baseline_ppr,
      actual_dk_points = .data$fantasy_points
    )
  defense <- build_dst_projection_pool(salary_pool) |>
    dplyr::transmute(
      .data$season, .data$week,
      player = .data$player_name,
      .data$position, .data$team,
      .data$opponent_team,
      .data$salary,
      .data$projected_ppr,
      .data$baseline_ppr,
      .data$actual_dk_points
    )
  pool <- dplyr::bind_rows(offense, defense) |>
    dplyr::filter(
      is.finite(.data$salary),
      is.finite(.data$projected_ppr),
      is.finite(.data$baseline_ppr),
      is.finite(.data$actual_dk_points)
    ) |>
    dplyr::mutate(row_id = dplyr::row_number())

  offense_weeks <- value_board |>
    dplyr::distinct(.data$season, .data$week)

  weeks <- pool |>
    dplyr::semi_join(offense_weeks, by = c("season", "week")) |>
    dplyr::distinct(.data$season, .data$week) |>
    dplyr::arrange(.data$season, .data$week)
  weekly <- list()
  lineups <- list()
  for (index in seq_len(nrow(weeks))) {
    season_value <- weeks$season[[index]]
    week_value <- weeks$week[[index]]
    message("Optimizing ", season_value, " week ", week_value)
    week_pool <- pool |>
      dplyr::filter(
        .data$season == season_value,
        .data$week == week_value
      )
    strategies <- c(
      model = "projected_ppr",
      baseline = "baseline_ppr",
      oracle = "actual_dk_points"
    )
    for (strategy_name in names(strategies)) {
      optimized <- optimize_dk_lineup(
        week_pool,
        strategies[[strategy_name]]
      )
      if (is.null(optimized)) next
      weekly[[paste(season_value, week_value, strategy_name)]] <-
        optimized$summary |>
        dplyr::mutate(
          season = season_value,
          week = week_value,
          strategy = strategy_name,
          .before = 1
        )
      lineups[[paste(season_value, week_value, strategy_name)]] <-
        optimized$lineup |>
        dplyr::mutate(
          season = season_value,
          week = week_value,
          strategy = strategy_name,
          .before = 1
        )
    }
  }
  list(
    weekly = dplyr::bind_rows(weekly),
    lineups = dplyr::bind_rows(lineups)
  )
}
