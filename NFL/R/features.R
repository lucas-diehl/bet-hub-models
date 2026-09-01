summarize_team_games <- function(pbp, regular_season_only = TRUE) {
  if (regular_season_only) {
    pbp <- dplyr::filter(pbp, .data$season_type == "REG")
  }

  plays <- pbp |>
    dplyr::filter(
      (.data$rush == 1 | .data$pass == 1),
      !is.na(.data$epa),
      !is.na(.data$posteam),
      !is.na(.data$defteam),
      is.na(.data$qb_kneel) | .data$qb_kneel == 0,
      is.na(.data$qb_spike) | .data$qb_spike == 0
    ) |>
    dplyr::mutate(
      explosive = dplyr::case_when(
        .data$pass == 1 ~ .data$yards_gained >= 20,
        .data$rush == 1 ~ .data$yards_gained >= 10,
        TRUE ~ FALSE
      ),
      success = .data$epa > 0,
      giveaway = dplyr::coalesce(.data$interception, 0) +
        dplyr::coalesce(.data$fumble_lost, 0),
      dropback = .data$pass == 1 | dplyr::coalesce(.data$sack, 0) == 1,
      early_down = .data$down %in% c(1, 2)
    )

  offense <- plays |>
    dplyr::group_by(
      .data$game_id, .data$season, .data$week,
      team = .data$posteam, opponent = .data$defteam
    ) |>
    dplyr::summarise(
      off_plays = dplyr::n(),
      off_epa = safe_mean(.data$epa),
      off_success = safe_mean(.data$success),
      pass_epa = safe_mean(.data$epa[.data$pass == 1]),
      rush_epa = safe_mean(.data$epa[.data$rush == 1]),
      pass_yards_play = safe_mean(.data$yards_gained[.data$pass == 1]),
      rush_yards_play = safe_mean(.data$yards_gained[.data$rush == 1]),
      explosive_rate = safe_mean(.data$explosive),
      giveaway_rate = sum(.data$giveaway, na.rm = TRUE) / dplyr::n(),
      sack_rate = sum(dplyr::coalesce(.data$sack, 0), na.rm = TRUE) /
        max(1, sum(.data$dropback, na.rm = TRUE)),
      early_down_pass_rate = safe_mean(.data$pass[.data$early_down]),
      .groups = "drop"
    )

  defense <- plays |>
    dplyr::group_by(.data$game_id, team = .data$defteam) |>
    dplyr::summarise(
      def_epa = safe_mean(.data$epa),
      def_success_allowed = safe_mean(.data$success),
      takeaways_rate = sum(.data$giveaway, na.rm = TRUE) / dplyr::n(),
      pressure_rate = sum(dplyr::coalesce(.data$sack, 0), na.rm = TRUE) /
        max(1, sum(.data$dropback, na.rm = TRUE)),
      explosive_allowed = safe_mean(.data$explosive),
      .groups = "drop"
    )

  offense |>
    dplyr::left_join(defense, by = c("game_id", "team")) |>
    dplyr::mutate(
      team = normalize_team(.data$team),
      opponent = normalize_team(.data$opponent)
    )
}

schedule_games <- function(schedules, regular_season_only = TRUE) {
  out <- schedules
  if (regular_season_only) out <- dplyr::filter(out, .data$game_type == "REG")

  out |>
    dplyr::transmute(
      game_id = .data$game_id,
      season = as.integer(.data$season),
      week = as.integer(.data$week),
      game_date = as.Date(.data$gameday),
      home_team = normalize_team(.data$home_team),
      away_team = normalize_team(.data$away_team),
      home_score = as.numeric(.data$home_score),
      away_score = as.numeric(.data$away_score),
      home_margin = .data$home_score - .data$away_score,
      game_total = .data$home_score + .data$away_score
    ) |>
    dplyr::filter(!is.na(.data$home_score), !is.na(.data$away_score))
}

add_team_context <- function(team_games, games) {
  home <- games |>
    dplyr::select(
      "game_id", "game_date",
      team = "home_team", points_for = "home_score",
      points_against = "away_score"
    )
  away <- games |>
    dplyr::select(
      "game_id", "game_date",
      team = "away_team", points_for = "away_score",
      points_against = "home_score"
    )

  context <- dplyr::bind_rows(home, away)
  team_games |>
    dplyr::left_join(context, by = c("game_id", "team")) |>
    dplyr::arrange(.data$team, .data$game_date, .data$game_id)
}

lagged_roll_mean <- function(x, window) {
  slider::slide_dbl(
    dplyr::lag(x),
    safe_mean,
    .before = window - 1,
    .complete = FALSE
  )
}

add_rolling_features <- function(team_games, windows = c(4, 8)) {
  id_cols <- c("game_id", "season", "week", "game_date", "team", "opponent")
  measure_cols <- setdiff(names(team_games), id_cols)
  measure_cols <- measure_cols[vapply(team_games[measure_cols], is.numeric, logical(1))]

  rolled <- team_games |>
    dplyr::group_by(.data$team) |>
    dplyr::arrange(.data$game_date, .data$game_id, .by_group = TRUE) |>
    dplyr::mutate(
      prior_games = dplyr::row_number() - 1L,
      rest_days = as.numeric(.data$game_date - dplyr::lag(.data$game_date)),
      rest_days = dplyr::if_else(
        is.na(.data$rest_days), 7, pmin(pmax(.data$rest_days, 3), 21)
      )
    )

  for (w in windows) {
    rolled <- rolled |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(measure_cols),
          ~ lagged_roll_mean(.x, w),
          .names = "{.col}_r{w}"
        )
      )
  }

  rolled |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$team, .data$season) |>
    dplyr::mutate(
      season_points_for = dplyr::coalesce(
        dplyr::lag(cumsum(dplyr::coalesce(.data$points_for, 0))), 0
      ),
      season_points_against = dplyr::coalesce(
        dplyr::lag(cumsum(dplyr::coalesce(.data$points_against, 0))), 0
      ),
      pythagorean_win_pct = dplyr::if_else(
        .data$season_points_for + .data$season_points_against > 0,
        .data$season_points_for^2.37 /
          (.data$season_points_for^2.37 + .data$season_points_against^2.37),
        0.5
      )
    ) |>
    dplyr::ungroup()
}

build_game_features <- function(games, rolled, odds, minimum_prior_games = 4) {
  feature_cols <- names(rolled)[
    stringr::str_detect(names(rolled), "_r\\d+$") |
      names(rolled) %in% c("prior_games", "rest_days", "pythagorean_win_pct")
  ]

  home <- games |>
    dplyr::select("game_id", team = "home_team") |>
    dplyr::left_join(rolled, by = c("game_id", "team")) |>
    dplyr::select("game_id", dplyr::all_of(feature_cols)) |>
    dplyr::rename_with(~ paste0("home_", .x), -dplyr::all_of("game_id"))
  away <- games |>
    dplyr::select("game_id", team = "away_team") |>
    dplyr::left_join(rolled, by = c("game_id", "team")) |>
    dplyr::select("game_id", dplyr::all_of(feature_cols)) |>
    dplyr::rename_with(~ paste0("away_", .x), -dplyr::all_of("game_id"))

  games |>
    dplyr::left_join(home, by = "game_id") |>
    dplyr::left_join(away, by = "game_id") |>
    join_odds(odds) |>
    dplyr::mutate(
      market_margin = -.data$home_line,
      market_total = .data$total_line,
      neutral_temperature = dplyr::if_else(.data$surface == "Dome", 70, .data$temperature),
      neutral_wind = dplyr::if_else(.data$surface == "Dome", 0, .data$wind_speed)
    ) |>
    dplyr::filter(
      .data$home_prior_games >= minimum_prior_games,
      .data$away_prior_games >= minimum_prior_games,
      !is.na(.data$home_line),
      !is.na(.data$total_line)
    )
}
