normalize_prop_player_name <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x <- gsub("\\b(jr|sr|ii|iii|iv)\\.?$", "", x)
  gsub("[^a-z0-9]", "", x)
}

td_lagged_roll <- function(x, window) {
  slider::slide_dbl(
    dplyr::lag(as.numeric(x)),
    safe_mean,
    .before = window - 1L,
    .complete = FALSE
  )
}

build_td_game_context <- function(schedules, rotowire, seasons = 2021:2025) {
  games <- schedules |>
    dplyr::filter(
      .data$season %in% seasons,
      .data$game_type == "REG"
    ) |>
    dplyr::transmute(
      game_id = .data$game_id,
      season = as.integer(.data$season),
      week = as.integer(.data$week),
      game_date = as.Date(.data$gameday),
      home_team = normalize_team(.data$home_team),
      away_team = normalize_team(.data$away_team),
      schedule_total = as.numeric(.data$total_line),
      schedule_home_line = as.numeric(.data$spread_line),
      schedule_temperature = as.numeric(.data$temp),
      schedule_wind = as.numeric(.data$wind),
      schedule_roof = tolower(as.character(.data$roof)),
      schedule_surface = as.character(.data$surface)
    ) |>
    join_odds(rotowire) |>
    dplyr::mutate(
      total_line = dplyr::coalesce(.data$total_line, .data$schedule_total),
      home_line = dplyr::coalesce(.data$home_line, .data$schedule_home_line),
      surface = dplyr::coalesce(.data$surface, .data$schedule_surface),
      is_dome = as.integer(
        .data$surface == "Dome" |
          .data$schedule_roof %in% c("dome", "closed")
      ),
      temperature = dplyr::if_else(
        .data$is_dome == 1L,
        70,
        dplyr::coalesce(.data$temperature, .data$schedule_temperature)
      ),
      wind_speed = dplyr::if_else(
        .data$is_dome == 1L,
        0,
        dplyr::coalesce(.data$wind_speed, .data$schedule_wind)
      ),
      precip_probability = dplyr::if_else(
        .data$is_dome == 1L,
        0,
        .data$precip_probability
      ),
      weather_text = tolower(paste(
        dplyr::coalesce(.data$weather, ""),
        dplyr::coalesce(.data$precip_type, "")
      )),
      rain_flag = as.integer(stringr::str_detect(
        .data$weather_text,
        "rain|shower|storm"
      )),
      snow_flag = as.integer(stringr::str_detect(
        .data$weather_text,
        "snow|sleet|flurr"
      )),
      grass_flag = as.integer(.data$surface == "Grass"),
      turf_flag = as.integer(.data$surface == "Turf"),
      cold_index = pmax(0, 40 - .data$temperature) / 40,
      high_wind_index = pmax(0, .data$wind_speed - 12) / 10
    ) |>
    dplyr::select(
      "game_id", "season", "week", "game_date",
      "home_team", "away_team", "total_line", "home_line",
      "temperature", "wind_speed", "precip_probability",
      "is_dome", "grass_flag", "turf_flag", "rain_flag", "snow_flag",
      "cold_index", "high_wind_index"
    )

  games
}

prepare_td_player_weeks <- function(player_stats, game_context) {
  measures <- c(
    "carries", "targets", "receptions", "rushing_yards", "receiving_yards",
    "rushing_tds", "receiving_tds", "rushing_first_downs",
    "receiving_first_downs", "target_share", "air_yards_share", "wopr"
  )

  players <- player_stats |>
    dplyr::filter(
      .data$season_type == "REG",
      .data$position %in% c("QB", "RB", "FB", "WR", "TE")
    ) |>
    dplyr::mutate(
      position = dplyr::if_else(.data$position == "FB", "RB", .data$position),
      team = normalize_team(.data$team),
      opponent_team = normalize_team(.data$opponent_team),
      dplyr::across(dplyr::all_of(measures), ~ dplyr::coalesce(as.numeric(.x), 0)),
      touches = .data$carries + .data$targets,
      anytime_td = as.integer(.data$rushing_tds + .data$receiving_tds > 0)
    )

  team_totals <- players |>
    dplyr::group_by(.data$game_id, .data$team) |>
    dplyr::summarise(
      team_carries = sum(.data$carries, na.rm = TRUE),
      team_targets = sum(.data$targets, na.rm = TRUE),
      team_offensive_tds = sum(
        .data$rushing_tds + .data$receiving_tds,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  players <- players |>
    dplyr::left_join(team_totals, by = c("game_id", "team")) |>
    dplyr::mutate(
      carry_share = dplyr::if_else(
        .data$team_carries > 0,
        .data$carries / .data$team_carries,
        0
      ),
      computed_target_share = dplyr::if_else(
        .data$team_targets > 0,
        .data$targets / .data$team_targets,
        0
      ),
      target_share = dplyr::if_else(
        is.finite(.data$target_share),
        .data$target_share,
        .data$computed_target_share
      ),
      team_td_share = dplyr::if_else(
        .data$team_offensive_tds > 0,
        (.data$rushing_tds + .data$receiving_tds) /
          .data$team_offensive_tds,
        0
      )
    ) |>
    dplyr::left_join(
      dplyr::select(game_context, "game_id", "game_date"),
      by = "game_id"
    )

  rolling_measures <- c(
    "carries", "targets", "receptions", "rushing_yards", "receiving_yards",
    "rushing_tds", "receiving_tds", "rushing_first_downs",
    "receiving_first_downs", "touches", "carry_share", "target_share",
    "air_yards_share", "wopr", "team_td_share"
  )

  players <- players |>
    dplyr::arrange(.data$player_id, .data$game_date, .data$game_id) |>
    dplyr::group_by(.data$player_id) |>
    dplyr::mutate(
      prior_games = dplyr::row_number() - 1L,
      career_td_rate = dplyr::if_else(
        .data$prior_games > 0,
        dplyr::lag(cumsum(.data$anytime_td)) / .data$prior_games,
        NA_real_
      )
    )

  for (window in c(3L, 5L)) {
    players <- players |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(rolling_measures),
          ~ td_lagged_roll(.x, window),
          .names = "{.col}_r{window}"
        )
      )
  }

  players |>
    dplyr::ungroup()
}

add_td_defense_features <- function(players) {
  defense_games <- players |>
    dplyr::group_by(
      .data$game_id, .data$game_date,
      defense = .data$opponent_team,
      .data$position
    ) |>
    dplyr::summarise(
      def_carries = sum(.data$carries, na.rm = TRUE),
      def_targets = sum(.data$targets, na.rm = TRUE),
      def_rush_tds = sum(.data$rushing_tds, na.rm = TRUE),
      def_rec_tds = sum(.data$receiving_tds, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$defense, .data$position, .data$game_date, .data$game_id) |>
    dplyr::group_by(.data$defense, .data$position) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(
          "def_carries", "def_targets", "def_rush_tds", "def_rec_tds"
        )),
        ~ td_lagged_roll(.x, 5L),
        .names = "{.col}_r5"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      "game_id", "defense", "position",
      dplyr::ends_with("_r5")
    )

  players |>
    dplyr::left_join(
      defense_games,
      by = c(
        "game_id",
        "opponent_team" = "defense",
        "position"
      )
    )
}

build_td_player_features <- function(player_stats, game_context) {
  players <- prepare_td_player_weeks(player_stats, game_context) |>
    add_td_defense_features() |>
    dplyr::left_join(game_context, by = c("game_id", "season", "week", "game_date")) |>
    dplyr::mutate(
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
      position_te = as.integer(.data$position == "TE"),
      wind_receiving_role = .data$high_wind_index *
        dplyr::coalesce(.data$target_share_r3, 0),
      precip_rushing_role = dplyr::coalesce(.data$precip_probability, 0) /
        100 * dplyr::coalesce(.data$carry_share_r3, 0),
      qb_rush_weather = .data$position_qb *
        dplyr::coalesce(.data$carries_r3, 0) *
        (1 + dplyr::coalesce(.data$precip_probability, 0) / 100),
      favorite_rush_role = as.numeric(.data$team_spread < 0) *
        dplyr::coalesce(.data$carry_share_r3, 0)
    )

  players
}

build_td_prop_board <- function(odds, player_features, player_directory = NULL) {
  priced_by_book <- odds |>
    dplyr::filter(
      .data$outcome == "Yes",
      !stringr::str_detect(.data$player, "D/ST|Defense|Field")
    ) |>
    dplyr::mutate(prop_name_key = normalize_prop_player_name(.data$player)) |>
    dplyr::group_by(
      .data$game_id, .data$season, .data$week,
      .data$player, .data$prop_name_key,
      .data$bookmaker_key, .data$bookmaker
    ) |>
    dplyr::arrange(dplyr::desc(.data$american_odds), .by_group = TRUE) |>
    dplyr::summarise(
      american_odds = dplyr::first(.data$american_odds),
      implied_probability = dplyr::first(.data$implied_probability),
      snapshot_lead_minutes = stats::median(
        .data$snapshot_lead_minutes,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  roster <- player_features |>
    dplyr::transmute(
      .data$season, .data$game_id, .data$player_id,
      player_display_name = .data$player_display_name,
      .data$position, .data$team,
      roster_name_key = normalize_prop_player_name(.data$player_display_name)
    ) |>
    dplyr::distinct()

  season_roster_base <- roster |>
    dplyr::select(
      .data$season, .data$player_id, .data$player_display_name,
      .data$position, .data$team, .data$roster_name_key
    ) |>
    dplyr::distinct()

  season_roster <- season_roster_base
  if (!is.null(player_directory)) {
    directory_aliases <- player_directory |>
      dplyr::transmute(
        player_id = .data$gsis_id,
        aliases = purrr::pmap(
          list(
            .data$display_name,
            .data$first_name,
            .data$common_first_name,
            .data$football_name,
            .data$last_name,
            .data$short_name
          ),
          function(display, first, common, football, last, short) {
            unique(stats::na.omit(c(
              display,
              paste(first, last),
              paste(common, last),
              paste(football, last),
              short
            )))
          }
        )
      ) |>
      tidyr::unnest_longer(.data$aliases) |>
      dplyr::transmute(
        .data$player_id,
        roster_name_key = normalize_prop_player_name(.data$aliases)
      ) |>
      dplyr::filter(nzchar(.data$roster_name_key)) |>
      dplyr::distinct()

    season_roster <- season_roster_base |>
      dplyr::select(-"roster_name_key") |>
      dplyr::left_join(directory_aliases, by = "player_id") |>
      dplyr::bind_rows(season_roster_base) |>
      dplyr::distinct()
  }

  mapped_by_book <- priced_by_book |>
    dplyr::left_join(
      season_roster,
      by = c(
        "season",
        "prop_name_key" = "roster_name_key"
      ),
      relationship = "many-to-many"
    ) |>
    dplyr::left_join(
      roster |>
        dplyr::transmute(
          .data$game_id,
          .data$player_id,
          current_game_match = TRUE
        ) |>
        dplyr::distinct(),
      by = c("game_id", "player_id")
    ) |>
    dplyr::mutate(
      current_game_match = dplyr::coalesce(.data$current_game_match, FALSE)
    ) |>
    dplyr::arrange(
      .data$game_id, .data$player, .data$bookmaker_key,
      dplyr::desc(.data$current_game_match)
    ) |>
    dplyr::group_by(
      .data$game_id, .data$player, .data$bookmaker_key
    ) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup()

  matched <- mapped_by_book |>
    dplyr::filter(!is.na(.data$player_id)) |>
    dplyr::arrange(
      .data$game_id, .data$player_id, .data$bookmaker_key,
      dplyr::desc(.data$american_odds)
    ) |>
    dplyr::group_by(
      .data$game_id, .data$player_id, .data$bookmaker_key
    ) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::group_by(
      .data$game_id, .data$season, .data$week, .data$player_id
    ) |>
    dplyr::arrange(dplyr::desc(.data$american_odds), .by_group = TRUE) |>
    dplyr::summarise(
      player = dplyr::first(.data$player_display_name),
      prop_name_key = dplyr::first(.data$prop_name_key),
      player_display_name = dplyr::first(.data$player_display_name),
      position = dplyr::first(.data$position),
      team = dplyr::first(.data$team),
      current_game_match = any(.data$current_game_match),
      best_american_odds = dplyr::first(.data$american_odds),
      best_book = dplyr::first(.data$bookmaker),
      best_implied_probability = dplyr::first(.data$implied_probability),
      consensus_probability = stats::median(
        .data$implied_probability,
        na.rm = TRUE
      ),
      books_available = dplyr::n_distinct(.data$bookmaker_key),
      snapshot_lead_minutes = stats::median(
        .data$snapshot_lead_minutes,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  unmatched <- mapped_by_book |>
    dplyr::filter(is.na(.data$player_id)) |>
    dplyr::group_by(
      .data$game_id, .data$season, .data$week,
      .data$player, .data$prop_name_key
    ) |>
    dplyr::arrange(dplyr::desc(.data$american_odds), .by_group = TRUE) |>
    dplyr::summarise(
      player_id = NA_character_,
      player_display_name = NA_character_,
      position = NA_character_,
      team = NA_character_,
      current_game_match = FALSE,
      best_american_odds = dplyr::first(.data$american_odds),
      best_book = dplyr::first(.data$bookmaker),
      best_implied_probability = dplyr::first(.data$implied_probability),
      consensus_probability = stats::median(
        .data$implied_probability,
        na.rm = TRUE
      ),
      books_available = dplyr::n_distinct(.data$bookmaker_key),
      snapshot_lead_minutes = stats::median(
        .data$snapshot_lead_minutes,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  mapped <- dplyr::bind_rows(matched, unmatched)

  feature_columns <- setdiff(
    names(player_features),
    c(
      "game_id", "player_id", "season", "week",
      "player_display_name", "position", "team",
      "home_team", "away_team"
    )
  )

  mapped |>
    dplyr::left_join(
      dplyr::select(
        player_features,
        "game_id", "player_id",
        dplyr::all_of(feature_columns)
      ),
      by = c("game_id", "player_id"),
      relationship = "many-to-one"
    )
}
