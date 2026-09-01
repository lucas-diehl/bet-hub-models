td_payload_records <- function(x) {
  if (is.null(x) || !length(x)) return(list())
  converted <- jsonlite::fromJSON(
    jsonlite::toJSON(
      x,
      dataframe = "rows",
      auto_unbox = TRUE,
      na = "null",
      null = "null"
    ),
    simplifyVector = FALSE
  )
  if (!is.null(names(converted)) && "id" %in% names(converted)) {
    return(list(converted))
  }
  converted
}

odds_api_team_abbreviations <- function() {
  c(
    "Arizona Cardinals" = "ARI",
    "Atlanta Falcons" = "ATL",
    "Baltimore Ravens" = "BAL",
    "Buffalo Bills" = "BUF",
    "Carolina Panthers" = "CAR",
    "Chicago Bears" = "CHI",
    "Cincinnati Bengals" = "CIN",
    "Cleveland Browns" = "CLE",
    "Dallas Cowboys" = "DAL",
    "Denver Broncos" = "DEN",
    "Detroit Lions" = "DET",
    "Green Bay Packers" = "GB",
    "Houston Texans" = "HOU",
    "Indianapolis Colts" = "IND",
    "Jacksonville Jaguars" = "JAX",
    "Kansas City Chiefs" = "KC",
    "Las Vegas Raiders" = "LV",
    "Los Angeles Chargers" = "LAC",
    "Los Angeles Rams" = "LAR",
    "Miami Dolphins" = "MIA",
    "Minnesota Vikings" = "MIN",
    "New England Patriots" = "NE",
    "New Orleans Saints" = "NO",
    "New York Giants" = "NYG",
    "New York Jets" = "NYJ",
    "Philadelphia Eagles" = "PHI",
    "Pittsburgh Steelers" = "PIT",
    "San Francisco 49ers" = "SF",
    "Seattle Seahawks" = "SEA",
    "Tampa Bay Buccaneers" = "TB",
    "Tennessee Titans" = "TEN",
    "Washington Commanders" = "WAS"
  )
}

match_td_events_to_schedule <- function(events, schedules) {
  records <- td_payload_records(events)
  if (!length(records)) {
    return(tibble::tibble(
      event_id = character(),
      game_id = character(),
      commence_time = character()
    ))
  }
  abbreviations <- odds_api_team_abbreviations()
  event_frame <- purrr::map_dfr(records, function(event) {
    tibble::tibble(
      event_id = as.character(event$id),
      commence_time = as.character(event$commence_time),
      event_home_team = unname(abbreviations[[as.character(event$home_team)]]),
      event_away_team = unname(abbreviations[[as.character(event$away_team)]])
    )
  }) |>
    dplyr::filter(
      !is.na(.data$event_home_team),
      !is.na(.data$event_away_team)
    )

  schedule_frame <- schedules |>
    dplyr::filter(.data$season == 2026, .data$game_type == "REG") |>
    dplyr::transmute(
      game_id = as.character(.data$game_id),
      schedule_home_team = normalize_team(.data$home_team),
      schedule_away_team = normalize_team(.data$away_team),
      schedule_kickoff = nfl_kickoff_utc(.data$gameday, .data$gametime)
    )

  event_frame |>
    dplyr::inner_join(
      schedule_frame,
      by = c(
        "event_home_team" = "schedule_home_team",
        "event_away_team" = "schedule_away_team"
      )
    ) |>
    dplyr::mutate(
      kickoff_gap_hours = abs(as.numeric(difftime(
        lubridate::ymd_hms(.data$commence_time, tz = "UTC"),
        lubridate::ymd_hms(.data$schedule_kickoff, tz = "UTC"),
        units = "hours"
      )))
    ) |>
    dplyr::group_by(.data$event_id) |>
    dplyr::slice_min(.data$kickoff_gap_hours, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$kickoff_gap_hours <= 12) |>
    dplyr::select(
      "event_id", "game_id", "commence_time", "schedule_kickoff",
      "event_home_team", "event_away_team"
    )
}

flatten_current_td_game_lines <- function(payload) {
  events <- td_payload_records(payload)
  if (!length(events)) {
    return(tibble::tibble(
      event_id = character(),
      home_spread = double(),
      total_line = double(),
      line_books = integer()
    ))
  }

  raw <- purrr::map_dfr(events, function(event) {
    bookmakers <- event$bookmakers %||% list()
    purrr::map_dfr(bookmakers, function(bookmaker) {
      markets <- bookmaker$markets %||% list()
      purrr::map_dfr(markets, function(market) {
        outcomes <- market$outcomes %||% list()
        if (!length(outcomes)) return(tibble::tibble())
        if (identical(market$key, "totals")) {
          over <- purrr::keep(
            outcomes,
            ~ identical(tolower(as.character(.x$name)), "over")
          )
          if (!length(over)) return(tibble::tibble())
          return(tibble::tibble(
            event_id = as.character(event$id),
            bookmaker_key = as.character(bookmaker$key),
            market = "total",
            point = as.numeric(over[[1]]$point)
          ))
        }
        if (identical(market$key, "spreads")) {
          home <- purrr::keep(
            outcomes,
            ~ identical(
              as.character(.x$name),
              as.character(event$home_team)
            )
          )
          if (!length(home)) return(tibble::tibble())
          return(tibble::tibble(
            event_id = as.character(event$id),
            bookmaker_key = as.character(bookmaker$key),
            market = "home_spread",
            point = as.numeric(home[[1]]$point)
          ))
        }
        tibble::tibble()
      })
    })
  })

  if (!nrow(raw)) {
    return(tibble::tibble(
      event_id = character(),
      home_spread = double(),
      total_line = double(),
      line_books = integer()
    ))
  }

  raw |>
    dplyr::group_by(.data$event_id) |>
    dplyr::summarise(
      home_spread = stats::median(
        .data$point[.data$market == "home_spread"],
        na.rm = TRUE
      ),
      total_line = stats::median(
        .data$point[.data$market == "total"],
        na.rm = TRUE
      ),
      line_books = dplyr::n_distinct(.data$bookmaker_key),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dplyr::across(
        c("home_spread", "total_line"),
        ~ dplyr::if_else(is.nan(.x), NA_real_, .x)
      )
    )
}

flatten_current_anytime_td <- function(payload, game) {
  wrapper <- list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    data = td_payload_records(payload)[[1]]
  )
  flatten_anytime_td_payload(wrapper, game)
}

build_td_2026_game_context <- function(
  schedules,
  event_map,
  game_lines,
  overrides = NULL
) {
  context <- schedules |>
    dplyr::filter(
      .data$season == 2026,
      .data$game_type == "REG",
      .data$game_id %in% event_map$game_id
    ) |>
    dplyr::transmute(
      game_id = as.character(.data$game_id),
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
      schedule_surface = tolower(as.character(.data$surface))
    ) |>
    dplyr::left_join(
      dplyr::left_join(game_lines, event_map, by = "event_id") |>
        dplyr::select("game_id", "home_spread", "total_line", "line_books"),
      by = "game_id"
    ) |>
    dplyr::mutate(
      total_line = dplyr::coalesce(.data$total_line, .data$schedule_total),
      home_line = dplyr::coalesce(
        .data$home_spread,
        .data$schedule_home_line
      ),
      is_dome = as.integer(
        .data$schedule_roof %in% c("dome", "closed")
      ),
      grass_flag = as.integer(stringr::str_detect(
        .data$schedule_surface,
        "grass"
      )),
      turf_flag = as.integer(.data$grass_flag == 0L)
    )

  if (!is.null(overrides) && nrow(overrides)) {
    overrides <- overrides |>
      dplyr::mutate(
        dplyr::across(
          c(
            "temperature", "wind_speed", "precip_probability",
            "rain_flag", "snow_flag"
          ),
          as.numeric
        )
      ) |>
      dplyr::rename(
        override_temperature = "temperature",
        override_wind_speed = "wind_speed",
        override_precip_probability = "precip_probability",
        override_rain_flag = "rain_flag",
        override_snow_flag = "snow_flag",
        override_weather_status = "weather_status"
      )
    context <- dplyr::left_join(context, overrides, by = "game_id")
  }

  required_override_columns <- c(
    "override_temperature", "override_wind_speed",
    "override_precip_probability", "override_rain_flag",
    "override_snow_flag", "override_weather_status"
  )
  for (column in setdiff(required_override_columns, names(context))) {
    context[[column]] <- NA
  }

  context |>
    dplyr::mutate(
      temperature = dplyr::if_else(
        .data$is_dome == 1L,
        70,
        dplyr::coalesce(
          as.numeric(.data$override_temperature),
          .data$schedule_temperature
        )
      ),
      wind_speed = dplyr::if_else(
        .data$is_dome == 1L,
        0,
        dplyr::coalesce(
          as.numeric(.data$override_wind_speed),
          .data$schedule_wind
        )
      ),
      precip_probability = dplyr::if_else(
        .data$is_dome == 1L,
        0,
        as.numeric(.data$override_precip_probability)
      ),
      weather_status = dplyr::case_when(
        .data$is_dome == 1L ~ "VERIFIED_DOME",
        toupper(as.character(.data$override_weather_status)) == "VERIFIED" ~
          "VERIFIED",
        TRUE ~ "PENDING"
      ),
      rain_flag = dplyr::if_else(
        .data$is_dome == 1L,
        0L,
        as.integer(dplyr::coalesce(
          as.numeric(.data$override_rain_flag),
          0
        ))
      ),
      snow_flag = dplyr::if_else(
        .data$is_dome == 1L,
        0L,
        as.integer(dplyr::coalesce(
          as.numeric(.data$override_snow_flag),
          0
        ))
      ),
      cold_index = pmax(0, 40 - .data$temperature) / 40,
      high_wind_index = pmax(0, .data$wind_speed - 12) / 10
    ) |>
    dplyr::select(
      "game_id", "season", "week", "game_date",
      "home_team", "away_team", "total_line", "home_line",
      "temperature", "wind_speed", "precip_probability",
      "is_dome", "grass_flag", "turf_flag", "rain_flag", "snow_flag",
      "cold_index", "high_wind_index", "weather_status", "line_books"
    )
}

build_td_2026_player_features <- function(
  player_stats,
  rosters,
  historical_game_context,
  upcoming_game_context
) {
  if (!nrow(upcoming_game_context)) return(tibble::tibble())

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
      team = normalize_team(.data$team)
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
    dplyr::mutate(season_type = "REG")

  combined_stats <- dplyr::bind_rows(player_stats, synthetic)
  combined_context <- dplyr::bind_rows(
    historical_game_context,
    upcoming_game_context
  )

  build_td_player_features(combined_stats, combined_context) |>
    dplyr::filter(
      .data$season == 2026,
      .data$game_id %in% upcoming_game_context$game_id
    )
}

td_probability_to_american <- function(probability) {
  probability <- clip_probability(probability)
  dplyr::if_else(
    probability >= 0.5,
    -100 * probability / (1 - probability),
    100 * (1 - probability) / probability
  )
}

score_td_deployment_board <- function(prop_board, deployment) {
  if (!nrow(prop_board)) return(prop_board)
  features <- deployment$features
  matrix_frame <- prop_board[, features, drop = FALSE]
  for (feature in features) {
    matrix_frame[[feature]] <- as.numeric(matrix_frame[[feature]])
    matrix_frame[[feature]][!is.finite(matrix_frame[[feature]])] <-
      deployment$medians[[feature]]
  }
  raw_probability <- clip_probability(stats::predict(
    revive_booster(deployment$fundamental_fit, "td fundamental"),
    as.matrix(matrix_frame)
  ))
  fundamental_probability <- apply_platt_calibrator(
    deployment$platt_fit,
    raw_probability
  )
  model_probability <- apply_market_calibrator(
    deployment$market_fit,
    fundamental_probability,
    prop_board$consensus_probability
  )
  decimal_odds <- dplyr::if_else(
    prop_board$best_american_odds > 0,
    1 + prop_board$best_american_odds / 100,
    1 + 100 / abs(prop_board$best_american_odds)
  )

  prop_board |>
    dplyr::mutate(
      fundamental_probability = fundamental_probability,
      model_probability = model_probability,
      fair_american_odds = td_probability_to_american(.data$model_probability),
      decimal_odds = decimal_odds,
      probability_edge = .data$model_probability -
        .data$best_implied_probability,
      relative_edge = .data$probability_edge /
        .data$best_implied_probability,
      expected_roi = .data$model_probability * .data$decimal_odds - 1
    )
}

td_2026_units <- function(edge, config) {
  units <- td_paper_units(edge)
  pmin(
    pmax(units, config$bankroll$minimum_units),
    config$bankroll$maximum_units
  )
}

prepare_td_2026_bet_card <- function(
  scored_board,
  rosters,
  config,
  bankroll = config$bankroll$starting_bankroll,
  player_overrides = NULL
) {
  if (!nrow(scored_board)) return(scored_board)

  roster_status <- rosters |>
    dplyr::filter(.data$season == config$season, !is.na(.data$gsis_id)) |>
    dplyr::arrange(.data$gsis_id, dplyr::desc(.data$week)) |>
    dplyr::group_by(.data$gsis_id) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      player_id = .data$gsis_id,
      roster_status = .data$status
    )

  card <- scored_board |>
    dplyr::left_join(roster_status, by = "player_id")

  if (!is.null(player_overrides) && nrow(player_overrides)) {
    card <- card |>
      dplyr::left_join(
        player_overrides |>
          dplyr::select("game_id", "player_id", "active_status"),
        by = c("game_id", "player_id")
      )
  }
  if (!"active_status" %in% names(card)) card$active_status <- NA_character_

  card |>
    dplyr::mutate(
      active_status = dplyr::case_when(
        toupper(.data$active_status) == "CONFIRMED" ~ "CONFIRMED",
        .data$roster_status != "ACT" ~ "OUT_OR_RESERVE",
        TRUE ~ "PENDING_GAME_DAY"
      ),
      eligible_price = .data$books_available >=
        config$market$minimum_books &
        .data$best_american_odds >= config$market$minimum_american_odds &
        .data$best_american_odds <= config$market$maximum_american_odds,
      low_total = .data$total_line <= config$strategy$low_total_max,
      tight_end = .data$position == "TE",
      heavy_favorite = .data$team_spread <=
        config$strategy$heavy_favorite_max_spread,
      core_signal = .data$eligible_price &
        .data$relative_edge >= config$strategy$core_edge &
        (.data$low_total | .data$tight_end),
      expanded_signal = .data$eligible_price &
        .data$relative_edge >= config$strategy$expanded_edge &
        (.data$low_total | .data$tight_end | .data$heavy_favorite),
      strategy_tier = dplyr::case_when(
        .data$core_signal ~ "CORE",
        .data$expanded_signal ~ "EXPANDED",
        .data$eligible_price & .data$relative_edge > 0 ~ "WATCHLIST",
        TRUE ~ "PASS"
      ),
      bet_reason = dplyr::case_when(
        .data$low_total & .data$tight_end & .data$heavy_favorite ~
          "Low total + TE + heavy favorite",
        .data$low_total & .data$tight_end ~
          "Low total + tight-end edge",
        .data$low_total & .data$heavy_favorite ~
          "Low total + heavy favorite",
        .data$tight_end & .data$heavy_favorite ~
          "Tight end + heavy favorite",
        .data$low_total ~ "Low-total game edge",
        .data$tight_end ~ "Tight-end allocation edge",
        .data$heavy_favorite ~ "Heavy-favorite scoring environment",
        TRUE ~ "No validated interaction"
      ),
      weather_ready = .data$is_dome == 1L |
        .data$weather_status %in% c("VERIFIED", "VERIFIED_DOME"),
      active_ready = .data$active_status == "CONFIRMED",
      line_ready = is.finite(.data$total_line) &
        is.finite(.data$team_spread),
      execution_status = dplyr::case_when(
        .data$strategy_tier == "PASS" ~ "PASS",
        .data$strategy_tier == "WATCHLIST" ~ "WATCHLIST",
        !.data$line_ready ~ "AWAITING_GAME_LINE",
        !.data$weather_ready ~ "AWAITING_WEATHER",
        !.data$active_ready ~ "AWAITING_ACTIVE_STATUS",
        config$mode == "paper" & .data$strategy_tier == "CORE" ~
          "PAPER_CORE_READY",
        config$mode == "paper" ~ "PAPER_EXPANDED_READY",
        .data$strategy_tier == "CORE" ~ "CORE_READY",
        TRUE ~ "EXPANDED_READY"
      ),
      units = dplyr::if_else(
        .data$strategy_tier %in% c("CORE", "EXPANDED"),
        as.numeric(td_2026_units(.data$relative_edge, config)),
        0
      ),
      unit_value = bankroll * config$bankroll$unit_fraction,
      recommended_stake = .data$units * .data$unit_value,
      refresh_timestamp_utc = format(
        Sys.time(),
        "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      )
    ) |>
    dplyr::group_by(.data$game_id) |>
    dplyr::mutate(
      game_qualified_bets = sum(
        .data$strategy_tier %in% c("CORE", "EXPANDED")
      ),
      game_exposure_units = sum(.data$units)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      factor(.data$strategy_tier, c("CORE", "EXPANDED", "WATCHLIST", "PASS")),
      dplyr::desc(.data$relative_edge)
    )
}

empty_td_2026_board <- function() {
  tibble::tibble(
    game_date = as.Date(character()),
    season = integer(),
    week = integer(),
    game_id = character(),
    player_id = character(),
    player = character(),
    position = character(),
    team = character(),
    opponent_team = character(),
    total_line = double(),
    team_spread = double(),
    temperature = double(),
    wind_speed = double(),
    precip_probability = double(),
    weather_status = character(),
    active_status = character(),
    best_book = character(),
    best_american_odds = double(),
    books_available = integer(),
    model_probability = double(),
    fair_american_odds = double(),
    best_implied_probability = double(),
    relative_edge = double(),
    expected_roi = double(),
    strategy_tier = character(),
    bet_reason = character(),
    execution_status = character(),
    units = double(),
    unit_value = double(),
    recommended_stake = double(),
    game_exposure_units = double(),
    refresh_timestamp_utc = character()
  )
}
