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
  ## Fantasy-model TD projection for the live board. Same input the calibrator
  ## was fitted with (R/td_fantasy_signal.R); players it does not cover fall
  ## back to the paired two-input fit rather than being dropped.
  if (!exists("td_fantasy_signal_live")) source("R/td_fantasy_signal.R")
  fsig <- td_fantasy_signal_live()
  fantasy_p <- NULL
  if (!is.null(fsig) && nrow(fsig)) {
    fantasy_p <- fsig$fantasy_p_td[
      match(as.character(prop_board$player_id), fsig$player_id)
    ]
    message(sprintf("TD board: fantasy TD projection matched %d of %d players.",
                    sum(is.finite(fantasy_p)), nrow(prop_board)))
  }

  model_probability <- apply_market_calibrator(
    deployment$market_fit,
    fundamental_probability,
    prop_board$consensus_probability,
    position = prop_board$position,
    fantasy = fantasy_p
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

  ## Resolved once, outside the mutate, so a config missing these keys falls
  ## back to "exclude nothing" instead of erroring or silently dropping rows.
  .excl_pos <- toupper(as.character(config$strategy$exclude_positions))
  if (!length(.excl_pos)) .excl_pos <- character(0)
  .excl_lo <- suppressWarnings(as.numeric(config$strategy$exclude_odds_low))
  .excl_hi <- suppressWarnings(as.numeric(config$strategy$exclude_odds_high))
  if (!length(.excl_lo) || !is.finite(.excl_lo)) .excl_lo <- Inf
  if (!length(.excl_hi) || !is.finite(.excl_hi)) .excl_hi <- -Inf

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

  scored <- card |>
    dplyr::mutate(
      active_status = dplyr::case_when(
        toupper(.data$active_status) == "CONFIRMED" ~ "CONFIRMED",
        .data$roster_status != "ACT" ~ "OUT_OR_RESERVE",
        TRUE ~ "PENDING_GAME_DAY"
      ),
      ## Segment exclusions, measured on 2024-25 walk-forward (the seasons where
      ## a calibrator exists). Each of the three removes a subset that LOSES
      ## money; together they lift selected ROI from +16.1% to +21.1% and PnL
      ## from +482.7u to +534.7u over 2,533 bets, week-block CI [+10.8%, +32.5%].
      ##
      ##   QB           192 bets, -14.8% ROI, the only position with a negative
      ##                Brier edge against the best available price
      ##   +400..+499   228 bets, -8.1%; also the worst-priced band in the
      ##                market overall (-13.5% betting blind)
      ##   worse than   67 bets, -7.8%; almost entirely short-priced RB props
      ##   -150         where the market is efficient and the model adds nothing
      ##
      ## Kept as an explicit named gate rather than folded into eligible_price
      ## so the bet card shows WHY a row was dropped.
      excluded_segment =
        toupper(.data$position) %in% .excl_pos |
        (.data$best_american_odds >= .excl_lo &
           .data$best_american_odds <= .excl_hi),
      eligible_price = .data$books_available >=
        config$market$minimum_books &
        .data$best_american_odds >= config$market$minimum_american_odds &
        .data$best_american_odds <= config$market$maximum_american_odds &
        !.data$excluded_segment,
      low_total = .data$total_line <= config$strategy$low_total_max,
      tight_end = .data$position == "TE",
      heavy_favorite = .data$team_spread <=
        config$strategy$heavy_favorite_max_spread,
      ## WIDE RECEIVER qualifies on its own, added 2026-10-01.
      ##
      ## The situational filter (low total / TE / heavy favourite) was blocking
      ## a genuinely profitable group. Among plays that were already eligible
      ## and already past the 2% edge threshold, the ones the filter rejected
      ## split sharply by position:
      ##
      ##   blocked RB  238 bets  ROI  -2.3%   -5.4u   P(ROI<=0) 0.632
      ##   blocked WR  192 bets  ROI +50.0%  +95.9u   P(ROI<=0) 0.000
      ##
      ## So the filter was right about backs and wrong about receivers. The WR
      ## result replicates (2024 +33.9%, 2025 +65.4%), holds across EVERY price
      ## band (+100s +38%, +200s +27%, +300-400s +56%, +500+ +81%) rather than
      ## living in one, and wins 24 of 36 weeks.
      ##
      ## Adding it improves BOTH axes, which is rare: 548 -> 740 bets,
      ## ROI +42.5% -> +44.5%, PnL +233.0u -> +329.0u, CI [+0.291, +0.596].
      ## RB deliberately still has to clear a situational reason.
      wide_receiver = .data$position == "WR",
      core_signal = .data$eligible_price &
        .data$relative_edge >= config$strategy$core_edge &
        (.data$low_total | .data$tight_end),
      expanded_signal = .data$eligible_price &
        .data$relative_edge >= config$strategy$expanded_edge &
        (.data$low_total | .data$tight_end | .data$heavy_favorite |
           .data$wide_receiver),
      strategy_tier = dplyr::case_when(
        .data$core_signal ~ "CORE",
        .data$expanded_signal ~ "EXPANDED",
        .data$eligible_price & .data$relative_edge > 0 ~ "WATCHLIST",
        TRUE ~ "PASS"
      ),
      bet_reason = dplyr::case_when(
        .data$wide_receiver & !.data$low_total & !.data$heavy_favorite ~
          "Wide-receiver allocation edge",
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
    )

  ## Weekly bet-count cap, added 2026-10-01.
  ##
  ## config$strategy$weekly_bet_cap was a long-standing null placeholder
  ## (config/td_2026.yml bankroll section even had a dead "daily_or_weekly_cap"
  ## field for this exact purpose). It needed populating the week the gap
  ## actually bit: the live board's positive-edge rate hit 64.0% (275 of 430
  ## priced players), where the full 2024-25 walk-forward NEVER exceeded 41.8%
  ## in any single week (36 weeks checked). Root cause only partly chased down
  ## - a real zero-history-player imputation bug was found and fixed (see
  ## predict_fantasy_deployment()'s "Confirmed bug 2026-10-01" comment and the
  ## draft_priority decay above), which helped individual outliers but barely
  ## moved the aggregate rate - so the qualified-bet count this week (69-77,
  ## vs a backtested ~15-20/week average) is not fully explained and should
  ## not be trusted at face value until it is.
  ##
  ## This caps the NUMBER of bets, not the per-bet stake - feed_stake_units()
  ## already stakes every TD prop at a flat 0.5u regardless of edge (TD props
  ## were never edge-scaled at publish time, only on this card's own
  ## informational `units` column), so the real lever for an anomalous week is
  ## how many bets go out, not how big each one is.
  ##
  ## CORE fills the cap first - it is the better-validated tier (2024-25 ROI
  ## +51.1% vs EXPANDED's +30.7%, and existed before any of today's changes).
  ## Any EXPANDED slots remaining are filled by LOWEST relative_edge among
  ## EXPANDED qualifiers, not highest - the inverted priority is deliberate:
  ## this week's own data shows the model's claimed edge is currently the LEAST
  ## trustworthy signal at its extremes (TE mean relative_edge sat at +59% to
  ## +73% against a historical baseline of -2%), so preferring modest, more
  ## plausible edges over extreme ones is the conservative reading of exactly
  ## what today found. Bumped rows become WATCHLIST - visible on the board,
  ## tracked, never staked - not silently dropped.
  cap <- suppressWarnings(as.integer(config$strategy$weekly_bet_cap))
  if (length(cap) && is.finite(cap)) {
    core_idx <- which(scored$strategy_tier == "CORE")
    core_keep <- core_idx
    remaining <- max(0L, cap - length(core_keep))
    exp_idx <- which(scored$strategy_tier == "EXPANDED")
    exp_order <- exp_idx[order(scored$relative_edge[exp_idx])]  # smallest edge first
    exp_keep <- utils::head(exp_order, remaining)
    exp_bump <- setdiff(exp_idx, exp_keep)
    if (length(core_keep) > cap) {
      # CORE alone exceeds the cap - keep the cap's worth, smallest edge first,
      # for the same reason EXPANDED does; bump the rest to WATCHLIST too.
      core_order <- core_idx[order(scored$relative_edge[core_idx])]
      core_keep <- utils::head(core_order, cap)
      scored$strategy_tier[setdiff(core_idx, core_keep)] <- "WATCHLIST"
    }
    if (length(exp_bump)) scored$strategy_tier[exp_bump] <- "WATCHLIST"
    n_bumped <- length(exp_bump) + max(0L, length(core_idx) - length(core_keep))
    if (n_bumped > 0) {
      message(sprintf(
        "Weekly bet cap (%d): %d bet(s) bumped CORE/EXPANDED -> WATCHLIST.",
        cap, n_bumped
      ))
      scored <- scored |>
        dplyr::mutate(
          units = dplyr::if_else(.data$strategy_tier %in% c("CORE", "EXPANDED"),
                                 .data$units, 0),
          recommended_stake = .data$units * .data$unit_value,
          execution_status = dplyr::if_else(.data$strategy_tier == "WATCHLIST",
                                            "WATCHLIST", .data$execution_status)
        )
    }
  }

  scored |>
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
    # Carried onto the card because the touchdown edge is a price-capture edge,
    # not a forecasting one: re-graded at the consensus price the core tier
    # returns -13.7% against +9.8% at the best of four, and break-even sits at
    # roughly 57% capture of the gap between them. Without the consensus on the
    # card there is no way to express that as a minimum acceptable price.
    consensus_probability = double(),
    relative_edge = double(),
    expected_roi = double(),
    # Diagnostic pass-through, not model inputs to anything downstream of the
    # card: how much of this row's rolling history is stale. carryover_r3
    # counts how many of the player's last 3 games were a prior season;
    # team_changed_r3 flags a roster change inside that window. Early-season
    # rows (Week 1 in particular, where every rolling window is 100% prior-
    # season carryover by construction) are where these matter most - a
    # bettor or a tighter eligibility rule needs to see them on the card
    # itself, not just have them silently feeding the model.
    carryover_r3 = integer(),
    team_changed_r3 = integer(),
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
