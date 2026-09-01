eligible_td_prices <- function(data) {
  data |>
    dplyr::filter(
      .data$books_available >= 2,
      .data$best_american_odds >= -300,
      .data$best_american_odds <= 700,
      is.finite(.data$relative_edge)
    )
}

cluster_bootstrap_td_roi <- function(data, iterations = 5000L, seed = 20260727L) {
  if (!nrow(data)) return(c(low = NA_real_, median = NA_real_, high = NA_real_))
  set.seed(seed)
  clusters <- split(data$flat_profit, data$game_id)
  cluster_names <- names(clusters)
  draws <- replicate(iterations, {
    sampled <- sample(cluster_names, length(cluster_names), replace = TRUE)
    profit <- unlist(clusters[sampled], use.names = FALSE)
    mean(profit)
  })
  stats::quantile(
    draws,
    probs = c(0.025, 0.5, 0.975),
    names = FALSE,
    na.rm = TRUE
  ) |>
    stats::setNames(c("low", "median", "high"))
}

summarise_td_bets <- function(data, label, bootstrap = FALSE) {
  result <- tibble::tibble(
    strategy = label,
    bets = nrow(data),
    games = dplyr::n_distinct(data$game_id),
    wins = sum(data$won),
    win_rate = mean(data$won),
    units_profit = sum(data$flat_profit),
    roi = mean(data$flat_profit),
    average_odds = mean(data$best_american_odds),
    average_edge_pct = 100 * mean(data$relative_edge)
  )
  if (bootstrap && nrow(data)) {
    interval <- cluster_bootstrap_td_roi(data)
    result$roi_ci_low <- interval[["low"]]
    result$roi_ci_high <- interval[["high"]]
  }
  result
}

td_edge_threshold_table <- function(
  data,
  thresholds = c(0, 0.03, 0.05, 0.075, 0.10, 0.15, 0.20, 0.30)
) {
  eligible <- eligible_td_prices(data)
  purrr::map_dfr(sort(unique(eligible$season)), function(test_season) {
    purrr::map_dfr(thresholds, function(threshold) {
      bets <- eligible[
        eligible$season == test_season &
          eligible$relative_edge >= threshold,
      ]
      summarise_td_bets(
        bets,
        paste0("edge_at_least_", 100 * threshold, "pct")
      ) |>
        dplyr::mutate(
          season = test_season,
          edge_threshold = threshold,
          .before = 1
        )
    })
  })
}

select_td_strategy_bets <- function(
  data,
  strategy = c("core", "expanded")
) {
  strategy <- match.arg(strategy)
  if (strategy == "core") {
    selected <- eligible_td_prices(data) |>
      dplyr::filter(
        .data$relative_edge >= 0.05,
        .data$total_line <= 42 | .data$position == "TE"
      )
  } else {
    selected <- eligible_td_prices(data) |>
      dplyr::filter(
        .data$relative_edge >= 0.02,
        .data$total_line <= 42 |
          .data$position == "TE" |
          .data$team_spread <= -6
      )
  }

  selected |>
    dplyr::mutate(
      bet_reason = dplyr::case_when(
        .data$total_line <= 42 & .data$position == "TE" &
          .data$team_spread <= -6 ~
          "Low total + TE + heavy favorite",
        .data$total_line <= 42 & .data$position == "TE" ~
          "Low total + tight-end edge",
        .data$total_line <= 42 & .data$team_spread <= -6 ~
          "Low total + heavy favorite",
        .data$position == "TE" & .data$team_spread <= -6 ~
          "Tight end + heavy favorite",
        .data$total_line <= 42 ~ "Low-total game edge",
        .data$position == "TE" ~ "Tight-end allocation edge",
        .data$team_spread <= -6 ~ "Heavy-favorite scoring environment",
        TRUE ~ "Qualified model edge"
      ),
      strategy_tier = strategy
    )
}

td_interaction_table <- function(data, edge_threshold = 0.03) {
  eligible <- eligible_td_prices(data) |>
    dplyr::filter(.data$relative_edge >= edge_threshold) |>
    dplyr::mutate(
      favorite_bucket = dplyr::case_when(
        .data$team_spread <= -6 ~ "Heavy favorite",
        .data$team_spread < 0 ~ "Favorite",
        .data$team_spread >= 6 ~ "Heavy underdog",
        TRUE ~ "Underdog/PK"
      ),
      total_bucket = dplyr::case_when(
        .data$total_line <= 42 ~ "Low (<=42)",
        .data$total_line >= 48 ~ "High (>=48)",
        TRUE ~ "Middle"
      ),
      weather_bucket = dplyr::case_when(
        .data$is_dome == 1 ~ "Dome",
        .data$wind_speed >= 15 ~ "High wind",
        .data$precip_probability >= 30 |
          .data$rain_flag == 1 |
          .data$snow_flag == 1 ~ "Precipitation",
        TRUE ~ "Normal outdoor"
      ),
      odds_bucket = cut(
        .data$best_american_odds,
        c(-Inf, 0, 200, 350, 500, Inf),
        labels = c("Minus", "+1 to +200", "+201 to +350", "+351 to +500", "+501+")
      )
    )

  group_summary <- function(group_name) {
    eligible |>
      dplyr::mutate(segment = as.character(.data[[group_name]])) |>
      dplyr::group_by(.data$season, .data$segment) |>
      dplyr::summarise(
        bets = dplyr::n(),
        wins = sum(.data$won),
        win_rate = mean(.data$won),
        roi = mean(.data$flat_profit),
        average_edge_pct = 100 * mean(.data$relative_edge),
        .groups = "drop"
      ) |>
      dplyr::mutate(interaction = group_name, .before = 1)
  }

  purrr::map_dfr(
    c(
      "position", "favorite_bucket", "total_bucket",
      "weather_bucket", "odds_bucket"
    ),
    group_summary
  )
}

td_paper_units <- function(edge) {
  dplyr::case_when(
    edge >= 0.50 ~ 10,
    edge >= 0.40 ~ 9,
    edge >= 0.30 ~ 8,
    edge >= 0.20 ~ 7,
    edge >= 0.15 ~ 6,
    edge >= 0.10 ~ 5,
    edge >= 0.075 ~ 4,
    TRUE ~ 3
  )
}

simulate_td_compounding_bankroll <- function(
  bets,
  starting_bankroll = 1000,
  unit_fraction = 0.01
) {
  bets <- bets |>
    dplyr::arrange(.data$game_date, .data$game_id, .data$player) |>
    dplyr::mutate(
      units = td_paper_units(.data$relative_edge),
      bankroll_before = NA_real_,
      daily_starting_bankroll = NA_real_,
      unit_value = NA_real_,
      stake = NA_real_,
      bet_profit = NA_real_,
      bankroll_after = NA_real_
    )
  bankroll <- starting_bankroll

  for (date in unique(bets$game_date)) {
    indices <- which(bets$game_date == date)
    daily_start <- bankroll
    unit_value <- daily_start * unit_fraction
    for (index in indices) {
      bets$bankroll_before[[index]] <- bankroll
      bets$daily_starting_bankroll[[index]] <- daily_start
      bets$unit_value[[index]] <- unit_value
      bets$stake[[index]] <- bets$units[[index]] * unit_value
      bets$bet_profit[[index]] <- bets$stake[[index]] *
        bets$flat_profit[[index]]
      bankroll <- bankroll + bets$bet_profit[[index]]
      bets$bankroll_after[[index]] <- bankroll
    }
  }

  bets |>
    dplyr::mutate(
      running_peak = cummax(.data$bankroll_after),
      drawdown = .data$bankroll_after / .data$running_peak - 1
    )
}
