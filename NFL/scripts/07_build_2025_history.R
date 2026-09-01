source("R/utilities.R")
assert_packages()
ensure_directories()

bets <- readr::read_csv("outputs/walk_forward_bets.csv", show_col_types = FALSE)
games <- readRDS("data/processed/game_features.rds") |>
  dplyr::select(
    "game_id", "home_score", "away_score",
    "surface", "temperature", "wind_speed"
  )

history <- bets |>
  dplyr::filter(
    .data$season == 2025,
    (.data$target == "game_total" & .data$model == "forward_linear") |
      (.data$target == "home_margin" & .data$model == "random_forest")
  ) |>
  # The challenger tournament made home selection a spread bet-eligibility
  # requirement, not just a tag: away selections returned -0.88% across the
  # full period while home selections carried the entire spread edge.
  dplyr::filter(
    .data$target == "game_total" | .data$bet_side == "home"
  ) |>
  dplyr::left_join(games, by = "game_id") |>
  dplyr::mutate(
    market = dplyr::if_else(.data$target == "game_total", "total", "spread"),
    model_name = dplyr::if_else(
      .data$target == "game_total",
      "forward_linear_total",
      "random_forest_margin"
    ),
    closing_line = dplyr::if_else(
      .data$target == "game_total", .data$total_line, .data$home_line
    ),
    market_projection = dplyr::if_else(
      .data$target == "game_total", .data$total_line, -.data$home_line
    ),
    selection = dplyr::case_when(
      .data$target == "game_total" & .data$bet_side == "over" ~
        paste0("Over ", .data$total_line),
      .data$target == "game_total" ~ paste0("Under ", .data$total_line),
      .data$bet_side == "home" ~
        paste0(.data$home_team, " ", sprintf("%+.1f", .data$home_line)),
      TRUE ~ paste0(.data$away_team, " ", sprintf("%+.1f", -.data$home_line))
    ),
    favorite_strength = abs(.data$home_line),
    interaction_tags = dplyr::case_when(
      .data$target == "game_total" & .data$bet_side == "over" &
        .data$favorite_strength >= 7 ~ "over_heavy_favorite",
      .data$target == "game_total" & .data$bet_side == "under" &
        .data$favorite_strength <= 2.5 ~ "under_small_favorite",
      .data$target == "game_total" & .data$bet_side == "under" ~ "total_under",
      .data$target == "game_total" ~ "total_over",
      .data$bet_side == "home" & .data$home_line < 0 ~ "spread_home_favorite",
      .data$bet_side == "home" ~ "spread_home_underdog",
      .data$home_line > 0 ~ "spread_away_favorite",
      TRUE ~ "spread_away_underdog"
    ),
    reason = dplyr::case_when(
      .data$target == "game_total" ~ paste0(
        "Forward-linear total edge ", sprintf("%+.2f", .data$edge),
        " points met the 5-point walk-forward threshold; projected ",
        toupper(.data$bet_side), "."
      ),
      TRUE ~ paste0(
        "Random-forest margin edge ", sprintf("%+.2f", .data$edge),
        " points met the 6-point walk-forward threshold; selected ",
        toupper(.data$bet_side), " side."
      )
    ),
    actual_score = paste0(.data$away_team, " ", .data$away_score, " - ",
                          .data$home_team, " ", .data$home_score),
    actual_total = .data$home_score + .data$away_score,
    actual_home_margin = .data$home_score - .data$away_score,
    result = dplyr::case_when(
      .data$bet_result > 0 ~ "W",
      .data$bet_result < 0 ~ "L",
      TRUE ~ "P"
    ),
    base_stake_units = dplyr::case_when(
      .data$market == "total" & abs(.data$edge) >= 8 ~ 4,
      .data$market == "total" & abs(.data$edge) >= 7 ~ 3,
      .data$market == "total" & abs(.data$edge) >= 6 ~ 2,
      .data$market == "total" ~ 1,
      .data$market == "spread" & abs(.data$edge) >= 9 ~ 3,
      .data$market == "spread" & abs(.data$edge) >= 7.5 ~ 2,
      TRUE ~ 1
    ),
    # Home selection is now a spread eligibility requirement, so it no longer
    # earns a stake boost; that would apply a uniform increase to every spread
    # wager while double-counting the signal that already gated the bet.
    interaction_unit_boost = dplyr::if_else(
      .data$interaction_tags %in% c(
        "over_heavy_favorite",
        "under_small_favorite"
      ),
      1, 0
    ),
    confidence_tier = pmin(
      5,
      .data$base_stake_units + .data$interaction_unit_boost
    ),
    stake_units = dplyr::case_when(
      .data$confidence_tier == 1 ~ 3,
      .data$confidence_tier == 2 ~ 5,
      .data$confidence_tier == 3 ~ 7,
      .data$confidence_tier == 4 ~ 9,
      TRUE ~ 10
    ),
    stake_pct = .data$stake_units * 0.01
  ) |>
  dplyr::arrange(.data$game_date, .data$game_id, .data$market)

dates <- unique(history$game_date)
bankroll <- 1000
history$bankroll_before_day <- NA_real_
history$unit_value <- NA_real_
history$stake_amount <- NA_real_
history$profit_amount <- NA_real_
history$bankroll_after_bet <- NA_real_

for (bet_date in dates) {
  idx <- which(history$game_date == bet_date)
  day_start <- bankroll
  day_unit <- day_start * 0.01
  running <- day_start
  for (i in idx) {
    stake <- day_unit * history$stake_units[i]
    pnl <- dplyr::case_when(
      history$bet_result[i] > 0 ~ stake * 100 / 110,
      history$bet_result[i] < 0 ~ -stake,
      TRUE ~ 0
    )
    history$bankroll_before_day[i] <- day_start
    history$unit_value[i] <- day_unit
    history$stake_amount[i] <- stake
    history$profit_amount[i] <- pnl
    running <- running + pnl
    history$bankroll_after_bet[i] <- running
  }
  bankroll <- running
}

history_output <- history |>
  dplyr::transmute(
    date = .data$game_date,
    week = .data$week,
    game = paste(.data$away_team, "@", .data$home_team),
    market = .data$market,
    model = .data$model_name,
    selection = .data$selection,
    model_prediction = round(.data$prediction, 2),
    closing_line = .data$closing_line,
    market_projection = .data$market_projection,
    raw_edge_points = round(.data$edge, 2),
    threshold_points = .data$selected_threshold,
    reason = .data$reason,
    interaction_tags = .data$interaction_tags,
    actual_score = .data$actual_score,
    actual_total = .data$actual_total,
    actual_home_margin = .data$actual_home_margin,
    result = .data$result,
    stake_units = .data$stake_units,
    stake_pct = .data$stake_pct,
    bankroll_before_day = round(.data$bankroll_before_day, 4),
    unit_value = round(.data$unit_value, 4),
    stake_amount = round(.data$stake_amount, 4),
    profit_amount = round(.data$profit_amount, 4),
    bankroll_after_bet = round(.data$bankroll_after_bet, 4)
  )

summary <- history_output |>
  dplyr::group_by(.data$market) |>
  dplyr::summarise(
    bets = dplyr::n(),
    wins = sum(.data$result == "W"),
    losses = sum(.data$result == "L"),
    pushes = sum(.data$result == "P"),
    amount_staked = sum(.data$stake_amount),
    profit = sum(.data$profit_amount),
    roi = .data$profit / .data$amount_staked,
    .groups = "drop"
  )

overall <- history_output |>
  dplyr::summarise(
    bets = dplyr::n(),
    wins = sum(.data$result == "W"),
    losses = sum(.data$result == "L"),
    pushes = sum(.data$result == "P"),
    amount_staked = sum(.data$stake_amount),
    profit = sum(.data$profit_amount),
    roi = .data$profit / .data$amount_staked,
    ending_bankroll = dplyr::last(.data$bankroll_after_bet)
  )
prior_peaks <- cummax(c(1000, head(history_output$bankroll_after_bet, -1)))
overall$peak_bankroll <- max(history_output$bankroll_after_bet)
overall$max_drawdown_units <- min(
  history_output$bankroll_after_bet - prior_peaks
)
overall$max_drawdown_pct <- min(
  (history_output$bankroll_after_bet - prior_peaks) / prior_peaks
)

readr::write_csv(history_output, "outputs/2025_betting_history.csv")
readr::write_csv(summary, "outputs/2025_betting_summary.csv")
readr::write_csv(overall, "outputs/2025_betting_overall.csv")

fmt_pct <- function(x) paste0(sprintf("%.2f", 100 * x), "%")
lines <- c(
  "# Illustrative 2025 restaked betting history",
  "",
  "Bet eligibility uses the core 2025 walk-forward rules: forward-linear total",
  "edges of at least 5 points and random-forest margin edges of at least 6",
  "points, with spread selections restricted to the home side per the",
  "challenger tournament. The 3–10 unit stake schedule is the newly requested",
  "2026 plan applied retrospectively, so this is an illustrative restaking",
  "replay rather than an audit-clean staking backtest.",
  "",
  "Staking begins with $1,000. One unit is 1% of the bankroll available at the",
  "start of the betting day. Every wager risks 3–10 units. Wagers on the same",
  "day use the same unit value; the unit compounds on the next betting day.",
  "All prices are -110, with no daily or weekly exposure cap.",
  "",
  "## Summary",
  "",
  paste0(
    "- Overall: ", overall$bets, " bets, ", overall$wins, "-",
    overall$losses, "-", overall$pushes, ", $",
    sprintf("%+.2f", overall$profit), ", ",
    fmt_pct(overall$roi), " ROI on amount staked."
  ),
  paste0("- Ending bankroll: $", sprintf("%.2f", overall$ending_bankroll), "."),
  paste0(
    "- Peak bankroll: $", sprintf("%.2f", overall$peak_bankroll),
    "; maximum drawdown: $", sprintf("%.2f", abs(overall$max_drawdown_units)),
    " (", fmt_pct(overall$max_drawdown_pct), ")."
  ),
  "",
  "| Market | Bets | W-L-P | Amount staked | Profit | ROI |",
  "|---|---:|---:|---:|---:|---:|"
)
for (i in seq_len(nrow(summary))) {
  lines <- c(
    lines,
    paste0(
      "| ", summary$market[i], " | ", summary$bets[i], " | ",
      summary$wins[i], "-", summary$losses[i], "-", summary$pushes[i],
      " | $", sprintf("%.2f", summary$amount_staked[i]),
      " | $", sprintf("%+.2f", summary$profit[i]),
      " | ", fmt_pct(summary$roi[i]), " |"
    )
  )
}

lines <- c(
  lines, "",
  "## Every wager",
  "",
  "| Date | Wk | Game | Bet | Prediction | Line | Edge | Tag | Units | Unit $ | Result | Stake | P/L | Bankroll |",
  "|---|---:|---|---|---:|---:|---:|---|---:|---:|:---:|---:|---:|---:|"
)
for (i in seq_len(nrow(history_output))) {
  lines <- c(
    lines,
    paste0(
      "| ", history_output$date[i], " | ", history_output$week[i],
      " | ", history_output$game[i],
      " | ", history_output$selection[i],
      " | ", sprintf("%.2f", history_output$model_prediction[i]),
      " | ", sprintf("%.1f", history_output$closing_line[i]),
      " | ", sprintf("%+.2f", history_output$raw_edge_points[i]),
      " | ", history_output$interaction_tags[i],
      " | ", sprintf("%.0f", history_output$stake_units[i]),
      " | $", sprintf("%.2f", history_output$unit_value[i]),
      " | ", history_output$result[i],
      " | $", sprintf("%.2f", history_output$stake_amount[i]),
      " | $", sprintf("%+.2f", history_output$profit_amount[i]),
      " | $", sprintf("%.2f", history_output$bankroll_after_bet[i]),
      " |"
    )
  )
}
writeLines(lines, "outputs/2025_betting_history.md")

bankroll_plot_data <- history_output |>
  dplyr::mutate(bet_number = dplyr::row_number())
bankroll_plot <- bankroll_plot_data |>
  ggplot2::ggplot(
    ggplot2::aes(.data$bet_number, .data$bankroll_after_bet)
  ) +
  ggplot2::geom_hline(yintercept = 1000, linetype = 2, color = "grey50") +
  ggplot2::geom_step(color = "#8C1D40", linewidth = 0.8) +
  ggplot2::labs(
    title = "Illustrative 2025 restaked walk-forward bankroll",
    subtitle = "$1,000 start; 1 unit = 1% of daily bankroll; 3–10 units per bet",
    x = "Wager number", y = "Bankroll"
  ) +
  ggplot2::theme_minimal(base_size = 12)
ggplot2::ggsave(
  "outputs/2025_walk_forward_bankroll.png",
  bankroll_plot,
  width = 9, height = 5.5, dpi = 160
)
message("2025 betting history written to outputs/.")
