source("R/utilities.R")
source("R/touchdown_backtest.R")
assert_packages()
ensure_directories()

walk_forward <- readRDS("data/processed/td_walk_forward_model.rds")
predictions <- walk_forward$predictions

thresholds <- td_edge_threshold_table(predictions)
interactions <- td_interaction_table(predictions, edge_threshold = 0.03)
core_bets <- select_td_strategy_bets(predictions, strategy = "core")
expanded_bets <- select_td_strategy_bets(predictions, strategy = "expanded")

strategy_summary <- dplyr::bind_rows(
  summarise_td_bets(
    dplyr::filter(expanded_bets, .data$season %in% 2023:2024),
    "Expanded: 2023-24 validation",
    bootstrap = TRUE
  ),
  summarise_td_bets(
    dplyr::filter(expanded_bets, .data$season == 2025),
    "Expanded: 2025 retrospective test",
    bootstrap = TRUE
  ),
  summarise_td_bets(
    dplyr::filter(core_bets, .data$season %in% 2023:2024),
    "Core: 2023-24 validation",
    bootstrap = TRUE
  ),
  summarise_td_bets(
    dplyr::filter(core_bets, .data$season == 2025),
    "Core: 2025 retrospective test",
    bootstrap = TRUE
  )
)

strategy_by_season <- purrr::map_dfr(
  c("expanded", "core"),
  function(strategy) {
    strategy_bets <- if (strategy == "expanded") expanded_bets else core_bets
    purrr::map_dfr(2023:2025, function(season) {
      summarise_td_bets(
        dplyr::filter(strategy_bets, .data$season == season),
        paste(strategy, season),
        bootstrap = TRUE
      )
    })
  }
)

bankroll <- expanded_bets |>
  dplyr::filter(.data$season == 2025) |>
  simulate_td_compounding_bankroll(
    starting_bankroll = 1000,
    unit_fraction = 0.01
  )
core_bankroll <- core_bets |>
  dplyr::filter(.data$season == 2025) |>
  simulate_td_compounding_bankroll(
    starting_bankroll = 1000,
    unit_fraction = 0.01
  )

bet_columns <- c(
  "game_date", "season", "week", "game_id", "player", "position",
  "team", "opponent_team", "total_line", "team_spread",
  "temperature", "wind_speed", "precip_probability", "is_dome",
  "best_book", "best_american_odds", "model_probability",
  "best_implied_probability", "probability_edge", "relative_edge",
  "expected_roi", "bet_reason", "strategy_tier", "won", "flat_profit"
)
strategy_bets_output <- dplyr::bind_rows(core_bets, expanded_bets) |>
  dplyr::select(dplyr::all_of(bet_columns))
bankroll_output <- bankroll |>
  dplyr::select(
    dplyr::all_of(bet_columns),
    "units", "unit_value", "stake", "bet_profit",
    "bankroll_before", "bankroll_after", "drawdown"
  )

readr::write_csv(thresholds, "outputs/td_edge_thresholds.csv")
readr::write_csv(interactions, "outputs/td_interactions.csv")
readr::write_csv(strategy_bets_output, "outputs/td_strategy_bets.csv")
readr::write_csv(strategy_summary, "outputs/td_strategy_summary.csv")
readr::write_csv(strategy_by_season, "outputs/td_strategy_by_season.csv")
readr::write_csv(bankroll_output, "outputs/td_2025_paper_bankroll.csv")
readr::write_csv(
  core_bankroll |>
    dplyr::select(
      dplyr::all_of(bet_columns),
      "units", "unit_value", "stake", "bet_profit",
      "bankroll_before", "bankroll_after", "drawdown"
    ),
  "outputs/td_2025_core_paper_bankroll.csv"
)
saveRDS(bankroll, "data/processed/td_2025_paper_bankroll_full.rds")

cat(
  "Expanded paper strategy: edge >= 2% and ",
  "(total <= 42 or TE or team favored by 6+)\n"
)
print(strategy_summary)
if (nrow(bankroll)) {
  cat("Paper bankroll ending value:", tail(bankroll$bankroll_after, 1), "\n")
  cat("Paper bankroll maximum drawdown:", min(bankroll$drawdown), "\n")
}
