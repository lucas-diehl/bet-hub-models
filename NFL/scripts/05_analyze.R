source("R/utilities.R")
source("R/backtest.R")
assert_packages()
ensure_directories()
cfg <- read_config()

predictions <- readr::read_csv("outputs/predictions.csv", show_col_types = FALSE)
bets <- readr::read_csv("outputs/walk_forward_bets.csv", show_col_types = FALSE)

aggregate_metrics <- predictions |>
  dplyr::group_by(.data$target, .data$model) |>
  dplyr::summarise(
    games = dplyr::n(),
    mae = mae_vec(.data$truth, .data$prediction),
    rmse = rmse_vec(.data$truth, .data$prediction),
    r_squared = r_squared_vec(.data$truth, .data$prediction),
    bias = mean(.data$prediction - .data$truth),
    .groups = "drop"
  )

selection_breakdown <- bets |>
  dplyr::mutate(
    selection = dplyr::case_when(
      .data$target == "home_margin" & .data$bet_side == "home" &
        .data$home_line < 0 ~ "home favorite",
      .data$target == "home_margin" & .data$bet_side == "home" ~ "home underdog",
      .data$target == "home_margin" & .data$bet_side == "away" &
        .data$home_line > 0 ~ "away favorite",
      .data$target == "home_margin" ~ "away underdog",
      TRUE ~ .data$bet_side
    )
  ) |>
  dplyr::group_by(.data$target, .data$model, .data$selection) |>
  dplyr::summarise(
    bets = dplyr::n(),
    wins = sum(.data$bet_result > 0),
    losses = sum(.data$bet_result < 0),
    pushes = sum(.data$bet_result == 0),
    win_rate = .data$wins / (.data$wins + .data$losses),
    profit_units = sum(.data$profit),
    roi = .data$profit_units / .data$bets,
    .groups = "drop"
  )

week_breakdown <- bets |>
  dplyr::mutate(
    period = cut(
      .data$week,
      c(0, 4, 9, 14, 18),
      labels = c("Weeks 1-4", "Weeks 5-9", "Weeks 10-14", "Weeks 15-18")
    )
  ) |>
  dplyr::group_by(.data$target, .data$model, .data$period) |>
  dplyr::summarise(
    bets = dplyr::n(),
    wins = sum(.data$bet_result > 0),
    losses = sum(.data$bet_result < 0),
    pushes = sum(.data$bet_result == 0),
    win_rate = .data$wins / (.data$wins + .data$losses),
    profit_units = sum(.data$profit),
    roi = .data$profit_units / .data$bets,
    .groups = "drop"
  )

uncertainty <- dplyr::bind_rows(
  betting_uncertainty(bets, 5000, cfg$backtest$seed, FALSE),
  betting_uncertainty(bets, 5000, cfg$backtest$seed, TRUE)
)

recommended <- bets |>
  dplyr::filter(
    (.data$target == "game_total" & .data$model == "forward_linear") |
      (.data$target == "home_margin" & .data$model == "random_forest")
  )
portfolio_bets <- recommended |>
  dplyr::mutate(target = "portfolio", model = "total_forward_plus_margin_rf")
portfolio <- bankroll_analysis(portfolio_bets, cfg)

readr::write_csv(aggregate_metrics, "outputs/model_metrics_aggregate.csv")
readr::write_csv(selection_breakdown, "outputs/selection_breakdown.csv")
readr::write_csv(week_breakdown, "outputs/week_breakdown.csv")
readr::write_csv(uncertainty, "outputs/betting_uncertainty.csv")
readr::write_csv(portfolio$summary, "outputs/recommended_portfolio_summary.csv")
readr::write_csv(portfolio$paths, "outputs/recommended_portfolio_paths.csv")
message("Analysis tables written to outputs/.")

