source("R/utilities.R")
source("R/models.R")
source("R/backtest.R")
source("R/market_residual_models.R")
assert_packages(c(
  required_packages,
  "mgcv"
))
ensure_directories()
cfg <- read_config()

features <- readRDS("data/processed/game_features.rds")
features <- add_challenger_features(features)

cat("Running market-residual challenger tournament...\n")
challengers <- walk_forward_challengers(features, cfg)
challenger_predictions <- challengers$predictions
readr::write_csv(
  challenger_predictions,
  "outputs/challenger_predictions.csv"
)
readr::write_csv(
  challengers$specifications,
  "outputs/challenger_selected_specs.csv"
)
saveRDS(
  challengers,
  "data/processed/spread_total_challengers.rds"
)

context <- features |>
  dplyr::select(
    "game_id", "market_margin", "market_total",
    "neutral_temperature", "neutral_wind", "surface"
  )
legacy <- readRDS("outputs/predictions.rds") |>
  dplyr::left_join(context, by = "game_id") |>
  dplyr::mutate(
    market_prediction = dplyr::if_else(
      .data$target == "home_margin",
      .data$market_margin,
      .data$market_total
    ),
    raw_residual_prediction =
      .data$prediction - .data$market_prediction,
    shrinkage_alpha = dplyr::if_else(
      .data$model == "sportsbook",
      0,
      1
    )
  )

all_predictions <- dplyr::bind_rows(
  legacy,
  challenger_predictions
) |>
  dplyr::arrange(
    .data$season, .data$week, .data$game_id,
    .data$target, .data$model
  )
readr::write_csv(
  all_predictions,
  "outputs/challenger_all_predictions.csv"
)

metrics_by_season <- score_predictions(all_predictions)
metrics_aggregate <- all_predictions |>
  dplyr::group_by(.data$target, .data$model) |>
  dplyr::summarise(
    games = dplyr::n(),
    mae = mae_vec(.data$truth, .data$prediction),
    rmse = rmse_vec(.data$truth, .data$prediction),
    r_squared = r_squared_vec(.data$truth, .data$prediction),
    bias = mean(.data$prediction - .data$truth),
    seasons_beating_market = sum(
      purrr::map_lgl(unique(.data$season), function(season_value) {
        model_mae <- mae_vec(
          .data$truth[.data$season == season_value],
          .data$prediction[.data$season == season_value]
        )
        market_mae <- mae_vec(
          .data$truth[.data$season == season_value],
          .data$market_prediction[.data$season == season_value]
        )
        model_mae < market_mae
      })
    ),
    .groups = "drop"
  )
readr::write_csv(
  metrics_by_season,
  "outputs/challenger_metrics_by_season.csv"
)
readr::write_csv(
  metrics_aggregate,
  "outputs/challenger_metrics_aggregate.csv"
)

cat("Selecting raw-edge thresholds walk-forward...\n")
raw_selected_bets <- selected_threshold_bets_walk_forward(
  all_predictions,
  cfg
)
raw_summary <- raw_selected_bets |>
  dplyr::group_by(.data$target, .data$model) |>
  dplyr::summarise(
    seasons = dplyr::n_distinct(.data$season),
    bets = dplyr::n(),
    wins = sum(.data$bet_result > 0),
    losses = sum(.data$bet_result < 0),
    pushes = sum(.data$bet_result == 0),
    win_rate = .data$wins / (.data$wins + .data$losses),
    profit_units = sum(.data$profit),
    roi = mean(.data$profit),
    profitable_seasons = sum(
      purrr::map_dbl(
        unique(.data$season),
        ~ sum(.data$profit[.data$season == .x])
      ) > 0
    ),
    .groups = "drop"
  )
readr::write_csv(
  raw_selected_bets,
  "outputs/challenger_raw_edge_bets.csv"
)
readr::write_csv(
  raw_summary,
  "outputs/challenger_raw_edge_summary.csv"
)

raw_uncertainty_individual <- betting_uncertainty(
  raw_selected_bets,
  iterations = 5000L,
  seed = cfg$backtest$seed,
  block_by_season = FALSE
)
raw_uncertainty_season <- betting_uncertainty(
  raw_selected_bets,
  iterations = 5000L,
  seed = cfg$backtest$seed,
  block_by_season = TRUE
)
raw_uncertainty <- dplyr::bind_rows(
  raw_uncertainty_individual,
  raw_uncertainty_season
)
readr::write_csv(
  raw_uncertainty,
  "outputs/challenger_raw_edge_uncertainty.csv"
)

cat("Calibrating sequential cover probabilities...\n")
probability_predictions <- add_sequential_cover_probabilities(
  challenger_predictions
)
probability_metrics <- probability_predictions |>
  dplyr::mutate(
    result_vs_market = .data$truth - .data$market_prediction,
    selected_win = as.numeric(
      .data$edge * .data$result_vs_market > 0
    )
  ) |>
  dplyr::filter(
    is.finite(.data$cover_probability),
    abs(.data$result_vs_market) > 1e-9,
    abs(.data$edge) > 1e-9
  ) |>
  dplyr::group_by(.data$target, .data$model, .data$season) |>
  dplyr::summarise(
    games = dplyr::n(),
    brier_score = mean(
      (.data$cover_probability - .data$selected_win)^2
    ),
    average_probability = mean(.data$cover_probability),
    actual_win_rate = mean(.data$selected_win),
    calibration_gap =
      .data$average_probability - .data$actual_win_rate,
    .groups = "drop"
  )
probability_bets <- select_probability_bets_walk_forward(
  probability_predictions,
  cfg$backtest$validation_seasons,
  cfg$backtest$minimum_bets
)
probability_summary <- probability_bets |>
  dplyr::group_by(.data$target, .data$model) |>
  dplyr::summarise(
    seasons = dplyr::n_distinct(.data$season),
    bets = dplyr::n(),
    wins = sum(.data$bet_result > 0),
    losses = sum(.data$bet_result < 0),
    pushes = sum(.data$bet_result == 0),
    win_rate = .data$wins / (.data$wins + .data$losses),
    average_probability = mean(.data$cover_probability),
    average_expected_value = mean(.data$expected_value),
    profit_units = sum(.data$profit),
    roi = mean(.data$profit),
    .groups = "drop"
  )
readr::write_csv(
  probability_predictions,
  "outputs/challenger_probability_predictions.csv"
)
readr::write_csv(
  probability_metrics,
  "outputs/challenger_probability_calibration.csv"
)
readr::write_csv(
  probability_bets,
  "outputs/challenger_probability_bets.csv"
)
readr::write_csv(
  probability_summary,
  "outputs/challenger_probability_summary.csv"
)

cat("Testing locked interactions and key-number crossings...\n")
locked_interactions <- summarize_locked_interactions(raw_selected_bets)
readr::write_csv(
  locked_interactions,
  "outputs/challenger_locked_interactions.csv"
)

key_number_summary <- add_locked_interaction_tags(raw_selected_bets) |>
  dplyr::filter(.data$target == "home_margin") |>
  dplyr::mutate(
    key_number_group = dplyr::case_when(
      .data$interaction_crosses_key_3 &
        .data$interaction_crosses_key_7 ~ "Crosses 3 and 7",
      .data$interaction_crosses_key_3 ~ "Crosses 3",
      .data$interaction_crosses_key_7 ~ "Crosses 7",
      TRUE ~ "Crosses neither"
    )
  ) |>
  dplyr::group_by(.data$model, .data$key_number_group) |>
  dplyr::summarise(
    bets = dplyr::n(),
    wins = sum(.data$bet_result > 0),
    losses = sum(.data$bet_result < 0),
    pushes = sum(.data$bet_result == 0),
    win_rate = .data$wins / (.data$wins + .data$losses),
    profit_units = sum(.data$profit),
    roi = mean(.data$profit),
    .groups = "drop"
  )
readr::write_csv(
  key_number_summary,
  "outputs/challenger_key_number_summary.csv"
)

best_accuracy <- metrics_aggregate |>
  dplyr::filter(.data$model != "sportsbook") |>
  dplyr::group_by(.data$target) |>
  dplyr::slice_min(.data$mae, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()
best_raw_roi <- raw_summary |>
  dplyr::group_by(.data$target) |>
  dplyr::slice_max(.data$roi, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()
best_probability <- probability_summary |>
  dplyr::group_by(.data$target) |>
  dplyr::slice_max(.data$roi, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()

format_target <- function(target) {
  ifelse(target == "game_total", "Total", "Spread")
}
accuracy_lines <- purrr::pmap_chr(
  best_accuracy,
  function(target, model, games, mae, rmse, r_squared, bias,
           seasons_beating_market) {
    paste0(
      "- ", format_target(target), ": ", model,
      " MAE ", sprintf("%.3f", mae),
      "; beat the market in ", seasons_beating_market,
      " of 8 seasons."
    )
  }
)
raw_lines <- purrr::pmap_chr(
  best_raw_roi,
  function(target, model, seasons, bets, wins, losses, pushes,
           win_rate, profit_units, roi, profitable_seasons) {
    paste0(
      "- ", format_target(target), ": ", model,
      ", ", bets, " bets, ",
      sprintf("%.1f%%", 100 * win_rate), " win rate, ",
      sprintf("%+.2f", profit_units), " units, ",
      sprintf("%.2f%%", 100 * roi), " ROI; ",
      profitable_seasons, " profitable seasons."
    )
  }
)
probability_lines <- if (nrow(best_probability)) {
  purrr::pmap_chr(
    best_probability,
    function(target, model, seasons, bets, wins, losses, pushes,
             win_rate, average_probability, average_expected_value,
             profit_units, roi) {
      paste0(
        "- ", format_target(target), ": ", model,
        ", ", bets, " probability-qualified bets, ",
        sprintf("%.1f%%", 100 * win_rate), " win rate, ",
        sprintf("%.2f%%", 100 * roi), " ROI."
      )
    }
  )
} else {
  "- No probability strategy met the prior-window minimum-bet requirement."
}

report <- c(
  "# Spread and total challenger tournament",
  "",
  "## Design",
  "",
  "All challenger forecasts are market-residual models: they predict the realized result minus the archived closing line. Hyperparameters, recency half-life, and market shrinkage are chosen on the two seasons immediately before each test season using MAE, not betting ROI. Betting thresholds are then selected independently from the prior two out-of-sample seasons.",
  "",
  "The tested challengers are recency-weighted ridge regression, random forest, XGBoost, and a restricted GAM. Existing direct-outcome models remain in the comparison.",
  "",
  "## Best predictive challengers",
  "",
  accuracy_lines,
  "",
  "## Best raw-edge walk-forward betting results",
  "",
  raw_lines,
  "",
  "## Probability-qualified results",
  "",
  probability_lines,
  "",
  "## Interpretation rules",
  "",
  "- A challenger is not promoted solely because it has the highest full-period ROI.",
  "- Predictive MAE, annual stability, bootstrap uncertainty, calibration, and locked interaction replication are considered together.",
  "- The archived line is treated as closing. Actual 2026 bet prices and line timestamps must be retained to evaluate executable expected value and closing-line value.",
  "- Interaction results are separately labeled discovery (2020-2022) and validation (2023-2025).",
  "- Fixed -110 grading remains a limitation of the historical archive."
)
writeLines(report, "outputs/challenger_model_report.md")

cat("Challenger tournament complete.\n")
cat("Predictions:", nrow(challenger_predictions), "\n")
cat("Raw-edge bets:", nrow(raw_selected_bets), "\n")
cat("Probability bets:", nrow(probability_bets), "\n")
