source("R/utilities.R")
source("R/touchdown_model.R")
assert_packages()
ensure_directories()

player_features <- readRDS("data/processed/td_player_features.rds")
prop_board <- readRDS("data/processed/td_prop_board.rds")

walk_forward <- walk_forward_td_predictions(
  player_features,
  prop_board,
  test_seasons = 2023:2025,
  seed = 20260727L
)

saveRDS(walk_forward, "data/processed/td_walk_forward_model.rds")
deployment <- fit_td_deployment_model(
  player_features,
  walk_forward$predictions,
  calibration_season = 2025L,
  seed = 20260727L
)
saveRDS(deployment, "data/processed/td_deployment_model.rds")
readr::write_csv(
  deployment$feature_importance,
  "outputs/td_feature_importance.csv"
)
readr::write_csv(
  walk_forward$predictions,
  "outputs/td_walk_forward_predictions.csv"
)

metrics <- td_probability_metrics(walk_forward$predictions)
readr::write_csv(metrics, "outputs/td_probability_metrics.csv")

calibration <- walk_forward$predictions |>
  dplyr::mutate(
    probability_band = cut(
      .data$model_probability,
      breaks = seq(0, 1, by = 0.1),
      include.lowest = TRUE
    )
  ) |>
  dplyr::group_by(.data$season, .data$probability_band) |>
  dplyr::summarise(
    player_games = dplyr::n(),
    predicted_probability = mean(.data$model_probability),
    actual_td_rate = mean(.data$anytime_td),
    market_probability = mean(.data$consensus_probability),
    .groups = "drop"
  )
readr::write_csv(calibration, "outputs/td_calibration_bands.csv")

cat("Walk-forward priced player-games:", nrow(walk_forward$predictions), "\n")
print(metrics)
