source("R/utilities.R")
source("R/models.R")
source("R/backtest.R")
assert_packages()
ensure_directories()
cfg <- read_config()
features <- readRDS("data/processed/game_features.rds")

predictions <- walk_forward_predictions(features, cfg)
metrics <- score_predictions(predictions)
thresholds <- threshold_table(predictions, cfg)
selected_thresholds <- select_thresholds_walk_forward(predictions, cfg)
selected_bets <- selected_threshold_bets_walk_forward(predictions, cfg)
bankroll <- bankroll_analysis(selected_bets, cfg)
edge_bins <- edge_bin_analysis(selected_bets)
bootstrap <- paired_bootstrap(
  predictions,
  cfg$backtest$bootstrap_iterations,
  cfg$backtest$seed
)

saveRDS(predictions, "outputs/predictions.rds")
readr::write_csv(predictions, "outputs/predictions.csv")
readr::write_csv(metrics, "outputs/model_metrics_by_season.csv")
readr::write_csv(thresholds, "outputs/threshold_results.csv")
readr::write_csv(
  selected_thresholds,
  "outputs/walk_forward_selected_thresholds.csv"
)
readr::write_csv(selected_bets, "outputs/walk_forward_bets.csv")
readr::write_csv(bankroll$summary, "outputs/bankroll_summary.csv")
readr::write_csv(bankroll$paths, "outputs/bankroll_paths.csv")
readr::write_csv(edge_bins, "outputs/edge_bins.csv")
readr::write_csv(bootstrap, "outputs/paired_bootstrap.csv")
message("Backtest outputs written to outputs/.")
