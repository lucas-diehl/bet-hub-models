source("R/utilities.R")
source("R/injury_features.R")
source("R/fantasy_prop_model.R")
source("R/player_role_features.R")
source("R/touchdown_features.R")
source("R/touchdown_model.R")
source("R/touchdown_backtest.R")
assert_packages()
ensure_directories()

# The role features improved the touchdown model's Brier score with a confidence
# interval that excluded zero. Better calibration is not the same thing as
# better betting, because bets are taken only where the model disagrees with a
# price. This runs the full priced walk-forward on both feature sets and grades
# the locked core and expanded tiers on each.

player_features <- readRDS("data/processed/td_player_features.rds")
prop_board <- readRDS("data/processed/td_prop_board.rds")
injuries <- readRDS("data/raw/injuries_2009_2025.rds")
inputs <- readRDS("data/processed/player_role_inputs.rds")

augmented <- augment_player_features(
  player_features, injuries, inputs$ff, inputs$snaps, inputs$crosswalk
)

baseline_cols <- td_model_feature_names(player_features)
augmented_cols <- td_model_feature_names(augmented)
cat("Baseline model features:", length(baseline_cols), "\n")
cat("Augmented model features:", length(augmented_cols), "\n")
cat("Added:", paste(setdiff(augmented_cols, baseline_cols), collapse = ", "), "\n\n")

arms <- list(baseline = player_features, upgraded = augmented)

results <- purrr::imap(arms, function(feature_table, arm_name) {
  message("Running priced walk-forward: ", arm_name)
  walk_forward_td_predictions(
    feature_table, prop_board, test_seasons = 2023:2025, seed = 20260727L
  )
})
saveRDS(results, "data/processed/td_strategy_ab.rds")

summarise_arm <- function(predictions, arm_name) {
  core <- select_td_strategy_bets(predictions, strategy = "core")
  expanded <- select_td_strategy_bets(predictions, strategy = "expanded")
  dplyr::bind_rows(
    summarise_td_bets(
      dplyr::filter(core, .data$season %in% 2023:2024),
      "Core: 2023-24 validation", bootstrap = TRUE
    ),
    summarise_td_bets(
      dplyr::filter(core, .data$season == 2025),
      "Core: 2025 retrospective", bootstrap = TRUE
    ),
    summarise_td_bets(
      dplyr::filter(expanded, .data$season %in% 2023:2024),
      "Expanded: 2023-24 validation", bootstrap = TRUE
    ),
    summarise_td_bets(
      dplyr::filter(expanded, .data$season == 2025),
      "Expanded: 2025 retrospective", bootstrap = TRUE
    )
  ) |>
    dplyr::mutate(arm = arm_name, .before = 1)
}

summary_table <- purrr::imap_dfr(
  results, function(res, arm_name) summarise_arm(res$predictions, arm_name)
)
readr::write_csv(summary_table, "outputs/td_strategy_upgraded_summary.csv")

cat("=== Locked tiers, both feature sets ===\n")
print(as.data.frame(
  summary_table |>
    dplyr::select(
      "arm", "strategy", "bets", "games", "wins", "win_rate",
      "units_profit", "roi", "roi_ci_low", "roi_ci_high"
    )
), digits = 4)

probability <- purrr::imap_dfr(results, function(res, arm_name) {
  td_probability_metrics(res$predictions) |>
    dplyr::mutate(arm = arm_name, .before = 1)
})
readr::write_csv(probability, "outputs/td_strategy_upgraded_probability.csv")
cat("\n=== Priced-subset probability quality ===\n")
print(as.data.frame(probability), digits = 5)

# Edge-threshold curve on the priced universe, so the shape of any improvement
# is visible rather than only the two locked cells.
curves <- purrr::imap_dfr(results, function(res, arm_name) {
  td_edge_threshold_table(res$predictions) |>
    dplyr::mutate(arm = arm_name, .before = 1)
})
readr::write_csv(curves, "outputs/td_strategy_upgraded_thresholds.csv")
cat("\n=== Edge threshold curve ===\n")
print(as.data.frame(curves), digits = 4)
