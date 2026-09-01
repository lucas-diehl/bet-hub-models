source("R/utilities.R")
source("R/odds.R")
source("R/touchdown_features.R")
source("R/fantasy_prop_model.R")
source("R/dfs_salaries.R")
source("R/dfs_value_backtest.R")
assert_packages()
ensure_directories()

seed <- 20260728L
test_seasons <- 2017:2021
player_stats <- readRDS("data/raw/player_stats_2014_2021.rds")
schedules <- readRDS("data/raw/schedules.rds")
rotowire <- read_rotowire("data/raw/rotowire_games_archive.json")
salaries <- readRDS("data/processed/dfs_salaries.rds")

if (
  file.exists("data/processed/dfs_backtest_fantasy_features.rds") &&
    file.exists("data/processed/dfs_value_walk_forward.rds")
) {
  cat("Using cached historical features and walk-forward models...\n")
  features <- readRDS("data/processed/dfs_backtest_fantasy_features.rds")
  walk_forward <- readRDS("data/processed/dfs_value_walk_forward.rds")
} else {
  cat("Building historical game context and leakage-safe features...\n")
  game_context <- build_td_game_context(
    schedules,
    rotowire,
    seasons = 2014:2021
  )
  features <- prepare_fantasy_player_features(player_stats, game_context)
  saveRDS(features, "data/processed/dfs_backtest_fantasy_features.rds")

  cat("Training 2017-2021 walk-forward component models...\n")
  walk_forward <- walk_forward_fantasy_models(
    features,
    test_seasons = test_seasons,
    seed = seed
  )
  saveRDS(walk_forward, "data/processed/dfs_value_walk_forward.rds")
}

predictions <- build_historical_ppr_predictions(walk_forward, features)
main_games <- dk_main_slate_games(schedules, test_seasons)
salary_history_games <- dk_main_slate_games(schedules, 2014:2021)
salary_pool <- prepare_dk_salary_pool(salaries, salary_history_games)
matched <- join_predictions_to_dk_salaries(
  dplyr::semi_join(predictions, main_games, by = "game_id"),
  salary_pool
)

match_audit <- matched |>
  dplyr::count(.data$season, .data$match_method, name = "player_games") |>
  dplyr::group_by(.data$season) |>
  dplyr::mutate(match_rate = .data$player_games / sum(.data$player_games)) |>
  dplyr::ungroup()
readr::write_csv(match_audit, "outputs/dfs_value_match_audit.csv")

value_board <- add_market_salary_expectation(
  matched,
  salary_pool,
  test_seasons
)
readr::write_csv(value_board, "outputs/dfs_value_player_board.csv")
saveRDS(value_board, "data/processed/dfs_value_player_board.rds")

summaries <- build_value_summaries(value_board)
readr::write_csv(summaries$deciles, "outputs/dfs_value_by_decile.csv")
readr::write_csv(summaries$by_season, "outputs/dfs_value_top_decile_by_season.csv")
readr::write_csv(summaries$by_position, "outputs/dfs_value_by_position.csv")
readr::write_csv(summaries$interactions, "outputs/dfs_value_interactions.csv")
readr::write_csv(
  summaries$selection_comparison,
  "outputs/dfs_value_selection_comparison.csv"
)
readr::write_csv(
  summaries$model_advantage,
  "outputs/dfs_value_model_advantage.csv"
)

projection_metrics <- value_board |>
  dplyr::group_by(.data$season) |>
  dplyr::summarise(
    player_games = dplyr::n(),
    model_mae = mean(abs(.data$projected_ppr - .data$fantasy_points)),
    baseline_mae = mean(abs(.data$baseline_ppr - .data$fantasy_points)),
    mae_improvement = .data$baseline_mae - .data$model_mae,
    model_rank_correlation = stats::cor(
      .data$projected_ppr,
      .data$fantasy_points,
      method = "spearman"
    ),
    baseline_rank_correlation = stats::cor(
      .data$baseline_ppr,
      .data$fantasy_points,
      method = "spearman"
    ),
    .groups = "drop"
  )
readr::write_csv(
  projection_metrics,
  "outputs/dfs_value_projection_metrics.csv"
)

cat("Optimizing historical DraftKings lineups...\n")
lineup_backtest <- run_weekly_lineup_backtest(value_board, salary_pool)
readr::write_csv(
  lineup_backtest$weekly,
  "outputs/dfs_value_weekly_lineups.csv"
)
readr::write_csv(
  lineup_backtest$lineups,
  "outputs/dfs_value_lineup_players.csv"
)

lineup_summary <- lineup_backtest$weekly |>
  dplyr::group_by(.data$strategy, .data$season) |>
  dplyr::summarise(
    weeks = dplyr::n(),
    average_actual_score = mean(.data$actual_score),
    median_actual_score = stats::median(.data$actual_score),
    score_150_rate = mean(.data$actual_score >= 150),
    score_180_rate = mean(.data$actual_score >= 180),
    average_salary = mean(.data$salary),
    .groups = "drop"
  )
readr::write_csv(lineup_summary, "outputs/dfs_value_lineup_summary.csv")

model_vs_baseline <- lineup_backtest$weekly |>
  dplyr::select(
    "season", "week", "strategy", "actual_score"
  ) |>
  tidyr::pivot_wider(
    names_from = "strategy",
    values_from = "actual_score",
    names_prefix = "actual_"
  ) |>
  dplyr::mutate(
    model_lift = .data$actual_model - .data$actual_baseline,
    model_win = .data$model_lift > 0,
    tie = .data$model_lift == 0
  )
readr::write_csv(
  model_vs_baseline,
  "outputs/dfs_value_model_vs_baseline.csv"
)

set.seed(seed)
bootstrap_means <- replicate(
  5000,
  mean(sample(
    model_vs_baseline$model_lift,
    replace = TRUE
  ))
)
lineup_comparison <- model_vs_baseline |>
  dplyr::summarise(
    weeks = dplyr::n(),
    average_model_score = mean(.data$actual_model),
    average_baseline_score = mean(.data$actual_baseline),
    average_lift = mean(.data$model_lift),
    median_lift = stats::median(.data$model_lift),
    model_wins = sum(.data$model_win),
    baseline_wins = sum(!.data$model_win & !.data$tie),
    ties = sum(.data$tie),
    model_win_rate = mean(.data$model_win),
    lift_ci_low = stats::quantile(bootstrap_means, 0.025),
    lift_ci_high = stats::quantile(bootstrap_means, 0.975),
    bootstrap_probability_positive = mean(bootstrap_means > 0)
  )
readr::write_csv(
  lineup_comparison,
  "outputs/dfs_value_lineup_comparison.csv"
)

cat("Matched player-games:", nrow(value_board), "\n")
cat("Lineup weeks:", nrow(model_vs_baseline), "\n")
cat(
  "Average model lineup lift:",
  round(mean(model_vs_baseline$model_lift), 2),
  "\n"
)
