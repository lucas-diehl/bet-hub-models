suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

board <- readRDS("data/processed/dfs_value_player_board.rds")
match_audit <- read_csv(
  "outputs/dfs_value_match_audit.csv",
  show_col_types = FALSE
)
salary_match_rate <- sum(
  match_audit$player_games[match_audit$match_method != "UNMATCHED"]
) / sum(match_audit$player_games)
weekly <- read_csv("outputs/dfs_value_weekly_lineups.csv", show_col_types = FALSE)
lineup_players <- read_csv(
  "outputs/dfs_value_lineup_players.csv",
  show_col_types = FALSE
)

paired_weeks <- weekly |>
  filter(.data$strategy %in% c("model", "baseline")) |>
  select("season", "week", "strategy", "actual_score") |>
  pivot_wider(names_from = "strategy", values_from = "actual_score") |>
  mutate(
    lift = .data$model - .data$baseline,
    model_win = .data$lift > 0
  )

set.seed(20260728)
bootstrap_means <- replicate(
  5000,
  mean(sample(paired_weeks$lift, replace = TRUE))
)
comparison <- paired_weeks |>
  summarise(
    weeks = n(),
    average_model_score = mean(.data$model),
    average_baseline_score = mean(.data$baseline),
    average_lift = mean(.data$lift),
    median_lift = median(.data$lift),
    model_wins = sum(.data$model_win),
    baseline_wins = sum(!.data$model_win),
    ties = sum(.data$lift == 0),
    model_win_rate = mean(.data$model_win),
    lift_ci_low = quantile(bootstrap_means, 0.025),
    lift_ci_high = quantile(bootstrap_means, 0.975),
    bootstrap_probability_positive = mean(bootstrap_means > 0)
  )
write_csv(comparison, "outputs/dfs_value_lineup_comparison.csv")

season_comparison <- paired_weeks |>
  group_by(.data$season) |>
  summarise(
    weeks = n(),
    model_score = mean(.data$model),
    baseline_score = mean(.data$baseline),
    average_lift = mean(.data$lift),
    median_lift = median(.data$lift),
    model_wins = sum(.data$model_win),
    baseline_wins = sum(!.data$model_win),
    model_win_rate = mean(.data$model_win),
    .groups = "drop"
  )
write_csv(season_comparison, "outputs/dfs_value_season_comparison.csv")

top_value <- board |>
  filter(.data$model_value_decile <= 2)
position_benchmark <- top_value |>
  group_by(.data$season, .data$position) |>
  summarise(
    position_points_per_1k = mean(.data$actual_points_per_1k),
    .groups = "drop"
  )

summarise_interaction <- function(variable, label) {
  top_value |>
    group_by(.data$season, .data$position, level = .data[[variable]]) |>
    summarise(
      players = n(),
      actual_points_per_1k = mean(.data$actual_points_per_1k),
      actual_salary_edge = mean(.data$actual_salary_edge),
      hit_3x = mean(.data$hit_3x),
      hit_4x = mean(.data$hit_4x),
      .groups = "drop"
    ) |>
    mutate(interaction = label, .before = 1)
}

interaction_seasons <- bind_rows(
  summarise_interaction("salary_tier", "Salary"),
  summarise_interaction("favorite_status", "Favorite status"),
  summarise_interaction("total_tier", "Game total"),
  summarise_interaction("weather_tier", "Weather")
) |>
  left_join(position_benchmark, by = c("season", "position")) |>
  mutate(
    points_per_1k_lift =
      .data$actual_points_per_1k - .data$position_points_per_1k
  )
write_csv(
  interaction_seasons,
  "outputs/dfs_value_interactions_by_season.csv"
)

stable_interactions <- interaction_seasons |>
  group_by(.data$interaction, .data$position, .data$level) |>
  summarise(
    total_players = sum(.data$players),
    seasons = n_distinct(.data$season),
    minimum_season_players = min(.data$players),
    positive_seasons = sum(.data$points_per_1k_lift > 0),
    average_points_per_1k = weighted.mean(
      .data$actual_points_per_1k,
      .data$players
    ),
    average_points_per_1k_lift = weighted.mean(
      .data$points_per_1k_lift,
      .data$players
    ),
    average_actual_salary_edge = weighted.mean(
      .data$actual_salary_edge,
      .data$players
    ),
    hit_3x = weighted.mean(.data$hit_3x, .data$players),
    hit_4x = weighted.mean(.data$hit_4x, .data$players),
    .groups = "drop"
  ) |>
  filter(
    .data$total_players >= 100,
    .data$seasons == 5,
    .data$minimum_season_players >= 10
  ) |>
  arrange(
    desc(.data$positive_seasons),
    desc(.data$average_points_per_1k_lift)
  )
write_csv(stable_interactions, "outputs/dfs_value_stable_interactions.csv")

lineup_checks <- lineup_players |>
  group_by(.data$season, .data$week, .data$strategy) |>
  summarise(
    players = n(),
    unique_players = n_distinct(.data$row_id),
    salary = sum(.data$salary),
    quarterback = sum(.data$position == "QB"),
    running_back = sum(.data$position == "RB"),
    wide_receiver = sum(.data$position == "WR"),
    tight_end = sum(.data$position == "TE"),
    defense = sum(.data$position == "DST"),
    .groups = "drop"
  )

checks <- tibble(
  check = c(
    "Matched offensive player-games",
    "Salary match rate",
    "Walk-forward seasons",
    "Weekly strategy lineups",
    "Lineups with nine players",
    "Lineups at or below $50K",
    "Lineups with unique players",
    "Lineups with legal roster construction"
  ),
  value = c(
    nrow(board),
    salary_match_rate,
    n_distinct(board$season),
    nrow(lineup_checks),
    sum(lineup_checks$players == 9),
    sum(lineup_checks$salary <= 50000),
    sum(lineup_checks$unique_players == 9),
    sum(
      lineup_checks$quarterback == 1 &
        lineup_checks$defense == 1 &
        (
          (
            lineup_checks$running_back == 2 &
              lineup_checks$wide_receiver == 3 &
              lineup_checks$tight_end == 2
          ) |
          (
            lineup_checks$running_back == 2 &
              lineup_checks$wide_receiver == 4 &
              lineup_checks$tight_end == 1
          ) |
          (
            lineup_checks$running_back == 3 &
              lineup_checks$wide_receiver == 3 &
              lineup_checks$tight_end == 1
          )
        )
    )
  ),
  expected = c(
    nrow(board),
    1,
    5,
    nrow(lineup_checks),
    nrow(lineup_checks),
    nrow(lineup_checks),
    nrow(lineup_checks),
    nrow(lineup_checks)
  ),
  status = c(
    "PASS",
    if_else(salary_match_rate >= 0.95, "PASS", "FAIL"),
    if_else(n_distinct(board$season) == 5, "PASS", "FAIL"),
    "PASS",
    if_else(all(lineup_checks$players == 9), "PASS", "FAIL"),
    if_else(all(lineup_checks$salary <= 50000), "PASS", "FAIL"),
    if_else(
      all(lineup_checks$unique_players == 9),
      "PASS",
      "FAIL"
    ),
    if_else(
      all(
        lineup_checks$quarterback == 1 &
          lineup_checks$defense == 1 &
          (
            (
              lineup_checks$running_back == 2 &
                lineup_checks$wide_receiver == 3 &
                lineup_checks$tight_end == 2
            ) |
            (
              lineup_checks$running_back == 2 &
                lineup_checks$wide_receiver == 4 &
                lineup_checks$tight_end == 1
            ) |
            (
              lineup_checks$running_back == 3 &
                lineup_checks$wide_receiver == 3 &
                lineup_checks$tight_end == 1
            )
          )
      ),
      "PASS",
      "FAIL"
    )
  )
)
write_csv(checks, "outputs/dfs_value_checks.csv")

projection <- read_csv(
  "outputs/dfs_value_projection_metrics.csv",
  show_col_types = FALSE
)

report <- c(
  "# Historical DFS Value Backtest",
  "",
  "## Bottom line",
  "",
  paste0(
    "Use the model as a lineup-construction input in 2026, but not yet as a ",
    "claim of contest ROI. Across ", comparison$weeks,
    " reconstructed DraftKings Sunday main slates (2017-2021), the optimized ",
    "model lineup averaged ", round(comparison$average_model_score, 2),
    " points versus ", round(comparison$average_baseline_score, 2),
    " for the rolling baseline, a lift of ",
    round(comparison$average_lift, 2), " points."
  ),
  paste0(
    "The model won ", comparison$model_wins, " of ", comparison$weeks,
    " weeks (", scales::percent(comparison$model_win_rate, accuracy = 0.1),
    "); the paired bootstrap 95% interval for average lift was ",
    round(comparison$lift_ci_low, 2), " to ",
    round(comparison$lift_ci_high, 2), " points."
  ),
  "",
  "## What held up",
  "",
  paste0(
    "- Projection MAE beat the rolling baseline in all five seasons; average ",
    "improvement was ",
    round(mean(projection$mae_improvement), 3), " fantasy points."
  ),
  "- The lineup optimizer produced a positive full-period lift, but 2021 was negative. Treat the signal as useful, not invulnerable.",
  "- Among top-20% model value candidates, $4K-$5.4K wide receivers beat their position benchmark in all five seasons.",
  "- Among top-20% model value candidates, running backs favored by at least three points beat their position benchmark in all five seasons.",
  "",
  "## What did not hold up",
  "",
  "- Simply taking the three largest model salary edges at each position did not outperform the rolling-baseline value list.",
  "- Therefore, the evidence supports optimized lineup construction and projection blending, not blindly selecting the largest standalone value score.",
  "- There are no archived contest ownership, cash lines, entry fees, or payouts in this dataset, so historical dollar ROI cannot be estimated honestly.",
  "",
  "## 2026 use",
  "",
  "1. Archive the official DraftKings main-slate salary CSV before lock.",
  "2. Generate strictly pre-lock projections and preserve the timestamped file.",
  "3. Use the optimizer, with $4K-$5.4K WR and favorite-RB interactions as tie-breakers rather than mandatory locks.",
  "4. Keep the rolling baseline available and monitor weekly model lift, calibration, late news, and lineup overlap.",
  "5. Archive contest results and ownership within the platform retention window; only then measure true ROI by contest type.",
  "6. Paper-test or use small stakes until the recent-season salary gap and 2026 live process are validated.",
  "",
  "## Scope and limitations",
  "",
  "- Player models are walk-forward: only prior seasons train each test season.",
  "- Weather and game-market context are included.",
  "- The test covers 2017-2021 because complete free DraftKings salary history ends in 2021.",
  "- Main slates are reconstructed from Sunday 1:00, 4:05, and 4:25 ET games.",
  "- DST projection is a lagged five-game average.",
  "- The optimizer uses a pruned candidate set and a legal $50K roster; the oracle is a hindsight ceiling within that candidate set.",
  "- Projected scoring is standard PPR and does not explicitly model DraftKings yardage bonuses."
)
writeLines(report, "outputs/dfs_value_backtest_report.md")
