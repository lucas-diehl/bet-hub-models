source("R/utilities.R")
assert_packages()
ensure_directories()

# Follow-up to §9: "beats the salary benchmark in all three seasons" was three
# point estimates with no interval. Before calling this a confident result,
# put a number on how sure that is - paired on the player-game (both model and
# salary predict the same outcome, so pairing is free variance reduction), and
# clustered by week rather than treated as independent player-games, since a
# whole slate's projections share the same game environment inputs.

value_board <- readRDS("data/processed/dfs_value_board_2022_2025.rds")
scored <- value_board |>
  dplyr::filter(is.finite(.data$projected_ppr), is.finite(.data$fantasy_points),
                is.finite(.data$salary_expected_points)) |>
  dplyr::mutate(
    abs_err_model = abs(.data$projected_ppr - .data$fantasy_points),
    abs_err_salary = abs(.data$salary_expected_points - .data$fantasy_points),
    abs_err_baseline = abs(.data$baseline_ppr - .data$fantasy_points),
    gap_vs_salary = .data$abs_err_salary - .data$abs_err_model,
    gap_vs_baseline = .data$abs_err_baseline - .data$abs_err_model
  )

cat("Player-games:", nrow(scored), "seasons",
    paste(sort(unique(scored$season)), collapse = ", "), "\n\n")

week_block_bootstrap <- function(gap, season, week, n = 4000L, seed = 20260904L) {
  key <- paste(season, week)
  blocks <- split(gap, key)
  keys <- names(blocks)
  set.seed(seed)
  vapply(seq_len(n), function(i) {
    picked <- sample(keys, length(keys), replace = TRUE)
    mean(unlist(blocks[picked], use.names = FALSE))
  }, numeric(1))
}

# pct_reduction is the mean gap relative to the benchmark's own mean error -
# how much smaller the model's typical miss is, in relative terms.
report <- function(gap, benchmark_err, season, week, label) {
  draws <- week_block_bootstrap(gap, season, week)
  tibble::tibble(
    comparison = label,
    player_games = length(gap),
    mean_mae_reduction = mean(gap),
    pct_reduction = mean(gap) / mean(benchmark_err),
    ci_lo = stats::quantile(draws, 0.025),
    ci_hi = stats::quantile(draws, 0.975),
    p_model_better = mean(draws > 0)
  )
}

overall_vs_salary <- report(
  scored$gap_vs_salary, scored$abs_err_salary, scored$season, scored$week,
  "Model vs salary-implied, pooled 2023-2025"
)
overall_vs_baseline <- report(
  scored$gap_vs_baseline, scored$abs_err_baseline, scored$season, scored$week,
  "Model vs rolling baseline, pooled 2023-2025"
)

cat("=== Pooled, week-block bootstrap (95% CI on mean MAE reduction) ===\n")
print(as.data.frame(dplyr::bind_rows(overall_vs_salary, overall_vs_baseline)),
      digits = 4, row.names = FALSE)

by_season <- scored |>
  dplyr::group_by(.data$season) |>
  dplyr::group_map(function(d, key) {
    dplyr::bind_cols(
      tibble::tibble(season = key$season[[1]]),
      report(d$gap_vs_salary, d$abs_err_salary, d$season, d$week,
            paste("Model vs salary-implied,", key$season[[1]]))
    )
  }) |>
  dplyr::bind_rows()

cat("\n=== By season, model vs salary-implied ===\n")
print(as.data.frame(by_season), digits = 4, row.names = FALSE)

# Rank correlation gap, same treatment - is the ordering more reliable, not
# just the average error smaller.
rho_by_season <- scored |>
  dplyr::group_by(.data$season) |>
  dplyr::summarise(
    model_rho = stats::cor(.data$projected_ppr, .data$fantasy_points,
                           method = "spearman"),
    salary_rho = stats::cor(.data$salary_expected_points, .data$fantasy_points,
                            method = "spearman"),
    .groups = "drop"
  )
cat("\n=== Rank correlation, model vs salary-implied price ===\n")
print(as.data.frame(rho_by_season), digits = 4, row.names = FALSE)

# Top-decile check: does the model's top value calls actually outperform the
# market's cheapest-relative-to-projection calls, which is closer to how a
# lineup actually gets built than raw MAE.
decile_check <- scored |>
  dplyr::group_by(.data$season) |>
  dplyr::mutate(
    model_value_rank = dplyr::ntile(-.data$projected_ppr / pmax(.data$salary, 1), 10),
    salary_value_rank = dplyr::ntile(
      -(.data$salary_expected_points - .data$salary / 1000) , 10
    )
  ) |>
  dplyr::ungroup()

top_model <- decile_check |> dplyr::filter(.data$model_value_rank == 1)
top_salary <- decile_check |> dplyr::filter(.data$salary_value_rank == 1)
cat(sprintf(
  "\nAll player-games mean actual PPR                : %6.2f (n=%d)\n",
  mean(scored$fantasy_points), nrow(scored)
))
cat(sprintf(
  "Model's top-decile value calls, mean actual PPR  : %6.2f (n=%d)\n",
  mean(top_model$fantasy_points), nrow(top_model)
))
cat(sprintf(
  "Salary's top-decile value calls, mean actual PPR : %6.2f (n=%d)\n",
  mean(top_salary$fantasy_points), nrow(top_salary)
))
cat("This is the practical version of the MAE gap: which top-decile 'cheap for\n")
cat("the projection' call actually scores more, if you built a lineup around it.\n")

readr::write_csv(
  dplyr::bind_rows(overall_vs_salary, overall_vs_baseline, by_season),
  "outputs/dfs_salary_margin_bootstrap.csv"
)
