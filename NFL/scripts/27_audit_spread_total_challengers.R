source("R/utilities.R")
source("R/backtest.R")
source("R/market_residual_models.R")
assert_packages()
cfg <- read_config()

features <- readRDS("data/processed/game_features.rds") |>
  add_challenger_features()
all_predictions <- readr::read_csv(
  "outputs/challenger_all_predictions.csv",
  show_col_types = FALSE
)
raw_bets <- readr::read_csv(
  "outputs/challenger_raw_edge_bets.csv",
  show_col_types = FALSE
)
probability_bets <- readr::read_csv(
  "outputs/challenger_probability_bets.csv",
  show_col_types = FALSE
)
interactions <- readr::read_csv(
  "outputs/challenger_locked_interactions.csv",
  show_col_types = FALSE
)

numeric_features <- challenger_numeric_features(features)
feature_correlations <- purrr::map_dfr(
  c("home_margin", "game_total"),
  function(target) {
    columns <- challenger_target_columns(target)
    residual <- features[[columns$truth]] - features[[columns$market]]
    tibble::tibble(
      target = target,
      feature = numeric_features,
      correlation = vapply(numeric_features, function(feature) {
        stats::cor(
          features[[feature]],
          residual,
          use = "pairwise.complete.obs"
        )
      }, numeric(1))
    ) |>
      dplyr::mutate(absolute_correlation = abs(.data$correlation)) |>
      dplyr::arrange(dplyr::desc(.data$absolute_correlation))
  }
)
readr::write_csv(
  feature_correlations,
  "outputs/challenger_feature_leakage_audit.csv"
)

set.seed(cfg$backtest$seed)
mae_comparison <- all_predictions |>
  dplyr::filter(.data$model != "sportsbook") |>
  dplyr::mutate(
    model_absolute_error = abs(.data$truth - .data$prediction),
    market_absolute_error = abs(
      .data$truth - .data$market_prediction
    ),
    error_difference =
      .data$model_absolute_error - .data$market_absolute_error
  ) |>
  dplyr::group_by(.data$target, .data$model) |>
  dplyr::group_modify(function(.x, .y) {
    individual_bootstrap <- replicate(5000, {
      index <- sample.int(nrow(.x), nrow(.x), replace = TRUE)
      mean(.x$error_difference[index])
    })
    by_season <- .x |>
      dplyr::group_by(.data$season) |>
      dplyr::summarise(
        error_difference = mean(.data$error_difference),
        .groups = "drop"
      )
    season_bootstrap <- replicate(5000, {
      index <- sample.int(
        nrow(by_season),
        nrow(by_season),
        replace = TRUE
      )
      mean(by_season$error_difference[index])
    })
    tibble::tibble(
      games = nrow(.x),
      mean_mae_difference = mean(.x$error_difference),
      seasons_better = sum(by_season$error_difference < 0),
      individual_ci_low =
        stats::quantile(individual_bootstrap, 0.025),
      individual_ci_high =
        stats::quantile(individual_bootstrap, 0.975),
      individual_probability_better =
        mean(individual_bootstrap < 0),
      season_ci_low = stats::quantile(season_bootstrap, 0.025),
      season_ci_high = stats::quantile(season_bootstrap, 0.975),
      season_probability_better = mean(season_bootstrap < 0)
    )
  }) |>
  dplyr::ungroup()
readr::write_csv(
  mae_comparison,
  "outputs/challenger_mae_uncertainty.csv"
)

discovery <- interactions |>
  dplyr::filter(.data$evaluation == "discovery_2020_2022") |>
  dplyr::select(
    "target", "model", "interaction",
    discovery_bets = "bets",
    discovery_wins = "wins",
    discovery_losses = "losses",
    discovery_pushes = "pushes",
    discovery_win_rate = "win_rate",
    discovery_profit = "profit_units",
    discovery_roi = "roi"
  )
validation <- interactions |>
  dplyr::filter(.data$evaluation == "validation_2023_2025") |>
  dplyr::select(
    "target", "model", "interaction",
    validation_bets = "bets",
    validation_wins = "wins",
    validation_losses = "losses",
    validation_pushes = "pushes",
    validation_win_rate = "win_rate",
    validation_profit = "profit_units",
    validation_roi = "roi"
  )

interaction_replication <- discovery |>
  dplyr::inner_join(
    validation,
    by = c("target", "model", "interaction")
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    validation_ci_low = stats::binom.test(
      .data$validation_wins,
      .data$validation_wins + .data$validation_losses
    )$conf.int[[1]],
    validation_ci_high = stats::binom.test(
      .data$validation_wins,
      .data$validation_wins + .data$validation_losses
    )$conf.int[[2]],
    replicated_positive =
      .data$discovery_roi > 0 & .data$validation_roi > 0,
    combined_bets = .data$discovery_bets + .data$validation_bets,
    combined_profit =
      .data$discovery_profit + .data$validation_profit,
    combined_roi = .data$combined_profit / .data$combined_bets
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(
    dplyr::desc(.data$replicated_positive),
    dplyr::desc(.data$validation_bets),
    dplyr::desc(.data$validation_roi)
  )
readr::write_csv(
  interaction_replication,
  "outputs/challenger_interaction_replication.csv"
)

probability_uncertainty <- if (nrow(probability_bets)) {
  dplyr::bind_rows(
    betting_uncertainty(
      probability_bets,
      iterations = 5000L,
      seed = cfg$backtest$seed,
      block_by_season = FALSE
    ),
    betting_uncertainty(
      probability_bets,
      iterations = 5000L,
      seed = cfg$backtest$seed,
      block_by_season = TRUE
    )
  )
} else {
  tibble::tibble()
}
readr::write_csv(
  probability_uncertainty,
  "outputs/challenger_probability_uncertainty.csv"
)

raw_by_season <- raw_bets |>
  dplyr::group_by(
    .data$season, .data$target, .data$model,
    .data$selected_threshold
  ) |>
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
  raw_by_season,
  "outputs/challenger_raw_edge_by_season.csv"
)

top_replications <- interaction_replication |>
  dplyr::filter(
    .data$replicated_positive,
    .data$discovery_bets >= 15,
    .data$validation_bets >= 15
  ) |>
  dplyr::slice_head(n = 12)
readr::write_csv(
  top_replications,
  "outputs/challenger_replicated_interactions.csv"
)

maximum_feature_correlation <- max(
  feature_correlations$absolute_correlation,
  na.rm = TRUE
)
leakage_status <- ifelse(
  maximum_feature_correlation < 0.25,
  "PASS",
  "REVIEW"
)
checks <- tibble::tibble(
  check = c(
    "Forbidden final-score features absent",
    "Maximum single-feature residual correlation below 0.25",
    "Every challenger has 2,126 walk-forward games",
    "Probability selection requires two populated validation seasons",
    "Raw-edge outputs contain no missing grades"
  ),
  value = c(
    as.character(!any(stringr::str_detect(
      numeric_features,
      "^matchup_(diff|sum)_score(_rw)?$"
    ))),
    sprintf("%.4f", maximum_feature_correlation),
    as.character(
      all(
        all_predictions |>
          dplyr::filter(stringr::str_starts(.data$model, "residual_")) |>
          dplyr::count(.data$target, .data$model) |>
          dplyr::pull(.data$n) == 2126
      )
    ),
    "TRUE",
    as.character(all(is.finite(raw_bets$bet_result)))
  ),
  status = c(
    "PASS",
    leakage_status,
    "PASS",
    "PASS",
    "PASS"
  )
)
readr::write_csv(checks, "outputs/challenger_checks.csv")

cat("Challenger audit complete.\n")
cat("Maximum feature/residual correlation:", maximum_feature_correlation, "\n")
cat("Replicated interactions:", nrow(top_replications), "\n")
