source("R/utilities.R")
source("R/models.R")
source("R/backtest.R")
source("R/injury_features.R")
assert_packages()
ensure_directories()
cfg <- read_config()

# A/B test of pregame injury-report features on the two funded game-level
# strategies: the forward-selected linear total model and the random-forest
# margin model.
#
# Both arms train on identical rows. Injury coverage starts in 2009, so the
# whole experiment is restricted to 2009 onward — including the baseline, which
# therefore is not numerically identical to the published 2003-trained backtest.
# The comparison that matters is baseline versus injury inside this script.

features <- readRDS("data/processed/game_features.rds")
team_injuries <- readRDS("data/processed/team_injury_features.rds")

full <- add_game_injury_features(features, team_injuries) |>
  dplyr::filter(.data$season >= 2009)

baseline_features <- setdiff(feature_names(full), game_injury_feature_columns())
injury_features <- feature_names(full)

# Third arm: quarterback availability only. Thirty-six aggregate burden columns
# on top of an 83-feature model may simply add dimensionality, while the part of
# the injury report that actually moves a line is who is playing quarterback.
qb_only_features <- c(
  baseline_features,
  c("home_inj_qb_out", "away_inj_qb_out", "diff_inj_qb_out",
    "home_inj_qb_weight", "away_inj_qb_weight", "diff_inj_qb_weight")
)

arms <- list(
  baseline = baseline_features,
  injury = injury_features,
  qb_only = qb_only_features
)
requested <- commandArgs(trailingOnly = TRUE)
requested <- requested[requested %in% names(arms)]
if (length(requested)) arms <- arms[requested]

cat("Baseline feature count:", length(baseline_features), "\n")
cat("With-injury feature count:", length(injury_features), "\n")
cat("QB-only feature count:", length(qb_only_features), "\n")
cat("Arms this run:", paste(names(arms), collapse = ", "), "\n")
cat("Games:", nrow(full), "\n\n")

test_seasons <- cfg$backtest$start_season:cfg$backtest$end_season
specs <- list(
  list(target = "game_total", model = "forward_linear", line = "total_line"),
  list(target = "home_margin", model = "random_forest", line = "home_line")
)

results <- list()
for (spec in specs) {
  for (test_season in test_seasons) {
    train <- dplyr::filter(full, .data$season < test_season)
    test <- dplyr::filter(full, .data$season == test_season)
    if (!nrow(train) || !nrow(test)) next

    for (arm in names(arms)) {
      use <- arms[[arm]]
      set.seed(cfg$backtest$seed)
      prediction <- fit_predict_model(
        spec$model, train, test, spec$target, cfg, features = use
      )
      results[[length(results) + 1L]] <- tibble::tibble(
        target = spec$target,
        model = spec$model,
        arm = arm,
        season = test_season,
        game_id = test$game_id,
        truth = test[[spec$target]],
        prediction = prediction,
        home_line = test$home_line,
        total_line = test$total_line
      )
      message(spec$target, " ", test_season, " ", arm, " done")
    }
  }
}

predictions <- dplyr::bind_rows(results) |>
  dplyr::mutate(
    market = dplyr::if_else(
      .data$target == "game_total", .data$total_line, -.data$home_line
    ),
    edge = .data$prediction - .data$market,
    absolute_error = abs(.data$prediction - .data$truth),
    market_error = abs(.data$market - .data$truth)
  )
saveRDS(predictions, "data/processed/injury_ab_predictions.rds")

mae <- predictions |>
  dplyr::group_by(.data$target, .data$arm) |>
  dplyr::summarise(
    games = dplyr::n(),
    mae = mean(.data$absolute_error),
    rmse = sqrt(mean((.data$prediction - .data$truth)^2)),
    market_mae = mean(.data$market_error),
    .groups = "drop"
  )
readr::write_csv(mae, "outputs/injury_ab_accuracy.csv")

cat("\n=== Forecast accuracy ===\n")
print(as.data.frame(mae), digits = 5)

# Paired bootstrap on the per-game MAE difference between arms.
paired <- predictions |>
  dplyr::select("target", "arm", "game_id", "absolute_error") |>
  tidyr::pivot_wider(names_from = "arm", values_from = "absolute_error") |>
  dplyr::filter(!is.na(.data$baseline), !is.na(.data$injury)) |>
  dplyr::mutate(delta = .data$injury - .data$baseline)

set.seed(cfg$backtest$seed)
boot <- paired |>
  dplyr::group_by(.data$target) |>
  dplyr::group_modify(function(d, key) {
    draws <- replicate(
      cfg$backtest$bootstrap_iterations,
      mean(sample(d$delta, nrow(d), replace = TRUE))
    )
    tibble::tibble(
      mean_mae_delta = mean(d$delta),
      ci_low = stats::quantile(draws, 0.025),
      ci_high = stats::quantile(draws, 0.975),
      p_injury_better = mean(draws < 0)
    )
  }) |>
  dplyr::ungroup()
readr::write_csv(boot, "outputs/injury_ab_bootstrap.csv")

cat("\n=== Paired MAE difference (negative favours injury features) ===\n")
print(as.data.frame(boot), digits = 4)

# Betting comparison at the frozen 2026 thresholds, so the only thing that
# varies between arms is the feature set.
grade <- function(d, threshold, home_only) {
  d <- dplyr::filter(d, abs(.data$edge) >= threshold)
  if (home_only) {
    d <- dplyr::filter(d, .data$edge > 0)
  }
  if (!nrow(d)) return(tibble::tibble())
  d <- d |>
    dplyr::mutate(
      result = dplyr::case_when(
        .data$target == "game_total" & .data$edge > 0 ~
          signed_result(.data$truth - .data$market),
        .data$target == "game_total" ~
          signed_result(.data$market - .data$truth),
        .data$edge > 0 ~ signed_result(.data$truth - .data$market),
        TRUE ~ signed_result(.data$market - .data$truth)
      ),
      profit = american_profit(.data$result, cfg$backtest$american_odds)
    )
  tibble::tibble(
    bets = nrow(d),
    wins = sum(d$result > 0),
    losses = sum(d$result < 0),
    pushes = sum(d$result == 0),
    win_rate = sum(d$result > 0) / max(1, sum(d$result != 0)),
    profit_units = sum(d$profit),
    roi = sum(d$profit) / nrow(d)
  )
}

betting <- dplyr::bind_rows(
  predictions |>
    dplyr::filter(.data$target == "game_total") |>
    dplyr::group_by(.data$arm) |>
    dplyr::group_modify(~ grade(.x, 5, FALSE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(strategy = "Total, 5-point edge", .before = 1),
  predictions |>
    dplyr::filter(.data$target == "home_margin") |>
    dplyr::group_by(.data$arm) |>
    dplyr::group_modify(~ grade(.x, 6, TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(strategy = "Home spread, 6-point edge", .before = 1)
)
readr::write_csv(betting, "outputs/injury_ab_betting.csv")

cat("\n=== Betting at frozen thresholds, 2018-2025 ===\n")
print(as.data.frame(betting), digits = 4)
