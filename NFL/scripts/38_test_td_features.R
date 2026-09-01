source("R/utilities.R")
source("R/injury_features.R")
source("R/fantasy_prop_model.R")
source("R/player_role_features.R")
source("R/touchdown_features.R")
source("R/touchdown_model.R")
assert_packages()
ensure_directories()

# A/B test of the same player-level additions on the anytime-touchdown model.
#
# This is the most plausible place for injury data to pay: whether a goal-line
# back is active, and whether the starting quarterback is out, bear directly on
# who scores. The evaluation mirrors the production walk-forward exactly, so the
# only thing that changes between arms is the feature set.

player_features <- readRDS("data/processed/td_player_features.rds")
injuries <- readRDS("data/raw/injuries_2009_2025.rds")
inputs <- readRDS("data/processed/player_role_inputs.rds")

augmented <- augment_player_features(
  player_features, injuries, inputs$ff, inputs$snaps, inputs$crosswalk
)

baseline_cols <- td_model_feature_names(player_features)
injury_cols <- player_injury_feature_columns()
role_cols <- grep("^(opp_|snap_pct_r)", names(augmented), value = TRUE)

cat("Baseline features:", length(baseline_cols), "\n")
cat("Injury features:", length(injury_cols), "\n")
cat("Role features:", length(role_cols), "\n")
cat("Rows:", nrow(augmented), "\n")
cat("Rows with own injury designation:",
    round(mean(augmented$inj_self_weight > 0), 4), "\n")
cat("Rows with team QB out:",
    round(mean(augmented$inj_team_qb_out > 0), 4), "\n\n")

arms <- list(
  baseline = baseline_cols,
  injury = c(baseline_cols, injury_cols),
  full = c(baseline_cols, injury_cols, role_cols)
)

test_seasons <- 2023:2025
results <- list()

for (arm_name in names(arms)) {
  use <- intersect(arms[[arm_name]], names(augmented))
  for (test_season in test_seasons) {
    train <- dplyr::filter(
      augmented, .data$season < test_season, .data$prior_games >= 1
    )
    test <- dplyr::filter(
      augmented, .data$season == test_season, .data$prior_games >= 1
    )
    if (!nrow(train) || !nrow(test)) next

    matrices <- td_matrix_pair(train, test, use)
    set.seed(20260727L)
    fit <- xgboost::xgb.train(
      params = list(
        objective = "binary:logistic", eval_metric = "logloss",
        learning_rate = 0.035, max_depth = 3, min_child_weight = 12,
        subsample = 0.8, colsample_bytree = 0.8,
        reg_lambda = 2, reg_alpha = 0.1, nthread = 1, verbosity = 0
      ),
      data = xgboost::xgb.DMatrix(
        matrices$train, label = train$anytime_td
      ),
      nrounds = 250
    )
    probability <- clip_probability(
      as.numeric(stats::predict(fit, matrices$test))
    )
    results[[length(results) + 1L]] <- tibble::tibble(
      arm = arm_name, season = test_season,
      game_id = test$game_id, player_id = test$player_id,
      anytime_td = test$anytime_td, probability = probability
    )
    message(arm_name, " ", test_season)
  }
}

predictions <- dplyr::bind_rows(results)
saveRDS(predictions, "data/processed/td_feature_ab_predictions.rds")

metrics <- predictions |>
  dplyr::group_by(.data$season, .data$arm) |>
  dplyr::summarise(
    player_games = dplyr::n(),
    actual_rate = mean(.data$anytime_td),
    predicted_rate = mean(.data$probability),
    brier = mean((.data$probability - .data$anytime_td)^2),
    log_loss = -mean(
      .data$anytime_td * log(.data$probability) +
        (1 - .data$anytime_td) * log(1 - .data$probability)
    ),
    .groups = "drop"
  )
readr::write_csv(metrics, "outputs/td_feature_ab_metrics.csv")

cat("=== Touchdown probability quality by arm ===\n")
print(as.data.frame(metrics), digits = 5)

overall <- predictions |>
  dplyr::group_by(.data$arm) |>
  dplyr::summarise(
    player_games = dplyr::n(),
    brier = mean((.data$probability - .data$anytime_td)^2),
    log_loss = -mean(
      .data$anytime_td * log(.data$probability) +
        (1 - .data$anytime_td) * log(1 - .data$probability)
    ),
    .groups = "drop"
  )
cat("\n=== Pooled 2023-2025 ===\n")
print(as.data.frame(overall), digits = 6)

paired <- predictions |>
  dplyr::mutate(loss = (.data$probability - .data$anytime_td)^2) |>
  dplyr::select("arm", "game_id", "player_id", "season", "loss") |>
  tidyr::pivot_wider(names_from = "arm", values_from = "loss")

set.seed(20260727L)
draw <- function(delta) {
  s <- replicate(1000, mean(sample(delta, length(delta), replace = TRUE)))
  tibble::tibble(
    delta = mean(delta),
    ci_low = stats::quantile(s, 0.025),
    ci_high = stats::quantile(s, 0.975),
    p_better = mean(s < 0)
  )
}
boot <- dplyr::bind_rows(
  dplyr::bind_cols(tibble::tibble(arm = "injury"),
                   draw(paired$injury - paired$baseline)),
  dplyr::bind_cols(tibble::tibble(arm = "full"),
                   draw(paired$full - paired$baseline))
)
readr::write_csv(boot, "outputs/td_feature_ab_bootstrap.csv")

cat("\n=== Paired Brier delta vs baseline (negative favours addition) ===\n")
print(as.data.frame(boot), digits = 5)
