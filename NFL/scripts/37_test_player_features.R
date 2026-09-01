source("R/utilities.R")
source("R/injury_features.R")
source("R/fantasy_prop_model.R")
source("R/player_role_features.R")
assert_packages()
ensure_directories()

# A/B test of the new player-level inputs on the fantasy component models.
#
# Three arms, so the two kinds of input can be told apart:
#   baseline    - the current feature set
#   injury      - plus pregame injury designations (new information)
#   full        - plus lagged opportunity and snap-share role measures
#
# Walk-forward seasons are unchanged (2023-2025), so any difference is
# attributable to the features rather than to the evaluation.

features <- readRDS("data/processed/fantasy_prop_features.rds")
injuries <- readRDS("data/raw/injuries_2009_2025.rds")

cache <- "data/processed/player_role_inputs.rds"
if (file.exists(cache)) {
  inputs <- readRDS(cache)
} else {
  seasons <- sort(unique(features$season))
  message("Downloading opportunity and snap data for ",
          paste(range(seasons), collapse = "-"))
  inputs <- list(
    ff = nflreadr::load_ff_opportunity(seasons),
    snaps = nflreadr::load_snap_counts(seasons),
    crosswalk = nflreadr::load_ff_playerids() |>
      dplyr::filter(!is.na(.data$gsis_id), !is.na(.data$pfr_id)) |>
      dplyr::select("gsis_id", "pfr_id") |>
      dplyr::distinct(.data$pfr_id, .keep_all = TRUE)
  )
  saveRDS(inputs, cache)
}

augmented <- augment_player_features(
  features, injuries, inputs$ff, inputs$snaps, inputs$crosswalk
)
saveRDS(augmented, "data/processed/fantasy_prop_features_augmented.rds")

baseline_cols <- fantasy_model_feature_names(features)
injury_cols <- player_injury_feature_columns()
role_cols <- grep("^(opp_|snap_pct_r)", names(augmented), value = TRUE)

cat("Baseline features:", length(baseline_cols), "\n")
cat("Injury features added:", length(injury_cols), "\n")
cat("Role/opportunity features added:", length(role_cols), "\n")

coverage <- augmented |>
  dplyr::summarise(
    rows = dplyr::n(),
    injury_nonzero = mean(.data$inj_self_weight > 0),
    qb_out_rate = mean(.data$inj_team_qb_out > 0),
    opp_coverage = mean(!is.na(.data$opp_receptions_exp_r3)),
    snap_coverage = mean(!is.na(.data$snap_pct_r3))
  )
cat("\nCoverage:\n"); print(as.data.frame(coverage), digits = 4)

arms <- list(
  baseline = baseline_cols,
  injury = c(baseline_cols, injury_cols),
  full = c(baseline_cols, injury_cols, role_cols)
)

specifications <- fantasy_target_specifications()
test_seasons <- 2023:2025
results <- list()

for (arm_name in names(arms)) {
  use <- intersect(arms[[arm_name]], names(augmented))
  for (target_name in names(specifications)) {
    specification <- specifications[[target_name]]
    pool <- fantasy_target_candidates(augmented, specification$family)
    for (test_season in test_seasons) {
      train <- dplyr::filter(
        pool, .data$season < test_season, .data$prior_games >= 1
      )
      test <- dplyr::filter(
        pool, .data$season == test_season, .data$prior_games >= 1
      )
      if (!nrow(train) || !nrow(test)) next

      matrices <- fantasy_matrix_pair(train, test, use)
      params <- list(
        objective = specification$objective,
        eval_metric = if (specification$objective == "count:poisson") {
          "poisson-nloglik"
        } else "rmse",
        learning_rate = 0.035, max_depth = 3, min_child_weight = 15,
        subsample = 0.8, colsample_bytree = 0.8,
        reg_lambda = 3, reg_alpha = 0.15, nthread = 1, verbosity = 0
      )
      if (specification$objective == "count:poisson") {
        params$max_delta_step <- 0.7
      }
      set.seed(20260728L + match(target_name, names(specifications)))
      fit <- xgboost::xgb.train(
        params = params,
        data = xgboost::xgb.DMatrix(
          matrices$train, label = as.numeric(train[[specification$outcome]])
        ),
        nrounds = 180
      )
      prediction <- pmax(0, as.numeric(stats::predict(fit, matrices$test)))

      results[[length(results) + 1L]] <- tibble::tibble(
        arm = arm_name, target = target_name, season = test_season,
        game_id = test$game_id, player_id = test$player_id,
        actual = as.numeric(test[[specification$outcome]]),
        prediction = prediction
      )
      message(arm_name, " ", target_name, " ", test_season)
    }
  }
}

predictions <- dplyr::bind_rows(results)
saveRDS(predictions, "data/processed/player_feature_ab_predictions.rds")

accuracy <- predictions |>
  dplyr::group_by(.data$target, .data$arm) |>
  dplyr::summarise(
    observations = dplyr::n(),
    mae = mean(abs(.data$prediction - .data$actual)),
    rmse = sqrt(mean((.data$prediction - .data$actual)^2)),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = "arm", values_from = c("mae", "rmse"),
    id_cols = c("target", "observations")
  ) |>
  dplyr::mutate(
    injury_gain = .data$mae_baseline - .data$mae_injury,
    full_gain = .data$mae_baseline - .data$mae_full
  )
readr::write_csv(accuracy, "outputs/player_feature_ab_accuracy.csv")

cat("\n=== Component MAE by arm (positive gain favours the addition) ===\n")
print(as.data.frame(
  accuracy |>
    dplyr::select("target", "observations", "mae_baseline", "mae_injury",
                  "mae_full", "injury_gain", "full_gain")
), digits = 5)

# Paired bootstrap per target on the per-row absolute-error difference.
paired <- predictions |>
  dplyr::mutate(absolute_error = abs(.data$prediction - .data$actual)) |>
  dplyr::select("target", "arm", "game_id", "player_id", "absolute_error") |>
  tidyr::pivot_wider(names_from = "arm", values_from = "absolute_error")

set.seed(20260728L)
boot <- paired |>
  dplyr::group_by(.data$target) |>
  dplyr::group_modify(function(d, key) {
    draw <- function(delta) {
      s <- replicate(500, mean(sample(delta, length(delta), replace = TRUE)))
      c(mean(delta), stats::quantile(s, 0.025), stats::quantile(s, 0.975),
        mean(s < 0))
    }
    inj <- draw(d$injury - d$baseline)
    fll <- draw(d$full - d$baseline)
    tibble::tibble(
      injury_delta = inj[[1]], injury_ci_low = inj[[2]],
      injury_ci_high = inj[[3]], p_injury_better = inj[[4]],
      full_delta = fll[[1]], full_ci_low = fll[[2]],
      full_ci_high = fll[[3]], p_full_better = fll[[4]]
    )
  }) |>
  dplyr::ungroup()
readr::write_csv(boot, "outputs/player_feature_ab_bootstrap.csv")

cat("\n=== Paired MAE deltas vs baseline (negative favours addition) ===\n")
print(as.data.frame(
  boot |> dplyr::select("target", "injury_delta", "p_injury_better",
                        "full_delta", "p_full_better")
), digits = 4)
