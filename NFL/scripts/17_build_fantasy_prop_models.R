source("R/utilities.R")
source("R/odds_api.R")
source("R/touchdown_features.R")
source("R/touchdown_model.R")
source("R/touchdown_backtest.R")
source("R/touchdown_deploy.R")
source("R/fantasy_prop_model.R")
assert_packages()
ensure_directories()

seed <- 20260728L
player_stats <- readRDS("data/raw/player_stats_2021_2025.rds")
historical_context <- readRDS("data/processed/td_game_context.rds")

cat("Building leakage-safe fantasy-stat features...\n")
features <- prepare_fantasy_player_features(
  player_stats,
  historical_context
)
saveRDS(features, "data/processed/fantasy_prop_features.rds")

cat("Running 2023-2025 walk-forward models...\n")
walk_forward <- walk_forward_fantasy_models(
  features,
  test_seasons = 2023:2025,
  seed = seed
)
blend_weights <- fantasy_deployment_blend_weights(
  walk_forward$predictions
)
blended_predictions <- walk_forward$predictions |>
  dplyr::rename(model_prediction = "prediction") |>
  dplyr::left_join(blend_weights, by = "target") |>
  dplyr::mutate(
    prediction =
      .data$model_weight * .data$model_prediction +
      (1 - .data$model_weight) * .data$baseline
  )

blended_metrics <- blended_predictions |>
  dplyr::group_by(.data$target, .data$season) |>
  dplyr::group_modify(~ fantasy_regression_metrics(.x)) |>
  dplyr::ungroup()
overall_metrics <- blended_predictions |>
  dplyr::group_by(.data$target) |>
  dplyr::group_modify(~ fantasy_regression_metrics(.x)) |>
  dplyr::ungroup()

component_wide <- blended_predictions |>
  dplyr::select(
    "game_id", "season", "week", "player_id", "position",
    "target", "actual", "prediction", "baseline"
  ) |>
  tidyr::pivot_wider(
    names_from = "target",
    values_from = c("actual", "prediction", "baseline"),
    values_fill = 0
  )
component_names <- names(fantasy_target_specifications())
required_components <- unlist(lapply(
  c("actual", "prediction", "baseline"),
  function(prefix) paste0(prefix, "_", component_names)
))
for (column in setdiff(required_components, names(component_wide))) {
  component_wide[[column]] <- 0
}
ppr_from_components <- function(data, prefix) {
  get_component <- function(target) data[[paste0(prefix, "_", target)]]
  0.04 * get_component("passing_yards") +
    4 * get_component("passing_tds") -
    2 * get_component("interceptions") +
    0.1 * get_component("rushing_yards") +
    6 * get_component("rushing_tds") +
    get_component("receptions") +
    0.1 * get_component("receiving_yards") +
    6 * get_component("receiving_tds") -
    2 * get_component("fumbles_lost")
}
ppr_actual <- features |>
  dplyr::select(
    "game_id", "player_id", "ppr_points_actual"
  ) |>
  dplyr::distinct()
ppr_predictions <- component_wide |>
  dplyr::left_join(ppr_actual, by = c("game_id", "player_id")) |>
  dplyr::mutate(
    actual = .data$ppr_points_actual,
    prediction = ppr_from_components(component_wide, "prediction"),
    baseline = ppr_from_components(component_wide, "baseline")
  ) |>
  dplyr::filter(is.finite(.data$actual))
ppr_metrics <- ppr_predictions |>
  dplyr::group_by(.data$season) |>
  dplyr::group_modify(~ fantasy_regression_metrics(.x)) |>
  dplyr::ungroup() |>
  dplyr::mutate(target = "ppr_points", .before = 1)
ppr_residual_intervals <- ppr_predictions |>
  dplyr::mutate(residual = .data$actual - .data$prediction) |>
  dplyr::group_by(.data$position) |>
  dplyr::summarise(
    ppr_residual_p10 = stats::quantile(.data$residual, 0.10, na.rm = TRUE),
    ppr_residual_p90 = stats::quantile(.data$residual, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

cat("Fitting deployment models on all 2021-2025 games...\n")
deployment <- fit_fantasy_deployment_models(features, seed)
saveRDS(
  list(models = deployment, blend_weights = blend_weights),
  "data/processed/fantasy_prop_deployment_models.rds"
)
saveRDS(
  list(
    predictions = blended_predictions,
    metrics = blended_metrics,
    raw_metrics = walk_forward$metrics,
    blend_weights = blend_weights
  ),
  "data/processed/fantasy_prop_walk_forward.rds"
)

specifications <- fantasy_target_specifications()
manifest <- purrr::imap_dfr(deployment, function(model, target_name) {
  training_rows <- nrow(fantasy_target_candidates(
    features,
    model$family
  ) |>
    dplyr::filter(.data$season <= 2025, .data$prior_games >= 1))
  tibble::tibble(
    target = target_name,
    label = model$label,
    outcome = model$outcome,
    family = model$family,
    objective = specifications[[target_name]]$objective,
    training_rows = training_rows,
    features = length(model$features)
  )
}) |>
  dplyr::left_join(blend_weights, by = "target")

importance <- purrr::imap_dfr(deployment, function(model, target_name) {
  xgboost::xgb.importance(
    feature_names = model$features,
    model = model$fit
  ) |>
    dplyr::slice_head(n = 25L) |>
    dplyr::mutate(target = target_name, .before = 1)
})

cat("Building preliminary 2026 Week 1 projection board...\n")
schedules <- readRDS("data/raw/schedules_2026.rds")
rosters <- readRDS("data/raw/rosters_2026.rds")
event_map <- match_td_events_to_schedule(
  readRDS("data/raw/td_2026_current_events.rds"),
  schedules
)
week_one_ids <- schedules |>
  dplyr::filter(
    .data$season == 2026,
    .data$game_type == "REG",
    .data$week == 1
  ) |>
  dplyr::pull(.data$game_id)
week_one_events <- event_map |>
  dplyr::filter(.data$game_id %in% week_one_ids)
game_lines <- flatten_current_td_game_lines(
  readRDS("data/raw/td_2026_current_game_lines.rds")
)
game_overrides <- readr::read_csv(
  "config/td_2026_game_overrides.csv",
  show_col_types = FALSE
)
week_one_context <- build_td_2026_game_context(
  schedules,
  week_one_events,
  game_lines,
  game_overrides
)
future_features <- build_fantasy_2026_features(
  player_stats,
  rosters,
  historical_context,
  week_one_context
)
long_projection <- predict_fantasy_deployment(
  deployment,
  future_features,
  blend_weights
)
intervals <- fantasy_residual_intervals(blended_predictions)
projection_board <- build_fantasy_projection_board(
  long_projection,
  intervals,
  features
) |>
  dplyr::left_join(ppr_residual_intervals, by = "position") |>
  dplyr::mutate(
    ppr_low = pmax(0, .data$projected_ppr + .data$ppr_residual_p10),
    ppr_high = pmax(0, .data$projected_ppr + .data$ppr_residual_p90)
  ) |>
  dplyr::filter(.data$projected_ppr >= 0.5) |>
  dplyr::mutate(
    overall_rank = dplyr::row_number(),
    .before = 1
  ) |>
  dplyr::group_by(.data$position) |>
  dplyr::mutate(position_rank = dplyr::row_number(), .after = "position") |>
  dplyr::ungroup()

readr::write_csv(
  projection_board,
  "outputs/fantasy_prop_2026_week1_projections.csv"
)
readr::write_csv(
  long_projection,
  "outputs/fantasy_prop_2026_week1_long.csv"
)
readr::write_csv(
  blended_metrics,
  "outputs/fantasy_prop_metrics_by_season.csv"
)
readr::write_csv(
  overall_metrics,
  "outputs/fantasy_prop_metrics_overall.csv"
)
readr::write_csv(
  ppr_metrics,
  "outputs/fantasy_ppr_metrics_by_season.csv"
)
readr::write_csv(
  manifest,
  "outputs/fantasy_prop_model_manifest.csv"
)
readr::write_csv(
  importance,
  "outputs/fantasy_prop_feature_importance.csv"
)
readr::write_csv(
  intervals,
  "outputs/fantasy_prop_residual_intervals.csv"
)

cat("Fantasy feature rows:", nrow(features), "\n")
cat("Walk-forward predictions:", nrow(blended_predictions), "\n")
cat("Week 1 projected players:", nrow(projection_board), "\n")
print(overall_metrics)
