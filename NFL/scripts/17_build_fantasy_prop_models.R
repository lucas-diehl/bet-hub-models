source("R/utilities.R")
source("R/odds_api.R")
source("R/touchdown_features.R")
source("R/touchdown_model.R")
source("R/touchdown_backtest.R")
source("R/touchdown_deploy.R")
source("R/fantasy_prop_model.R")
source("R/dfs_value_backtest.R")    # dfs_player_name_key(), fuzzy_match_salary_index()
source("R/fantasy_salary_stack.R")
source("R/inactive_probability.R")
source("R/fantasy_variance.R")
assert_packages()
ensure_directories()

# Weekly per-player fantasy point projections.
#
# Two separable pieces of work live here, deliberately decoupled so this can
# run every week without paying for a full retrain each time:
#   1. TRAINING - walk-forward validate + fit the deployment models on
#      2021-2025 history. That history never changes week to week, so this
#      only needs to happen once (or when deliberately refreshed via
#      --retrain); the fitted models are cached to data/processed/.
#   2. PROJECTION - build this week's feature rows (current roster, this
#      week's matchups, and each player's actual 2026-season-to-date
#      role/opportunity) and score them with the cached models. This is the
#      part that must run fresh every week.
#
# Originally this script was hardcoded to "Week 1" (a one-time preseason
# board) and was never wired into any weekly job - the file went stale the
# moment Week 1 kicked off. --week/--execute below, plus reading the
# 2021-2026 stats file (which script 15 refreshes with --refresh-stats) so
# recent-opportunity features actually reflect the season in progress, are
# what make this safe to run on a real weekly cadence.
#
#   --week=3       which regular-season week to project (default: next
#                   unplayed, same rule scripts/57 and 78 use)
#   --retrain       rebuild the walk-forward + deployment models from scratch
#                   (slow, ~10-20 min) instead of reusing the cache. Historical
#                   2021-2025 data doesn't change, so this is normally only
#                   needed after a code change to the model itself.
#   --execute       write the projection files (otherwise dry run, preview only)

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", flag, "="), "", hit[[1]])
}
execute <- "--execute" %in% args
retrain <- "--retrain" %in% args
seed <- 20260728L

deployment_path <- "data/processed/fantasy_prop_deployment_models.rds"
walk_forward_path <- "data/processed/fantasy_prop_walk_forward.rds"
salary_stack_path <- "data/processed/fantasy_salary_stack.rds"
have_cache <- file.exists(deployment_path) && file.exists(walk_forward_path)
if (!have_cache && !retrain) {
  cat("No cached deployment models found; training from scratch regardless of --retrain.\n")
  retrain <- TRUE
}

# Prefer the 2021-2026 file (script 15's --refresh-stats keeps this current
# with each week's box scores) so recent-opportunity features for week 2+
# reflect actual in-season usage rather than falling back to the preseason
# draft-priority proxy for every player, every week.
stats_2026_path <- "data/raw/player_stats_2021_2026.rds"
stats_hist_path <- "data/raw/player_stats_2021_2025.rds"
stats_path <- if (file.exists(stats_2026_path)) stats_2026_path else stats_hist_path
cat("Using player stats:", stats_path, "\n")
player_stats <- readRDS(stats_path)
historical_context <- readRDS("data/processed/td_game_context.rds")

# Needed every run, not just on --retrain: build_fantasy_projection_board()
# below uses this for its 2023-2025 two-point-conversion rate-by-position
# adjustment, which a single week's projection rows can't supply on their own.
cat("Building leakage-safe fantasy-stat features...\n")
features <- prepare_fantasy_player_features(
  player_stats,
  historical_context
)

if (retrain) {
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
    deployment_path
  )
  saveRDS(
    list(
      predictions = blended_predictions,
      metrics = blended_metrics,
      raw_metrics = walk_forward$metrics,
      blend_weights = blend_weights
    ),
    walk_forward_path
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

  readr::write_csv(blended_metrics, "outputs/fantasy_prop_metrics_by_season.csv")
  readr::write_csv(overall_metrics, "outputs/fantasy_prop_metrics_overall.csv")
  readr::write_csv(ppr_metrics, "outputs/fantasy_ppr_metrics_by_season.csv")
  readr::write_csv(manifest, "outputs/fantasy_prop_model_manifest.csv")
  readr::write_csv(importance, "outputs/fantasy_prop_feature_importance.csv")

  ## Salary stack, fit on the walk-forward predictions just produced. Worth
  ## +0.0144 R2 on total PPR over the model alone (scripts/80). Non-fatal: a
  ## missing or unmatched salary file should cost us the stack, not the whole
  ## retrain, and apply_fantasy_salary_stack() degrades to model-only.
  cat("Fitting salary stack...\n")
  salary_stack <- tryCatch({
    frame <- fantasy_salary_training_frame()
    frame <- frame[frame$match_method != "UNMATCHED", , drop = FALSE]
    fit_fantasy_salary_stack(frame)
  }, error = function(e) {
    cat("  Salary stack fit failed:", conditionMessage(e), "\n")
    cat("  Projections will be model-only until this is fixed.\n")
    NULL
  })
  ## Variance model, refit here rather than only in scripts/84. It is estimated
  ## FROM these walk-forward residuals, so a retrain that left it cached would
  ## pair new point models with spreads calibrated to the old ones - stale in a
  ## way nothing downstream could detect. scripts/84 remains the place to see
  ## the coverage/sharpness validation; this just keeps the artifact honest.
  cat("Fitting variance model...\n")
  variance_fit <- tryCatch({
    vfr <- fantasy_salary_training_frame(verbose = FALSE) |>
      dplyr::filter(is.finite(.data$ppr_actual), is.finite(.data$ppr_model)) |>
      dplyr::left_join(
        features |>
          dplyr::select(dplyr::any_of(c("game_id", "player_id",
                                        "fantasy_points_ppr_sd5", "opportunity_r5"))) |>
          dplyr::distinct(.data$game_id, .data$player_id, .keep_all = TRUE),
        by = c("game_id", "player_id")
      )
    fit_fantasy_variance(vfr)
  }, error = function(e) {
    cat("  Variance fit failed:", conditionMessage(e),
        "- keeping position-constant bands.\n")
    NULL
  })
  if (!is.null(variance_fit)) {
    saveRDS(variance_fit, "data/processed/fantasy_variance_model.rds")
    cat("  Variance model refit on", variance_fit$n, "rows.\n")
  }

  if (!is.null(salary_stack)) {
    saveRDS(salary_stack, salary_stack_path)
    stack_summary <- purrr::map_dfr(salary_stack$fits, function(f) {
      tibble::tibble(
        position = f$position, n = f$n,
        w_model = round(unname(f$stack_coef[["ppr_model"]]), 4),
        w_salary = round(unname(f$stack_coef[["ppr_salary"]]), 4),
        degenerate = f$degenerate,
        in_sample_r2_model = round(f$in_sample_r2_model, 4),
        in_sample_r2_stack = round(f$in_sample_r2_stack, 4)
      )
    })
    print(as.data.frame(stack_summary))
    readr::write_csv(stack_summary, "outputs/fantasy_salary_stack_weights.csv")
  }
} else {
  cat("Reusing cached deployment models (", deployment_path, "). Pass --retrain to rebuild.\n", sep = "")
  cached <- readRDS(deployment_path)
  deployment <- cached$models
  blend_weights <- cached$blend_weights
  walk_forward_cache <- readRDS(walk_forward_path)
  blended_predictions <- walk_forward_cache$predictions
  # Recompute the same PPR residual intervals the training branch produces,
  # from the cached walk-forward predictions, so a --execute-only run doesn't
  # need to re-run training just to get interval bounds.
  ppr_residual_intervals <- blended_predictions |>
    dplyr::select("game_id", "season", "week", "player_id", "position",
                  "target", "actual", "prediction") |>
    tidyr::pivot_wider(
      names_from = "target", values_from = c("actual", "prediction"),
      values_fill = 0
    )
  get_col <- function(data, prefix, target) {
    col <- paste0(prefix, "_", target)
    if (col %in% names(data)) data[[col]] else 0
  }
  ppr_of <- function(data, prefix) {
    0.04 * get_col(data, prefix, "passing_yards") +
      4 * get_col(data, prefix, "passing_tds") -
      2 * get_col(data, prefix, "interceptions") +
      0.1 * get_col(data, prefix, "rushing_yards") +
      6 * get_col(data, prefix, "rushing_tds") +
      get_col(data, prefix, "receptions") +
      0.1 * get_col(data, prefix, "receiving_yards") +
      6 * get_col(data, prefix, "receiving_tds") -
      2 * get_col(data, prefix, "fumbles_lost")
  }
  ppr_residual_intervals <- ppr_residual_intervals |>
    dplyr::mutate(
      actual = ppr_of(ppr_residual_intervals, "actual"),
      prediction = ppr_of(ppr_residual_intervals, "prediction"),
      residual = .data$actual - .data$prediction
    ) |>
    dplyr::group_by(.data$position) |>
    dplyr::summarise(
      ppr_residual_p10 = stats::quantile(.data$residual, 0.10, na.rm = TRUE),
      ppr_residual_p90 = stats::quantile(.data$residual, 0.90, na.rm = TRUE),
      .groups = "drop"
    )
}

cat("Building", if (execute) "and writing" else "(dry run)", "the weekly projection board...\n")
schedules <- readRDS("data/raw/schedules_2026.rds")
rosters <- readRDS("data/raw/rosters_2026.rds")
event_map <- match_td_events_to_schedule(
  readRDS("data/raw/td_2026_current_events.rds"),
  schedules
)

target_week <- arg_value("--week")
if (is.null(target_week)) {
  upcoming <- schedules |>
    dplyr::filter(.data$game_type == "REG", as.Date(.data$gameday) >= Sys.Date()) |>
    dplyr::arrange(.data$gameday)
  if (!nrow(upcoming)) stop("No upcoming games on the schedule.", call. = FALSE)
  target_week <- upcoming$week[[1]]
}
target_week <- as.integer(target_week)
cat("Projecting week:", target_week, "\n")

target_week_ids <- schedules |>
  dplyr::filter(
    .data$season == 2026,
    .data$game_type == "REG",
    .data$week == target_week
  ) |>
  dplyr::pull(.data$game_id)
if (!length(target_week_ids)) {
  stop("No week ", target_week, " games on the 2026 schedule.", call. = FALSE)
}
target_week_events <- event_map |>
  dplyr::filter(.data$game_id %in% target_week_ids)
game_lines <- flatten_current_td_game_lines(
  readRDS("data/raw/td_2026_current_game_lines.rds")
)
game_overrides <- readr::read_csv(
  "config/td_2026_game_overrides.csv",
  show_col_types = FALSE
)
target_week_context <- build_td_2026_game_context(
  schedules,
  target_week_events,
  game_lines,
  game_overrides
)
future_features <- build_fantasy_2026_features(
  player_stats,
  rosters,
  historical_context,
  target_week_context,
  schedules = schedules
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
  dplyr::filter(.data$projected_ppr >= 0.5)

## Salary stack. Applied before ranking so the board's order reflects the
## number we actually believe. Rows with no DK salary (off the main slate, or
## the weekly capture did not run) keep the model-only value and are flagged
## via salary_stack_applied, since a silently model-only board looks identical
## to a working one.
salary_stack <- if (file.exists(salary_stack_path)) {
  readRDS(salary_stack_path)
} else {
  NULL
}
projection_board <- projection_board |>
  dplyr::mutate(
    salary_join_key = paste(
      normalize_team(.data$team), .data$position,
      dfs_player_name_key(.data$player), sep = "|"
    )
  )
projection_board <- apply_fantasy_salary_stack(
  projection_board,
  salary_stack,
  fantasy_salary_lookup(2026L, target_week)
)

## Player-specific spread, replacing the position-constant ppr_low/ppr_high set
## above. Those bands added one fixed width per position regardless of how large
## the projection was, which left stud projections covering only 67% of their
## actual outcomes - understating exactly the ceiling GPP lineups are built on.
## scripts/84 fits and validates; see R/fantasy_variance.R for the method.
## Non-fatal: a missing model leaves the existing bands untouched.
variance_model <- if (file.exists("data/processed/fantasy_variance_model.rds")) {
  readRDS("data/processed/fantasy_variance_model.rds")
} else {
  NULL
}
if (!is.null(variance_model)) {
  ## The scale model wants the same trailing-volatility/opportunity features it
  ## was fit on; attach them from the feature frame by (game_id, player_id).
  vfeats <- features |>
    dplyr::select(dplyr::any_of(c("game_id", "player_id",
                                  "fantasy_points_ppr_sd5", "opportunity_r5"))) |>
    dplyr::distinct(.data$game_id, .data$player_id, .keep_all = TRUE)
  projection_board <- projection_board |>
    dplyr::left_join(vfeats, by = c("game_id", "player_id"))
}
projection_board <- apply_fantasy_variance(projection_board, variance_model)

## Inactive risk. DFS ENGINE hardcodes p_zero = 0.03 for every player it takes
## from our file, so a Questionable back and an ironman starter price the same.
## This supplies a real graded number per player (scripts/83 for the rates).
projection_board <- add_inactive_probability(projection_board, 2026L, target_week)

projection_board <- projection_board |>
  dplyr::arrange(dplyr::desc(.data$projected_ppr)) |>
  dplyr::mutate(
    overall_rank = dplyr::row_number(),
    .before = 1
  ) |>
  dplyr::group_by(.data$position) |>
  dplyr::mutate(position_rank = dplyr::row_number(), .after = "position") |>
  dplyr::ungroup()

cat("Week", target_week, "projected players:", nrow(projection_board), "\n")
print(utils::head(
  dplyr::select(projection_board, "overall_rank", "player", "position", "team", "projected_ppr"),
  15
))

if (!execute) {
  cat("\nDry run. Add --execute to write projection files.\n")
  quit(save = "no", status = 0)
}

week_tag <- sprintf("2026_week%d", target_week)
readr::write_csv(projection_board, sprintf("outputs/fantasy_prop_%s_projections.csv", week_tag))
readr::write_csv(projection_board, "outputs/fantasy_prop_2026_latest_projections.csv")
readr::write_csv(long_projection, sprintf("outputs/fantasy_prop_%s_long.csv", week_tag))
readr::write_csv(long_projection, "outputs/fantasy_prop_2026_latest_long.csv")
readr::write_csv(intervals, "outputs/fantasy_prop_residual_intervals.csv")

cat("Wrote outputs/fantasy_prop_", week_tag, "_projections.csv (+ _latest copy)\n", sep = "")
