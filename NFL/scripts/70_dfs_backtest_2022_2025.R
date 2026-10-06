source("R/utilities.R")
source("R/odds.R")
source("R/touchdown_features.R")
source("R/fantasy_prop_model.R")
source("R/dfs_salaries.R")
source("R/dfs_value_backtest.R")
assert_packages()
ensure_directories()

# P3 section 9.
#
# The DFS backtest covers 2017-2021, because that is where free RotoGuru salary
# history ends. Four seasons of DraftKings salaries have since been recovered
# from the draftgroups API (scripts/19, 48, 50; 99.70% exact agreement with
# RotoGuru on the overlapping 2021 season), so the honest test is available and
# has simply not been run.
#
# It matters more than a routine extension. 2017-2021 is the era the model was
# built in; 2022-2025 is out-of-era as well as out-of-sample, and it covers the
# period the model would actually have been deployed into. A lift that survives
# only in the seasons the features were designed around is a different claim
# from one that survives afterwards.
#
# Three things, per the review:
#   1. The full value backtest and lineup optimisation, 2022-2025.
#   2. Rare-count components re-scored under Poisson deviance and log-loss.
#      Touchdowns and receptions are counts; squared error rewards a model for
#      predicting the mean of a heavily zero-inflated variable and says almost
#      nothing about whether the rare event is being anticipated.
#   3. A salary-implied benchmark. DraftKings prices players on expected points,
#      so salary alone is a forecast. Beating a rolling average is a weak bar;
#      beating the price is the bar that matters.

seed <- 20260728L
test_seasons <- 2022:2025
history_seasons <- 2014:2025

# --------------------------------------------------------------------------
# Inputs, stitched across the file boundaries
# --------------------------------------------------------------------------

player_stats <- dplyr::bind_rows(
  readRDS("data/raw/player_stats_2014_2021.rds"),
  readRDS("data/raw/player_stats_2021_2025.rds")
) |>
  dplyr::distinct(.data$player_id, .data$season, .data$week, .keep_all = TRUE)

schedules <- readRDS("data/raw/schedules.rds")
rotowire <- read_rotowire("data/raw/rotowire_games_archive.json")

salaries <- dplyr::bind_rows(
  readRDS("data/processed/dfs_salaries.rds"),
  readRDS("data/processed/dfs_salaries_dk_2022_plus.rds")
) |>
  deduplicate_dfs_salaries()

# The recovered 2022+ salary rows come from DraftKings' draftgroups API, which
# lists a slate's salaries and nothing else - there is no post-game score in
# that endpoint, so scripts/50 wrote fantasy_points = NA for every one of them.
# The 2017-2021 rows are RotoGuru, which does carry DraftKings' own recorded
# score (site scoring, with milestone bonuses, -1 per INT/fumble). Left as-is,
# every 2022+ row is unusable for grading: no actual result to compare a
# projection against.
#
# Reconstructed here from box-score stats using DraftKings' published classic
# scoring rules (0.04/pass yd, 4/pass TD, -1/INT, +3 at 300+ pass yds;
# 0.1/rush or rec yd, 6/TD, +3 at 100+ yds; 1/reception; -1/fumble lost;
# 2/two-point conversion). This is a proxy, not an archived result, and it is
# on a **different scale** than ppr_points_actual used elsewhere in this
# project (which scores -2 for INT/fumble and has no yardage bonuses) - it is
# deliberately built to match RotoGuru's convention instead, since that is what
# it has to be comparable against for 2017-2021 continuity.
# Takes explicit vectors rather than a data frame, so there is no dependence on
# tidy-eval column capture inside a pipeline.
dk_classic_points <- function(pass_yd, pass_td, ints, rush_yd, rush_td,
                              rec, rec_yd, rec_td, fumbles_lost, two_pt) {
  z <- function(x) dplyr::coalesce(as.numeric(x), 0)
  pass_yd <- z(pass_yd); rush_yd <- z(rush_yd); rec_yd <- z(rec_yd)
  0.04 * pass_yd + 4 * z(pass_td) - 1 * z(ints) + 3 * as.integer(pass_yd >= 300) +
    0.1 * rush_yd + 6 * z(rush_td) + 3 * as.integer(rush_yd >= 100) +
    z(rec) + 0.1 * rec_yd + 6 * z(rec_td) + 3 * as.integer(rec_yd >= 100) -
    1 * z(fumbles_lost) + 2 * z(two_pt)
}

reconstructed_lookup <- player_stats |>
  dplyr::filter(.data$season %in% test_seasons, .data$season_type == "REG") |>
  dplyr::transmute(
    .data$season, .data$week,
    team = normalize_dfs_team(.data$team),
    salary_player_name_key = dfs_player_name_key(.data$player_display_name),
    reconstructed_points = dk_classic_points(
      .data$passing_yards, .data$passing_tds, .data$passing_interceptions,
      .data$rushing_yards, .data$rushing_tds,
      .data$receptions, .data$receiving_yards, .data$receiving_tds,
      .data$fumbles_lost_total,
      dplyr::coalesce(as.numeric(.data$passing_2pt_conversions), 0) +
        dplyr::coalesce(as.numeric(.data$rushing_2pt_conversions), 0) +
        dplyr::coalesce(as.numeric(.data$receiving_2pt_conversions), 0)
    )
  ) |>
  dplyr::distinct(.data$season, .data$week, .data$team,
                  .data$salary_player_name_key, .keep_all = TRUE)

before_na <- sum(is.na(salaries$fantasy_points) & salaries$season %in% test_seasons)
salaries <- salaries |>
  dplyr::mutate(salary_player_name_key = dfs_player_name_key(.data$player_name)) |>
  dplyr::left_join(
    reconstructed_lookup,
    by = c("season", "week", "team", "salary_player_name_key")
  ) |>
  dplyr::mutate(
    fantasy_points = dplyr::coalesce(.data$fantasy_points, .data$reconstructed_points)
  ) |>
  dplyr::select(-"reconstructed_points", -"salary_player_name_key")
after_na <- sum(is.na(salaries$fantasy_points) & salaries$season %in% test_seasons)
cat(sprintf(
  "Reconstructed actual scores for %d of %d NA salary rows in %s (still NA: %d).\n",
  before_na - after_na, before_na, paste(range(test_seasons), collapse = "-"),
  after_na
))

# Team defense (DEF/DST) needs its own reconstruction: player_stats has no
# team-defense rows at all, so the block above never touches it, and the
# lineup optimizer treats a DST slot as mandatory - with zero playable
# defenses the optimizer returns no lineup for every 2022-2025 week, which is
# why the first attempt at this produced an empty results file.
#
# Only points allowed is reconstructed here, from the final score in
# schedules.rds, using DraftKings' published points-allowed tiers. Sacks,
# takeaways, safeties and defensive/return touchdowns are not reconstructed -
# that needs play-by-play the project does not have cached for this range -
# so every DST score here is a partial total, understated relative to a real
# result. This is accepted rather than worked around, because the same partial
# formula feeds the model projection, the rolling baseline, and the actual
# result alike: what is being measured is the *lift* between the model lineup
# and the baseline lineup, and DST is one of nine roster slots, so a shared
# understatement does not favour either strategy. It would be a problem for an
# absolute DST accuracy claim; no such claim is made.
dk_points_allowed_score <- function(points_allowed) {
  dplyr::case_when(
    points_allowed <= 0 ~ 10,
    points_allowed <= 6 ~ 7,
    points_allowed <= 13 ~ 4,
    points_allowed <= 20 ~ 1,
    points_allowed <= 27 ~ 0,
    points_allowed <= 34 ~ -1,
    TRUE ~ -4
  )
}

dst_lookup <- schedules |>
  dplyr::filter(.data$season %in% test_seasons, .data$game_type == "REG") |>
  dplyr::transmute(
    .data$season, .data$week,
    team = normalize_dfs_team(.data$home_team),
    reconstructed_points = dk_points_allowed_score(.data$away_score)
  ) |>
  dplyr::bind_rows(
    schedules |>
      dplyr::filter(.data$season %in% test_seasons, .data$game_type == "REG") |>
      dplyr::transmute(
        .data$season, .data$week,
        team = normalize_dfs_team(.data$away_team),
        reconstructed_points = dk_points_allowed_score(.data$home_score)
      )
  )

before_dst_na <- sum(
  is.na(salaries$fantasy_points) & salaries$season %in% test_seasons &
    salaries$position %in% c("DEF", "DST")
)
salaries <- salaries |>
  dplyr::mutate(team = normalize_dfs_team(.data$team)) |>
  dplyr::left_join(dst_lookup, by = c("season", "week", "team")) |>
  dplyr::mutate(
    fantasy_points = dplyr::if_else(
      .data$position %in% c("DEF", "DST"),
      dplyr::coalesce(.data$fantasy_points, .data$reconstructed_points),
      .data$fantasy_points
    )
  ) |>
  dplyr::select(-"reconstructed_points")
after_dst_na <- sum(
  is.na(salaries$fantasy_points) & salaries$season %in% test_seasons &
    salaries$position %in% c("DEF", "DST")
)
cat(sprintf(
  "Reconstructed points-allowed-only DST scores for %d of %d NA defense rows.\n",
  before_dst_na - after_dst_na, before_dst_na
))

cat("Player-weeks:", nrow(player_stats),
    "seasons", min(player_stats$season), "-", max(player_stats$season), "\n")
cat("Salary rows:", nrow(salaries),
    "seasons", min(salaries$season), "-", max(salaries$season), "\n")

salary_coverage <- salaries |>
  dplyr::group_by(.data$season) |>
  dplyr::summarise(rows = dplyr::n(), weeks = dplyr::n_distinct(.data$week),
                   .groups = "drop")
cat("\nSalary coverage by season:\n")
print(as.data.frame(salary_coverage), row.names = FALSE)

# A season with materially fewer weeks than the others cannot support a lineup
# backtest, and silently reporting one would be worse than skipping it.
thin <- salary_coverage |>
  dplyr::filter(.data$season %in% test_seasons, .data$weeks < 12)
if (nrow(thin)) {
  cat("\nDropping seasons with fewer than 12 salary weeks:",
      paste(thin$season, collapse = ", "), "\n")
  test_seasons <- setdiff(test_seasons, thin$season)
}
cat("\nTest seasons:", paste(test_seasons, collapse = ", "), "\n\n")

# --------------------------------------------------------------------------
# Features and walk-forward component models
# --------------------------------------------------------------------------

feature_path <- "data/processed/dfs_backtest_features_2022_2025.rds"
wf_path <- "data/processed/dfs_value_walk_forward_2022_2025.rds"

if (file.exists(feature_path) && file.exists(wf_path)) {
  cat("Using cached features and walk-forward models...\n")
  features <- readRDS(feature_path)
  walk_forward <- readRDS(wf_path)
} else {
  cat("Building game context and leakage-safe features through 2025...\n")
  game_context <- build_td_game_context(schedules, rotowire,
                                        seasons = history_seasons)
  features <- prepare_fantasy_player_features(player_stats, game_context)
  saveRDS(features, feature_path)

  # One season at a time, keeping only the predictions.
  #
  # walk_forward_fantasy_models() returns an `artifacts` entry holding every
  # fitted model for every target and season, including their training
  # matrices. Asking for three seasons at once retains all of them and this
  # machine ran out of memory on twelve seasons of features. Nothing downstream
  # reads artifacts - build_historical_ppr_predictions() uses only
  # $predictions - so they are dropped as each season completes.
  season_predictions <- list()
  season_metrics <- list()
  for (test_season in test_seasons) {
    cat("Training walk-forward component models for", test_season, "...\n")
    fitted <- walk_forward_fantasy_models(
      features, test_seasons = test_season, seed = seed
    )
    season_predictions[[as.character(test_season)]] <- fitted$predictions
    season_metrics[[as.character(test_season)]] <- fitted$metrics
    rm(fitted)
    gc(verbose = FALSE)
  }
  walk_forward <- list(
    predictions = dplyr::bind_rows(season_predictions),
    metrics = dplyr::bind_rows(season_metrics)
  )
  rm(season_predictions, season_metrics)
  gc(verbose = FALSE)
  saveRDS(walk_forward, wf_path)
}

predictions <- build_historical_ppr_predictions(walk_forward, features)
main_games <- dk_main_slate_games(schedules, test_seasons)
salary_pool <- prepare_dk_salary_pool(
  salaries, dk_main_slate_games(schedules, history_seasons)
)
matched <- join_predictions_to_dk_salaries(
  dplyr::semi_join(predictions, main_games, by = "game_id"), salary_pool
)

cat("Matched player-games:", nrow(matched), "\n")
match_audit <- matched |>
  dplyr::count(.data$season, .data$match_method, name = "player_games") |>
  dplyr::group_by(.data$season) |>
  dplyr::mutate(match_rate = .data$player_games / sum(.data$player_games)) |>
  dplyr::ungroup()
readr::write_csv(match_audit, "outputs/dfs_match_audit_2022_2025.csv")

value_board <- add_market_salary_expectation(matched, salary_pool, test_seasons)
saveRDS(value_board, "data/processed/dfs_value_board_2022_2025.rds")

# --------------------------------------------------------------------------
# 3. Salary-implied benchmark
#
# `baseline_ppr` is a rolling player average. `salary_expected_ppr` is what the
# price implies. The first is a weak opponent; the second is the actual market.
# --------------------------------------------------------------------------

# add_market_salary_expectation() fits fantasy_points ~ salary + salary^2 per
# position on prior seasons only and calls its output salary_expected_points -
# this is the market-implied benchmark, built the same walk-forward way as the
# model, so the comparison is on equal footing.
salary_column <- "salary_expected_points"
if (!salary_column %in% names(value_board)) {
  stop("Expected column '", salary_column, "' is missing from the value board.",
       call. = FALSE)
}

scored <- value_board |>
  dplyr::filter(is.finite(.data$projected_ppr), is.finite(.data$fantasy_points),
                is.finite(.data[[salary_column]]))

projection_metrics <- scored |>
  dplyr::group_by(.data$season) |>
  dplyr::summarise(
    player_games = dplyr::n(),
    model_mae = mean(abs(.data$projected_ppr - .data$fantasy_points)),
    baseline_mae = mean(abs(.data$baseline_ppr - .data$fantasy_points)),
    salary_mae = mean(abs(.data[[salary_column]] - .data$fantasy_points)),
    model_rho = stats::cor(.data$projected_ppr, .data$fantasy_points,
                           method = "spearman"),
    baseline_rho = stats::cor(.data$baseline_ppr, .data$fantasy_points,
                              method = "spearman"),
    salary_rho = stats::cor(.data[[salary_column]], .data$fantasy_points,
                            method = "spearman"),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    beats_baseline = .data$baseline_mae - .data$model_mae,
    beats_salary = .data$salary_mae - .data$model_mae
  )

cat("\n=== Projection accuracy, 2022-2025 (positive = model better) ===\n")
print(as.data.frame(projection_metrics), digits = 4, row.names = FALSE)
readr::write_csv(projection_metrics, "outputs/dfs_projection_metrics_2022_2025.csv")

# --------------------------------------------------------------------------
# 2. Rare counts under Poisson deviance and log-loss
# --------------------------------------------------------------------------

poisson_deviance <- function(actual, predicted) {
  predicted <- pmax(predicted, 1e-6)
  terms <- ifelse(actual > 0, actual * log(actual / predicted), 0)
  mean(2 * (terms - (actual - predicted)))
}
log_loss <- function(actual_binary, probability) {
  p <- pmin(pmax(probability, 1e-6), 1 - 1e-6)
  -mean(actual_binary * log(p) + (1 - actual_binary) * log(1 - p))
}

# The component predictions are pivoted wide by build_historical_ppr_predictions
# into actual_<target> / prediction_<target> pairs.
count_targets <- c("rushing_tds", "receiving_tds", "receptions")
count_rows <- purrr::map_dfr(count_targets, function(target) {
  actual_col <- paste0("actual_", target)
  predicted_col <- paste0("prediction_", target)
  if (!all(c(actual_col, predicted_col) %in% names(scored))) {
    return(tibble::tibble())
  }
  d <- scored |>
    dplyr::filter(is.finite(.data[[actual_col]]),
                  is.finite(.data[[predicted_col]]))
  if (!nrow(d)) return(tibble::tibble())
  actual <- d[[actual_col]]
  predicted <- pmax(d[[predicted_col]], 0)
  tibble::tibble(
    target = target, rows = nrow(d),
    base_rate = mean(actual > 0), mean_actual = mean(actual),
    mean_predicted = mean(predicted),
    rmse = sqrt(mean((actual - predicted)^2)),
    poisson_deviance = poisson_deviance(actual, predicted),
    # Under a Poisson mean mu, P(at least one) is 1 - exp(-mu). This turns a
    # count projection into the probability the rare event happens at all,
    # which is the quantity a lineup actually cares about.
    log_loss_any = log_loss(as.integer(actual > 0), 1 - exp(-predicted)),
    log_loss_base_rate = log_loss(as.integer(actual > 0),
                                  rep(mean(actual > 0), nrow(d)))
  )
})

if (nrow(count_rows)) {
  cat("\n=== Rare-count components, scored as counts rather than by squared error ===\n")
  print(as.data.frame(count_rows), digits = 4, row.names = FALSE)
  cat("\nlog_loss_any below log_loss_base_rate means the model knows something\n")
  cat("about who scores, beyond how often anyone does.\n")
  readr::write_csv(count_rows, "outputs/dfs_count_metrics_2022_2025.csv")
} else {
  cat("\nNo component count predictions on the board; skipping count scoring.\n")
}

# --------------------------------------------------------------------------
# 1. Lineup backtest
# --------------------------------------------------------------------------

cat("\nOptimising DraftKings lineups, 2022-2025...\n")
lineup_backtest <- run_weekly_lineup_backtest(value_board, salary_pool)
readr::write_csv(lineup_backtest$weekly, "outputs/dfs_weekly_lineups_2022_2025.csv")

lineup_summary <- lineup_backtest$weekly |>
  dplyr::group_by(.data$strategy, .data$season) |>
  dplyr::summarise(
    weeks = dplyr::n(), average_actual_score = mean(.data$actual_score),
    median_actual_score = stats::median(.data$actual_score),
    score_150_rate = mean(.data$actual_score >= 150),
    .groups = "drop"
  )
cat("\n=== Lineup scores by season and strategy ===\n")
print(as.data.frame(lineup_summary), digits = 4, row.names = FALSE)
readr::write_csv(lineup_summary, "outputs/dfs_lineup_summary_2022_2025.csv")

model_vs_baseline <- lineup_backtest$weekly |>
  dplyr::select("season", "week", "strategy", "actual_score") |>
  tidyr::pivot_wider(names_from = "strategy", values_from = "actual_score",
                     names_prefix = "actual_")

if (all(c("actual_model", "actual_baseline") %in% names(model_vs_baseline))) {
  model_vs_baseline <- model_vs_baseline |>
    dplyr::mutate(model_lift = .data$actual_model - .data$actual_baseline)

  # Paired on the week, because both lineups face the same slate.
  set.seed(seed)
  draws <- vapply(seq_len(4000L), function(i) {
    mean(sample(model_vs_baseline$model_lift,
                nrow(model_vs_baseline), replace = TRUE))
  }, numeric(1))

  cat("\n=== Model lineup versus rolling baseline, 2022-2025 ===\n")
  cat(sprintf("  Slates                 : %d\n", nrow(model_vs_baseline)))
  cat(sprintf("  Mean lift              : %+.2f points\n",
              mean(model_vs_baseline$model_lift)))
  cat(sprintf("  Weeks won              : %d (%.1f%%)\n",
              sum(model_vs_baseline$model_lift > 0),
              100 * mean(model_vs_baseline$model_lift > 0)))
  cat(sprintf("  95%% interval           : %+.2f to %+.2f\n",
              stats::quantile(draws, 0.025), stats::quantile(draws, 0.975)))
  cat(sprintf("  P(lift > 0)            : %.4f\n", mean(draws > 0)))
  cat("\nFor comparison, 2017-2021 was +7.98 points over 86 slates,\n")
  cat("60.5% of weeks, interval +2.04 to +13.95.\n")

  readr::write_csv(model_vs_baseline, "outputs/dfs_model_vs_baseline_2022_2025.csv")
}

cat("\nNo contest ownership, entry fees or payouts exist for these seasons\n")
cat("either, so this remains a lineup-quality result and not a dollar ROI.\n")
cat("Nothing here licenses a claim that these lineups beat cash games.\n")
