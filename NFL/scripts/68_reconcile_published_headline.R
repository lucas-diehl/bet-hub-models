source("R/utilities.R")
source("R/feature_registry.R")
source("R/models.R")
source("R/backtest.R")
assert_packages()
ensure_directories()
cfg <- read_config()

# Reconciling the published headline with the rule that actually emits picks.
#
# outputs/walk_forward_bets.csv holds one row per model x target x per-season
# *selected* threshold. The 525-bet, +9.18% portfolio in the handoff is
# forward-linear totals (356 bets at selected thresholds of 3.5, 4.0 and 5.0)
# plus random-forest home spreads (169 bets at 3.5, 5.0 and 6.0). It is labelled
# "5+ pts" and "6+ pts", which are only the modal selections.
#
# The live feed does something different. build_game_bet_candidates() applies a
# fixed 5.0 to totals and a fixed 6.0 to spreads, every week, with no per-season
# reselection. So the backtested strategy and the deployed strategy are not the
# same strategy, and the number on the site describes the one nobody is betting.
#
# This script measures the deployed rule on the same footing, separating the two
# things that changed at once: the window (the published record covers 2020-2025;
# the config backtest covers 2018-2025) and the rule.

features <- readRDS("data/processed/game_features.rds")
test_seasons <- cfg$backtest$start_season:cfg$backtest$end_season

walk_forward_one <- function(model, target, seed) {
  purrr::map_dfr(test_seasons, function(s) {
    train <- dplyr::filter(features, .data$season < s)
    test <- dplyr::filter(features, .data$season == s)
    if (!nrow(train) || !nrow(test)) return(tibble::tibble())
    set.seed(seed)
    prediction <- fit_predict_model(model, train, test, target, cfg, seed = seed)
    tibble::tibble(
      season = s, game_id = test$game_id, truth = test[[target]],
      prediction = prediction, home_line = test$home_line,
      total_line = test$total_line
    )
  })
}

spread_path <- "data/processed/spread_seed_averaged_predictions.rds"
if (!file.exists(spread_path)) {
  stop("Run scripts/64 first to build seed-averaged spread predictions.",
       call. = FALSE)
}
spread_pred <- readRDS(spread_path)
total_pred <- walk_forward_one("forward_linear", "game_total", cfg$backtest$seed)

grade_bets <- function(pred, market, threshold, home_only) {
  d <- pred |>
    dplyr::mutate(
      market_value = if (market == "total") .data$total_line else -.data$home_line,
      edge = .data$prediction - .data$market_value
    ) |>
    dplyr::filter(abs(.data$edge) >= threshold)
  if (home_only) d <- dplyr::filter(d, .data$edge > 0)
  if (!nrow(d)) return(tibble::tibble())
  d |>
    dplyr::mutate(
      result = dplyr::if_else(
        .data$edge > 0,
        signed_result(.data$truth - .data$market_value),
        signed_result(.data$market_value - .data$truth)
      ),
      profit = american_profit(.data$result, cfg$backtest$american_odds)
    )
}

boot <- function(profit, n = 4000L, seed = 20260901L) {
  if (length(profit) < 20) return(c(NA_real_, NA_real_, NA_real_))
  set.seed(seed)
  draws <- vapply(seq_len(n),
                  function(i) mean(sample(profit, length(profit), replace = TRUE)),
                  numeric(1))
  c(stats::quantile(draws, c(0.025, 0.975)), mean(draws > 0))
}

report <- function(label, seasons) {
  bets <- dplyr::bind_rows(
    grade_bets(dplyr::filter(spread_pred, .data$season %in% seasons),
               "spread", 6.0, TRUE),
    grade_bets(dplyr::filter(total_pred, .data$season %in% seasons),
               "total", 5.0, FALSE)
  )
  ci <- boot(bets$profit)
  # Extract before building the tibble: a column named `bets` would shadow the
  # data frame for every expression after it.
  n_bets <- nrow(bets)
  wins <- sum(bets$result > 0)
  decided <- max(1L, sum(bets$result != 0))
  roi <- mean(bets$profit)
  tibble::tibble(
    definition = label, seasons = paste(range(seasons), collapse = "-"),
    bets = n_bets, win_rate = wins / decided, roi = roi,
    ci_lo = ci[[1]], ci_hi = ci[[2]], p_positive = ci[[3]]
  )
}

deployed <- dplyr::bind_rows(
  report("Deployed fixed rule (5.0 total / 6.0 home spread)", 2020:2025),
  report("Deployed fixed rule (5.0 total / 6.0 home spread)", 2018:2025)
)

# The published portfolio, read back from the record that produced it.
published <- readr::read_csv("outputs/walk_forward_bets.csv",
                             show_col_types = FALSE) |>
  dplyr::filter(
    abs(.data$edge) >= .data$selected_threshold,
    (.data$target == "game_total" & .data$model == "forward_linear") |
      (.data$target == "home_margin" & .data$model == "random_forest" &
         .data$edge > 0)
  )
pub_ci <- boot(published$profit)

pub_n <- nrow(published)
pub_wins <- sum(published$bet_result > 0)
pub_decided <- max(1L, sum(published$bet_result != 0))
pub_roi <- mean(published$profit)
pub_seasons <- paste(range(published$season), collapse = "-")

cat("=== The published headline, recomputed from its own record ===\n")
print(as.data.frame(tibble::tibble(
  definition = "Published: per-season selected thresholds",
  seasons = pub_seasons, bets = pub_n,
  win_rate = pub_wins / pub_decided, roi = pub_roi,
  ci_lo = pub_ci[[1]], ci_hi = pub_ci[[2]], p_positive = pub_ci[[3]]
)), digits = 4, row.names = FALSE)

cat("\nThresholds the published portfolio actually selected:\n")
print(as.data.frame(
  published |>
    dplyr::group_by(.data$target, .data$selected_threshold) |>
    dplyr::summarise(bets = dplyr::n(), roi = mean(.data$profit),
                     .groups = "drop")
), digits = 4, row.names = FALSE)

cat("\n=== The rule the feed actually publishes ===\n")
print(as.data.frame(deployed), digits = 4, row.names = FALSE)

readr::write_csv(
  dplyr::bind_rows(deployed), "outputs/published_vs_deployed_rule.csv"
)

cat("\nIf these two differ materially, the number on the site is describing a\n")
cat("strategy nobody is betting, and the deployed row is the honest headline.\n")
