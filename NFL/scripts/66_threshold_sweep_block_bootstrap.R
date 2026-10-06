source("R/utilities.R")
source("R/models.R")
source("R/backtest.R")
assert_packages()
ensure_directories()
cfg <- read_config()

# P1 sections 3 and 4.
#
# Section 3: the 6.0 spread threshold and the home-only filter were both chosen
# after seeing the data. If ROI is a plateau across neighbouring thresholds then
# they are a reasonable operating point; if 6.0 is a lone spike surrounded by
# nothing, the rule is fitted to noise and the arm belongs on paper. Same
# question for the home filter: a genuine home effect should not vanish when the
# threshold moves half a point.
#
# The rule is stated before the numbers are read:
#   plateau (neighbouring thresholds within roughly a third of the headline)
#     -> keep the filter, keep the arm funded
#   isolated spike at 6.0 -> demote spreads to paper
#
# Section 4: bets are not independent. Both sides of a correlated week move
# together, and a bet-level bootstrap treats each as fresh information, which
# narrows the interval. Resampling whole weeks respects that.

sweep_thresholds <- seq(4.0, 7.0, by = 0.25)
n_boot <- 4000L

features <- readRDS("data/processed/game_features.rds")
test_seasons <- cfg$backtest$start_season:cfg$backtest$end_season

weeks <- readRDS("data/raw/schedules.rds") |>
  dplyr::select("game_id", "week")

walk_forward_one <- function(model, target, seed) {
  purrr::map_dfr(test_seasons, function(s) {
    train <- dplyr::filter(features, .data$season < s)
    test <- dplyr::filter(features, .data$season == s)
    if (!nrow(train) || !nrow(test)) return(tibble::tibble())
    set.seed(seed)
    prediction <- fit_predict_model(
      model, train, test, target, cfg, seed = seed
    )
    tibble::tibble(
      season = s, game_id = test$game_id, truth = test[[target]],
      prediction = prediction, home_line = test$home_line,
      total_line = test$total_line
    )
  })
}

# Spreads: prefer the seed-averaged predictions, since a sweep run on a single
# fit would be measuring the seed as much as the threshold.
spread_path <- "data/processed/spread_seed_averaged_predictions.rds"
if (file.exists(spread_path)) {
  spread_pred <- readRDS(spread_path)
  cat("Spread predictions: seed-averaged\n")
} else {
  spread_pred <- walk_forward_one("random_forest", "home_margin",
                                  cfg$backtest$seed)
  cat("Spread predictions: single seed (run scripts/64 for the average)\n")
}

# Totals are a forward-selected linear model with no RNG, so one pass is the
# whole distribution.
total_pred <- walk_forward_one("forward_linear", "game_total", cfg$backtest$seed)

grade_bets <- function(pred, market, threshold, side_filter) {
  d <- pred |>
    dplyr::mutate(
      market_value = if (market == "total") .data$total_line else -.data$home_line,
      edge = .data$prediction - .data$market_value
    ) |>
    dplyr::filter(abs(.data$edge) >= threshold)
  if (side_filter == "home_over") d <- dplyr::filter(d, .data$edge > 0)
  if (side_filter == "away_under") d <- dplyr::filter(d, .data$edge < 0)
  if (!nrow(d)) return(tibble::tibble())
  d |>
    dplyr::mutate(
      result = dplyr::if_else(
        .data$edge > 0,
        signed_result(.data$truth - .data$market_value),
        signed_result(.data$market_value - .data$truth)
      ),
      profit = american_profit(.data$result, cfg$backtest$american_odds)
    ) |>
    dplyr::left_join(weeks, by = "game_id")
}

summarise_bets <- function(d) {
  if (!nrow(d)) return(tibble::tibble(bets = 0L, win_rate = NA_real_,
                                      roi = NA_real_))
  tibble::tibble(
    bets = nrow(d),
    win_rate = sum(d$result > 0) / max(1L, sum(d$result != 0)),
    roi = sum(d$profit) / nrow(d)
  )
}

# --------------------------------------------------------------------------
# Section 3: threshold sweep
# --------------------------------------------------------------------------

sides <- c("all", "home_over", "away_under")
sweep <- purrr::map_dfr(sweep_thresholds, function(th) {
  purrr::map_dfr(sides, function(sd) {
    dplyr::bind_rows(
      dplyr::bind_cols(
        tibble::tibble(market = "spread", threshold = th, side = sd),
        summarise_bets(grade_bets(spread_pred, "spread", th, sd))
      ),
      dplyr::bind_cols(
        tibble::tibble(market = "total", threshold = th, side = sd),
        summarise_bets(grade_bets(total_pred, "total", th, sd))
      )
    )
  })
})

cat("\n=== Threshold sweep, spreads ===\n")
print(as.data.frame(
  sweep |> dplyr::filter(.data$market == "spread") |>
    tidyr::pivot_wider(id_cols = "threshold", names_from = "side",
                       values_from = c("bets", "roi"))
), digits = 4)

cat("\n=== Threshold sweep, totals ===\n")
print(as.data.frame(
  sweep |> dplyr::filter(.data$market == "total") |>
    tidyr::pivot_wider(id_cols = "threshold", names_from = "side",
                       values_from = c("bets", "roi"))
), digits = 4)

readr::write_csv(sweep, "outputs/threshold_sweep.csv")

# Is 6.0 a plateau or a spike? Compare it against its neighbours on a like
# sample size, using only thresholds with enough bets to mean anything.
neighbourhood <- sweep |>
  dplyr::filter(.data$market == "spread", .data$side == "home_over",
                .data$bets >= 60)
if (nrow(neighbourhood)) {
  at_six <- neighbourhood$roi[neighbourhood$threshold == 6.0]
  others <- neighbourhood$roi[neighbourhood$threshold != 6.0]
  cat("\n=== Is 6.0 a plateau or a spike (spreads, home only)? ===\n")
  cat(sprintf("  ROI at 6.0        : %+.4f\n", if (length(at_six)) at_six else NA))
  cat(sprintf("  Neighbour median  : %+.4f\n", stats::median(others)))
  cat(sprintf("  Neighbour range   : %+.4f to %+.4f\n", min(others), max(others)))
  cat(sprintf("  Thresholds positive: %d of %d\n",
              sum(neighbourhood$roi > 0), nrow(neighbourhood)))
}

# --------------------------------------------------------------------------
# Section 4: block bootstrap, week as the resampling unit
# --------------------------------------------------------------------------

block_bootstrap <- function(d, n = n_boot, seed = 20260901L) {
  if (!nrow(d)) return(NULL)
  d <- dplyr::filter(d, !is.na(.data$week))
  d$block <- paste(d$season, d$week, sep = "-")
  blocks <- split(d$profit, d$block)
  keys <- names(blocks)
  set.seed(seed)
  draws <- vapply(seq_len(n), function(i) {
    picked <- sample(keys, length(keys), replace = TRUE)
    pooled <- unlist(blocks[picked], use.names = FALSE)
    if (!length(pooled)) return(NA_real_)
    mean(pooled)
  }, numeric(1))
  draws[is.finite(draws)]
}

bet_bootstrap <- function(d, n = n_boot, seed = 20260901L) {
  if (!nrow(d)) return(NULL)
  set.seed(seed)
  vapply(seq_len(n), function(i) mean(sample(d$profit, nrow(d), replace = TRUE)),
         numeric(1))
}

report_interval <- function(label, d) {
  if (!nrow(d)) return(tibble::tibble())
  blk <- block_bootstrap(d)
  bet <- bet_bootstrap(d)
  tibble::tibble(
    arm = label, bets = nrow(d),
    weeks = dplyr::n_distinct(paste(d$season, d$week)),
    roi = mean(d$profit),
    bet_lo = stats::quantile(bet, 0.025), bet_hi = stats::quantile(bet, 0.975),
    bet_p_pos = mean(bet > 0),
    block_lo = stats::quantile(blk, 0.025), block_hi = stats::quantile(blk, 0.975),
    block_p_pos = mean(blk > 0)
  )
}

spread_bets <- grade_bets(spread_pred, "spread", 6.0, "home_over")
total_bets <- grade_bets(total_pred, "total", 5.0, "all")
portfolio <- dplyr::bind_rows(
  dplyr::mutate(spread_bets, arm = "spread"),
  dplyr::mutate(total_bets, arm = "total")
)

intervals <- dplyr::bind_rows(
  report_interval("spread rf 6.0 home", spread_bets),
  report_interval("total fl 5.0", total_bets),
  report_interval("combined portfolio", portfolio)
)

cat("\n=== Bet-level vs week-block bootstrap (", n_boot, " draws) ===\n", sep = "")
print(as.data.frame(intervals), digits = 4, row.names = FALSE)
readr::write_csv(intervals, "outputs/block_bootstrap_intervals.csv")

cat("\nThe week-block interval is the one to quote. Both sides of a correlated",
    "\nweek move together, so a bet-level resample counts them as independent",
    "\nevidence and reports a narrower interval than the data supports.\n")
