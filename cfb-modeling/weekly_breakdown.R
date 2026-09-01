suppressPackageStartupMessages(library(tidyverse))

bt <- read.csv("backtest_results.csv")

MIN_EDGE <- 0.03

bt <- bt %>%
  mutate(
    spread_correct = (spread_prob > 0.5 & home_covered == 1) | (spread_prob <= 0.5 & home_covered == 0),
    spread_edge    = abs(spread_prob - 0.5),
    spread_ev      = spread_prob * 0.909 - (1 - spread_prob),
    spread_rec     = spread_edge >= MIN_EDGE & spread_ev > 0,
    totals_correct = (totals_prob > 0.5 & over_hit == 1) | (totals_prob <= 0.5 & over_hit == 0),
    totals_edge    = abs(totals_prob - 0.5),
    totals_ev      = totals_prob * 0.909 - (1 - totals_prob),
    totals_rec     = totals_edge >= MIN_EDGE & totals_ev > 0
  ) %>%
  filter(!is.na(week))

period_summary <- function(df, rec_col, correct_col, label) {
  cat(sprintf("\n=== %s: BY WEEK PERIOD ===\n", label))
  df %>%
    filter(.data[[rec_col]], !is.na(.data[[correct_col]])) %>%
    mutate(week_period = factor(week_period, levels = c("early", "mid", "late"))) %>%
    group_by(week_period) %>%
    summarise(
      bets    = n(),
      wins    = sum(.data[[correct_col]]),
      losses  = bets - wins,
      win_pct = round(wins / bets * 100, 1),
      profit  = round(wins * 0.909 - losses, 1),
      roi     = round(profit / bets * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(week_period) %>%
    print()
}

week_summary <- function(df, rec_col, correct_col, label) {
  cat(sprintf("\n=== %s: BY INDIVIDUAL WEEK ===\n", label))
  df %>%
    filter(.data[[rec_col]], !is.na(.data[[correct_col]]), week >= 1, week <= 15) %>%
    group_by(week) %>%
    summarise(
      bets    = n(),
      wins    = sum(.data[[correct_col]]),
      win_pct = round(wins / bets * 100, 1),
      profit  = round(wins * 0.909 - (bets - wins), 1),
      roi     = round(profit / bets * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(week) %>%
    print(n = 20)
}

period_summary(bt, "spread_rec", "spread_correct", "SPREAD")
period_summary(bt, "totals_rec", "totals_correct", "TOTALS")

week_summary(bt, "spread_rec", "spread_correct", "SPREAD")
week_summary(bt, "totals_rec", "totals_correct", "TOTALS")
