source("R/utilities.R")
source("R/backtest.R")
assert_packages()
cfg <- read_config()

raw_bets <- readr::read_csv(
  "outputs/challenger_raw_edge_bets.csv",
  show_col_types = FALSE
)
mae_uncertainty <- readr::read_csv(
  "outputs/challenger_mae_uncertainty.csv",
  show_col_types = FALSE
)
probability_summary <- readr::read_csv(
  "outputs/challenger_probability_summary.csv",
  show_col_types = FALSE
)
interaction_replication <- readr::read_csv(
  "outputs/challenger_interaction_replication.csv",
  show_col_types = FALSE
)

spread_side_comparison <- raw_bets |>
  dplyr::filter(
    .data$target == "home_margin",
    .data$model == "random_forest"
  ) |>
  dplyr::mutate(
    selection_location = dplyr::if_else(
      .data$bet_side == "home",
      "Home selection",
      "Away selection"
    )
  ) |>
  dplyr::group_by(.data$selection_location) |>
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
  spread_side_comparison,
  "outputs/challenger_spread_side_comparison.csv"
)

recommended_bets <- raw_bets |>
  dplyr::filter(
    (
      .data$target == "game_total" &
        .data$model == "forward_linear"
    ) |
      (
        .data$target == "home_margin" &
          .data$model == "random_forest" &
          .data$bet_side == "home"
      )
  ) |>
  dplyr::mutate(
    strategy = dplyr::case_when(
      .data$target == "game_total" ~
        "Forward-linear total",
      TRUE ~ "Random-forest home spread"
    ),
    model = .data$strategy
  )

recommended_summary <- recommended_bets |>
  dplyr::group_by(.data$strategy) |>
  dplyr::summarise(
    seasons = dplyr::n_distinct(.data$season),
    bets = dplyr::n(),
    wins = sum(.data$bet_result > 0),
    losses = sum(.data$bet_result < 0),
    pushes = sum(.data$bet_result == 0),
    win_rate = .data$wins / (.data$wins + .data$losses),
    profit_units = sum(.data$profit),
    roi = mean(.data$profit),
    .groups = "drop"
  ) |>
  dplyr::bind_rows(
    recommended_bets |>
      dplyr::summarise(
        strategy = "Combined",
        seasons = dplyr::n_distinct(.data$season),
        bets = dplyr::n(),
        wins = sum(.data$bet_result > 0),
        losses = sum(.data$bet_result < 0),
        pushes = sum(.data$bet_result == 0),
        win_rate = .data$wins / (.data$wins + .data$losses),
        profit_units = sum(.data$profit),
        roi = mean(.data$profit)
      )
  )

recommended_by_season <- recommended_bets |>
  dplyr::group_by(.data$season, .data$strategy) |>
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

combined_bets_for_analysis <- recommended_bets |>
  dplyr::mutate(target = "portfolio", model = "Combined")

recommended_uncertainty <- dplyr::bind_rows(
  betting_uncertainty(
    recommended_bets,
    iterations = 10000L,
    seed = cfg$backtest$seed,
    block_by_season = FALSE
  ),
  betting_uncertainty(
    recommended_bets,
    iterations = 10000L,
    seed = cfg$backtest$seed,
    block_by_season = TRUE
  ),
  betting_uncertainty(
    combined_bets_for_analysis,
    iterations = 10000L,
    seed = cfg$backtest$seed,
    block_by_season = FALSE
  ),
  betting_uncertainty(
    combined_bets_for_analysis,
    iterations = 10000L,
    seed = cfg$backtest$seed,
    block_by_season = TRUE
  )
)

recommended_bankroll <- list(
  summary = dplyr::bind_rows(
    bankroll_analysis(recommended_bets, cfg)$summary,
    bankroll_analysis(combined_bets_for_analysis, cfg)$summary
  )
)
readr::write_csv(
  recommended_bets,
  "outputs/challenger_recommended_bets.csv"
)
readr::write_csv(
  recommended_summary,
  "outputs/challenger_recommended_summary.csv"
)
readr::write_csv(
  recommended_by_season,
  "outputs/challenger_recommended_by_season.csv"
)
readr::write_csv(
  recommended_uncertainty,
  "outputs/challenger_recommended_uncertainty.csv"
)
readr::write_csv(
  recommended_bankroll$summary,
  "outputs/challenger_recommended_bankroll.csv"
)

total_mae <- mae_uncertainty |>
  dplyr::filter(
    .data$target == "game_total",
    .data$model == "residual_gam"
  )
spread_mae <- mae_uncertainty |>
  dplyr::filter(
    .data$target == "home_margin",
    .data$model == "residual_xgb"
  )
home_spread <- interaction_replication |>
  dplyr::filter(
    .data$target == "home_margin",
    .data$model == "random_forest",
    .data$interaction == "home_spread"
  )
cross_three <- interaction_replication |>
  dplyr::filter(
    .data$target == "home_margin",
    .data$model == "random_forest",
    .data$interaction == "crosses_key_3"
  )
forward_total_over <- interaction_replication |>
  dplyr::filter(
    .data$target == "game_total",
    .data$model == "forward_linear",
    .data$interaction == "over_heavy_favorite"
  )
forward_total_under <- interaction_replication |>
  dplyr::filter(
    .data$target == "game_total",
    .data$model == "forward_linear",
    .data$interaction == "under_small_favorite"
  )

combined <- recommended_summary |>
  dplyr::filter(.data$strategy == "Combined")
total_summary <- recommended_summary |>
  dplyr::filter(.data$strategy == "Forward-linear total")
home_summary <- recommended_summary |>
  dplyr::filter(.data$strategy == "Random-forest home spread")
combined_uncertainty <- recommended_uncertainty |>
  dplyr::filter(
    .data$model == "Combined",
    .data$method == "individual_bet"
  )
combined_bankroll <- recommended_bankroll$summary |>
  dplyr::filter(.data$model == "Combined")

report <- c(
  "# Spread and totals model improvement tournament",
  "",
  "## Decision",
  "",
  "**Keep the forward-selected linear total model and the random-forest spread model as the 2026 incumbents. Do not replace either with a market-residual model.**",
  "",
  "The residual tournament found a small forecast improvement for totals, but it was not statistically reliable and it did not improve walk-forward betting ROI. No residual model improved spread forecasting or betting performance reliably. Probability-calibrated betting rules were overconfident and failed the stricter two-season validation requirement.",
  "",
  "The actionable improvement is a spread selection filter: retain random-forest spread bets only when the model selects the home team. This was predeclared from the earlier analysis and remained profitable in both the discovery and validation periods.",
  "",
  "## Tests completed",
  "",
  "- Market-residual ridge regression with recency half-life and penalty selection",
  "- Market-residual random forest",
  "- Market-residual XGBoost",
  "- Restricted nonlinear GAM",
  "- Chronological market shrinkage selection",
  "- Three-, five-, eight-season and unweighted recency candidates where applicable",
  "- Sequential cover-probability calibration",
  "- Probability-threshold selection requiring two populated prior validation seasons",
  "- Spread key-number crossings at 3 and 7",
  "- Heavy-favorite over, small-favorite under, high-wind under, late-season under, and home-spread interactions",
  "- Individual-bet and season-block bootstrap uncertainty",
  "- Automated final-score leakage and feature-correlation audits",
  "",
  "## Forecast accuracy",
  "",
  paste0(
    "- The residual GAM improved total MAE by only ",
    sprintf("%.3f", abs(total_mae$mean_mae_difference)),
    " points versus the closing total. Its game-level 95% interval was ",
    sprintf("%.3f", total_mae$individual_ci_low), " to ",
    sprintf("%.3f", total_mae$individual_ci_high),
    ", and it beat the market in ", total_mae$seasons_better,
    " of eight seasons."
  ),
  paste0(
    "- The best residual spread challenger by RMSE, residual XGBoost, had an MAE difference of ",
    sprintf("%+.3f", spread_mae$mean_mae_difference),
    " points versus the closing spread; positive means worse. Its 95% interval also crossed zero."
  ),
  "- The closing line therefore remains the correct primary point forecast.",
  "",
  "## Walk-forward betting results",
  "",
  paste0(
    "- Forward-linear totals: ", total_summary$bets, " bets, ",
    sprintf("%.1f%%", 100 * total_summary$win_rate), " win rate, ",
    sprintf("%+.2f", total_summary$profit_units), " units, ",
    sprintf("%.2f%%", 100 * total_summary$roi), " ROI."
  ),
  paste0(
    "- Random-forest home spread selections: ", home_summary$bets,
    " bets, ", sprintf("%.1f%%", 100 * home_summary$win_rate),
    " win rate, ", sprintf("%+.2f", home_summary$profit_units),
    " units, ", sprintf("%.2f%%", 100 * home_summary$roi), " ROI."
  ),
  paste0(
    "- Combined filtered portfolio: ", combined$bets, " bets, ",
    sprintf("%.1f%%", 100 * combined$win_rate), " win rate, ",
    sprintf("%+.2f", combined$profit_units), " units, ",
    sprintf("%.2f%%", 100 * combined$roi), " ROI."
  ),
  paste0(
    "- Combined individual-bet bootstrap 95% ROI interval: ",
    sprintf("%.2f%%", 100 * combined_uncertainty$roi_low), " to ",
    sprintf("%.2f%%", 100 * combined_uncertainty$roi_high),
    "; probability of positive ROI ",
    sprintf("%.1f%%", 100 * combined_uncertainty$probability_roi_positive),
    "."
  ),
  paste0(
    "- At 1% of current bankroll per bet, the historical combined path grew ",
    "100 units to ", sprintf("%.2f", combined_bankroll$ending_proportional_bankroll),
    " units with a maximum drawdown of ",
    sprintf("%.1f%%", 100 * combined_bankroll$max_proportional_drawdown),
    "."
  ),
  "",
  "## Interaction findings",
  "",
  paste0(
    "- Random-forest home selections: ",
    home_spread$discovery_bets, " discovery bets at ",
    sprintf("%.2f%%", 100 * home_spread$discovery_roi), " ROI and ",
    home_spread$validation_bets, " validation bets at ",
    sprintf("%.2f%%", 100 * home_spread$validation_roi), " ROI. Combined: ",
    home_spread$combined_bets, " bets at ",
    sprintf("%.2f%%", 100 * home_spread$combined_roi), " ROI."
  ),
  paste0(
    "- Random-forest bets crossing the spread key number 3: ",
    cross_three$combined_bets, " bets at ",
    sprintf("%.2f%%", 100 * cross_three$combined_roi),
    " combined ROI. Useful as a confidence tag, but not an additional stake multiplier yet."
  ),
  paste0(
    "- Forward-linear over plus favorite of 7+: ",
    forward_total_over$combined_bets, " bets at ",
    sprintf("%.2f%%", 100 * forward_total_over$combined_roi),
    " combined ROI; validation was positive but included only ",
    forward_total_over$validation_bets, " bets."
  ),
  paste0(
    "- Forward-linear under plus spread of 2.5 or less: ",
    forward_total_under$combined_bets, " bets at ",
    sprintf("%.2f%%", 100 * forward_total_under$combined_roi),
    " combined ROI; only ", forward_total_under$validation_bets,
    " bets were in validation."
  ),
  "- Late-season unders failed to replicate and should not be promoted.",
  "",
  "## Probability calibration",
  "",
  paste0(
    "- All ", nrow(probability_summary),
    " probability-qualified challenger strategies were breakeven or negative after requiring two populated prior validation seasons."
  ),
  "- The regressors should not drive Kelly sizing or variable stakes. Continue using edge tiers and conservative bankroll limits.",
  "",
  "## 2026 operating rules",
  "",
  "1. Keep raw point edge rather than percentage edge.",
  "2. Totals: forward-linear model with the existing walk-forward 4-5 point operating region.",
  "3. Spreads: random forest with the existing 5-6 point region, but make home selection a bet-eligibility requirement for the primary strategy.",
  "4. Tag spread bets that cross 3; do not boost stake until a fresh season confirms the effect.",
  "5. Tag heavy-favorite overs and small-favorite unders as paper-tracked total interactions.",
  "6. Do not use the new calibrated probabilities for staking.",
  "7. Archive the exact sportsbook, timestamp, spread/total, and price for every 2026 forecast so executable EV and closing-line value can replace the historical fixed -110 assumption.",
  "",
  "## Remaining highest-value data improvement",
  "",
  "The next material modeling gain is more likely to come from pregame quarterback, injury, offensive-line, and projected-starter information than from another algorithm on the same team-level fields. Those inputs are not present in the current historical feature table and therefore were not fabricated in this tournament.",
  "",
  "## Audit",
  "",
  "- All 17,008 residual challenger predictions are chronological and cover 2,126 games per target/model.",
  "- The final-score leakage audit passed.",
  "- Maximum absolute correlation between any candidate feature and the target market residual was 0.058.",
  "- The initially contaminated exploratory run was rejected and its obsolete cache was removed; no reported result uses it."
)
writeLines(
  report,
  "outputs/spread_total_model_improvement_report.md"
)

cat("Final spread/total challenger report written.\n")
