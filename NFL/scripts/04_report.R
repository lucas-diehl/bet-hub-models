source("R/utilities.R")
assert_packages()
ensure_directories()

metrics <- readr::read_csv("outputs/model_metrics_by_season.csv", show_col_types = FALSE)
thresholds <- readr::read_csv("outputs/threshold_results.csv", show_col_types = FALSE)

metric_plot <- metrics |>
  ggplot2::ggplot(ggplot2::aes(.data$season, .data$mae, color = .data$model)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~ target, scales = "free_y") +
  ggplot2::labs(
    title = "Walk-forward NFL prediction error",
    x = NULL, y = "MAE", color = "Model"
  ) +
  ggplot2::theme_minimal(base_size = 12)
ggplot2::ggsave("outputs/mae_by_season.png", metric_plot, width = 10, height = 6, dpi = 160)

roi_plot <- thresholds |>
  dplyr::filter(.data$bets >= 40) |>
  ggplot2::ggplot(
    ggplot2::aes(.data$threshold, .data$roi, color = .data$model)
  ) +
  ggplot2::geom_hline(yintercept = 0, linetype = 2) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~ target) +
  ggplot2::scale_y_continuous(labels = scales::label_percent()) +
  ggplot2::labs(
    title = "Flat-stake ROI by model-edge threshold",
    subtitle = "Aggregate walk-forward backtest; points of edge; minimum 40 bets",
    x = "Minimum edge (points)", y = "ROI", color = "Model"
  ) +
  ggplot2::theme_minimal(base_size = 12)
ggplot2::ggsave("outputs/roi_by_threshold.png", roi_plot, width = 10, height = 6, dpi = 160)

if (file.exists("outputs/recommended_portfolio_paths.csv")) {
  portfolio <- readr::read_csv(
    "outputs/recommended_portfolio_paths.csv",
    show_col_types = FALSE
  )
  bankroll_plot <- portfolio |>
    ggplot2::ggplot(ggplot2::aes(.data$game_date, .data$flat_bankroll)) +
    ggplot2::geom_hline(yintercept = 100, linetype = 2, color = "grey50") +
    ggplot2::geom_line(color = "#8C1D40", linewidth = 0.8) +
    ggplot2::labs(
      title = "Recommended-model portfolio: flat-stake bankroll",
      subtitle = "100 starting units; one unit per qualifying wager",
      x = NULL, y = "Bankroll (units)"
    ) +
    ggplot2::theme_minimal(base_size = 12)
  ggplot2::ggsave(
    "outputs/recommended_bankroll.png",
    bankroll_plot,
    width = 10, height = 5.5, dpi = 160
  )
}

if (file.exists("outputs/edge_bins.csv")) {
  edges <- readr::read_csv("outputs/edge_bins.csv", show_col_types = FALSE) |>
    dplyr::filter(
      (.data$target == "game_total" & .data$model == "forward_linear") |
        (.data$target == "home_margin" & .data$model == "random_forest")
    )
  edge_plot <- edges |>
    ggplot2::ggplot(
      ggplot2::aes(.data$edge_bin, .data$roi, fill = .data$target)
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::labs(
      title = "ROI by absolute model edge",
      subtitle = "Walk-forward-selected wagers only",
      x = "Model edge (points)", y = "ROI", fill = "Market"
    ) +
    ggplot2::theme_minimal(base_size = 12)
  ggplot2::ggsave(
    "outputs/roi_by_edge_bin.png",
    edge_plot,
    width = 9, height = 5.5, dpi = 160
  )
}
message("Report figures written to outputs/.")
