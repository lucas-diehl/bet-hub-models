source("R/utilities.R")
assert_packages()

# Does posting picks a day or more ahead of kickoff break the selection rules?
#
# The 2026 plan says to price bets 60-90 minutes before kickoff. That is fine
# for grading and closing-line value but useless operationally: nobody can place
# a bet that appears an hour before the game. The question is whether an earlier
# line still produces the same picks.
#
# The 2025 snapshots bought for the closing-line-value work answer it directly.
# Each game has an early line from the Tuesday of its own game week (median 123
# hours out) and a late line a median of 1.3 hours before kickoff. If a bet that
# qualifies on Tuesday still qualifies at the close, then anything inside that
# window - a day ahead included - is safe, because Tuesday is the furthest out.

movement <- readr::read_csv("outputs/line_movement_2025.csv",
                            show_col_types = FALSE)
predictions <- readr::read_csv("outputs/predictions.csv",
                               show_col_types = FALSE) |>
  dplyr::filter(
    .data$season == 2025,
    (.data$target == "game_total" & .data$model == "forward_linear") |
      (.data$target == "home_margin" & .data$model == "random_forest")
  ) |>
  dplyr::select("game_id", "target", "prediction") |>
  tidyr::pivot_wider(names_from = "target", values_from = "prediction")

joined <- movement |>
  dplyr::inner_join(predictions, by = "game_id") |>
  dplyr::mutate(
    total_edge_open = .data$game_total - .data$open_total,
    total_edge_close = .data$game_total - .data$close_total,
    margin_edge_open = .data$home_margin + .data$open_home_line,
    margin_edge_close = .data$home_margin + .data$close_home_line
  ) |>
  dplyr::filter(
    is.finite(.data$total_edge_open), is.finite(.data$total_edge_close),
    is.finite(.data$margin_edge_open), is.finite(.data$margin_edge_close)
  )

cat("Games with an early and a late line:", nrow(joined), "\n")
cat("Median hours out, early line:",
    round(stats::median(joined$open_lead_hours), 1), "\n")
cat("Median hours out, late line:",
    round(stats::median(joined$close_lead_hours), 1), "\n\n")

agreement <- function(open, close, threshold, home_only = FALSE) {
  q_open <- if (home_only) open >= threshold else abs(open) >= threshold
  q_close <- if (home_only) close >= threshold else abs(close) >= threshold
  same_side <- if (home_only) TRUE else sign(open) == sign(close)
  tibble::tibble(
    qualified_early = sum(q_open),
    qualified_late = sum(q_close),
    still_qualifies = sum(q_open & q_close & same_side),
    retention = sum(q_open & q_close & same_side) / max(1, sum(q_open)),
    flipped_side = sum(q_open & q_close & !same_side),
    new_at_close = sum(!q_open & q_close)
  )
}

cat("=== Totals, 5-point rule ===\n")
print(as.data.frame(
  agreement(joined$total_edge_open, joined$total_edge_close, 5)
), digits = 4)

cat("\n=== Home spreads, 6-point rule ===\n")
print(as.data.frame(
  agreement(joined$margin_edge_open, joined$margin_edge_close, 6,
            home_only = TRUE)
), digits = 4)

cat("\n=== How far the edge itself drifts ===\n")
print(as.data.frame(tibble::tibble(
  market = c("total", "margin"),
  mean_abs_drift = c(
    mean(abs(joined$total_edge_close - joined$total_edge_open)),
    mean(abs(joined$margin_edge_close - joined$margin_edge_open))
  ),
  pct_drift_under_1pt = c(
    mean(abs(joined$total_edge_close - joined$total_edge_open) < 1),
    mean(abs(joined$margin_edge_close - joined$margin_edge_open) < 1)
  ),
  pct_drift_over_2pt = c(
    mean(abs(joined$total_edge_close - joined$total_edge_open) >= 2),
    mean(abs(joined$margin_edge_close - joined$margin_edge_open) >= 2)
  )
)), digits = 4)

# What the early-line bets actually returned, since retention alone does not
# say whether betting early is profitable.
schedule <- readRDS("data/raw/schedules.rds") |>
  dplyr::filter(.data$season == 2025, .data$game_type == "REG") |>
  dplyr::transmute(
    .data$game_id,
    margin = as.numeric(.data$home_score) - as.numeric(.data$away_score),
    total = as.numeric(.data$home_score) + as.numeric(.data$away_score)
  )

graded <- joined |>
  dplyr::inner_join(schedule, by = "game_id")

grade_totals <- function(d, edge_col, line_col, label) {
  b <- d |> dplyr::filter(abs(.data[[edge_col]]) >= 5)
  if (!nrow(b)) return(tibble::tibble())
  result <- dplyr::case_when(
    b$total == b[[line_col]] ~ 0,
    (b$total > b[[line_col]]) == (b[[edge_col]] > 0) ~ 1,
    TRUE ~ -1
  )
  tibble::tibble(
    priced = label, bets = nrow(b), wins = sum(result > 0),
    roi = sum(dplyr::if_else(result > 0, 100 / 110, dplyr::if_else(result < 0, -1, 0))) /
      nrow(b)
  )
}
grade_spreads <- function(d, edge_col, line_col, label) {
  b <- d |> dplyr::filter(.data[[edge_col]] >= 6)
  if (!nrow(b)) return(tibble::tibble())
  result <- dplyr::case_when(
    b$margin + b[[line_col]] == 0 ~ 0,
    b$margin + b[[line_col]] > 0 ~ 1,
    TRUE ~ -1
  )
  tibble::tibble(
    priced = label, bets = nrow(b), wins = sum(result > 0),
    roi = sum(dplyr::if_else(result > 0, 100 / 110, dplyr::if_else(result < 0, -1, 0))) /
      nrow(b)
  )
}

cat("\n=== 2025 results, early line versus late line ===\n")
cat("Totals:\n")
print(as.data.frame(dplyr::bind_rows(
  grade_totals(graded, "total_edge_open", "open_total", "early (Tue)"),
  grade_totals(graded, "total_edge_close", "close_total", "late (~kickoff)")
)), digits = 4)
cat("Home spreads:\n")
print(as.data.frame(dplyr::bind_rows(
  grade_spreads(graded, "margin_edge_open", "open_home_line", "early (Tue)"),
  grade_spreads(graded, "margin_edge_close", "close_home_line", "late (~kickoff)")
)), digits = 4)
