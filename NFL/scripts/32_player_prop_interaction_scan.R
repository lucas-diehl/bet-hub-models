source("R/utilities.R")
assert_packages()
ensure_directories()

# Systematic interaction scan over the positive-EV player prop bets.
#
# The point of this script is as much to count the search as to run it. Slicing
# 11,500 bets enough ways will always surface profitable-looking cells, so every
# segment is measured on 2024 (discovery) and then on 2025 (validation), and the
# summary compares how many discovery-positive cells survived against how many
# would survive by coin flip. A scan that beats chance is interesting; one that
# matches it is noise dressed as a strategy.

priced <- readr::read_csv(
  "outputs/player_prop_probability_bets.csv", show_col_types = FALSE
) |>
  dplyr::filter(.data$prob_ev > 0)

features <- readRDS("data/processed/fantasy_prop_features.rds") |>
  dplyr::select("season", "player_id", "position") |>
  dplyr::distinct()

context <- readRDS("data/processed/td_game_context.rds")
context_cols <- intersect(
  c("game_id", "team", "is_home", "total_line", "team_spread"), names(context)
)
game_context <- context |>
  dplyr::select(dplyr::all_of(context_cols)) |>
  dplyr::group_by(.data$game_id) |>
  dplyr::summarise(
    total_line = if ("total_line" %in% context_cols) {
      stats::median(.data$total_line, na.rm = TRUE)
    } else NA_real_,
    .groups = "drop"
  )

bets <- priced |>
  dplyr::left_join(features, by = c("season", "player_id")) |>
  dplyr::left_join(game_context, by = "game_id") |>
  dplyr::mutate(
    position = dplyr::coalesce(.data$position, "UNK"),
    price_bucket = dplyr::case_when(
      .data$prob_price >= -105 ~ "price >= -105",
      .data$prob_price >= -115 ~ "price -106..-115",
      TRUE ~ "price < -115"
    ),
    week_bucket = dplyr::case_when(
      .data$week <= 6 ~ "weeks 1-6",
      .data$week <= 12 ~ "weeks 7-12",
      TRUE ~ "weeks 13-18"
    ),
    books_bucket = dplyr::if_else(.data$books >= 6, "6+ books", "2-5 books"),
    ev_bucket = dplyr::case_when(
      .data$prob_ev >= 0.20 ~ "ev 20%+",
      .data$prob_ev >= 0.10 ~ "ev 10-20%",
      TRUE ~ "ev 0-10%"
    ),
    total_bucket = dplyr::case_when(
      is.na(.data$total_line) ~ "total unknown",
      .data$total_line <= 44 ~ "total <= 44",
      .data$total_line <= 48 ~ "total 44-48",
      TRUE ~ "total > 48"
    ),
    line_bucket = dplyr::case_when(
      .data$level_bin <= 2L ~ "low line",
      .data$level_bin == 3L ~ "mid line",
      TRUE ~ "high line"
    )
  )

roi_of <- function(d) {
  if (!nrow(d)) return(c(bets = 0, roi = NA_real_, win_rate = NA_real_))
  decisions <- sum(d$prob_result != 0)
  c(
    bets = nrow(d),
    roi = sum(d$prob_profit) / nrow(d),
    win_rate = if (decisions) sum(d$prob_result > 0) / decisions else NA_real_
  )
}

dimensions <- c(
  "target", "prob_side", "position", "price_bucket", "week_bucket",
  "books_bucket", "ev_bucket", "total_bucket", "line_bucket"
)

# One-way cells, then every two-way combination of the same dimensions.
combos <- c(
  purrr::map(dimensions, ~ .x),
  purrr::map(utils::combn(dimensions, 2, simplify = FALSE), identity)
)

minimum_bets <- 40L
scan <- purrr::map_dfr(combos, function(dims) {
  bets |>
    dplyr::group_by(dplyr::across(dplyr::all_of(dims))) |>
    dplyr::group_modify(function(d, key) {
      disc <- roi_of(dplyr::filter(d, .data$season == 2024))
      val <- roi_of(dplyr::filter(d, .data$season == 2025))
      tibble::tibble(
        segment = paste(dims, collapse = " x "),
        discovery_bets = disc[["bets"]], discovery_roi = disc[["roi"]],
        validation_bets = val[["bets"]], validation_roi = val[["roi"]],
        validation_win_rate = val[["win_rate"]]
      )
    }) |>
    dplyr::ungroup() |>
    tidyr::unite("cell", dplyr::all_of(dims), sep = " | ")
}) |>
  dplyr::filter(
    .data$discovery_bets >= minimum_bets,
    .data$validation_bets >= minimum_bets
  )

readr::write_csv(scan, "outputs/player_prop_interaction_scan.csv")

positive_discovery <- scan |> dplyr::filter(.data$discovery_roi > 0)
survivors <- positive_discovery |> dplyr::filter(.data$validation_roi > 0)

cat("=== Search size ===\n")
cat("Segments tested (>=", minimum_bets, "bets both seasons):", nrow(scan), "\n")
cat("Profitable in 2024 discovery:", nrow(positive_discovery), "\n")
cat("Of those, still profitable in 2025:", nrow(survivors), "\n")
cat("Survival rate:",
    round(nrow(survivors) / max(1L, nrow(positive_discovery)), 4), "\n")
cat("Coin-flip expectation if no real edge: ~0.5\n")

if (nrow(positive_discovery)) {
  p_value <- stats::binom.test(
    nrow(survivors), nrow(positive_discovery), p = 0.5, alternative = "greater"
  )$p.value
  cat("One-sided binomial p vs chance:", signif(p_value, 4), "\n")
}

cat("\n=== Best surviving segments, by validation ROI ===\n")
if (nrow(survivors)) {
  print(as.data.frame(
    survivors |>
      dplyr::arrange(dplyr::desc(.data$validation_roi)) |>
      dplyr::select(
        "segment", "cell", "discovery_bets", "discovery_roi",
        "validation_bets", "validation_roi", "validation_win_rate"
      ) |>
      head(15)
  ), digits = 3)
} else {
  cat("None.\n")
}

cat("\n=== Aggregate of every surviving segment, pooled ===\n")
if (nrow(survivors)) {
  cat("Mean validation ROI across survivors:",
      round(mean(survivors$validation_roi), 4), "\n")
  cat("Mean validation ROI across ALL discovery-positive cells:",
      round(mean(positive_discovery$validation_roi), 4), "\n")
  cat("Mean validation ROI across ALL tested cells:",
      round(mean(scan$validation_roi, na.rm = TRUE), 4), "\n")
}
