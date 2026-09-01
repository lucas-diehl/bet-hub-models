source("R/utilities.R")
assert_packages()
ensure_directories()

# Re-runs the player prop test using the upgraded component models.
#
# The role and opportunity features measurably improved the projections. The
# question this answers is whether that improvement is enough to beat a posted
# line, which is a different and much harder bar than beating a lagged mean.
# Both arms are graded against the same cached odds, so no credits are spent.

ab <- readRDS("data/processed/player_feature_ab_predictions.rds")
graded <- readr::read_csv(
  "outputs/player_prop_graded_bets.csv", show_col_types = FALSE
) |>
  dplyr::select(
    "game_id", "season", "week", "target", "player_id", "player",
    "consensus_line", "american_odds_over", "american_odds_under",
    "actual", "outcome"
  )

markets <- c("receiving_yards", "receptions", "rushing_yards")
decimal_payout <- function(american) {
  dplyr::if_else(american > 0, american / 100, 100 / abs(american))
}

bin_count <- 5L
probability_over <- function(target_name, level_bin, gap, pool) {
  r <- pool$residual[pool$target == target_name & pool$level_bin == level_bin]
  if (length(r) < 100L) r <- pool$residual[pool$target == target_name]
  if (!length(r)) return(NA_real_)
  mean(r > gap)
}

price_arm <- function(arm_name) {
  pool_all <- ab |>
    dplyr::filter(.data$arm == arm_name, .data$target %in% markets) |>
    dplyr::mutate(residual = .data$actual - .data$prediction)

  purrr::map_dfr(c(2024, 2025), function(test_season) {
    train <- dplyr::filter(pool_all, .data$season < test_season)
    if (!nrow(train)) return(tibble::tibble())

    breaks <- split(train, train$target) |>
      purrr::map(function(d) {
        b <- stats::quantile(
          d$prediction, probs = seq(0, 1, length.out = bin_count + 1L),
          na.rm = TRUE
        )
        b[[1]] <- -Inf; b[[length(b)]] <- Inf
        unique(b)
      })
    bin_of <- function(d) {
      d |>
        dplyr::group_by(.data$target) |>
        dplyr::mutate(level_bin = as.integer(cut(
          .data$prediction, breaks = breaks[[dplyr::first(.data$target)]],
          include.lowest = TRUE, labels = FALSE
        ))) |>
        dplyr::ungroup() |>
        dplyr::mutate(level_bin = dplyr::coalesce(.data$level_bin, 1L))
    }
    train_binned <- bin_of(train)

    test <- pool_all |>
      dplyr::filter(.data$season == test_season) |>
      dplyr::select("target", "season", "game_id", "player_id", "prediction") |>
      dplyr::inner_join(
        graded, by = c("game_id", "season", "target", "player_id")
      ) |>
      bin_of()
    if (!nrow(test)) return(tibble::tibble())

    test |>
      dplyr::mutate(
        arm = arm_name,
        gap = .data$consensus_line - .data$prediction,
        p_over = purrr::pmap_dbl(
          list(.data$target, .data$level_bin, .data$gap),
          function(t, b, g) probability_over(t, b, g, train_binned)
        ),
        ev_over = .data$p_over * decimal_payout(.data$american_odds_over) -
          (1 - .data$p_over),
        ev_under = (1 - .data$p_over) *
          decimal_payout(.data$american_odds_under) - .data$p_over,
        side = dplyr::if_else(.data$ev_over >= .data$ev_under, "over", "under"),
        ev = pmax(.data$ev_over, .data$ev_under),
        price = dplyr::if_else(
          .data$side == "over",
          .data$american_odds_over, .data$american_odds_under
        ),
        result = dplyr::case_when(
          .data$outcome == "push" ~ 0,
          .data$outcome == .data$side ~ 1,
          TRUE ~ -1
        ),
        profit = dplyr::case_when(
          .data$result > 0 ~ decimal_payout(.data$price),
          .data$result < 0 ~ -1,
          TRUE ~ 0
        )
      ) |>
      dplyr::filter(is.finite(.data$ev))
  })
}

priced <- dplyr::bind_rows(price_arm("baseline"), price_arm("full"))
readr::write_csv(priced, "outputs/player_prop_upgraded_bets.csv")

cat("=== Calibration: predicted vs actual over rate, by arm ===\n")
print(as.data.frame(
  priced |>
    dplyr::mutate(band = cut(.data$p_over, seq(0, 1, by = 0.2),
                             include.lowest = TRUE)) |>
    dplyr::group_by(.data$arm, .data$band) |>
    dplyr::summarise(
      n = dplyr::n(),
      predicted = mean(.data$p_over),
      actual = mean(.data$outcome == "over"),
      .groups = "drop"
    )
), digits = 4)

summarise_bets <- function(d) {
  decisions <- sum(d$result != 0)
  tibble::tibble(
    bets = nrow(d),
    win_rate = if (decisions) sum(d$result > 0) / decisions else NA_real_,
    profit_units = sum(d$profit),
    roi = if (nrow(d)) sum(d$profit) / nrow(d) else NA_real_
  )
}

cat("\n=== Positive-EV bets by arm and season ===\n")
print(as.data.frame(
  priced |>
    dplyr::filter(.data$ev > 0) |>
    dplyr::group_by(.data$arm, .data$season) |>
    dplyr::group_modify(~ summarise_bets(.x)) |>
    dplyr::ungroup()
), digits = 4)

cat("\n=== Positive-EV bets by arm and market, pooled ===\n")
print(as.data.frame(
  priced |>
    dplyr::filter(.data$ev > 0) |>
    dplyr::group_by(.data$arm, .data$target) |>
    dplyr::group_modify(~ summarise_bets(.x)) |>
    dplyr::ungroup()
), digits = 4)

ev_grid <- seq(0, 0.20, by = 0.02)
curves <- purrr::map_dfr(unique(priced$arm), function(a) {
  purrr::map_dfr(ev_grid, function(threshold) {
    purrr::map_dfr(c(2024, 2025), function(s) {
      d <- priced |>
        dplyr::filter(.data$arm == a, .data$season == s, .data$ev >= threshold)
      if (!nrow(d)) return(tibble::tibble())
      dplyr::bind_cols(
        tibble::tibble(arm = a, threshold = threshold, season = s),
        summarise_bets(d)
      )
    })
  })
})
readr::write_csv(curves, "outputs/player_prop_upgraded_curves.csv")

cat("\n=== EV threshold selected on 2024, applied to 2025 ===\n")
forward <- curves |>
  dplyr::filter(.data$season == 2024, .data$bets >= 40) |>
  dplyr::group_by(.data$arm) |>
  dplyr::slice_max(.data$roi, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select("arm", "threshold", selection_roi = "roi",
                selection_bets = "bets") |>
  dplyr::inner_join(
    curves |> dplyr::filter(.data$season == 2025), by = c("arm", "threshold")
  )
print(as.data.frame(forward), digits = 4)
