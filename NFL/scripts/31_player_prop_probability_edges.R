source("R/utilities.R")
source("R/fantasy_prop_model.R")
assert_packages()
ensure_directories()

# Prices the player props as probabilities rather than as yards of line
# disagreement.
#
# scripts/30 showed the projections lose because a conditional mean is compared
# with a two-sided price: the model's overs fire too often and pay -120. The
# correction is to turn (projection, line) into P(actual > line) using the
# model's own residual distribution, then bet only when expected value against
# the actual offered price is positive.
#
# Leakage control: the residual distribution used to price a season is built
# only from earlier seasons. 2024 is priced from 2023 residuals; 2025 from
# 2023-2024. Bet thresholds are then selected on 2024 and applied to 2025.

graded <- readr::read_csv(
  "outputs/player_prop_graded_bets.csv", show_col_types = FALSE
)
walk_forward <- readRDS("data/processed/fantasy_prop_walk_forward.rds")

residual_pool <- walk_forward$predictions |>
  dplyr::filter(.data$target %in% unique(graded$target)) |>
  dplyr::transmute(
    .data$target, .data$season, .data$prediction, .data$actual,
    residual = .data$actual - .data$prediction
  )

# Residual spread grows with the projection, so the empirical distribution is
# taken within projection-level bins rather than pooled across a whole market.
bin_count <- 5L
add_bins <- function(d, breaks_lookup) {
  d |>
    dplyr::group_by(.data$target) |>
    dplyr::mutate(
      level_bin = as.integer(cut(
        .data$prediction,
        breaks = breaks_lookup[[dplyr::first(.data$target)]],
        include.lowest = TRUE, labels = FALSE
      ))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(level_bin = dplyr::coalesce(.data$level_bin, 1L))
}

probability_over <- function(target_name, level_bin, gap, pool) {
  # P(actual > line) = P(residual > line - projection)
  r <- pool$residual[pool$target == target_name & pool$level_bin == level_bin]
  if (length(r) < 100L) {
    r <- pool$residual[pool$target == target_name]
  }
  if (!length(r)) return(NA_real_)
  mean(r > gap)
}

decimal_payout <- function(american) {
  dplyr::if_else(american > 0, american / 100, 100 / abs(american))
}

priced <- purrr::map_dfr(c(2024, 2025), function(test_season) {
  train <- residual_pool |> dplyr::filter(.data$season < test_season)
  if (!nrow(train)) return(tibble::tibble())

  breaks_lookup <- split(train, train$target) |>
    purrr::map(function(d) {
      b <- stats::quantile(
        d$prediction, probs = seq(0, 1, length.out = bin_count + 1L),
        na.rm = TRUE
      )
      b[[1]] <- -Inf
      b[[length(b)]] <- Inf
      unique(b)
    })

  train_binned <- add_bins(train, breaks_lookup)
  test <- graded |>
    dplyr::filter(.data$season == test_season) |>
    add_bins(breaks_lookup)

  test |>
    dplyr::mutate(
      gap = .data$consensus_line - .data$prediction,
      p_over = purrr::pmap_dbl(
        list(.data$target, .data$level_bin, .data$gap),
        function(t, b, g) probability_over(t, b, g, train_binned)
      ),
      p_under = 1 - .data$p_over,
      payout_over = decimal_payout(.data$american_odds_over),
      payout_under = decimal_payout(.data$american_odds_under),
      ev_over = .data$p_over * .data$payout_over - (1 - .data$p_over),
      ev_under = .data$p_under * .data$payout_under - (1 - .data$p_under),
      prob_side = dplyr::if_else(
        .data$ev_over >= .data$ev_under, "over", "under"
      ),
      prob_ev = pmax(.data$ev_over, .data$ev_under),
      prob_price = dplyr::if_else(
        .data$prob_side == "over",
        .data$american_odds_over, .data$american_odds_under
      ),
      prob_result = dplyr::case_when(
        .data$outcome == "push" ~ 0,
        .data$outcome == .data$prob_side ~ 1,
        TRUE ~ -1
      ),
      prob_profit = dplyr::case_when(
        .data$prob_result > 0 ~ decimal_payout(.data$prob_price),
        .data$prob_result < 0 ~ -1,
        TRUE ~ 0
      )
    ) |>
    dplyr::filter(is.finite(.data$prob_ev))
})

readr::write_csv(priced, "outputs/player_prop_probability_bets.csv")

summarise_bets <- function(d) {
  decisions <- sum(d$prob_result != 0)
  tibble::tibble(
    bets = nrow(d),
    wins = sum(d$prob_result > 0),
    losses = sum(d$prob_result < 0),
    win_rate = if (decisions) sum(d$prob_result > 0) / decisions else NA_real_,
    profit_units = sum(d$prob_profit),
    roi = if (nrow(d)) sum(d$prob_profit) / nrow(d) else NA_real_
  )
}

cat("=== Calibration of the residual-implied probability ===\n")
print(as.data.frame(
  priced |>
    dplyr::mutate(band = cut(.data$p_over, seq(0, 1, by = 0.1),
                             include.lowest = TRUE)) |>
    dplyr::group_by(.data$band) |>
    dplyr::summarise(
      n = dplyr::n(),
      predicted_over = mean(.data$p_over),
      actual_over = mean(.data$outcome == "over"),
      .groups = "drop"
    )
), digits = 4)

ev_grid <- seq(0, 0.20, by = 0.01)
curves <- purrr::map_dfr(unique(priced$target), function(target_name) {
  purrr::map_dfr(ev_grid, function(threshold) {
    purrr::map_dfr(c(2024, 2025), function(s) {
      d <- priced |>
        dplyr::filter(
          .data$target == target_name, .data$season == s,
          .data$prob_ev >= threshold
        )
      if (!nrow(d)) return(tibble::tibble())
      dplyr::bind_cols(
        tibble::tibble(target = target_name, threshold = threshold, season = s),
        summarise_bets(d)
      )
    })
  })
})
readr::write_csv(curves, "outputs/player_prop_probability_curves.csv")

cat("\n=== Every positive-EV bet, by market and season ===\n")
print(as.data.frame(
  priced |>
    dplyr::filter(.data$prob_ev > 0) |>
    dplyr::group_by(.data$target, .data$season) |>
    dplyr::group_modify(~ summarise_bets(.x)) |>
    dplyr::ungroup()
), digits = 4)

minimum_bets <- 40L
forward <- curves |>
  dplyr::filter(.data$season == 2024, .data$bets >= minimum_bets) |>
  dplyr::group_by(.data$target) |>
  dplyr::slice_max(.data$roi, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(
    "target", "threshold",
    selection_bets = "bets", selection_roi = "roi"
  ) |>
  dplyr::inner_join(
    curves |> dplyr::filter(.data$season == 2025),
    by = c("target", "threshold")
  )
readr::write_csv(forward, "outputs/player_prop_probability_forward.csv")

cat("\n=== EV threshold selected on 2024, applied to 2025 ===\n")
if (nrow(forward)) print(as.data.frame(forward), digits = 4) else
  cat("Nothing cleared the minimum bet count.\n")

# Pooled across markets, which is where the volume is.
pooled <- purrr::map_dfr(ev_grid, function(threshold) {
  purrr::map_dfr(c(2024, 2025), function(s) {
    d <- priced |>
      dplyr::filter(.data$season == s, .data$prob_ev >= threshold)
    if (!nrow(d)) return(tibble::tibble())
    dplyr::bind_cols(
      tibble::tibble(threshold = threshold, season = s), summarise_bets(d)
    )
  })
})
readr::write_csv(pooled, "outputs/player_prop_probability_pooled.csv")
cat("\n=== Pooled across markets, by EV threshold ===\n")
print(as.data.frame(pooled |> dplyr::arrange(.data$threshold, .data$season)),
      digits = 4)
