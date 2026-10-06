source("R/utilities.R")
assert_packages()
ensure_directories()

# P2 section 6, three parts.
#
# 1. Re-grade the touchdown tiers at the CONSENSUS price rather than the best
#    price across the permitted books. Best-of-N is a real number a bettor can
#    take, but it flatters the strategy: taking the highest of four quotes means
#    systematically selecting the book whose number is most wrong, and some of
#    the measured "edge" is that selection rather than the model. The consensus
#    is the pessimistic bound. The truth is between them, and the gap is the
#    honest measure of how much of this strategy is line shopping.
#
# 2. Suppress the 2025 figures. They rest on 23 core bets, and a +51.7% headline
#    on 23 bets is not a result.
#
# 3. Paired bootstrap of model Brier against market Brier on the same rows, so
#    "the model beats the market" is a measured claim rather than an assumed one.

preds <- readr::read_csv("outputs/td_walk_forward_predictions.csv",
                         show_col_types = FALSE) |>
  dplyr::select(
    "game_id", "season", "week", "player", "position", "anytime_td",
    "best_american_odds", "best_book", "best_implied_probability",
    "consensus_probability", "books_available", "model_probability",
    "total_line"
  ) |>
  dplyr::filter(!is.na(.data$anytime_td), !is.na(.data$model_probability))

bets <- readr::read_csv("outputs/td_strategy_bets.csv", show_col_types = FALSE) |>
  dplyr::select("game_id", "player", "season", "strategy_tier", "won",
                "flat_profit", "best_american_odds", "relative_edge")

cat("Prediction rows:", nrow(preds), "\n")
cat("Selected bets:", nrow(bets), "\n\n")

# --------------------------------------------------------------------------
# 1. Best price versus consensus price
# --------------------------------------------------------------------------

graded <- bets |>
  dplyr::inner_join(
    preds |> dplyr::select("game_id", "player", "consensus_probability",
                           "books_available", "anytime_td"),
    by = c("game_id", "player")
  ) |>
  dplyr::filter(!is.na(.data$consensus_probability),
                .data$consensus_probability > 0) |>
  dplyr::mutate(
    # A price is a probability with the vig left in. Payout at the consensus is
    # one over that probability, minus the stake.
    consensus_payout = 1 / .data$consensus_probability - 1,
    best_payout = dplyr::if_else(
      .data$best_american_odds > 0, .data$best_american_odds / 100,
      100 / abs(.data$best_american_odds)
    ),
    profit_best = dplyr::if_else(.data$anytime_td > 0, .data$best_payout, -1),
    profit_consensus = dplyr::if_else(
      .data$anytime_td > 0, .data$consensus_payout, -1
    )
  )

cat("Bets matched to a consensus price:", nrow(graded), "of", nrow(bets), "\n\n")

summarise_tier <- function(d, label) {
  tibble::tibble(
    scope = label, bets = nrow(d),
    hit_rate = mean(d$anytime_td > 0),
    mean_best_payout = mean(d$best_payout),
    mean_consensus_payout = mean(d$consensus_payout),
    roi_best = mean(d$profit_best),
    roi_consensus = mean(d$profit_consensus)
  )
}

by_tier <- graded |>
  dplyr::group_split(.data$strategy_tier) |>
  purrr::map_dfr(~ summarise_tier(.x, .x$strategy_tier[[1]]))

cat("=== Best-of-four price versus consensus price ===\n")
print(as.data.frame(by_tier), digits = 4, row.names = FALSE)

# --------------------------------------------------------------------------
# 2. Small-n suppression
# --------------------------------------------------------------------------

min_bets <- 100L
by_season <- graded |>
  dplyr::group_by(.data$strategy_tier, .data$season) |>
  dplyr::summarise(
    bets = dplyr::n(),
    roi_best = mean(.data$profit_best),
    roi_consensus = mean(.data$profit_consensus),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    reportable = .data$bets >= min_bets,
    roi_best = dplyr::if_else(.data$reportable, .data$roi_best, NA_real_),
    roi_consensus = dplyr::if_else(.data$reportable, .data$roi_consensus,
                                   NA_real_)
  )

cat("\n=== By season, figures below", min_bets, "bets suppressed ===\n")
print(as.data.frame(by_season), digits = 4, row.names = FALSE)
cat("\nSuppressed cells are not weak evidence, they are no evidence. The 2025\n")
cat("core-tier line in particular has been quoted at +51.7% on 23 bets.\n")

readr::write_csv(by_season, "outputs/td_consensus_by_season.csv")

# --------------------------------------------------------------------------
# 3. Paired Brier bootstrap, model against market
# --------------------------------------------------------------------------

# The market's quoted probabilities carry vig, so they sum above one and are
# biased high as forecasts. Comparing raw would hand the model a win it did not
# earn. A single multiplicative rescale removes the level bias without giving
# the market any shape advantage it does not already have - it is the most
# generous correction that is still one parameter.
scored <- preds |>
  dplyr::filter(!is.na(.data$consensus_probability),
                .data$consensus_probability > 0)
scale_factor <- mean(scored$anytime_td > 0) / mean(scored$consensus_probability)
scored <- scored |>
  dplyr::mutate(
    outcome = as.integer(.data$anytime_td > 0),
    market_devig = pmin(.data$consensus_probability * scale_factor, 0.999),
    brier_model = (.data$model_probability - .data$outcome)^2,
    brier_market_raw = (.data$consensus_probability - .data$outcome)^2,
    brier_market = (.data$market_devig - .data$outcome)^2
  )

cat("\n=== Brier scores on", nrow(scored), "player-games ===\n")
cat(sprintf("  Base rate                 : %.4f\n", mean(scored$outcome)))
cat(sprintf("  Mean market probability   : %.4f (vig scale %.3f)\n",
            mean(scored$consensus_probability), scale_factor))
cat(sprintf("  Mean model probability    : %.4f\n",
            mean(scored$model_probability)))
cat(sprintf("  Model Brier               : %.5f\n", mean(scored$brier_model)))
cat(sprintf("  Market Brier, de-vigged   : %.5f\n", mean(scored$brier_market)))
cat(sprintf("  Market Brier, raw quote   : %.5f\n",
            mean(scored$brier_market_raw)))

# Paired and clustered on the game: two players in the same game share a script,
# so treating them as independent draws would narrow the interval.
paired_bootstrap <- function(d, n = 4000L, seed = 20260902L) {
  games <- split(seq_len(nrow(d)), d$game_id)
  keys <- names(games)
  set.seed(seed)
  vapply(seq_len(n), function(i) {
    picked <- unlist(games[sample(keys, length(keys), replace = TRUE)],
                     use.names = FALSE)
    mean(d$brier_model[picked]) - mean(d$brier_market[picked])
  }, numeric(1))
}

draws <- paired_bootstrap(scored)
observed <- mean(scored$brier_model) - mean(scored$brier_market)

cat("\n=== Paired bootstrap, model Brier minus market Brier ===\n")
cat("Negative means the model is the better forecast.\n")
cat(sprintf("  Observed difference : %+.6f\n", observed))
cat(sprintf("  95%% interval        : %+.6f to %+.6f\n",
            stats::quantile(draws, 0.025), stats::quantile(draws, 0.975)))
cat(sprintf("  P(model better)     : %.4f\n", mean(draws < 0)))

verdict <- if (stats::quantile(draws, 0.975) < 0) {
  "The model is a better forecaster than the de-vigged market on this sample."
} else if (stats::quantile(draws, 0.025) > 0) {
  "The market is the better forecaster. Any ROI is coming from price selection."
} else {
  paste("Indistinguishable from the market as a forecaster. Any measured ROI",
        "\nhas to be explained by price selection rather than by better",
        "\nprobabilities, and that is a line-shopping edge, not a model edge.")
}
cat("\nVerdict:", verdict, "\n")

# --------------------------------------------------------------------------
# 4. The decisive test: does the model add anything to line shopping?
#
# The two results above point the same way. The measured edge lives entirely in
# taking the best of four quotes, and the model is a worse forecaster than the
# market it is betting against. That suggests the model is not finding
# mispriced players at all - it is acting as a noisy detector of books that have
# drifted off consensus, which is a job that can be done directly and without a
# model.
#
# So do it directly. Rank every candidate by how far the best available price
# sits above the consensus, with no model input whatsoever, take the same number
# of bets the model took, and grade at the same best price.
# --------------------------------------------------------------------------

shop <- scored |>
  dplyr::filter(!is.na(.data$best_implied_probability),
                .data$best_implied_probability > 0) |>
  dplyr::mutate(
    # How much cheaper the best quote is than the consensus, in implied
    # probability. Larger means a bigger outlier.
    shop_edge = .data$consensus_probability - .data$best_implied_probability,
    best_payout = 1 / .data$best_implied_probability - 1,
    profit = dplyr::if_else(.data$outcome > 0, .data$best_payout, -1)
  )

n_core <- sum(graded$strategy_tier == "core")
n_all <- nrow(graded)

compare <- purrr::map_dfr(
  list(c(label = "matched to core tier", n = n_core),
       c(label = "matched to all selected bets", n = n_all)),
  function(spec) {
    n <- as.integer(spec[["n"]])
    top <- shop |> dplyr::slice_max(.data$shop_edge, n = n, with_ties = FALSE)
    tibble::tibble(
      selection = paste("Pure line shopping,", spec[["label"]]),
      bets = nrow(top), hit_rate = mean(top$outcome > 0),
      roi = mean(top$profit)
    )
  }
)

model_rows <- tibble::tibble(
  selection = c("Model core tier", "Model, all selected bets"),
  bets = c(n_core, n_all),
  hit_rate = c(mean(graded$anytime_td[graded$strategy_tier == "core"] > 0),
               mean(graded$anytime_td > 0)),
  roi = c(mean(graded$profit_best[graded$strategy_tier == "core"]),
          mean(graded$profit_best))
)

cat("\n=== Does the model beat pure line shopping, at the same bet count? ===\n")
print(as.data.frame(dplyr::bind_rows(model_rows, compare)), digits = 4,
      row.names = FALSE)
cat("\nBoth are graded at the best of four books. The only difference is how\n")
cat("the bets were chosen: by the model, or by price dispersion alone.\n")

readr::write_csv(dplyr::bind_rows(model_rows, compare),
                 "outputs/td_model_vs_line_shopping.csv")

# --------------------------------------------------------------------------
# 5. How much of the best-price gap has to be captured for this to work?
#
# The strategy is positive at the best of four and deeply negative at the
# consensus, so the operative question is not "is there an edge" but "how much
# of the shopping gap survives contact with reality". Picks publish on Tuesday
# and are bet later, by which time the outlier quote may be gone.
#
# Grade the same bets at a payout partway between consensus and best.
# --------------------------------------------------------------------------

capture_curve <- purrr::map_dfr(seq(0, 1, by = 0.125), function(lambda) {
  core <- dplyr::filter(graded, .data$strategy_tier == "core") |>
    dplyr::mutate(
      payout = .data$consensus_payout +
        lambda * (.data$best_payout - .data$consensus_payout),
      profit = dplyr::if_else(.data$anytime_td > 0, .data$payout, -1)
    )
  tibble::tibble(
    capture = lambda, mean_payout = mean(core$payout), roi = mean(core$profit)
  )
})

cat("\n=== Core tier ROI by share of the best-price gap captured ===\n")
print(as.data.frame(capture_curve), digits = 4, row.names = FALSE)

crossing <- capture_curve$capture[which(capture_curve$roi > 0)]
cat(sprintf(
  "\nBreak-even sits between %.0f%% and %.0f%% capture of the gap between the\nconsensus price and the best of four.\n",
  100 * max(capture_curve$capture[capture_curve$roi <= 0]),
  100 * min(crossing)
))
cat("Below that, the strategy loses. This is the number to monitor, and it is\n")
cat("why min_price is on every published row.\n")

readr::write_csv(capture_curve, "outputs/td_price_capture_curve.csv")

readr::write_csv(
  tibble::tibble(
    metric = c("brier_model", "brier_market_devig", "brier_market_raw",
               "difference", "ci_lo", "ci_hi", "p_model_better"),
    value = c(mean(scored$brier_model), mean(scored$brier_market),
              mean(scored$brier_market_raw), observed,
              stats::quantile(draws, 0.025), stats::quantile(draws, 0.975),
              mean(draws < 0))
  ),
  "outputs/td_brier_paired_bootstrap.csv"
)
