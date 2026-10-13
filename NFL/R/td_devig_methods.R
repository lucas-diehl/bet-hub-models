## ---------------------------------------------------------------------------
## R/td_devig_methods.R
##
## Priority 3 (external audit §3.2). Four de-vig methods for a 2-outcome
## market (multiplicative, additive, power, Shin), plus hold-by-price-band
## estimation from genuinely two-sided anytime-TD rows.
##
## STEP 1 FINDING, documented here (not just in the report): the CURRENT
## method computes NO de-vig at all.
##   - R/odds_api.R's flatten_*_payload() functions compute `implied_probability`
##     as the textbook American-odds-to-probability conversion (no vig
##     removal): if (odds<0) abs(odds)/(abs(odds)+100) else 100/(odds+100).
##   - R/touchdown_features.R's build_td_prop_board() sets
##     `consensus_probability = median(implied_probability across books)` --
##     a robustness-to-outlier-books choice (median vs mean), not a de-vig
##     method. The raw, vig-inflated number IS the "market probability" used
##     everywhere today (fit_market_calibrator(), td_scorecard(), etc).
##   - ONE-SIDED HANDLING: build_td_prop_board() explicitly filters
##     `outcome == "Yes"` (R/touchdown_features.R line ~327) before any
##     aggregation. Confirmed empirically against the raw historical cache
##     (data/processed/anytime_td_odds_early_2023_2025.csv, 161,689 rows):
##     1,462 rows (0.9%) carry outcome == "No", ALL from a single book
##     (barstool), and EVERY one of them pairs with a matching "Yes" row for
##     the same (game_id, player, bookmaker_key, event_snapshot) -- 1,462
##     genuine two-sided anytime-TD markets exist in this project's own data
##     and are currently discarded without ever being used for anything,
##     including vig estimation. This is better than the audit anticipated
##     (it expected needing a different market as the hold-estimation proxy)
##     -- real same-market two-sided data turned out to be sitting unused.
## ---------------------------------------------------------------------------

## ===========================================================================
## THE FOUR METHODS, for a single 2-outcome pair (p1, p2), both RAW implied
## probabilities (p1 + p2 = B > 1 is the overround/hold).
## ===========================================================================

td_devig_multiplicative <- function(p1, p2) {
  b <- p1 + p2
  list(fair1 = p1 / b, fair2 = p2 / b)
}

td_devig_additive <- function(p1, p2) {
  b <- p1 + p2
  half_over <- (b - 1) / 2
  list(fair1 = pmax(0, pmin(1, p1 - half_over)), fair2 = pmax(0, pmin(1, p2 - half_over)))
}

## Power method: find k such that p1^k + p2^k = 1 (k >= 1 compresses the
## favorite-longshot gap vs multiplicative; k solved per-row via uniroot).
td_devig_power_pair <- function(p1, p2) {
  if (!is.finite(p1) || !is.finite(p2) || p1 <= 0 || p2 <= 0 || p1 + p2 <= 1) {
    return(c(fair1 = NA_real_, fair2 = NA_real_))
  }
  f <- function(k) p1^k + p2^k - 1
  if (f(1) <= 0) return(c(fair1 = p1, fair2 = p2))  # already no overround
  k <- tryCatch(stats::uniroot(f, c(1, 50))$root, error = function(e) NA_real_)
  if (!is.finite(k)) return(c(fair1 = NA_real_, fair2 = NA_real_))
  c(fair1 = p1^k, fair2 = p2^k)
}
td_devig_power <- function(p1, p2) {
  out <- mapply(td_devig_power_pair, p1, p2)
  list(fair1 = out["fair1", ], fair2 = out["fair2", ])
}

## Shin's method (favorite-longshot / insider-trading model), 2-outcome case.
## z solves sum_i [(sqrt(z^2 + 4(1-z)*p_i^2/B) - z) / (2(1-z))] = 1, via
## root-finding (avoids transcription risk from the closed-form variants
## cited across different papers -- this numeric form is the one most
## commonly implemented, e.g. in the `implied` R package, and generalizes
## cleanly to n outcomes though only 2 are needed here).
td_devig_shin_pair <- function(p1, p2) {
  if (!is.finite(p1) || !is.finite(p2) || p1 <= 0 || p2 <= 0 || p1 + p2 <= 1) {
    return(c(fair1 = NA_real_, fair2 = NA_real_))
  }
  b <- p1 + p2
  pi_of_z <- function(z, p) (sqrt(z^2 + 4 * (1 - z) * p^2 / b) - z) / (2 * (1 - z))
  g <- function(z) pi_of_z(z, p1) + pi_of_z(z, p2) - 1
  if (!is.finite(g(1e-6)) || !is.finite(g(0.999))) return(c(fair1 = NA_real_, fair2 = NA_real_))
  z <- tryCatch(stats::uniroot(g, c(1e-6, 0.999))$root, error = function(e) NA_real_)
  if (!is.finite(z)) return(c(fair1 = NA_real_, fair2 = NA_real_))
  c(fair1 = pi_of_z(z, p1), fair2 = pi_of_z(z, p2))
}
td_devig_shin <- function(p1, p2) {
  out <- mapply(td_devig_shin_pair, p1, p2)
  list(fair1 = out["fair1", ], fair2 = out["fair2", ])
}

## ===========================================================================
## PRICE BANDS -- bucketed on the Yes-side American odds, matching the
## +100..+700 range already flagged for longshot overpricing in earlier work
## today, with explicit bands outside that range too.
## ===========================================================================
td_price_band <- function(american_odds) {
  cut(
    american_odds,
    breaks = c(-Inf, -100, 100, 150, 200, 300, 400, 500, 700, 1000, Inf),
    labels = c("<=-100", "-100..100", "100..150", "150..200", "200..300",
              "300..400", "400..500", "500..700", "700..1000", ">1000"),
    right = FALSE
  )
}

## ===========================================================================
## HOLD-BY-BAND ESTIMATION from genuinely paired two-sided rows.
## ===========================================================================
td_estimate_hold_by_band <- function(pairs_df) {
  pairs_df |>
    dplyr::mutate(
      band = td_price_band(.data$american_odds_yes),
      hold = .data$p_yes + .data$p_no - 1
    ) |>
    dplyr::group_by(.data$band) |>
    dplyr::summarise(n = dplyr::n(), mean_hold = mean(.data$hold), sd_hold = stats::sd(.data$hold),
                     .groups = "drop")
}

## ===========================================================================
## ONE-SIDED DE-VIG: reconstruct an estimated p_no from the band's estimated
## hold, then apply one of the 4 methods to (p_yes_observed, p_no_estimated).
## ===========================================================================
td_devig_one_sided <- function(p_yes, american_odds, hold_table, method = c("multiplicative", "additive", "power", "shin")) {
  method <- match.arg(method)
  band <- td_price_band(american_odds)
  hold <- hold_table$mean_hold[match(band, hold_table$band)]
  hold <- dplyr::coalesce(hold, mean(hold_table$mean_hold, na.rm = TRUE))  # fallback: overall mean hold
  p_no_est <- pmax(1e-6, 1 + hold - p_yes)
  switch(
    method,
    multiplicative = td_devig_multiplicative(p_yes, p_no_est)$fair1,
    additive = td_devig_additive(p_yes, p_no_est)$fair1,
    power = td_devig_power(p_yes, p_no_est)$fair1,
    shin = td_devig_shin(p_yes, p_no_est)$fair1
  )
}
