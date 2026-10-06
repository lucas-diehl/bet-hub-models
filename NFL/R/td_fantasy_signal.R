## ---------------------------------------------------------------------------
## R/td_fantasy_signal.R
##
## The fantasy projection model's own touchdown expectation, exposed as a
## probability the touchdown model can consume.
##
## WHY. Measured on 2023-2025 walk-forward (15,559 joined player-games), the
## fantasy model's TD components carry MORE touchdown information than the TD
## model's own fundamental:
##
##   anytime_td ~ td_fundamental + market + fantasy_td
##     td_fundamental  -0.1457  (p = 0.017)   <- NEGATIVE once fantasy is present
##     market          +0.6480
##     fantasy_td      +0.6579  (p < 1e-16)
##
##   2025 holdout Brier 0.145390 -> 0.143969, gain +0.00142,
##   week-block CI [+0.00061, +0.00220], P(gain<=0) = 0.000
##
## That gain is ~22x the TE-specific blend and ~24x the TD model's entire
## honest edge over the market. The reason is not subtle: the fantasy model has
## NGS tracking, QB-receiver pair chemistry, snap share and variance modelling,
## and is under active development, while the TD fundamental is not.
##
## Per position the fantasy signal is strongest exactly where the TD model is
## weakest - at TE it nearly replaces the market price (fantasy +0.996 vs
## market +0.074).
##
## CONVERSION. The fantasy model predicts EXPECTED rushing and receiving TDs.
## Anytime-TD is P(at least one), so the two expectations are summed and pushed
## through a Poisson tail: P = 1 - exp(-lambda). Poisson is the right family
## here - it is what the fantasy model's own TD targets are fit with
## (count:poisson, see fantasy_target_specifications()).
## ---------------------------------------------------------------------------

td_fantasy_prob_from_lambda <- function(rush_td, rec_td) {
  lambda <- pmax(
    dplyr::coalesce(as.numeric(rush_td), 0) +
      dplyr::coalesce(as.numeric(rec_td), 0),
    1e-6
  )
  pmin(pmax(1 - exp(-lambda), 1e-6), 1 - 1e-6)
}

## HISTORICAL: from the fantasy walk-forward predictions. Those are genuinely
## out-of-sample per season (each fold trains on season < test_season), and the
## `prediction` column is PRE salary-stack, so no market information leaks in
## through DK pricing.
td_fantasy_signal_history <- function(
  path = "data/processed/fantasy_prop_walk_forward.rds"
) {
  if (!file.exists(path)) return(NULL)
  fw <- tryCatch(readRDS(path)$predictions, error = function(e) NULL)
  if (is.null(fw) || !nrow(fw)) return(NULL)
  need <- c("game_id", "player_id", "target", "prediction")
  if (!all(need %in% names(fw))) return(NULL)

  wide <- fw |>
    dplyr::filter(.data$target %in% c("rushing_tds", "receiving_tds")) |>
    dplyr::select("game_id", "player_id", "target", "prediction") |>
    tidyr::pivot_wider(
      names_from = "target", values_from = "prediction",
      values_fn = mean, values_fill = 0
    )
  ## A fold that produced only one of the two targets would otherwise drop the
  ## missing column and error below.
  for (cc in c("rushing_tds", "receiving_tds")) {
    if (!cc %in% names(wide)) wide[[cc]] <- 0
  }
  wide |>
    dplyr::mutate(
      fantasy_p_td = td_fantasy_prob_from_lambda(
        .data$rushing_tds, .data$receiving_tds
      )
    ) |>
    dplyr::select("game_id", "player_id", "fantasy_p_td")
}

## LIVE: from the current week's projection board, which carries the same
## components as `projected_rushing_tds` / `projected_receiving_tds`.
##
## STALENESS. On the daily job scripts/15 runs BEFORE scripts/17, because
## scripts/17 needs the box scores that scripts/15 --refresh-stats pulls. So the
## board this reads is normally the PREVIOUS run's - at most one day old on a
## daily cadence. That is acceptable for a projection whose inputs move weekly,
## but the age is logged on every call rather than hidden, and a board older
## than `max_age_days` is refused outright so a dead scheduler cannot quietly
## feed week-old numbers into live prices.
td_fantasy_signal_live <- function(
  path = "outputs/fantasy_prop_2026_latest_projections.csv",
  max_age_days = 8
) {
  if (!file.exists(path)) {
    message("TD fantasy signal: no ", path, "; falling back to market+fundamental only.")
    return(NULL)
  }
  age <- as.numeric(difftime(Sys.time(), file.mtime(path), units = "days"))
  if (age > max_age_days) {
    message(sprintf(
      "TD fantasy signal: %s is %.1f days old (limit %d) - REFUSED, using market+fundamental only.",
      path, age, max_age_days))
    return(NULL)
  }
  d <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE),
    error = function(e) NULL
  )
  if (is.null(d) || !nrow(d)) return(NULL)
  need <- c("player_id", "projected_rushing_tds", "projected_receiving_tds")
  if (!all(need %in% names(d))) {
    message("TD fantasy signal: ", path, " lacks projected TD components; skipped.")
    return(NULL)
  }
  message(sprintf("TD fantasy signal: using %s (%.1f days old, %d players).",
                  basename(path), age, nrow(d)))
  d |>
    dplyr::transmute(
      player_id = as.character(.data$player_id),
      fantasy_p_td = td_fantasy_prob_from_lambda(
        .data$projected_rushing_tds, .data$projected_receiving_tds
      )
    ) |>
    dplyr::filter(!is.na(.data$player_id)) |>
    dplyr::distinct(.data$player_id, .keep_all = TRUE)
}
