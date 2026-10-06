source("R/utilities.R")
source("R/feature_registry.R")
source("R/models.R")
source("R/backtest.R")
assert_packages()
ensure_directories()
cfg <- read_config()

# P2 section 7.
#
# Adding injury features to the model made it worse, and that result stands.
# But it answered the wrong question. The model does not know about injuries, so
# the question that matters operationally is not "do injuries improve the
# prediction" but "does the model's edge survive in games where it is most
# ignorant". Those are different: a feature can fail to help a fit and still
# mark out the subset where the fit should not be trusted.
#
# If the edge is flat across injury burden, the omission is benign and the arm
# can be bet as-is. If it collapses in the top burden quartile, that is a filter
# worth having, and it is a filter the model can apply without ever learning
# what an injury does - which is the version least likely to overfit.

features <- readRDS("data/processed/game_features.rds")
injuries <- readRDS("data/processed/team_injury_features.rds") |>
  dplyr::mutate(team = normalize_team(.data$team))
test_seasons <- cfg$backtest$start_season:cfg$backtest$end_season

walk_forward_one <- function(model, target, seed) {
  purrr::map_dfr(test_seasons, function(s) {
    train <- dplyr::filter(features, .data$season < s)
    test <- dplyr::filter(features, .data$season == s)
    if (!nrow(train) || !nrow(test)) return(tibble::tibble())
    set.seed(seed)
    prediction <- fit_predict_model(
      model, train, test, target, cfg, seed = seed
    )
    tibble::tibble(
      season = s, game_id = test$game_id, week = test$week,
      home_team = normalize_team(test$home_team),
      away_team = normalize_team(test$away_team),
      truth = test[[target]], prediction = prediction,
      home_line = test$home_line, total_line = test$total_line
    )
  })
}

spread_path <- "data/processed/spread_seed_averaged_predictions.rds"
spread_pred <- walk_forward_one("random_forest", "home_margin", cfg$backtest$seed)
if (file.exists(spread_path)) {
  averaged <- readRDS(spread_path) |>
    dplyr::select("game_id", averaged_prediction = "prediction")
  spread_pred <- spread_pred |>
    dplyr::left_join(averaged, by = "game_id") |>
    dplyr::mutate(prediction = dplyr::coalesce(.data$averaged_prediction,
                                               .data$prediction)) |>
    dplyr::select(-"averaged_prediction")
  cat("Spread predictions: seed-averaged\n")
} else {
  cat("Spread predictions: single seed\n")
}
total_pred <- walk_forward_one("forward_linear", "game_total", cfg$backtest$seed)

attach_injuries <- function(pred) {
  pred |>
    dplyr::left_join(
      injuries |> dplyr::select("season", "week", team = "team",
                                home_burden = "inj_burden",
                                home_qb_out = "inj_qb_out",
                                home_burden_off = "inj_burden_off"),
      by = c("season", "week", "home_team" = "team")
    ) |>
    dplyr::left_join(
      injuries |> dplyr::select("season", "week", team = "team",
                                away_burden = "inj_burden",
                                away_qb_out = "inj_qb_out",
                                away_burden_off = "inj_burden_off"),
      by = c("season", "week", "away_team" = "team")
    ) |>
    dplyr::mutate(
      total_burden = .data$home_burden + .data$away_burden,
      # Positive means the home side is the more injured of the two, which is
      # the direction that should hurt a home-only spread rule if it hurts
      # anything.
      burden_gap = .data$home_burden - .data$away_burden,
      any_qb_out = as.integer(
        dplyr::coalesce(.data$home_qb_out, 0) > 0 |
          dplyr::coalesce(.data$away_qb_out, 0) > 0
      )
    )
}

grade_bets <- function(pred, market, threshold, home_only) {
  d <- pred |>
    dplyr::mutate(
      market_value = if (market == "total") .data$total_line else -.data$home_line,
      edge = .data$prediction - .data$market_value
    ) |>
    dplyr::filter(abs(.data$edge) >= threshold)
  if (home_only) d <- dplyr::filter(d, .data$edge > 0)
  if (!nrow(d)) return(tibble::tibble())
  d |>
    dplyr::mutate(
      market = market,
      result = dplyr::if_else(
        .data$edge > 0,
        signed_result(.data$truth - .data$market_value),
        signed_result(.data$market_value - .data$truth)
      ),
      profit = american_profit(.data$result, cfg$backtest$american_odds)
    )
}

bets <- dplyr::bind_rows(
  grade_bets(attach_injuries(spread_pred), "spread", 6.0, TRUE),
  grade_bets(attach_injuries(total_pred), "total", 5.0, FALSE)
) |>
  dplyr::filter(!is.na(.data$total_burden))

cat("Qualifying bets with injury data:", nrow(bets), "\n\n")

boot_roi <- function(profit, n = 4000L, seed = 20260901L) {
  if (length(profit) < 10) return(c(NA_real_, NA_real_))
  set.seed(seed)
  draws <- vapply(seq_len(n),
                  function(i) mean(sample(profit, length(profit), replace = TRUE)),
                  numeric(1))
  stats::quantile(draws, c(0.025, 0.975))
}

by_quartile <- function(d, column, label) {
  d <- d |>
    dplyr::mutate(quartile = dplyr::ntile(.data[[column]], 4))
  out <- d |>
    dplyr::group_by(.data$quartile) |>
    dplyr::summarise(
      bets = dplyr::n(),
      min_value = min(.data[[column]]), max_value = max(.data[[column]]),
      win_rate = sum(.data$result > 0) / max(1L, sum(.data$result != 0)),
      roi = mean(.data$profit),
      .groups = "drop"
    )
  intervals <- d |>
    dplyr::group_by(.data$quartile) |>
    dplyr::group_map(~ boot_roi(.x$profit))
  out$ci_lo <- vapply(intervals, function(x) x[[1]], numeric(1))
  out$ci_hi <- vapply(intervals, function(x) x[[2]], numeric(1))
  cat("=== ", label, " ===\n", sep = "")
  print(as.data.frame(out), digits = 4, row.names = FALSE)
  cat("\n")
  dplyr::mutate(out, measure = label)
}

results <- dplyr::bind_rows(
  by_quartile(bets, "total_burden", "Combined injury burden, both teams"),
  by_quartile(bets, "burden_gap", "Home minus away burden"),
  by_quartile(dplyr::filter(bets, .data$market == "spread"), "total_burden",
              "Combined burden, spreads only"),
  by_quartile(dplyr::filter(bets, .data$market == "total"), "total_burden",
              "Combined burden, totals only")
)

cat("=== Games with a quarterback out ===\n")
print(as.data.frame(
  bets |>
    dplyr::group_by(.data$any_qb_out) |>
    dplyr::summarise(
      bets = dplyr::n(),
      win_rate = sum(.data$result > 0) / max(1L, sum(.data$result != 0)),
      roi = mean(.data$profit), .groups = "drop"
    )
), digits = 4, row.names = FALSE)

# A monotone decline is the pattern that would justify a filter. A single low
# quartile with a wide interval is the pattern of a subgroup found by looking,
# and four quartiles across two markets is already eight chances to find one.
top <- dplyr::filter(results, .data$measure == "Combined injury burden, both teams")
if (nrow(top) == 4) {
  rho <- stats::cor(top$quartile, top$roi, method = "spearman")
  cat(sprintf(
    "\nRank correlation between burden quartile and ROI: %+.2f\n", rho
  ))
  cat("A filter is justified only by a monotone decline whose bottom quartile",
      "\ninterval excludes the top quartile's point estimate. Anything less is",
      "\none subgroup out of eight tested.\n")
}

readr::write_csv(results, "outputs/injury_burden_conditioning.csv")

# CLV by burden cannot be computed on this record: the archived line is itself
# the closing number, so there is nothing to compare it against. It becomes
# available once scripts/58 has captured near-kickoff snapshots across a live
# season, and belongs in the weekly grading rather than here.
cat("\nCLV by burden quartile needs live captured closes; see scripts/58.\n")
