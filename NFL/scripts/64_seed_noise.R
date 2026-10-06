source("R/utilities.R")
source("R/models.R")
source("R/backtest.R")
assert_packages()
ensure_directories()
cfg <- read_config()

# P0: how much of the reported edge is RNG?
#
# The params$seed bug moved the touchdown expanded tier by roughly four ROI
# points, which is the same order as every effect being called an edge here. If
# refitting the same model on a different seed moves ROI by several points, then
# a single-fit number is not a measurement, and deployment should use the
# average across seeds rather than whichever fit happened to run.
#
#   --seeds=15   how many seeds per arm
#   --arm=       spread | total | all

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", flag, "="), "", hit[[1]])
}
n_seeds <- as.integer(arg_value("--seeds", "15"))
arm <- arg_value("--arm", "all")

features <- readRDS("data/processed/game_features.rds")
test_seasons <- cfg$backtest$start_season:cfg$backtest$end_season
seeds <- seq_len(n_seeds) * 7919L

walk_forward_one <- function(model, target, seed) {
  purrr::map_dfr(test_seasons, function(s) {
    train <- dplyr::filter(features, .data$season < s)
    test <- dplyr::filter(features, .data$season == s)
    if (!nrow(train) || !nrow(test)) return(tibble::tibble())
    # The seed goes in as an argument. ranger takes its seed as a parameter and
    # ignores the ambient RNG, so an outer set.seed() alone silently produced
    # fifteen identical fits and a noise floor of exactly zero.
    set.seed(seed)
    prediction <- fit_predict_model(
      model, train, test, target, cfg, seed = seed
    )
    tibble::tibble(
      season = s, game_id = test$game_id,
      truth = test[[target]], prediction = prediction,
      home_line = test$home_line, total_line = test$total_line
    )
  })
}

grade <- function(pred, market, threshold, home_only) {
  d <- pred |>
    dplyr::mutate(
      market_value = if (market == "total") .data$total_line else -.data$home_line,
      edge = .data$prediction - .data$market_value
    ) |>
    dplyr::filter(abs(.data$edge) >= threshold)
  if (home_only) d <- dplyr::filter(d, .data$edge > 0)
  if (!nrow(d)) return(tibble::tibble(bets = 0L, roi = NA_real_,
                                      win_rate = NA_real_))
  d <- d |>
    dplyr::mutate(
      result = dplyr::if_else(
        .data$edge > 0,
        signed_result(.data$truth - .data$market_value),
        signed_result(.data$market_value - .data$truth)
      ),
      profit = american_profit(.data$result, cfg$backtest$american_odds)
    )
  tibble::tibble(
    bets = nrow(d), wins = sum(d$result > 0),
    win_rate = sum(d$result > 0) / max(1, sum(d$result != 0)),
    roi = sum(d$profit) / nrow(d)
  )
}

results <- list()
predictions_by_seed <- list()

# --------------------------------------------------------------------------
# Totals: forward-selected linear. lm() and step() carry no RNG, so this should
# be identical across seeds. Confirm rather than assume, then stop.
# --------------------------------------------------------------------------

if (arm %in% c("all", "total")) {
  cat("Checking whether the total model is deterministic...\n")
  a <- walk_forward_one("forward_linear", "game_total", 1L)
  b <- walk_forward_one("forward_linear", "game_total", 999983L)
  identical_preds <- isTRUE(all.equal(a$prediction, b$prediction))
  cat("Total model deterministic across seeds:", identical_preds, "\n")
  g <- grade(a, "total", 5, FALSE)
  cat(sprintf("  totals 5+: %d bets, %.1f%% win, ROI %+.4f\n",
              g$bets, 100 * g$win_rate, g$roi))
  if (!identical_preds) {
    for (s in seeds) {
      p <- walk_forward_one("forward_linear", "game_total", s)
      results[[length(results) + 1L]] <- dplyr::bind_cols(
        tibble::tibble(arm = "total_fl5", seed = s), grade(p, "total", 5, FALSE)
      )
    }
  } else {
    results[[length(results) + 1L]] <- dplyr::bind_cols(
      tibble::tibble(arm = "total_fl5", seed = NA_integer_), g
    )
    predictions_by_seed[["total"]] <- a
  }
}

# --------------------------------------------------------------------------
# Spreads: ranger has its own RNG and is the funded arm most exposed to it.
# --------------------------------------------------------------------------

if (arm %in% c("all", "spread")) {
  cat("\nRefitting the spread model across", n_seeds, "seeds...\n")
  for (i in seq_along(seeds)) {
    p <- walk_forward_one("random_forest", "home_margin", seeds[[i]])
    predictions_by_seed[[paste0("spread_", seeds[[i]])]] <- p
    g <- grade(p, "spread", 6, TRUE)
    results[[length(results) + 1L]] <- dplyr::bind_cols(
      tibble::tibble(arm = "spread_rf6_home", seed = seeds[[i]]), g
    )
    cat(sprintf("  seed %6d: %3d bets, %.1f%% win, ROI %+.4f\n",
                seeds[[i]], g$bets, 100 * g$win_rate, g$roi))
  }
}

summary_table <- dplyr::bind_rows(results)
readr::write_csv(summary_table, "outputs/seed_noise_by_seed.csv")

cat("\n=== Noise floor per arm ===\n")
noise <- summary_table |>
  dplyr::filter(!is.na(.data$seed)) |>
  dplyr::group_by(.data$arm) |>
  dplyr::summarise(
    seeds = dplyr::n(),
    mean_roi = mean(.data$roi), sd_roi = stats::sd(.data$roi),
    min_roi = min(.data$roi), max_roi = max(.data$roi),
    mean_bets = mean(.data$bets), sd_bets = stats::sd(.data$bets),
    .groups = "drop"
  )
print(as.data.frame(noise), digits = 4)
readr::write_csv(noise, "outputs/seed_noise_floor.csv")

# --------------------------------------------------------------------------
# Seed-averaged deployment: average the prediction across seeds, then apply the
# threshold once. This is what should be deployed - a single fit is one draw
# from the distribution above.
# --------------------------------------------------------------------------

spread_keys <- grep("^spread_", names(predictions_by_seed), value = TRUE)
if (length(spread_keys) > 1) {
  averaged <- purrr::map_dfr(spread_keys, function(k) predictions_by_seed[[k]]) |>
    dplyr::group_by(.data$season, .data$game_id) |>
    dplyr::summarise(
      truth = dplyr::first(.data$truth),
      prediction = mean(.data$prediction),
      home_line = dplyr::first(.data$home_line),
      total_line = dplyr::first(.data$total_line),
      .groups = "drop"
    )
  g <- grade(averaged, "spread", 6, TRUE)
  cat("\n=== Seed-averaged spread model, 6+ points, home only ===\n")
  print(as.data.frame(g), digits = 4)
  saveRDS(averaged, "data/processed/spread_seed_averaged_predictions.rds")

  cat("\nSingle-seed ROI range:",
      sprintf("%+.4f to %+.4f", min(noise$min_roi), max(noise$max_roi)),
      "| averaged:", sprintf("%+.4f", g$roi), "\n")
}
