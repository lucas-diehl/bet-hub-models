source("R/utilities.R")
source("R/feature_registry.R")
source("R/models.R")
source("R/backtest.R")
source("R/nfl_dvoa_ratings.R")
assert_packages()
ensure_directories()
cfg <- read_config()

# Porting the cfb-modeling project's DVOA-style opponent-adjusted rating
# construction to NFL, and testing it two ways against the existing
# spread/totals model - the same two-gate structure that produced a real,
# if modest, finding in CFB (standalone: null; consensus-agreement filter on
# an existing edge: real).
#
#   Gate 1 (standalone): does a DVOA variant, alone, beat the existing
#     seed-averaged model in a paired comparison on identical games?
#   Gate 2 (consensus): among the bets the existing model ALREADY qualifies
#     (spread edge>=6 home-only, total edge>=5), does requiring N of 5 DVOA
#     variants to independently agree raise the win rate?
#
# PBP pull: 2016-2025, giving 2018 (the backtest's first test season) two
# real prior seasons for the carryover regression rather than a cold start.

PBP_SEASONS <- 2016:2025
cache_path <- "data/processed/nfl_dvoa_asof_variants.rds"

if (file.exists(cache_path)) {
  cat("Using cached", cache_path, "\n")
  dvoa <- readRDS(cache_path)
} else {
  cat("Pulling NFL play-by-play,", min(PBP_SEASONS), "-", max(PBP_SEASONS), "...\n")
  pbp <- nflreadr::load_pbp(PBP_SEASONS)
  cat("  ", format(nrow(pbp), big.mark = ","), "plays loaded.\n")
  dvoa <- nfl_dvoa_build_all(pbp)
  saveRDS(dvoa, cache_path)
  rm(pbp); gc(verbose = FALSE)
}
cat("DVOA as-of rows:", nrow(dvoa), "\n\n")

# --------------------------------------------------------------------------
# Attach to game_features.rds (season, week, home/away team, actual results,
# closing lines - this project's one established line source, see
# outputs/review_response.md §1 for its closing-line provenance).
# --------------------------------------------------------------------------

# Kept clean (no DVOA columns) specifically so it can still be passed to
# fit_predict_model() for the totals recompute below - that function's
# feature_names() asserts against the registered schema (R/feature_registry.R)
# and correctly rejects any unregistered numeric column, DVOA included. The
# DVOA-augmented copy (`games`, below) is for this script's own side analysis
# only and is never itself passed to fit_predict_model().
games_raw <- readRDS("data/processed/game_features.rds") |>
  dplyr::mutate(home_team = normalize_team(.data$home_team),
                away_team = normalize_team(.data$away_team))
games <- games_raw

VARIANTS <- nfl_dvoa_variant_names()
dvh <- dvoa |> dplyr::rename_with(~ paste0("h_", .x), -c("season", "as_of_week", "team")) |>
  dplyr::rename(home_team = "team")
dva <- dvoa |> dplyr::rename_with(~ paste0("a_", .x), -c("season", "as_of_week", "team")) |>
  dplyr::rename(away_team = "team")
games <- games |>
  dplyr::left_join(dvh, by = c("season", "week" = "as_of_week", "home_team")) |>
  dplyr::left_join(dva, by = c("season", "week" = "as_of_week", "away_team"))

for (v in VARIANTS) {
  ho <- games[[paste0("h_aoff_", v)]]; ad <- games[[paste0("a_adef_", v)]]
  ao <- games[[paste0("a_aoff_", v)]]; hd <- games[[paste0("h_adef_", v)]]
  games[[paste0("dvoa_margin_", v)]] <- (ho + ad) - (ao + hd)
  games[[paste0("dvoa_total_", v)]]  <- (ho + ad) + (ao + hd)
}

test_seasons <- cfg$backtest$start_season:cfg$backtest$end_season
cat("Test window:", paste(range(test_seasons), collapse = "-"), "\n\n")

# --------------------------------------------------------------------------
# Existing-model baseline: reuse the already-built seed-averaged spread
# predictions (avoids a ~2h45m re-fit); totals is deterministic (no RNG) so a
# single walk-forward pass reproduces exactly.
# --------------------------------------------------------------------------

spread_path <- "data/processed/spread_seed_averaged_predictions.rds"
if (!file.exists(spread_path)) {
  stop("Run scripts/64 first to build the seed-averaged spread predictions.", call. = FALSE)
}
spread_pred <- readRDS(spread_path) |>
  dplyr::rename(margin_pred_existing = "prediction")

total_pred <- purrr::map_dfr(test_seasons, function(s) {
  train <- dplyr::filter(games_raw, .data$season < s)
  test <- dplyr::filter(games_raw, .data$season == s)
  if (!nrow(train) || !nrow(test)) return(tibble::tibble())
  set.seed(cfg$backtest$seed)
  pred <- fit_predict_model("forward_linear", train, test, "game_total", cfg,
                            seed = cfg$backtest$seed)
  tibble::tibble(season = s, game_id = test$game_id, total_pred_existing = pred)
})

games <- games |>
  dplyr::left_join(dplyr::select(spread_pred, "season", "game_id", "margin_pred_existing"),
                   by = c("season", "game_id")) |>
  dplyr::left_join(total_pred, by = c("season", "game_id"))

# --------------------------------------------------------------------------
# Walk-forward calibration of each DVOA variant, as-of (train season < S),
# matching this project's own convention throughout.
# --------------------------------------------------------------------------

cal_margin <- list(); cal_total <- list()
for (s in test_seasons) {
  train <- dplyr::filter(games, .data$season < s)
  cal_margin[[as.character(s)]] <- list(); cal_total[[as.character(s)]] <- list()
  for (v in VARIANTS) {
    mcol <- paste0("dvoa_margin_", v); tcol <- paste0("dvoa_total_", v)
    hm <- train[!is.na(train[[mcol]]), ]
    ht <- train[!is.na(train[[tcol]]), ]
    cal_margin[[as.character(s)]][[v]] <- if (nrow(hm) >= 200) {
      stats::lm(stats::as.formula(paste0("home_margin ~ ", mcol)), data = hm)
    } else NULL
    cal_total[[as.character(s)]][[v]] <- if (nrow(ht) >= 200) {
      stats::lm(stats::as.formula(paste0("game_total ~ ", tcol)), data = ht)
    } else NULL
  }
}

test_games <- dplyr::filter(games, .data$season %in% test_seasons,
                            is.finite(.data$margin_pred_existing),
                            is.finite(.data$total_pred_existing))
cat("Test games with both existing-model predictions:", nrow(test_games), "\n\n")

for (v in VARIANTS) {
  mcol <- paste0("dvoa_margin_", v); tcol <- paste0("dvoa_total_", v)
  test_games[[paste0("dvoa_margin_pred_", v)]] <- vapply(seq_len(nrow(test_games)), function(i) {
    s <- as.character(test_games$season[[i]]); m <- cal_margin[[s]][[v]]
    if (is.null(m) || is.na(test_games[[mcol]][[i]])) return(NA_real_)
    as.numeric(stats::predict(m, test_games[i, ]))
  }, numeric(1))
  test_games[[paste0("dvoa_total_pred_", v)]] <- vapply(seq_len(nrow(test_games)), function(i) {
    s <- as.character(test_games$season[[i]]); m <- cal_total[[s]][[v]]
    if (is.null(m) || is.na(test_games[[tcol]][[i]])) return(NA_real_)
    as.numeric(stats::predict(m, test_games[i, ]))
  }, numeric(1))
}

readr::write_csv(test_games, "outputs/nfl_dvoa_test_games.csv")
cat("Wrote outputs/nfl_dvoa_test_games.csv (", nrow(test_games), "rows )\n\n")

# --------------------------------------------------------------------------
# Grading helpers (same convention used throughout this project: -110,
# american_profit()/signed_result() from R/backtest.R)
# --------------------------------------------------------------------------

grade_spread <- function(pick_home_edge, truth_margin, home_line) {
  d <- tibble::tibble(edge = pick_home_edge, truth = truth_margin, home_line = home_line) |>
    dplyr::filter(is.finite(.data$edge), .data$edge > 0) |>   # home-only, matches deployed rule
    dplyr::mutate(
      result = signed_result(.data$truth - (-.data$home_line)),
      profit = american_profit(.data$result, cfg$backtest$american_odds)
    )
  d
}
grade_total <- function(pred, total_line, actual_total) {
  d <- tibble::tibble(pred = pred, line = total_line, actual = actual_total) |>
    dplyr::mutate(edge = .data$pred - .data$line) |>
    dplyr::filter(is.finite(.data$edge), abs(.data$edge) >= 5) |>
    dplyr::mutate(
      result = dplyr::if_else(
        .data$edge > 0, signed_result(.data$actual - .data$line),
        signed_result(.data$line - .data$actual)
      ),
      profit = american_profit(.data$result, cfg$backtest$american_odds)
    )
  d
}
summarise_bets <- function(d) {
  if (!nrow(d)) return(tibble::tibble(bets = 0L, win_rate = NA_real_, roi = NA_real_))
  tibble::tibble(
    bets = nrow(d), win_rate = sum(d$result > 0) / max(1L, sum(d$result != 0)),
    roi = mean(d$profit)
  )
}

# --------------------------------------------------------------------------
# GATE 1 — standalone: DVOA variant alone vs existing model, paired on
# identical games, at the deployed thresholds (spread edge>=6 home-only,
# total edge>=5).
# --------------------------------------------------------------------------

cat("=== GATE 1: standalone, deployed thresholds ===\n\n")

existing_spread_edge <- test_games$margin_pred_existing - (-test_games$home_line)
existing_spread <- grade_spread(
  dplyr::if_else(existing_spread_edge >= 6, existing_spread_edge, NA_real_),
  test_games$home_margin, test_games$home_line
)
cat("Existing model, spread (edge>=6 home):\n")
print(as.data.frame(summarise_bets(existing_spread)), digits = 4)

existing_total <- grade_total(test_games$total_pred_existing, test_games$total_line,
                              test_games$game_total)
cat("\nExisting model, total (|edge|>=5):\n")
print(as.data.frame(summarise_bets(existing_total)), digits = 4)

cat("\n--- DVOA variants, standalone, same thresholds ---\n")
gate1_results <- purrr::map_dfr(VARIANTS, function(v) {
  mpred <- test_games[[paste0("dvoa_margin_pred_", v)]]
  medge <- mpred - (-test_games$home_line)
  sp <- grade_spread(dplyr::if_else(medge >= 6, medge, NA_real_),
                     test_games$home_margin, test_games$home_line)
  tpred <- test_games[[paste0("dvoa_total_pred_", v)]]
  tt <- grade_total(tpred, test_games$total_line, test_games$game_total)
  dplyr::bind_rows(
    dplyr::bind_cols(tibble::tibble(variant = v, market = "spread"), summarise_bets(sp)),
    dplyr::bind_cols(tibble::tibble(variant = v, market = "total"), summarise_bets(tt))
  )
})
print(as.data.frame(gate1_results), digits = 4)
readr::write_csv(gate1_results, "outputs/nfl_dvoa_gate1_standalone.csv")

# Paired head-to-head: on games where BOTH the existing model and a DVOA
# variant qualify for a home-side spread bet (edge>=6, so both picked the
# same side by construction - the deployed rule is home-only), do they agree
# on WHICH games clear the bar, and whose picks win more often on the
# overlap? Mirrors CFB's decisive paired test.
cat("\n--- Paired head-to-head (spread), existing model vs each variant ---\n")
paired_results <- purrr::map_dfr(VARIANTS, function(v) {
  mpred <- test_games[[paste0("dvoa_margin_pred_", v)]]
  medge <- mpred - (-test_games$home_line)
  d <- test_games |>
    dplyr::mutate(exist_edge = existing_spread_edge, dvoa_edge = medge) |>
    dplyr::filter(is.finite(.data$exist_edge), is.finite(.data$dvoa_edge),
                  .data$exist_edge >= 6, .data$dvoa_edge >= 6) |>
    dplyr::mutate(
      result = signed_result(.data$home_margin - (-.data$home_line)),
      profit = american_profit(.data$result, cfg$backtest$american_odds)
    ) |>
    dplyr::filter(.data$result != 0)
  tibble::tibble(
    variant = v, n_overlap = nrow(d),
    win_rate_on_overlap = if (nrow(d)) mean(d$result > 0) else NA_real_,
    roi_on_overlap = if (nrow(d)) mean(d$profit) else NA_real_
  )
})
print(as.data.frame(paired_results), digits = 4)
readr::write_csv(paired_results, "outputs/nfl_dvoa_paired_overlap.csv")

# --------------------------------------------------------------------------
# GATE 2 — consensus: among the bets the EXISTING model already qualifies
# (the deployed rule, unchanged), does requiring N of 5 DVOA variants to
# independently show edge in the SAME direction raise the win rate? This is
# the test that actually found something in CFB - not a better rating, a
# confidence filter on the rating already in production.
# --------------------------------------------------------------------------

cat("\n\n=== GATE 2: consensus filter on the EXISTING model's own bets ===\n\n")

vote_mat_spread <- sapply(VARIANTS, function(v) {
  mpred <- test_games[[paste0("dvoa_margin_pred_", v)]]
  dvoa_edge <- mpred - (-test_games$home_line)
  as.integer(!is.na(dvoa_edge) & abs(dvoa_edge) >= 6 &
               sign(dvoa_edge) == sign(existing_spread_edge))
})
spread_votes <- rowSums(vote_mat_spread)

spread_qualifying <- test_games |>
  dplyr::mutate(edge = existing_spread_edge, votes = spread_votes) |>
  dplyr::filter(is.finite(.data$edge), .data$edge >= 6) |>
  dplyr::mutate(
    result = signed_result(.data$home_margin - (-.data$home_line)),
    profit = american_profit(.data$result, cfg$backtest$american_odds)
  )

cat("--- Spread: win rate by minimum DVOA-variant agreement (cumulative) ---\n")
spread_dose <- purrr::map_dfr(0:5, function(k) {
  d <- dplyr::filter(spread_qualifying, .data$votes >= k, .data$result != 0)
  tibble::tibble(min_votes = k, n = nrow(d),
                win_pct = if (nrow(d)) round(100 * mean(d$result > 0), 1) else NA_real_,
                roi = if (nrow(d)) round(100 * mean(d$profit), 2) else NA_real_)
})
print(as.data.frame(spread_dose))

cat("\n--- Spread: MARGINAL win rate at exactly k votes ---\n")
spread_marginal <- purrr::map_dfr(0:5, function(k) {
  d <- dplyr::filter(spread_qualifying, .data$votes == k, .data$result != 0)
  tibble::tibble(votes = k, n = nrow(d),
                win_pct = if (nrow(d)) round(100 * mean(d$result > 0), 1) else NA_real_)
})
print(as.data.frame(spread_marginal))

# Same for totals.
vote_mat_total <- sapply(VARIANTS, function(v) {
  tpred <- test_games[[paste0("dvoa_total_pred_", v)]]
  dvoa_edge <- tpred - test_games$total_line
  existing_edge <- test_games$total_pred_existing - test_games$total_line
  as.integer(!is.na(dvoa_edge) & abs(dvoa_edge) >= 5 &
               sign(dvoa_edge) == sign(existing_edge))
})
total_votes <- rowSums(vote_mat_total)
existing_total_edge <- test_games$total_pred_existing - test_games$total_line

total_qualifying <- test_games |>
  dplyr::mutate(edge = existing_total_edge, votes = total_votes) |>
  dplyr::filter(is.finite(.data$edge), abs(.data$edge) >= 5) |>
  dplyr::mutate(
    result = dplyr::if_else(
      .data$edge > 0, signed_result(.data$game_total - .data$total_line),
      signed_result(.data$total_line - .data$game_total)
    ),
    profit = american_profit(.data$result, cfg$backtest$american_odds)
  )

cat("\n--- Total: win rate by minimum DVOA-variant agreement (cumulative) ---\n")
total_dose <- purrr::map_dfr(0:5, function(k) {
  d <- dplyr::filter(total_qualifying, .data$votes >= k, .data$result != 0)
  tibble::tibble(min_votes = k, n = nrow(d),
                win_pct = if (nrow(d)) round(100 * mean(d$result > 0), 1) else NA_real_,
                roi = if (nrow(d)) round(100 * mean(d$profit), 2) else NA_real_)
})
print(as.data.frame(total_dose))

cat("\n--- Total: MARGINAL win rate at exactly k votes ---\n")
total_marginal <- purrr::map_dfr(0:5, function(k) {
  d <- dplyr::filter(total_qualifying, .data$votes == k, .data$result != 0)
  tibble::tibble(votes = k, n = nrow(d),
                win_pct = if (nrow(d)) round(100 * mean(d$result > 0), 1) else NA_real_)
})
print(as.data.frame(total_marginal))

readr::write_csv(spread_dose, "outputs/nfl_dvoa_spread_consensus_dose.csv")
readr::write_csv(spread_marginal, "outputs/nfl_dvoa_spread_consensus_marginal.csv")
readr::write_csv(total_dose, "outputs/nfl_dvoa_total_consensus_dose.csv")
readr::write_csv(total_marginal, "outputs/nfl_dvoa_total_consensus_marginal.csv")

cat("\n=== DONE ===\n")
