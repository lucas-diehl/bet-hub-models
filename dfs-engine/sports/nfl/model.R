# ==============================================================================
# NFL plugin — projection MODEL (opportunity-first, like WNBA minutes-first)
# The field chases last week's fantasy points (noisy, priced-in). We instead model
# each player from their STICKY OPPORTUNITY — rolling target_share, wopr, air-yards
# share, carries, targets — plus recent efficiency, then predict DK points. Volume is
# far more predictive week to week than points, and the field under-weights it, so
# this is where public data converts to edge vs the FIELD.
#
# All features are PRIOR-game rolling (leak-free, adaptive/expanding early season), so
# a single pass yields walk-forward projections for the backtest + live state.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table); library(xgboost) })

NFL_MODEL_PATH <- function() dfs_path("data", "models", "nfl_models.rds")

NFL_FEATS <- c("r_dk_pts", "r_target_share", "r_air_yards_share", "r_wopr", "r_targets",
               "r_receptions", "r_carries", "r_rec_yds", "r_rush_yds", "r_pass_yds",
               "r_attempts", "r_ypt", "r_ypc", "g_played", "is_QB", "is_RB", "is_WR", "is_TE")

# leak-free PRIOR-game rolling mean (expanding up to `window`, ignores the current game)
.nfl_roll <- function(x, window = 5L) {
  x <- as.numeric(x); n <- pmin(seq_along(x), window)
  frollmean(shift(x, 1L), n, adaptive = TRUE, na.rm = TRUE)
}

nfl_build_features <- function(W, window = 5L) {
  D <- as.data.table(copy(W)); setorder(D, player_id, season, week)
  D[, gi := seq_len(.N), by = player_id]
  rollmap <- c(dk_pts = "r_dk_pts", target_share = "r_target_share",
               air_yards_share = "r_air_yards_share", wopr = "r_wopr", targets = "r_targets",
               receptions = "r_receptions", carries = "r_carries", receiving_yards = "r_rec_yds",
               rushing_yards = "r_rush_yds", passing_yards = "r_pass_yds", attempts = "r_attempts")
  for (col in names(rollmap)) {
    if (col %in% names(D)) D[, (rollmap[[col]]) := .nfl_roll(get(col), window), by = player_id]
  }
  D[, r_ypt := .nfl_roll(receiving_yards, window) / pmax(.nfl_roll(targets, window), 0.5), by = player_id]
  D[, r_ypc := .nfl_roll(rushing_yards, window) / pmax(.nfl_roll(carries, window), 0.5), by = player_id]
  D[, g_played := gi - 1L]
  D[, `:=`(is_QB = as.integer(position == "QB"), is_RB = as.integer(position == "RB"),
           is_WR = as.integer(position == "WR"), is_TE = as.integer(position == "TE"))]
  D[]
}

.nfl_mat <- function(D) xgb.DMatrix(as.matrix(D[, NFL_FEATS, with = FALSE]), label = D$dk_pts)

nfl_fit <- function(D, nrounds = 500L) {
  base <- list(eta = 0.03, max_depth = 5, min_child_weight = 20, subsample = 0.8,
               colsample_bytree = 0.8, nthread = 2, objective = "reg:squarederror")
  m <- xgb.train(base, .nfl_mat(D), nrounds = nrounds)
  D[, pred := predict(m, .nfl_mat(D))]
  D[, tier := cut(pred, c(-Inf, 5, 10, 16, 24, Inf))]
  tsd <- D[, .(resid_sd = sd(dk_pts - pred)), by = tier][is.na(resid_sd) | resid_sd <= 0, resid_sd := 7]
  list(model = m, tier_sd = tsd, feats = NFL_FEATS, trained_on = format(Sys.time(), "%Y-%m-%d"))
}

nfl_train <- function(seasons = NULL, refresh = FALSE) {
  if (refresh || !file.exists(nfl_weekly_path())) nfl_ingest(seasons)
  W <- as.data.table(readRDS(nfl_weekly_path()))
  D <- nfl_build_features(W)[!is.na(dk_pts) & !is.na(r_dk_pts)]     # need >=1 prior game
  fit <- nfl_fit(D)
  dir.create(dirname(NFL_MODEL_PATH()), recursive = TRUE, showWarnings = FALSE)
  saveRDS(fit, NFL_MODEL_PATH())
  msg("Saved NFL model ->", NFL_MODEL_PATH(), "(", nrow(D), "player-games )")
  invisible(fit)
}

nfl_load_models <- function() { p <- NFL_MODEL_PATH(); if (file.exists(p)) readRDS(p) else NULL }

# LIVE PROJECTION — features a set of players ENTER their next game with (their current
# rolling form), for projecting an upcoming slate. Appends a synthetic "next" row per
# player after their last real game, so the leak-free prior-game rolling yields the state
# entering that game (mirrors how each training row was built). `W` = full weekly history.
nfl_entering_features <- function(W, ids, window = 5L) {
  W <- as.data.table(W); ids <- unique(ids[!is.na(ids)])
  hist <- W[player_id %in% ids]; if (!nrow(hist)) return(data.table())
  nxt <- hist[order(season, week), .SD[.N], by = player_id]        # each player's most recent game
  nxt[, `:=`(season = max(W$season) + 1L, week = 0L, dk_pts = NA_real_)]
  statcols <- intersect(c("target_share","air_yards_share","wopr","targets","receptions",
    "carries","receiving_yards","rushing_yards","passing_yards","attempts"), names(nxt))
  if (length(statcols)) nxt[, (statcols) := NA_real_]
  F <- nfl_build_features(rbind(hist, nxt, fill = TRUE), window)
  F[season == max(W$season) + 1L, c("player_id", "position", NFL_FEATS), with = FALSE]
}

# Predict proj + a variance distribution (sim_sd/ceil/floor from the tier residual sd) for
# entering-features rows. Returns one row per player_id.
nfl_predict <- function(models, feat) {
  feat <- as.data.table(feat); if (!nrow(feat)) return(feat)
  proj <- pmax(predict(models$model, .nfl_mat(copy(feat)[, dk_pts := 0])), 0)
  tier <- cut(proj, c(-Inf, 5, 10, 16, 24, Inf))
  ts <- as.data.table(models$tier_sd)
  sd <- ts$resid_sd[match(as.character(tier), as.character(ts$tier))]
  sd[is.na(sd) | sd <= 0] <- 7
  data.table(player_id = feat$player_id, proj = round(proj, 2), sim_sd = pmax(sd, 4),
             ceil = round(proj + 0.84 * sd, 2), floor = round(pmax(proj - 1.2 * sd, 0), 2),
             p_zero = 0.02)
}

# Walk-forward accuracy: train on seasons < holdout, predict holdout. Baseline = the
# recency signal the field uses (trailing DK-points mean, r_dk_pts).
nfl_accuracy_backtest <- function(holdout_season = NULL) {
  W <- as.data.table(readRDS(nfl_weekly_path()))
  D <- nfl_build_features(W)[!is.na(dk_pts) & !is.na(r_dk_pts)]
  if (is.null(holdout_season)) holdout_season <- max(D$season)
  tr <- D[season < holdout_season]; te <- D[season == holdout_season]
  if (!nrow(tr) || !nrow(te)) { cat("insufficient seasons for holdout", holdout_season, "\n"); return(invisible()) }
  fit <- nfl_fit(copy(tr))
  te[, proj := predict(fit$model, .nfl_mat(te))]
  rmse <- function(a, b) sqrt(mean((a - b)^2)); mae <- function(a, b) mean(abs(a - b))
  cat(sprintf("\n=== NFL ACCURACY BACKTEST (holdout %d, %d player-games) ===\n", holdout_season, nrow(te)))
  cat(sprintf("MODEL     RMSE %.2f  MAE %.2f  corr %.3f\n", rmse(te$proj, te$dk_pts), mae(te$proj, te$dk_pts), cor(te$proj, te$dk_pts)))
  cat(sprintf("BASELINE  RMSE %.2f  MAE %.2f  corr %.3f   (trailing DK-pts mean = what the field uses)\n",
      rmse(te$r_dk_pts, te$dk_pts), mae(te$r_dk_pts, te$dk_pts), cor(te$r_dk_pts, te$dk_pts)))
  cat("beats baseline on RMSE:", rmse(te$proj, te$dk_pts) < rmse(te$r_dk_pts, te$dk_pts), "\n")
  cat("\nby position (model RMSE vs baseline RMSE):\n")
  print(te[, .(n = .N, model_rmse = round(rmse(proj, dk_pts), 2), base_rmse = round(rmse(r_dk_pts, dk_pts), 2),
               model_corr = round(cor(proj, dk_pts), 3)), by = position][order(-n)])
  invisible(te)
}
