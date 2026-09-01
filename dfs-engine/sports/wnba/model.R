# ==============================================================================
# WNBA plugin — projection MODEL (minutes-first)
# Minutes is the biggest edge in WNBA DFS, so we model it explicitly and separately
# from efficiency:
#   1. MINUTES model      (xgboost on leak-free lagged usage/role features)
#   2. PER-MINUTE model   (xgboost on lagged efficiency features; heavily regularized
#                          because per-minute production is noisy)
#   proj = E[minutes] * E[fp_per_minute]
#   distribution: residual-SD by predicted-minute tier -> sim_sd, ceil, floor;
#   p_zero from predicted minutes (low-minute players carry DNP/blowout risk).
#
# All features are computed identically two ways and MUST stay in sync:
#   - training:  vectorized frollmean(shift(x,1), k)  = mean of the last k PRIOR games
#   - inference: tail(x, k) over a player's games strictly before the slate date
# This symmetry is what keeps the backtest honest (no leakage).
# ==============================================================================

suppressPackageStartupMessages({ library(data.table); library(xgboost) })

WNBA_MODEL_PATH <- function() dfs_path("data", "models", "wnba_models.rds")

WNBA_FEATS <- c("min_r3","min_r5","min_r10","min_r20","start_r5",
                "usage_r5","fppm_r5","fppm_r10","rest","is_home")
# NOTE (2026-07-21): tested adding appr_r10 / min_trend / min_sd5 (role security/trend/
# volatility) to fight the LIVE over-projection. Walk-forward backtest (holdout 2025,
# incl DNPs) showed NO improvement — RMSE ~9.46 either way, and the model is already well
# CALIBRATED (bias -0.24). The live over-projection is NOT a model problem: it's DK-slate
# players who don't play (inactives). The fix is the lock-time inactives safeguard, not
# more features. Kept the feature code below (harmless, unused) as a record.

# ── feature engineering (training view: leak-free lagged) ─────────────────────
wnba_build_features <- function(box) {
  D <- as.data.table(copy(box))
  D[, game_date := as.Date(game_date)]
  D[, starter01 := as.numeric(starter %in% TRUE)]
  fga <- if ("field_goals_attempted" %in% names(D)) D$field_goals_attempted else 0
  fta <- if ("free_throws_attempted" %in% names(D)) D$free_throws_attempted else 0
  D[, usage := fga + 0.44 * fta + tov]
  setorder(D, athlete_id, game_date, game_id)
  rm5 <- function(x, k) frollmean(shift(x, 1), k, na.rm = TRUE)
  D[, `:=`(
    min_r3  = rm5(min, 3),  min_r5  = rm5(min, 5),
    min_r10 = rm5(min, 10), min_r20 = rm5(min, 20),
    start_r5 = rm5(starter01, 5), usage_r5 = rm5(usage, 5)
  ), by = athlete_id]
  D[, fppm_r5  := frollsum(shift(dk_pts, 1), 5,  na.rm = TRUE) /
                  pmax(frollsum(shift(min, 1), 5,  na.rm = TRUE), 1), by = athlete_id]
  D[, fppm_r10 := frollsum(shift(dk_pts, 1), 10, na.rm = TRUE) /
                  pmax(frollsum(shift(min, 1), 10, na.rm = TRUE), 1), by = athlete_id]
  # NEW role features (leak-free, prior games): appearance rate (rotation/DNP security),
  # minutes trend (rising vs falling role), and minutes volatility (unstable role = risk).
  D[, played01 := as.numeric(!is.na(min) & min > 0)]
  D[, appr_r10 := rm5(played01, 10), by = athlete_id]
  D[, min_sq5  := frollmean(shift(min, 1)^2, 5, na.rm = TRUE), by = athlete_id]   # E[min^2] of prior 5
  D[, min_sd5  := sqrt(pmax(min_sq5 - min_r5^2, 0))]                              # rolling SD via E[x^2]-E[x]^2
  D[, min_trend := min_r3 - min_r10]
  D[, rest := as.numeric(game_date - shift(game_date, 1)), by = athlete_id]
  D[is.na(rest) | rest > 7, rest := 7][rest < 1, rest := 1]
  D[, is_home := as.numeric(home_away == "home")]
  D[]
}

# ── feature engineering (inference view: entering the next game) ──────────────
# For each athlete in `athlete_ids`, build the same features from games strictly
# before `as_of`. Returns dt(athlete_id, <WNBA_FEATS>).
wnba_entering_features <- function(box, as_of, athlete_ids = NULL) {
  H <- as.data.table(copy(box)); H[, game_date := as.Date(game_date)]
  H <- H[game_date < as.Date(as_of)]
  if (!is.null(athlete_ids)) H <- H[athlete_id %in% athlete_ids]
  if (!nrow(H)) return(data.table(athlete_id = athlete_ids))
  H[, starter01 := as.numeric(starter %in% TRUE)]
  fga <- if ("field_goals_attempted" %in% names(H)) H$field_goals_attempted else 0
  fta <- if ("free_throws_attempted" %in% names(H)) H$free_throws_attempted else 0
  H[, usage := fga + 0.44 * fta + tov]
  setorder(H, athlete_id, game_date)
  H[, {
    tk <- function(x, k) tail(x, k)
    m3 <- mean(tk(min, 3)); m10 <- mean(tk(min, 10)); m5 <- tk(min, 5)
    list(
      min_r3  = m3,                min_r5  = mean(tk(min, 5)),
      min_r10 = m10,               min_r20 = mean(tk(min, 20)),
      start_r5 = mean(tk(starter01, 5)),
      usage_r5 = mean(tk(usage, 5)),
      fppm_r5  = sum(tk(dk_pts, 5))  / max(sum(tk(min, 5)),  1),
      fppm_r10 = sum(tk(dk_pts, 10)) / max(sum(tk(min, 10)), 1),
      # NEW role features (match training exactly)
      appr_r10 = mean(tk(as.numeric(!is.na(min) & min > 0), 10)),
      min_sd5  = sqrt(pmax(mean(m5^2) - mean(m5)^2, 0)),
      min_trend = m3 - m10,
      rest = pmin(as.numeric(as.Date(as_of) - max(game_date)), 7),
      is_home = 0.5)                         # unknown pre-slate; neutral
  }, by = athlete_id]
}

.wnba_fillna <- function(M, fill) {
  for (f in names(fill)) { v <- M[[f]]; v[!is.finite(v)] <- fill[[f]]; M[[f]] <- v }
  M
}

# ── train ─────────────────────────────────────────────────────────────────────
wnba_train_models <- function(D) {
  D <- D[is.finite(min_r5)]                  # need some history
  fill <- as.list(vapply(WNBA_FEATS, function(f) median(D[[f]], na.rm = TRUE), numeric(1)))
  D <- .wnba_fillna(D, fill)
  Xall <- as.matrix(D[, ..WNBA_FEATS])
  base <- list(eta = 0.05, max_depth = 4, min_child_weight = 20,
               subsample = 0.8, colsample_bytree = 0.8, nthread = 2)

  msg("  training minutes model (", nrow(D), "rows )...")
  m_min <- xgb.train(c(base, objective = "reg:squarederror"),
                     xgb.DMatrix(Xall, label = D$min), nrounds = 350, verbose = 0)

  eff <- D[min >= 8]                          # per-minute target on meaningful samples
  m_fppm <- xgb.train(c(base, objective = "reg:squarederror"),
                      xgb.DMatrix(as.matrix(eff[, ..WNBA_FEATS]), label = eff$dk_pts / eff$min),
                      nrounds = 350, verbose = 0)

  # residual SD by predicted-minute tier (for sim_sd / ceiling)
  pmin_hat  <- pmax(predict(m_min, Xall), 0)
  pfppm_hat <- pmax(predict(m_fppm, Xall), 0)
  D[, proj_hat := pmin_hat * pfppm_hat]
  D[, mtier := cut(pmin_hat, c(-1, 10, 18, 25, 32, 100))]
  tier_sd <- D[, .(resid_sd = sd(dk_pts - proj_hat)), by = mtier]
  list(m_min = m_min, m_fppm = m_fppm, fill = fill, tier_sd = tier_sd,
       trained_on = format(Sys.time(), "%Y-%m-%d"), n = nrow(D))
}

# Predict proj distribution from an entering-features table -> adds proj/sim_sd/ceil/floor/p_zero.
wnba_predict <- function(models, feat) {
  feat <- .wnba_fillna(as.data.table(copy(feat)), models$fill)
  X <- as.matrix(feat[, ..WNBA_FEATS])
  pmin_hat  <- pmax(predict(models$m_min, X), 0)
  pfppm_hat <- pmax(predict(models$m_fppm, X), 0)
  feat[, pred_min := pmin_hat]
  feat[, proj := pmin_hat * pfppm_hat]
  feat[, mtier := cut(pmin_hat, c(-1, 10, 18, 25, 32, 100))]
  feat <- merge(feat, models$tier_sd, by = "mtier", all.x = TRUE, sort = FALSE)
  feat[is.na(resid_sd) | resid_sd <= 0, resid_sd := mean(models$tier_sd$resid_sd, na.rm = TRUE)]
  feat[, sim_sd := resid_sd]
  feat[, ceil  := proj + 0.84 * sim_sd]
  feat[, floor := pmax(proj - 1.2 * sim_sd, 0)]
  feat[, p_zero := pmin(pmax(0.30 - 0.012 * pred_min, 0.01), 0.35)]
  feat[]
}

# ── orchestrate: ingest if needed -> features -> train -> cache ────────────────
wnba_train <- function(seasons = NULL, refresh = FALSE) {
  bp <- wnba_box_path()
  if (refresh || !file.exists(bp)) wnba_ingest(seasons)
  box <- as.data.table(readRDS(bp))
  D <- wnba_build_features(box)
  models <- wnba_train_models(D)
  dir.create(dirname(WNBA_MODEL_PATH()), recursive = TRUE, showWarnings = FALSE)
  saveRDS(models, WNBA_MODEL_PATH())
  msg("Saved WNBA models ->", WNBA_MODEL_PATH(), "(", models$n, "training rows )")
  invisible(models)
}

wnba_load_models <- function() {
  p <- WNBA_MODEL_PATH(); if (file.exists(p)) readRDS(p) else NULL
}

# ── walk-forward accuracy backtest (needs NO salaries — works today) ──────────
# Train on seasons < Y, project every player-game in season Y from entering features,
# compare to actual DK points. Baseline = player's trailing-10 mean DK points.
wnba_accuracy_backtest <- function(holdout_season = NULL) {
  box <- as.data.table(readRDS(wnba_box_path()))
  box[, game_date := as.Date(game_date)]
  seasons <- sort(unique(box$season))
  if (is.null(holdout_season)) holdout_season <- max(seasons)
  tr <- box[season < holdout_season]
  if (!nrow(tr)) stop("Not enough prior seasons to backtest ", holdout_season)
  models <- wnba_train_models(wnba_build_features(tr))

  te <- box[season == holdout_season & min > 0]      # graded on games actually played
  setorder(te, game_date)
  dates <- sort(unique(te$game_date))
  hist <- box[season <= holdout_season]
  res <- rbindlist(lapply(dates, function(dt) {
    day <- te[game_date == dt]
    feat <- wnba_entering_features(hist, dt, athlete_ids = unique(day$athlete_id))
    if (!nrow(feat)) return(NULL)
    pred <- wnba_predict(models, feat)[, .(athlete_id, proj, sim_sd)]
    # trailing-10 baseline
    bl <- wnba_entering_features(hist, dt, unique(day$athlete_id))
    base <- merge(day[, .(athlete_id, total_pts = dk_pts, position = athlete_position_abbreviation)],
                  pred, by = "athlete_id")
    base[, base_proj := fifelse(is.finite(feat$min_r10[match(athlete_id, feat$athlete_id)] *
                                          feat$fppm_r10[match(athlete_id, feat$athlete_id)]),
                                feat$min_r10[match(athlete_id, feat$athlete_id)] *
                                feat$fppm_r10[match(athlete_id, feat$athlete_id)], NA_real_)]
    base
  }))
  res <- res[is.finite(proj) & is.finite(total_pts)]
  res[, position := fifelse(position %in% c("C","F"), "F", "G")]
  cat("=== WNBA ACCURACY BACKTEST (holdout", holdout_season, ",", nrow(res), "player-games) ===\n")
  print(validate_accuracy(res)$model)
  cat("model beats trailing-10 baseline (RMSE):",
      sqrt(mean((res$proj - res$total_pts)^2)) < sqrt(mean((res$base_proj - res$total_pts)^2, na.rm = TRUE)), "\n")
  cat("\n=== CALIBRATION ===\n"); print(validate_calibration(res))
  invisible(res)
}
