# ==============================================================================
# NCAAF (CFB) plugin — projection MODEL
# Same "opportunity is stickier than points" philosophy as sports/nfl/model.R, but a
# lean position-specific linear model (not xgboost) — CFB has far more teams/players
# and a thinner per-player sample than NFL, so a simpler, harder-to-overfit model is
# the safer choice here. Features are PRIOR-game rolling (leak-free), so one pass
# yields walk-forward projections for both backtest and live serving.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

NCAAF_MODEL_PATH <- function() dfs_path("data", "models", "ncaaf_models.rds")
CFB_FEATS <- c("r_dk_pts", "r_pass_yds", "r_pass_td", "r_rush_yds", "r_rush_td",
               "r_rec_yds", "r_rec_td", "r_receptions", "r_carries", "g_played")

# leak-free PRIOR-game rolling mean (expanding up to `window`, excludes current game)
.cfb_roll <- function(x, window = 5L) {
  x <- as.numeric(x); n <- pmin(seq_along(x), window)
  frollmean(shift(x, 1L), n, adaptive = TRUE, na.rm = TRUE)
}

ncaaf_build_features <- function(W, window = 5L) {
  D <- as.data.table(copy(W)); setorder(D, athlete_id, season, wk)
  D[, g_played := seq_len(.N) - 1L, by = athlete_id]
  rollmap <- c(dk_pts = "r_dk_pts", pass_yds = "r_pass_yds", pass_td = "r_pass_td",
               rush_yds = "r_rush_yds", rush_td = "r_rush_td", rec_yds = "r_rec_yds",
               rec_td = "r_rec_td", receptions = "r_receptions", carries = "r_carries")
  for (col in names(rollmap)) if (col %in% names(D)) D[, (rollmap[[col]]) := .cfb_roll(get(col), window), by = athlete_id]
  for (f in CFB_FEATS) if (!f %in% names(D)) D[, (f) := 0]
  for (f in CFB_FEATS) D[is.na(get(f)), (f) := 0]
  D
}

# Train one lm per position on prior-game rolling features -> this game's dk_pts.
# refresh=TRUE re-ingests fresh CFBD data first (jobs/train_models.R --sport ncaaf).
ncaaf_train <- function(seasons = NULL, refresh = FALSE) {
  if (!exists("cfb_ingest")) source(dfs_path("sports", "ncaaf", "ingest.R"))
  W <- if (refresh || !file.exists(cfb_weekly_path())) cfb_ingest(refresh = refresh)
       else as.data.table(readRDS(cfb_weekly_path()))
  if (!is.null(seasons)) W <- W[season %in% seasons]
  D <- ncaaf_build_features(W)
  D <- D[g_played >= 1]                                       # need at least 1 prior game for rolling feats
  models <- list()
  for (p in c("QB", "RB", "WR")) {
    Dp <- D[pos == p]
    if (nrow(Dp) < 50) { msg("  ncaaf: too few rows for", p, "(", nrow(Dp), ") -> skip"); next }
    fit <- tryCatch(lm(reformulate(CFB_FEATS, "dk_pts"), data = Dp), error = function(e) NULL)
    if (!is.null(fit)) models[[p]] <- fit
  }
  if (!length(models)) stop("ncaaf_train: no position models trained — check ingest data")
  out <- list(models = models, feats = CFB_FEATS, trained_on = format(Sys.time(), "%Y-%m-%d"),
              n = nrow(D), seasons = sort(unique(D$season)))
  dir.create(dirname(NCAAF_MODEL_PATH()), recursive = TRUE, showWarnings = FALSE)
  saveRDS(out, NCAAF_MODEL_PATH())
  msg(sprintf("  ncaaf model saved: %d rows, positions %s -> %s", out$n,
              paste(names(models), collapse = "/"), NCAAF_MODEL_PATH()))
  invisible(out)
}

# Walk-forward holdout check: train on all seasons but the latest, evaluate cor vs a
# naive baseline (last known dk_pts) on the held-out season. Run standalone to validate.
ncaaf_accuracy_backtest <- function() {
  if (!exists("cfb_ingest")) source(dfs_path("sports", "ncaaf", "ingest.R"))
  W <- as.data.table(readRDS(cfb_weekly_path()))
  yrs <- sort(unique(W$season)); if (length(yrs) < 2) { msg("  need >= 2 seasons for holdout"); return(invisible(NULL)) }
  holdout <- max(yrs); D <- ncaaf_build_features(W)[g_played >= 1]
  tr <- D[season != holdout]; te <- D[season == holdout]
  res <- rbindlist(lapply(c("QB", "RB", "WR"), function(p) {
    fit <- tryCatch(lm(reformulate(CFB_FEATS, "dk_pts"), data = tr[pos == p]), error = function(e) NULL)
    Tp <- te[pos == p]; if (is.null(fit) || !nrow(Tp)) return(NULL)
    pred <- predict(fit, Tp)
    data.table(pos = p, n = nrow(Tp),
               model_cor = suppressWarnings(cor(pred, Tp$dk_pts)),
               baseline_cor = suppressWarnings(cor(Tp$r_dk_pts, Tp$dk_pts)))
  }))
  cat("\n=== NCAAF walk-forward holdout (season", holdout, ") ===\n"); print(res)
  invisible(res)
}

# Entering features for an UPCOMING (unplayed) game: each player's rolling stats over
# their last `window` games (no leak-guard shift needed here — unlike ncaaf_build_features,
# which trains on historical rows and must exclude the row's own game).
ncaaf_entering_features <- function(W, athlete_ids, window = 5L) {
  D <- as.data.table(W)[athlete_id %in% athlete_ids]; setorder(D, athlete_id, season, wk)
  agg <- D[, .(g_played = .N, r_dk_pts = mean(tail(dk_pts, window)),
               r_pass_yds = mean(tail(pass_yds, window)), r_pass_td = mean(tail(pass_td, window)),
               r_rush_yds = mean(tail(rush_yds, window)), r_rush_td = mean(tail(rush_td, window)),
               r_rec_yds = mean(tail(rec_yds, window)), r_rec_td = mean(tail(rec_td, window)),
               r_receptions = mean(tail(receptions, window)), r_carries = mean(tail(carries, window)),
               last_pos = pos[.N]), by = athlete_id]
  agg
}

# Predict dk_pts for a feature-built pool (must have pos + CFB_FEATS columns).
ncaaf_predict <- function(pool) {
  m <- if (file.exists(NCAAF_MODEL_PATH())) readRDS(NCAAF_MODEL_PATH()) else NULL
  if (is.null(m)) return(NULL)
  P <- as.data.table(copy(pool)); P[, proj := NA_real_]
  for (p in names(m$models)) {
    idx <- which(P$pos == p)
    if (length(idx)) P$proj[idx] <- pmax(as.numeric(predict(m$models[[p]], P[idx])), 0)
  }
  P$proj
}
