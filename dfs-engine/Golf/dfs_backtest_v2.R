#!/usr/bin/env Rscript
# ==============================================================================
# DFS BACKTEST v2 — certifies the strategies dfs_pipeline_v2.R deploys
#
# Upgrades over dfs_backtest.R:
#   * Grades the SAME three lineup families the live script enters
#     (cash / balanced / leverage), not just one max-projection lineup.
#   * Tune/validate split: leverage + blend parameters are chosen on
#     2023-2024 ONLY; 2025 is touched once, as the verdict.
#   * Quantile ceiling model (matches live spec), salary floor in optimizer,
#     duplication-aware GPP grading (duplicate field lineups split the prize).
#   * Writes golf_picks/dfs_gates.rds: the go/no-go switches + blend weight
#     the live script obeys. Gates:
#       cash_enabled : 2025 double-up win rate >= 58% over >= 30 slates
#       gpp_enabled  : 2025 leverage-family indicative GPP ROI > 0
#   * setwd() before source() so paths don't depend on launch directory.
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({
  library(data.table); library(xgboost)
}))
setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
Sys.setenv(WF_SOURCE_ONLY = "1")
source("claude/wf_top20.R")                      # helpers; FEATS overridden below
# certify gates on the SAME rich feature set the live pipeline now uses
FEATS <- readRDS(file.path("golf_picks", "rich_selected_feats.rds"))
fill_consistent <- function(D) {
  for (f in intersect(FEATS, names(D))) {
    md <- median(D[[f]], na.rm = TRUE); if (is.na(md)) md <- 0
    D[is.na(get(f)), (f) := md]
  }
  D
}
msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S]"), ..., "\n", sep = " ")
OUTPUT_DIR <- "golf_picks"
SALARY_CAP <- 50000L; SALARY_FLOOR <- 49000L; N_PLAYERS <- 6L
set.seed(42)

ilp_lineup <- function(obj, salary, cap = SALARY_CAP, floor = SALARY_FLOOR, n = N_PLAYERS) {
  if (!requireNamespace("lpSolve", quietly = TRUE)) stop("install.packages('lpSolve')")
  sol <- lpSolve::lp("max", obj,
                     const.mat = rbind(salary, salary, rep(1, length(obj))),
                     const.dir = c("<=", ">=", "="), const.rhs = c(cap, floor, n),
                     all.bin = TRUE)
  if (sol$status != 0) return(NULL)
  which(as.logical(sol$solution))
}

xgb_mat <- function(dt, lab = TRUE) {
  m <- as.matrix(dt[, FEATS, with = FALSE])
  # xgboost 1.7.x errors on an explicit label=NULL; omit the arg when unlabeled.
  if (lab) xgb.DMatrix(m, label = dt$total_pts) else xgb.DMatrix(m)
}

train_pair <- function(tr) {
  base <- list(eta = 0.04, max_depth = 5, min_child_weight = 12,
               subsample = 0.8, colsample_bytree = 0.8, nthread = 2)
  dtr <- xgb_mat(tr)
  m_mean <- xgb.train(c(base, objective = "reg:squarederror"), dtr, nrounds = 600)
  m_ceil <- tryCatch(
    xgb.train(c(base, objective = "reg:quantileerror", quantile_alpha = 0.80),
              dtr, nrounds = 600), error = function(e) NULL)
  list(mean = m_mean, ceil = m_ceil)
}

gpp_payout <- function(pct) fifelse(pct >= .999, 100, fifelse(pct >= .99, 20,
              fifelse(pct >= .97, 6, fifelse(pct >= .93, 3,
              fifelse(pct >= .85, 1.8, fifelse(pct >= .78, 1.2, 0))))))

grade_slate <- function(sl, lev_gamma, blend_w) {
  # projections for this slate are already in sl$model_mean / model_ceil / dg_proj
  sl[, proj := ifelse(!is.na(dg_proj) & dg_proj > 0,
                      blend_w * model_mean + (1 - blend_w) * dg_proj, model_mean)]
  sl[, ceil_prem := pmax(model_ceil - model_mean, 0)]
  own <- pmax(ifelse(is.na(sl$ownership) | sl$ownership <= 0, 0.1, sl$ownership), 0.1) / 100

  builds <- list(
    cash     = sl$proj,
    balanced = sl$proj + 0.5 * sl$ceil_prem,
    leverage = sl$proj + 0.8 * sl$ceil_prem - lev_gamma * own * mean(sl$proj))
  idxs <- lapply(builds, function(o) ilp_lineup(o, sl$salary))
  if (any(vapply(idxs, is.null, logical(1)))) return(NULL)

  act <- sl$total_pts; sal <- sl$salary
  # ownership field with duplication tracking
  M <- 1200L; fkeys <- character(0); ftot <- numeric(0); tries <- 0L
  while (length(ftot) < M && tries < M * 8L) {
    pick <- sample(nrow(sl), N_PLAYERS, prob = own); tries <- tries + 1L
    if (sum(sal[pick]) <= SALARY_CAP) {
      ftot  <- c(ftot, sum(act[pick]))
      fkeys <- c(fkeys, paste(sort(pick), collapse = "-"))
    }
  }
  if (length(ftot) < 200) return(NULL)
  cash_line <- quantile(ftot, 0.5, names = FALSE)

  out <- lapply(names(idxs), function(nm) {
    ix <- idxs[[nm]]; tot <- sum(act[ix])
    key <- paste(sort(ix), collapse = "-")
    dupes <- sum(fkeys == key) + 1L            # we are one of the dupes
    pct <- mean(tot >= ftot)
    data.table(family = nm, actual = tot,
               cashed = as.integer(tot >= cash_line),
               du_roi = ifelse(tot >= cash_line, 0.8, -1.0),
               pctile = pct,
               gpp_roi = gpp_payout(pct) / dupes - 1)        # dupes split the prize
  })
  rbindlist(out)
}

main <- function() {
  msg("=== DFS BACKTEST v2 (walk-forward; tune 2023-24, validate 2025) ===")
  s <- as.data.table(readRDS(file.path(OUTPUT_DIR, "rich_snapshots.rds"))$snap)
  d <- as.data.table(readRDS(file.path(OUTPUT_DIR, "dfs_raw_data.rds")))
  s[, `:=`(event_id = as.character(event_id), year = as.integer(year))]
  d[, `:=`(event_id = as.character(event_id), year = as.integer(year),
           player_id = as.integer(dg_id))]
  dfs <- d[!is.na(total_pts) & !is.na(salary) & salary > 0,
           .(event_id, year, player_id, player_name, salary = as.numeric(salary),
             total_pts = as.numeric(total_pts), ownership = as.numeric(ownership),
             dg_proj = if ("proj_points_total" %in% names(d))
                         as.numeric(proj_points_total) else NA_real_)]
  dfs <- dfs[, .SD[1], by = .(event_id, year, player_id)]
  D <- merge(dfs, s[, c("event_id","year","player_id", FEATS), with = FALSE],
             by = c("event_id","year","player_id"), all.x = TRUE)

  # walk-forward model predictions per test year
  D[, `:=`(model_mean = NA_real_, model_ceil = NA_real_)]
  for (Y in 2023:2025) {
    tr <- fill_consistent(copy(D[year < Y & !is.na(sg_last_24)]))
    if (nrow(tr) < 1500) next
    mdl <- train_pair(tr)
    te_i <- which(D$year == Y & !is.na(D$sg_last_24))
    te <- fill_consistent(copy(D[te_i]))
    Xte <- xgb_mat(te, lab = FALSE)
    D$model_mean[te_i] <- predict(mdl$mean, Xte)
    D$model_ceil[te_i] <- if (!is.null(mdl$ceil)) predict(mdl$ceil, Xte)
                          else D$model_mean[te_i] + 0.84 * sd(tr$total_pts - predict(mdl$mean, xgb_mat(tr, FALSE)))
    msg("  year ", Y, ": projected ", length(te_i), " player-slots")
  }
  B <- D[!is.na(model_mean)]
  slates <- unique(B[, .(event_id, year)])

  run_config <- function(years, lev_gamma, blend_w) {
    res <- list()
    for (i in seq_len(nrow(slates))) {
      if (!slates$year[i] %in% years) next
      sl <- copy(B[event_id == slates$event_id[i] & year == slates$year[i]])
      sl <- sl[!is.na(total_pts) & salary > 0]
      if (nrow(sl) < 25) next
      g <- grade_slate(sl, lev_gamma, blend_w)
      if (!is.null(g)) { g[, `:=`(event_id = slates$event_id[i], year = slates$year[i])]
                         res[[length(res) + 1]] <- g }
    }
    rbindlist(res)
  }

  # ---- TUNE on 2023-2024 ----
  msg("Tuning (2023-2024 only)...")
  grid <- CJ(lev_gamma = c(2, 4, 6, 8), blend_w = c(0.20, 0.35, 0.50))
  tune <- rbindlist(lapply(seq_len(nrow(grid)), function(g) {
    r <- run_config(2023:2024, grid$lev_gamma[g], grid$blend_w[g])
    data.table(grid[g],
               cash_win = mean(r[family == "cash"]$cashed),
               lev_gpp  = mean(r[family == "leverage"]$gpp_roi))
  }))
  print(tune)
  best <- tune[which.max(0.5 * cash_win + 0.5 * pmin(lev_gpp, 0.5))]
  msg("Chosen: gamma=", best$lev_gamma, " blend_w=", best$blend_w)

  # ---- VALIDATE on 2025 (touched once) ----
  msg("Validating on 2025...")
  V <- run_config(2025, best$lev_gamma, best$blend_w)
  summ <- V[, .(slates = .N, cash_win = round(100 * mean(cashed), 1),
                du_roi = round(100 * mean(du_roi), 1),
                gpp_roi = round(100 * mean(gpp_roi), 1),
                mean_pctile = round(mean(pctile), 3)), by = family]
  cat("\n================ 2025 VALIDATION ================\n"); print(summ)

  n_cash <- nrow(V[family == "cash"])
  gates <- list(
    cash_enabled  = n_cash >= 30 && summ[family == "cash"]$cash_win >= 58,
    gpp_enabled   = summ[family == "leverage"]$gpp_roi > 0,
    blend_w_model = best$blend_w,
    lev_gamma     = best$lev_gamma,
    validated_on  = "2025", n_slates = n_cash, run_date = Sys.Date())
  saveRDS(gates, file.path(OUTPUT_DIR, "dfs_gates.rds"))
  fwrite(V, file.path(OUTPUT_DIR, "dfs_backtest_v2_results.csv"))
  cat("\nGates written:\n"); str(gates)
  cat("\nReminder: GPP ROI uses an invented payout curve and a simulated field —",
      "\ntreat it as directional. The cash gate is the trustworthy one.\n")
}
main()
