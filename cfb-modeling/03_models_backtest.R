# ============================================================================
# 03_models_backtest.R  —  Phase 3-6: three PPP engines + benchmark bake-off
# ----------------------------------------------------------------------------
# Walk-forward (expanding window) over 2021-2025. For each test season we train
# on 2019..(yr-1) and project every game with:
#   A  pure opponent-adjusted PPD -> projected margin & total  (no training)
#   B  XGBoost regression on PPP matchup features               (market-blind)
#   C  XGBoost regression on B's features + A's projection + market line
#   BENCH  the existing cover/over classifier (from backtest_results.csv)
# Projections -> cover/over probabilities (normal residual model) -> bets.
#
# Reports per model: ATS% & totals% ROI @ -110, projected-margin/total RMSE vs
# the market, calibration Brier, binomial significance, and closing-line value.
# Outputs: ppp_backtest_results.csv, model_bakeoff_summary.csv
# ============================================================================

source("00_ppp_common.R")
set.seed(42)
BACKTEST_YEARS <- 2021:2025
PTS_THRESH     <- c(2, 3, 4, 6)   # points-edge tiers (conviction curve)
# Recency half-life (in "global weeks", ~20/season) for the as-of calibration.
# Inf = weight all history equally (original). Finite = emphasize recent games
# to track the drifting scoring environment (fewer possessions since 2023).
RECENCY_HL     <- as.numeric(Sys.getenv("PPP_RECENCY_HL", "Inf"))
gidx <- function(s, w) (s - 2019) * 20 + w

cat("STEP 3-6: PPP MODEL BAKE-OFF\n"); cat(strrep("=", 78), "\n")

# ----------------------------------------------------------------------------
# Load cached inputs (no pbp, no cfbfastR)
# ----------------------------------------------------------------------------
asof          <- read_rds_retry(file.path(CACHE, "asof_ratings.rds"))
betting_lines <- read_rds_retry(file.path(CACHE, "betting_lines.rds"))
game_info     <- read_rds_retry(file.path(CACHE, "game_info.rds"))
sp_ratings    <- read_rds_retry(file.path(CACHE, "sp_ratings.rds"))
fbs_map       <- make_fbs_mapper(sp_ratings)

game_dates <- game_info %>%
  mutate(game_id = if ("game_id" %in% names(.)) game_id else id,
         season  = if ("season"  %in% names(.)) season  else year,
         week    = if ("week"    %in% names(.)) week    else NA_integer_) %>%
  transmute(game_id, season, week,
            start_date = as.POSIXct(start_date, format = "%Y-%m-%dT%H:%M:%S"),
            home_team, away_team,
            home_score = as.numeric(home_points), away_score = as.numeric(away_points),
            total_points = home_score + away_score)

model_data <- build_model_data(betting_lines, game_dates)
md <- attach_ratings(model_data, asof, fbs_map) %>%
  filter(home_team != "FCS", away_team != "FCS") %>%   # bet only FBS-vs-FBS (real ratings)
  mutate(market_margin_pred = -spread, market_total_pred = over_under)
cat(sprintf("  FBS-vs-FBS games with lines + ratings: %d (seasons %d-%d)\n",
            nrow(md), min(md$season), max(md$season)))

# ----------------------------------------------------------------------------
# Walk-forward: produce projections + probabilities for A / B / C per test year
# ----------------------------------------------------------------------------
bt_list <- list()
for (yr in BACKTEST_YEARS) {
  tr <- md %>% filter(season %in% 2019:(yr-1), !is.na(actual_margin))
  te <- md %>% filter(season == yr,           !is.na(actual_margin))
  if (nrow(te) == 0) next

  # --- B & C: XGBoost regressions trained once on prior seasons. They use the
  #     RAW A projections as features (trees are scale-invariant, so no calibration
  #     needed). B is market-blind; C adds the market line.
  mB_m <- train_reg(tr, FEATS_B, "actual_margin"); mB_t <- train_reg(tr, FEATS_B, "total_points")
  te$proj_margin_B <- predict_reg(mB_m, te, FEATS_B); te$proj_total_B <- predict_reg(mB_t, te, FEATS_B)
  sig_mB <- sd(tr$actual_margin - predict_reg(mB_m, tr, FEATS_B), na.rm = TRUE)
  sig_tB <- sd(tr$total_points  - predict_reg(mB_t, tr, FEATS_B), na.rm = TRUE)
  mC_m <- train_reg(tr, FEATS_C, "actual_margin"); mC_t <- train_reg(tr, FEATS_C, "total_points")
  te$proj_margin_C <- predict_reg(mC_m, te, FEATS_C); te$proj_total_C <- predict_reg(mC_t, te, FEATS_C)
  sig_mC <- sd(tr$actual_margin - predict_reg(mC_m, tr, FEATS_C), na.rm = TRUE)
  sig_tC <- sd(tr$total_points  - predict_reg(mC_t, tr, FEATS_C), na.rm = TRUE)

  # --- A: AS-OF calibration, mirroring production (04_weekly_picks.R). The raw
  #     PPD->points projection is over-extreme AND drifts as the scoring
  #     environment changes (possessions/game fell 11.8->10.8 with 2023+ clock
  #     rules). Recalibrating on ALL games completed before each week — including
  #     the current season to date — removes that time-drift. We keep raw
  #     proj_*_A (B/C features + output) and write calibrated proj_*_A_cal.
  te$proj_margin_A_cal <- NA_real_; te$proj_total_A_cal <- NA_real_
  te$A_spread_prob <- NA_real_;     te$A_totals_prob <- NA_real_
  for (w in sort(unique(te$week))) {
    cal <- bind_rows(tr, te %>% filter(week < w))          # completed strictly before w
    wt  <- if (is.finite(RECENCY_HL))
             0.5 ^ ((gidx(yr, w) - gidx(cal$season, cal$week)) / RECENCY_HL) else rep(1, nrow(cal))
    cm  <- lm(actual_margin ~ proj_margin_A, data = cal, weights = wt)
    ct  <- lm(total_points  ~ proj_total_A,  data = cal, weights = wt)
    s_m <- sd(cal$actual_margin - predict(cm, cal), na.rm = TRUE)
    s_t <- sd(cal$total_points  - predict(ct, cal), na.rm = TRUE)
    idx <- which(te$week == w)
    pm  <- predict(cm, te[idx, ]); pt <- predict(ct, te[idx, ])
    te$proj_margin_A_cal[idx] <- pm; te$proj_total_A_cal[idx] <- pt
    te$A_spread_prob[idx] <- prob_cover(pm, te$spread[idx],     s_m)
    te$A_totals_prob[idx] <- prob_over (pt, te$over_under[idx], s_t)
  }
  # use calibrated A for evaluation/output (raw kept for B/C features)
  te$proj_margin_A <- te$proj_margin_A_cal; te$proj_total_A <- te$proj_total_A_cal

  te <- te %>% mutate(
    B_spread_prob = prob_cover(proj_margin_B, spread, sig_mB),
    B_totals_prob = prob_over (proj_total_B,  over_under, sig_tB),
    C_spread_prob = prob_cover(proj_margin_C, spread, sig_mC),
    C_totals_prob = prob_over (proj_total_C,  over_under, sig_tC))
  bt_list[[as.character(yr)]] <- te
  cat(sprintf("  %d: train=%d test=%d (as-of A calibration)\n", yr, nrow(tr), nrow(te)))
}
bt <- bind_rows(bt_list)

# ----------------------------------------------------------------------------
# Benchmark: existing classifier probabilities from backtest_results.csv
# ----------------------------------------------------------------------------
bench_ok <- file.exists("backtest_results.csv")
if (bench_ok) {
  bench <- read.csv("backtest_results.csv") %>%
    transmute(game_id, BENCH_spread_prob = spread_prob, BENCH_totals_prob = totals_prob)
  bt <- bt %>% left_join(bench, by = "game_id")
}

# ----------------------------------------------------------------------------
# Bake-off report
# ----------------------------------------------------------------------------
eval_model <- function(df, tag, pm_col, pt_col, sp_col, tp_col) {
  s <- summarize_bets(df, sp_col, "home_covered", paste(tag, "spread"), quiet = TRUE)
  t <- summarize_bets(df, tp_col, "over_hit",     paste(tag, "totals"), quiet = TRUE)
  merr <- if (!is.null(pm_col) && pm_col %in% names(df)) proj_error(df, pm_col, "actual_margin") else c(rmse=NA, mae=NA)
  terr <- if (!is.null(pt_col) && pt_col %in% names(df)) proj_error(df, pt_col, "total_points")  else c(rmse=NA, mae=NA)
  tibble(model = tag,
         sp_bets = s$rec_n, sp_win = round(100*s$win_pct,1), sp_roi = round(s$roi,1),
         to_bets = t$rec_n, to_win = round(100*t$win_pct,1), to_roi = round(t$roi,1),
         margin_rmse = round(merr["rmse"],2), total_rmse = round(terr["rmse"],2),
         clv = round(clv_points(df, sp_col),2))
}

cat("\n=== BAKE-OFF SUMMARY (walk-forward 2021-2025, bets @ |edge|>=", MIN_EDGE, ", graded vs close) ===\n", sep="")
summary_tbl <- bind_rows(
  eval_model(bt, "A_ratings",   "proj_margin_A", "proj_total_A", "A_spread_prob", "A_totals_prob"),
  eval_model(bt, "B_ml_blind",  "proj_margin_B", "proj_total_B", "B_spread_prob", "B_totals_prob"),
  eval_model(bt, "C_ml_market", "proj_margin_C", "proj_total_C", "C_spread_prob", "C_totals_prob")
)
# market baseline (RMSE reference) + benchmark classifier
mkt <- tibble(model = "MARKET", sp_bets=NA,sp_win=NA,sp_roi=NA,to_bets=NA,to_win=NA,to_roi=NA,
              margin_rmse = round(proj_error(bt,"market_margin_pred","actual_margin")["rmse"],2),
              total_rmse  = round(proj_error(bt,"market_total_pred","total_points")["rmse"],2), clv=NA)
summary_tbl <- bind_rows(summary_tbl, mkt)
if (bench_ok)
  summary_tbl <- bind_rows(summary_tbl,
    eval_model(bt, "BENCH_clf", NULL, NULL, "BENCH_spread_prob", "BENCH_totals_prob"))
print(as.data.frame(summary_tbl), row.names = FALSE)

cat("\n  (margin_rmse/total_rmse: lower is better; beating MARKET row = real projection edge)\n")

# --- projection-native view: points-edge selection (sigma-free) --------------
pts_edge_eval <- function(df, proj_m, proj_t, thresh) {
  s <- df %>% filter(!is.na(.data[[proj_m]]), !is.na(home_covered)) %>%
    mutate(e = .data[[proj_m]] + spread, bet = abs(e) >= thresh,
           correct = if_else(e > 0, home_covered, 1L - home_covered)) %>% filter(bet)
  t <- df %>% filter(!is.na(.data[[proj_t]]), !is.na(over_hit)) %>%
    mutate(e = .data[[proj_t]] - over_under, bet = abs(e) >= thresh,
           correct = if_else(e > 0, over_hit, 1L - over_hit)) %>% filter(bet)
  roi <- function(x) if (nrow(x)) round((sum(x$correct)*PAYOUT - (nrow(x)-sum(x$correct)))/nrow(x)*100,1) else NA
  pv  <- function(x) if (nrow(x) >= 10) round(binom.test(sum(x$correct), nrow(x), BREAKEVEN_RATE, alternative="greater")$p.value, 3) else NA
  tibble(thresh = thresh,
         sp_bets=nrow(s), sp_win=if(nrow(s))round(100*mean(s$correct),1) else NA, sp_roi=roi(s), sp_p=pv(s),
         to_bets=nrow(t), to_win=if(nrow(t))round(100*mean(t$correct),1) else NA, to_roi=roi(t), to_p=pv(t))
}
for (tag in c("A","B","C")) {
  cat(sprintf("\n  points-edge selection — model %s:\n", tag))
  print(as.data.frame(bind_rows(lapply(PTS_THRESH, function(th)
    pts_edge_eval(bt, paste0("proj_margin_",tag), paste0("proj_total_",tag), th)))), row.names = FALSE)
}

# --- significance + calibration (Brier) on the prob view --------------------
cat("\n=== SIGNIFICANCE (binomial vs 0.524 breakeven) ===\n")
for (tag in c("A","B","C")) {
  sig_test(bt, paste0(tag,"_spread_prob"), "home_covered", paste0(tag," spread"))
  sig_test(bt, paste0(tag,"_totals_prob"), "over_hit",     paste0(tag," totals"))
}
if (bench_ok) {
  sig_test(bt, "BENCH_spread_prob", "home_covered", "BENCH spread")
  sig_test(bt, "BENCH_totals_prob", "over_hit",     "BENCH totals")
}
cat("\n  Brier scores (spread / totals), lower better:\n")
for (tag in c("A","B","C")) {
  bs <- mean((bt[[paste0(tag,"_spread_prob")]] - bt$home_covered)^2, na.rm = TRUE)
  bt_ <- mean((bt[[paste0(tag,"_totals_prob")]] - bt$over_hit)^2, na.rm = TRUE)
  cat(sprintf("    %s: %.4f / %.4f\n", tag, bs, bt_))
}

# ----------------------------------------------------------------------------
# Save game-level results + summary
# ----------------------------------------------------------------------------
out_cols <- c("game_id","season","week","home_team","away_team","spread","spread_feature",
              "over_under","line_movement","actual_margin","total_points","home_covered","over_hit",
              "proj_margin_A","proj_total_A","proj_margin_B","proj_total_B","proj_margin_C","proj_total_C",
              "A_spread_prob","A_totals_prob","B_spread_prob","B_totals_prob","C_spread_prob","C_totals_prob",
              grep("^BENCH_", names(bt), value = TRUE))
write.csv(bt %>% select(any_of(out_cols)), "ppp_backtest_results.csv", row.names = FALSE)
write.csv(summary_tbl, "model_bakeoff_summary.csv", row.names = FALSE)
cat("\n✓ saved ppp_backtest_results.csv and model_bakeoff_summary.csv\n")

cat("\nGO-LIVE GATE: only bet a market/model where ROI>0 AND CLV>0 AND significance p<0.05.\n")
