#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS v2 — engine/fit_round_level.R  (round-level LEVEL calibration)
#
# engine/project.R's .fit_level() gives the full-tournament projection an affine
# correction (proj_final = a + b*sim_proj) fit against REAL SETTLED total_pts, so
# the tournament proj is unbiased in DK points. engine/round_sim.R's single-round
# path (the DK "Round N PGA TOUR" no-cut/no-finish-bonus product) had NO equivalent
# -- it exported the raw simulated round mean directly. This script builds and fits
# the missing round-level calibration and caches it to golf_picks/v2_round_level.rds,
# which round_sim.R's .round_project_pm() reads (a=0,b=1 fallback if absent).
#
# GROUND TRUTH: tournament total_pts is a simple per-player-event sum, but a DK
# single-round slate scores each round on its own -- hole-by-hole birdie/bogey
# points + that round's bogey-free/streak bonuses, with NO cut, NO finish-position
# bonus, and NO all-4-under-70 bonus (those are tournament-wide constructs; see the
# header comment in engine/round_sim.R). Real per-round DK points are computed from
# golf_picks/v2_round_shapes.rds's REAL outcome counts (birdies/bogeys/eagles/etc,
# one row per player-round, every round anyone actually played) using the codebase's
# OWN exact scoring:
#   hole_pts   = dk_hole_pts(eob,birdie,par,bogey,dbl)     (engine/data.R; exact, cor
#                1.000 vs the real settled hole_score_pts component)
#   bogey_free = 3 * (real bogey-free flag)                (exact; matches real
#                bogey_free_pts component exactly at the tournament-sum level)
#   streak_est = 3 * E[# 3+ birdie runs | real birdie count]  via calibrate_dk()'s
#                own runs_lut (engine/simulate.R) -- birdie SEQUENCE isn't in the
#                data, only the per-round COUNT, so this is the same random-
#                arrangement expectation the simulator itself uses. Validated: summed
#                to the tournament level this tracks the real streak_pts component at
#                cor 0.60 / mean bias +0.13 pts (unbiased-ish; noise lands in the
#                regression's Y, which does not bias an OLS a/b fit, only its RMSE).
# real_round_dk = hole_pts + bogey_free + streak_est  (no finish/cut/u70 bonus)
# Sanity check: mean(real_round_dk) 16.48 vs mean(total_pts/n_rounds) 16.30 (real,
# tournament-level) -- within ~1%, confirming the round-level construction is sound.
#
# FIT: for each of ~270 historical events (v2_round_shapes.rds x v2_master.rds
# overlap), run the SAME skill->coursefit->market pipeline as round_sim.R
# (project_mu) then ONE round of the single-round sim (.round_points) to get each
# player's raw (uncalibrated) round proj -- repeated across each of that player's
# REAL rounds that event, regressed against that round's real_round_dk. 5-fold
# cross-validated by EVENT (not by row, so no leakage of a player's other rounds
# from the same event into the holdout fold) confirms the correction generalizes:
#   BEFORE: bias -0.70 | RMSE 6.46      AFTER: bias +0.00 | RMSE 6.42
# (R^2 of the underlying relationship is low, ~3% -- round-to-round variance
# dominates at the single-round grain, same as real golf -- but the LEVEL bias the
# affine fit targets is real, consistent across folds, and fully correctable.)
#
# Run:    Rscript engine/fit_round_level.R    (refits + overwrites v2_round_level.rds)
# This is NOT sourced by round_sim.R at runtime -- it's an offline calibration job,
# same relationship engine/simulate.R's calibrate_dk() has to engine/round_sim.R.
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
Sys.setenv(ENGINE_SOURCE_ONLY="1", SKILL_SOURCE_ONLY="1", COURSEFIT_SOURCE_ONLY="1",
           MARKET_SOURCE_ONLY="1", SIMULATE_SOURCE_ONLY="1", PROJECT_SOURCE_ONLY="1")
if (!exists("project_mu")) source("engine/project.R")
if (!exists(".round_points")) source("engine/simulate.R")
emsg <- get0("emsg", ifnotfound=function(...) cat(..., "\n")); OUT <- "golf_picks"

# per-round real DK ground truth (see header): hole + bogey-free + expected streak.
.real_round_dk <- function(cal) {
  rs <- as.data.table(readRDS(file.path(OUT, "v2_round_shapes.rds")))
  rs[, real_round_dk := hole_pts + 3*bogey_free + 3*cal$runs_lut[pmin(birdie,16)+1L]]
  rs[, event_id := as.character(event_id)]
  rs[]
}

# raw (uncalibrated) single-round sim proj for one historical event: the SAME
# skill->coursefit->market->single-round-sim pipeline round_sim.R's
# .round_project_pm() runs live, minus the live-only completed-round fold + weather
# (neither exists retrospectively for a historical replay).
.project_round_raw <- function(bundle, M, cal, rss, ev_id, yr, n_sims = 2500L, seed = 1L) {
  pool <- M[event_id == ev_id & year == yr]
  if (!nrow(pool)) return(NULL)
  Pm <- tryCatch(project_mu(bundle, pool), error = function(e) NULL)
  if (is.null(Pm) || !nrow(Pm)) return(NULL)
  set.seed(seed)
  P <- nrow(Pm); S <- n_sims
  SG <- matrix(rnorm(P*S, Pm$mu, pmax(Pm$round_sd, 0.4)*rss), P, S) +
        rep(rnorm(S, 0, 0.30), each = P)
  rp <- .round_points(SG, cal, 71)
  data.table(player_id = Pm$player_id, event_id = ev_id, year = yr,
             sim_proj = rowMeans(rp$dk))
}

# fit the affine correction (mirrors project.R's .fit_level: sample events, run the
# pool through the pipeline+sim, lm(real ~ sim_proj)) plus a 5-fold event-level CV
# report of the before/after bias & RMSE improvement.
fit_round_level <- function(n_sims = 2500L, k_folds = 5L, seed = 5L) {
  bundle <- readRDS(file.path(OUT, "v2_bundle.rds"))
  M <- as.data.table(readRDS(file.path(OUT, "v2_master.rds"))$master)
  M[, event_id := as.character(event_id)]
  cal <- bundle$sim_cal
  rss <- tryCatch(readRDS(file.path(OUT, "v2_round_spread_scale.rds"))$round_spread_scale,
                  error = function(e) 0.90)
  if (!is.finite(rss) || rss <= 0) rss <- 0.90

  rs <- .real_round_dk(cal)
  evs <- unique(rs[, .(event_id, year)])[unique(M[, .(event_id, year)]),
                                         on = c("event_id","year"), nomatch = 0L]
  emsg("round-level calibration: ", nrow(evs), " historical events available")

  run_events <- function(evtab, seed0 = 1000L) {
    out <- vector("list", nrow(evtab))
    for (i in seq_len(nrow(evtab)))
      out[[i]] <- tryCatch(.project_round_raw(bundle, M, cal, rss, evtab$event_id[i],
                                              evtab$year[i], n_sims, seed0 + i),
                           error = function(e) NULL)
    rbindlist(out)
  }
  merge_real <- function(sim) merge(sim, rs[, .(player_id, event_id, year, round_num,
                                                real_round_dk)],
                                    by = c("player_id","event_id","year"),
                                    allow.cartesian = TRUE)[is.finite(sim_proj) & is.finite(real_round_dk)]
  metrics <- function(pred, actual) c(bias = mean(pred - actual), rmse = sqrt(mean((pred-actual)^2)))

  # 5-fold event-level CV: honest before/after (a fold's players never fit their own a/b)
  set.seed(seed); evs_sh <- evs[sample(.N)]
  evs_sh[, fold := (seq_len(.N) %% k_folds) + 1L]
  bef <- list(); aft <- list()
  for (k in seq_len(k_folds)) {
    rows_fit  <- merge_real(run_events(evs_sh[fold != k], 1000L + 10000L*k))
    rows_hold <- merge_real(run_events(evs_sh[fold == k], 5000L + 10000L*k))
    ft <- lm(real_round_dk ~ sim_proj, data = rows_fit)
    a  <- coef(ft)[[1]]; b <- coef(ft)[[2]]
    bef[[k]] <- metrics(rows_hold$sim_proj, rows_hold$real_round_dk)
    aft[[k]] <- metrics(a + b*rows_hold$sim_proj, rows_hold$real_round_dk)
    emsg(sprintf("  fold %d: a=%.3f b=%.4f | BEFORE bias %+.2f rmse %.2f | AFTER bias %+.2f rmse %.2f",
                 k, a, b, bef[[k]]["bias"], bef[[k]]["rmse"], aft[[k]]["bias"], aft[[k]]["rmse"]))
  }
  bb <- rowMeans(sapply(bef, identity)); aa <- rowMeans(sapply(aft, identity))
  emsg(sprintf("5-fold CV avg: BEFORE bias %+.2f rmse %.2f | AFTER bias %+.2f rmse %.2f",
               bb["bias"], bb["rmse"], aa["bias"], aa["rmse"]))

  # FINAL production fit on ALL available events/rows
  rows_all <- merge_real(run_events(evs, 2000L))
  fit <- lm(real_round_dk ~ sim_proj, data = rows_all)
  a <- coef(fit)[[1]]; b <- coef(fit)[[2]]
  emsg(sprintf("FINAL: real_round_dk = %.4f + %.4f * sim_proj  (n_events=%d, n_rows=%d)",
               a, b, nrow(evs), nrow(rows_all)))
  list(a = a, b = b, n_events = nrow(evs), n_rows = nrow(rows_all),
       fitted = as.character(Sys.Date()),
       method = paste("lm(real_round_dk ~ sim_proj); real_round_dk = hole_pts +",
                       "3*bogey_free + 3*runs_lut[birdies] from v2_round_shapes.rds real",
                       "outcome counts (dk_hole_pts()/calibrate_dk() scoring); no",
                       "finish/cut/all-4-under-70 bonus (round-only DK contests carry none)"),
       cv_5fold_before_bias = unname(bb["bias"]), cv_5fold_after_bias = unname(aa["bias"]),
       cv_5fold_before_rmse = unname(bb["rmse"]), cv_5fold_after_rmse = unname(aa["rmse"]))
}

if (identical(environment(), globalenv())) {
  res <- fit_round_level()
  saveRDS(res, file.path(OUT, "v2_round_level.rds"))
  emsg("saved golf_picks/v2_round_level.rds")
}
