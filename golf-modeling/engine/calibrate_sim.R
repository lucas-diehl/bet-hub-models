#!/usr/bin/env Rscript
# ==============================================================================
# GOLF v2 — engine/calibrate_sim.R   Win-TAIL calibration of the tournament sim
#
# The sim over-states longshot top-20/win probabilities (top-20 bets lost; outright
# AUC trailed the market). Cause: the round-SG spread. We grid-search a spread_scale
# (multiplies round_sd + cond_sd) and pick the value whose simulated top-20 / win /
# make-cut probabilities best match ACTUAL frequencies (log-loss + calibration),
# walk-forward on 2025-26. The winner is written to the bundle's spread_scale.
#   Rscript engine/calibrate_sim.R [--events N] [--sims S]
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table); library(xgboost) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
Sys.setenv(ENGINE_SOURCE_ONLY="1", SKILL_SOURCE_ONLY="1", COURSEFIT_SOURCE_ONLY="1",
           MARKET_SOURCE_ONLY="1", SIMULATE_SOURCE_ONLY="1", PROJECT_SOURCE_ONLY="1")
if (!exists("train_v2")) source("engine/project.R")
emsg <- get0("emsg", ifnotfound=function(...) cat(..., "\n")); OUT <- "golf_picks"
a <- commandArgs(trailingOnly=TRUE)
NEV <- { i<-which(a=="--events"); if(length(i)) as.integer(a[i+1]) else 24L }
NS  <- { i<-which(a=="--sims");   if(length(i)) as.integer(a[i+1]) else 2500L }
SCALES <- c(0.80, 0.90, 1.00, 1.10, 1.25)
logloss <- function(p,y){p<-pmin(pmax(p,1e-6),1-1e-6); -mean(y*log(p)+(1-y)*log(1-p))}

M <- as.data.table(readRDS(file.path(OUT,"v2_master.rds"))$master); M <- M[is.finite(finish)]
rows <- list()
for (Y in 2025:2026) {
  tr <- M[year<Y]; te <- M[year==Y]; if (nrow(tr)<2000||nrow(te)<50) next
  emsg("year ", Y, " ...")
  b <- train_v2(tr, calibrate_level=FALSE)
  evs <- unique(te[, .(event_id, year)]); set.seed(1); evs <- evs[sample(.N, min(NEV, .N))]
  for (i in seq_len(nrow(evs))) {
    e <- te[event_id==evs$event_id[i] & year==evs$year[i]]
    if (nrow(e)<40) next; if (!"course_par"%in%names(e)) e[,course_par:=71L]
    for (s in SCALES) {
      b$spread_scale <- s
      pj <- tryCatch(project_pool(b, e, n_sims=NS, calibrate=FALSE), error=function(z) NULL)
      if (is.null(pj)) next
      d <- merge(e[,.(player_id, finish)], pj[,.(player_id, p_top20, p_win, make_cut)], by="player_id")
      d[, `:=`(scale=s, t20=as.integer(finish<=20), win=as.integer(finish==1),
               mc=as.integer(finish<=65))]     # made-cut proxy: finished (<=~cut)
      rows[[length(rows)+1]] <- d
    }
  }
}
A <- rbindlist(rows)
cat("\n================ SPREAD-SCALE CALIBRATION ================\n")
cat(sprintf("%-7s %10s %10s %10s %12s %10s\n","scale","t20_ll","win_ll","mc_cal","t20_slope","score"))
res <- rbindlist(lapply(SCALES, function(s){
  d <- A[scale==s]
  # calibration slope: regress actual t20 on predicted p_top20 (1 = perfect)
  sl <- tryCatch(coef(lm(t20 ~ p_top20, d))[2], error=function(e) NA)
  data.table(scale=s, t20_ll=logloss(d$p_top20,d$t20), win_ll=logloss(d$p_win,d$win),
             mc_cal=mean(d$make_cut)-mean(d$mc), t20_slope=sl)
}))
res[, score := scale(t20_ll)[,1] + scale(win_ll)[,1]]      # minimise tail log-loss
for (i in seq_len(nrow(res))) cat(sprintf("%-7.2f %10.4f %10.4f %+10.3f %12.2f %10.3f\n",
  res$scale[i], res$t20_ll[i], res$win_ll[i], res$mc_cal[i], res$t20_slope[i], res$score[i]))
best <- res[which.min(t20_ll + win_ll)]$scale
cat(sprintf("\nBEST spread_scale = %.2f (min tail log-loss). Baseline was 1.00.\n", best))
saveRDS(best, file.path(OUT, "v2_spread_scale.rds"))
emsg("saved v2_spread_scale.rds")
