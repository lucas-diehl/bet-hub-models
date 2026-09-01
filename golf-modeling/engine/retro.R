#!/usr/bin/env Rscript
# ==============================================================================
# GOLF v2 — engine/retro.R   LEAKAGE-FREE contest retro (the --asof guard)
#
# For each past event, trains a bundle on data STRICTLY BEFORE that event's date
# (no lookahead), projects it (no live pulls), builds the top-N lineups ranked by
# PROJECTED GPP ROI (the sim's field grading), and scores them on the ACTUAL DK
# points already stored in the master -- so "how would we have done" is trustworthy
# by construction (never re-scores the live dfs_projections.rds, which leaks).
#   Rscript engine/retro.R [--events "525,100,541"] [--n 20]
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table); library(xgboost) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
Sys.setenv(ENGINE_SOURCE_ONLY="1", SKILL_SOURCE_ONLY="1", COURSEFIT_SOURCE_ONLY="1",
           MARKET_SOURCE_ONLY="1", SIMULATE_SOURCE_ONLY="1", PROJECT_SOURCE_ONLY="1", OWNERSHIP_SOURCE_ONLY="1")
if (!exists("project_pool")) source("engine/project.R")
source("engine/live_lineups.R")          # prep_sl, make_candidates, simulate_slate, ilp_lineup
emsg <- get0("emsg", ifnotfound=function(...) cat(..., "\n")); OUT <- "golf_picks"
a <- commandArgs(trailingOnly=TRUE)
NL <- { i<-which(a=="--n"); if(length(i)) as.integer(a[i+1]) else 20L }

M <- as.data.table(readRDS(file.path(OUT,"v2_master.rds"))$master)
evs_arg <- { i<-which(a=="--events"); if(length(i)) strsplit(a[i+1],",")[[1]] else NULL }
if (is.null(evs_arg)) {   # auto: 3 most recent MAIN events (field>=120) with DFS labels
  cat_ev <- M[!is.na(total_pts), .(date=max(event_date), field=.N), by=.(event_id,year)][field>=120][order(-date)]
  picks <- head(cat_ev, 3)
} else picks <- M[event_id %in% evs_arg, .(date=max(event_date)), by=.(event_id,year)][order(-date)]
emsg("retro events: ", paste(picks$event_id, picks$year, sep="/", collapse=", "))

allsum <- list()
for (k in seq_len(nrow(picks))) {
  eid <- picks$event_id[k]; yr <- picks$year[k]; edate <- picks$date[k]
  e <- M[event_id==eid & year==yr & !is.na(total_pts) & !is.na(salary) & salary>0]
  if (nrow(e) < 40) next
  emsg(sprintf("\n=== event %s/%s (%s, %d players) — training on data before it ===", eid, yr, edate, nrow(e)))
  b <- train_v2(M[event_date < edate], calibrate_level=FALSE)     # LEAKAGE-FREE
  if (!"course_par" %in% names(e)) e[, course_par := 71L]
  pj <- project_pool(b, e, n_sims=4000L, calibrate=TRUE)
  pj <- merge(pj, e[, .(player_id, actual=total_pts)], by="player_id", all.x=TRUE)
  pj[!is.finite(actual), actual := 0]
  # historical golf ownership in the master is degenerate/near-zero -> use OUR trained
  # ownership model (cor .81) so the field sim (hence projected GPP ROI) is meaningful.
  pj[, own := tryCatch(predict_ownership(pj), error=function(e) NA_real_)]
  sl <- prep_sl(pj)
  cands <- make_candidates(sl); res <- simulate_slate(sl, cands)
  top <- res[order(-gpp_ev)][seq_len(min(NL, .N))]               # TOP-N by PROJECTED GPP ROI
  top[, actual := vapply(idx, function(ix) sum(sl$actual[ix]), numeric(1))]
  opt <- ilp_lineup(sl$actual, sl$salary); optscore <- sum(sl$actual[opt])
  cat(sprintf("  top-%d-by-projected-ROI: actual best %.1f | median %.1f | mean %.1f | hindsight-opt %.1f\n",
      nrow(top), max(top$actual), median(top$actual), mean(top$actual), optscore))
  cat("  #  proj_roi%  avg_own%  ACTUAL   lineup\n")
  for (i in seq_len(min(8L,nrow(top)))) { ix<-top$idx[[i]]
    cat(sprintf("  %2d  %+7.0f  %6.0f  %6.1f   %s\n", i, 100*top$gpp_ev[i], 100*top$avg_own[i],
        top$actual[i], paste(sl$player_name[ix], collapse=", "))) }
  # did projected ROI predict actual? (rank cor over all candidates)
  res[, act := vapply(idx, function(ix) sum(sl$actual[ix]), numeric(1))]
  rc  <- suppressWarnings(cor(res$gpp_ev, res$act, method="spearman", use="complete.obs"))
  rcp <- suppressWarnings(cor(res$proj,  res$act, method="spearman", use="complete.obs"))
  # top-20 by projected POINTS (the model's core strength, not the leverage ranking)
  topp <- res[order(-proj)][seq_len(min(NL,.N))]
  cat(sprintf("  proj-POINTS vs actual spearman = %.3f | proj-ROI vs actual = %.3f\n", rcp, rc))
  cat(sprintf("  top-%d by projected POINTS: actual best %.1f | median %.1f | mean %.1f\n",
      NL, max(topp$act), median(topp$act), mean(topp$act)))
  allsum[[k]] <- data.table(event=paste0(eid,"/",yr),
                            roi_best=max(top$actual), roi_mean=round(mean(top$actual),1),
                            pts_best=max(topp$act), pts_mean=round(mean(topp$act),1),
                            opt=optscore, pts_cor=round(rcp,3), roi_cor=round(rc,3))
}
S <- rbindlist(allsum)
cat("\n================ RETRO SUMMARY (top-", NL, ") ================\n", sep="")
print(S, row.names=FALSE)
cat(sprintf("\nacross 3 events: by-ROI mean-best %.1f | by-POINTS mean-best %.1f | mean optimal %.1f\n",
    mean(S$roi_best), mean(S$pts_best), mean(S$opt)))
cat(sprintf("proj-POINTS predicts actual (mean spearman %.3f) | proj-ROI (mean %.3f)\n",
    mean(S$pts_cor), mean(S$roi_cor)))
