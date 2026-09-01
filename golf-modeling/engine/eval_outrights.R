#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS v2 — engine/eval_outrights.R
# Does the v2 tournament sim predict OUTRIGHT WINNERS and TOP-20 finishes better
# than v1 (xgboost points), the betting MARKET (win odds), and the SALARY line?
# Walk-forward (train < Y, test = Y, 2024-26). Per-event AUC (rank skill at picking
# winners / top-20), precision@6 (of each model's top-6, how many finished top-20),
# and log-loss for the probabilistic models (v2 sim, market).
#   Rscript engine/eval_outrights.R [--sims N]
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table); library(xgboost) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
Sys.setenv(ENGINE_SOURCE_ONLY="1", SKILL_SOURCE_ONLY="1", COURSEFIT_SOURCE_ONLY="1",
           MARKET_SOURCE_ONLY="1", SIMULATE_SOURCE_ONLY="1", PROJECT_SOURCE_ONLY="1")
if (!exists("train_v2")) source("engine/project.R")
emsg <- get0("emsg", ifnotfound=function(...) cat(..., "\n")); OUT <- "golf_picks"
args <- commandArgs(trailingOnly=TRUE)
NSIMS <- { i<-which(args=="--sims"); if(length(i)) as.integer(args[i+1]) else 2500L }

# rank-based AUC (P(score of a positive > score of a negative)); higher score => label 1
auc <- function(score, label) {
  ok <- is.finite(score) & !is.na(label); score<-score[ok]; label<-label[ok]
  n1 <- sum(label==1); n0 <- sum(label==0); if (n1==0||n0==0) return(NA_real_)
  r <- rank(score); (sum(r[label==1]) - n1*(n1+1)/2) / (n1*n0)
}
prec_at <- function(score, label, k=6) { o <- order(-score); mean(label[head(o,k)]==1) }
logloss <- function(p, y) { p<-pmin(pmax(p,1e-6),1-1e-6); -mean(y*log(p)+(1-y)*log(1-p)) }

# v1 proxy: xgboost on rich FEATS -> DK points
v1_project <- function(tr, te, feats) {
  fill <- function(D){for(f in feats){m<-median(D[[f]],na.rm=TRUE);if(!is.finite(m))m<-0
    D[!is.finite(get(f)),(f):=m]};D}
  tr<-fill(copy(tr[is.finite(total_pts)])); te<-fill(copy(te))   # v1 label = DK pts
  m <- xgb.train(list(eta=.04,max_depth=5,min_child_weight=12,subsample=.8,
    colsample_bytree=.8,nthread=2,objective="reg:squarederror"),
    xgb.DMatrix(as.matrix(tr[,feats,with=FALSE]),label=tr$total_pts), nrounds=500)
  predict(m, as.matrix(te[,feats,with=FALSE]))
}

emsg("=== eval_outrights: winners + top-20, v2 vs v1 vs market vs salary ===")
Mf <- readRDS(file.path(OUT,"v2_master.rds")); M <- as.data.table(Mf$master); feats <- Mf$feats
M <- M[is.finite(finish)]
odds <- load_win_odds()

rows <- list()
for (Y in 2024:2026) {
  tr <- M[year<Y]; te <- M[year==Y]
  if (nrow(tr)<2000 || nrow(te)<50) next
  emsg("year ", Y, " ...")
  bundle <- train_v2(tr, calibrate_level=FALSE)
  te[, v1 := v1_project(tr, te, feats)]
  evs <- unique(te[, .(event_id, year)])
  for (i in seq_len(nrow(evs))) {
    e <- te[event_id==evs$event_id[i] & year==evs$year[i]]
    if (nrow(e) < 40 || sum(e$finish==1,na.rm=TRUE)<1) next
    if (!"course_par" %in% names(e)) e[, course_par:=71L]
    pj <- tryCatch(project_pool(bundle, e, n_sims=NSIMS, calibrate=FALSE), error=function(z) NULL)
    if (is.null(pj)) next
    d <- merge(e[, .(player_id, finish, v1, salary)],
               pj[, .(player_id, sim_win=p_win, p_top20, proj)], by="player_id")
    d[, `:=`(event_id=evs$event_id[i], year=evs$year[i])]
    rows[[length(rows)+1]] <- d
  }
}
A <- rbindlist(rows)
A <- merge(A, odds[, .(event_id, year, player_id, mkt_win=p_win)],
           by=c("event_id","year","player_id"), all.x=TRUE)          # market win prob
A[, `:=`(win = as.integer(finish==1), t20 = as.integer(finish<=20))]

evwise <- function(scorecol, label) {
  s <- A[, .(a=auc(get(scorecol), get(label)), p=prec_at(get(scorecol), get(label), 6)),
         by=.(event_id,year)]
  c(auc=mean(s$a,na.rm=TRUE), prec6=mean(s$p,na.rm=TRUE))
}
cat("\n================ TOP-20 finish ================\n")
cat(sprintf("%-10s  AUC    prec@6\n",""))
for (m in list(c("v2 sim","p_top20"), c("v1 xgb","v1"), c("market","mkt_win"), c("salary","salary"))) {
  r <- evwise(m[2], "t20"); cat(sprintf("%-10s  %.3f  %.3f\n", m[1], r["auc"], r["prec6"])) }

cat("\n================ OUTRIGHT WIN ================\n")
cat(sprintf("%-10s  AUC    winnerTop5%%\n",""))
win_top5 <- function(sc) { s<-A[, .(hit=as.integer(player_id[which.max(get(sc))] %in%
  player_id[finish<=5])), by=.(event_id,year)]; mean(s$hit,na.rm=TRUE) }
for (m in list(c("v2 sim","sim_win"), c("v1 xgb","v1"), c("market","mkt_win"), c("salary","salary"))) {
  r <- evwise(m[2], "win"); cat(sprintf("%-10s  %.3f  %.3f\n", m[1], r["auc"], win_top5(m[2]))) }

cat("\n-- log-loss (probabilistic models only) --\n")
cat(sprintf("top20: v2 %.4f | market(win as proxy) %.4f\n",
    logloss(A$p_top20, A$t20), logloss(pmin(A$mkt_win*4,0.99), A$t20)))
cat(sprintf("win  : v2 %.4f | market %.4f\n",
    logloss(A$sim_win, A$win), logloss(A$mkt_win, A$win)))
emsg("done; player-events graded: ", nrow(A))
