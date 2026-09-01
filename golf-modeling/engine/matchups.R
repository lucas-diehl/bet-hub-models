#!/usr/bin/env Rscript
# ==============================================================================
# GOLF v2 — engine/matchups.R   3-ball & head-to-head matchup pricing + backtest
#
# v2's strongest suit is RELATIVE ranking (who beats whom) with far less variance
# than outright GPP -- exactly what matchup markets pay for. We price each matchup
# from the v2 tournament sim and backtest, leakage-free (train < event year), vs
# DataGolf HISTORICAL matchup odds (golf_picks/wf_odds_cache/ev_*.rds):
#   * 72-hole Match  -> P(p1 finishes ahead of p2)   from 72-hole rank
#   * R1 3-Ball      -> P(each is best in round 1)    from R1 SG
# Reports vs-market accuracy (AUC / log-loss) + betting ROI (bet when our prob >
# the offered implied prob), open & close prices.
#   Rscript engine/matchups.R [--sims N]
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table); library(xgboost) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
Sys.setenv(ENGINE_SOURCE_ONLY="1", SKILL_SOURCE_ONLY="1", COURSEFIT_SOURCE_ONLY="1",
           MARKET_SOURCE_ONLY="1", SIMULATE_SOURCE_ONLY="1", PROJECT_SOURCE_ONLY="1")
if (!exists("train_v2")) source("engine/project.R")
emsg <- get0("emsg", ifnotfound=function(...) cat(..., "\n")); OUT <- "golf_picks"
a <- commandArgs(trailingOnly=TRUE); NS <- { i<-which(a=="--sims"); if(length(i)) as.integer(a[i+1]) else 6000L }
.am2dec <- function(am){am<-as.numeric(am); fifelse(am>0,am/100+1,100/abs(am)+1)}
auc <- function(s,y){ok<-is.finite(s)&!is.na(y);s<-s[ok];y<-y[ok];n1<-sum(y==1);n0<-sum(y==0)
  if(n1==0||n0==0) return(NA_real_); r<-rank(s); (sum(r[y==1])-n1*(n1+1)/2)/(n1*n0)}
logloss<-function(p,y){p<-pmin(pmax(p,1e-6),1-1e-6);-mean(y*log(p)+(1-y)*log(1-p))}

# ── load historical matchup odds (72-hole H2H + R1 3-ball) ────────────────────
load_matchup_odds <- function() {
  fs <- list.files(file.path(OUT,"wf_odds_cache"), pattern="^ev_.*\\.rds$", full.names=TRUE)
  d <- rbindlist(lapply(fs, function(f) tryCatch(as.data.table(readRDS(f)), error=function(e) NULL)),
                 use.names=TRUE, fill=TRUE)
  if (!nrow(d) || !"bet_type" %in% names(d)) return(list(h2h=data.table(), b3=data.table()))
  d[, `:=`(event_id=as.character(event_id), year=as.integer(year))]
  # NB wf_odds_cache stores DECIMAL odds already (e.g. 1.83, 2.25) -- NOT American.
  h2h <- d[bet_type=="72-hole Match" & !is.na(p1_dg_id) & !is.na(p2_dg_id),
    .(event_id, year, p1=as.integer(p1_dg_id), p2=as.integer(p2_dg_id),
      p1_open=as.numeric(p1_open), p1_close=as.numeric(p1_close),
      p2_open=as.numeric(p2_open), p2_close=as.numeric(p2_close),
      y=suppressWarnings(as.numeric(p1_outcome)))][y %in% c(0,1)]
  b3 <- d[bet_type=="R1 3-Ball" & !is.na(p1_dg_id) & !is.na(p2_dg_id) & !is.na(p3_dg_id),
    .(event_id, year, p1=as.integer(p1_dg_id), p2=as.integer(p2_dg_id), p3=as.integer(p3_dg_id),
      p1o=as.numeric(p1_open), p2o=as.numeric(p2_open), p3o=as.numeric(p3_open),
      w=fcase(p1_outcome==1,1L, p2_outcome==1,2L, p3_outcome==1,3L, default=NA_integer_))][!is.na(w)]
  list(h2h=h2h, b3=b3)
}

# ── light rank sim for an event pool (mu + round_sd) -> 72-hole rank + R1 SG ───
sim_event <- function(pool, n_sims=NS, cut_n=65L, cond_sd=0.35, spread_scale=1.0, seed=1L) {
  set.seed(seed); pl <- as.data.table(pool)[is.finite(mu)&is.finite(round_sd)]
  P <- nrow(pl); if (P<3) return(NULL)
  sd <- pmax(pl$round_sd,0.4)*spread_scale
  draw <- function() matrix(rnorm(P*n_sims, pl$mu, sd),P,n_sims)+rep(rnorm(n_sims,0,cond_sd*spread_scale),each=P)
  R1<-draw();R2<-draw();R3<-draw();R4<-draw(); h36<-R1+R2
  made <- apply(h36,2L,function(c) c >= sort(c,decreasing=TRUE)[min(cut_n,length(c))]-1e-9)
  score <- h36+R3+R4; score[!made] <- h36[!made]-1000
  rnk <- apply(score,2L,function(c) frank(-c,ties.method="first"))
  list(ids=pl$player_id, rank=rnk, r1=R1)
}
.row <- function(s,id) match(id, s$ids)
h2h_prob <- function(s,a,b){ia<-.row(s,a);ib<-.row(s,b); if(is.na(ia)||is.na(ib)) return(NA); mean(s$rank[ia,]<s$rank[ib,])}
b3_prob  <- function(s,ids){r<-.row(s,ids); if(anyNA(r)) return(rep(NA,3)); M<-s$r1[r,,drop=FALSE]
  w<-max.col(t(M),ties.method="first"); tabulate(w,3)/ncol(M)}

# ── walk-forward backtest ─────────────────────────────────────────────────────
emsg("=== matchup backtest (72-hole H2H + R1 3-ball) ===")
M <- as.data.table(readRDS(file.path(OUT,"v2_master.rds"))$master); M<-M[is.finite(finish)]
sc <- tryCatch(readRDS(file.path(OUT,"v2_spread_scale.rds")), error=function(e) 1.0)
odds <- load_matchup_odds(); emsg("H2H rows: ", nrow(odds$h2h), " | R1 3-ball rows: ", nrow(odds$b3))
H <- list(); B <- list()
for (Y in 2024:2026) {
  tr<-M[year<Y]; te<-M[year==Y]; if(nrow(tr)<2000||nrow(te)<50) next
  emsg("year ", Y, " ..."); b<-train_v2(tr, calibrate_level=FALSE); b$spread_scale<-sc
  evs <- unique(te[,.(event_id,year)])
  for (i in seq_len(nrow(evs))) {
    e<-te[event_id==evs$event_id[i]&year==evs$year[i]]; if(nrow(e)<30) next
    if(!"course_par"%in%names(e)) e[,course_par:=71L]
    Pm<-tryCatch(project_mu(b,e), error=function(z) NULL); if(is.null(Pm)) next
    s<-sim_event(Pm[,.(player_id,mu,round_sd)], spread_scale=sc); if(is.null(s)) next
    hh<-odds$h2h[event_id==evs$event_id[i]&year==evs$year[i]]
    if(nrow(hh)){ hh[, p_model := mapply(function(a,b2) h2h_prob(s,a,b2), p1, p2)]; H[[length(H)+1]]<-hh }
    bb<-odds$b3[event_id==evs$event_id[i]&year==evs$year[i]]
    if(nrow(bb)){ pr<-t(mapply(function(a,b2,c) b3_prob(s,c(a,b2,c)), bb$p1,bb$p2,bb$p3))
      bb[, `:=`(pm1=pr[,1],pm2=pr[,2],pm3=pr[,3])]; B[[length(B)+1]]<-bb }
  }
}
H<-rbindlist(H); B<-rbindlist(B)
saveRDS(list(h2h=H, b3=B), file.path(OUT,"v2_matchup_graded.rds"))

# threshold sweep: bet a leg when our prob * offered odds - 1 > thr
sweep <- function(prob, dec, y) {
  ev <- prob*dec - 1
  cat(sprintf("  %-8s %6s %8s %9s %9s\n","edge","bets","win%","ROI%","avg_odds"))
  for (thr in c(0, 0.03, 0.07, 0.12)) {
    bet <- ev > thr & is.finite(dec) & is.finite(prob)
    if (!sum(bet)) { cat(sprintf("  EV>%.2f       0\n", thr)); next }
    pr <- y[bet]*(dec[bet]-1) - (1-y[bet])
    cat(sprintf("  EV>%.2f  %6d %7.1f%% %8.1f%% %9.2f\n",
        thr, sum(bet), 100*mean(y[bet]), 100*mean(pr), mean(dec[bet])))
  }
}

cat("\n================ 72-HOLE HEAD-TO-HEAD ================\n")
H<-H[is.finite(p_model)]
cat(sprintf("graded %d | MODEL AUC %.3f vs MARKET AUC %.3f (market = 1/p1_open)\n",
    nrow(H), auc(H$p_model,H$y), auc(1/H$p1_open, H$y)))
# bet BOTH sides: p1 leg (prob=p_model) + p2 leg (prob=1-p_model)
Hl <- rbindlist(list(
  H[, .(prob=p_model,   dec=p1_open, y=y)],
  H[, .(prob=1-p_model, dec=p2_open, y=1L-y)]))[is.finite(prob)&is.finite(dec)]
cat("-- betting (both sides, open odds) --\n"); sweep(Hl$prob, Hl$dec, Hl$y)

cat("\n================ R1 3-BALL ================\n")
if (nrow(B)) {
  L <- rbindlist(list(
    B[,.(prob=pm1, dec=p1o, y=as.integer(w==1))],
    B[,.(prob=pm2, dec=p2o, y=as.integer(w==2))],
    B[,.(prob=pm3, dec=p3o, y=as.integer(w==3))]))[is.finite(prob)&is.finite(dec)]
  cat(sprintf("legs graded %d | MODEL AUC %.3f\n", nrow(L), auc(L$prob,L$y)))
  cat("-- betting (all legs, open odds) --\n"); sweep(L$prob, L$dec, L$y)
}
emsg("done")
