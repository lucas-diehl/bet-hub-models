#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS v2 — engine/backtest.R  (Phase 6: the go/no-go)
#
# Walk-forward (train < Y, test = Y) over 2024-2026. Two verdicts:
#   1) PROJECTION ACCURACY vs actual DK points -- v2 vs the two honest baselines:
#        * salary line   (the price = the market's implicit projection; the DFS
#                          benchmark you must beat), and
#        * v1 proxy      (xgboost on the rich FEATS -> DK points, the old approach).
#      Reported per-slate (the DFS-relevant metric: ranking WITHIN a slate) + pooled.
#   2) CONTEST ROI -- cash win% + duplication-aware GPP ROI on the historical slates
#      (reuses the v1 backtest's field/grade logic), v2 vs v1.
#
# Writes golf_picks/dfs_gates_v2.rds (cash/gpp switches, market_w, level calib,
# engine="v2") and prints the CUTOVER verdict. Does NOT overwrite dfs_gates.rds --
# flip GOLF_ENGINE_V2=1 (or set gates$engine="v2") to cut over once v2 wins.
#
# Run:  Rscript engine/backtest.R  [--sims N] [--fast]
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table); library(xgboost) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
Sys.setenv(ENGINE_SOURCE_ONLY="1", SKILL_SOURCE_ONLY="1", COURSEFIT_SOURCE_ONLY="1",
           MARKET_SOURCE_ONLY="1", SIMULATE_SOURCE_ONLY="1", PROJECT_SOURCE_ONLY="1")
if (!exists("train_v2")) source("engine/project.R")
emsg <- get0("emsg", ifnotfound = function(...) cat(..., "\n"))
OUT  <- "golf_picks"
args <- commandArgs(trailingOnly = TRUE)
NSIMS <- { i <- which(args == "--sims"); if (length(i)) as.integer(args[i+1]) else 1200L }
FAST  <- "--fast" %in% args

rmse <- function(a,b) sqrt(mean((a-b)^2, na.rm=TRUE))
co   <- function(a,b) suppressWarnings(cor(a,b, use="complete.obs"))
scor <- function(a,b) suppressWarnings(cor(a,b, method="spearman", use="complete.obs"))

# per-slate mean correlation (rank skill within an event = what DFS needs)
per_slate <- function(D, pcol) {
  D <- D[is.finite(get(pcol)) & is.finite(total_pts)]
  s <- D[, .(c = co(get(pcol), total_pts), n = .N), by = .(event_id, year)][n >= 25]
  mean(s$c, na.rm = TRUE)
}

# ── v1 proxy: xgboost on the rich FEATS -> DK points (the old architecture) ────
v1_project <- function(tr, te, feats) {
  fill <- function(D){ for(f in feats){m<-median(D[[f]],na.rm=TRUE); if(!is.finite(m))m<-0
    D[!is.finite(get(f)),(f):=m]}; D }
  tr <- fill(copy(tr)); te <- fill(copy(te))
  dtr <- xgb.DMatrix(as.matrix(tr[,feats,with=FALSE]), label = tr$total_pts)
  m <- xgb.train(list(eta=0.04,max_depth=5,min_child_weight=12,subsample=0.8,
                      colsample_bytree=0.8,nthread=2,objective="reg:squarederror"),
                 dtr, nrounds=500)
  predict(m, as.matrix(te[,feats,with=FALSE]))
}

# ── contest grade (adapted from dfs_backtest_v2::grade_slate) ─────────────────
SAL_CAP <- 50000L; SAL_FLR <- 49000L; NP <- 6L
ilp <- function(obj, salary) {
  s <- lpSolve::lp("max", obj, rbind(salary,salary,rep(1,length(obj))),
                   c("<=",">=","="), c(SAL_CAP,SAL_FLR,NP), all.bin=TRUE)
  if (s$status!=0) NULL else which(as.logical(s$solution))
}
gpp_pay <- function(p) fifelse(p>=.999,100,fifelse(p>=.99,20,fifelse(p>=.97,6,
           fifelse(p>=.93,3,fifelse(p>=.85,1.8,fifelse(p>=.78,1.2,0))))))
grade_slate <- function(sl, gamma=6, lev_ceilw=0.8) {
  sl <- sl[is.finite(proj) & is.finite(total_pts) & salary>0]
  if (nrow(sl) < 25) return(NULL)
  sl[, cp := pmax(ceil - proj, 0)]
  own <- pmax(fifelse(is.finite(sl$ownership) & sl$ownership>0, sl$ownership, 0.1),0.1)/100
  builds <- list(cash=sl$proj, balanced=sl$proj+0.5*sl$cp,
                 leverage=sl$proj+lev_ceilw*sl$cp - gamma*own*mean(sl$proj))
  idxs <- lapply(builds, function(o) ilp(o, sl$salary))
  if (any(vapply(idxs, is.null, logical(1)))) return(NULL)
  act <- sl$total_pts; sal <- sl$salary
  M<-1000L; fk<-character(0); ft<-numeric(0); tries<-0L
  while(length(ft)<M && tries<M*8L){ pk<-sample(nrow(sl),NP,prob=own); tries<-tries+1L
    if(sum(sal[pk])<=SAL_CAP){ft<-c(ft,sum(act[pk])); fk<-c(fk,paste(sort(pk),collapse="-"))}}
  if(length(ft)<200) return(NULL)
  cl <- quantile(ft,0.5,names=FALSE)
  rbindlist(lapply(names(idxs), function(nm){ ix<-idxs[[nm]]; tot<-sum(act[ix])
    dup<-sum(fk==paste(sort(ix),collapse="-"))+1L; pct<-mean(tot>=ft)
    data.table(family=nm, cashed=as.integer(tot>=cl), gpp_roi=gpp_pay(pct)/dup-1, pctile=pct)}))
}

# ── main ──────────────────────────────────────────────────────────────────────
emsg("=== engine/backtest.R — v2 vs v1/salary (walk-forward) + contest ROI ===")
Mfull <- readRDS(file.path(OUT,"v2_master.rds")); M <- as.data.table(Mfull$master); feats <- Mfull$feats
M <- M[!is.na(total_pts)]

ACC <- list(); yrs <- if (FAST) 2026 else 2024:2026
for (Y in yrs) {
  tr <- M[year < Y]; te <- M[year == Y]
  if (nrow(tr) < 2000 || nrow(te) < 50) next
  emsg("year ", Y, ": train ", nrow(tr), " project ", nrow(te), " ...")
  bundle <- train_v2(tr, calibrate_level = FALSE)   # cor/rank are scale-invariant
  evs <- unique(te[, .(event_id, year)])
  pj <- rbindlist(lapply(seq_len(nrow(evs)), function(i){
    e <- te[event_id==evs$event_id[i] & year==evs$year[i]]
    if (nrow(e) < 25) return(NULL)
    if (!"course_par" %in% names(e)) e[, course_par:=71L]
    o <- tryCatch(project_pool(bundle, e, n_sims=NSIMS, calibrate=FALSE),
                  error=function(err) NULL)
    if (is.null(o)) return(NULL)
    merge(o[,.(player_id, v2=proj)], e[,.(player_id,event_id,year,total_pts,salary,ownership)],
          by="player_id")
  }), fill=TRUE)
  pj[, v1 := v1_project(tr, te, feats)[match(pj$player_id, te$player_id)]]
  pj[, sal_base := salary]                        # salary as the price-line projection
  ACC[[as.character(Y)]] <- pj
}
A <- rbindlist(ACC, fill=TRUE)

cat("\n================ PROJECTION ACCURACY (vs actual DK points) ================\n")
cat(sprintf("%-10s %8s %8s %8s\n","metric","v2","v1(xgb)","salary"))
cat(sprintf("%-10s %8.3f %8.3f %8.3f\n","pooled cor", co(A$v2,A$total_pts), co(A$v1,A$total_pts), co(A$sal_base,A$total_pts)))
cat(sprintf("%-10s %8.3f %8.3f %8.3f\n","spearman",  scor(A$v2,A$total_pts), scor(A$v1,A$total_pts), scor(A$sal_base,A$total_pts)))
cat(sprintf("%-10s %8.3f %8.3f %8.3f\n","per-slate", per_slate(A,"v2"), per_slate(A,"v1"), per_slate(A,"sal_base")))
win_acc <- per_slate(A,"v2") >= per_slate(A,"v1") && per_slate(A,"v2") > per_slate(A,"sal_base")
cat(sprintf("  -> v2 %s v1 and %s salary on per-slate rank skill\n",
    ifelse(per_slate(A,"v2")>=per_slate(A,"v1"),"BEATS/ties","LOSES to"),
    ifelse(per_slate(A,"v2")>per_slate(A,"sal_base"),"beats","loses to")))

# ── contest ROI (v2 vs v1) on the historical slates with ownership ────────────
cat("\n================ CONTEST ROI (historical slates) ================\n")
A2 <- copy(A)[is.finite(v2)]
# ceiling proxy for the optimizer's leverage/balanced builds (both models same rule)
A2[, `:=`(v2_ceil = v2 * 1.6, v1_ceil = v1 * 1.6)]
roi_run <- function(pcol, ccol) {
  evs <- unique(A2[, .(event_id, year)])
  rbindlist(lapply(seq_len(nrow(evs)), function(i){
    e <- A2[event_id==evs$event_id[i] & year==evs$year[i]]
    grade_slate(e[, .(proj=get(pcol), ceil=get(ccol), total_pts, salary, ownership)])
  }))
}
Rv2 <- roi_run("v2", "v2_ceil")
Rv1 <- roi_run("v1", "v1_ceil")
smy <- function(R) if (is.null(R)||!nrow(R)) data.table() else R[, .(slates=.N,
  cash_win=round(100*mean(cashed),1), gpp_roi=round(100*mean(gpp_roi),1)), by=family]
cat("\n-- v2 --\n"); print(smy(Rv2), row.names=FALSE)
cat("\n-- v1 --\n"); print(smy(Rv1), row.names=FALSE)

# ── gates + cutover verdict ───────────────────────────────────────────────────
cash_v2 <- if (nrow(Rv2)) mean(Rv2[family=="cash"]$cashed) else NA
gpp_v2  <- if (nrow(Rv2)) mean(Rv2[family=="leverage"]$gpp_roi) else NA
gates <- list(engine="v2", market_w=MARKET_W_DEFAULT,
              cash_enabled = isTRUE(cash_v2 >= 0.55),
              gpp_enabled  = isTRUE(gpp_v2 > 0),
              per_slate_cor = per_slate(A,"v2"),
              v1_per_slate_cor = per_slate(A,"v1"),
              salary_per_slate_cor = per_slate(A,"sal_base"),
              beats_v1 = win_acc, validated_on = paste(range(yrs),collapse="-"),
              run_date = Sys.Date())
# level calibration for the production bundle (once, on all data)
bfull <- train_v2(M, calibrate_level = TRUE)
gates$level <- bfull$level
saveRDS(bfull, file.path(OUT, "v2_bundle.rds"))
saveRDS(gates, file.path(OUT, "dfs_gates_v2.rds"))

cat("\n================ CUTOVER VERDICT ================\n")
cat(sprintf("v2 per-slate rank skill %.3f | v1 %.3f | salary %.3f\n",
    gates$per_slate_cor, gates$v1_per_slate_cor, gates$salary_per_slate_cor))
cat(sprintf("cash gate %s | gpp gate %s\n", gates$cash_enabled, gates$gpp_enabled))
if (win_acc) cat("RECOMMEND CUTOVER: set GOLF_ENGINE_V2=1 (or gates$engine='v2').\n") else
  cat("HOLD: v2 does not yet clearly beat v1 -- inspect before cutover.\n")
emsg("wrote dfs_gates_v2.rds + refreshed v2_bundle.rds")
