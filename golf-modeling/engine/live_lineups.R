#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS v2 — engine/live_lineups.R
# Build top-N sim-optimized DK lineups from a v2 projection (this-week live slate,
# or a historical event). Reuses v1's proven ILP optimizer + Monte-Carlo field sim
# + diversified portfolio (dfs_pipeline_v2.R) fed by the v2 projection.
#
#   Rscript engine/live_lineups.R --this          # live DataGolf slate (before R1)
#   Rscript engine/live_lineups.R --event 525 2026 # a historical full-tournament slate
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
if (!nzchar(Sys.getenv("DATAGOLF_API_KEY")))
  for (p in c(".Renviron","../DFS ENGINE/.Renviron","~/.Renviron"))
    if (file.exists(p)) { readRenviron(p); if (nzchar(Sys.getenv("DATAGOLF_API_KEY"))) break }
Sys.setenv(ENGINE_SOURCE_ONLY="1", SKILL_SOURCE_ONLY="1", COURSEFIT_SOURCE_ONLY="1",
           MARKET_SOURCE_ONLY="1", SIMULATE_SOURCE_ONLY="1", PROJECT_SOURCE_ONLY="1")
if (!exists("project_pool")) source("engine/project.R")
Sys.setenv(GOLF_DFS_SOURCE_ONLY="1")
if (!exists("make_candidates")) source("dfs_pipeline_v2.R")   # ilp/candidates/field-sim/portfolio
emsg <- get0("emsg", ifnotfound=function(...) cat(..., "\n"))
OUT <- "golf_picks"

.z <- function(x){ s<-sd(x,na.rm=TRUE); if(!is.finite(s)||s==0) rep(0,length(x)) else (x-mean(x,na.rm=TRUE))/s }

# adapt a v2 projection into the sl structure v1's optimizer/sim expects
prep_sl <- function(pj) {
  sl <- as.data.table(copy(pj))
  if (!"own" %in% names(sl)) sl[, own := NA_real_]
  if ("ownership" %in% names(sl))
    sl[!is.finite(own) & is.finite(ownership), own := pmax(ownership,0.1)/100]
  if (all(!is.finite(sl$own)) || max(sl$own,na.rm=TRUE) <= 0.0015) {      # no feed ownership
    # use our TRAINED ownership model (walk-forward cor .81 vs actual) instead of a
    # hand-tuned synthetic -> realistic concentration -> the field sim punishes chalk.
    if (!exists("predict_ownership")) source("engine/ownership.R")
    sl[, own := tryCatch(predict_ownership(sl), error=function(e) {
      val<-proj/pmax(salary/1000,1); w<-exp(pmax(1.35*.z(proj)+1.05*.z(val),-3)); pmin(6*w/sum(w),0.40) })]
  }
  sl[, `:=`(model_mean=proj, model_ceil=ceil, ceil_prem=pmax(ceil-proj,0),
            sim_sd=pmax(sim_sd,4), dg_proj=proj)]
  sl[, leverage := frank(model_ceil)/.N - own]; sl[!is.finite(leverage), leverage:=0]
  sl[]
}

# diversified portfolio with a per-player EXPOSURE CAP so one bust can't sink the set.
# cash lineups (top by p_cash) are uncapped; GPP lineups (top by gpp_ev) capped.
build_portfolio_capped <- function(res, sl, n=10, max_exposure=0.6, cash_frac=0.2) {
  P <- nrow(sl); cap <- ceiling(n * max_exposure); expo <- integer(P); picks <- list()
  is_dupe    <- function(ix) any(vapply(picks, function(p) setequal(p$idx[[1]], ix), logical(1)))
  overlap_ok <- function(ix) all(vapply(picks, function(p) length(intersect(ix, p$idx[[1]]))<=4, logical(1)))
  add <- function(pool, role, count, use_cap) {
    got <- 0L
    for (i in seq_len(nrow(pool))) {
      if (got >= count) break
      ix <- pool$idx[[i]]
      if (is_dupe(ix) || !overlap_ok(ix)) next
      if (use_cap && any(expo[ix] >= cap)) next
      picks[[length(picks)+1L]] <<- c(as.list(pool[i]), role=role)
      expo[ix] <<- expo[ix] + 1L; got <- got + 1L
    }
    got
  }
  n_cash <- max(1L, round(n*cash_frac))
  add(res[order(-p_cash)], "Cash anchor", n_cash, FALSE)
  add(res[order(-gpp_ev)], "GPP (capped)", n - length(picks), TRUE)
  if (length(picks) < n) add(res[order(-gpp_ev)], "GPP fill", n - length(picks), FALSE)  # relax cap to fill
  attr(picks, "expo") <- expo
  picks
}

top_lineups <- function(pj, event, n=10, max_exposure=0.6) {
  sl <- prep_sl(pj)
  cat(sprintf("\n########## %s  (%d players) ##########\n", event, nrow(sl)))
  cat("\n-- top projected players --\n")
  has_mc <- "make_cut" %in% names(sl) && any(is.finite(sl$make_cut))
  tp <- sl[order(-proj)][seq_len(min(14,.N))]
  cols <- list(Player=tp$player_name, Sal=tp$salary, Proj=round(tp$proj,1),
               Ceil=round(tp$ceil,1), Floor=round(tp$floor,1))
  if (has_mc) cols$MC <- round(100*tp$make_cut)
  cols$Own <- round(100*tp$own,1)
  print(as.data.table(cols), row.names=FALSE)
  cands <- make_candidates(sl)
  res   <- simulate_slate(sl, cands)
  picks <- build_portfolio_capped(res, sl, n, max_exposure)
  cat(sprintf("\n-- TOP %d SIM-OPTIMIZED LINEUPS (max exposure %.0f%%) --\n",
              length(picks), 100*max_exposure))
  lu <- rbindlist(lapply(seq_along(picks), function(i){ p<-picks[[i]]; ix<-p$idx[[1]]
    data.table(`#`=i, Role=p$role,
      Lineup=paste(sl$player_name[ix], collapse=", "),
      Sal=p$salary, Proj=round(p$proj,1), Ceil=round(sum(sl$model_ceil[ix]),1),
      Own=round(100*p$avg_own), Cash=round(100*p$p_cash), GPP_EV=round(100*p$gpp_ev),
      Top1=round(100*p$p_top1,1)) }))
  print(lu, row.names=FALSE)
  expo <- attr(picks, "expo")
  cat("\n-- player exposure across the portfolio --\n")
  ex <- data.table(Player=sl$player_name, In=expo)[In>0][order(-In)]
  ex[, Pct := round(100*In/length(picks))]
  print(head(ex, 12), row.names=FALSE)
  invisible(picks)
}

args <- commandArgs(trailingOnly=TRUE)
bundle <- if (file.exists(file.path(OUT,"v2_bundle.rds"))) readRDS(file.path(OUT,"v2_bundle.rds")) else
  train_v2(as.data.table(readRDS(file.path(OUT,"v2_master.rds"))$master))

if ("--this" %in% args) {
  emsg("projecting live DataGolf slate (before R1)...")
  pj <- project_live(bundle, "pga", n_sims=4000L)
  top_lineups(pj, paste0("THIS WEEK (before R1): ", attr(pj,"event")), 10)
} else if ("--event" %in% args) {
  i <- which(args=="--event"); eid <- args[i+1]; yr <- as.integer(args[i+2])
  M <- as.data.table(readRDS(file.path(OUT,"v2_master.rds"))$master)
  pj <- project_event(bundle, M, eid, yr, n_sims=4000L)
  top_lineups(pj, paste0("EVENT ", eid, "/", yr, " (before R1)"), 10)
}
