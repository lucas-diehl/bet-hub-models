#!/usr/bin/env Rscript
# ==============================================================================
# GOLF v2 — engine/bet_top20.R   Top-20 outright betting backtest (leakage-free)
#
# Does our v2 model beat the TOP-20 market? Walk-forward (train < event year), so
# no lookahead: p_top20 from the tournament sim on pre-event features, priced against
# DataGolf HISTORICAL top-20 odds (wf_top20_odds_cache.rds, open + close American).
# Bet a player top-20 when our prob > the offered implied prob (EV>0). Grade vs the
# actual finish. Reports win rate, ROI, #bets at several edge thresholds, open & close.
#
#   Rscript engine/bet_top20.R [--sims N] [--pure]   (--pure = model w/o market blend)
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
PURE  <- "--pure" %in% args

.am2dec <- function(am){ am<-as.numeric(am); fifelse(am>0, am/100+1, 100/abs(am)+1) }

# reshape wf_top20_odds_cache.rds (wide X<ev>.<yr>.{player_id,open_am,close_am}) -> long
load_top20_odds <- function(path=file.path(OUT,"wf_top20_odds_cache.rds")) {
  w <- as.data.table(readRDS(path)); cn <- names(w)
  keys <- unique(sub("\\.(player_id|open_am|close_am)$","",cn))
  long <- rbindlist(lapply(keys, function(k){
    parts <- strsplit(sub("^X","",k), ".", fixed=TRUE)[[1]]
    data.table(event_id=parts[1], year=as.integer(parts[2]),
               player_id=suppressWarnings(as.integer(w[[paste0(k,".player_id")]])),
               open_am =suppressWarnings(as.numeric(w[[paste0(k,".open_am")]])),
               close_am=suppressWarnings(as.numeric(w[[paste0(k,".close_am")]])))
  }), fill=TRUE)
  long <- long[!is.na(player_id) & (is.finite(open_am)|is.finite(close_am))]
  long[, `:=`(dec_open=.am2dec(open_am), dec_close=.am2dec(close_am))]
  unique(long[, .(event_id=as.character(event_id), year, player_id, dec_open, dec_close)],
         by=c("event_id","year","player_id"))
}

# walk-forward p_top20 per player-event (cached)
CACHE <- file.path(OUT, if (PURE) "v2_wf_p20_pure.rds" else "v2_wf_p20.rds")
get_wf_p20 <- function() {
  if (file.exists(CACHE)) { emsg("using cached p_top20: ", CACHE); return(readRDS(CACHE)) }
  M <- as.data.table(readRDS(file.path(OUT,"v2_master.rds"))$master); M <- M[is.finite(finish)]
  rows <- list()
  for (Y in 2024:2026) {
    tr <- M[year<Y]; te <- M[year==Y]; if (nrow(tr)<2000||nrow(te)<50) next
    emsg("project ", Y, " ...")
    b <- train_v2(tr, calibrate_level=FALSE); if (PURE) b$market_w <- 0
    evs <- unique(te[, .(event_id, year)])
    for (i in seq_len(nrow(evs))) {
      e <- te[event_id==evs$event_id[i] & year==evs$year[i]]
      if (nrow(e)<40 || sum(e$finish<=20,na.rm=TRUE)<1) next
      if (!"course_par"%in%names(e)) e[,course_par:=71L]
      pj <- tryCatch(project_pool(b, e, n_sims=NSIMS, calibrate=FALSE), error=function(z) NULL)
      if (is.null(pj)) next
      rows[[length(rows)+1]] <- merge(e[,.(event_id,year,player_id,finish)],
                                      pj[,.(player_id,p_top20)], by="player_id")
    }
  }
  P <- rbindlist(rows); saveRDS(P, CACHE); P
}

emsg("=== top-20 betting backtest (", if(PURE)"PURE model" else "full v2", ") ===")
P <- get_wf_p20(); odds <- load_top20_odds()
D <- merge(P, odds, by=c("event_id","year","player_id"))
D[, t20 := as.integer(finish<=20)]
emsg("player-events with model + top20 odds: ", nrow(D),
     " | events ", uniqueN(paste(D$event_id,D$year)))

grade <- function(deccol, label) {
  d <- D[is.finite(get(deccol))]
  d[, `:=`(dec=get(deccol), ev=p_top20*get(deccol)-1)]      # EV per unit stake at offered price
  cat(sprintf("\n---- %s odds ----\n", label))
  cat(sprintf("%-10s %6s %8s %9s %9s\n","edge(EV>)","bets","win%","ROI%","avg_odds"))
  for (thr in c(0, 0.02, 0.05, 0.10)) {
    b <- d[ev > thr]
    if (!nrow(b)) { cat(sprintf("%-10s %6d\n", paste0("EV>",thr), 0)); next }
    prof <- b$t20*(b$dec-1) - (1-b$t20)                     # win: +(dec-1), lose: -1
    cat(sprintf("%-10s %6d %7.1f%% %8.1f%% %8.2f\n", paste0("EV>",thr), nrow(b),
        100*mean(b$t20), 100*mean(prof), mean(b$dec)))
  }
}
grade("dec_open",  "OPEN")
grade("dec_close", "CLOSE")

# baseline: bet EVERY top-20 offer flat (market efficiency check)
cat("\n---- baselines (no model) ----\n")
allo <- D[is.finite(dec_open)]
cat(sprintf("bet ALL @open:  bets %d  win%% %.1f  ROI%% %.1f\n", nrow(allo),
    100*mean(allo$t20), 100*mean(allo$t20*(allo$dec_open-1)-(1-allo$t20))))
emsg("done")
