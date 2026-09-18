#!/usr/bin/env Rscript
# ==============================================================================
# RICH FEATURES  (Roadmap Phase 3) — creative, leakage-free, from rich_rounds_long
#
# Every rolling feature uses shift(,1): the current round/event is EXCLUDED.
# Event snapshot = round 1 state -> no round from the predicted event leaks in.
# Produces golf_picks/rich_snapshots.rds + the RICH_FEATS vector.
#
# Moved here from claude/ (gitignored wholesale for ~18 OTHER scripts that
# hardcode the DataGolf key) -- this script makes no network calls at all, it
# only reshapes golf_picks/rich_rounds_long.rds, so it was safe to move and is
# needed so the daily train.yml retrain can actually refresh rich_snapshots.rds
# instead of re-deriving from a copy frozen at its 2026-09-01 commit.
#
# Source: Sys.setenv(RICH_SOURCE_ONLY="1"); source("engine/rich_features.R")
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages(library(data.table)))
if (dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S]"), ..., "\n", sep="")
OUT <- "golf_picks"

lagroll  <- function(x, n) frollmean(shift(x, 1L), n, align="right", na.rm=TRUE)
lagsd    <- function(x, n) { s<-shift(x,1L); if (length(s)>=n) frollapply(s,n,sd,align="right") else rep(NA_real_,length(s)) }
lagrate  <- function(x, n, thr) frollmean(shift(as.numeric(x>thr),1L), n, align="right", na.rm=TRUE)

build_rich <- function() {
  long <- as.data.table(readRDS(file.path(OUT, "rich_rounds_long.rds")))
  long[, finish := suppressWarnings(as.integer(gsub("[^0-9]","",fin_text)))]
  long[is.na(finish), finish := 999L]
  setorder(long, player_id, event_date, round_num)
  g <- "player_id"
  msg("rich rounds: ", nrow(long), " | players ", uniqueN(long$player_id))

  # ---- core SG rolling (lagged) ----
  for (n in c(4,8,12,24,50,100)) long[, paste0("sg_last_",n) := lagroll(sg_total,n), by=g]
  for (c in c("sg_putt","sg_app","sg_ott","sg_arg","sg_t2g"))
    long[, paste0(c,"_24") := lagroll(get(c),24), by=g]
  long[, form       := sg_last_12 - sg_last_24]
  long[, long_form  := sg_last_24 - sg_last_100]
  long[, form_short := sg_last_4  - sg_last_12]
  long[, sg_sd_24   := lagsd(sg_total,24), by=g]
  long[, great_round_rate := lagrate(sg_total,24,2), by=g]
  long[, sg_p75_24  := { s<-shift(sg_total,1L); if(.N>=24) frollapply(s,24,function(z)quantile(z,.75,na.rm=TRUE),align="right") else rep(NA_real_,.N) }, by=g]
  long[, sg_ewma := { s<-shift(sg_total,1L); o<-rep(NA_real_,.N)
    if(.N>=2) for(j in 2:.N){lo<-max(1,j-23);xs<-s[lo:j];xs<-xs[!is.na(xs)];if(length(xs)){w<-0.95^rev(seq_along(xs)-1);o[j]<-sum(xs*w)/sum(w)}}; o }, by=g]

  # ---- PERSONALIZED FORM: recent SG vs the player's OWN career baseline (not the
  #      field). Validated ~2x the predictive strength of rolling form; window 12
  #      most significant/reliable. Expanding mean, lagged -> leakage-free. ----
  long[, cs_ := cumsum(fcoalesce(sg_total,0)), by=g]
  long[, cn_ := cumsum(!is.na(sg_total)),      by=g]
  long[, career_sg := shift(cs_/cn_, 1L), by=g]
  long[, pform_12  := sg_last_12 - career_sg]
  long[, c("cs_","cn_") := NULL]

  # ---- PRECISION / "miss magnitude" (your idea #1) ----
  long[, prox_fw_24  := lagroll(prox_fw,24),  by=g]
  long[, prox_rgh_24 := lagroll(prox_rgh,24), by=g]
  long[, rough_penalty_24 := prox_rgh_24 - prox_fw_24]
  long[, driving_acc_24  := lagroll(driving_acc,24),  by=g]
  long[, driving_dist_24 := lagroll(driving_dist,24), by=g]
  long[, wildness_cost_24 := (1 - driving_acc_24) * rough_penalty_24]

  # ---- SHOT QUALITY / "relevant shots" (your idea #2) ----
  long[, great_shot_rate_24 := lagroll(great_shots,24), by=g]
  long[, poor_shot_rate_24  := lagroll(poor_shots,24),  by=g]
  long[, signal_to_noise_24 := great_shot_rate_24 / (poor_shot_rate_24 + 1)]

  # ---- SCORING SHAPE (variance the simulator wants) ----
  long[, ceiling_rate_24  := lagroll(eagles_or_better + birdies, 24), by=g]
  long[, disaster_rate_24 := lagroll(doubles_or_worse, 24), by=g]
  long[, bogey_avoid_24   := lagroll(1 - (bogies + doubles_or_worse)/18, 24), by=g]
  long[, birdie_rate_24   := lagroll(birdies,24), by=g]
  long[, bogey_rate_24    := lagroll(bogies,24),  by=g]

  # ---- SHORT GAME ----
  long[, scrambling_24 := lagroll(scrambling,24), by=g]
  long[, gir_24        := lagroll(gir,24), by=g]

  # ---- CLUTCH: weekend (R3-4) minus early (R1-2), rolling lagged over events ----
  wd <- long[, .(wd = mean(sg_total[round_num>=3],na.rm=TRUE) - mean(sg_total[round_num<=2],na.rm=TRUE)),
             by=.(player_id, event_id, year, event_date)]
  setorder(wd, player_id, event_date)
  wd[, weekend_delta := frollmean(shift(wd,1L), 12, align="right", na.rm=TRUE), by=player_id]

  # ---- MOMENTUM/CONGESTION: starts in the prior 35 days (validated early-market
  #      matchup signal; recent activity the opening line lags). Leakage-free:
  #      counts only events strictly before this one's known start date. ----
  ev <- long[round_num==1L, .(player_id, event_id, year, event_date)]
  setorder(ev, player_id, event_date)
  ev[, congestion_35 := {
       d <- as.integer(event_date)
       vapply(seq_along(d), function(i) sum(d[seq_len(i-1)] >= d[i]-35 & d[seq_len(i-1)] < d[i]), integer(1))
     }, by=player_id]

  # ---- EVENT SNAPSHOT (round 1 = pre-event) ----
  PF <- c("sg_last_4","sg_last_8","sg_last_12","sg_last_24","sg_last_50","sg_last_100",
          "career_sg","pform_12",
          "sg_putt_24","sg_app_24","sg_ott_24","sg_arg_24","sg_t2g_24",
          "form","long_form","form_short","sg_ewma","sg_sd_24","great_round_rate","sg_p75_24",
          "prox_fw_24","prox_rgh_24","rough_penalty_24","driving_acc_24","driving_dist_24","wildness_cost_24",
          "great_shot_rate_24","poor_shot_rate_24","signal_to_noise_24",
          "ceiling_rate_24","disaster_rate_24","bogey_avoid_24","birdie_rate_24","bogey_rate_24",
          "scrambling_24","gir_24","course_num")
  snap <- long[round_num==1L, c("player_id","event_id","year","event_date","finish",PF), with=FALSE]
  snap <- merge(snap, wd[, .(player_id,event_id,year,weekend_delta)], by=c("player_id","event_id","year"), all.x=TRUE)
  snap <- merge(snap, ev[, .(player_id,event_id,year,congestion_35)], by=c("player_id","event_id","year"), all.x=TRUE)

  # realised target (this event mean SG) for the regression engine
  agg <- long[!is.na(sg_total), .(target_sg=mean(sg_total)), by=.(player_id,event_id,year)]
  snap <- merge(snap, agg, by=c("player_id","event_id","year"))
  snap[, top20 := as.integer(finish<=20)]

  # ---- COURSE features (prior-only player history; static course profiles) ----
  # course profile = its characteristic vector from all rounds at the course
  cp <- long[!is.na(course_num), .(
    c_score = mean(score - course_par, na.rm=TRUE),
    c_len   = mean(driving_dist, na.rm=TRUE),
    c_prox  = mean(prox_fw, na.rm=TRUE),
    c_rough = mean(prox_rgh - prox_fw, na.rm=TRUE),
    c_gir   = mean(gir, na.rm=TRUE)), by=course_num]
  snap <- merge(snap, cp, by="course_num", all.x=TRUE)

  # player's prior SG at THIS course (leakage-safe: events strictly earlier)
  pe <- long[round_num==1L, .(player_id, course_num, event_date)]
  ps <- agg[long[round_num==1L,.(player_id,event_id,year,course_num,event_date)],
            on=.(player_id,event_id,year)]
  setorder(ps, player_id, course_num, event_date)
  ps[, player_course_sg := frollmean(shift(target_sg,1L), 20, align="right", na.rm=TRUE), by=.(player_id,course_num)]
  ps[, player_course_n  := shift(seq_len(.N),1L), by=.(player_id,course_num)]
  ps[is.na(player_course_n), player_course_n := 0]
  snap <- merge(snap, ps[, .(player_id,event_id,year,player_course_sg,player_course_n)],
                by=c("player_id","event_id","year"), all.x=TRUE)

  # course_fit: player strengths aligned with course demands (vectorized).
  # long course rewards distance; high-rough penalises wildness; hard greens
  # (far proximity) reward approach; easy/birdie courses reward upside.
  zc <- function(x){ x<-as.numeric(x); m<-mean(x,na.rm=TRUE); s<-sd(x,na.rm=TRUE)
    if(is.na(s)||s==0) rep(0,length(x)) else (x-m)/s }
  snap[, `:=`(z_len=zc(c_len), z_rough=zc(c_rough), z_prox=zc(c_prox), z_score=zc(c_score),
              z_dist=zc(driving_dist_24), z_wild=zc(wildness_cost_24), z_app=zc(sg_app_24),
              z_ceil=zc(ceiling_rate_24))]
  snap[, course_fit := z_len*z_dist - z_rough*z_wild + z_prox*z_app + (-z_score)*z_ceil]
  snap[!is.finite(course_fit), course_fit := 0]
  snap[, c("z_len","z_rough","z_prox","z_score","z_dist","z_wild","z_app","z_ceil") := NULL]

  # field context
  snap[, field_avg_sg := mean(sg_last_24, na.rm=TRUE), by=.(event_id,year)]
  snap[, rank_pct := frank(-sg_last_24, ties.method="min", na.last="keep")/.N, by=.(event_id,year)]

  msg("rich snapshots: ", nrow(snap), " player-events | ", uniqueN(paste(snap$event_id,snap$year)), " events")
  list(snap=snap, feats=setdiff(c(PF[PF!="course_num"], "weekend_delta", "congestion_35",
       "c_score","c_len","c_prox","c_rough","c_gir","player_course_sg","player_course_n",
       "course_fit","field_avg_sg","rank_pct"), "course_num"))
}

RICH_FEATS <- NULL
if (Sys.getenv("RICH_SOURCE_ONLY") == "") {
  r <- build_rich()
  saveRDS(list(snap=r$snap, feats=r$feats), file.path(OUT, "rich_snapshots.rds"))
  msg("saved rich_snapshots.rds | ", length(r$feats), " features")
} else {
  # expose builder for sourcing
  RICH_FEATS <- NULL
}
