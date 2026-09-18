#!/usr/bin/env Rscript
# ==============================================================================
# FETCH RICH ROUNDS — pulls historical-raw-data/rounds with the FULL column set
# (prox_fw, prox_rgh, scrambling, great_shots, poor_shots, sg_t2g, scoring shape,
# teetime/start_hole) into one leakage-ready long table for new feature work.
# Resumable: per-event cache in golf_picks/rich_rounds_cache/.
#
# Moved here (from claude/, which is gitignored wholesale because ~18 OTHER
# scripts there hardcode the DataGolf key) so the daily train.yml retrain can
# actually run it -- this script never hardcoded a key (reads DATAGOLF_API_KEY
# from the environment, same as every other tracked engine/ script), so it was
# safe to move. Without this step, engine/data.R + engine/elo.R were rebuilding
# v2_master/v2_elo from a rich_rounds_long.rds frozen at its one-time 2026-09-01
# commit every single day -- deterministic re-derivation of unchanged input, so
# the daily "success" never actually reflected new results.
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table); library(curl); library(jsonlite) }))
if (dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S]"), ..., "\n", sep="")
KEY <- Sys.getenv("DATAGOLF_API_KEY"); stopifnot(nchar(KEY)>0)
OUT <- "golf_picks"; CDIR <- file.path(OUT,"rich_rounds_cache"); dir.create(CDIR, showWarnings=FALSE)

dg <- function(ep, params=list(), retries=4) {
  params$key<-KEY; params$file_format<-"json"
  qs<-paste(vapply(names(params),function(k)paste0(k,"=",URLencode(as.character(params[[k]]),reserved=TRUE)),character(1)),collapse="&")
  for (a in seq_len(retries)) {
    r<-tryCatch(curl_fetch_memory(paste0("https://feeds.datagolf.com/",ep,"?",qs)),error=function(e)NULL)
    if(is.null(r)){Sys.sleep(2*a);next}
    if(r$status_code==200){t<-rawToChar(r$content);Encoding(t)<-"UTF-8";return(tryCatch(fromJSON(t),error=function(e)NULL))}
    if(r$status_code==429){Sys.sleep(8*a);next}
    return(NULL)
  }; NULL
}

RC <- c("sg_total","sg_putt","sg_app","sg_ott","sg_arg","sg_t2g","score",
        "birdies","bogies","pars","eagles_or_better","doubles_or_worse",
        "driving_acc","driving_dist","gir","prox_fw","prox_rgh","scrambling",
        "great_shots","poor_shots","course_num","course_par","teetime","start_hole")

reshape_event <- function(raw, eid, yr) {
  dt <- as.data.table(raw)
  if (!"scores.dg_id" %in% names(dt)) return(NULL)
  base <- data.table(event_id=as.character(eid), year=as.integer(yr),
                     event_date=as.Date(dt$event_completed),
                     player_id=as.integer(dt$scores.dg_id),
                     fin_text=as.character(dt$scores.fin_text))
  parts <- lapply(1:4, function(r) {
    pre <- paste0("scores.round_", r, "."); d <- copy(base); d[, round_num := r]
    for (c in RC) { col<-paste0(pre,c)
      d[, (c) := if (col %in% names(dt)) suppressWarnings(as.numeric(dt[[col]])) else NA_real_] }
    d
  })
  long <- rbindlist(parts, use.names=TRUE)
  long[!is.na(sg_total) & !is.na(player_id)]
}

main <- function() {
  msg("=== FETCH RICH ROUNDS (2021-2026) ===")
  done <- 0L; new <- 0L
  for (yr in 2021:2026) {
    ev <- as.data.table(dg("historical-raw-data/event-list", list(tour="pga", year=yr)))
    if (!"event_id" %in% names(ev) && "id" %in% names(ev)) setnames(ev,"id","event_id")
    ev[, event_id := as.character(event_id)]
    ev[, d := as.Date(date)]
    ev <- ev[!is.na(d) & d >= as.Date(paste0(yr,"-01-01")) & d <= as.Date(paste0(yr,"-12-31")) & d <= Sys.Date()]
    msg("Year ", yr, ": ", uniqueN(ev$event_id), " events")
    for (eid in unique(ev$event_id)) {
      cf <- file.path(CDIR, paste0("ev_", eid, "_", yr, ".rds"))
      edate <- ev[event_id == eid, min(d)]
      if (file.exists(cf)) {
        # An event cached as EMPTY means DataGolf had no data at fetch time -- which
        # is expected/permanent for an event that hasn't been played yet, but WRONG
        # to treat as permanent once the event has actually completed (DataGolf's
        # historical-raw-data just hadn't posted it yet). Only trust an empty verdict
        # once the event is well past its date (processing lag, not "no data ever").
        cached_empty <- tryCatch(!nrow(readRDS(cf)), error = function(e) TRUE)
        if (!cached_empty || (Sys.Date() - edate) > 10) { done<-done+1L; next }
      }
      raw <- dg("historical-raw-data/rounds", list(tour="pga", event_id=eid, year=yr))
      Sys.sleep(1.3)
      lr <- if (!is.null(raw)) tryCatch(reshape_event(raw, eid, yr), error=function(e)NULL) else NULL
      saveRDS(if (is.null(lr)) data.table() else lr, cf)   # cache empties too (subject to the retry window above)
      if (!is.null(lr) && nrow(lr)) new<-new+1L
      if ((new+done) %% 20 == 0) msg("  progress: new=", new, " skipped=", done)
    }
  }
  files <- list.files(CDIR, pattern="^ev_.*\\.rds$", full.names=TRUE)
  parts <- lapply(files, function(f){x<-tryCatch(as.data.table(readRDS(f)),error=function(e)NULL); if(!is.null(x)&&nrow(x)) x else NULL})
  long <- rbindlist(parts[!sapply(parts,is.null)], use.names=TRUE, fill=TRUE)
  setorder(long, player_id, event_date, round_num)
  saveRDS(long, file.path(OUT, "rich_rounds_long.rds"))
  msg("DONE: ", nrow(long), " player-rounds | ", uniqueN(paste(long$event_id,long$year)),
      " events | ", as.character(min(long$event_date)), "..", as.character(max(long$event_date)),
      " -> golf_picks/rich_rounds_long.rds")
}
main()
