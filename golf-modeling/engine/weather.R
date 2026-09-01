#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS v2 — engine/weather.R   (P0b: live weather -> AM/PM wave conditions)
#
# Turns the tee-time wave flag into a REAL per-wave scoring adjustment using a free,
# no-key weather forecast (Open-Meteo) at the course's lat/long (from DataGolf's
# schedule). Windier wave => harder scoring => negative SG shift + higher variance.
# Feeds engine/coursefit.R::apply_conditions() (and the single-round R2 sim).
#
#   wave_conditions(tour, round, date) -> data.table(player_id, wave, wind, wave_shift, vol_mult)
# No-ops gracefully (neutral waves) if any feed is unreachable.
#
# Source: Sys.setenv(ENGINE_SOURCE_ONLY="1"); source("engine/weather.R")
# Run:    Rscript engine/weather.R [--round 2]   (prints tomorrow's wave conditions)
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table); library(httr2) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
if (!exists(".EKEY")) source("engine/enrich.R")   # reuses .EKEY(), .dg()
emsg <- get0("emsg", ifnotfound=function(...) cat(..., "\n"))

WIND_K   <- 0.06   # base fallback SG strokes per mph of wave wind vs the day's mean
# ROUND-SPECIFIC, calibrated on 67,358 realised player-rounds (wave_weather.csv AM/PM
# wind vs field-relative round score): wind hurts scoring FAR more in Round 1 (full
# field + clean AM/PM wave split) and decays to noise by the weekend (post-cut re-pairing
# by score washes out the wave). strokes/mph above the day's mean:
#   R1 +0.165 (p 1e-23) | R2 +0.065 | R3 +0.035 (n.s.) | R4 +0.027 (n.s.)
WIND_K_ROUND <- c("1" = 0.16, "2" = 0.07, "3" = 0.03, "4" = 0.03)
.wind_k <- function(round) { k <- WIND_K_ROUND[as.character(round)]; if (is.na(k)) WIND_K else as.numeric(k) }
WIND_CAP <- 1.2
VOL_M    <- 0.020  # variance bump per mph vs mean

.gj <- function(u) tryCatch(httr2::request(u)|>httr2::req_timeout(30)|>
  httr2::req_perform()|>httr2::resp_body_json(simplifyVector=TRUE), error=function(e) NULL)

# course lat/long for the live event (match schedule by event name)
course_latlon <- function(tour = "pga") {
  fu <- .dg("field-updates", list(tour = tour)); if (is.null(fu)) return(NULL)
  ev_name <- fu$event_name
  sc <- .dg("get-schedule", list(tour = tour)); if (is.null(sc$schedule)) return(NULL)
  s <- as.data.table(sc$schedule)
  row <- s[event_name == ev_name]
  if (!nrow(row)) row <- s[grepl(substr(ev_name,1,10), event_name, fixed = TRUE)]
  if (!nrow(row)) return(NULL)
  list(lat = as.numeric(row$latitude[1]), lon = as.numeric(row$longitude[1]),
       event = ev_name, event_id = row$event_id[1])
}

# hourly wind (mph) for a date at lat/long
openmeteo_wind <- function(lat, lon, date) {
  u <- sprintf(paste0("https://api.open-meteo.com/v1/forecast?latitude=%.4f&longitude=%.4f",
       "&hourly=wind_speed_10m,wind_gusts_10m,precipitation&wind_speed_unit=mph",
       "&timezone=auto&start_date=%s&end_date=%s"), lat, lon, date, date)
  w <- .gj(u); if (is.null(w$hourly)) return(NULL)
  h <- as.data.table(w$hourly)
  h[, hour := as.integer(substr(time, 12, 13))]
  h[, .(hour, wind = as.numeric(wind_speed_10m),
        gust = as.numeric(wind_gusts_10m), precip = as.numeric(precipitation))]
}

# "2026-08-20 09:47" -> 9.78 (decimal tee hour)
.tee_hour <- function(dt) { s <- as.character(dt); hm <- sub("^\\S+\\s+", "", s)
  h <- suppressWarnings(as.integer(sub(":.*", "", hm))); m <- suppressWarnings(as.integer(sub(".*:", "", hm)))
  h + m / 60 }

# field-updates carries a nested `teetimes` list (per player: round_num, teetime, wave).
# Extract this round's tee hour + date + wave per player. NULL if unavailable.
.parse_teetimes <- function(field_dt, round) {
  tt <- field_dt$teetimes; if (is.null(tt)) return(NULL)
  ids <- as.integer(field_dt$dg_id)
  d <- rbindlist(lapply(seq_along(tt), function(i) {
    x <- tt[[i]]; if (is.null(x) || !NROW(x)) return(NULL); x <- as.data.table(x)
    r <- x[as.integer(round_num) == round]; if (!nrow(r)) return(NULL)
    data.table(player_id = ids[i], tee = .tee_hour(r$teetime[1]),
               tee_date = substr(as.character(r$teetime[1]), 1, 10), wave = as.character(r$wave[1]))
  }), fill = TRUE)
  if (is.null(d) || !nrow(d)) return(NULL)
  d[is.finite(tee)]
}

# per-player conditions: map each golfer's ACTUAL tee time to the wind over their ~5h
# playing window (finer than a 6h AM/PM block -> captures the real calm-vs-windy spread),
# scaled by the round-specific wind sensitivity. Falls back to AM/PM blocks (on the `am`
# flag), then to neutral, if tee times or the forecast are missing.
wave_conditions <- function(tour = "pga", round = 2L, date = NULL, verbose = TRUE) {
  neutral <- function(fu) {
    d <- if (is.null(fu) || is.null(fu$field)) data.table(player_id=integer(0)) else
      as.data.table(fu$field)[, .(player_id = as.integer(dg_id),
                                  wave = suppressWarnings(as.integer(am)))]
    if (nrow(d)) d[, `:=`(wind = NA_real_, wave_shift = 0, vol_mult = 1)]
    d
  }
  fu <- .dg("field-updates", list(tour = tour))
  if (is.null(fu) || is.null(fu$field)) { if (verbose) emsg("weather: no field feed -> neutral"); return(neutral(fu)) }
  loc <- course_latlon(tour)
  if (is.null(loc) || !is.finite(loc$lat)) { if (verbose) emsg("weather: no coords -> neutral"); return(neutral(fu)) }
  f  <- as.data.table(fu$field)
  K  <- .wind_k(round)                                         # round-specific (R1 strongest; wind only — heat/rain add nothing)
  clamp_sh <- function(wd, m) -max(min(K * (wd - m), WIND_CAP), -WIND_CAP)
  clamp_vm <- function(wd, m) max(min(1 + VOL_M * (wd - m), 1.4), 0.85)

  # PRIMARY: per-tee-time — each golfer's wind over their actual ~4.5h playing window
  tt <- .parse_teetimes(f, round)
  if (!is.null(tt) && nrow(tt) >= 10L) {
    d0 <- if (!is.null(date)) as.character(date) else names(sort(table(tt$tee_date), decreasing = TRUE))[1]
    wx <- openmeteo_wind(loc$lat, loc$lon, d0)
    if (!is.null(wx) && nrow(wx)) {
      winw <- function(tee) { hrs <- seq(floor(tee), ceiling(tee + 4.5))
        w <- wx[hour %in% hrs]$wind; if (length(w)) mean(w, na.rm = TRUE) else NA_real_ }
      tt[, wind := vapply(tee, winw, numeric(1))]
      mean_w <- mean(tt$wind, na.rm = TRUE)
      tt[, `:=`(wave_shift = vapply(wind, clamp_sh, numeric(1), m = mean_w),
                vol_mult   = vapply(wind, clamp_vm, numeric(1), m = mean_w))]
      tt[!is.finite(wave_shift), `:=`(wave_shift = 0, vol_mult = 1)]
      if (verbose) emsg(sprintf("weather @ %s R%d %s: per-tee-time wind %.1f-%.1f mph (mean %.1f, K=%.2f) -> shift %+.2f..%+.2f",
          loc$event, round, d0, min(tt$wind, na.rm = TRUE), max(tt$wind, na.rm = TRUE), mean_w, K,
          min(tt$wave_shift, na.rm = TRUE), max(tt$wave_shift, na.rm = TRUE)))
      return(tt[, .(player_id, wave, wind = round(wind, 1), wave_shift, vol_mult)])
    }
  }

  # FALLBACK: coarse AM/PM block on the `am` flag (when tee times / forecast missing)
  if (is.null(date)) date <- as.character(Sys.Date() + 1L)
  wx <- openmeteo_wind(loc$lat, loc$lon, date)
  if (is.null(wx) || !nrow(wx)) { if (verbose) emsg("weather: no forecast -> neutral"); return(neutral(fu)) }
  wind_am <- mean(wx[hour >= 7 & hour <= 12]$wind, na.rm = TRUE)
  wind_pm <- mean(wx[hour >= 12 & hour <= 18]$wind, na.rm = TRUE)
  mean_w  <- mean(c(wind_am, wind_pm), na.rm = TRUE)
  if (verbose) emsg(sprintf("weather @ %s %s (AM/PM block fallback): AM %.1f / PM %.1f mph, K=%.2f",
      loc$event, date, wind_am, wind_pm, K))
  d <- f[, .(player_id = as.integer(dg_id), wave = suppressWarnings(as.integer(am)))]
  d[, `:=`(wind = fifelse(wave == 1L, wind_am, wind_pm),
           wave_shift = fifelse(wave == 1L, clamp_sh(wind_am, mean_w), clamp_sh(wind_pm, mean_w)),
           vol_mult   = fifelse(wave == 1L, clamp_vm(wind_am, mean_w), clamp_vm(wind_pm, mean_w)))]
  d[!is.finite(wave_shift), `:=`(wave_shift = 0, vol_mult = 1)]
  d[]
}

if (identical(environment(), globalenv()) && !nzchar(Sys.getenv("WEATHER_SOURCE_ONLY"))) {
  a <- commandArgs(trailingOnly = TRUE)
  rnd <- { i<-which(a=="--round"); if(length(i)) as.integer(a[i+1]) else 2L }
  wc <- wave_conditions("pga", rnd)
  cat("\n== wave conditions (round", rnd, ") ==\n")
  if (nrow(wc)) print(wc[, .(n=.N, wind=round(mean(wind),1),
      shift=round(mean(wave_shift),2), vol=round(mean(vol_mult),2)), by=wave][order(wave)], row.names=FALSE)
}
