#!/usr/bin/env Rscript
# ==============================================================================
# golf-modeling — emit_elo.R
# Snapshot the current personalized golf Elo ratings to the Bet Hub feed:
#   <FEED_DIR>/golf-modeling/pga/elo_<date>.json
# Contract: {contract_version, source:"golf-modeling", sport:"pga", generated_at,
#            ratings:[{rank, name, elo, change}]}  (name+elo required; rank/change opt).
# Each new file replaces the prior snapshot (ingest keys on the ratings array).
# Run daily (wired into DFS ENGINE run_all.R alongside the DG-ownership log).
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table); library(jsonlite) }))
local({
  win <- "c:/Users/ljdie/OneDrive/Documents/golf-modeling"
  if (dir.exists(win)) setwd(win) else {  # CI/Linux: find the golf root from --file
    m <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(m)) { d <- dirname(normalizePath(sub("^--file=", "", m[1])))
      while (!dir.exists(file.path(d, "golf_picks")) && dirname(d) != d) d <- dirname(d)
      if (dir.exists(file.path(d, "golf_picks"))) setwd(d) } }
})
FEED <- Sys.getenv("FEED_DIR", "C:/Users/ljdie/OneDrive/Documents/dashboard_feed")

elo <- as.data.table(readRDS("golf_picks/v2_elo.rds"))            # player_id, event_id, year, elo_pre
M   <- as.data.table(readRDS("golf_picks/v2_master.rds")$master)
ed  <- unique(M[, .(event_id, year, event_date)])
nm  <- unique(M[!is.na(player_name), .(player_id, player_name)])

elo <- merge(elo, ed, by = c("event_id", "year"), all.x = TRUE)
elo <- elo[is.finite(elo_pre) & !is.na(event_date)][order(player_id, event_date)]
# current rating = latest pre-event Elo per player; change = movement over the last event
cur <- elo[, .(elo = elo_pre[.N],
               change = if (.N >= 2) elo_pre[.N] - elo_pre[.N - 1] else NA_real_,
               last_date = max(event_date)), by = player_id]
cur <- merge(cur, nm, by = "player_id")
cur <- cur[last_date >= max(last_date, na.rm = TRUE) - 400]        # active players only (~last year)
cur <- cur[order(-elo)][, rank := .I]

disp <- function(x) { x <- as.character(x); ifelse(grepl(",", x), sub("^\\s*([^,]+),\\s*(.*)$", "\\2 \\1", trimws(x)), x) }
ratings <- lapply(seq_len(nrow(cur)), function(i) { r <- cur[i]
  item <- list(rank = r$rank, name = disp(r$player_name), elo = round(as.numeric(r$elo)))
  if (is.finite(r$change)) item$change <- round(as.numeric(r$change))
  item })
out <- list(contract_version = "1.0", source = "golf-modeling", sport = "pga",
            generated_at = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
            ratings = ratings)
dir <- file.path(FEED, "golf-modeling", "pga"); dir.create(dir, recursive = TRUE, showWarnings = FALSE)
f <- file.path(dir, sprintf("elo_%s.json", Sys.Date()))
write_json(out, f, auto_unbox = TRUE, pretty = TRUE, null = "null")
cat("wrote", f, "(", length(ratings), "players )\n")
