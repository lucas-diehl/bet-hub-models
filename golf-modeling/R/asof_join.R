library(data.table)

build_event_player_table <- function(results_dt, events_dt) {
  res <- as.data.table(results_dt)
  ev  <- as.data.table(events_dt)
  
  # Ensure 1 row per event_id in events (choose earliest start_date)
  ev <- ev[, .(start_date = min(as.Date(start_date))), by = event_id]
  setkey(ev, event_id)
  
  # Unique player-event pairs from results
  ep <- unique(res[, .(event_id, player_id)])
  
  # Safe join (no cartesian possible now)
  ep <- ev[ep, on = "event_id"]
  
  # Drop anything missing dates
  ep <- ep[!is.na(start_date)]
  
  ep[]
}

library(data.table)

join_asof_features <- function(rounds_feat_long_dt, event_players_dt) {
  feat <- as.data.table(rounds_feat_long_dt)
  ep   <- as.data.table(event_players_dt)
  
  stopifnot(all(c("player_id", "round_date") %in% names(feat)))
  stopifnot(all(c("event_id", "player_id", "start_date") %in% names(ep)))
  
  # Standardize types
  feat[, player_id := as.integer(player_id)]
  ep[,   player_id := as.integer(player_id)]
  
  feat[, asof_date := as.Date(round_date)]
  ep[,   asof_date := as.Date(start_date)]
  
  # Keep only what we need from feat (avoid event_id collisions)
  keep_feat <- setdiff(names(feat), c("event_id", "round_date"))
  feat2 <- feat[, ..keep_feat]
  # feat2 includes player_id + asof_date + feature columns
  
  # Rolling join: last known features for that player on/before event start
  setkey(feat2, player_id, asof_date)
  setkey(ep,    player_id, asof_date)
  
  out <- feat2[ep, roll = TRUE]
  
  # bring back event_id explicitly from ep (comes as i.event_id)
  if ("i.event_id" %in% names(out)) out[, event_id := i.event_id]
  if ("i.start_date" %in% names(out)) out[, start_date := as.Date(i.start_date)]
  
  # drop helper i.* columns except event_id/start_date we just copied
  drop_i <- grep("^i\\.", names(out), value = TRUE)
  drop_i <- setdiff(drop_i, c("i.event_id", "i.start_date"))
  if (length(drop_i) > 0) out[, (drop_i) := NULL]
  
  # Ensure one row per event_id-player_id
  out <- unique(out, by = c("event_id", "player_id"))
  
  out[]
}