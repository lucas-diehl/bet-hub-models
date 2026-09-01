# R/splits.R
library(data.table)

# Create chronological time split of events (train/valid/test)
create_time_split_events <- function(events_dt, train_frac = 0.60, valid_frac = 0.20) {
  ev <- unique(as.data.table(events_dt)[, .(event_id, start_date)])
  ev[, start_date := as.Date(start_date)]
  setorder(ev, start_date)
  
  n <- nrow(ev)
  if (n == 0) stop("events_dt appears empty in create_time_split_events()")
  
  train_cut <- floor(train_frac * n)
  valid_cut <- floor((train_frac + valid_frac) * n)
  
  ev[, idx := .I]
  ev[, split := fifelse(idx <= train_cut, "train",
                        fifelse(idx <= valid_cut, "valid", "test"))]
  ev[, idx := NULL]
  ev[]
}