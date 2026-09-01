# R/splits_year.R
library(data.table)

# train_years: integer vector, e.g. 2015:2023
# test_years : integer vector, e.g. 2024:2025
split_events_by_year <- function(events_dt, train_years, test_years) {
  ev <- unique(as.data.table(events_dt)[, .(event_id, start_date)])
  ev[, start_date := as.Date(start_date)]
  ev <- ev[!is.na(event_id) & !is.na(start_date)]
  ev[, year := as.integer(format(start_date, "%Y"))]
  
  ev[, split := fifelse(year %in% train_years, "train",
                        fifelse(year %in% test_years, "test", NA_character_))]
  
  # keep only rows that belong to either split
  ev <- ev[!is.na(split)]
  
  ev[, .(event_id = as.character(event_id), split, year)]
}