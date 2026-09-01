library(data.table)
library(jsonlite)

library(data.table)

dg_rounds_year <- function(tour = "pga", year = 2025, event_id = "all") {
  
  params <- list(
    tour = tour,
    event_id = event_id,
    year = year,
    file_format = "csv"   # <-- key change
  )
  
  # Always download to file for speed + avoids huge JSON parse
  f <- dg_get("historical-raw-data/rounds", params, timeout_sec = 180, to_file = TRUE)
  
  # fread handles big CSVs efficiently
  dt <- data.table::fread(f, showProgress = FALSE)
  
  # Ensure event_id type consistent
  if ("event_id" %in% names(dt)) dt[, event_id := as.character(event_id)]
  if ("dg_id" %in% names(dt)) dt[, dg_id := as.integer(dg_id)]
  
  dt[]
}
dg_schedule <- function(tour = c("pga", "alt"), season = 2025) {
  tour <- match.arg(tour)
  
  x <- dg_get(
    "get-schedule",
    list(tour = tour, season = season, file_format = "json")
  )
  
  as.data.table(x$schedule %||% x)
}