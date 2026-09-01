# R/dg_safe.R
library(data.table)

dg_rounds_event_safe <- function(tour = "pga", event_id, year, sleep_sec = 0.5) {
  out <- tryCatch(
    dg_rounds_event(tour = tour, event_id = event_id, year = year, sleep_sec = sleep_sec),
    error = function(e) NULL
  )
  out
}
# R/dg_rounds_event_safe.R
dg_rounds_event_safe_hist <- function(tour = "pga", event_id, year, sleep_sec = 0.8) {
  tryCatch(
    dg_rounds_event(tour = tour, event_id = event_id, year = year, sleep_sec = sleep_sec),
    error = function(e) {
      message("dg_rounds_event_safe_hist(): failed event_id=", event_id, " year=", year, " :: ", conditionMessage(e))
      NULL
    }
  )
}