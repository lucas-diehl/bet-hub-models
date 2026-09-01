library(data.table)

dg_rounds_event <- function(tour = "pga", event_id, year = NULL, sleep_sec = 0.5) {
  
  # ---- HARD GUARDS ----
  if (missing(event_id) || is.null(event_id) || length(event_id) == 0) {
    stop("dg_rounds_event(): event_id is missing/NULL/empty.")
  }
  event_id <- as.character(event_id)[1]
  if (is.na(event_id) || trimws(event_id) == "") {
    stop("dg_rounds_event(): event_id is NA/blank after coercion.")
  }
  
  if (is.null(year) || length(year) == 0 || is.na(year)) {
    stop("dg_rounds_event(): year is required for this endpoint. event_id=", event_id)
  }
  year <- as.integer(year)[1]
  
  # Small polite delay...
  if (!is.null(sleep_sec) && sleep_sec > 0) Sys.sleep(sleep_sec)
  
  params <- list(
    tour = tour,
    event_id = event_id,
    year = year,
    file_format = "csv"
  )
  # Some endpoints accept year; safe to include if provided
  if (!is.null(year)) params$year <- year
  
  f <- dg_get("historical-raw-data/rounds", params, timeout_sec = 180, to_file = TRUE)
  dt <- data.table::fread(f, showProgress = FALSE)
  
  # normalize types
  if ("event_id" %in% names(dt)) dt[, event_id := as.character(event_id)]
  if ("dg_id" %in% names(dt)) dt[, dg_id := as.integer(dg_id)]
  
  dt[]
}