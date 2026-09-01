library(data.table)

build_events_2025 <- function(schedule_pga, schedule_alt) {
  pga <- as.data.table(schedule_pga)
  alt <- as.data.table(schedule_alt)
  
  pga[, sched_tour := "pga"]
  alt[, sched_tour := "alt"]
  
  ev <- rbindlist(list(pga, alt), fill = TRUE)
  
  # ---- event_id ----
  if (!("event_id" %in% names(ev))) {
    id_col <- intersect(names(ev), c("id", "eventId", "event_key", "dg_event_id"))[1]
    if (is.na(id_col) || is.null(id_col)) stop("Could not find event id column in schedule feed.")
    setnames(ev, id_col, "event_id")
  }
  
  # ---- event_name ----
  if (!("event_name" %in% names(ev))) {
    nm_col <- intersect(names(ev), c("event", "name", "tournament_name", "eventName"))[1]
    if (!is.na(nm_col) && !is.null(nm_col)) setnames(ev, nm_col, "event_name")
  }
  if (!("event_name" %in% names(ev))) ev[, event_name := NA_character_]
  
  # ---- start_date ----
  if (!("start_date" %in% names(ev))) {
    dt_col <- intersect(names(ev), c("start_date", "date", "event_start_date", "start"))[1]
    if (is.na(dt_col) || is.null(dt_col)) stop("Could not find start date column in schedule feed.")
    setnames(ev, dt_col, "start_date")
  }
  ev[, start_date := as.Date(start_date)]
  
  # ---- flags / context ----
  ev[, is_alt := as.integer(sched_tour == "alt")]
  
  # Major flag is optional; uses event_name heuristic
  majors_pat <- "Masters|U\\.S\\. Open|The Open|Open Championship|PGA Championship"
  ev[, major_flag := as.integer(grepl(majors_pat, event_name, ignore.case = TRUE))]
  
  # DO NOT assume field_size exists here; we’ll compute from results later
  ev[, field_size := NA_integer_]
  
  unique(ev[, .(event_id, event_name, start_date, sched_tour, is_alt, major_flag, field_size)])
}