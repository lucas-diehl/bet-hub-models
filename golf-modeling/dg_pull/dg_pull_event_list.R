library(data.table)

dg_event_list <- function(tour = "pga", file_format = "json") {
  x <- dg_get("historical-raw-data/event-list", list(tour = tour, file_format = file_format))
  
  # Some responses come back already as data.frame
  if (is.data.frame(x)) return(as.data.table(x))
  
  # Or nested under $events / $event_list depending on feed version
  if (!is.null(x$events) && is.data.frame(x$events)) return(as.data.table(x$events))
  if (!is.null(x$event_list) && is.data.frame(x$event_list)) return(as.data.table(x$event_list))
  
  # last resort
  as.data.table(x)
}