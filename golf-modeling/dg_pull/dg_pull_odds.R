library(data.table)

dg_odds_outrights <- function(year = 2025, market = "top_20",
                              tour = "pga", book = "draftkings",
                              event_id = "all", odds_format = "american") {
  
  x <- dg_get(
    "historical-odds/outrights",
    list(tour = tour, event_id = event_id, year = year,
         market = market, book = book,
         odds_format = odds_format, file_format = "json")
  )
  
  if (is.data.frame(x)) return(as.data.table(x))
  if (!is.null(x$odds) && is.data.frame(x$odds)) return(as.data.table(x$odds))
  if (!is.null(x$data) && is.data.frame(x$data)) return(as.data.table(x$data))
  
  if (is.list(x) && length(x) > 0 && !is.null(names(x))) {
    dt_list <- lapply(names(x), function(eid) {
      obj <- x[[eid]]
      if (is.data.frame(obj)) dt <- as.data.table(obj)
      else if (!is.null(obj$odds) && is.data.frame(obj$odds)) dt <- as.data.table(obj$odds)
      else if (!is.null(obj$data) && is.data.frame(obj$data)) dt <- as.data.table(obj$data)
      else dt <- as.data.table(obj)
      
      dt[, event_id := as.integer(eid)]
      dt
    })
    return(rbindlist(dt_list, fill = TRUE))
  }
  
  stop("dg_odds_outrights(): Unrecognized response shape. Inspect with str().")
}