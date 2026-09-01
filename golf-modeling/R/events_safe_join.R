# R/events_safe_join.R
library(data.table)

# Helper: return NA of a given class
na_of_class <- function(x) {
  cls <- class(x)[1]
  switch(cls,
         integer = NA_integer_,
         numeric = NA_real_,
         double = NA_real_,
         numeric = NA_real_,
         Date = as.Date(NA),
         POSIXct = as.POSIXct(NA),
         logical = NA, 
         character = NA_character_,
         NA_character_)
}

# Robust events_one_per_id: prefer pga row when present; otherwise earliest start_date.
# Guarantees consistent column types in returned table.
events_one_per_id <- function(events_dt) {
  ev <- as.data.table(events_dt)
  if ("start_date" %in% names(ev)) ev[, start_date := as.IDate(start_date)]  # fast Date-like
  
  if (!("event_id" %in% names(ev))) stop("events_dt missing event_id")
  
  # ensure sched_tour exists (if not, create NA_character_)
  if (!("sched_tour" %in% names(ev))) ev[, sched_tour := NA_character_]
  
  # We'll build a template of column classes from the full table (first non-NA sample)
  template <- lapply(ev, function(col) {
    # pick first non-NA value to detect class; fallback to NA of that type
    v <- col[!is.na(col)][1]
    if (is.null(v)) return(na_of_class(col))
    # preserve Date class as IDate/Date
    if (inherits(v, "Date") || inherits(v, "IDate")) return(as.IDate(v))
    # coerce numeric/integer/character/logical as appropriate
    if (is.integer(v)) return(as.integer(v))
    if (is.numeric(v)) return(as.numeric(v))
    if (is.logical(v)) return(as.logical(v))
    as.character(v)
  })
  
  # Ensure start_date in template is Date-like
  if ("start_date" %in% names(template)) template$start_date <- as.IDate(template$start_date)
  
  cols_all <- names(ev)
  
  # group by event_id and produce one-row-per-group with consistent types
  out <- ev[, {
    # prefer first pga row if exists
    rows_pga <- which(sched_tour == "pga")
    if (length(rows_pga) >= 1) {
      chosen <- .SD[rows_pga[1]]
      # ensure chosen has same columns
      res <- vector("list", length = length(cols_all))
      names(res) <- cols_all
      for (nm in cols_all) {
        val <- chosen[[nm]]
        # coerce to template type
        templ <- template[[nm]]
        if (inherits(templ, "IDate")) {
          res[[nm]] <- if (!is.na(val)) as.IDate(val) else NA
        } else if (is.integer(templ)) {
          res[[nm]] <- suppressWarnings(as.integer(val))
        } else if (is.numeric(templ)) {
          res[[nm]] <- suppressWarnings(as.numeric(val))
        } else if (is.logical(templ)) {
          res[[nm]] <- as.logical(val)
        } else {
          # character fallback
          res[[nm]] <- if (is.null(val) || length(val) == 0) NA_character_ else as.character(val)
        }
      }
      as.list(res)
    } else {
      # fallback: earliest start_date + first non-NA for other cols
      res <- vector("list", length = length(cols_all))
      names(res) <- cols_all
      # start_date
      if ("start_date" %in% cols_all) {
        sd <- min(as.IDate(start_date), na.rm = TRUE)
        if (is.infinite(sd)) sd <- NA
        res[["start_date"]] <- sd
      }
      for (nm in setdiff(cols_all, "start_date")) {
        vals <- .SD[[nm]]
        nn <- vals[!is.na(vals)]
        val <- if (length(nn) > 0) nn[1] else NA
        templ <- template[[nm]]
        if (inherits(templ, "IDate")) {
          res[[nm]] <- if (!is.na(val)) as.IDate(val) else NA
        } else if (is.integer(templ)) {
          res[[nm]] <- suppressWarnings(as.integer(val))
        } else if (is.numeric(templ)) {
          res[[nm]] <- suppressWarnings(as.numeric(val))
        } else if (is.logical(templ)) {
          res[[nm]] <- as.logical(val)
        } else {
          res[[nm]] <- if (is.null(val) || length(val) == 0) NA_character_ else as.character(val)
        }
      }
      as.list(res)
    }
  }, by = event_id]
  
  # coerce start_date back to Date (not POSIX)
  if ("start_date" %in% names(out)) out[, start_date := as.Date(start_date)]
  
  # ensure column order
  other_cols2 <- setdiff(names(out), c("event_id", "start_date"))
  setcolorder(out, c("event_id", "start_date", other_cols2))
  
  out[]
}