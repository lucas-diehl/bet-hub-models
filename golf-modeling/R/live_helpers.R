library(data.table)

# Pick the first existing column from a set of candidates
pick_col <- function(dt, cands) {
  c <- intersect(cands, names(dt))[1]
  if (is.na(c) || is.null(c)) return(NULL)
  c
}

# Robustly derive an event_id for live betting-tools tables.
# If no event id exists, we return a synthetic stable id.
live_event_id_from_odds <- function(odds_dt, tour_tag = "PGA") {
  o <- as.data.table(odds_dt)
  
  # common candidates seen across feeds
  cand_event_id <- c("event_id", "eventId", "event", "event_key", "dg_event_id", "tournament_id")
  col_id <- pick_col(o, cand_event_id)
  
  if (!is.null(col_id)) {
    eid <- as.character(o[[col_id]][!is.na(o[[col_id]])][1])
    if (!is.na(eid) && trimws(eid) != "") return(trimws(eid))
  }
  
  # fallback: try an event name column and hash-ish it
  cand_name <- c("event_name", "tournament", "tournament_name", "name", "event")
  col_nm <- pick_col(o, cand_name)
  if (!is.null(col_nm)) {
    nm <- as.character(o[[col_nm]][!is.na(o[[col_nm]])][1])
    nm <- trimws(nm)
    if (!is.na(nm) && nm != "") {
      # stable synthetic key from name
      nm_key <- gsub("[^A-Za-z0-9]+", "_", toupper(nm))
      return(paste0("LIVE_", tour_tag, "_", nm_key))
    }
  }
  
  # final fallback: generic
  paste0("LIVE_", tour_tag)
}

# Ensure an odds table has an event_id column with the chosen id
stamp_live_event_id <- function(odds_dt, event_id) {
  o <- as.data.table(odds_dt)
  o[, event_id := as.character(event_id)]
  o[]
}