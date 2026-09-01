library(data.table)

american_to_prob <- function(odds) {
  odds <- as.numeric(odds)
  ifelse(odds > 0, 100 / (odds + 100), (-odds) / ((-odds) + 100))
}

american_profit_per_1 <- function(odds) {
  odds <- as.numeric(odds)
  ifelse(odds > 0, odds / 100, 100 / abs(odds))
}

# Helper: try to detect DK odds column patterns in live tables
extract_dk_odds <- function(dt) {
  d <- copy(as.data.table(dt))
  
  # Common shapes:
  # A) historical: open_odds/close_odds exist already
  if ("open_odds" %in% names(d)) return(d)
  
  # B) long format with book + odds
  if (all(c("book", "odds") %in% names(d))) {
    d <- d[tolower(book) %in% c("draftkings", "dk")]
    if (nrow(d) == 0) return(d)
    setnames(d, "odds", "open_odds")
    if (!("close_odds" %in% names(d))) d[, close_odds := as.numeric(NA)]
    return(d)
  }
  
  # C) wide format with a DK column
  cand <- c("draftkings", "draftkings_odds", "dk", "dk_odds")
  dk_col <- intersect(cand, names(d))[1]
  if (!is.na(dk_col) && !is.null(dk_col)) {
    setnames(d, dk_col, "open_odds")
    if (!("close_odds" %in% names(d))) d[, close_odds := as.numeric(NA)]
    return(d)
  }
  
  # If we get here, we couldn't find odds
  d[0]
}

join_dk_open_close <- function(pred_dt, dk_odds_dt, alpha = 0.70,
                               odds_min = -500, odds_max = 1000) {
  pred <- as.data.table(pred_dt)
  dk0  <- as.data.table(dk_odds_dt)
  
  # standardize keys in pred
  pred[, event_id  := trimws(as.character(event_id))]
  pred[, player_id := trimws(as.character(player_id))]
  
  # normalize DK odds table to have open_odds/close_odds + dg_id -> player_id
  dk <- extract_dk_odds(dk0)
  
  # map ids
  if ("dg_id" %in% names(dk) && !("player_id" %in% names(dk))) {
    dk[, player_id := trimws(as.character(dg_id))]
  } else if ("player_id" %in% names(dk)) {
    dk[, player_id := trimws(as.character(player_id))]
  }
  
  if ("event_id" %in% names(dk)) {
    dk[, event_id := trimws(as.character(event_id))]
  } else {
    # live feed is usually for a single event; if missing, set NA and let join fail gracefully
    dk[, event_id := NA_character_]
  }
  
  # coerce odds
  dk[, open_odds  := suppressWarnings(as.numeric(open_odds))]
  dk[, close_odds := suppressWarnings(as.numeric(close_odds))]
  
  # bounds + required
  dk <- dk[!is.na(player_id) & !is.na(open_odds)]
  dk <- dk[open_odds >= odds_min & open_odds <= odds_max]
  
  # implied probs
  dk[, p_open_imp  := american_to_prob(open_odds)]
  dk[, p_close_imp := fifelse(is.na(close_odds), NA_real_, american_to_prob(close_odds))]
  
  # join
  setkey(dk, event_id, player_id)
  setkey(pred, event_id, player_id)
  out <- dk[pred, on = .(event_id, player_id), nomatch = 0]
  
  if (nrow(out) == 0) {
    message("DK join produced 0 rows. Diagnostics:")
    message("  pred events: ", length(unique(pred$event_id)), " | dk events: ", length(unique(dk$event_id)))
    message("  pred players: ", length(unique(pred$player_id)), " | dk players: ", length(unique(dk$player_id)))
    return(out[])
  }
  
  # edges (raw implied)
  out[, edge_open_raw := p_top20 - p_open_imp]
  out[, edge_open := edge_open_raw]
  
  # EV at open (per $1)
  out[, profit_per_1 := american_profit_per_1(open_odds)]
  out[, ev_per_1 := p_top20 * profit_per_1 - (1 - p_top20)]
  
  # -------------------------
  # Devig within event
  # NOTE: for Top20, sum(raw implied) is not 1. We devig to sum to 20
  # (your check shows devig sums to 20 for most events).
  # -------------------------
  out[, p_open_devig := p_open_imp * (20 / sum(p_open_imp, na.rm = TRUE)), by = event_id]
  
  # Blend model toward devig market
  out[, p_blend := alpha * p_top20 + (1 - alpha) * p_open_devig]
  out[, ev_blend_per_1 := p_blend * profit_per_1 - (1 - p_blend)]
  
  out[]
}