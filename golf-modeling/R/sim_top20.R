# R/sim_top20.R
library(data.table)
# Convert factor/character/integer to integer 0/1 safely
y01 <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  as.integer(x)
}
select_bets_top20 <- function(dk_join_dt,
                              ev_col = "ev_blend_per_1",
                              ev_min = 0.00,
                              max_bets_per_event = 3,
                              odds_min = -500,
                              odds_max = 1000,
                              require_positive_ev = TRUE) {
  
  dt <- as.data.table(dk_join_dt)
  
  need_cols <- c("event_id", "player_id", "open_odds", ev_col)
  if (nrow(dt) == 0 || !all(need_cols %in% names(dt))) return(dt[0])
  
  dt[, open_odds := suppressWarnings(as.numeric(open_odds))]
  dt[, ev_used   := suppressWarnings(as.numeric(get(ev_col)))]
  
  dt <- dt[
    !is.na(event_id) & !is.na(player_id) &
      is.finite(ev_used) &
      !is.na(open_odds) &
      open_odds >= odds_min & open_odds <= odds_max
  ]
  
  if (require_positive_ev) dt <- dt[ev_used > 0]
  
  setorder(dt, event_id, -ev_used)
  dt <- dt[, head(.SD, max_bets_per_event), by = event_id]
  dt <- dt[ev_used >= ev_min]
  
  dt[]
}

grade_bets_flat <- function(bets_dt, outcomes_dt) {
  bets <- as.data.table(bets_dt)
  outc <- as.data.table(outcomes_dt)
  
  # normalize labels
  outc[, top20 := y01(top20)]
  
  # standardize join keys
  bets[, event_id  := trimws(as.character(event_id))]
  bets[, player_id := trimws(as.character(player_id))]
  
  outc[, event_id  := trimws(as.character(event_id))]
  outc[, player_id := trimws(as.character(player_id))]
  
  setkey(bets, event_id, player_id)
  setkey(outc, event_id, player_id)
  
  # IMPORTANT: grade ONLY bets (inner join)
  g <- outc[bets, nomatch = 0]
  # carry helpful columns through if they exist on bets
  if ("start_date" %in% names(bets) && !("start_date" %in% names(g))) {
    g[, start_date := as.Date(i.start_date)]
  }
  if ("p_blend" %in% names(bets) && !("p_blend" %in% names(g))) {
    g[, p_blend := as.numeric(i.p_blend)]
  }
  # profit per $1 stake using american odds
  g[, profit_per_1 := ifelse(open_odds > 0, open_odds / 100, 100 / abs(open_odds))]
  g[, stake := 1.0]
  g[, pnl := ifelse(top20 == 1, profit_per_1 * stake, -stake)]
  
  g[]
}

summarize_bets <- function(graded_dt) {
  g <- as.data.table(graded_dt)
  if (nrow(g) == 0) {
    return(data.table(
      n_bets = 0, hit_rate = NA_real_, avg_open_odds = NA_real_,
      avg_p = NA_real_, avg_edge = NA_real_, avg_ev = NA_real_,
      roi = NA_real_, avg_clv_model_minus_close = NA_real_,
      avg_clv_mkt_move = NA_real_
    ))
  }
  
  g[, .(
    n_bets = .N,
    hit_rate = mean(top20),
    avg_open_odds = mean(open_odds, na.rm = TRUE),
    avg_p = mean(p_top20, na.rm = TRUE),
    avg_edge = mean(edge_open, na.rm = TRUE),
    avg_ev = mean(ev_used %||% ev_per_1, na.rm = TRUE),
    roi = sum(pnl) / sum(stake),
    avg_clv_model_minus_close = mean(clv_model_minus_close, na.rm = TRUE),
    avg_clv_mkt_move = mean(clv_mkt_move, na.rm = TRUE)
  )]
}