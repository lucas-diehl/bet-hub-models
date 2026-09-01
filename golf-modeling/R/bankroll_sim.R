# R/bankroll_sim.R
library(data.table)

american_to_decimal <- function(odds) {
  odds <- as.numeric(odds)
  ifelse(odds > 0, 1 + odds/100, 1 + 100/abs(odds))
}

kelly_fraction <- function(p, odds_american) {
  # Kelly for binary bet with decimal odds d:
  # b = d - 1, q = 1 - p, f* = (bp - q)/b
  d <- american_to_decimal(odds_american)
  b <- d - 1
  q <- 1 - p
  f <- (b * p - q) / b
  pmax(0, f) # no negative Kelly (skip)
}

compute_drawdown <- function(equity) {
  peak <- cummax(equity)
  dd <- (equity / peak) - 1
  dd
}

simulate_bankroll <- function(graded_bets_dt,
                              stake_mode = c("flat", "kelly"),
                              flat_stake = 1.0,
                              kelly_mult = 0.25,
                              p_col = "p_blend",
                              date_col = "start_date",
                              bankroll0 = 100.0) {
  stake_mode <- match.arg(stake_mode)
  dt <- as.data.table(graded_bets_dt)
  if (nrow(dt) == 0) {
    return(list(
      curve = data.table(),
      summary = data.table(
        stake_mode = stake_mode,
        n_bets = 0, bankroll0 = bankroll0, bankroll_final = bankroll0,
        roi = NA_real_, max_drawdown = NA_real_
      )
    ))
  }
  
  # basic requirements
  need <- c("event_id", "open_odds", "pnl", "stake", "top20")
  miss <- setdiff(need, names(dt))
  if (length(miss) > 0) stop("simulate_bankroll(): missing cols: ", paste(miss, collapse = ", "))
  
  # ensure date
  if (!(date_col %in% names(dt))) {
    dt[, (date_col) := as.Date(NA)]
  }
  dt[, (date_col) := as.Date(get(date_col))]
  
  # choose p for kelly sizing
  if (!(p_col %in% names(dt))) dt[, (p_col) := NA_real_]
  dt[, p_used := as.numeric(get(p_col))]
  
  # size stakes
  if (stake_mode == "flat") {
    dt[, stake_sim := flat_stake]
  } else {
    dt[, kf := kelly_fraction(p_used, open_odds)]
    dt[, stake_sim := flat_stake * kelly_mult * kf]  # flat_stake acts as bankroll unit scaler if you want
    dt[!is.finite(stake_sim) | is.na(stake_sim), stake_sim := 0]
  }
  
  # convert pnl to match stake_sim
  # pnl in your grader is per $1 stake. So scale.
  dt[, pnl_sim := pnl * stake_sim]
  
  # aggregate by event date (or event_id fallback)
  if (all(is.na(dt[[date_col]]))) {
    by_key <- "event_id"
  } else {
    by_key <- date_col
  }
  
  agg <- dt[, .(
    n_bets = .N,
    stake = sum(stake_sim, na.rm = TRUE),
    pnl = sum(pnl_sim, na.rm = TRUE),
    hit_rate = mean(top20, na.rm = TRUE)
  ), by = by_key]
  
  data.table::setorderv(agg, cols = by_key)
  
  agg[, bankroll := bankroll0 + cumsum(pnl)]
  agg[, drawdown := compute_drawdown(bankroll)]
  
  summ <- data.table(
    stake_mode = stake_mode,
    n_bets = nrow(dt),
    bankroll0 = bankroll0,
    bankroll_final = agg$bankroll[.N],
    roi = sum(agg$pnl) / sum(pmax(agg$stake, 1e-9)),
    max_drawdown = min(agg$drawdown, na.rm = TRUE),
    avg_stake = mean(agg$stake, na.rm = TRUE)
  )
  
  list(curve = agg, summary = summ)
}