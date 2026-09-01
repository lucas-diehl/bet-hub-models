# R/sim_grid.R
library(data.table)

# expects these to already exist from R/sim_top20.R:
# - select_bets_top20()
# - grade_bets_flat()
# - summarize_bets()

sweep_rules <- function(dk_join_dt, outcomes_dt,
                        ev_col = "ev_blend_per_1",
                        ev_grid = c(0.00, 0.01, 0.02, 0.03),
                        cap_grid = c(1, 2, 3),
                        odds_min = -500, odds_max = 1000) {
  res <- list()
  
  for (ev_min in ev_grid) {
    for (cap in cap_grid) {
      bets <- select_bets_top20(
        dk_join_dt,
        ev_col = ev_col,
        ev_min = ev_min,
        max_bets_per_event = cap,
        odds_min = odds_min,
        odds_max = odds_max
      )
      
      if (nrow(bets) == 0) {
        res[[length(res) + 1]] <- data.table(
          ev_min = ev_min, cap = cap,
          n_bets = 0, hit_rate = NA_real_, roi = NA_real_,
          avg_ev = NA_real_, avg_edge = NA_real_,
          avg_clv_model_minus_close = NA_real_
        )
        next
      }
      
      graded <- grade_bets_flat(bets, outcomes_dt)
      summ <- summarize_bets(graded)
      summ[, `:=`(ev_min = ev_min, cap = cap)]
      res[[length(res) + 1]] <- summ[, .(
        ev_min, cap, n_bets, hit_rate, roi, avg_ev, avg_edge, avg_clv_model_minus_close
      )]
    }
  }
  
  rbindlist(res, fill = TRUE)[order(-roi)]
}