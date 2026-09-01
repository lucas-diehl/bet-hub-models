# ============================================================================
# 05_bet_slip.R  —  historical bet slip + per-year ROI for the UNDER strategy
# ----------------------------------------------------------------------------
# Reads the leak-free backtest (ppp_backtest_results.csv) and reconstructs, for
# each past season, exactly which totals bets the strategy would have placed and
# how they graded. Reports flat-unit ROI and a quarter-Kelly bankroll.
#
# Strategy (from the leak-free investigation — see README_PPP.md):
#   the model over-projects totals, so its OVER signals are noise and only the
#   UNDER side has an edge. Two variants:
#     UNDER4      : bet UNDER when over_under - proj_total_A >= 4
#     UNDER3_DOG  : bet UNDER when edge >= 3 AND the model also likes the market
#                   underdog to cover (proj_margin agrees with taking the points)
#
# Outputs: ppp_bet_slip_UNDER4.csv, ppp_bet_slip_UNDER3_DOG.csv
# ============================================================================

source("00_ppp_common.R")
KELLY_FRAC <- 0.25; STAKE_CAP <- 0.05; BANKROLL0 <- 100

d <- read.csv("ppp_backtest_results.csv") %>%
  filter(!is.na(proj_total_A), !is.na(over_hit), !is.na(over_under)) %>%
  mutate(
    edge_pts = over_under - proj_total_A,                      # + = model leans UNDER
    p_under  = 1 - A_totals_prob,                              # model P(under)
    dog_ats  = (!is.na(spread) & spread > 0 & (proj_margin_A + spread) > 0) |
               (!is.na(spread) & spread < 0 & (proj_margin_A + spread) < 0),
    result   = if_else(over_hit == 0, "WIN", "LOSS"),          # UNDER wins when total stays under
    correct  = 1L - over_hit)

# ---- select bets for a strategy --------------------------------------------
select_bets <- function(edge, require_dog) {
  b <- d %>% filter(edge_pts >= edge)
  if (require_dog) b <- b %>% filter(dog_ats)
  b %>% arrange(season, week, game_id)
}

# ---- flat-unit ROI, per season + overall -----------------------------------
flat_table <- function(b) {
  s <- b %>% group_by(season) %>%
    summarise(bets=n(), wins=sum(correct), win=round(100*mean(correct),1),
              units=round(sum(correct)*PAYOUT - (n()-sum(correct)),1),
              roi=round((sum(correct)*PAYOUT-(n()-sum(correct)))/n()*100,1), .groups="drop")
  tot <- b %>% summarise(season=NA, bets=n(), wins=sum(correct), win=round(100*mean(correct),1),
              units=round(sum(correct)*PAYOUT-(n()-sum(correct)),1),
              roi=round((sum(correct)*PAYOUT-(n()-sum(correct)))/n()*100,1))
  bind_rows(s, tot %>% mutate(season=as.integer(NA)))
}

# ---- quarter-Kelly compounding bankroll + drawdown -------------------------
kelly_slip <- function(b) {
  b <- b %>% mutate(ev = p_under*PAYOUT - (1-p_under),
                    kfrac = pmin(pmax((ev/PAYOUT)*KELLY_FRAC, 0), STAKE_CAP))
  bk <- BANKROLL0; peak <- bk; maxdd <- 0
  stake_u <- pnl <- bankroll <- numeric(nrow(b))
  for (i in seq_len(nrow(b))) {
    su <- b$kfrac[i]*bk; stake_u[i] <- su
    p  <- if (b$correct[i]==1) su*PAYOUT else -su
    pnl[i] <- p; bk <- bk + p; bankroll[i] <- bk
    peak <- max(peak, bk); maxdd <- max(maxdd, (peak-bk)/peak)
  }
  b$stake_units <- round(stake_u,2); b$pnl <- round(pnl,2); b$bankroll <- round(bankroll,2)
  attr(b, "final") <- bk; attr(b, "maxdd") <- maxdd
  b
}

report <- function(edge, require_dog, tag, outfile) {
  b <- select_bets(edge, require_dog)
  cat("\n", strrep("=",70), "\n", tag, "\n", strrep("=",70), "\n", sep="")
  cat("FLAT 1-unit staking, per season:\n")
  print(as.data.frame(flat_table(b) %>% mutate(season = ifelse(is.na(season),"ALL",as.character(season)))), row.names=FALSE)
  k <- kelly_slip(b)
  cat(sprintf("\nQuarter-Kelly bankroll (start 100u): final=%.1fu  (%.2fx)  max drawdown=%.0f%%\n",
              attr(k,"final"), attr(k,"final")/BANKROLL0, 100*attr(k,"maxdd")))
  slip <- k %>% transmute(season, week, home_team, away_team, over_under,
                          proj_total = round(proj_total_A,1), edge = round(edge_pts,1),
                          pick = "UNDER", dog_ats, total_points, result,
                          stake_units, pnl, bankroll)
  write.csv(slip, outfile, row.names = FALSE)
  cat(sprintf("saved %s  (%d bets)\n", outfile, nrow(slip)))
  invisible(slip)
}

cat("PPP UNDER-STRATEGY BET SLIP  (leak-free backtest 2021-2025)\n")
report(4, FALSE, "STRATEGY 1: UNDER >= 4 pts",                         "ppp_bet_slip_UNDER4.csv")
report(3, TRUE,  "STRATEGY 2: UNDER >= 3 pts AND model likes dog ATS", "ppp_bet_slip_UNDER3_DOG.csv")

cat("\nNOTE: in-sample (strategy chosen after seeing 2021-2025). Treat ROI as an\n")
cat("upper bound; the honest test is forward performance on 2026. OVER bets are\n")
cat("excluded on purpose (no edge — the model systematically over-projects totals).\n")
