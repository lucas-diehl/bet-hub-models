suppressPackageStartupMessages(library(tidyverse))

df <- read.csv("predictions_2025.csv") %>%
  filter(!is.na(home_covered), !is.na(over_hit)) %>%
  arrange(week, game_id)

# ── EV thresholds ──────────────────────────────────────────────────────────
SPREAD_EV_MIN <- 0.15
TOTALS_EV_MIN <- 0.14
STARTING_BK   <- 100   # units
ODDS_PAYOUT   <- 0.909 # payout per unit at -110

# ── Kelly fraction ─────────────────────────────────────────────────────────
# Full Kelly: f = EV / payout_odds
# Quarter Kelly used here — standard conservative practice
KELLY_FRAC <- 0.25

# ── Bet sizing: proportion of CURRENT bankroll via fractional Kelly ─────────
calc_kelly_units <- function(ev, bankroll, kelly_frac = KELLY_FRAC) {
  pct  <- (ev / ODDS_PAYOUT) * kelly_frac   # fraction of bankroll to wager
  pct  <- pmin(pmax(pct, 0), 0.05)          # cap at 5% per bet (risk mgmt)
  bankroll * pct
}

# ── Tag qualifying bets ────────────────────────────────────────────────────
bets <- bind_rows(
  df %>%
    filter(spread_ev >= SPREAD_EV_MIN) %>%
    transmute(
      game_id, week, home_team, away_team,
      bet_type   = "spread",
      ev         = spread_ev,
      pick       = if_else(spread_prob > 0.5, home_team, away_team),
      win        = as.integer(
                     (spread_prob > 0.5 & home_covered == 1) |
                     (spread_prob <= 0.5 & home_covered == 0))
    ),
  df %>%
    filter(totals_ev >= TOTALS_EV_MIN) %>%
    transmute(
      game_id, week, home_team, away_team,
      bet_type   = "totals",
      ev         = totals_ev,
      pick       = if_else(totals_prob > 0.5, "OVER", "UNDER"),
      win        = as.integer(
                     (totals_prob > 0.5 & over_hit == 1) |
                     (totals_prob <= 0.5 & over_hit == 0))
    )
) %>%
arrange(week, game_id, bet_type)

cat(sprintf("Total bets: %d  (spread: %d  totals: %d)\n\n",
            nrow(bets),
            sum(bets$bet_type == "spread"),
            sum(bets$bet_type == "totals")))

# ── Simulate bankroll ──────────────────────────────────────────────────────
bankroll   <- STARTING_BK
peak_bk    <- STARTING_BK
max_dd     <- 0
bet_log    <- vector("list", nrow(bets))

for (i in seq_len(nrow(bets))) {
  b      <- bets[i, ]
  stake  <- calc_kelly_units(b$ev, bankroll)
  pnl    <- if (b$win == 1) stake * ODDS_PAYOUT else -stake
  bankroll <- bankroll + pnl

  peak_bk <- max(peak_bk, bankroll)
  dd      <- (peak_bk - bankroll) / peak_bk * 100
  max_dd  <- max(max_dd, dd)

  bet_log[[i]] <- tibble(
    i          = i,
    week       = b$week,
    bet_type   = b$bet_type,
    ev         = b$ev,
    stake      = round(stake, 3),
    win        = b$win,
    pnl        = round(pnl, 3),
    bankroll   = round(bankroll, 2),
    drawdown   = round(dd, 1)
  )
}

log_df <- bind_rows(bet_log)

# ── Summary stats ──────────────────────────────────────────────────────────
total_wagered <- sum(log_df$stake)
total_pnl     <- bankroll - STARTING_BK
total_roi     <- total_pnl / total_wagered * 100
wins          <- sum(log_df$win)
losses        <- nrow(log_df) - wins

cat(strrep("=", 70), "\n")
cat("BANKROLL SIMULATION — 2025 SEASON\n")
cat(strrep("=", 70), "\n\n")
cat(sprintf("  Bet sizing   : Quarter-Kelly (capped 5%% bankroll per bet)\n"))
cat(sprintf("  EV threshold : Spread >= %.0f%%  |  Totals >= %.0f%%\n\n",
            SPREAD_EV_MIN * 100, TOTALS_EV_MIN * 100))
cat(sprintf("  Starting bankroll : %.0f units\n", STARTING_BK))
cat(sprintf("  Final bankroll    : %.1f units  (%+.1f units)\n",
            bankroll, total_pnl))
cat(sprintf("  Total wagered     : %.1f units\n", total_wagered))
cat(sprintf("  Overall ROI       : %+.1f%%\n", total_roi))
cat(sprintf("  Record            : %d-%d  (%.1f%%)\n",
            wins, losses, wins / nrow(log_df) * 100))
cat(sprintf("  Max drawdown      : %.1f%%\n", max_dd))
cat(sprintf("  Peak bankroll     : %.1f units\n\n", peak_bk))

# ── By bet type ────────────────────────────────────────────────────────────
cat(strrep("-", 70), "\n")
cat("BY BET TYPE:\n")
log_df %>%
  group_by(bet_type) %>%
  summarise(
    bets      = n(),
    wins      = sum(win),
    win_pct   = round(wins / bets * 100, 1),
    wagered   = round(sum(stake), 1),
    profit    = round(sum(pnl), 1),
    roi       = round(sum(pnl) / sum(stake) * 100, 1),
    avg_stake = round(mean(stake), 2),
    avg_ev    = round(mean(ev) * 100, 1),
    .groups   = "drop"
  ) %>%
  print()

# ── By week ────────────────────────────────────────────────────────────────
cat("\n")
cat(strrep("-", 70), "\n")
cat("BY WEEK:\n")
log_df %>%
  filter(!is.na(week)) %>%
  group_by(week) %>%
  summarise(
    bets    = n(),
    wins    = sum(win),
    win_pct = round(wins / bets * 100, 1),
    profit  = round(sum(pnl), 1),
    roi     = round(sum(pnl) / sum(stake) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(week) %>%
  print(n = 20)

# ── Bankroll by week ───────────────────────────────────────────────────────
cat("\n")
cat(strrep("-", 70), "\n")
cat("BANKROLL PROGRESSION BY WEEK:\n")
log_df %>%
  filter(!is.na(week)) %>%
  group_by(week) %>%
  summarise(
    end_bk   = round(last(bankroll), 1),
    week_pnl = round(sum(pnl), 1),
    bets     = n(),
    .groups  = "drop"
  ) %>%
  arrange(week) %>%
  mutate(
    bar = strrep("|", pmax(0, round(week_pnl)))
  ) %>%
  print(n = 20)

# ── Worst stretches ────────────────────────────────────────────────────────
cat("\n")
cat(strrep("-", 70), "\n")
cat("LARGEST LOSING STREAKS:\n")
rle_res <- rle(log_df$win == 0)
streak_df <- tibble(
  length  = rle_res$lengths,
  is_loss = rle_res$values
) %>%
  filter(is_loss) %>%
  arrange(desc(length)) %>%
  head(5)
print(streak_df)

cat("\n✓ Full log saved: bankroll_log_2025.csv\n")
write.csv(log_df, "bankroll_log_2025.csv", row.names = FALSE)
