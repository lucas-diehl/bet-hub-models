# ============================================================================
# 06_early_ats.R  —  early-season 4th-down aggressiveness ATS edge
# ----------------------------------------------------------------------------
# Validated signal (leak-free): in the first weeks of the season the market is
# slow to price coaches' 4th-down aggressiveness (a stable, roster-independent
# trait). Betting the MORE 4th-down-aggressive team against the spread, when the
# gap in as-of go-for-it rate is large enough, has beaten the closing spread:
#
#   weeks 1-4, gap>=0.10 : 57.4% ATS, +9.5% ROI, 326 bets, 5/5 seasons (p=.041)
#   weeks 1-2 (strongest): 60.4% ATS, +15.3% ROI
#   weeks 5+             : NO edge (market has caught up) -> not bet
#
# Aggressiveness = coach-persistent as-of go-for-it rate (build_asof_aggressiveness).
# Outputs: early_ats_bet_slip.csv (every historical pick, graded) + prints the
# full slip for the most recent complete season and a per-season ROI summary.
# For a live upcoming slate set PPP_SEASON / PPP_WEEK (only weeks in EARLY_WEEKS).
# ============================================================================

source("00_ppp_common.R")
set.seed(42)

# ---- strategy config --------------------------------------------------------
EARLY_WEEKS <- 1:4     # window where the edge lives (wk1-2 strongest)
AGG_GAP     <- 0.10    # min gap in go-for-it rate to fire (conviction)
N4_MIN      <- 20      # min 4th-down history (attempts) for a stable go-rate
STRONG_GAP  <- 0.20    # higher-conviction tier flag

cat("STEP 6: EARLY-SEASON 4TH-DOWN AGGRESSIVENESS ATS\n"); cat(strrep("=",70), "\n")

betting_lines <- read_rds_retry(file.path(CACHE, "betting_lines.rds"))
game_info     <- read_rds_retry(file.path(CACHE, "game_info.rds"))
sp_ratings    <- read_rds_retry(file.path(CACHE, "sp_ratings.rds"))
fbs_map       <- make_fbs_mapper(sp_ratings)

game_dates <- game_info %>%
  mutate(game_id = if ("game_id" %in% names(.)) game_id else id,
         season  = if ("season"  %in% names(.)) season  else year,
         week    = if ("week"    %in% names(.)) week    else NA_integer_) %>%
  transmute(game_id, season, week,
            home_team, away_team,
            home_score = as.numeric(home_points), away_score = as.numeric(away_points),
            total_points = home_score + away_score)

md <- build_model_data(betting_lines, game_dates) %>%
  mutate(home_team = fbs_map(season, home_team), away_team = fbs_map(season, away_team)) %>%
  filter(home_team != "FCS", away_team != "FCS")

agg <- build_asof_aggressiveness()
md <- md %>%
  left_join(agg %>% rename(h4 = asof_4d_go, h_n4 = n4), by = c("season","game_id","home_team"="team")) %>%
  left_join(agg %>% rename(a4 = asof_4d_go, a_n4 = n4), by = c("season","game_id","away_team"="team"))

# ---- apply the rule ---------------------------------------------------------
picks <- md %>%
  filter(week %in% EARLY_WEEKS, !is.na(h4), !is.na(a4), h_n4 >= N4_MIN, a_n4 >= N4_MIN,
         abs(h4 - a4) >= AGG_GAP) %>%
  mutate(agg_diff   = h4 - a4,
         pick_home  = agg_diff > 0,
         pick_team  = if_else(pick_home, home_team, away_team),
         pick_spread= if_else(pick_home, spread, -spread),   # the number our side lays/gets
         gap        = round(abs(agg_diff), 3),
         tier       = if_else(gap >= STRONG_GAP, "STRONG", "std"),
         go_rate_pick = round(if_else(pick_home, h4, a4), 3),
         go_rate_opp  = round(if_else(pick_home, a4, h4), 3),
         # graded only if the game is complete
         correct = if_else(!is.na(home_covered),
                           as.integer(if_else(pick_home, home_covered, 1L - home_covered)), NA_integer_)) %>%
  arrange(season, week, desc(gap))

# ---- per-season ROI ---------------------------------------------------------
graded <- picks %>% filter(!is.na(correct))
roi <- function(x) if (length(x)) round((sum(x)*PAYOUT - (length(x)-sum(x)))/length(x)*100,1) else NA
cat(sprintf("\nRule: weeks %s, bet MORE 4th-down-aggressive team ATS when go-rate gap >= %.2f (history >= %d)\n\n",
            paste(range(EARLY_WEEKS), collapse="-"), AGG_GAP, N4_MIN))
cat("Per-season (flat 1u):\n")
print(as.data.frame(graded %>% group_by(season) %>%
  summarise(bets=n(), wins=sum(correct), win=round(100*mean(correct),1),
            units=round(sum(correct)*PAYOUT-(n()-sum(correct)),1), roi=roi(correct), .groups="drop")), row.names=FALSE)
cat(sprintf("  OVERALL: %d bets, %.1f%%, %+.1fu, ROI %+.1f%%  |  STRONG tier (gap>=%.2f): %d bets, %.1f%%\n",
            nrow(graded), 100*mean(graded$correct), sum(graded$correct)*PAYOUT-(nrow(graded)-sum(graded$correct)),
            roi(graded$correct), STRONG_GAP,
            sum(graded$tier=="STRONG"), 100*mean(graded$correct[graded$tier=="STRONG"])))

# ---- full bet slip for the most recent COMPLETE season ---------------------
last_season <- max(graded$season)
cat(sprintf("\n===== FULL BET SLIP — %d (weeks %s) =====\n", last_season, paste(range(EARLY_WEEKS), collapse="-")))
slip_cols <- picks %>% transmute(season, week, matchup = paste0(away_team, " @ ", home_team),
                                 pick = pick_team, pick_spread = round(pick_spread,1), gap, tier,
                                 go_rate_pick, go_rate_opp,
                                 result = case_when(is.na(correct) ~ "pending", correct==1 ~ "WIN", TRUE ~ "LOSS"))
print(as.data.frame(slip_cols %>% filter(season == last_season)), row.names = FALSE)

write.csv(slip_cols, "early_ats_bet_slip.csv", row.names = FALSE)
cat(sprintf("\n✓ saved early_ats_bet_slip.csv (%d picks, all seasons)\n", nrow(slip_cols)))

# ---- live mode: upcoming slate (weeks 1-4 only) ----------------------------
tgt_s <- suppressWarnings(as.integer(Sys.getenv("PPP_SEASON", "")))
tgt_w <- suppressWarnings(as.integer(Sys.getenv("PPP_WEEK", "")))
if (!is.na(tgt_s) && !is.na(tgt_w)) {
  if (tgt_w %in% EARLY_WEEKS) {
    up <- slip_cols %>% filter(season == tgt_s, week == tgt_w)
    cat(sprintf("\n===== UPCOMING PICKS %d week %d =====\n", tgt_s, tgt_w))
    if (nrow(up)) print(as.data.frame(up), row.names = FALSE) else cat("  no qualifying games\n")
  } else cat(sprintf("\n(week %d is outside the early-season edge window %s — no ATS picks)\n",
                     tgt_w, paste(range(EARLY_WEEKS), collapse="-")))
}

cat("\nNOTE: in-sample (edge found on 2021-2025); forward-test on 2026. Flat 1u staking\n")
cat("(no calibrated prob for this rule). Edge is strongest in weeks 1-2 and gone by week 5.\n")
