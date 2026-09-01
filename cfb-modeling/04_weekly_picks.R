# ============================================================================
# 04_weekly_picks.R  —  Phase 7: weekly PPP projections + recommended bets
# ----------------------------------------------------------------------------
# Produces projected totals/margins for one week's slate using the validated
# engine (Approach A: opponent-adjusted PPD, scale-calibrated on completed
# history) and recommends bets where the projection diverges from the market.
#
# Backtest verdict (03_models_backtest.R): the edge is on TOTALS. Bet over/under
# when |proj_total - over_under| >= TOT_EDGE. Spreads showed no edge vs the
# closing line, so spread rows are informational only (recommended = FALSE)
# unless a strict SPREAD_EDGE is cleared (off by default).
#
# Weekly workflow during the season:
#   1) refresh data (add the new season to 01/02 caches — see refresh_2026())
#   2) source 01_build_possessions.R and 02_build_asof_ratings.R
#   3) run this script with TARGET_SEASON / TARGET_WEEK set to the UPCOMING week
#
# Output: weekly_picks_<season>_wk<week>.csv
# ============================================================================

source("00_ppp_common.R")
set.seed(42)

# ---- config -----------------------------------------------------------------
TARGET_SEASON <- as.integer(Sys.getenv("PPP_SEASON", "2025"))  # default: dry-run on 2025
TARGET_WEEK   <- suppressWarnings(as.integer(Sys.getenv("PPP_WEEK", "")))  # NA -> auto
# STRATEGY (leak-free investigation): the model over-projects totals, so only the
# UNDER side has an edge (OVER is noise). Two recommended variants:
#   UNDER4      : bet UNDER when (over_under - proj_total) >= UNDER_EDGE  [56.6%, 5/5 seasons]
#   UNDER3_DOG  : bet UNDER when edge >= DOG_EDGE AND model likes the dog ATS [57.2%, 5/5]
# We recommend the UNION and tag which variant each bet satisfies.
UNDER_EDGE    <- 4.0     # simple UNDER threshold (points)
DOG_EDGE      <- 3.0     # lower threshold when the dog-ATS interaction also holds
SPREAD_EDGE   <- Inf     # spreads: no reliable edge -> never recommended
# Kelly staking (matches bankroll_sim.R): quarter Kelly, 5% cap, 100u bankroll
KELLY_FRAC <- 0.25; STAKE_CAP <- 0.05; BANKROLL <- 100
calc_kelly_units <- function(ev) {
  pct <- pmin(pmax((ev / PAYOUT) * KELLY_FRAC, 0), STAKE_CAP); BANKROLL * pct
}

cat("STEP 7: WEEKLY PICKS\n"); cat(strrep("=", 78), "\n")

# ---- load cached inputs -----------------------------------------------------
asof          <- read_rds_retry(file.path(CACHE, "asof_ratings.rds"))
betting_lines <- read_rds_retry(file.path(CACHE, "betting_lines.rds"))
game_info     <- read_rds_retry(file.path(CACHE, "game_info.rds"))
sp_ratings    <- read_rds_retry(file.path(CACHE, "sp_ratings.rds"))
fbs_map       <- make_fbs_mapper(sp_ratings)

game_dates <- game_info %>%
  mutate(game_id = if ("game_id" %in% names(.)) game_id else id,
         season  = if ("season"  %in% names(.)) season  else year,
         week    = if ("week"    %in% names(.)) week    else NA_integer_) %>%
  transmute(game_id, season, week,
            start_date = as.POSIXct(start_date, format = "%Y-%m-%dT%H:%M:%S"),
            home_team, away_team,
            home_score = as.numeric(home_points), away_score = as.numeric(away_points),
            total_points = home_score + away_score)

model_data <- build_model_data(betting_lines, game_dates)
md <- attach_ratings(model_data, asof, fbs_map) %>%
  filter(home_team != "FCS", away_team != "FCS")   # bet only FBS-vs-FBS (real ratings)

# auto-pick the target week if not supplied: latest week in the target season
if (is.na(TARGET_WEEK)) {
  wk <- md %>% filter(season == TARGET_SEASON)
  if (nrow(wk) == 0) stop(sprintf("No games cached for season %d. Run the weekly refresh first.", TARGET_SEASON))
  TARGET_WEEK <- max(wk$week, na.rm = TRUE)
}
cat(sprintf("  target: season %d, week %d\n", TARGET_SEASON, TARGET_WEEK))

# ---- fit production calibration on COMPLETED history (leak-free) ------------
hist <- md %>% filter(!is.na(actual_margin),
                      season < TARGET_SEASON | (season == TARGET_SEASON & week < TARGET_WEEK))
if (nrow(hist) < 500) stop("Not enough completed history to calibrate.")
cal_m <- lm(actual_margin ~ proj_margin_A, data = hist)
cal_t <- lm(total_points  ~ proj_total_A,  data = hist)
sig_t <- sd(hist$total_points  - predict(cal_t, hist), na.rm = TRUE)
sig_m <- sd(hist$actual_margin - predict(cal_m, hist), na.rm = TRUE)
cat(sprintf("  calibrated on %d games (sigma_total=%.1f, sigma_margin=%.1f)\n", nrow(hist), sig_t, sig_m))

# ---- project the target slate ----------------------------------------------
slate <- md %>% filter(season == TARGET_SEASON, week == TARGET_WEEK)
if (nrow(slate) == 0) stop(sprintf("No games with lines for %d week %d.", TARGET_SEASON, TARGET_WEEK))

slate <- slate %>% mutate(
  proj_total  = predict(cal_t, .),
  proj_margin = predict(cal_m, .),
  under_edge  = over_under - proj_total,                  # + => model leans UNDER
  spread_edge = proj_margin + spread,                    # + => model leans HOME (ATS)
  # model likes the market underdog to cover the spread
  dog_ats     = (!is.na(spread) & spread > 0 & spread_edge > 0) |
                (!is.na(spread) & spread < 0 & spread_edge < 0),
  p_under     = 1 - prob_over(proj_total, over_under, sig_t),
  totals_ev   = p_under * PAYOUT - (1 - p_under),
  # UNDER-only strategy (OVER has no edge). Two variants, union recommended.
  bet_under4     = under_edge >= UNDER_EDGE,
  bet_under3_dog = under_edge >= DOG_EDGE & dog_ats,
  totals_bet     = (bet_under4 | bet_under3_dog) & totals_ev > 0,
  strategy       = case_when(bet_under4 & bet_under3_dog ~ "UNDER4+DOG",
                             bet_under4                  ~ "UNDER4",
                             bet_under3_dog              ~ "UNDER3_DOG", TRUE ~ ""),
  totals_units   = if_else(totals_bet, round(calc_kelly_units(totals_ev), 2), 0),
  # spread projection is informational only (no reliable edge)
  spread_pick    = if_else(spread_edge > 0, home_team, away_team)
)

picks <- slate %>%
  transmute(season, week, home_team, away_team,
            over_under, proj_total = round(proj_total, 1), under_edge = round(under_edge, 1),
            pick = if_else(totals_bet, "UNDER", ""), strategy, dog_ats,
            p_under = round(p_under, 3), totals_ev = round(totals_ev, 3),
            totals_bet, totals_units,
            spread, proj_margin = round(proj_margin, 1), spread_pick_info = spread_pick,
            actual_margin, total_points, over_hit) %>%
  arrange(desc(under_edge))

# ---- report -----------------------------------------------------------------
rec <- picks %>% filter(totals_bet)
cat(sprintf("\n  %d games on slate; %d recommended UNDER bets:\n\n", nrow(picks), nrow(rec)))
print(as.data.frame(rec %>% select(home_team, away_team, over_under, proj_total,
                                   under_edge, strategy, dog_ats, p_under, totals_units)), row.names = FALSE)

# dry-run grading if the week is already complete (UNDER wins when over_hit==0)
if (nrow(rec) > 0 && all(!is.na(rec$over_hit))) {
  rec_g <- rec %>% mutate(correct = 1L - over_hit)
  w <- sum(rec_g$correct); n <- nrow(rec_g)
  cat(sprintf("\n  [dry-run grade] UNDER %d-%d (%.1f%%)  profit=%+.2fu (flat)  (Kelly staked %.1fu)\n",
              w, n - w, 100*w/n, w*PAYOUT - (n - w), sum(rec_g$totals_units)))
}

out <- sprintf("weekly_picks_%d_wk%d.csv", TARGET_SEASON, TARGET_WEEK)
write.csv(picks, out, row.names = FALSE)
cat(sprintf("\n✓ saved %s\n", out))

# ============================================================================
# refresh_2026(): call once the 2026 season is underway to pull new data.
# Requires cfbfastR + the CFBD API key (already in .Renviron as CFB_API_KEY).
# Appends 2026 to the caches; then re-source 01 and 02 before running picks.
# ============================================================================
refresh_2026 <- function(year = 2026) {
  message("Loading cfbfastR (network)…")
  suppressWarnings(suppressMessages(library(cfbfastR)))
  key <- Sys.getenv("CFB_API_KEY"); if (key == "") key <- Sys.getenv("CFBD_API_KEY")
  if (key != "") Sys.setenv(CFBD_API_KEY = key)
  add <- function(cache, loader) {
    old <- if (file.exists(cache)) readRDS(cache) else NULL
    new <- tryCatch(loader(year), error = function(e) { message("  failed: ", conditionMessage(e)); NULL })
    if (!is.null(new)) saveRDS(dplyr::bind_rows(old %>% dplyr::filter(if ("season" %in% names(.)) season != year else TRUE), new), cache)
  }
  add(file.path(CACHE,"pbp_data.rds"),     function(y) cfbfastR::load_cfb_pbp(seasons = y))
  add(file.path(CACHE,"game_info.rds"),    function(y) cfbfastR::cfbd_game_info(year = y, season_type = "regular"))
  add(file.path(CACHE,"betting_lines.rds"),function(y) cfbfastR::cfbd_betting_lines(year = y, season_type = "regular"))
  add(file.path(CACHE,"sp_ratings.rds"),   function(y) cfbfastR::cfbd_ratings_sp(year = y))
  message("Done. Now re-source 01_build_possessions.R and 02_build_asof_ratings.R, then re-run this script.")
}
