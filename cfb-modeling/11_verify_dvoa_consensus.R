# ============================================================================
# 11_verify_dvoa_consensus.R — independent reproduction of the DVOA-consensus
#                               result claimed in FEATURE_REGISTRY.md
#                               (ARM_O_V2, dated 2026-09-09) and deployed in
#                               07_write_feed.R.
# ----------------------------------------------------------------------------
# Not a rewrite of that work — a from-scratch check that it reproduces, using
# the SAME production functions (00_ppp_common.R) and the SAME DVOA formula
# and vote logic as 07_write_feed.R (copied verbatim from there, not
# reinvented), but computed independently here rather than trusted from the
# registry text.
#
# Data source note: this uses the NATIVELY cached `spread_open` field in
# betting_lines.rds (~29-35% population per DATA_SCIENTIST_HANDOFF §2.2) —
# the "cached Bovada/DK spread_open" arm the registry describes as one of two
# independent confirmations, NOT the cleaner Odds API historical pull
# (odds_hist_raw.csv/odds_open_raw.csv). That pull needs team-name matching
# logic that exists only in the (unsaved) exploratory session that produced
# the registry entries; reusing the always-available native field means this
# reproduction costs zero new API calls and zero new matching code, at the
# price of a smaller, though still large, sample.
#
# Walk-forward: train 2021-2023, test 2024-2025 (matches every OOS split
# already used in this project).
# ============================================================================

source("00_ppp_common.R")
set.seed(42)

cat("STEP 11: INDEPENDENT VERIFICATION — DVOA consensus vs PPD alone\n\n")

asof <- read_rds_retry(file.path(CACHE, "asof_ratings.rds"))
bl   <- read_rds_retry(file.path(CACHE, "betting_lines.rds"))
gi   <- read_rds_retry(file.path(CACHE, "game_info.rds"))
sp   <- read_rds_retry(file.path(CACHE, "sp_ratings.rds"))
dvr  <- read_rds_retry(file.path(CACHE, "dvoa_asof_variants.rds"))
fbs  <- make_fbs_mapper(sp)

gi2 <- gi %>% mutate(game_id = if ("game_id" %in% names(.)) game_id else id,
                     season  = if ("season"  %in% names(.)) season  else year)
gd <- gi2 %>% transmute(game_id, season, week, home_team, away_team,
  home_score = as.numeric(home_points), away_score = as.numeric(away_points),
  total_points = home_score + away_score)

md <- attach_ratings(build_model_data(bl, gd), asof, fbs) %>%
  filter(home_team != "FCS", away_team != "FCS")

before_n <- nrow(md)
md <- md %>% filter(!is.na(spread_open))   # TRUE opens only, no close-fallback
cat(sprintf("Games with a native spread_open: %d of %d (%.1f%%)\n",
           nrow(md), before_n, 100 * nrow(md) / before_n))

# ---- DVOA join, exact formula from 07_write_feed.R --------------------------
DVOA_VARIANTS <- c("v3", "v4", "v5", "v6", "v7")
dvh <- dvr %>% rename_with(~paste0("h_", .x), -c(season, as_of_week, team)) %>% rename(home_team = team)
dva <- dvr %>% rename_with(~paste0("a_", .x), -c(season, as_of_week, team)) %>% rename(away_team = team)
md <- md %>% left_join(dvh, by = c("season", "week" = "as_of_week", "home_team")) %>%
             left_join(dva, by = c("season", "week" = "as_of_week", "away_team"))
for (v in DVOA_VARIANTS)
  md[[paste0("dvoa_", v)]] <- (md[[paste0("h_aoff_", v)]] + md[[paste0("a_adef_", v)]]) -
                              (md[[paste0("a_aoff_", v)]] + md[[paste0("h_adef_", v)]])

md$open_margin <- -md$spread_open   # spread is home-line convention; margin is home-minus-away

# ---- walk-forward: train 2021-23, test 2024-25 (matches every OOS split already used) ----
train <- md %>% filter(season %in% 2021:2023, !is.na(actual_margin))
test  <- md %>% filter(season %in% 2024:2025, !is.na(actual_margin))
cat(sprintf("Train (2021-23): %d games. Test (2024-25): %d games.\n\n", nrow(train), nrow(test)))

cal_ppd  <- lm(actual_margin ~ proj_margin_A, data = train)
cal_dvoa <- list()
for (v in DVOA_VARIANTS) {
  h2 <- train[!is.na(train[[paste0("dvoa_", v)]]), ]
  cal_dvoa[[v]] <- if (nrow(h2) >= 200) lm(as.formula(paste0("actual_margin ~ dvoa_", v)), data = h2) else NULL
  cat(sprintf("  %s calibrated on %d train games\n", v, nrow(h2)))
}

test <- test %>% mutate(
  proj_margin  = predict(cal_ppd, .),
  ppd_edge     = proj_margin - open_margin,
  ppd_mag      = abs(ppd_edge),
  ppd_pick_home= ppd_edge > 0,
  # Graded against the OPEN, because that's the number actually in the bet
  # slip for this arm (Arm O bets the disagreement vs the frozen opening
  # line, not the close) - grading vs the close would silently re-introduce
  # exactly the number this arm exists to beat.
  covered_home_open = as.integer((actual_margin + spread_open) > 0),
  push_open          = (actual_margin + spread_open) == 0
)

ATS_OPEN_EDGE_MIN <- 3.0
ATS_CONSENSUS_MIN <- 3L

vote_mat <- sapply(DVOA_VARIANTS, function(v) {
  if (is.null(cal_dvoa[[v]])) return(rep(0L, nrow(test)))
  pv <- suppressWarnings(predict(cal_dvoa[[v]], test))
  e  <- pv - test$open_margin
  as.integer(!is.na(e) & abs(e) >= ATS_OPEN_EDGE_MIN & sign(e) == sign(test$ppd_edge))
})
test$votes <- rowSums(vote_mat)

qualifying <- test %>% filter(ppd_mag >= ATS_OPEN_EDGE_MIN)
cat(sprintf("\nQualifying PPD-vs-open bets (mag>=%.0f), test period: n=%d\n",
           ATS_OPEN_EDGE_MIN, nrow(qualifying)))

grade <- function(d) {
  win <- ifelse(d$ppd_pick_home, d$covered_home_open == 1, d$covered_home_open == 0)
  d2 <- d[!d$push_open, ]
  win2 <- win[!d$push_open]
  tibble(n = length(win2), wins = sum(win2), win_pct = mean(win2))
}

cat("\n=== Win rate by consensus vote count (dose-response) ===\n")
dose <- lapply(0:5, function(k) {
  g <- grade(qualifying %>% filter(votes >= k))
  tibble(min_votes = k, n = g$n, win_pct = round(100 * g$win_pct, 1))
})
dose_tbl <- bind_rows(dose)
print(as.data.frame(dose_tbl))

cat("\n=== Deployed rule: PPD mag>=3 AND votes>=3 ===\n")
deployed <- grade(qualifying %>% filter(votes >= ATS_CONSENSUS_MIN))
cat(sprintf("n=%d win=%.1f%% (breakeven at -110 = 52.4%%)\n",
           deployed$n, 100 * deployed$win_pct))
if (deployed$n >= 10) {
  bt <- binom.test(deployed$wins, deployed$n, p = 0.524, alternative = "greater")
  cat(sprintf("binom.test p=%.4f\n", bt$p.value))
}

cat("\n=== Baseline: PPD alone, no consensus filter ===\n")
baseline <- grade(qualifying)
cat(sprintf("n=%d win=%.1f%%\n", baseline$n, 100 * baseline$win_pct))

cat("\n=== Monotonicity check (the key evidence per the registry) ===\n")
increasing <- all(diff(dose_tbl$win_pct[dose_tbl$n >= 15]) >= -0.5)  # allow tiny noise
cat("Win rate non-decreasing in vote count (allowing small noise):", increasing, "\n")

readr::write_csv(dose_tbl, "outputs/dvoa_consensus_verification.csv")
cat("\nWrote outputs/dvoa_consensus_verification.csv\n")
