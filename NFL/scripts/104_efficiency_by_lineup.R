source("R/utilities.R")
source("R/qb_starter.R")
source("R/fantasy_prop_model.R")
source("C:/Users/ljdie/OneDrive/Documents/DFS ENGINE/spine/R/lineup_percentile.R")
assert_packages(); ensure_directories()
suppressPackageStartupMessages(library(data.table))
options(width = 205)

## RE-TEST THE EFFICIENCY SCORE ON THE METRIC THAT TRACKS MONEY.
##
## ep_per_touch (nflfastR expected points of the game state BEFORE each touch =
## opportunity QUALITY, independent of what the player did with it) was built in
## script 91, evaluated in script 93 on MAE / dR2, scored +0.004, and was KILLED.
##
## Script 102 then showed dR2 and median lineup percentile rank features in a
## DIFFERENT ORDER -- so that rejection was made on a yardstick the thesis argues
## is the wrong one for DFS. This re-runs the same feature against the ranking
## metric. Script 103's discipline applies: a lift that is really a coverage
## artifact does not count, so coverage is reported alongside.

feat <- as.data.table(readRDS("data/processed/fantasy_prop_features.rds"))
stats <- as.data.table(readRDS("data/raw/player_stats_2021_2026.rds"))[season_type == "REG"]
ep <- as.data.table(readRDS("data/processed/player_ep_per_touch.rds"))

## LAGGED roll, exactly as add_ngs_features does -- the value for week w must use
## only weeks < w, or the comparison is leakage rather than signal. The _r5 suffix
## is what makes fantasy_model_feature_names() sweep it up (regex _r3|_r5|_r8|_sd5).
setorder(ep, player_id, season, week)
ep[, eppt_r5 := fantasy_lagged_roll(ep_per_touch, 5L, "mean"), by = player_id]
ep[, eptouch_r5 := fantasy_lagged_roll(ep_touches, 5L, "mean"), by = player_id]

feat <- merge(feat, ep[, .(player_id, season, week, eppt_r5, eptouch_r5)],
              by = c("player_id", "season", "week"), all.x = TRUE)
eff_cols <- c("eppt_r5", "eptouch_r5")
cat(sprintf("efficiency coverage: %.1f%% of %d player-weeks\n",
            100 * mean(!is.na(feat$eppt_r5)), nrow(feat)))
cat(sprintf("mean fantasy pts  covered=%.2f  uncovered=%.2f\n",
            feat[!is.na(eppt_r5), mean(fantasy_points_ppr, na.rm = TRUE)],
            feat[is.na(eppt_r5), mean(fantasy_points_ppr, na.rm = TRUE)]))

nkey <- function(x) tolower(gsub("[^a-z]", "", x))
sal_hist <- as.data.table(readRDS("data/processed/dfs_salaries_dk_2022_plus.rds"))
sal_hist[, nk := nkey(player_name)]

specs <- fantasy_target_specifications()[c("receptions", "receiving_yards",
                                           "receiving_tds", "rushing_yards",
                                           "rushing_tds", "passing_yards",
                                           "passing_tds")]
ppr_of <- function(d) {
  g <- function(n) if (n %in% names(d)) d[[n]] else 0
  g("receptions") + 0.1 * g("receiving_yards") + 6 * g("receiving_tds") +
    0.1 * g("rushing_yards") + 6 * g("rushing_tds") +
    0.04 * g("passing_yards") + 4 * g("passing_tds")
}

pred_ppr <- function(ff, label) {
  preds <- list()
  for (tn in names(specs)) {
    sp <- specs[[tn]]
    td <- as.data.table(fantasy_target_candidates(as.data.frame(ff), sp$family))
    for (ts in 2024:2025) {
      tr <- td[season < ts & prior_games >= 1]
      te <- td[season == ts & prior_games >= 1]
      if (nrow(tr) < 200 || !nrow(te)) next
      fit <- fit_fantasy_stat_model(as.data.frame(tr), as.data.frame(te),
                                    sp$outcome, sp$objective,
                                    20260728L + ts + match(tn, names(specs)))
      preds[[length(preds) + 1L]] <- data.table(
        player_id = te$player_id, season = te$season, week = te$week,
        target = tn, pred = fit$prediction)
      rm(fit); gc(FALSE)
    }
  }
  P <- dcast(rbindlist(preds), player_id + season + week ~ target,
             value.var = "pred", fill = 0)
  P[, proj_ppr := ppr_of(P)]
  cat("  ", label, "->", nrow(P), "player-weeks\n")
  P[, .(player_id, season, week, proj_ppr)]
}

## Hold NGS and qb_pair OUT of both arms so the ONLY difference is the efficiency
## score -- otherwise the NGS coverage artifact from script 103 contaminates this.
hold_out <- c(grep("^ngs_", names(feat), value = TRUE),
              "qb_pair_delta_r5", "qb_pair_games_r5")

variants <- list(
  baseline   = c(hold_out, eff_cols),   # zero everything incl. efficiency
  efficiency = hold_out                 # efficiency ON
)

cat("\nbuilding projections per variant (2024-2025 held out)...\n")
PV <- lapply(names(variants), function(v) {
  ff <- copy(feat)
  z <- intersect(variants[[v]], names(ff))
  if (length(z)) ff[, (z) := 0]
  pred_ppr(ff, v)
})
names(PV) <- names(variants)

actual <- stats[, .(player_id, season, week, act = fantasy_points_ppr,
                    nk = nkey(player_display_name))]
res <- list()
for (v in names(PV)) {
  P <- merge(PV[[v]], actual, by = c("player_id", "season", "week"))
  P <- merge(P, sal_hist[, .(nk, season, week, salary, pos = position)],
             by = c("nk", "season", "week"))
  P <- P[is.finite(salary) & salary > 0 & pos %in% c("QB", "RB", "WR", "TE")]
  slates <- split(P, by = c("season", "week"), drop = TRUE)
  slates <- slates[vapply(slates, nrow, integer(1)) >= 40]
  sl <- lapply(slates, function(d)
    list(proj = d$proj_ppr, actual = d$act, salary = d$salary, pos = d$pos))
  out <- lineup_percentile_summary(sl, n_slots = 8L, cap = 47000L,
                                   n_sample = 5000L, label = v,
                                   pos_req = LP_NFL_POS_REQ)
  res[[v]] <- out
  cat(sprintf("  %-12s slates=%2d  MEDIAN pctile=%.4f  mean=%.4f  beat-median=%.1f%%\n",
              v, out$slates, out$median_pctile, out$mean_pctile,
              100 * out$beat_median_pct))
}

cat("\n=== efficiency score, judged on lineup percentile instead of dR2 ===\n")
print(rbindlist(lapply(res, as.data.table))[order(-median_pctile)])
cat(sprintf("\ndelta vs baseline: %+.4f median pctile\n",
            res$efficiency$median_pctile - res$baseline$median_pctile))
cat("(script 93 killed this on dR2 = +0.004)\n")
