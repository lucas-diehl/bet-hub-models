source("R/utilities.R")
source("R/qb_starter.R")
source("R/fantasy_prop_model.R")
source("C:/Users/ljdie/OneDrive/Documents/DFS ENGINE/spine/R/lineup_percentile.R")
assert_packages(); ensure_directories()
suppressPackageStartupMessages(library(data.table))
options(width = 205)

## Script 102 showed NGS moving median lineup percentile +0.166 while dR2 said
## only +0.004. Before believing that, rule out the explanation I already
## confirmed for the dR2 artifact earlier: NGS publishes ONLY qualifying
## receivers (21.4% coverage, 4.98 vs 1.12 mean receptions), and the columns are
## left_joined with NA elsewhere -- so "NGS is present" is an implicit
## "established high-volume receiver" flag a tree can split on directly.
##
## That flag barely helps ERROR (hence dR2 ~0) but should help RANKING a lot,
## which is exactly the disagreement we observed. So:
##
##   baseline    : no NGS at all
##   flag_only   : NGS values all zeroed, but keep a 0/1 coverage indicator
##   ngs_full    : NGS as shipped
##
## If flag_only ~= ngs_full, the tracking measurements contribute nothing and the
## lift is a volume proxy we should source from an honest feature (target share)
## rather than from NGS coverage. If ngs_full > flag_only, the separation/cushion
## /air-yards numbers carry real ranking signal and NGS earns its place.

feat <- as.data.table(readRDS("data/processed/fantasy_prop_features.rds"))
stats <- as.data.table(readRDS("data/raw/player_stats_2021_2026.rds"))[season_type == "REG"]

ngs_cols <- grep("^ngs_", names(feat), value = TRUE)
qbp_cols <- c("qb_pair_delta_r5", "qb_pair_games_r5")
cat("NGS columns:", paste(ngs_cols, collapse = ", "), "\n")

## coverage indicator, from the same lagged roll the model actually sees
probe <- ngs_cols[1]
feat[, ngs_covered_r5 := as.integer(!is.na(get(probe)))]
cat(sprintf("NGS coverage: %.1f%% of %d player-weeks\n",
            100 * mean(feat$ngs_covered_r5), nrow(feat)))
cat(sprintf("mean receptions  covered=%.2f  uncovered=%.2f\n",
            feat[ngs_covered_r5 == 1, mean(receptions, na.rm = TRUE)],
            feat[ngs_covered_r5 == 0, mean(receptions, na.rm = TRUE)]))

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

## three variants, all sharing one coverage-flag column so the only thing that
## changes is whether the NGS VALUES are informative
make_variant <- function(v) {
  ff <- copy(feat)
  ff[, (qbp_cols) := 0]                       # hold qb_pair out of this test
  if (v == "baseline")  { ff[, (ngs_cols) := 0]; ff[, ngs_covered_r5 := 0] }
  if (v == "flag_only") { ff[, (ngs_cols) := 0] }   # flag kept as-is
  ff
}

cat("\nbuilding projections per variant (2024-2025 held out)...\n")
vnames <- c("baseline", "flag_only", "ngs_full")
PV <- lapply(vnames, function(v) pred_ppr(make_variant(v), v))
names(PV) <- vnames

actual <- stats[, .(player_id, season, week, act = fantasy_points_ppr,
                    nk = nkey(player_display_name))]
res <- list()
for (v in vnames) {
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

cat("\n=== is the NGS lift real signal, or just the coverage flag? ===\n")
R <- rbindlist(lapply(res, as.data.table))
print(R[order(-median_pctile)])
b <- res$baseline$median_pctile
cat(sprintf("\nflag alone      : %+.4f vs baseline\n", res$flag_only$median_pctile - b))
cat(sprintf("full NGS        : %+.4f vs baseline\n", res$ngs_full$median_pctile - b))
cat(sprintf("measurements add : %+.4f beyond the flag\n",
            res$ngs_full$median_pctile - res$flag_only$median_pctile))
