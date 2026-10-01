source("R/utilities.R")
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
options(width = 215)

## ACCEPTANCE TEST for the receiving-model collapse (2026-10-01).
##
## The NGS features took 43-44% of gain in the receiving models after the 09-28
## retrain. With ~20% coverage the dominant inputs were missing for 4 of 5 players,
## predictions collapsed toward a constant, and -- because a mean-collapsed model has
## BETTER MAE -- the least-squares blend weight promoted it 0.726 -> 1.000 and threw
## away a baseline that was still correctly spread.
##
## So this script deliberately does NOT look at MAE or R^2. It checks the two things
## that actually broke, and it compares against week 3, which was healthy.
##
##   1. SPREAD  -- does the model still discriminate? Model sd must be a reasonable
##                 fraction of the baseline's; near-zero means collapse.
##   2. LEVEL   -- do the players who actually score get projected like it? Top-12 WR
##                 by realised PPR should project near their realised mean.
##
## Thresholds are deliberately loose: this is a circuit-breaker test, not a tuning
## objective. Optimising against these numbers would repeat the original mistake.

SPREAD_FLOOR <- 0.60   # model sd / baseline sd, per FANTASY_COLLAPSE_SPREAD_FLOOR
LEVEL_FLOOR  <- 0.55   # top-12 WR model / actual. Week 3 (healthy) was 0.59.

st <- as.data.table(readRDS("data/raw/player_stats_2021_2026.rds"))[
  season_type == "REG" & season == 2026]

long_path <- "outputs/fantasy_prop_2026_latest_long.csv"
json_path <- "outputs/dfs_projections_latest.json"
stopifnot(file.exists(long_path), file.exists(json_path))
L <- fread(long_path)
J <- as.data.table(fromJSON(json_path)$players)

cat("=== 1. SPREAD: does each receiving model still discriminate? (WRs) ===\n")
sp <- L[position == "WR", .(
  model_sd  = sd(model_prediction, na.rm = TRUE),
  base_sd   = sd(baseline_prediction, na.rm = TRUE),
  final_sd  = sd(prediction, na.rm = TRUE),
  model_max = max(model_prediction, na.rm = TRUE),
  weight    = mean(model_weight, na.rm = TRUE)), by = target]
sp[, spread_ratio := round(model_sd / pmax(base_sd, 1e-9), 3)]
## Judge ONLY the receiving VOLUME targets here, and only on WR rows.
##  - rushing_yards / rushing_tds on WR rows are legitimately near-constant, because
##    WRs barely carry the ball. Flagging those as "collapsed" says nothing about the
##    model; my first version did exactly that and reported a spurious FAIL.
##  - the count targets (receiving_tds etc.) have a narrow spread by construction for
##    a ~Poisson rate, which is why FANTASY_COLLAPSE_CAP_TARGETS excludes them.
JUDGE <- c("receptions", "receiving_yards")
sp[, judged := target %in% JUDGE]
sp[, verdict := ifelse(!judged, "n/a (not a WR volume target)",
                ifelse(spread_ratio >= SPREAD_FLOOR, "ok", "COLLAPSED"))]
print(sp[order(-judged, target), .(target, model_sd = round(model_sd, 3),
                                   base_sd = round(base_sd, 3), spread_ratio,
                                   model_max = round(model_max, 2),
                                   weight = round(weight, 3), verdict)])
spread_pass <- all(sp[judged == TRUE]$spread_ratio >= SPREAD_FLOOR)

cat("\n=== 2. LEVEL: population calibration (UNBIASED) ===\n")
## NOT top-12-by-actual. Selecting players BY OUTCOME and then comparing to their
## projections guarantees the projections look low, because the top scorers are by
## construction the ones who beat expectation -- a perfectly calibrated model fails
## that test. My first version of this script made exactly that error and produced a
## bogus "41% of reality". Compare over the SAME population instead, and separately
## report what OUR top-12 picks (chosen by projection) actually went on to score.
pos_check <- function(ps) {
  a <- st[position == ps, .(actual = mean(fantasy_points_ppr, na.rm = TRUE)),
          by = .(name = player_display_name)]
  m <- merge(J[pos == ps, .(name, proj_ppr)], a, by = "name")
  if (nrow(m) < 20) return(NULL)
  t12 <- head(m[order(-proj_ppr)], 12)
  data.table(pos = ps, n = nrow(m),
             mean_proj = round(mean(m$proj_ppr), 2),
             mean_actual = round(mean(m$actual), 2),
             pop_ratio = round(mean(m$proj_ppr) / mean(m$actual), 3),
             top12_by_proj = round(mean(t12$proj_ppr), 2),
             they_scored = round(mean(t12$actual), 2),
             spearman = round(suppressWarnings(
               stats::cor(m$proj_ppr, m$actual, method = "spearman")), 3))
}
lv <- rbindlist(lapply(c("WR", "TE", "RB", "QB"), pos_check), fill = TRUE)
lv[, verdict := ifelse(pop_ratio >= LEVEL_FLOOR, "ok", "TOO LOW")]
print(lv)
cat("  (actuals here are each player's 2026 season mean, so this is a LEVEL check,\n")
cat("   not a week-specific forecast test -- week 4 has not been played yet.)\n")
level_pass <- isTRUE(lv[pos == "WR", pop_ratio] >= LEVEL_FLOOR) &&
              isTRUE(lv[pos == "TE", pop_ratio] >= LEVEL_FLOOR)

cat("\n=== 3. the four from tonight's PIT@CLE showdown ===\n")
who <- c("DK Metcalf", "Roman Wilson", "Harold Fannin Jr.", "Denzel Boston")
print(J[name %in% who, .(name, pos, team, proj_ppr = round(proj_ppr, 2),
                         rec = round(components.receptions, 2),
                         ryd = round(components.rec_yds, 1))][order(-proj_ppr)])
cat("\n  for scale, the kickers/DST they were projected BELOW are ~10-11 DK pts\n")

cat("\n=== 4. no NGS columns leaked back into the models ===\n")
fi_path <- "outputs/fantasy_prop_feature_importance.csv"
if (file.exists(fi_path)) {
  fi <- fread(fi_path)
  n_ngs <- sum(grepl("^ngs_", fi$Feature))
  cat("  ngs_ features present in the trained models:", n_ngs, "(must be 0)\n")
  if (n_ngs == 0) {
    cat("  top 6 features for receptions now:\n")
    print(head(fi[target == "receptions"][order(-Gain), .(Feature, Gain = round(Gain, 4))], 6))
  }
  ngs_pass <- n_ngs == 0
} else { cat("  (no importance file)\n"); ngs_pass <- NA }

cat("\n======================= VERDICT =======================\n")
cat(sprintf("  spread (models discriminate) : %s\n", if (spread_pass) "PASS" else "FAIL"))
cat(sprintf("  level  (WR/TE not depressed) : %s\n", if (level_pass) "PASS" else "FAIL"))
cat(sprintf("  no NGS in models             : %s\n", if (isTRUE(ngs_pass)) "PASS" else "FAIL"))
cat(sprintf("\n  week-3 reference (healthy): WR receptions model sd 1.350, max 5.86,\n",
            ""))
cat("                             blend weight 0.726, top-12 WR level 0.59\n")
cat(sprintf("  week-4 before this fix    : sd 0.638, max 2.77, weight 1.000, level 0.41\n"))
if (!(spread_pass && level_pass && isTRUE(ngs_pass)))
  cat("\n  DO NOT SHIP. At least one circuit-breaker check failed.\n")
