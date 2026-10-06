source("R/utilities.R")
source("R/qb_starter.R")
source("R/fantasy_prop_model.R")
assert_packages(); ensure_directories()
suppressPackageStartupMessages(library(data.table))
options(width = 205)

## Does NFL Next Gen Stats tracking data improve receiving projections beyond the
## current feature set? These are the free, repeatable analogues of what was asked
## for: avg_separation is route-running/separation, avg_yac_above_expectation is
## skill after the catch, avg_cushion is how defenses treat him, intended-air-yards
## share is role depth. (Raw top speed, agility splits and spiral/throw telemetry
## are Zebra chip data the NFL does not publish -- not obtainable at any tier.)
##
## Same walk-forward harness as scripts 93/94/96/99 so results are comparable:
## 2023-2025 held out, baseline = the current feature frame untouched.

features <- as.data.table(readRDS("data/processed/fantasy_prop_features.rds"))
cat("baseline feature frame:", nrow(features), "rows\n")

ngs <- as.data.table(nflreadr::load_nextgen_stats(2021:2026, stat_type = "receiving"))
ngs <- ngs[week > 0 & season_type == "REG"]
setnames(ngs, "player_gsis_id", "player_id")
keep <- c("avg_cushion", "avg_separation", "avg_intended_air_yards",
          "percent_share_of_intended_air_yards", "avg_yac_above_expectation")
ngs <- ngs[, c("player_id", "season", "week", keep), with = FALSE]
cat("NGS receiving weekly rows:", nrow(ngs), "\n")

## Leak-free: roll each metric over the player's PRIOR games only, exactly like the
## existing _r5 features. Names end in _r5 so fantasy_model_feature_names() sweeps them.
lagroll <- function(x, w = 5L) {
  slider::slide_dbl(dplyr::lag(as.numeric(x)), function(v) {
    if (!length(v) || all(is.na(v))) return(NA_real_)
    mean(v, na.rm = TRUE)
  }, .before = w - 1L, .complete = FALSE)
}
setorder(ngs, player_id, season, week)
for (k in keep) ngs[, (paste0("ngs_", k, "_r5")) := lagroll(get(k)), by = player_id]
ngs_feat <- ngs[, c("player_id", "season", "week", paste0("ngs_", keep, "_r5")), with = FALSE]

feat <- merge(features, ngs_feat, by = c("player_id", "season", "week"), all.x = TRUE)
newcols <- paste0("ngs_", keep, "_r5")
cov <- sapply(newcols, function(c) round(100*mean(is.finite(feat[[c]])), 1))
cat("\ncoverage of the merged NGS features (% of feature rows):\n")
print(cov)
cat("\ncoverage among WR rows only:\n")
print(sapply(newcols, function(c) round(100*mean(is.finite(feat[position == "WR"][[c]])), 1)))

specs <- fantasy_target_specifications()[c("receptions", "receiving_yards", "receiving_tds")]
run_wf <- function(ff, label) {
  out <- list()
  for (tname in names(specs)) {
    spec <- specs[[tname]]
    td <- as.data.table(fantasy_target_candidates(as.data.frame(ff), spec$family))
    for (ts in 2023:2025) {
      tr <- td[season < ts & prior_games >= 1]; te <- td[season == ts & prior_games >= 1]
      if (!nrow(tr) || !nrow(te)) next
      fit <- fit_fantasy_stat_model(as.data.frame(tr), as.data.frame(te), spec$outcome,
                                     spec$objective, 20260728L + ts + match(tname, names(specs)))
      out[[length(out)+1L]] <- data.table(target = tname, position = te$position,
        season = te$season, week = te$week, player_id = te$player_id,
        actual = as.numeric(te[[spec$outcome]]), prediction = fit$prediction)
    }
  }
  cat("  ", label, "done\n"); rbindlist(out)
}

RB <- run_wf(features, "baseline")
RP <- run_wf(feat, "+NGS")

## tag rows that actually HAVE ngs coverage -- averaging over rows where the feature
## is absent just dilutes whatever signal exists
tags <- unique(feat[, .(season, week, player_id,
                        has_ngs = as.integer(is.finite(ngs_avg_separation_r5)))],
               by = c("season","week","player_id"))
RB <- merge(RB, tags, by = c("season","week","player_id"), all.x = TRUE)
RP <- merge(RP, tags, by = c("season","week","player_id"), all.x = TRUE)

r2 <- function(d) 1 - sum((d$prediction - d$actual)^2)/sum((d$actual - mean(d$actual))^2)
mae <- function(d) mean(abs(d$prediction - d$actual))
show <- function(b, p, lab) {
  if (nrow(b) < 50) return(invisible())
  cat(sprintf("  %-22s n=%5d  MAE %.4f -> %.4f (%+.2f%%)  dR2 %+.6f\n",
              lab, nrow(b), mae(b), mae(p), 100*(mae(p)-mae(b))/mae(b), r2(p)-r2(b)))
}
for (tname in unique(RB$target)) {
  cat("\n===", toupper(tname), "===\n")
  b <- RB[target == tname]; p <- RP[target == tname]
  setorder(b, season, week, player_id); setorder(p, season, week, player_id)
  show(b, p, "ALL")
  show(b[has_ngs == 1], p[has_ngs == 1], "rows WITH NGS")
  show(b[has_ngs == 1 & position == "WR"], p[has_ngs == 1 & position == "WR"], "WR with NGS")
}
cat("\n\nDone.\n")
