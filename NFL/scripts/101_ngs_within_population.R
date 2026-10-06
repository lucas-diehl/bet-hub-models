source("R/utilities.R")
source("R/qb_starter.R")
source("R/fantasy_prop_model.R")
assert_packages(); ensure_directories()
suppressPackageStartupMessages(library(data.table))
options(width = 205)

## Script 100 showed a huge apparent gain from NGS (receptions dR2 +0.15). The leak
## audit found no TIME leakage -- the lag is clean -- but did find the cause:
## NGS only publishes qualifying receivers, so "has an NGS row" is a proxy for
## "is a real receiver" (4.98 vs 1.12 mean receptions). Median-imputing the 79%
## of rows without coverage turns the tracking columns into that indicator, and
## the model learns the population split rather than anything about route running.
##
## Clean test: restrict BOTH training and evaluation to NGS-covered rows only.
## The indicator is then constant and can do no work, so whatever remains is
## genuine tracking signal (separation, YAC over expected, cushion, air-yards share).

features <- as.data.table(readRDS("data/processed/fantasy_prop_features.rds"))
ngs <- as.data.table(nflreadr::load_nextgen_stats(2021:2026, stat_type = "receiving"))
ngs <- ngs[week > 0 & season_type == "REG"]
setnames(ngs, "player_gsis_id", "player_id")
setorder(ngs, player_id, season, week)
lagroll <- function(x, w = 5L) {
  slider::slide_dbl(dplyr::lag(as.numeric(x)), function(v) {
    if (!length(v) || all(is.na(v))) return(NA_real_); mean(v, na.rm = TRUE)
  }, .before = w - 1L, .complete = FALSE)
}
keep <- c("avg_cushion","avg_separation","avg_intended_air_yards",
          "percent_share_of_intended_air_yards","avg_yac_above_expectation")
for (k in keep) ngs[, (paste0("ngs_", k, "_r5")) := lagroll(get(k)), by = player_id]
ngs_feat <- ngs[, c("player_id","season","week", paste0("ngs_", keep, "_r5")), with = FALSE]

feat <- merge(features, ngs_feat, by = c("player_id","season","week"), all.x = TRUE)
covered <- feat[is.finite(ngs_avg_separation_r5)]
cat("NGS-covered rows (train+test population):", nrow(covered), "\n")
cat("mean receptions in this population:", round(mean(covered$receptions, na.rm=TRUE), 2), "\n")

base_cov <- copy(covered)[, paste0("ngs_", keep, "_r5") := NULL]

specs <- fantasy_target_specifications()[c("receptions","receiving_yards","receiving_tds")]
run_wf <- function(ff, label) {
  out <- list()
  for (tname in names(specs)) {
    spec <- specs[[tname]]
    td <- as.data.table(fantasy_target_candidates(as.data.frame(ff), spec$family))
    for (ts in 2023:2025) {
      tr <- td[season < ts & prior_games >= 1]; te <- td[season == ts & prior_games >= 1]
      if (nrow(tr) < 100 || !nrow(te)) next
      fit <- fit_fantasy_stat_model(as.data.frame(tr), as.data.frame(te), spec$outcome,
                                     spec$objective, 20260728L + ts + match(tname, names(specs)))
      out[[length(out)+1L]] <- data.table(target = tname, position = te$position,
        season = te$season, week = te$week, player_id = te$player_id,
        actual = as.numeric(te[[spec$outcome]]), prediction = fit$prediction)
    }
  }
  cat("  ", label, "done\n"); rbindlist(out)
}

RB <- run_wf(base_cov, "baseline (NGS population, no NGS cols)")
RP <- run_wf(covered,  "+NGS tracking features")

r2 <- function(d) 1 - sum((d$prediction-d$actual)^2)/sum((d$actual-mean(d$actual))^2)
mae <- function(d) mean(abs(d$prediction-d$actual))
cat("\n=== WITHIN the NGS population -- indicator effect removed ===\n")
for (tname in unique(RB$target)) {
  b <- RB[target==tname]; p <- RP[target==tname]
  setorder(b, season, week, player_id); setorder(p, season, week, player_id)
  cat(sprintf("  %-16s n=%5d  MAE %.4f -> %.4f (%+.2f%%)  dR2 %+.6f\n",
              tname, nrow(b), mae(b), mae(p), 100*(mae(p)-mae(b))/mae(b), r2(p)-r2(b)))
  bw <- b[position=="WR"]; pw <- p[position=="WR"]
  if (nrow(bw) > 100)
    cat(sprintf("  %-16s WR only n=%5d  MAE %.4f -> %.4f (%+.2f%%)  dR2 %+.6f\n",
                "", nrow(bw), mae(bw), mae(pw), 100*(mae(pw)-mae(bw))/mae(bw), r2(pw)-r2(bw)))
}
cat("\n\nDone.\n")
