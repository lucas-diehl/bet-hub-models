source("R/utilities.R")
source("R/fantasy_prop_model.R")
assert_packages(); ensure_directories()
suppressPackageStartupMessages(library(data.table))
options(width = 200)

## Final gate before shipping: validate the PRODUCTION implementation of the gated
## QB-pair feature (now living inside prepare_fantasy_player_features) against a
## baseline with the feature zeroed out. Full walk-forward, all three receiving
## targets, 2023-2025 held out.

player_stats <- readRDS("data/raw/player_stats_2021_2026.rds")
historical_context <- readRDS("data/processed/td_game_context.rds")
cat("building production features...\n")
feat <- as.data.table(prepare_fantasy_player_features(player_stats, historical_context))
cat("nonzero qb_pair_delta_r5:", sum(feat$qb_pair_delta_r5 != 0),
    sprintf("(%.2f%%)\n", 100*mean(feat$qb_pair_delta_r5 != 0)))
cat("QBs with nonzero delta (should be 0):", sum(feat$position == "QB" & feat$qb_pair_delta_r5 != 0), "\n")

base <- copy(feat)[, c("qb_pair_delta_r5", "qb_pair_games_r5") := NULL]

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

RB <- run_wf(base, "baseline"); RP <- run_wf(feat, "+qb_pair(gated)")
tags <- unique(feat[, .(season, week, player_id, fired = as.integer(qb_pair_delta_r5 != 0))],
               by = c("season","week","player_id"))
RB <- merge(RB, tags, by = c("season","week","player_id"), all.x = TRUE)
RP <- merge(RP, tags, by = c("season","week","player_id"), all.x = TRUE)

r2 <- function(d) 1 - sum((d$prediction - d$actual)^2) / sum((d$actual - mean(d$actual))^2)
mae <- function(d) mean(abs(d$prediction - d$actual))

for (tname in unique(RB$target)) {
  b <- RB[target == tname]; p <- RP[target == tname]
  cat(sprintf("\n%-16s ALL      n=%5d  MAE %.4f -> %.4f (%+.2f%%)  dR2 %+.6f\n",
              tname, nrow(b), mae(b), mae(p), 100*(mae(p)-mae(b))/mae(b), r2(p)-r2(b)))
  bf <- b[fired == 1]; pf <- p[fired == 1]
  if (nrow(bf) > 40)
    cat(sprintf("%-16s FIRED    n=%5d  MAE %.4f -> %.4f (%+.2f%%)  dR2 %+.6f\n",
                "", nrow(bf), mae(bf), mae(pf), 100*(mae(pf)-mae(bf))/mae(bf), r2(pf)-r2(bf)))
  bw <- b[fired == 1 & position == "WR"]; pw <- p[fired == 1 & position == "WR"]
  if (nrow(bw) > 40)
    cat(sprintf("%-16s FIRED/WR n=%5d  MAE %.4f -> %.4f (%+.2f%%)  dR2 %+.6f\n",
                "", nrow(bw), mae(bw), mae(pw), 100*(mae(pw)-mae(bw))/mae(bw), r2(pw)-r2(bw)))
}
cat("\n\nDone.\n")
