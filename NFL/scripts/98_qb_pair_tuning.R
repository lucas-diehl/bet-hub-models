source("R/utilities.R")
source("R/fantasy_prop_model.R")
assert_packages(); ensure_directories()
suppressPackageStartupMessages(library(data.table))
options(width = 200)

## Script 97 showed the pair feature helps where the QB changed (WR receiving yards
## MAE -0.97%, R2 +0.0139) but HURTS in the tail: at the top 5% of |pair_delta| it
## cost R2 -0.0095. Extreme deltas are small-sample noise the booster over-trusts.
## This tunes the shrinkage/gating to keep the QB-change lift while killing the tail
## damage. Variants:
##   K4       - current: delta * n/(n+4)
##   K12      - heavier shrinkage
##   K4_cap5  - current shrinkage, hard cap at +/-5 PPR
##   minN4_K8 - require >=4 prior games with that QB, else 0, plus K=8
##   gated    - apply the delta ONLY when the QB changed from the player's last game
##              (uses the feature strictly as a "new QB" correction)

features <- as.data.table(readRDS("data/processed/fantasy_prop_features.rds"))
s <- as.data.table(readRDS("data/raw/player_stats_2021_2026.rds"))[season_type == "REG"]

qbg <- s[position == "QB" & !is.na(attempts) & attempts > 0,
         .(qb = player_display_name[which.max(attempts)]), by = .(season, week, team)]
rec <- merge(s[, .(season, week, team, player_id, ppr = fantasy_points_ppr)], qbg,
             by = c("season", "week", "team"))
setorder(rec, player_id, season, week)
rec[, g_idx := seq_len(.N), by = player_id]
rec[, prior_ppr := (cumsum(ppr) - ppr) / pmax(g_idx - 1, 1), by = player_id]
rec[g_idx == 1, prior_ppr := NA_real_]
rec[, pair_idx := seq_len(.N), by = .(player_id, qb)]
rec[, prior_pair_ppr := (cumsum(ppr) - ppr) / pmax(pair_idx - 1, 1), by = .(player_id, qb)]
rec[pair_idx == 1, prior_pair_ppr := NA_real_]
rec[, n_pair := pair_idx - 1L]
rec[, prev_qb := shift(qb), by = player_id]
rec[, qb_changed := !is.na(prev_qb) & qb != prev_qb]
rec[, raw := fifelse(is.na(prior_pair_ppr - prior_ppr), 0, prior_pair_ppr - prior_ppr)]

shrink <- function(raw, n, K) raw * n / (n + K)
variants <- list(
  K4       = function(d) shrink(d$raw, d$n_pair, 4),
  K12      = function(d) shrink(d$raw, d$n_pair, 12),
  K4_cap5  = function(d) pmax(pmin(shrink(d$raw, d$n_pair, 4), 5), -5),
  minN4_K8 = function(d) fifelse(d$n_pair >= 4, shrink(d$raw, d$n_pair, 8), 0),
  gated    = function(d) fifelse(d$qb_changed, shrink(d$raw, d$n_pair, 8), 0)
)

specs <- fantasy_target_specifications()[c("receptions", "receiving_yards")]
run_wf <- function(ff) {
  out <- list()
  for (tname in names(specs)) {
    spec <- specs[[tname]]
    td <- as.data.table(fantasy_target_candidates(as.data.frame(ff), spec$family))
    for (ts in 2023:2025) {
      tr <- td[season < ts & prior_games >= 1]; te <- td[season == ts & prior_games >= 1]
      if (!nrow(tr) || !nrow(te)) next
      fitted <- fit_fantasy_stat_model(as.data.frame(tr), as.data.frame(te), spec$outcome,
                                        spec$objective, 20260728L + ts + match(tname, names(specs)))
      out[[length(out)+1L]] <- data.table(target = tname, position = te$position,
        season = te$season, week = te$week, player_id = te$player_id,
        qb_changed = te$qb_changed_flag,
        actual = as.numeric(te[[spec$outcome]]), prediction = fitted$prediction)
    }
  }
  rbindlist(out)
}

mk_frame <- function(delta_fn) {
  rr <- copy(rec)
  rr[, qb_pair_delta_r5 := delta_fn(rr)]
  rr[, qb_pair_games_r5 := as.numeric(n_pair)]
  rr[, qb_changed_flag := as.integer(qb_changed)]
  f <- merge(features, rr[, .(season, week, player_id, qb_pair_delta_r5, qb_pair_games_r5, qb_changed_flag)],
             by = c("season", "week", "player_id"), all.x = TRUE)
  f[is.na(qb_pair_delta_r5), qb_pair_delta_r5 := 0]
  f[is.na(qb_pair_games_r5), qb_pair_games_r5 := 0]
  f[is.na(qb_changed_flag), qb_changed_flag := 0L]
  f
}

base_frame <- mk_frame(function(d) rep(0, nrow(d)))[, c("qb_pair_delta_r5","qb_pair_games_r5") := NULL]
cat("running baseline...\n"); RB <- run_wf(base_frame)

r2 <- function(d) 1 - sum((d$prediction - d$actual)^2) / sum((d$actual - mean(d$actual))^2)
mae <- function(d) mean(abs(d$prediction - d$actual))

for (vn in names(variants)) {
  cat("\nrunning", vn, "...\n")
  fr <- mk_frame(variants[[vn]])
  tags <- unique(fr[, .(season, week, player_id, pair_abs = abs(qb_pair_delta_r5))],
                 by = c("season","week","player_id"))
  RP <- run_wf(fr)
  RPt <- merge(RP, tags, by = c("season","week","player_id"), all.x = TRUE)
  RBt <- merge(RB, tags, by = c("season","week","player_id"), all.x = TRUE)
  for (tname in unique(RB$target)) {
    b <- RBt[target == tname]; p <- RPt[target == tname]
    setorder(b, season, week, player_id); setorder(p, season, week, player_id)
    tail_cut <- quantile(p$pair_abs, 0.95, na.rm = TRUE)
    cat(sprintf("  [%-9s] %-16s ALL dR2=%+.5f | QBchg dR2=%+.5f (dMAE %+.2f%%) | TAIL5%% dR2=%+.5f\n",
      vn, tname,
      r2(p) - r2(b),
      r2(p[qb_changed == 1]) - r2(b[qb_changed == 1]),
      100*(mae(p[qb_changed == 1]) - mae(b[qb_changed == 1]))/mae(b[qb_changed == 1]),
      r2(p[pair_abs > tail_cut]) - r2(b[pair_abs > tail_cut])))
  }
}
cat("\n\nDone.\n")
