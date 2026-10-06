source("R/utilities.R")
source("R/fantasy_prop_model.R")
assert_packages(); ensure_directories()
suppressPackageStartupMessages(library(data.table))
options(width = 200)

## The aggregate walk-forward (script 96) showed consistent but small gains
## (dR2 ~ +0.002). That average is diluted: most receiver-games have a stable QB
## and a near-zero pair delta, so the feature does nothing there by construction.
## The DFS value is concentrated in the minority of spots where the QB situation
## is unusual -- exactly the London/Penix case. This measures the lift WHERE IT
## SHOULD MATTER instead of averaging it away.

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
rec[, prior_pair_n := pair_idx - 1L]
rec[, prev_qb := shift(qb), by = player_id]
rec[, qb_changed := !is.na(prev_qb) & qb != prev_qb]
K <- 4
rec[, raw_delta := prior_pair_ppr - prior_ppr]
rec[, qb_pair_delta_r5 := fifelse(is.na(raw_delta), 0, raw_delta * prior_pair_n / (prior_pair_n + K))]
rec[, qb_pair_games_r5 := as.numeric(prior_pair_n)]

feat <- merge(features, rec[, .(season, week, player_id, qb_pair_delta_r5, qb_pair_games_r5,
                                 qb_changed, qb)],
              by = c("season", "week", "player_id"), all.x = TRUE)
feat[is.na(qb_pair_delta_r5), qb_pair_delta_r5 := 0]
feat[is.na(qb_pair_games_r5), qb_pair_games_r5 := 0]
feat[is.na(qb_changed), qb_changed := FALSE]

specs <- fantasy_target_specifications()[c("receptions", "receiving_yards")]
run_wf <- function(ff, label) {
  out <- list()
  for (tname in names(specs)) {
    spec <- specs[[tname]]
    td <- as.data.table(fantasy_target_candidates(as.data.frame(ff), spec$family))
    for (ts in 2023:2025) {
      tr <- td[season < ts & prior_games >= 1]; te <- td[season == ts & prior_games >= 1]
      if (!nrow(tr) || !nrow(te)) next
      fitted <- fit_fantasy_stat_model(as.data.frame(tr), as.data.frame(te), spec$outcome,
                                        spec$objective, 20260728L + ts + match(tname, names(specs)))
      out[[length(out)+1L]] <- data.table(target = tname, season = ts, position = te$position,
        qb_changed = te$qb_changed, pair_delta = te$qb_pair_delta_r5,
        actual = as.numeric(te[[spec$outcome]]), prediction = fitted$prediction)
    }
  }
  cat("  ", label, "done\n"); rbindlist(out)
}

base_frame <- copy(feat)[, c("qb_pair_delta_r5","qb_pair_games_r5") := NULL]
rb <- run_wf(base_frame, "baseline"); rp <- run_wf(feat, "+qb_pair")
rb[, variant := "base"]; rp[, variant := "pair"]

cmp <- function(sub_b, sub_p, label) {
  if (nrow(sub_b) < 50) { cat(sprintf("  %-34s n too small (%d)\n", label, nrow(sub_b))); return(invisible()) }
  mae_b <- mean(abs(sub_b$prediction - sub_b$actual)); mae_p <- mean(abs(sub_p$prediction - sub_p$actual))
  r2 <- function(d) 1 - sum((d$prediction - d$actual)^2) / sum((d$actual - mean(d$actual))^2)
  cat(sprintf("  %-34s n=%5d  MAE %.4f -> %.4f (%+.2f%%)   R2 %+.5f\n",
              label, nrow(sub_b), mae_b, mae_p, 100*(mae_p-mae_b)/mae_b, r2(sub_p) - r2(sub_b)))
}

for (tname in unique(rb$target)) {
  cat("\n\n================", toupper(tname), "================\n")
  b <- rb[target == tname]; p <- rp[target == tname]
  # align rows (same order out of the same harness)
  cat(" -- by QB-change status --\n")
  cmp(b[qb_changed == FALSE], p[qb_changed == FALSE], "QB unchanged")
  cmp(b[qb_changed == TRUE],  p[qb_changed == TRUE],  "QB CHANGED since last game")
  cat(" -- by strength of the pair signal --\n")
  q <- quantile(abs(p$pair_delta), c(0.5, 0.8, 0.95), na.rm = TRUE)
  cmp(b[abs(p$pair_delta) <= q[1]], p[abs(pair_delta) <= q[1]], "|pair_delta| bottom 50%")
  cmp(b[abs(p$pair_delta) > q[2]],  p[abs(pair_delta) > q[2]],  "|pair_delta| top 20%")
  cmp(b[abs(p$pair_delta) > q[3]],  p[abs(pair_delta) > q[3]],  "|pair_delta| top 5%")
  cat(" -- WR only, QB changed --\n")
  cmp(b[qb_changed == TRUE & position == "WR"], p[qb_changed == TRUE & position == "WR"], "WR, QB CHANGED")
}
cat("\n\nDone.\n")
