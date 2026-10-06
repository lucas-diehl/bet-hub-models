source("R/utilities.R")
source("R/fantasy_prop_model.R")
assert_packages(); ensure_directories()
suppressPackageStartupMessages(library(data.table))
options(width = 200)

## Definitive test for the QB x receiver pair feature, same harness as scripts 93/94.
## Script 95 found pair chemistry persists with dR2 +0.028 (PPR) / +0.035 (target
## share), coefficient ~1.0 -- i.e. a receiver's historical over/under-performance
## with a specific QB carries forward at nearly full strength. That is 7-9x the
## effect size of the efficiency score, which failed here. Crucially this is NOT
## redundant with the existing feature set: the model has NO QB identity input at all.
##
## CAVEAT built into this test: it uses the ACTUAL primary QB of each game. In live
## deployment we need the PROJECTED starter pre-lock. For most games that is known
## with high confidence; late scratches would degrade it. So treat this as an upper
## bound on real-world lift.

features <- as.data.table(readRDS("data/processed/fantasy_prop_features.rds"))
s <- as.data.table(readRDS("data/raw/player_stats_2021_2026.rds"))[season_type == "REG"]

## primary QB per team-game + that receiver's leak-free pair history
qbg <- s[position == "QB" & !is.na(attempts) & attempts > 0,
         .(qb = player_display_name[which.max(attempts)], team_att = sum(attempts, na.rm = TRUE)),
         by = .(season, week, team)]

rec <- s[, .(season, week, team, player_id, ppr = fantasy_points_ppr,
             targets = fifelse(is.na(targets), 0, as.numeric(targets)))]
rec <- merge(rec, qbg, by = c("season", "week", "team"))
setorder(rec, player_id, season, week)
rec[, g_idx := seq_len(.N), by = player_id]
rec[, prior_ppr := (cumsum(ppr) - ppr) / pmax(g_idx - 1, 1), by = player_id]
rec[g_idx == 1, prior_ppr := NA_real_]
rec[, pair_idx := seq_len(.N), by = .(player_id, qb)]
rec[, prior_pair_ppr := (cumsum(ppr) - ppr) / pmax(pair_idx - 1, 1), by = .(player_id, qb)]
rec[pair_idx == 1, prior_pair_ppr := NA_real_]
rec[, prior_pair_n := pair_idx - 1L]

K <- 4  # shrinkage: with 4 prior pair-games the raw delta gets half weight
rec[, raw_delta := prior_pair_ppr - prior_ppr]
rec[, qb_pair_delta_r5 := fifelse(is.na(raw_delta), 0, raw_delta * prior_pair_n / (prior_pair_n + K))]
rec[, qb_pair_games_r5 := as.numeric(prior_pair_n)]

feat <- merge(features, rec[, .(season, week, player_id, qb_pair_delta_r5, qb_pair_games_r5)],
              by = c("season", "week", "player_id"), all.x = TRUE)
feat[is.na(qb_pair_delta_r5), qb_pair_delta_r5 := 0]
feat[is.na(qb_pair_games_r5), qb_pair_games_r5 := 0]
cat("feature frame:", nrow(feat), "rows | nonzero pair delta:",
    sum(feat$qb_pair_delta_r5 != 0), sprintf("(%.1f%%)", 100*mean(feat$qb_pair_delta_r5 != 0)), "\n")

specs <- fantasy_target_specifications()[c("receptions", "receiving_yards", "receiving_tds")]
run_wf <- function(ff, label) {
  out <- list()
  for (tname in names(specs)) {
    spec <- specs[[tname]]
    td <- fantasy_target_candidates(as.data.frame(ff), spec$family)
    for (ts in 2023:2025) {
      tr <- td |> dplyr::filter(.data$season < ts, .data$prior_games >= 1)
      te <- td |> dplyr::filter(.data$season == ts, .data$prior_games >= 1)
      if (!nrow(tr) || !nrow(te)) next
      fitted <- fit_fantasy_stat_model(tr, te, spec$outcome, spec$objective,
                                        20260728L + ts + match(tname, names(specs)))
      out[[length(out) + 1L]] <- data.frame(target = tname, season = ts, position = te$position,
        actual = as.numeric(te[[spec$outcome]]), prediction = fitted$prediction)
    }
  }
  cat("  ", label, "done\n"); dplyr::bind_rows(out)
}

base_frame <- copy(feat)[, c("qb_pair_delta_r5", "qb_pair_games_r5") := NULL]
res_base <- run_wf(base_frame, "baseline")
res_pair <- run_wf(feat, "+qb_pair")

summ <- function(r, label, pf = NULL) {
  d <- if (is.null(pf)) r else r[r$position %in% pf, ]
  as.data.table(d)[, .(variant = label, n = .N,
    mae = mean(abs(prediction - actual)),
    rmse = sqrt(mean((prediction - actual)^2)),
    r2 = 1 - sum((prediction - actual)^2) / sum((actual - mean(actual))^2)), by = target]
}

for (ps in list(list(n = "WR only", p = "WR"), list(n = "TE only", p = "TE"),
                list(n = "ALL receiving positions", p = NULL))) {
  cat("\n\n================", ps$n, "================\n")
  b <- summ(res_base, "baseline", ps$p); f <- summ(res_pair, "+qb_pair", ps$p)
  m <- merge(b, f, by = "target", suffixes = c("_b", "_f"))
  for (i in seq_len(nrow(m))) {
    cat(sprintf("  %-16s baseline MAE=%.5f R2=%.5f | +qb_pair MAE=%.5f R2=%.5f | dMAE=%+.5f dR2=%+.6f\n",
                m$target[i], m$mae_b[i], m$r2_b[i], m$mae_f[i], m$r2_f[i],
                m$mae_f[i] - m$mae_b[i], m$r2_f[i] - m$r2_b[i]))
  }
}
cat("\n\nDone.\n")
