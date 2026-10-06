source("R/utilities.R")
assert_packages(); ensure_directories()
suppressPackageStartupMessages(library(data.table))
options(width = 210)

## Does WHO is throwing change a receiver's production, beyond team pass volume?
## Motivating case: Drake London averages 20.5 PPR with Penix vs 13.9 with Cousins
## on nearly identical team pass volume (31.3 vs 31.9 att/gm). The model has no QB
## identity feature at all, so when ATL switched Rush -> Penix in week 3 it projected
## London off his Rush-era sample (4.5 tgt/gm) and missed by 17 points.
##
## Two distinct effects to separate:
##   (a) QB QUALITY  -- a good QB lifts ALL his receivers (may already be captured by
##       implied_team_total / team-level features)
##   (b) QB x RECEIVER CHEMISTRY -- this QB favors THIS receiver more than his others
##       (pair-specific; definitely NOT captured today)
## Only (b) justifies a new pair-level feature; (a) would be redundant.

s <- as.data.table(readRDS("data/raw/player_stats_2021_2026.rds"))[season_type == "REG"]

## primary QB per team-game
qbg <- s[position == "QB" & !is.na(attempts) & attempts > 0,
         .(qb = player_display_name[which.max(attempts)],
           team_att = sum(attempts, na.rm = TRUE)),
         by = .(season, week, team)]

rec <- s[position %in% c("WR", "TE", "RB") & !is.na(targets),
         .(season, week, team, player_id, player = player_display_name, position,
           targets, receptions, receiving_yards, receiving_tds,
           ppr = fantasy_points_ppr)]
rec <- merge(rec, qbg, by = c("season", "week", "team"))
rec <- rec[team_att >= 10]                       # drop games with no real passing game
rec[, tgt_share := targets / team_att]

cat("receiver-games:", nrow(rec), " | distinct QBs:", uniqueN(rec$qb), "\n")

## ---------------------------------------------------------------------------
## PART A: the natural experiment. Receivers who played for >=2 primary QBs
## WITHIN the same team+season (injury/benching), >=3 games with each. Same
## roster, same scheme, same year -- the QB is close to the only thing changing.
## ---------------------------------------------------------------------------
cell <- rec[, .(g = .N, tgt = mean(targets), tshare = mean(tgt_share), ppr = mean(ppr)),
            by = .(season, team, player_id, player, position, qb)][g >= 3]
multi <- cell[, if (uniqueN(qb) >= 2) .SD, by = .(season, team, player_id, player, position)]
cat("\nreceiver-seasons with >=2 QBs (>=3 games each):", uniqueN(multi[, paste(season, team, player_id)]), "\n")

## within each receiver-season, deviation of each QB-cell from that receiver's own mean
multi[, `:=`(ppr_dev = ppr - mean(ppr), tshare_dev = tshare - mean(tshare)),
      by = .(season, team, player_id)]
cat("\n=== spread in a receiver's own production across his QBs (same season) ===\n")
spread <- multi[, .(qbs = uniqueN(qb),
                    ppr_range = round(max(ppr) - min(ppr), 2),
                    tshare_range = round(max(tshare) - min(tshare), 4)),
                by = .(season, team, player, position)]
cat("  median PPR/gm swing between a receiver's QBs:", round(median(spread$ppr_range), 2), "\n")
cat("  mean   PPR/gm swing:", round(mean(spread$ppr_range), 2), "\n")
cat("  median target-share swing:", round(median(spread$tshare_range), 4), "\n")
cat("\n  biggest QB-driven swings:\n")
print(head(spread[order(-ppr_range)], 15))

## ---------------------------------------------------------------------------
## PART B: is it QB QUALITY or PAIR CHEMISTRY?
## Fit, on receiver-games: ppr ~ receiver fixed effect + QB fixed effect, then ask
## whether the residual still varies systematically by PAIR. If pair chemistry is
## real, a receiver's PRIOR deviation with a QB should predict his FUTURE deviation
## with that same QB -- out of sample. That is the only test that matters for a feature.
## ---------------------------------------------------------------------------
cat("\n\n=== PART B: does pair chemistry PERSIST (the only thing that makes it a usable feature)? ===\n")
setorder(rec, player_id, season, week)

## leak-free: for each receiver-game, the receiver's prior mean, and his prior mean
## with THIS specific QB, using only earlier games.
rec[, g_idx := seq_len(.N), by = player_id]
rec[, prior_ppr := (cumsum(ppr) - ppr) / pmax(g_idx - 1, 1), by = player_id]
rec[g_idx == 1, prior_ppr := NA_real_]
rec[, pair_idx := seq_len(.N), by = .(player_id, qb)]
rec[, prior_pair_ppr := (cumsum(ppr) - ppr) / pmax(pair_idx - 1, 1), by = .(player_id, qb)]
rec[pair_idx == 1, prior_pair_ppr := NA_real_]
rec[, prior_pair_n := pair_idx - 1L]

## the candidate feature: how much better/worse this receiver has been with THIS QB
## than with his QBs generally, shrunk by sample size (small n -> pull toward 0).
K <- 4  # shrinkage constant: with 4 prior pair-games the estimate gets half weight
rec[, pair_delta_raw := prior_pair_ppr - prior_ppr]
rec[, pair_delta := fifelse(is.na(pair_delta_raw), 0,
                             pair_delta_raw * prior_pair_n / (prior_pair_n + K))]

test <- rec[!is.na(prior_ppr) & prior_pair_n >= 2 & season >= 2023]
cat("  test rows (>=2 prior games with that QB, 2023+):", nrow(test), "\n")
cat("  corr(pair_delta, actual ppr):", round(cor(test$pair_delta, test$ppr), 4), "\n")

base <- lm(ppr ~ prior_ppr, data = test)
full <- lm(ppr ~ prior_ppr + pair_delta, data = test)
a <- anova(base, full)
cat(sprintf("  base R2=%.4f  +pair_delta R2=%.4f  dR2=%+.5f  p=%s\n",
            summary(base)$r.squared, summary(full)$r.squared,
            summary(full)$r.squared - summary(base)$r.squared, signif(a$`Pr(>F)`[2], 4)))
print(round(summary(full)$coefficients, 4))

## same test for target share (role, less TD-variance noise)
setorder(rec, player_id, season, week)
rec[, prior_tshare := (cumsum(tgt_share) - tgt_share) / pmax(g_idx - 1, 1), by = player_id]
rec[g_idx == 1, prior_tshare := NA_real_]
rec[, prior_pair_tshare := (cumsum(tgt_share) - tgt_share) / pmax(pair_idx - 1, 1), by = .(player_id, qb)]
rec[pair_idx == 1, prior_pair_tshare := NA_real_]
rec[, pair_tshare_delta_raw := prior_pair_tshare - prior_tshare]
rec[, pair_tshare_delta := fifelse(is.na(pair_tshare_delta_raw), 0,
                                    pair_tshare_delta_raw * prior_pair_n / (prior_pair_n + K))]
t2 <- rec[!is.na(prior_tshare) & prior_pair_n >= 2 & season >= 2023 & is.finite(tgt_share)]
cat("\n  --- target share version (role signal, less TD noise) ---\n")
b2 <- lm(tgt_share ~ prior_tshare, data = t2)
f2 <- lm(tgt_share ~ prior_tshare + pair_tshare_delta, data = t2)
a2 <- anova(b2, f2)
cat(sprintf("  base R2=%.4f  +pair_delta R2=%.4f  dR2=%+.5f  p=%s\n",
            summary(b2)$r.squared, summary(f2)$r.squared,
            summary(f2)$r.squared - summary(b2)$r.squared, signif(a2$`Pr(>F)`[2], 4)))
print(round(summary(f2)$coefficients, 5))

## ---------------------------------------------------------------------------
## PART C: the operationally urgent case -- a QB CHANGE since the receiver's
## recent games. That is what burned us on London. How often does it happen and
## how big is the miss when it does?
## ---------------------------------------------------------------------------
cat("\n\n=== PART C: QB CHANGES -- frequency and impact ===\n")
rec[, prev_qb := shift(qb), by = .(player_id)]
rec[, qb_changed := !is.na(prev_qb) & qb != prev_qb]
chg <- rec[season >= 2023 & !is.na(prior_ppr)]
cat("  receiver-games where the QB changed from the player's previous game:",
    sum(chg$qb_changed), "of", nrow(chg), sprintf("(%.1f%%)", 100*mean(chg$qb_changed)), "\n")
cat("  MAE of 'prior_ppr' as a predictor:\n")
cat("    when QB unchanged:", round(mean(abs(chg[qb_changed == FALSE]$ppr - chg[qb_changed == FALSE]$prior_ppr)), 3), "\n")
cat("    when QB CHANGED  :", round(mean(abs(chg[qb_changed == TRUE]$ppr - chg[qb_changed == TRUE]$prior_ppr)), 3), "\n")

saveRDS(rec, "data/processed/qb_receiver_pairs.rds")
cat("\nSaved data/processed/qb_receiver_pairs.rds\n")
