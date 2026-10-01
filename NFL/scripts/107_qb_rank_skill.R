source("R/utilities.R")
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
options(width = 215)

## IS QB RANK SKILL ACTUALLY BROKEN, OR WAS THAT A SMALL-SAMPLE ARTIFACT?
##
## The week-3 scorecard put QB Spearman at -0.031 against WR 0.566 / TE 0.578 / RB
## 0.683, which looks alarming. But it came from ONE week with n = 26 QBs, where the
## standard error on a rank correlation is roughly 1/sqrt(n-1) ~ 0.20. A point estimate
## of -0.03 +/- 0.20 is consistent with anything from -0.4 to +0.4, so on its own it
## establishes nothing. Before spending real effort on a "fix", measure it on enough
## data to tell signal from noise.
##
## Three sources, smallest to largest:
##   1. 2026 weeks 1-3, week by week (what I actually observed -- n ~26 each)
##   2. 2026 pooled, within-week ranks (removes between-week scoring level)
##   3. the 2023-2025 walk-forward predictions (hundreds of QB-weeks -- the real test)
##
## Pooling raw points across weeks would inflate the correlation by picking up
## week-to-week league scoring swings, so pooled figures rank WITHIN each week first.

boot_spearman <- function(x, y, B = 2000L, seed = 7L) {
  set.seed(seed)
  n <- length(x)
  if (n < 6) return(c(NA, NA))
  s <- replicate(B, {
    i <- sample.int(n, n, replace = TRUE)
    suppressWarnings(stats::cor(x[i], y[i], method = "spearman"))
  })
  stats::quantile(s, c(0.025, 0.975), na.rm = TRUE)
}

st <- as.data.table(readRDS("data/raw/player_stats_2021_2026.rds"))[season_type == "REG"]

cat("=========== 1. 2026 week by week (the small samples) ===========\n")
rows <- list()
for (wk in 1:3) {
  f <- sprintf("outputs/fantasy_prop_2026_week%d_projections.json", wk)
  if (!file.exists(f)) { cat(sprintf("week %d: no projection file\n", wk)); next }
  J <- as.data.table(fromJSON(f)$players)
  act <- st[season == 2026 & week == wk, .(name = player_display_name, position,
                                           actual = fantasy_points_ppr)]
  for (ps in c("QB", "WR", "RB", "TE")) {
    m <- merge(J[pos == ps, .(name, proj = proj_ppr)], act[position == ps], by = "name")
    if (nrow(m) < 6) next
    rho <- suppressWarnings(stats::cor(m$proj, m$actual, method = "spearman"))
    ci <- boot_spearman(m$proj, m$actual)
    rows[[length(rows) + 1L]] <- data.table(week = wk, pos = ps, n = nrow(m),
                                            spearman = round(rho, 3),
                                            lo95 = round(ci[1], 3), hi95 = round(ci[2], 3))
  }
}
R1 <- rbindlist(rows)
print(R1[order(pos, week)])

cat("\n=========== 2. 2026 pooled, ranked WITHIN week ===========\n")
pool <- list()
for (wk in 1:3) {
  f <- sprintf("outputs/fantasy_prop_2026_week%d_projections.json", wk)
  if (!file.exists(f)) next
  J <- as.data.table(fromJSON(f)$players)
  act <- st[season == 2026 & week == wk, .(name = player_display_name, position,
                                           actual = fantasy_points_ppr)]
  m <- merge(J[, .(name, pos, proj = proj_ppr)], act, by = "name")
  m[, week := wk]
  pool[[length(pool) + 1L]] <- m
}
P <- rbindlist(pool, fill = TRUE)
if (nrow(P)) {
  P <- P[!is.na(proj) & !is.na(actual)]
  P[, `:=`(pr = frank(-proj, ties.method = "average"),
           ar = frank(-actual, ties.method = "average")), by = .(week, pos)]
  out2 <- P[, {
    rho <- suppressWarnings(stats::cor(pr, ar, method = "spearman"))
    ci <- boot_spearman(pr, ar)
    .(n = .N, weeks = uniqueN(week), spearman = round(rho, 3),
      lo95 = round(ci[1], 3), hi95 = round(ci[2], 3))
  }, by = pos]
  print(out2[order(-spearman)])
}

cat("\n=========== 3. 2023-2025 walk-forward (the real sample size) ===========\n")
wfp <- "data/processed/fantasy_prop_walk_forward.rds"
if (!file.exists(wfp)) {
  cat("  no walk-forward file at", wfp, "-- skipping\n")
} else {
  W <- as.data.table(readRDS(wfp))
  cat("  rows:", nrow(W), " cols:", paste(head(names(W), 14), collapse = ", "), "\n")
  pc <- intersect(c("prediction", "pred"), names(W))[1]
  ac <- intersect(c("actual", "outcome"), names(W))[1]
  tc <- intersect(c("target"), names(W))[1]
  poc <- intersect(c("position", "pos"), names(W))[1]
  if (any(is.na(c(pc, ac, tc)))) {
    cat("  unexpected schema; cannot score\n")
  } else {
    ## score the PPR-relevant passing targets for QBs, and the volume targets elsewhere
    show <- W[, {
      rho <- suppressWarnings(stats::cor(get(pc), get(ac), method = "spearman"))
      .(n = .N, spearman = round(rho, 3))
    }, by = c(tc, if (!is.na(poc)) poc else NULL)]
    if (!is.na(poc)) {
      cat("\n  by target x position (QB rows are the ones in question):\n")
      print(show[get(poc) %in% c("QB", "WR", "RB", "TE")][order(get(tc), get(poc))])
    } else {
      cat("\n  by target:\n"); print(show[order(get(tc))])
    }
  }
}

cat("\n=========== VERDICT GUIDE ===========\n")
cat("If QB spearman is near 0 with a 95% CI that EXCLUDES 0.3+ across weeks AND in the\n")
cat("walk-forward, QB ordering is genuinely broken and worth real work. If the CI is\n")
cat("wide and overlaps the other positions, the week-3 reading was noise and chasing it\n")
cat("would be exactly the mistake that produced the NGS regression.\n")
