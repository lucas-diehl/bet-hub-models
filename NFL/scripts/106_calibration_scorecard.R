source("R/utilities.R")
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
options(width = 215)

## PROPER CALIBRATION SCORECARD -- replaces the biased benchmark I used earlier today.
##
## My first pass compared projections to the mean of the TOP-12 WRs BY ACTUAL SCORE and
## called the result "41% of reality". That is selection on the outcome: the players who
## scored highest are, by construction, the ones who most beat their expectation. A
## PERFECTLY calibrated model fails that test. The number was not a measurement.
##
## What is actually diagnostic:
##   A. population calibration -- mean(proj) vs mean(actual) over the SAME players
##   B. calibration curve      -- bin by PROJECTION (never by actual); within each bin a
##                                calibrated model has mean(proj) ~= mean(actual)
##   C. top-N BY PROJECTION    -- the DFS-relevant question: if we roster who the model
##                                likes, what do they score?
##   D. rank skill             -- Spearman: does it ORDER players correctly? This is what
##                                lineup construction actually consumes.
##
## The SPREAD collapse is reported separately and is NOT subject to this bias: it is the
## sd of the model's own predictions, computed without reference to any outcome.

st <- as.data.table(readRDS("data/raw/player_stats_2021_2026.rds"))[
  season_type == "REG" & season == 2026]

score_week <- function(json_path, label) {
  if (!file.exists(json_path)) { cat("missing:", json_path, "\n"); return(NULL) }
  x <- fromJSON(json_path)
  wk <- as.integer(x$week)
  J <- as.data.table(x$players)
  act <- st[week == wk, .(name = player_display_name, position,
                          actual = fantasy_points_ppr)]
  if (!nrow(act)) { cat(sprintf("\n%s (week %d): no actuals yet -- cannot calibrate\n", label, wk)); return(NULL) }

  cat(sprintf("\n################ %s  (week %d) ################\n", label, wk))
  out <- list()
  for (ps in c("WR", "TE", "RB", "QB")) {
    m <- merge(J[pos == ps, .(name, proj = proj_ppr)],
               act[position == ps, .(name, actual)], by = "name")
    if (nrow(m) < 20) next

    cat(sprintf("\n--- %s  (n = %d matched) ---\n", ps, nrow(m)))
    cat(sprintf("A. population calibration : mean proj %6.2f   mean actual %6.2f   ratio %.3f\n",
                mean(m$proj), mean(m$actual), mean(m$proj) / mean(m$actual)))

    ## B. bin by PROJECTION, not by actual
    m[, bin := cut(proj, stats::quantile(proj, seq(0, 1, 0.25), na.rm = TRUE),
                   include.lowest = TRUE, labels = c("Q1 low", "Q2", "Q3", "Q4 high"))]
    cc <- m[, .(n = .N, mean_proj = round(mean(proj), 2), mean_actual = round(mean(actual), 2)),
            by = bin][order(bin)]
    cc[, ratio := round(mean_proj / pmax(mean_actual, 1e-9), 3)]
    cat("B. calibration curve (binned by PROJECTION -- unbiased):\n")
    print(cc)

    ## C. top-12 by PROJECTION -- what we would actually have rostered
    t12 <- head(m[order(-proj)], 12)
    cat(sprintf("C. top-12 BY PROJECTION   : mean proj %6.2f   they actually scored %6.2f   ratio %.3f\n",
                mean(t12$proj), mean(t12$actual), mean(t12$proj) / mean(t12$actual)))

    ## D. rank skill -- what lineup construction consumes
    rho <- suppressWarnings(stats::cor(m$proj, m$actual, method = "spearman"))
    pear <- suppressWarnings(stats::cor(m$proj, m$actual))
    cat(sprintf("D. rank skill             : spearman %.3f   pearson %.3f\n", rho, pear))

    ## how much of the real top-12 did we have in OUR top-12? (hit rate, unbiased)
    real12 <- head(m[order(-actual)], 12)$name
    hit <- length(intersect(t12$name, real12))
    cat(sprintf("   overlap with the actual top-12: %d of 12\n", hit))

    out[[ps]] <- data.table(label = label, week = wk, pos = ps, n = nrow(m),
                            pop_ratio = round(mean(m$proj) / mean(m$actual), 3),
                            top12proj_ratio = round(mean(t12$proj) / mean(t12$actual), 3),
                            spearman = round(rho, 3), hit12 = hit)
  }
  rbindlist(out, fill = TRUE)
}

res <- rbindlist(list(
  score_week("outputs/fantasy_prop_2026_week3_projections.json", "WEEK 3 build (pre-collapse)"),
  score_week("outputs/fantasy_prop_2026_week4_projections.json", "WEEK 4 build (current)")
), fill = TRUE)

if (nrow(res)) {
  cat("\n\n======================= SUMMARY =======================\n")
  cat("pop_ratio       : mean(proj)/mean(actual) on the same players. 1.00 = calibrated.\n")
  cat("top12proj_ratio : our top-12 picks' proj vs what they scored. 1.00 = calibrated.\n")
  cat("spearman        : rank skill, what the optimizer actually consumes.\n")
  cat("hit12           : how many of the real top-12 our top-12 caught.\n\n")
  print(res)
}
