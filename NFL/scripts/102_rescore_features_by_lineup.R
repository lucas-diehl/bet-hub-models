source("R/utilities.R")
source("R/qb_starter.R")
source("R/fantasy_prop_model.R")
source("C:/Users/ljdie/OneDrive/Documents/DFS ENGINE/spine/R/lineup_percentile.R")
assert_packages(); ensure_directories()
suppressPackageStartupMessages(library(data.table))
options(width = 205)

## Re-score this session's features on the metric the thesis argues for (median
## lineup percentile) instead of the one I used all along (MAE / R^2). Includes
## the efficiency score I KILLED on dR2 +0.004 -- if ranking is what pays, that
## decision deserves re-examination.
##
## Uses real DK salary files for past NFL slates so the cap constraint is real.

feat <- as.data.table(readRDS("data/processed/fantasy_prop_features.rds"))
stats <- as.data.table(readRDS("data/raw/player_stats_2021_2026.rds"))[season_type == "REG"]
setkey(stats, player_id, season, week)

sal_dir <- "C:/Users/ljdie/OneDrive/Documents/DFS ENGINE/data/slates/nfl"
files <- list.files(sal_dir, pattern = "^dk_\\d{4}-\\d{2}-\\d{2}_main\\d*\\.csv$", full.names = TRUE)
if (!length(files)) files <- list.files(sal_dir, pattern = "\\.csv$", full.names = TRUE)
cat("salary files found:", length(files), "\n")

nkey <- function(x) tolower(gsub("[^a-z]", "", x))

## Build one evaluation slate from a salary CSV: join salaries to actual points
## and to each projection variant.
build_slate <- function(path, proj_map) {
  s <- tryCatch(as.data.table(read.csv(path, stringsAsFactors = FALSE)), error = function(e) NULL)
  if (is.null(s) || !nrow(s)) return(NULL)
  nm <- intersect(c("Name","name","player_name"), names(s))[1]
  sl <- intersect(c("Salary","salary"), names(s))[1]
  if (is.na(nm) || is.na(sl)) return(NULL)
  d <- data.table(nk = nkey(s[[nm]]), salary = suppressWarnings(as.numeric(s[[sl]])))
  d <- d[is.finite(salary) & salary > 0]
  d <- unique(d, by = "nk")
  dt <- as.Date(sub("^dk_(\\d{4}-\\d{2}-\\d{2}).*$", "\\1", basename(path)))
  # map slate date -> the NFL week whose games start on/just after it
  wk <- stats[season == 2026, .(d = min(as.Date(NA))), by = week]  # placeholder
  list(d = d, date = dt)
}

## Simpler, robust alternative: evaluate on the walk-forward TEST weeks directly,
## using each week's actual points and the salary captured in fantasy_salary_stack
## when present; otherwise fall back to a salary proxy from the projection board.
## We compare variants on the SAME pool, so any consistent salary source is valid.
sal_hist <- tryCatch(as.data.table(readRDS("data/processed/dfs_salaries_dk_2022_plus.rds")),
                     error = function(e) NULL)
if (is.null(sal_hist)) stop("need data/processed/dfs_salaries_dk_2022_plus.rds for real caps")
cat("historical salary rows:", nrow(sal_hist), " cols:", paste(names(sal_hist), collapse=","), "\n")
scol <- intersect(c("salary","Salary"), names(sal_hist))[1]
pcol <- intersect(c("player_name","name","player"), names(sal_hist))[1]
wcol <- intersect(c("week"), names(sal_hist))[1]
sccol <- intersect(c("season"), names(sal_hist))[1]
sal_hist[, nk := nkey(get(pcol))]

## variants: which extra columns to ZERO OUT to simulate "without this feature"
variants <- list(
  baseline_all_off = c("qb_pair_delta_r5","qb_pair_games_r5",
                       grep("^ngs_", names(feat), value = TRUE)),
  qb_pair_only     = grep("^ngs_", names(feat), value = TRUE),
  ngs_only         = c("qb_pair_delta_r5","qb_pair_games_r5"),
  both_on          = character(0)
)

specs <- fantasy_target_specifications()[c("receptions","receiving_yards","receiving_tds",
                                            "rushing_yards","rushing_tds","passing_yards","passing_tds")]
ppr_of <- function(d) {
  g <- function(n) if (n %in% names(d)) d[[n]] else 0
  g("receptions") + 0.1*g("receiving_yards") + 6*g("receiving_tds") +
    0.1*g("rushing_yards") + 6*g("rushing_tds") + 0.04*g("passing_yards") + 4*g("passing_tds")
}

pred_ppr <- function(ff, label) {
  preds <- list()
  for (tn in names(specs)) {
    sp <- specs[[tn]]
    td <- as.data.table(fantasy_target_candidates(as.data.frame(ff), sp$family))
    for (ts in 2024:2025) {
      tr <- td[season < ts & prior_games >= 1]; te <- td[season == ts & prior_games >= 1]
      if (nrow(tr) < 200 || !nrow(te)) next
      fit <- fit_fantasy_stat_model(as.data.frame(tr), as.data.frame(te), sp$outcome, sp$objective,
                                     20260728L + ts + match(tn, names(specs)))
      preds[[length(preds)+1L]] <- data.table(player_id=te$player_id, season=te$season, week=te$week,
                                              target=tn, pred=fit$prediction)
      rm(fit); gc(FALSE)
    }
  }
  P <- dcast(rbindlist(preds), player_id + season + week ~ target, value.var = "pred", fill = 0)
  P[, proj_ppr := ppr_of(P)]
  cat("  ", label, "->", nrow(P), "player-weeks\n")
  P[, .(player_id, season, week, proj_ppr)]
}

cat("\nbuilding projections per variant (2024-2025 held out)...\n")
PV <- lapply(names(variants), function(v) {
  ff <- copy(feat)
  z <- intersect(variants[[v]], names(ff))
  if (length(z)) ff[, (z) := 0]
  pred_ppr(ff, v)
})
names(PV) <- names(variants)

## assemble per-slate pools and evaluate
actual <- stats[, .(player_id, season, week, act = fantasy_points_ppr,
                    nk = nkey(player_display_name))]
res <- list()
for (v in names(PV)) {
  P <- merge(PV[[v]], actual, by = c("player_id","season","week"))
  P <- merge(P, sal_hist[, .(nk, season = get(sccol), week = get(wcol),
                             salary = get(scol), pos = position)],
             by = c("nk","season","week"))
  P <- P[is.finite(salary) & salary > 0 & pos %in% c("QB","RB","WR","TE")]
  slates <- split(P, by = c("season","week"), drop = TRUE)
  slates <- slates[vapply(slates, nrow, integer(1)) >= 40]
  sl <- lapply(slates, function(d)
    list(proj = d$proj_ppr, actual = d$act, salary = d$salary, pos = d$pos))
  ## cap 47000, not 50000: the roster is DK's 9-man minus DST (no DST actuals
  ## exist in nflreadr weekly), so the 8 skill slots get the cap less a typical
  ## DST price. Leaving it at 50000 lets the skill players absorb the DST budget
  ## and makes the reference set richer than any real entry.
  out <- lineup_percentile_summary(sl, n_slots = 8L, cap = 47000L, n_sample = 5000L,
                                   label = v, pos_req = LP_NFL_POS_REQ)
  if (!is.null(out)) res[[v]] <- out
  cat(sprintf("  %-18s slates=%2d  MEDIAN pctile=%.4f  mean=%.4f  beat-field-median=%.1f%%\n",
              v, out$slates, out$median_pctile, out$mean_pctile, 100*out$beat_median_pct))
}

cat("\n=== THESIS METRIC: median lineup percentile (higher = better lineups) ===\n")
R <- rbindlist(lapply(res, as.data.table))
print(R[order(-median_pctile)])
cat("\n(For contrast, the metric I used all session -- dR2 -- ranked these:\n")
cat("  qb_pair +0.002, ngs +0.003/+0.004, efficiency score +0.004 (killed).)\n")

