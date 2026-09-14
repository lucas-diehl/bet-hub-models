# ============================================================================
# 10_build_dvoa_ratings.R  —  as-of DVOA-style ratings (5 variants) for the
#                             OPEN_ATS consensus-agreement filter
# ----------------------------------------------------------------------------
# WHY THIS EXISTS (read before changing):
# Arm O (`proj_vs_open`) bets our PPD projection against the FROZEN opening line.
# Validation (FEATURE_REGISTRY 2026-09-09) found that requiring a SECOND,
# independently-constructed rating to agree on the side lifts the win rate from
# ~55.1% to ~58% OOS — an effect that is NOT reproducible with cheaper proxies
# (rating maturity / volatility are near-uncorrelated with agreement, r=-.09/+.07,
# and are non-significant once agreement is in the model).
#
# We deploy CONSENSUS (>=3 of 5 variants agree), NOT a single variant:
#   * single-variant win rates (56.5-61.2%) are statistically indistinguishable —
#     empirical-Bayes finds between-variant variance of exactly ZERO, so the
#     spread is sampling noise and the best single-variant number (v7's 61.2%)
#     is winner's curse.
#   * consensus shows a clean MONOTONIC dose-response (>=1..>=5 votes ->
#     56.3/56.8/58.6/58.9/61.9%), which is far harder to produce by chance.
#   * k>=3 (simple majority) is defensible a priori rather than chosen for score,
#     and lands at 58.6% / n=473 -- matching the shrunk cross-variant estimate.
# EXPECT ~58%, NOT 61.9%. Sizing on the higher number over-stakes by ~57% (Kelly).
#
# The 5 variants deliberately span DVOA's real design space:
#   v3 tight cap (5/95)   v4 loose cap (1/99)   v5 success-rate only
#   v6 passing-down situational weighting       v7 separate rush/pass ratings
#
# Leak-free: play winsorization/standardization use only data available at build
# time (in production that is strictly past games); ratings are computed as-of
# (`week < w`) with prior-season carryover, mirroring 02_build_asof_ratings.R.
#
# Output: data_cache/dvoa_asof_variants.rds  (season, as_of_week, team, per-variant
#         adj_off_/adj_def_ columns). Consumed by 07_write_feed.R.
# ============================================================================

source("00_ppp_common.R")
suppressWarnings(suppressMessages({ library(dplyr); library(tibble) }))
set.seed(42)

K_SHRINK <- 5    # matches 02
ITERS    <- 8
REG      <- 0.5  # prior-season carryover regression toward league mean
MIN_PLAYS_SIDE <- 15   # per team-game, both sides, else drop the team-game

cat("STEP 10: BUILD AS-OF DVOA RATINGS (5 variants)\n")

pbp <- read_rds_retry(file.path(CACHE, "pbp_data.rds"))
p <- pbp %>%
  filter(!is.na(EPA), !is.na(pos_team), !is.na(def_pos_team), pos_team != "", def_pos_team != "") %>%
  transmute(season, week, game_id, pos_team, def_pos_team, EPA,
            period = suppressWarnings(as.numeric(period)),
            psd    = suppressWarnings(as.numeric(pos_score_diff)),
            down   = suppressWarnings(as.numeric(down)),
            dist   = suppressWarnings(as.numeric(distance)),
            pass   = suppressWarnings(as.numeric(pass)),
            rush   = suppressWarnings(as.numeric(rush)))
# garbage-time filter mirrors 01_build_possessions.R (score+period based)
p <- p %>% mutate(garbage = (!is.na(period) & !is.na(psd)) &
                    ((period >= 3 & abs(psd) > 28) | (period == 4 & abs(psd) > 21))) %>%
  filter(!garbage)
cat(sprintf("  plays after garbage filter: %d\n", nrow(p)))

q05 <- quantile(p$EPA, c(0.05, 0.95), na.rm = TRUE)
q02 <- quantile(p$EPA, c(0.02, 0.98), na.rm = TRUE)
q01 <- quantile(p$EPA, c(0.01, 0.99), na.rm = TRUE)
cap <- function(x, q) pmin(pmax(x, q[1]), q[2])
p <- p %>% mutate(
  pdown = (!is.na(down) & !is.na(dist)) & ((down == 2 & dist >= 8) | (down >= 3 & dist >= 5)),
  m_v3 = cap(EPA, q05),
  m_v4 = cap(EPA, q01),
  m_v5 = as.integer(EPA > 0),
  m_v6 = ifelse(pdown, 1.5, 1.0) * cap(EPA, q02))

agg_simple <- function(col) {
  o <- p %>% group_by(season, week, game_id, team = pos_team, opponent = def_pos_team) %>%
    summarise(ov = mean(.data[[col]], na.rm = TRUE), n_o = n(), .groups = "drop")
  d <- p %>% group_by(season, week, game_id, team = def_pos_team, opponent = pos_team) %>%
    summarise(dv = mean(.data[[col]], na.rm = TRUE), n_d = n(), .groups = "drop")
  o %>% inner_join(d, by = c("season","week","game_id","team","opponent")) %>%
    filter(n_o >= MIN_PLAYS_SIDE, n_d >= MIN_PLAYS_SIDE) %>% select(-n_o, -n_d)
}
# v7: separate rush/pass ratings, recombined 0.6 pass / 0.4 rush (mirrors how DVOA is published)
agg_rushpass <- function() {
  o <- p %>% group_by(season, week, game_id, team = pos_team, opponent = def_pos_team) %>%
    summarise(op = mean(cap(EPA, q02)[pass == 1], na.rm = TRUE),
              orr = mean(cap(EPA, q02)[rush == 1], na.rm = TRUE), n_o = n(), .groups = "drop")
  d <- p %>% group_by(season, week, game_id, team = def_pos_team, opponent = pos_team) %>%
    summarise(dp = mean(cap(EPA, q02)[pass == 1], na.rm = TRUE),
              dr = mean(cap(EPA, q02)[rush == 1], na.rm = TRUE), n_d = n(), .groups = "drop")
  o %>% inner_join(d, by = c("season","week","game_id","team","opponent")) %>%
    filter(n_o >= MIN_PLAYS_SIDE, n_d >= MIN_PLAYS_SIDE,
           !is.na(op), !is.na(orr), !is.na(dp), !is.na(dr)) %>%
    transmute(season, week, game_id, team, opponent, ov = 0.6*op + 0.4*orr, dv = 0.6*dp + 0.4*dr)
}
tgs <- list(v3 = agg_simple("m_v3"), v4 = agg_simple("m_v4"), v5 = agg_simple("m_v5"),
            v6 = agg_simple("m_v6"), v7 = agg_rushpass())

# ---- iterative opponent adjustment (same shape as 02's generic_adjust) -------
adjust <- function(rows, prior_off, prior_def, lg, K = K_SHRINK, iters = ITERS) {
  rows <- rows[!is.na(rows$oval) & !is.na(rows$dval) & !is.na(rows$opponent), , drop = FALSE]
  teams <- union(rows$team, rows$opponent)
  if (!length(teams)) return(tibble(team = character(), adj_off = numeric(), adj_def = numeric()))
  fill <- function(pri) { v <- setNames(rep(lg, length(teams)), teams)
                          k <- intersect(names(pri), teams); v[k] <- pri[k]; v }
  ao <- fill(prior_off); ad <- fill(prior_def); po <- ao; pd <- ad
  n_i <- tapply(rep(1, nrow(rows)), rows$team, sum)
  for (it in seq_len(iters)) {
    off_c <- rows$oval - (ad[rows$opponent] - lg)
    def_c <- rows$dval - (ao[rows$opponent] - lg)
    ar <- tapply(off_c, rows$team, mean); dr <- tapply(def_c, rows$team, mean)
    tu <- names(ar); ni <- as.numeric(n_i[tu])
    ao[tu] <- (ni * as.numeric(ar[tu]) + K * po[tu]) / (ni + K)
    ad[tu] <- (ni * as.numeric(dr[tu]) + K * pd[tu]) / (ni + K)
  }
  tibble(team = names(ao), adj_off = as.numeric(ao), adj_def = as.numeric(ad))
}
build_asof <- function(tg, label) {
  LG <- mean(c(tg$ov, tg$dv), na.rm = TRUE)
  out <- list(); fin <- list()
  for (s in sort(unique(tg$season))) {
    prev <- fin[[as.character(s - 1)]]
    if (is.null(prev)) { po <- setNames(numeric(0), character(0)); pd <- po } else {
      po <- setNames(LG + REG * (prev$adj_off - LG), prev$team)
      pd <- setNames(LG + REG * (prev$adj_def - LG), prev$team) }
    ss <- tg %>% filter(season == s)
    for (w in sort(unique(ss$week))) {
      h <- ss %>% filter(week < w)
      if (nrow(h) < 20) next     # not enough in-season history yet -> no rating this week
      out[[length(out) + 1]] <- adjust(data.frame(team = h$team, opponent = h$opponent,
                                                  oval = h$ov, dval = h$dv), po, pd, LG) %>%
        mutate(season = s, as_of_week = w)
    }
    # COLD-START / UPCOMING WEEKS: tg is built from PLAYED games only, so the loop
    # above never emits a rating for a week whose games haven't happened yet — which
    # is exactly the week we need to bet. Emit as-of rows for the next few unplayed
    # weeks using all games played so far (mirrors 02's preseason-seed fix).
    wmax <- suppressWarnings(max(ss$week, na.rm = TRUE))
    if (is.finite(wmax) && nrow(ss) >= 20) {
      fitted <- adjust(data.frame(team = ss$team, opponent = ss$opponent,
                                  oval = ss$ov, dval = ss$dv), po, pd, LG)
      for (w in (wmax + 1):(wmax + 4))
        out[[length(out) + 1]] <- fitted %>% mutate(season = s, as_of_week = w)
    }
    fin[[as.character(s)]] <- adjust(data.frame(team = ss$team, opponent = ss$opponent,
                                                oval = ss$ov, dval = ss$dv), po, pd, LG)
  }
  bind_rows(out) %>%
    rename(!!paste0("aoff_", label) := adj_off, !!paste0("adef_", label) := adj_def) %>%
    mutate(!!paste0("lg_", label) := LG)
}
res <- NULL
for (nm in names(tgs)) {
  a <- build_asof(tgs[[nm]], nm)
  res <- if (is.null(res)) a else full_join(res, a, by = c("team", "season", "as_of_week"))
  cat(sprintf("  %s: %d as-of rows\n", nm, nrow(a)))
}
saveRDS(res, file.path(CACHE, "dvoa_asof_variants.rds"))
cat(sprintf("✓ saved %s (%d rows, seasons %s)\n", file.path(CACHE, "dvoa_asof_variants.rds"),
            nrow(res), paste(range(res$season), collapse = "-")))
