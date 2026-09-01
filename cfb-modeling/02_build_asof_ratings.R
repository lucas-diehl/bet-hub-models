# ============================================================================
# 02_build_asof_ratings.R  —  Phase 1-2: leak-free opponent-adjusted ratings
# ----------------------------------------------------------------------------
# For every (season, as_of_week) we estimate each team's opponent-adjusted
# offensive & defensive Points-Per-Drive (and unit ratings + tempo), using
# ONLY games completed strictly before that week, blended with a preseason
# prior (prior-season carryover, optionally nudged by SP+). This is the
# engine feeding all three projection approaches in 03_models_backtest.R.
#
# Method: iterative empirical-Bayes opponent adjustment (KenPom-style).
#   observed_off_ig = adj_off_i + (adj_def_j - LG) + hfa*home + resid
#   adj_off_i = shrink_to_prior( mean_g[ observed - (adj_def_j - LG) - hfa*home ] )
# Shrinkage weight K (in games) makes the prior dominate early season and the
# data take over by ~week 5.
#
# Output: data_cache/asof_ratings.rds  (one row per season, as_of_week, team)
# ============================================================================

suppressWarnings(suppressMessages({ library(dplyr); library(tidyr) }))
set.seed(42)
CACHE <- "data_cache"

# --- hyper-parameters (tuned later in the bake-off) --------------------------
K_SHRINK <- 5      # prior weight in "games"; empirical == prior at n=K
ITERS    <- 8      # opponent-adjustment iterations (converges fast)
REG      <- 0.50   # season-to-season regression of carryover toward the mean
# SP+ blend weight into the preseason prior. Env-overridable so we can run a
# clean SP+=0 robustness check. IMPORTANT: the prior for season s uses PRIOR-year
# (s-1) SP+, never same-season (cfbd_ratings_sp is end-of-season = lookahead).
SP_WEIGHT <- as.numeric(Sys.getenv("PPP_SP_WEIGHT", "0.35"))

read_rds_retry <- function(path, tries = 4, wait = 2) {
  for (i in seq_len(tries)) {
    out <- tryCatch(readRDS(path), error = function(e) e)
    if (!inherits(out, "error")) return(out)
    if (i < tries) Sys.sleep(wait)
  }
  stop(sprintf("Failed to read %s after %d tries", path, tries))
}

cat("STEP 1-2: OPPONENT-ADJUSTED AS-OF RATINGS\n"); cat(strrep("-", 78), "\n")

tg <- read_rds_retry(file.path(CACHE, "team_game_ppp.rds"))
sp <- tryCatch(read_rds_retry(file.path(CACHE, "sp_ratings.rds")), error = function(e) NULL)

# Opponent/defense value columns paired with each offensive metric.
# For each metric we adjust an offense rating and a defense (allowed) rating.
METRICS <- list(
  ppd  = c(off = "off_ppd",      def = "def_ppd"),
  epa  = c(off = "off_epa",      def = "def_epa"),
  pass = c(off = "off_pass_epa", def = "def_pass_epa"),
  rush = c(off = "off_rush_epa", def = "def_rush_epa"),
  expl = c(off = "off_td_rate",  def = "def_td_rate"),
  fin  = c(off = "off_rz_ppd",   def = "def_rz_ppd")
)
LG <- sapply(METRICS, function(m) mean(c(tg[[m["off"]]], tg[[m["def"]]]), na.rm = TRUE))
LG_PACE <- mean(tg$off_drives + tg$def_drives, na.rm = TRUE)  # total drives / game

# ----------------------------------------------------------------------------
# Core iterative opponent-adjustment solver for ONE metric on a set of games.
#   rows: team, opponent, is_home, oval (offense value), dval (allowed value)
#   prior_off/prior_def: named numeric vectors (full team set), anchor for shrink
# ----------------------------------------------------------------------------
generic_adjust <- function(rows, prior_off, prior_def, lg, K = K_SHRINK, iters = ITERS) {
  rows <- rows[!is.na(rows$oval) & !is.na(rows$dval) & !is.na(rows$opponent), , drop = FALSE]
  teams <- union(rows$team, rows$opponent)
  if (length(teams) == 0) return(tibble(team = character(), adj_off = numeric(), adj_def = numeric()))
  fill <- function(pri) { v <- setNames(rep(lg, length(teams)), teams)
                          k <- intersect(names(pri), teams); v[k] <- pri[k]; v }
  ao <- fill(prior_off); ad <- fill(prior_def)
  po <- ao; pd <- ad  # fixed prior anchors
  home_sgn <- 2 * rows$is_home - 1
  # home-field advantage on this metric, estimated leak-free from these games
  hfa_half <- if (sum(rows$is_home == 1) > 5 && sum(rows$is_home == 0) > 5)
    0.5 * (mean(rows$oval[rows$is_home == 1], na.rm = TRUE) -
           mean(rows$oval[rows$is_home == 0], na.rm = TRUE)) else 0
  n_i <- tapply(rep(1, nrow(rows)), rows$team, sum)
  for (it in seq_len(iters)) {
    off_c <- rows$oval - (ad[rows$opponent] - lg) - hfa_half * home_sgn
    def_c <- rows$dval - (ao[rows$opponent] - lg) + hfa_half * home_sgn
    ao_raw <- tapply(off_c, rows$team, mean)
    ad_raw <- tapply(def_c, rows$team, mean)
    tu <- names(ao_raw); ni <- as.numeric(n_i[tu])
    ao[tu] <- (ni * as.numeric(ao_raw[tu]) + K * po[tu]) / (ni + K)
    ad[tu] <- (ni * as.numeric(ad_raw[tu]) + K * pd[tu]) / (ni + K)
  }
  attr_hfa <<- hfa_half
  tibble(team = names(ao), adj_off = as.numeric(ao), adj_def = as.numeric(ad))
}

# Additive tempo model: total_drives_g = pace_i + pace_j (each centered LG_PACE/2)
pace_adjust <- function(rows, iters = ITERS, K = K_SHRINK) {
  rows <- rows[!is.na(rows$total_drives) & !is.na(rows$opponent), , drop = FALSE]
  teams <- union(rows$team, rows$opponent)
  if (!length(teams)) return(tibble(team = character(), pace = numeric()))
  base <- LG_PACE / 2
  pc <- setNames(rep(base, length(teams)), teams)
  n_i <- tapply(rep(1, nrow(rows)), rows$team, sum)
  for (it in seq_len(iters)) {
    contrib <- rows$total_drives - pc[rows$opponent]
    raw <- tapply(contrib, rows$team, mean)
    tu <- names(raw); ni <- as.numeric(n_i[tu])
    pc[tu] <- (ni * as.numeric(raw[tu]) + K * base) / (ni + K)
  }
  tibble(team = names(pc), pace = as.numeric(pc))
}

# Adjust every metric on one slice of games; returns wide tibble team x metrics.
adjust_all <- function(rows_slice, priors) {
  out <- NULL; hfa_ppd <- 0
  for (nm in names(METRICS)) {
    m <- METRICS[[nm]]
    r <- data.frame(team = rows_slice$team, opponent = rows_slice$opponent,
                    is_home = rows_slice$is_home,
                    oval = rows_slice[[m["off"]]], dval = rows_slice[[m["def"]]])
    attr_hfa <<- 0
    adj <- generic_adjust(r, priors[[nm]]$off, priors[[nm]]$def, LG[[nm]])
    if (nm == "ppd") hfa_ppd <- attr_hfa
    names(adj) <- c("team", paste0("adj_off_", nm), paste0("adj_def_", nm))
    out <- if (is.null(out)) adj else full_join(out, adj, by = "team")
  }
  pr <- data.frame(team = rows_slice$team, opponent = rows_slice$opponent,
                   total_drives = rows_slice$off_drives + rows_slice$def_drives)
  out <- full_join(out, pace_adjust(pr), by = "team")
  attr(out, "hfa_ppd") <- hfa_ppd
  out
}

# ----------------------------------------------------------------------------
# SP+ preseason deviations (optional prior nudge), mapped to PPD scale via z.
# ----------------------------------------------------------------------------
sp_dev <- NULL
if (!is.null(sp)) {
  ycol <- intersect(c("year", "season"), names(sp))[1]
  tcol <- intersect(c("team", "school"), names(sp))[1]
  # Use POINT ratings, not *_ranking (rank 1..N is inverted-scale). SP+
  # offense_rating: higher = better offense. defense_rating: higher = MORE
  # points allowed = worse defense (so it aligns positively with def_ppd).
  ocol <- if ("offense_rating" %in% names(sp)) "offense_rating" else grep("offense.*rating", names(sp), value = TRUE)[1]
  dcol <- if ("defense_rating" %in% names(sp)) "defense_rating" else grep("defense.*rating", names(sp), value = TRUE)[1]
  if (!is.na(ocol) && !is.na(dcol) && !is.na(ycol) && !is.na(tcol)) {
    sd_ppd_off <- sd(tapply(tg$off_ppd, paste(tg$season, tg$team), mean, na.rm = TRUE), na.rm = TRUE)
    sp_dev <- sp %>%
      transmute(season = as.integer(.data[[ycol]]), team = .data[[tcol]],
                spo = suppressWarnings(as.numeric(.data[[ocol]])),
                spd = suppressWarnings(as.numeric(.data[[dcol]]))) %>%
      group_by(season) %>%
      mutate(off_dev =  scale(spo)[, 1] * sd_ppd_off,   # better offense -> higher off_ppd
             def_dev =  scale(spd)[, 1] * sd_ppd_off) %>% # worse defense -> higher def_ppd allowed
      ungroup() %>% select(season, team, off_dev, def_dev)
    if (SP_WEIGHT > 0)
      cat(sprintf("  SP+ prior enabled, weight=%.2f, prior-year (off '%s', def '%s')\n",
                  SP_WEIGHT, ocol, dcol))
    else cat("  SP+ prior DISABLED (SP_WEIGHT=0)\n")
  }
}
if (is.null(sp_dev)) cat("  SP+ prior unavailable -> carryover/league-mean priors only\n")

# ----------------------------------------------------------------------------
# Pass 1: full-season adjusted PPD per season (seed for next-season carryover)
# ----------------------------------------------------------------------------
flat_prior <- function(nm) list(off = setNames(numeric(0), character(0)),
                                def = setNames(numeric(0), character(0)))
season_final <- list()
for (s in sort(unique(tg$season))) {
  rs <- tg %>% filter(season == s)
  priors0 <- setNames(lapply(names(METRICS), flat_prior), names(METRICS))
  a <- adjust_all(rs, priors0)
  season_final[[as.character(s)]] <- a %>% select(team, adj_off_ppd, adj_def_ppd)
}

# Build preseason prior (named vectors) for a season, per metric.
build_priors <- function(s) {
  pri <- setNames(vector("list", length(METRICS)), names(METRICS))
  prev <- season_final[[as.character(s - 1)]]
  # PPD prior: carryover (regressed) + optional SP+ nudge
  off <- setNames(numeric(0), character(0)); def <- off
  if (!is.null(prev)) {
    off <- setNames(LG[["ppd"]] + REG * (prev$adj_off_ppd - LG[["ppd"]]), prev$team)
    def <- setNames(LG[["ppd"]] + REG * (prev$adj_def_ppd - LG[["ppd"]]), prev$team)
  }
  if (!is.null(sp_dev) && SP_WEIGHT > 0) {
    spd <- sp_dev %>% filter(season == s - 1)   # PRIOR-year SP+ (no lookahead)
    if (nrow(spd) > 0) {
      addO <- setNames(LG[["ppd"]] + spd$off_dev, spd$team)
      addD <- setNames(LG[["ppd"]] + spd$def_dev, spd$team)
      tt <- union(names(off), names(addO))
      blendO <- setNames(rep(LG[["ppd"]], length(tt)), tt)
      blendD <- setNames(rep(LG[["ppd"]], length(tt)), tt)
      for (t in tt) {
        co <- if (t %in% names(off)) off[[t]] else LG[["ppd"]]
        so <- if (t %in% names(addO)) addO[[t]] else co
        blendO[t] <- (1 - SP_WEIGHT) * co + SP_WEIGHT * so
        cd <- if (t %in% names(def)) def[[t]] else LG[["ppd"]]
        sdd <- if (t %in% names(addD)) addD[[t]] else cd
        blendD[t] <- (1 - SP_WEIGHT) * cd + SP_WEIGHT * sdd
      }
      off <- blendO; def <- blendD
    }
  }
  pri[["ppd"]] <- list(off = off, def = def)
  # other metrics: shrink to league mean (empty prior -> generic_adjust fills LG)
  for (nm in setdiff(names(METRICS), "ppd"))
    pri[[nm]] <- list(off = setNames(numeric(0), character(0)),
                      def = setNames(numeric(0), character(0)))
  pri
}

# ----------------------------------------------------------------------------
# Pass 2: as-of ratings for every (season, week), games strictly before week
# ----------------------------------------------------------------------------
asof_list <- list()
for (s in sort(unique(tg$season))) {
  rs <- tg %>% filter(season == s)
  priors <- build_priors(s)
  wks <- sort(unique(rs$week[!is.na(rs$week)]))
  for (w in wks) {
    to_date <- rs %>% filter(week < w)                 # LEAK-FREE: strictly before
    stopifnot(nrow(to_date) == 0 || max(to_date$week) < w)  # leakage assertion
    if (nrow(to_date) == 0) {
      # preseason: use priors directly
      teams <- sort(unique(rs$team))
      base <- tibble(team = teams)
      for (nm in names(METRICS)) {
        po <- priors[[nm]]$off; pd <- priors[[nm]]$def
        base[[paste0("adj_off_", nm)]] <- ifelse(teams %in% names(po), po[teams], LG[[nm]])
        base[[paste0("adj_def_", nm)]] <- ifelse(teams %in% names(pd), pd[teams], LG[[nm]])
      }
      base$pace <- LG_PACE / 2
      a <- base; hfa_ppd <- 0.10
    } else {
      a <- adjust_all(to_date, priors)
      hfa_ppd <- attr(a, "hfa_ppd")
    }
    a$season <- s; a$as_of_week <- w
    a$hfa_ppd <- hfa_ppd; a$lg_ppd <- LG[["ppd"]]; a$lg_pace <- LG_PACE
    a$n_games_to_date <- nrow(to_date)
    asof_list[[paste(s, w, sep = "_")]] <- a
  }
  cat(sprintf("  season %d: %d as-of weeks\n", s, length(wks)))
}

# ----------------------------------------------------------------------------
# Preseason seed for the NEXT (unplayed) season so Week-1 games are modelable
# before any of its games exist in team_game_ppp. Uses build_priors(next) =
# regressed prior-season carryover + prior-year SP+ (no lookahead). Superseded
# automatically once real games arrive: that season becomes the max and is built
# by the loop above, and this block then targets the following season.
# ----------------------------------------------------------------------------
next_s <- max(sort(unique(tg$season))) + 1
pri <- build_priors(next_s)
pre_teams <- sort(unique(names(pri[["ppd"]]$off)))          # prior-season FBS carryover set
if (length(pre_teams) > 0) {
  base <- tibble(team = pre_teams)
  for (nm in names(METRICS)) {
    po <- pri[[nm]]$off; pd <- pri[[nm]]$def
    base[[paste0("adj_off_", nm)]] <- ifelse(pre_teams %in% names(po), po[pre_teams], LG[[nm]])
    base[[paste0("adj_def_", nm)]] <- ifelse(pre_teams %in% names(pd), pd[pre_teams], LG[[nm]])
  }
  base$pace <- LG_PACE / 2
  for (w in 1:4) {                                          # cover the early-season window
    a <- base; a$season <- next_s; a$as_of_week <- w
    a$hfa_ppd <- 0.10; a$lg_ppd <- LG[["ppd"]]; a$lg_pace <- LG_PACE; a$n_games_to_date <- 0L
    asof_list[[paste(next_s, w, sep = "_")]] <- a
  }
  cat(sprintf("  season %d (preseason seed): weeks 1-4 for %d teams\n", next_s, length(pre_teams)))
}

asof_ratings <- bind_rows(asof_list) %>%
  filter(!is.na(team), team != "FCS" | TRUE)   # keep FCS rating (used for opp adj)

# ----------------------------------------------------------------------------
# Diagnostics + save
# ----------------------------------------------------------------------------
cat("\n=== RATINGS SANITY ===\n")
cat(sprintf("  rows: %d  (season x week x team)\n", nrow(asof_ratings)))
cat(sprintf("  adj_off_ppd range: [%.2f, %.2f], mean %.2f (LG=%.2f)\n",
            min(asof_ratings$adj_off_ppd, na.rm=TRUE), max(asof_ratings$adj_off_ppd, na.rm=TRUE),
            mean(asof_ratings$adj_off_ppd, na.rm=TRUE), LG[["ppd"]]))
# End-of-2024 top-10 offenses as a face-validity check
cat("\n  Top-10 adjusted offenses, final 2024 as-of week:\n")
last24 <- max(asof_ratings$as_of_week[asof_ratings$season == 2024])
print(asof_ratings %>% filter(season == 2024, as_of_week == last24, team != "FCS") %>%
        arrange(desc(adj_off_ppd)) %>% transmute(team, adj_off_ppd = round(adj_off_ppd,3),
                                                 adj_def_ppd = round(adj_def_ppd,3)) %>% head(10))

saveRDS(asof_ratings, file.path(CACHE, "asof_ratings.rds"))
cat("\n✓ saved data_cache/asof_ratings.rds\n")
