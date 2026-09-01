# ============================================================================
# 00_ppp_common.R  —  shared helpers for the PPP projection model
# ----------------------------------------------------------------------------
# Sourced by 03_models_backtest.R and 04_weekly_picks.R. Provides:
#   * betting-line -> game-level modeling table (targets + open/close spreads)
#   * FBS team mapper (non-FBS -> "FCS", matching 02's ratings universe)
#   * attach as-of ratings to games + build matchup features
#   * Approach A: opponent-adjusted PPD -> projected margin & total
#   * projection -> cover/over probability (normal residual model)
#   * XGBoost regression train/predict (approaches B & C)
#   * evaluation: summarize_bets / run_calibration / sig_test (from the
#     existing model, adapted) + margin/total error + CLV
# NOTE: no library(cfbfastR) here — we read local caches only.
# ============================================================================

suppressWarnings(suppressMessages({
  library(dplyr); library(tidyr); library(xgboost)
}))

CACHE <- "data_cache"
PAYOUT        <- 0.909    # -110 win return per unit risked
MIN_EDGE      <- 0.03     # |p-0.5| threshold (matches existing model)
BREAKEVEN_RATE<- 0.524    # -110 breakeven win rate

read_rds_retry <- function(path, tries = 4, wait = 2) {
  for (i in seq_len(tries)) {
    out <- tryCatch(readRDS(path), error = function(e) e)
    if (!inherits(out, "error")) return(out)
    if (i < tries) Sys.sleep(wait)
  }
  stop(sprintf("Failed to read %s after %d tries", path, tries))
}

# ----------------------------------------------------------------------------
# FBS mapper: any team not FBS-in-season collapses to "FCS" (as in 01/02).
# ----------------------------------------------------------------------------
make_fbs_mapper <- function(sp_ratings) {
  tcol  <- intersect(c("team", "school"), names(sp_ratings))[1]
  ycol  <- intersect(c("year", "season"), names(sp_ratings))[1]
  yrs   <- as.integer(sp_ratings[[ycol]])
  fbs   <- split(sp_ratings[[tcol]], yrs)
  avail <- sort(unique(yrs))
  # FBS list for a season; if that season is missing (e.g. current-year SP+ not
  # published yet), fall back to the latest earlier season (membership is stable).
  team_list <- function(s) {
    st <- fbs[[as.character(s)]]
    if (!is.null(st)) return(st)
    le <- avail[avail <= s]; fbs[[as.character(if (length(le)) max(le) else max(avail))]]
  }
  function(season, team) {
    ifelse(mapply(function(s, t) { st <- team_list(s); !is.null(st) && t %in% st }, season, team), team, "FCS")
  }
}

# ----------------------------------------------------------------------------
# Book-specific lines: keep ONLY DraftKings / FanDuel (the two we bet), one row
# per game, DraftKings preferred. Returns game_id, book, and the book's closing +
# opening spread/total (for CLV). Games neither book prices are absent (dropped).
# NOTE: cfbfastR currently carries DraftKings but not FanDuel; the FanDuel branch
# is kept so it is picked up automatically if the provider appears.
# ----------------------------------------------------------------------------
BOOKS_ALLOWED <- c("DraftKings", "FanDuel")
book_lines <- function(betting_lines) {
  betting_lines %>%
    mutate(book = dplyr::case_when(
             grepl("draft\\s*kings", provider, ignore.case = TRUE) ~ "DraftKings",
             grepl("fan\\s*duel",    provider, ignore.case = TRUE) ~ "FanDuel",
             TRUE ~ NA_character_),
           pref = dplyr::case_when(book == "DraftKings" ~ 1L, book == "FanDuel" ~ 2L, TRUE ~ NA_integer_)) %>%
    filter(!is.na(book), !is.na(spread) | !is.na(over_under)) %>%
    arrange(pref) %>% distinct(game_id, .keep_all = TRUE) %>%   # DK preferred (pref=1)
    transmute(game_id, book,
              bk_spread      = suppressWarnings(as.numeric(spread)),
              bk_spread_open = suppressWarnings(as.numeric(spread_open)),
              bk_total       = suppressWarnings(as.numeric(over_under)),
              bk_total_open  = suppressWarnings(as.numeric(over_under_open)))
}

# ----------------------------------------------------------------------------
# Coach-persistent, as-of 4th-down aggressiveness (for the early-season ATS edge).
# Cumulative go-for-it rate in decision range over all PRIOR games under the same
# coaching staff (persists across seasons & schools). Leak-free (lagged). Returns
# one row per (season, game_id, team): asof_4d_go and n4 (attempts of history).
# Requires data_cache/team_game_features.rds (05_build_features.R) + coaches.rds.
# ----------------------------------------------------------------------------
build_asof_aggressiveness <- function(cache = CACHE) {
  feats <- read_rds_retry(file.path(cache, "team_game_features.rds")) %>%
    filter(team != "FCS", !is.na(week))
  co <- read_rds_retry(file.path(cache, "coaches.rds")) %>%
    mutate(coach = paste(first_name, last_name)) %>%
    group_by(school, year) %>% slice_max(games, n = 1, with_ties = FALSE) %>% ungroup() %>%
    transmute(team = school, season = as.integer(year), coach)
  feats %>%
    left_join(co, by = c("team", "season")) %>%
    mutate(ckey = coalesce(coach, paste0("T_", team))) %>%
    arrange(ckey, season, week) %>% group_by(ckey) %>%
    mutate(c_go  = dplyr::lag(cumsum(tidyr::replace_na(n4_go, 0))),
           c_tot = dplyr::lag(cumsum(tidyr::replace_na(n4_tot, 0)))) %>% ungroup() %>%
    transmute(season, game_id, team, asof_4d_go = c_go / pmax(c_tot, 1), n4 = c_tot)
}

# ----------------------------------------------------------------------------
# LATEST as-of aggressiveness per team, for a target season's UPCOMING games.
# build_asof_aggressiveness() keys by game_id (completed games only), so unplayed
# games get NA and the early-ATS strategy can't fire pre-game. Here we take each
# coaching staff's cumulative go-for-it rate through ALL completed games in
# team_game_features (played games only -> leak-free), then map it to each
# target-season team via that season's coach. Join to upcoming games BY TEAM.
# Once the season's own games complete they enter team_game_features and raise the
# staff's cumulative, so later weeks pick up the current-season form automatically.
# ----------------------------------------------------------------------------
latest_aggressiveness <- function(target_season, cache = CACHE) {
  feats <- read_rds_retry(file.path(cache, "team_game_features.rds")) %>%
    filter(team != "FCS", !is.na(week))
  co <- read_rds_retry(file.path(cache, "coaches.rds")) %>%
    mutate(coach = paste(first_name, last_name)) %>%
    group_by(school, year) %>% slice_max(games, n = 1, with_ties = FALSE) %>% ungroup() %>%
    transmute(team = school, season = as.integer(year), coach)
  coach_cum <- feats %>%
    left_join(co, by = c("team", "season")) %>%
    mutate(ckey = coalesce(coach, paste0("T_", team))) %>%
    group_by(ckey) %>%
    summarise(c_go = sum(n4_go, na.rm = TRUE), c_tot = sum(n4_tot, na.rm = TRUE), .groups = "drop") %>%
    mutate(asof_4d_go = c_go / pmax(c_tot, 1), n4 = c_tot)
  co %>% filter(season == target_season) %>%
    mutate(ckey = coalesce(coach, paste0("T_", team))) %>%
    left_join(coach_cum %>% select(ckey, asof_4d_go, n4), by = "ckey") %>%
    transmute(season = target_season, team, asof_4d_go, n4 = coalesce(n4, 0))
}

# ----------------------------------------------------------------------------
# Game-level modeling table from betting lines (mirrors Sections 9-10 of
# complete_cfb_betting_model.R). Returns one row per game with lines + targets.
# ----------------------------------------------------------------------------
build_model_data <- function(betting_lines, game_dates, include_upcoming = FALSE) {
  betting_clean <- betting_lines %>%
    filter(!is.na(spread)) %>%
    mutate(spread = as.numeric(spread)) %>%
    left_join(game_dates %>% select(game_id, gi_home_score = home_score,
                                    gi_away_score = away_score), by = "game_id") %>%
    mutate(home_score = coalesce(as.numeric(home_score), gi_home_score),
           away_score = coalesce(as.numeric(away_score), gi_away_score)) %>%
    select(-gi_home_score, -gi_away_score)
  # LIVE feed path keeps UPCOMING (unplayed) games so pre-game picks can be posted.
  # They carry NA scores/actual_margin and are excluded from calibration downstream
  # (07's `hist` filters !is.na(actual_margin)). Backtest default drops them as before.
  if (!include_upcoming)
    betting_clean <- betting_clean %>% filter(!is.na(home_score), !is.na(away_score))
  betting_clean <- betting_clean %>%
    mutate(actual_margin = home_score - away_score,
           home_covered  = as.integer((actual_margin + spread) > 0))

  betting_clean %>%
    group_by(game_id) %>%
    mutate(has_consensus = any(provider == "consensus", na.rm = TRUE)) %>%
    filter((has_consensus & (provider == "consensus" | is.na(provider))) | !has_consensus) %>%
    summarise(
      season = first(na.omit(season)), week = first(na.omit(week)),
      home_team = first(na.omit(home_team)), away_team = first(na.omit(away_team)),
      home_score = first(na.omit(home_score)), away_score = first(na.omit(away_score)),
      actual_margin = first(na.omit(actual_margin)),
      home_covered  = first(na.omit(home_covered)),
      spread      = mean(spread, na.rm = TRUE),                    # closing
      spread_open = mean(suppressWarnings(as.numeric(spread_open)), na.rm = TRUE),
      over_under  = mean(suppressWarnings(as.numeric(over_under)),  na.rm = TRUE),
      .groups = "drop") %>%
    mutate(
      spread_feature = coalesce(spread_open, spread),              # opening (early view)
      line_movement  = spread - coalesce(spread_open, spread),     # close - open
      total_points   = home_score + away_score,
      over_hit       = as.integer((total_points - over_under) > 0)) %>%
    filter(!is.na(spread), !is.na(over_under))
}

# ----------------------------------------------------------------------------
# Attach home/away as-of ratings + build matchup features + Approach A proj.
#   model_data : output of build_model_data (+ needs season, week, teams)
#   asof       : asof_ratings.rds
#   fbs_map    : make_fbs_mapper(...)
# ----------------------------------------------------------------------------
RATING_METRICS <- c("ppd","epa","pass","rush","expl","fin")

attach_ratings <- function(model_data, asof, fbs_map) {
  md <- model_data %>%
    mutate(home_team = fbs_map(season, home_team),
           away_team = fbs_map(season, away_team))

  keep <- c("season","as_of_week","team", "pace","hfa_ppd","lg_ppd","lg_pace","n_games_to_date",
            paste0("adj_off_", RATING_METRICS), paste0("adj_def_", RATING_METRICS))
  a <- asof %>% select(any_of(keep))

  h <- a %>% rename_with(~ paste0("h_", .x), -c(season, as_of_week, team))
  aw <- a %>% rename_with(~ paste0("a_", .x), -c(season, as_of_week, team))

  md <- md %>%
    left_join(h,  by = c("season", "week" = "as_of_week", "home_team" = "team")) %>%
    left_join(aw, by = c("season", "week" = "as_of_week", "away_team" = "team"))

  LG   <- md$h_lg_ppd
  hfa  <- coalesce(md$h_hfa_ppd, 0.10)
  drv  <- (coalesce(md$h_pace, md$h_lg_pace/2) + coalesce(md$a_pace, md$a_lg_pace/2)) / 2

  # Approach A: opponent-adjusted points-per-drive -> points
  home_ppd <- coalesce(md$h_adj_off_ppd, LG) + coalesce(md$a_adj_def_ppd, LG) - LG + hfa
  away_ppd <- coalesce(md$a_adj_off_ppd, LG) + coalesce(md$h_adj_def_ppd, LG) - LG - hfa
  md$exp_drives_each <- drv
  md$proj_home_pts_A <- home_ppd * drv
  md$proj_away_pts_A <- away_ppd * drv
  md$proj_margin_A   <- (home_ppd - away_ppd) * drv
  md$proj_total_A    <- (home_ppd + away_ppd) * drv

  # Matchup edges (offense vs opponent defense) for the ML approaches
  ef <- function(off_h, def_a) coalesce(off_h, LG) - coalesce(def_a, LG)
  md <- md %>% mutate(
    home_off_edge  = ef(h_adj_off_ppd,  a_adj_def_ppd),
    away_off_edge  = ef(a_adj_off_ppd,  h_adj_def_ppd),
    net_ppd_edge   = home_off_edge - away_off_edge,
    sum_ppd_edge   = home_off_edge + away_off_edge,
    pace_sum       = coalesce(h_pace, h_lg_pace/2) + coalesce(a_pace, a_lg_pace/2),
    home_pass_edge = h_adj_off_pass - a_adj_def_pass,
    away_pass_edge = a_adj_off_pass - h_adj_def_pass,
    home_rush_edge = h_adj_off_rush - a_adj_def_rush,
    away_rush_edge = a_adj_off_rush - h_adj_def_rush,
    home_expl_edge = h_adj_off_expl - a_adj_def_expl,
    away_expl_edge = a_adj_off_expl - h_adj_def_expl,
    home_fin_edge  = h_adj_off_fin  - a_adj_def_fin,
    away_fin_edge  = a_adj_off_fin  - h_adj_def_fin,
    home_epa_edge  = h_adj_off_epa  - a_adj_def_epa,
    away_epa_edge  = a_adj_off_epa  - h_adj_def_epa,
    n_games_home   = coalesce(h_n_games_to_date, 0),
    n_games_away   = coalesce(a_n_games_to_date, 0)
  )
  md
}

# Feature sets for the ML approaches (all leak-free as-of ratings).
FEATS_B <- c("home_off_edge","away_off_edge","net_ppd_edge","sum_ppd_edge","pace_sum",
             "home_pass_edge","away_pass_edge","home_rush_edge","away_rush_edge",
             "home_expl_edge","away_expl_edge","home_fin_edge","away_fin_edge",
             "home_epa_edge","away_epa_edge","exp_drives_each","n_games_home","n_games_away")
# Approach C adds the physical projection + the market line (market-aware)
FEATS_C <- c(FEATS_B, "proj_margin_A","proj_total_A","spread_feature","over_under","line_movement")

# ----------------------------------------------------------------------------
# projection -> probability via normal residual model
#   home_covered = (actual_margin + spread) > 0  ->  P = pnorm((proj+spread)/sigma)
#   over_hit     = (total - over_under)     > 0  ->  P = pnorm((proj-ou)/sigma)
# ----------------------------------------------------------------------------
prob_cover <- function(proj_margin, spread, sigma) pnorm((proj_margin + spread) / sigma)
prob_over  <- function(proj_total, over_under, sigma) pnorm((proj_total - over_under) / sigma)

# ----------------------------------------------------------------------------
# XGBoost regression (approaches B & C)
# ----------------------------------------------------------------------------
xgb_reg_params <- list(objective = "reg:squarederror", eval_metric = "rmse",
                       max_depth = 3, eta = 0.05, subsample = 0.7,
                       colsample_bytree = 0.7, min_child_weight = 12, gamma = 1.0)

train_reg <- function(df, feats, target, nrounds = 300, min_rows = 100) {
  d <- df %>% select(all_of(c(feats, target))) %>%
    mutate(across(all_of(feats), ~replace_na(.x, 0))) %>%
    filter(!is.na(.data[[target]]))
  if (nrow(d) < min_rows) return(NULL)
  dm <- xgb.DMatrix(as.matrix(d[, feats, drop = FALSE]), label = d[[target]])
  xgb.train(xgb_reg_params, dm, nrounds = nrounds, verbose = 0)
}
predict_reg <- function(model, df, feats) {
  if (is.null(model)) return(rep(NA_real_, nrow(df)))
  mat <- df %>% select(all_of(feats)) %>% mutate(across(everything(), ~replace_na(.x, 0))) %>% as.matrix()
  predict(model, xgb.DMatrix(mat))
}

# ----------------------------------------------------------------------------
# Evaluation helpers (adapted from complete_cfb_betting_model.R Sections 15/15B)
# ----------------------------------------------------------------------------
summarize_bets <- function(df, prob_col, result_col, label, min_edge = MIN_EDGE, quiet = FALSE) {
  d <- df %>%
    filter(!is.na(.data[[prob_col]]), !is.na(.data[[result_col]])) %>%
    mutate(prob = .data[[prob_col]], result = .data[[result_col]],
           correct = as.integer((prob > 0.5 & result == 1) | (prob <= 0.5 & result == 0)),
           edge = abs(prob - 0.5), ev = prob * PAYOUT - (1 - prob),
           recommend = edge >= min_edge & ev > 0)
  rec <- d %>% filter(recommend)
  wins <- sum(rec$correct); losses <- nrow(rec) - wins
  profit <- wins * PAYOUT - losses
  roi <- if (nrow(rec) > 0) profit / nrow(rec) * 100 else NA_real_
  if (!quiet) cat(sprintf(
    "  %-30s all=%4d (%.1f%%) | rec=%4d %3d-%3d win=%.1f%% ROI=%+.1f%% profit=%+.1fu\n",
    label, nrow(d), mean(d$correct)*100, nrow(rec), wins, losses,
    if (nrow(rec)>0) wins/nrow(rec)*100 else 0, coalesce(roi, 0), profit))
  tibble(label = label, all_n = nrow(d), all_acc = mean(d$correct),
         rec_n = nrow(rec), wins = wins, losses = losses,
         win_pct = if (nrow(rec)>0) wins/nrow(rec) else NA_real_,
         roi = roi, profit = profit)
}

sig_test <- function(df, prob_col, result_col, label, min_edge = MIN_EDGE) {
  rec <- df %>%
    filter(!is.na(.data[[prob_col]]), !is.na(.data[[result_col]])) %>%
    mutate(prob = .data[[prob_col]], result = .data[[result_col]],
           correct = as.integer((prob>0.5&result==1)|(prob<=0.5&result==0))) %>%
    filter(abs(prob-0.5) >= min_edge, prob*PAYOUT-(1-prob) > 0)
  n <- nrow(rec); wins <- sum(rec$correct)
  if (n < 10) { cat(sprintf("  %-24s too few bets (n=%d)\n", label, n)); return(invisible(NULL)) }
  bt <- binom.test(wins, n, p = BREAKEVEN_RATE, alternative = "greater")
  stars <- if (bt$p.value<0.01) "***" else if (bt$p.value<0.05) "**" else if (bt$p.value<0.1) "*" else ""
  cat(sprintf("  %-24s n=%d win=%.1f%% p=%.4f %s edge=%+.1fpp\n",
              label, n, wins/n*100, bt$p.value, stars, (wins/n - BREAKEVEN_RATE)*100))
  invisible(bt$p.value)
}

# Regression error of a point projection vs the market's implied projection.
proj_error <- function(df, proj_col, actual_col) {
  d <- df %>% filter(!is.na(.data[[proj_col]]), !is.na(.data[[actual_col]]))
  resid <- d[[actual_col]] - d[[proj_col]]
  c(rmse = sqrt(mean(resid^2)), mae = mean(abs(resid)))
}

# Closing-line value (points): for recommended spread bets, did the market move
# toward our side between open and close? mean signed movement in points.
clv_points <- function(df, prob_col, min_edge = MIN_EDGE) {
  d <- df %>%
    filter(!is.na(.data[[prob_col]]), !is.na(spread_feature), !is.na(spread)) %>%
    mutate(prob = .data[[prob_col]], edge = abs(prob - 0.5)) %>%
    filter(edge >= min_edge) %>%
    # bet home if prob>0.5. CLV+ if closing spread moved toward the bet side.
    mutate(clv = if_else(prob > 0.5, spread - spread_feature, spread_feature - spread))
  if (nrow(d) == 0) return(NA_real_)
  mean(d$clv, na.rm = TRUE)
}
