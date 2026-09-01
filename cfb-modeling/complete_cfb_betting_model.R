# ============================================================================
# CFB BETTING MODEL v3 — FULL EDGE STACK
# Walk-Forward OOS | Platt Calibrated | Elo | Sharp Money | Weather
# ============================================================================

library(cfbfastR)
library(tidyverse)
library(xgboost)
library(lubridate)
library(zoo)

# ============================================================================
# SECTION 0: CONFIGURATION
# ============================================================================

cfbd_key <- Sys.getenv("CFB_API_KEY")
if (cfbd_key == "") cfbd_key <- Sys.getenv("CFBD_API_KEY")
if (cfbd_key != "") {
  Sys.setenv(CFBD_API_KEY = cfbd_key)
  cat("✓ API key loaded\n\n")
} else {
  stop("CFB_API_KEY not found in .Renviron")
}

set.seed(42)

cat(strrep("=", 90), "\n")
cat("CFB BETTING MODEL v3 — FULL EDGE STACK\n")
cat(strrep("=", 90), "\n\n")

ALL_YEARS          <- 2019:2025
TRAIN_YEARS        <- 2019:2024
FINAL_TEST_YEAR    <- 2025
BACKTEST_YEARS     <- 2021:2025
FORCE_RELOAD_YEARS <- c(2024, 2025)

MIN_EDGE       <- 0.03
BREAKEVEN_RATE <- 0.524

# Elo configuration
ELO_INITIAL  <- 1500
ELO_HOME_ADV <- 65
ELO_K        <- 20
ELO_REGRESS  <- 0.67   # fraction of deviation from mean retained across seasons

EARLY_SEASON_WEEKS <- 1:3
MID_SEASON_WEEKS   <- 4:8
LATE_SEASON_WEEKS  <- 9:15

P4_CONFERENCES <- c("ACC", "Big Ten", "Big 12", "SEC", "Pac-12",
                     "Big Ten Conference", "Big 12 Conference")

cat(sprintf("Training  : %s\n", paste(TRAIN_YEARS, collapse = ", ")))
cat(sprintf("Backtest  : %s\n", paste(BACKTEST_YEARS, collapse = ", ")))
cat(sprintf("Min edge  : %.1f%%\n\n", MIN_EDGE * 100))

# ============================================================================
# SECTION 1: DATA COLLECTION
# ============================================================================

cat("STEP 1: DATA COLLECTION\n")
cat(strrep("-", 90), "\n\n")

dir.create("data_cache", showWarnings = FALSE)

safe_load <- function(years, data_name, load_func, cache_file = NULL) {
  needs_reload <- !is.null(cache_file) && any(years %in% FORCE_RELOAD_YEARS)
  if (!is.null(cache_file) && file.exists(cache_file) && !needs_reload) {
    cat(sprintf("  [cache] %s\n", data_name))
    return(readRDS(cache_file))
  }
  cat(sprintf("  [api]   %s", data_name))
  data_list <- list()
  failed    <- c()
  for (yr in years) {
    cat(".")
    tryCatch({
      d <- load_func(yr)
      if (!is.null(d) && nrow(d) > 0) data_list[[as.character(yr)]] <- d
      else failed <- c(failed, yr)
    }, error = function(e) { failed <<- c(failed, yr) })
  }
  cat(" done\n")
  if (length(failed) > 0)
    cat(sprintf("    (failed: %s)\n", paste(failed, collapse = ",")))
  if (length(data_list) == 0) { warning(sprintf("No data for %s", data_name)); return(NULL) }
  result <- bind_rows(data_list)
  if (!is.null(cache_file)) saveRDS(result, cache_file)
  result
}

pbp_data        <- safe_load(ALL_YEARS, "Play-by-Play",   function(yr) load_cfb_pbp(seasons = yr),                        "data_cache/pbp_data.rds")
game_info       <- safe_load(ALL_YEARS, "Game Info",       function(yr) cfbd_game_info(year = yr, season_type = "regular"), "data_cache/game_info.rds")
betting_lines   <- safe_load(ALL_YEARS, "Betting Lines",   function(yr) cfbd_betting_lines(year = yr, season_type = "regular"), "data_cache/betting_lines.rds")
recruiting_data <- safe_load(ALL_YEARS, "Recruiting",      function(yr) cfbd_recruiting_team(year = yr),                   "data_cache/recruiting.rds")
returning_prod  <- safe_load(ALL_YEARS, "Returning Prod",  function(yr) cfbd_player_returning(year = yr),                  "data_cache/returning_production.rds")
sp_ratings      <- safe_load(ALL_YEARS, "SP+ Ratings",     function(yr) cfbd_ratings_sp(year = yr),                        "data_cache/sp_ratings.rds")
coaches_data    <- safe_load(ALL_YEARS, "Coaches",         function(yr) cfbd_coaches(year = yr),                           "data_cache/coaches.rds")
team_info       <- safe_load(ALL_YEARS, "Team Info",       function(yr) cfbd_team_info(year = yr),                         "data_cache/team_info.rds")
portal_data     <- safe_load(ALL_YEARS, "Portal",          function(yr) cfbd_player_portal(year = yr),                     "data_cache/portal.rds")
weather_data    <- safe_load(ALL_YEARS, "Weather",         function(yr) tryCatch(cfbd_game_weather(year = yr), error = function(e) NULL), "data_cache/weather.rds")
adv_stats       <- safe_load(ALL_YEARS, "Advanced Stats",  function(yr) cfbd_stats_game_advanced(year = yr, season_type = "regular"), "data_cache/adv_stats.rds")

cat("\n✓ Data collection complete\n\n")
if (is.null(game_info) || nrow(game_info) == 0) stop("Missing game_info — check API key")

# ============================================================================
# SECTION 2: GAME-LEVEL EPA + SCORING + TURNOVERS
# ============================================================================

cat("STEP 2: EPA, SCORING & TURNOVERS\n")
cat(strrep("-", 90), "\n\n")

game_dates <- game_info %>%
  mutate(
    game_id    = if ("game_id" %in% names(.)) game_id else if ("id" %in% names(.)) id else row_number(),
    season     = if ("season"  %in% names(.)) season  else year,
    week       = if ("week"    %in% names(.)) week    else NA_integer_
  ) %>%
  select(game_id, season, week, start_date, home_team, away_team, home_points, away_points) %>%
  mutate(
    start_date   = as.POSIXct(start_date, format = "%Y-%m-%dT%H:%M:%S"),
    home_score   = as.numeric(home_points),
    away_score   = as.numeric(away_points),
    total_points = home_score + away_score
  )

game_offense_epa <- pbp_data %>%
  filter(!is.na(EPA)) %>%
  group_by(season, game_id, pos_team) %>%
  summarise(
    off_epa_per_play     = mean(EPA, na.rm = TRUE),
    off_success_rate     = mean(EPA > 0, na.rm = TRUE),
    off_explosiveness    = mean(EPA[EPA > 0], na.rm = TRUE),
    off_pass_epa         = mean(EPA[pass == 1], na.rm = TRUE),
    off_rush_epa         = mean(EPA[rush == 1], na.rm = TRUE),
    off_pass_success     = mean(EPA[pass == 1] > 0, na.rm = TRUE),
    off_rush_success     = mean(EPA[rush == 1] > 0, na.rm = TRUE),
    off_third_down_conv  = mean(EPA[down == 3] > 0, na.rm = TRUE),
    pass_rate            = mean(pass == 1, na.rm = TRUE),
    n_plays              = n(),
    .groups = "drop"
  ) %>%
  rename(team = pos_team)

game_defense_epa <- pbp_data %>%
  filter(!is.na(EPA)) %>%
  group_by(season, game_id, def_pos_team) %>%
  summarise(
    def_epa_per_play    = mean(EPA, na.rm = TRUE),
    def_success_rate    = mean(EPA > 0, na.rm = TRUE),
    def_pass_epa        = mean(EPA[pass == 1], na.rm = TRUE),
    def_rush_epa        = mean(EPA[rush == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(team = def_pos_team)

# Per-game scoring (for totals model)
team_scoring <- game_dates %>%
  filter(!is.na(home_score), !is.na(away_score)) %>%
  select(game_id, season, start_date, home_team, away_team, home_score, away_score) %>%
  pivot_longer(c(home_team, away_team), names_to = "side", values_to = "team") %>%
  mutate(
    pts_scored  = if_else(side == "home_team", home_score, away_score),
    pts_allowed = if_else(side == "home_team", away_score, home_score)
  ) %>%
  select(season, game_id, start_date, team, pts_scored, pts_allowed)

# Turnovers from pbp (interceptions + fumbles lost)
turnover_stats <- pbp_data %>%
  mutate(
    is_turnover = as.integer(grepl(
      "Interception|Fumble Recovery.*Opponent|Punt.*Block.*TD|interception",
      play_type, ignore.case = TRUE
    ))
  ) %>%
  group_by(season, game_id, pos_team) %>%
  summarise(turnovers_lost = sum(is_turnover, na.rm = TRUE), .groups = "drop") %>%
  rename(team = pos_team)

cat("✓ EPA, scoring, turnovers computed\n\n")

# ============================================================================
# SECTION 3: ROLLING AVERAGES (LAGGED)
# ============================================================================

cat("STEP 3: ROLLING AVERAGES\n")
cat(strrep("-", 90), "\n\n")

game_stats_full <- game_offense_epa %>%
  left_join(game_defense_epa,                                            by = c("season", "game_id", "team")) %>%
  left_join(game_dates %>% select(game_id, season, week, start_date),   by = c("season", "game_id")) %>%
  left_join(team_scoring %>% select(season, game_id, team, pts_scored, pts_allowed), by = c("season", "game_id", "team")) %>%
  left_join(turnover_stats,                                              by = c("season", "game_id", "team")) %>%
  arrange(season, team, start_date)

rolling_stats <- game_stats_full %>%
  group_by(season, team) %>%
  arrange(start_date) %>%
  mutate(
    game_num = row_number(),

    # EPA rolling
    roll3_off_epa = rollmean(off_epa_per_play, k = 3, fill = NA, align = "right"),
    roll3_def_epa = rollmean(def_epa_per_play, k = 3, fill = NA, align = "right"),
    roll3_success  = rollmean(off_success_rate, k = 3, fill = NA, align = "right"),
    roll5_off_epa  = rollmean(off_epa_per_play, k = 5, fill = NA, align = "right"),
    roll5_def_epa  = rollmean(def_epa_per_play, k = 5, fill = NA, align = "right"),

    # Scoring rolling
    roll3_pts_scored  = rollmean(coalesce(pts_scored,  28), k = 3, fill = NA, align = "right"),
    roll3_pts_allowed = rollmean(coalesce(pts_allowed, 28), k = 3, fill = NA, align = "right"),
    roll5_pts_scored  = rollmean(coalesce(pts_scored,  28), k = 5, fill = NA, align = "right"),
    roll5_pts_allowed = rollmean(coalesce(pts_allowed, 28), k = 5, fill = NA, align = "right"),

    # Turnovers rolling
    roll5_turnovers = rollmean(coalesce(turnovers_lost, 0), k = 5, fill = NA, align = "right"),

    # Season-to-date
    std_off_epa     = cummean(off_epa_per_play),
    std_def_epa     = cummean(def_epa_per_play),
    std_success     = cummean(off_success_rate),
    std_pts_scored  = cummean(coalesce(pts_scored,  28)),
    std_pts_allowed = cummean(coalesce(pts_allowed, 28)),
    std_turnovers   = cummean(coalesce(turnovers_lost, 0)),

    # LAG BY 1 — no lookahead
    roll3_off_epa_lag     = lag(roll3_off_epa,    1),
    roll3_def_epa_lag     = lag(roll3_def_epa,    1),
    roll3_success_lag     = lag(roll3_success,    1),
    roll5_off_epa_lag     = lag(roll5_off_epa,    1),
    roll5_def_epa_lag     = lag(roll5_def_epa,    1),
    std_off_epa_lag       = lag(std_off_epa,      1),
    std_def_epa_lag       = lag(std_def_epa,      1),
    roll3_pts_scored_lag  = lag(roll3_pts_scored,  1),
    roll3_pts_allowed_lag = lag(roll3_pts_allowed, 1),
    std_pts_scored_lag    = lag(std_pts_scored,    1),
    std_pts_allowed_lag   = lag(std_pts_allowed,   1),
    turnover_margin_lag   = lag(std_turnovers,     1),  # turnovers committed per game

    epa_trend = lag(roll3_off_epa, 1) - lag(roll5_off_epa, 1),

    # Lagged style metrics (no current-game leakage)
    std_pass_rate_lag     = lag(cummean(pass_rate),          1),
    std_explosiveness_lag = lag(cummean(coalesce(off_explosiveness, 0)), 1),
    std_third_down_lag    = lag(cummean(off_third_down_conv), 1),
    std_pass_success_lag  = lag(cummean(off_pass_success),   1),
    std_rush_success_lag  = lag(cummean(off_rush_success),   1)
  ) %>%
  ungroup()

cat("✓ Rolling averages calculated (1-game lag applied)\n\n")

# ============================================================================
# SECTION 4: STRENGTH OF SCHEDULE (ROLLING, LEAK-FREE)
# ============================================================================

cat("STEP 4: STRENGTH OF SCHEDULE\n")
cat(strrep("-", 90), "\n\n")

team_epa_lookup <- rolling_stats %>%
  select(season, game_id, team,
         opp_off_quality = std_off_epa_lag,
         opp_def_quality = std_def_epa_lag)

game_opponent_map <- bind_rows(
  game_dates %>% transmute(season, game_id, team = home_team, opponent = away_team),
  game_dates %>% transmute(season, game_id, team = away_team, opponent = home_team)
)

opponent_quality_by_game <- game_opponent_map %>%
  left_join(team_epa_lookup %>% rename(opponent = team), by = c("season", "game_id", "opponent")) %>%
  left_join(rolling_stats %>% select(season, game_id, team, start_date), by = c("season", "game_id", "team")) %>%
  arrange(season, team, start_date) %>%
  group_by(season, team) %>%
  mutate(
    cum_opp_off         = cummean(coalesce(opp_off_quality, 0)),
    cum_opp_def         = cummean(coalesce(opp_def_quality, 0)),
    sos_opp_off_quality = lag(cum_opp_off, 1),
    sos_opp_def_quality = lag(cum_opp_def, 1)
  ) %>%
  ungroup() %>%
  select(season, game_id, team, sos_opp_off_quality, sos_opp_def_quality)

sos_stats <- rolling_stats %>%
  left_join(opponent_quality_by_game, by = c("season", "game_id", "team")) %>%
  mutate(
    off_epa_sos_adj = coalesce(std_off_epa_lag, 0) - coalesce(sos_opp_def_quality, 0),
    def_epa_sos_adj = coalesce(std_def_epa_lag, 0) - coalesce(sos_opp_off_quality, 0),
    sos_score       = (coalesce(sos_opp_off_quality, 0) + abs(coalesce(sos_opp_def_quality, 0))) / 2
  )

cat("✓ SOS calculated (rolling, leak-free)\n\n")

# ============================================================================
# SECTION 4B: TRANSFER PORTAL
# ============================================================================

cat("STEP 4B: TRANSFER PORTAL\n")
cat(strrep("-", 90), "\n\n")

if (!is.null(portal_data) && nrow(portal_data) > 0) {
  p <- portal_data %>%
    rename_with(tolower) %>%
    mutate(
      origin      = if ("origin"      %in% names(.)) origin      else NA_character_,
      destination = if ("destination" %in% names(.)) destination else NA_character_,
      stars       = as.numeric(coalesce(
        if ("stars"  %in% names(.)) .data[["stars"]]  else NA_real_,
        if ("rating" %in% names(.)) .data[["rating"]] else NA_real_,
        2
      ))
    ) %>%
    filter(!is.na(season))

  portal_out <- p %>% filter(!is.na(origin), origin != "") %>%
    group_by(season, team = origin) %>%
    summarise(portal_out_stars = sum(stars, na.rm=TRUE), portal_out_n = n(), .groups="drop")

  portal_in <- p %>% filter(!is.na(destination), destination != "") %>%
    group_by(season, team = destination) %>%
    summarise(portal_in_stars = sum(stars, na.rm=TRUE), portal_in_n = n(), .groups="drop")

  portal_metrics <- full_join(portal_out, portal_in, by = c("season","team")) %>%
    mutate(across(where(is.numeric), ~coalesce(., 0))) %>%
    mutate(portal_net_stars = portal_in_stars - portal_out_stars,
           portal_net_n     = portal_in_n     - portal_out_n)
  cat(sprintf("✓ Portal: %d team-seasons\n\n", nrow(portal_metrics)))
} else {
  cat("  Warning: no portal data\n\n")
  portal_metrics <- tibble(season=integer(), team=character(),
                            portal_net_stars=numeric(), portal_net_n=numeric(),
                            portal_in_stars=numeric(), portal_out_stars=numeric())
}

# ============================================================================
# SECTION 4C: RETURNING QB
# ============================================================================

cat("STEP 4C: RETURNING QB\n")
cat(strrep("-", 90), "\n\n")

passer_col <- intersect(c("passer_player_name", "passer", "player_name"), names(pbp_data))[1]

if (!is.na(passer_col)) {
  top_qb <- pbp_data %>%
    filter(pass == 1, !is.na(.data[[passer_col]]), .data[[passer_col]] != "") %>%
    group_by(season, team = pos_team, qb_name = .data[[passer_col]]) %>%
    summarise(n_attempts = n(), .groups = "drop") %>%
    group_by(season, team) %>%
    slice_max(n_attempts, n = 1, with_ties = FALSE) %>%
    ungroup()

  returning_qb <- top_qb %>%
    arrange(team, season) %>%
    group_by(team) %>%
    mutate(
      prev_qb      = lag(qb_name),
      returning_qb = as.integer(!is.na(prev_qb) & qb_name == prev_qb)
    ) %>%
    ungroup() %>%
    select(season, team, returning_qb)
  cat(sprintf("✓ QB flag: %d team-seasons  (%.1f%% returning)\n\n",
              nrow(returning_qb), mean(returning_qb$returning_qb, na.rm=TRUE)*100))
} else {
  returning_qb <- tibble(season=integer(), team=character(), returning_qb=integer())
  cat("  Warning: passer column not found\n\n")
}

# ============================================================================
# SECTION 4D: ACTUAL REST DAYS + BYE WEEK
# ============================================================================

cat("STEP 4D: REST DAYS\n")
cat(strrep("-", 90), "\n\n")

rest_data <- bind_rows(
  game_dates %>% transmute(game_id, season, start_date, team = home_team),
  game_dates %>% transmute(game_id, season, start_date, team = away_team)
) %>%
  filter(!is.na(start_date)) %>%
  arrange(season, team, start_date) %>%
  group_by(season, team) %>%
  mutate(
    days_rest  = as.numeric(difftime(start_date, lag(start_date), units = "days")),
    bye_week   = as.integer(coalesce(days_rest, 14) >= 13),   # 13+ days = had a bye
    short_rest = as.integer(coalesce(days_rest, 14) <= 6)     # 6 or fewer days (Thu game)
  ) %>%
  ungroup() %>%
  select(season, game_id, team, days_rest, bye_week, short_rest)

cat(sprintf("✓ Rest days: %d team-games  (bye week rate: %.1f%%)\n\n",
            nrow(rest_data), mean(rest_data$bye_week, na.rm=TRUE)*100))

# ============================================================================
# SECTION 4E: ELO RATINGS (LEAK-FREE CROSS-SEASON TEAM QUALITY)
# ============================================================================

cat("STEP 4E: ELO RATINGS\n")
cat(strrep("-", 90), "\n\n")

# Process all completed games chronologically — Elo is updated AFTER storing
# the pre-game rating, so there is zero lookahead.
games_elo <- game_dates %>%
  filter(!is.na(home_score), !is.na(away_score)) %>%
  arrange(season, start_date) %>%
  select(game_id, season, home_team, away_team, home_score, away_score)

elo_vec   <- setNames(numeric(0), character(0))
prev_seas <- NA_integer_
elo_rows  <- vector("list", nrow(games_elo))

for (i in seq_len(nrow(games_elo))) {
  g <- games_elo[i, ]

  # Season transition: regress all teams toward mean
  if (!is.na(prev_seas) && g$season != prev_seas) {
    for (t in names(elo_vec)) {
      elo_vec[[t]] <- ELO_INITIAL + (elo_vec[[t]] - ELO_INITIAL) * ELO_REGRESS
    }
  }
  prev_seas <- g$season

  h <- if (!is.na(elo_vec[g$home_team])) elo_vec[[g$home_team]] else ELO_INITIAL
  a <- if (!is.na(elo_vec[g$away_team])) elo_vec[[g$away_team]] else ELO_INITIAL

  # Store PRE-game ratings
  elo_rows[[i]] <- data.frame(
    game_id      = g$game_id,
    home_elo_pre = h,
    away_elo_pre = a,
    elo_diff_pre = h - a + ELO_HOME_ADV
  )

  # Update based on result
  exp_h  <- 1 / (1 + 10^(-(h - a + ELO_HOME_ADV) / 400))
  actual <- if (g$home_score > g$away_score) 1.0
            else if (g$home_score < g$away_score) 0.0 else 0.5
  margin <- log(abs(g$home_score - g$away_score) + 1)

  elo_vec[[g$home_team]] <- h + ELO_K * margin * (actual - exp_h)
  elo_vec[[g$away_team]] <- a + ELO_K * margin * ((1 - actual) - (1 - exp_h))
}

elo_df <- bind_rows(elo_rows)
cat(sprintf("✓ Elo: %d games processed  (%.0f teams tracked)\n\n",
            nrow(elo_df), length(elo_vec)))

# ============================================================================
# SECTION 5: RECRUITING & TALENT
# ============================================================================

cat("STEP 5: RECRUITING & TALENT\n")
cat(strrep("-", 90), "\n\n")

recruiting_clean <- recruiting_data %>%
  select(year, team, points, rank) %>%
  rename(recruit_points = points, recruit_rank = rank) %>%
  mutate(
    season             = year + 1,
    recruit_percentile = 1 - (recruit_rank / max(recruit_rank, na.rm = TRUE))
  ) %>%
  select(-year)

if (!is.null(returning_prod) && nrow(returning_prod) > 0) {
  returning_clean <- returning_prod %>%
    mutate(
      ppa_value   = coalesce(total_ppa, percent_ppa, 0),
      usage_value = coalesce(usage, 50)
    ) %>%
    select(season, team, ppa_value, usage_value) %>%
    group_by(season, team) %>% dplyr::slice(1) %>% ungroup() %>%
    mutate(returning_production_pct = (ppa_value + usage_value) / 2)
} else {
  returning_clean <- tibble(season=integer(), team=character(),
                             ppa_value=numeric(), usage_value=numeric(),
                             returning_production_pct=numeric())
}

talent_metrics <- recruiting_clean %>%
  left_join(returning_clean, by = c("season","team")) %>%
  mutate(
    talent_composite = 0.6 * coalesce(recruit_percentile, 0.5) +
                       0.4 * coalesce(returning_production_pct / 100, 0.5),
    blue_chip_ratio  = coalesce(recruit_points, 0) / 1000
  )

cat("✓ Talent metrics processed\n\n")

# ============================================================================
# SECTION 6: COACHING STABILITY
# ============================================================================

cat("STEP 6: COACHING STABILITY\n")
cat(strrep("-", 90), "\n\n")

if (!is.null(coaches_data) && nrow(coaches_data) > 0) {
  coaching_metrics <- coaches_data %>%
    mutate(season = if ("season" %in% names(.)) season
                    else if ("year" %in% names(.)) year else 2019) %>%
    group_by(season, team = school, coach = first_name) %>%
    dplyr::slice(1) %>% ungroup() %>%
    arrange(team, season) %>%
    group_by(team) %>%
    mutate(years_at_school = row_number(), first_year_coach = as.integer(years_at_school == 1)) %>%
    ungroup() %>%
    select(season, team, years_at_school, first_year_coach)
} else {
  coaching_metrics <- tibble(season=integer(), team=character(),
                              years_at_school=numeric(), first_year_coach=numeric())
}
cat("✓ Coaching metrics calculated\n\n")

# ============================================================================
# SECTION 7: PRESEASON SP+ (PRIOR-YEAR FINAL — NO LOOKAHEAD)
# ============================================================================

cat("STEP 7: PRESEASON RANKINGS\n")
cat(strrep("-", 90), "\n\n")

# Try preseason projection (week=0) first; fall back to prior-year final.
sp_preseason_raw <- tryCatch({
  safe_load(ALL_YEARS, "SP+ preseason", function(yr) cfbd_ratings_sp(year=yr, week=0),
            "data_cache/sp_preseason.rds")
}, error = function(e) NULL)

if (!is.null(sp_preseason_raw) && nrow(sp_preseason_raw) > 0 &&
    "week" %in% names(sp_preseason_raw)) {
  sp_preseason <- sp_preseason_raw %>%
    group_by(year, team) %>% dplyr::slice(1) %>% ungroup() %>%
    select(season = year, team, sp_rating_preseason = rating, sp_rank_preseason = ranking) %>%
    mutate(sp_percentile = 1 - (sp_rank_preseason / max(sp_rank_preseason, na.rm=TRUE)))
  cat("  Using true preseason SP+ (week=0)\n")
} else if (!is.null(sp_ratings) && nrow(sp_ratings) > 0) {
  # Prior-year final SP+ as preseason proxy — shift by +1 year
  sp_preseason <- sp_ratings %>%
    group_by(year, team) %>% dplyr::slice(1) %>% ungroup() %>%
    mutate(season = year + 1) %>%
    select(season, team, sp_rating_preseason = rating, sp_rank_preseason = ranking) %>%
    mutate(sp_percentile = 1 - (sp_rank_preseason / max(sp_rank_preseason, na.rm=TRUE)))
  cat("  Using prior-year final SP+ as preseason proxy (year+1 shift)\n")
} else {
  sp_preseason <- tibble(season=integer(), team=character(),
                          sp_rating_preseason=numeric(), sp_rank_preseason=numeric(),
                          sp_percentile=numeric())
}
cat("✓ SP+ ratings processed\n\n")

# ============================================================================
# SECTION 8: SITUATIONAL FEATURES
# ============================================================================

cat("STEP 8: SITUATIONAL FEATURES\n")
cat(strrep("-", 90), "\n\n")

ti_clean <- team_info %>%
  select(school, conference, state) %>%
  distinct(school, .keep_all = TRUE)

situational_features <- game_dates %>%
  left_join(ti_clean, by = c("home_team" = "school")) %>%
  rename(home_conference = conference, home_state = state) %>%
  left_join(ti_clean %>% select(school, conference, state), by = c("away_team" = "school")) %>%
  rename(away_conference = conference, away_state = state) %>%
  mutate(
    conference_game    = as.integer(home_conference == away_conference),
    same_state_game    = as.integer(!is.na(home_state) & home_state == away_state),
    early_season       = as.integer(week %in% EARLY_SEASON_WEEKS),
    mid_season         = as.integer(week %in% MID_SEASON_WEEKS),
    late_season        = as.integer(week %in% LATE_SEASON_WEEKS),
    home_is_p4         = as.integer(coalesce(home_conference, "") %in% P4_CONFERENCES),
    away_is_p4         = as.integer(coalesce(away_conference, "") %in% P4_CONFERENCES),
    conference_tier    = home_is_p4 + away_is_p4   # 0=G5vsG5, 1=mixed, 2=P4vsP4
  ) %>%
  select(game_id, season, week,
         conference_game, same_state_game,
         early_season, mid_season, late_season,
         home_is_p4, away_is_p4, conference_tier)

cat("✓ Situational features created\n\n")

# ============================================================================
# SECTION 8B: WEATHER FEATURES
# ============================================================================

cat("STEP 8B: WEATHER FEATURES\n")
cat(strrep("-", 90), "\n\n")

if (!is.null(weather_data) && nrow(weather_data) > 0) {
  weather_cols <- names(weather_data)
  cat(sprintf("  Weather columns: %s\n", paste(head(weather_cols, 8), collapse=", ")))

  weather_features <- weather_data %>%
    mutate(
      wind_speed = as.numeric(coalesce(
        if ("wind_speed"  %in% weather_cols) .data[["wind_speed"]]  else NA_real_,
        if ("wind"        %in% weather_cols) .data[["wind"]]        else NA_real_,
        0
      )),
      precipitation = as.numeric(coalesce(
        if ("precipitation_chance" %in% weather_cols) .data[["precipitation_chance"]] else NA_real_,
        if ("precipitation"        %in% weather_cols) .data[["precipitation"]]        else NA_real_,
        0
      )),
      temperature = as.numeric(coalesce(
        if ("temperature" %in% weather_cols) .data[["temperature"]] else NA_real_,
        65
      )),
      is_dome = as.integer(
        grepl("dome|indoor|retractable", coalesce(
          if ("weather_condition" %in% weather_cols) .data[["weather_condition"]] else NA_character_,
          ""
        ), ignore.case = TRUE) |
        wind_speed == 0 & precipitation == 0 & temperature == 72  # dome proxy
      ),
      # Wind impact on scoring: >15 mph materially reduces passing
      high_wind      = as.integer(wind_speed >= 15 & !is_dome),
      very_high_wind = as.integer(wind_speed >= 25 & !is_dome),
      precip_game    = as.integer(precipitation >= 30 & !is_dome)
    ) %>%
    select(game_id, wind_speed, precipitation, temperature, is_dome,
           high_wind, very_high_wind, precip_game)

  cat(sprintf("✓ Weather: %d games with data\n\n", nrow(weather_features)))
} else {
  cat("  Note: no weather data available — totals weather features will be 0\n\n")
  weather_features <- tibble(game_id=integer(), wind_speed=numeric(),
                              precipitation=numeric(), temperature=numeric(),
                              is_dome=integer(), high_wind=integer(),
                              very_high_wind=integer(), precip_game=integer())
}

# ============================================================================
# SECTION 9: HISTORICAL ATS PERFORMANCE
# ============================================================================

cat("STEP 9: HISTORICAL ATS PERFORMANCE\n")
cat(strrep("-", 90), "\n\n")

# cfbd_betting_lines() often omits scores for recent seasons — fill from game_info
betting_clean <- betting_lines %>%
  filter(!is.na(spread)) %>%
  mutate(spread = as.numeric(spread)) %>%
  left_join(
    game_dates %>% select(game_id, gi_home_score = home_score, gi_away_score = away_score),
    by = "game_id"
  ) %>%
  mutate(
    home_score = coalesce(as.numeric(home_score), gi_home_score),
    away_score = coalesce(as.numeric(away_score), gi_away_score)
  ) %>%
  select(-gi_home_score, -gi_away_score) %>%
  filter(!is.na(home_score), !is.na(away_score)) %>%
  mutate(
    actual_margin = home_score - away_score,
    home_covered  = as.integer((actual_margin + spread) > 0),
    away_covered  = as.integer((actual_margin + spread) < 0)
  )

ats_performance <- betting_clean %>%
  select(season, game_id, home_team, away_team, home_covered, away_covered) %>%
  pivot_longer(c(home_team, away_team), names_to = "side", values_to = "team") %>%
  mutate(covered = if_else(side == "home_team", home_covered, away_covered)) %>%
  left_join(game_dates %>% select(game_id, start_date), by = "game_id") %>%
  arrange(season, team, start_date) %>%
  group_by(season, team) %>%
  mutate(ats_record_std = cummean(covered), ats_record_lag = lag(ats_record_std, 1)) %>%
  ungroup()

cat("✓ ATS performance calculated\n\n")

# ============================================================================
# SECTION 10: BUILD MODELING DATASET
# ============================================================================

cat("STEP 10: BUILDING MODELING DATASET\n")
cat(strrep("-", 90), "\n\n")

# Use consensus when available (2019-2022); average all providers otherwise (2023+)
model_data_base <- betting_clean %>%
  group_by(game_id) %>%
  mutate(has_consensus = any(provider == "consensus", na.rm = TRUE)) %>%
  filter((has_consensus & (provider == "consensus" | is.na(provider))) | !has_consensus) %>%
  summarise(
    season        = first(na.omit(season)),
    week          = first(na.omit(week)),
    home_team     = first(na.omit(home_team)),
    away_team     = first(na.omit(away_team)),
    home_score    = first(na.omit(home_score)),
    away_score    = first(na.omit(away_score)),
    actual_margin = first(na.omit(actual_margin)),
    home_covered  = first(na.omit(home_covered)),
    away_covered  = first(na.omit(away_covered)),
    spread        = mean(spread, na.rm = TRUE),           # closing — for cover calc
    spread_open   = mean(as.numeric(spread_open), na.rm = TRUE),  # opening — for model feature
    over_under    = mean(as.numeric(over_under),  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Opening spread = pre-sharp-money market view (model feature)
    spread_feature = coalesce(spread_open, spread),
    # Line movement = close - open. Negative = line moved toward home (sharp on home)
    line_movement  = spread - coalesce(spread_open, spread),
    spread_result  = actual_margin + spread,
    home_covered   = coalesce(as.integer(home_covered), as.integer(spread_result > 0)),
    total_points   = home_score + away_score,
    over_hit       = as.integer((total_points - over_under) > 0)
  ) %>%
  filter(!is.na(spread), !is.na(over_under))

cat(sprintf("Games with betting lines: %d\n", nrow(model_data_base)))

model_data_base <- model_data_base %>%
  left_join(game_dates %>% select(game_id, season, week, start_date),
            by = "game_id", suffix = c("", "_gd"), relationship = "many-to-one") %>%
  mutate(season = coalesce(season, season_gd), week = coalesce(week, week_gd)) %>%
  select(-ends_with("_gd"))

# ── Feature tables ──────────────────────────────────────────────────────────

sos_feature_cols <- c(
  "season","game_id","team","game_num",
  "roll3_off_epa_lag","roll3_def_epa_lag","roll3_success_lag",
  "roll5_off_epa_lag","roll5_def_epa_lag",
  "std_off_epa_lag","std_def_epa_lag",
  "off_epa_sos_adj","def_epa_sos_adj","sos_score","epa_trend",
  "std_explosiveness_lag","std_pass_success_lag","std_rush_success_lag",
  "std_third_down_lag","std_pass_rate_lag",
  "roll3_pts_scored_lag","roll3_pts_allowed_lag",
  "std_pts_scored_lag","std_pts_allowed_lag",
  "turnover_margin_lag"
)
sos_features_df <- sos_stats %>%
  select(any_of(sos_feature_cols)) %>%
  distinct(season, game_id, team, .keep_all = TRUE)

add_team_feats <- function(df, feat_df, game_team_col, suffix) {
  key_cols     <- c("season", "game_id", "team")
  feat_renamed <- feat_df %>% rename_with(~paste0(., suffix), -any_of(key_cols))
  df %>% left_join(feat_renamed,
                   by = setNames(key_cols, c("season","game_id",game_team_col)),
                   relationship = "many-to-one")
}

talent_season  <- talent_metrics  %>% distinct(season, team, .keep_all = TRUE)
coach_season   <- coaching_metrics %>% distinct(season, team, .keep_all = TRUE)
sp_season      <- sp_preseason    %>% distinct(season, team, .keep_all = TRUE)
portal_season  <- portal_metrics  %>% distinct(season, team, .keep_all = TRUE)
qb_season      <- returning_qb    %>% distinct(season, team, .keep_all = TRUE)
rest_season    <- rest_data       %>% distinct(season, game_id, team, .keep_all = TRUE)

model_data <- model_data_base %>%
  # Game-level rolling EPA + scoring + turnovers
  add_team_feats(sos_features_df, "home_team", "_home") %>%
  add_team_feats(sos_features_df, "away_team", "_away") %>%
  # Season-level talent
  left_join(talent_season %>% rename_with(~paste0(.,  "_home"), -c(season,team)), by=c("season","home_team"="team"), relationship="many-to-one") %>%
  left_join(talent_season %>% rename_with(~paste0(.,  "_away"), -c(season,team)), by=c("season","away_team"="team"), relationship="many-to-one") %>%
  # Coaching
  left_join(coach_season  %>% rename_with(~paste0(.,  "_home"), -c(season,team)), by=c("season","home_team"="team"), relationship="many-to-one") %>%
  left_join(coach_season  %>% rename_with(~paste0(.,  "_away"), -c(season,team)), by=c("season","away_team"="team"), relationship="many-to-one") %>%
  # SP+
  left_join(sp_season     %>% rename_with(~paste0(.,  "_home"), -c(season,team)), by=c("season","home_team"="team"), relationship="many-to-one") %>%
  left_join(sp_season     %>% rename_with(~paste0(.,  "_away"), -c(season,team)), by=c("season","away_team"="team"), relationship="many-to-one") %>%
  # Portal
  left_join(portal_season %>% rename_with(~paste0(.,  "_home"), -c(season,team)), by=c("season","home_team"="team"), relationship="many-to-one") %>%
  left_join(portal_season %>% rename_with(~paste0(.,  "_away"), -c(season,team)), by=c("season","away_team"="team"), relationship="many-to-one") %>%
  # Returning QB
  left_join(qb_season     %>% rename_with(~paste0(.,  "_home"), -c(season,team)), by=c("season","home_team"="team"), relationship="many-to-one") %>%
  left_join(qb_season     %>% rename_with(~paste0(.,  "_away"), -c(season,team)), by=c("season","away_team"="team"), relationship="many-to-one") %>%
  # Rest days (game-level)
  left_join(rest_season   %>% rename_with(~paste0(.,  "_home"), -c(season,game_id,team)), by=c("season","game_id","home_team"="team"), relationship="many-to-one") %>%
  left_join(rest_season   %>% rename_with(~paste0(.,  "_away"), -c(season,game_id,team)), by=c("season","game_id","away_team"="team"), relationship="many-to-one") %>%
  # Historical ATS
  left_join(ats_performance %>% filter(side=="home_team") %>%
              select(season,game_id,team,ats_record_lag) %>%
              distinct(season,game_id,team,.keep_all=TRUE) %>%
              rename(ats_record_lag_home=ats_record_lag),
            by=c("season","game_id","home_team"="team"), relationship="many-to-one") %>%
  left_join(ats_performance %>% filter(side=="away_team") %>%
              select(season,game_id,team,ats_record_lag) %>%
              distinct(season,game_id,team,.keep_all=TRUE) %>%
              rename(ats_record_lag_away=ats_record_lag),
            by=c("season","game_id","away_team"="team"), relationship="many-to-one") %>%
  # Elo (game-level)
  left_join(elo_df, by = "game_id", relationship = "many-to-one") %>%
  # Weather (game-level)
  left_join(weather_features, by = "game_id", relationship = "many-to-one") %>%
  # Situational
  left_join(situational_features, by=c("game_id","season","week"), relationship="many-to-one")

cat(sprintf("Dataset: %d rows × %d columns\n\n", nrow(model_data), ncol(model_data)))

# ============================================================================
# SECTION 11: DIFFERENTIAL & COMPOSITE FEATURES
# ============================================================================

cat("STEP 11: DIFFERENTIAL & COMPOSITE FEATURES\n")
cat(strrep("-", 90), "\n\n")

model_data <- model_data %>%
  mutate(
    # Best EPA estimate
    home_off_epa_r = coalesce(roll3_off_epa_lag_home, std_off_epa_lag_home, 0),
    home_def_epa_r = coalesce(roll3_def_epa_lag_home, std_def_epa_lag_home, 0),
    away_off_epa_r = coalesce(roll3_off_epa_lag_away, std_off_epa_lag_away, 0),
    away_def_epa_r = coalesce(roll3_def_epa_lag_away, std_def_epa_lag_away, 0),

    # ── SPREAD FEATURES ──────────────────────────────────────────────────────

    # Elo (cross-season team quality — strongest single prior)
    elo_diff           = coalesce(home_elo_pre - away_elo_pre, 0),

    # Sharp money: how much the line moved and in which direction
    # Negative line_movement = line moved toward home = sharp action on home
    sharp_home_signal  = as.integer(coalesce(line_movement, 0) < -1.0),
    sharp_away_signal  = as.integer(coalesce(line_movement, 0) >  1.0),
    line_move_magnitude = abs(coalesce(line_movement, 0)),

    # Rest / bye week
    home_bye_week      = coalesce(bye_week_home,   0L),
    away_bye_week      = coalesce(bye_week_away,   0L),
    bye_adv            = home_bye_week - away_bye_week,
    home_short_rest    = coalesce(short_rest_home, 0L),
    away_short_rest    = coalesce(short_rest_away, 0L),
    rest_days_diff     = coalesce(days_rest_home - days_rest_away, 0),

    # EPA differentials
    epa_diff_recent     = home_off_epa_r - away_off_epa_r,
    def_epa_diff_recent = home_def_epa_r - away_def_epa_r,
    epa_trend_diff      = coalesce(epa_trend_home - epa_trend_away, 0),
    epa_diff_sos        = coalesce(off_epa_sos_adj_home - off_epa_sos_adj_away, 0),
    def_epa_diff_sos    = coalesce(def_epa_sos_adj_home - def_epa_sos_adj_away, 0),
    sos_diff            = coalesce(sos_score_home - sos_score_away, 0),

    # Ratings differentials
    sp_rating_diff      = coalesce(sp_rating_preseason_home - sp_rating_preseason_away, 0),
    sp_percentile_diff  = coalesce(sp_percentile_home - sp_percentile_away, 0),
    talent_diff         = coalesce(talent_composite_home - talent_composite_away, 0),
    recruiting_diff     = coalesce(recruit_percentile_home - recruit_percentile_away, 0),
    returning_prod_diff = coalesce(returning_production_pct_home - returning_production_pct_away, 0),

    # Betting history
    ats_diff            = coalesce(ats_record_lag_home - ats_record_lag_away, 0),

    # Coaching
    coaching_stab_diff  = coalesce(years_at_school_home - years_at_school_away, 0),
    home_first_yr_coach = coalesce(first_year_coach_home, 0),
    away_first_yr_coach = coalesce(first_year_coach_away, 0),

    # Turnover differential
    turnover_diff       = coalesce(turnover_margin_lag_away - turnover_margin_lag_home, 0),

    # Style differentials
    success_diff        = coalesce(roll3_success_lag_home      - roll3_success_lag_away,      0),
    explosiveness_diff  = coalesce(std_explosiveness_lag_home  - std_explosiveness_lag_away,  0),
    third_down_diff     = coalesce(std_third_down_lag_home     - std_third_down_lag_away,     0),
    pass_rate_diff      = coalesce(std_pass_rate_lag_home      - std_pass_rate_lag_away,      0),

    # Portal & QB
    portal_net_diff     = coalesce(portal_net_stars_home - portal_net_stars_away, 0),
    home_portal_net     = coalesce(portal_net_stars_home, 0),
    away_portal_net     = coalesce(portal_net_stars_away, 0),
    home_returning_qb   = coalesce(returning_qb_home, 0L),
    away_returning_qb   = coalesce(returning_qb_away, 0L),
    qb_continuity_diff  = home_returning_qb - away_returning_qb,

    # Market features
    conference_tier     = coalesce(conference_tier, 1L),
    avg_game_num        = (coalesce(game_num_home, 6) + coalesce(game_num_away, 6)) / 2,

    # ── TOTALS FEATURES ──────────────────────────────────────────────────────
    home_pts_scored_r   = coalesce(roll3_pts_scored_lag_home,  std_pts_scored_lag_home,  28),
    home_pts_allowed_r  = coalesce(roll3_pts_allowed_lag_home, std_pts_allowed_lag_home, 28),
    away_pts_scored_r   = coalesce(roll3_pts_scored_lag_away,  std_pts_scored_lag_away,  28),
    away_pts_allowed_r  = coalesce(roll3_pts_allowed_lag_away, std_pts_allowed_lag_away, 28),

    expected_total_off  = home_pts_scored_r  + away_pts_scored_r,
    expected_total_def  = home_pts_allowed_r + away_pts_allowed_r,
    expected_total_avg  = (expected_total_off + expected_total_def) / 2,
    total_vs_ou         = expected_total_avg - over_under,

    total_off_epa       = home_off_epa_r + away_off_epa_r,
    total_def_epa       = home_def_epa_r + away_def_epa_r,
    pace_factor         = coalesce(std_pass_rate_lag_home + std_pass_rate_lag_away, 1),
    explosiveness_total = coalesce(std_explosiveness_lag_home + std_explosiveness_lag_away, 0),

    # Weather adjustments for totals
    wind_speed          = coalesce(wind_speed, 0),
    high_wind           = coalesce(high_wind,  0L),
    very_high_wind      = coalesce(very_high_wind, 0L),
    precip_game         = coalesce(precip_game,    0L),
    is_dome             = coalesce(is_dome,         0L),
    # Wind-adjusted expected total: every 5 mph above 15 cuts ~1 point
    wind_scoring_penalty = pmax(0, (wind_speed - 10) / 5),
    weather_adj_total   = expected_total_avg - wind_scoring_penalty * high_wind

  ) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

cat(sprintf("Final dataset: %d games × %d features\n\n", nrow(model_data), ncol(model_data)))
saveRDS(model_data, "data_cache/model_data_v3.rds")

# ============================================================================
# SECTION 12: FEATURE SETS
# ============================================================================

cat("STEP 12: FEATURE SETS\n")
cat(strrep("-", 90), "\n\n")

# Lean feature sets — fewer features reduces overfitting with small training sets.
# Core: the 9 signals with the most independent information.
core_feats <- c(
  "spread_feature",      # opening market (strong prior)
  "elo_diff",            # cross-season quality (leak-free)
  "line_movement",       # sharp money direction
  "sharp_home_signal", "sharp_away_signal",
  "sp_rating_diff",      # prior-year SP+ differential
  "ats_diff",            # historical cover tendency
  "bye_adv",             # rest/bye week advantage
  "conference_tier"      # P4 vs G5 market efficiency
)

# Early: priors dominate because sample is tiny
early_spread_feats <- c(
  core_feats,
  "talent_diff", "recruiting_diff",
  "epa_diff_recent", "qb_continuity_diff"
)

# Mid: blend priors + recent form
mid_spread_feats <- c(
  core_feats,
  "epa_diff_recent", "def_epa_diff_recent",
  "epa_diff_sos", "talent_diff", "turnover_diff"
)

# Late: in-season signal strongest
late_spread_feats <- c(
  core_feats,
  "epa_diff_recent", "def_epa_diff_recent", "epa_trend_diff",
  "epa_diff_sos", "success_diff", "turnover_diff"
)

# Totals: scoring pace + market prior
totals_feats <- c(
  "over_under",
  "expected_total_avg",
  "total_vs_ou",
  "elo_diff",
  "spread_feature",
  "conference_tier",
  "pace_factor",
  "sp_rating_diff"
)

early_spread_feats <- intersect(early_spread_feats, names(model_data))
mid_spread_feats   <- intersect(mid_spread_feats,   names(model_data))
late_spread_feats  <- intersect(late_spread_feats,  names(model_data))
totals_feats       <- intersect(totals_feats,        names(model_data))

cat(sprintf("Early: %d | Mid: %d | Late: %d | Totals: %d features\n\n",
            length(early_spread_feats), length(mid_spread_feats),
            length(late_spread_feats),  length(totals_feats)))

# ============================================================================
# SECTION 13: MODEL HELPERS WITH PLATT CALIBRATION
# ============================================================================

cat("STEP 13: MODEL HELPERS\n")
cat(strrep("-", 90), "\n\n")

xgb_params <- list(
  objective        = "binary:logistic",
  eval_metric      = "logloss",
  max_depth        = 3,    # shallow trees — less memorisation
  eta              = 0.05,
  subsample        = 0.7,
  colsample_bytree = 0.6,
  min_child_weight = 15,   # requires more data per leaf — strong anti-overfit
  gamma            = 1.0   # minimum loss reduction to split
)

# Train XGBoost + Platt scaling calibration
# Returns a list(model, cal_model) so predictions are calibrated probabilities
train_xgb <- function(df, feats, target, nrounds = 100, min_rows = 50) {
  d <- df %>% select(all_of(c(feats, target))) %>% drop_na()
  if (nrow(d) < min_rows) return(NULL)

  dmat <- xgb.DMatrix(
    data  = as.matrix(d %>% select(all_of(feats))),
    label = d[[target]]
  )

  model <- xgb.train(params = xgb_params, data = dmat, nrounds = nrounds, verbose = 0)

  # Platt scaling: fit logistic regression on in-sample raw outputs -> actuals
  # Simple 2-parameter fit; does not meaningfully overfit
  raw_probs <- predict(model, dmat)
  cal_df    <- data.frame(raw = raw_probs, outcome = d[[target]])
  cal_model <- tryCatch(
    glm(outcome ~ raw, data = cal_df, family = binomial()),
    error = function(e) NULL
  )

  list(model = model, cal_model = cal_model, feats = feats)
}

# Predict with calibration applied
predict_xgb <- function(model_obj, df, feats) {
  if (is.null(model_obj)) return(rep(NA_real_, nrow(df)))

  mat <- df %>%
    select(all_of(feats)) %>%
    mutate(across(everything(), ~replace_na(., 0))) %>%
    as.matrix()

  raw_probs <- predict(model_obj$model, xgb.DMatrix(data = mat))

  if (!is.null(model_obj$cal_model)) {
    return(predict(model_obj$cal_model,
                   newdata = data.frame(raw = raw_probs),
                   type    = "response"))
  }
  raw_probs
}

add_week_period <- function(df) {
  df %>% mutate(week_period = case_when(
    early_season == 1 ~ "early",
    mid_season   == 1 ~ "mid",
    late_season  == 1 ~ "late",
    TRUE             ~ "mid"
  ))
}

train_suite <- function(train_df) {
  train_df <- add_week_period(train_df)
  list(
    spread_early = train_xgb(train_df %>% filter(week_period == "early"), early_spread_feats, "home_covered"),
    spread_mid   = train_xgb(train_df %>% filter(week_period == "mid"),   mid_spread_feats,   "home_covered"),
    spread_late  = train_xgb(train_df %>% filter(week_period == "late"),  late_spread_feats,  "home_covered"),
    totals       = train_xgb(train_df,                                    totals_feats,        "over_hit")
  )
}

predict_suite <- function(models, test_df) {
  test_df <- add_week_period(test_df) %>% mutate(.rid = row_number())

  spread_rows <- bind_rows(
    test_df %>% filter(week_period == "early") %>%
      mutate(spread_prob = predict_xgb(models$spread_early, ., early_spread_feats)),
    test_df %>% filter(week_period == "mid") %>%
      mutate(spread_prob = predict_xgb(models$spread_mid,   ., mid_spread_feats)),
    test_df %>% filter(week_period == "late") %>%
      mutate(spread_prob = predict_xgb(models$spread_late,  ., late_spread_feats))
  ) %>% arrange(.rid) %>% select(.rid, spread_prob)

  test_df %>%
    left_join(spread_rows, by = ".rid") %>%
    mutate(totals_prob = predict_xgb(models$totals, ., totals_feats)) %>%
    select(-.rid)
}

cat("✓ Helpers ready (Platt calibration enabled)\n\n")

# ============================================================================
# SECTION 14: WALK-FORWARD BACKTEST
# ============================================================================

cat("STEP 14: WALK-FORWARD BACKTEST\n")
cat(strrep("-", 90), "\n\n")

backtest_list <- list()
for (test_yr in BACKTEST_YEARS) {
  train_yrs <- 2019:(test_yr - 1)
  bt_train  <- model_data %>% filter(season %in% train_yrs, avg_game_num >= 2, !is.na(home_covered))
  bt_test   <- model_data %>% filter(season == test_yr,     avg_game_num >= 2, !is.na(home_covered))
  if (nrow(bt_test) == 0) { cat(sprintf("  %d: no test data\n", test_yr)); next }
  cat(sprintf("  %d  train=%d  test=%d ...", test_yr, nrow(bt_train), nrow(bt_test)))
  m_yr   <- train_suite(bt_train)
  res_yr <- predict_suite(m_yr, bt_test)
  backtest_list[[as.character(test_yr)]] <- res_yr %>%
    select(game_id, season, week, week_period, start_date,
           home_team, away_team, spread, spread_feature, over_under,
           line_movement, sharp_home_signal, sharp_away_signal,
           home_covered, over_hit,
           spread_prob, totals_prob,
           actual_margin, home_score, away_score, total_points,
           conference_tier, bye_adv, elo_diff)
  cat(" done\n")
}

backtest_all <- bind_rows(backtest_list)
cat(sprintf("\n✓ Backtest: %d games across %d seasons\n\n",
            nrow(backtest_all), length(backtest_list)))

# ============================================================================
# SECTION 15: PERFORMANCE ANALYSIS
# ============================================================================

cat("STEP 15: PERFORMANCE ANALYSIS\n")
cat(strrep("=", 90), "\n\n")

summarize_bets <- function(df, prob_col, result_col, label) {
  d <- df %>%
    filter(!is.na(.data[[prob_col]]), !is.na(.data[[result_col]])) %>%
    mutate(
      prob      = .data[[prob_col]],
      result    = .data[[result_col]],
      correct   = as.integer((prob > 0.5 & result == 1) | (prob <= 0.5 & result == 0)),
      edge      = abs(prob - 0.5),
      ev        = prob * 0.909 - (1 - prob),
      recommend = edge >= MIN_EDGE & ev > 0
    )
  rec    <- d %>% filter(recommend)
  wins   <- sum(rec$correct)
  losses <- nrow(rec) - wins
  profit <- wins * 0.909 - losses
  roi    <- if (nrow(rec) > 0) profit / nrow(rec) * 100 else NA_real_
  cat(sprintf(
    "  %-42s  all=%4d (%.1f%%)  |  rec=%4d  %3d-%3d  win=%.1f%%  ROI=%+.1f%%  profit=%+.1f u\n",
    label, nrow(d), mean(d$correct)*100,
    nrow(rec), wins, losses,
    if(nrow(rec)>0) wins/nrow(rec)*100 else 0,
    coalesce(roi, 0), profit))
  invisible(rec)
}

cat("SPREAD — YEAR BY YEAR:\n"); cat(strrep("-", 90), "\n")
for (yr in BACKTEST_YEARS) {
  d <- backtest_all %>% filter(season == yr)
  if (nrow(d) > 0) summarize_bets(d, "spread_prob", "home_covered", sprintf("%d Spread", yr))
}
cat(strrep("-", 90), "\n")
spread_rec <- summarize_bets(backtest_all, "spread_prob", "home_covered", "SPREAD TOTAL")
cat("\n")

cat("TOTALS — YEAR BY YEAR:\n"); cat(strrep("-", 90), "\n")
for (yr in BACKTEST_YEARS) {
  d <- backtest_all %>% filter(season == yr)
  if (nrow(d) > 0) summarize_bets(d, "totals_prob", "over_hit", sprintf("%d Totals", yr))
}
cat(strrep("-", 90), "\n")
totals_rec <- summarize_bets(backtest_all, "totals_prob", "over_hit", "TOTALS TOTAL")
cat("\n")

write.csv(backtest_all, "backtest_results.csv", row.names = FALSE)
cat("✓ Saved: backtest_results.csv\n\n")

# ============================================================================
# SECTION 15B: CALIBRATION + SIGNIFICANCE
# ============================================================================

cat("STEP 15B: CALIBRATION & SIGNIFICANCE\n")
cat(strrep("=", 90), "\n\n")

run_calibration <- function(df, prob_col, result_col, label) {
  d <- df %>%
    filter(!is.na(.data[[prob_col]]), !is.na(.data[[result_col]])) %>%
    mutate(
      prob      = .data[[prob_col]],
      result    = .data[[result_col]],
      prob_fold = if_else(prob >= 0.5, prob, 1 - prob),
      bucket    = cut(prob_fold, breaks=seq(0.45, 0.80, by=0.05),
                      include.lowest=TRUE, right=FALSE),
      correct   = as.integer((prob>0.5 & result==1)|(prob<=0.5 & result==0))
    )
  brier <- mean((d$prob - d$result)^2)
  cat(sprintf("%s calibration  (Brier: %.4f)\n", label, brier))
  d %>% filter(!is.na(bucket)) %>% group_by(bucket) %>%
    summarise(bets=n(), avg_pred=mean(prob_fold), actual=mean(correct), .groups="drop") %>%
    arrange(bucket) %>%
    mutate(diff = actual - avg_pred) %>%
    rowwise() %>%
    { for (i in seq_len(nrow(.))) {
        r <- .[i,]
        bar <- if(r$diff>=0) paste0("+", strrep("|", min(round(abs(r$diff)*200),20)))
               else          paste0("-", strrep("|", min(round(abs(r$diff)*200),20)))
        cat(sprintf("  %-14s %4d  pred=%.3f  actual=%.1f%%  %+.1f%%  %s\n",
                    as.character(r$bucket), r$bets, r$avg_pred, r$actual*100, r$diff*100, bar))
      }; . } %>% invisible()
  cat("\n")
}

sig_test <- function(df, prob_col, result_col, label) {
  rec <- df %>%
    filter(!is.na(.data[[prob_col]]), !is.na(.data[[result_col]])) %>%
    mutate(prob=.data[[prob_col]], result=.data[[result_col]],
           correct=as.integer((prob>0.5&result==1)|(prob<=0.5&result==0))) %>%
    filter(abs(prob-0.5)>=MIN_EDGE, prob*0.909-(1-prob)>0)
  n    <- nrow(rec)
  wins <- sum(rec$correct)
  if (n < 10) { cat(sprintf("%s: too few bets\n\n", label)); return(invisible(NULL)) }
  bt <- binom.test(wins, n, p=BREAKEVEN_RATE, alternative="greater")
  cat(sprintf("%s  n=%d  win=%.1f%%  p=%s  95%%CI_lower=%.1f%%  edge=%+.1f pp\n\n",
              label, n, wins/n*100,
              if(bt$p.value<0.01)"<0.001 ***" else if(bt$p.value<0.05) sprintf("%.4f **",bt$p.value)
              else if(bt$p.value<0.1) sprintf("%.4f *",bt$p.value) else sprintf("%.4f",bt$p.value),
              bt$conf.int[1]*100, (wins/n - BREAKEVEN_RATE)*100))
}

run_calibration(backtest_all, "spread_prob", "home_covered", "SPREAD")
run_calibration(backtest_all, "totals_prob", "over_hit",     "TOTALS")
sig_test(backtest_all, "spread_prob", "home_covered", "SPREAD significance")
sig_test(backtest_all, "totals_prob", "over_hit",     "TOTALS significance")

# Sharp-confirmed bets only
cat("SPREAD — SHARP MONEY CONFIRMED ONLY:\n")
cat(strrep("-", 90), "\n")
backtest_all %>%
  filter(!is.na(spread_prob), !is.na(home_covered)) %>%
  mutate(
    correct   = as.integer((spread_prob>0.5&home_covered==1)|(spread_prob<=0.5&home_covered==0)),
    edge      = abs(spread_prob-0.5),
    ev        = spread_prob*0.909-(1-spread_prob),
    # Sharp confirmation: model agrees with direction of line movement
    model_likes_home = spread_prob > 0.5,
    sharp_confirmed  = (model_likes_home & sharp_home_signal==1) |
                       (!model_likes_home & sharp_away_signal==1)
  ) %>%
  filter(edge >= MIN_EDGE, ev > 0) %>%
  group_by(sharp_confirmed) %>%
  summarise(
    bets    = n(),
    wins    = sum(correct),
    win_pct = round(wins/bets*100, 1),
    profit  = round(wins*0.909 - (bets-wins), 1),
    roi     = round(profit/bets*100, 1),
    .groups = "drop"
  ) %>%
  print()
cat("\n")

# ============================================================================
# SECTION 16: FINAL PRODUCTION MODELS
# ============================================================================

cat("STEP 16: TRAINING FINAL PRODUCTION MODELS\n")
cat(strrep("-", 90), "\n\n")

final_train <- model_data %>%
  filter(season %in% TRAIN_YEARS, avg_game_num >= 2, !is.na(home_covered))
cat(sprintf("Training on %d games (%d–%d)...\n",
            nrow(final_train), min(TRAIN_YEARS), max(TRAIN_YEARS)))

final_models <- train_suite(final_train)

if (!is.null(final_models$spread_early)) invisible(xgb.save(final_models$spread_early$model, "model_spread_early.bin"))
if (!is.null(final_models$spread_mid))   invisible(xgb.save(final_models$spread_mid$model,   "model_spread_mid.bin"))
if (!is.null(final_models$spread_late))  invisible(xgb.save(final_models$spread_late$model,  "model_spread_late.bin"))
if (!is.null(final_models$totals))       invisible(xgb.save(final_models$totals$model,       "model_totals.bin"))
saveRDS(final_models, "final_models.rds")
cat("✓ Models saved\n\n")

# ============================================================================
# SECTION 17: 2025 SEASON EVALUATION
# ============================================================================

cat("STEP 17: 2025 SEASON\n")
cat(strrep("-", 90), "\n\n")

test_2025 <- model_data %>% filter(season == FINAL_TEST_YEAR, avg_game_num >= 2)

if (nrow(test_2025) == 0) {
  cat("No 2025 data available.\n\n")
} else {
  preds_2025 <- predict_suite(final_models, test_2025) %>%
    mutate(
      spread_pick  = if_else(spread_prob > 0.5, home_team, away_team),
      spread_edge  = abs(spread_prob - 0.5),
      spread_ev    = spread_prob * 0.909 - (1 - spread_prob),
      model_likes_home = spread_prob > 0.5,
      sharp_confirmed  = (model_likes_home & sharp_home_signal==1) |
                         (!model_likes_home & sharp_away_signal==1),
      # Recommended: model edge + sharp confirmation preferred
      spread_bet   = spread_edge >= MIN_EDGE & spread_ev > 0,
      spread_bet_sharp = spread_bet & sharp_confirmed,

      totals_pick  = if_else(totals_prob > 0.5, "OVER", "UNDER"),
      totals_edge  = abs(totals_prob - 0.5),
      totals_ev    = totals_prob * 0.909 - (1 - totals_prob),
      totals_bet   = totals_edge >= MIN_EDGE & totals_ev > 0
    )

  known <- preds_2025 %>% filter(!is.na(home_covered))
  if (nrow(known) > 0) {
    cat("2025 ALL BETS:\n"); cat(strrep("-", 90), "\n")
    summarize_bets(known, "spread_prob", "home_covered", sprintf("2025 Spread      (n=%d)", nrow(known)))
    summarize_bets(known, "totals_prob", "over_hit",     sprintf("2025 Totals      (n=%d)", nrow(known)))

    cat("\n2025 SHARP-CONFIRMED SPREAD BETS ONLY:\n"); cat(strrep("-", 90), "\n")
    known_sharp <- known %>%
      filter(spread_bet_sharp) %>%
      mutate(spread_prob_col = spread_prob, home_covered_col = home_covered)
    if (nrow(known_sharp) > 0) {
      summarize_bets(known_sharp, "spread_prob", "home_covered",
                     sprintf("2025 Sharp spread (n=%d)", nrow(known_sharp)))
    }
    cat("\n")
  }

  output_cols <- c("game_id","season","week","home_team","away_team","spread","spread_feature",
                   "over_under","line_movement","sharp_home_signal","sharp_away_signal",
                   "elo_diff","bye_adv","conference_tier",
                   "spread_prob","spread_pick","spread_edge","spread_ev",
                   "spread_bet","spread_bet_sharp","sharp_confirmed",
                   "totals_prob","totals_pick","totals_edge","totals_ev","totals_bet",
                   "home_covered","over_hit","actual_margin","total_points")
  write.csv(preds_2025 %>% select(any_of(output_cols)), "predictions_2025.csv", row.names=FALSE)

  cat(sprintf("✓ predictions_2025.csv  (%d games | %d spread | %d sharp spread | %d totals)\n\n",
              nrow(preds_2025),
              sum(preds_2025$spread_bet, na.rm=TRUE),
              sum(preds_2025$spread_bet_sharp, na.rm=TRUE),
              sum(preds_2025$totals_bet, na.rm=TRUE)))
}

cat(strrep("=", 90), "\n")
cat("MODEL v3 COMPLETE\n")
cat(strrep("=", 90), "\n\n")
cat("New features vs v2: Elo, sharp money, actual rest days, turnover differential,\n")
cat("  weather, Platt calibration, conference tier\n")
cat("Key output: predictions_2025.csv (spread_bet_sharp = highest-confidence picks)\n\n")
