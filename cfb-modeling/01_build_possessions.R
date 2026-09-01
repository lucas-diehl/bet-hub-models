# ============================================================================
# 01_build_possessions.R  —  Phase 0: possession/drive extraction + team-game PPP
# ----------------------------------------------------------------------------
# Reads cached play-by-play (data_cache/pbp_data.rds) and derives a drive-level
# table plus a team-game Points-Per-Drive (PPP) table. These are the raw
# materials for the opponent-adjusted ratings in 02_build_asof_ratings.R.
#
# Outputs (cached, small — downstream scripts never touch the 519MB pbp again):
#   data_cache/drive_data.rds       one row per (season, game_id, drive_id)
#   data_cache/team_game_ppp.rds    one row per (season, game_id, team)
#
# NOTE: deliberately does NOT library(cfbfastR) — that triggers a slow network
#       fetch. We only read local caches here.
# ============================================================================

suppressWarnings(suppressMessages({
  library(dplyr); library(tidyr); library(stringr)
}))
set.seed(42)

CACHE <- "data_cache"

# --- toggles (exposed so the bake-off can test with/without) -----------------
# Garbage-time in CFB must be defined by SCORE MARGIN + PERIOD, not win-prob:
# heavy favorites sit above wp 0.95 from the opening drive, so a wp band would
# wrongly discard their (real, high-scoring) possessions. Default OFF until the
# reconciliation check validates; the flag is always computed for optional use.
GARBAGE_FILTER <- FALSE
GARB_H2_MARGIN <- 28       # 2nd half, |margin| beyond this = garbage
GARB_Q4_MARGIN <- 21       # 4th quarter, tighter threshold
RZ_YTG <- 20               # red-zone = reached <=20 yards to goal

# OneDrive-synced files occasionally throw transient read errors — retry.
read_rds_retry <- function(path, tries = 4, wait = 2) {
  for (i in seq_len(tries)) {
    out <- tryCatch(readRDS(path), error = function(e) e)
    if (!inherits(out, "error")) return(out)
    if (i < tries) Sys.sleep(wait)
  }
  stop(sprintf("Failed to read %s after %d tries", path, tries))
}

cat("STEP 0: POSSESSION EXTRACTION\n"); cat(strrep("-", 78), "\n")

# ----------------------------------------------------------------------------
# 1. Load pbp, immediately trim to needed columns (362 -> ~20) to save memory
# ----------------------------------------------------------------------------
cat("  loading pbp (large; ~1-3 min)...\n")
pbp_full <- read_rds_retry(file.path(CACHE, "pbp_data.rds"))
need <- c("season","week","game_id","drive_id","pos_team","def_pos_team",
          "drive_is_home_offense","drive_result","drive_pts","new_drive_pts",
          "drive_start_yards_to_goal","drive_end_yards_to_goal","drive_yards",
          "yards_to_goal","wp_before","period","pos_score_diff","EPA","ppa",
          "pass","rush","down")
pbp <- pbp_full %>% select(any_of(need))
rm(pbp_full); invisible(gc())
cat(sprintf("  pbp trimmed: %d plays x %d cols\n", nrow(pbp), ncol(pbp)))

game_info <- read_rds_retry(file.path(CACHE, "game_info.rds"))
sp_ratings <- read_rds_retry(file.path(CACHE, "sp_ratings.rds"))

# ----------------------------------------------------------------------------
# 2. game_dates (scores/dates/weeks) — mirrors complete_cfb_betting_model.R
# ----------------------------------------------------------------------------
game_dates <- game_info %>%
  mutate(
    game_id = if ("game_id" %in% names(.)) game_id else if ("id" %in% names(.)) id else row_number(),
    season  = if ("season"  %in% names(.)) season  else year,
    week    = if ("week"    %in% names(.)) week    else NA_integer_
  ) %>%
  transmute(
    game_id, season, week,
    start_date = as.POSIXct(start_date, format = "%Y-%m-%dT%H:%M:%S"),
    home_team, away_team,
    home_score = as.numeric(home_points),
    away_score = as.numeric(away_points),
    total_points = home_score + away_score
  )

# ----------------------------------------------------------------------------
# 3. FBS universe per season (SP+ is computed only for FBS teams).
#    Non-FBS pos_team/def_pos_team collapse to a single "FCS" bucket so the
#    ratings model has ~130 FBS teams + 1 replacement-level entity.
# ----------------------------------------------------------------------------
sp_team_col <- intersect(c("team","school"), names(sp_ratings))[1]
fbs_by_season <- sp_ratings %>%
  transmute(season = as.integer(year), team = .data[[sp_team_col]]) %>%
  filter(!is.na(team)) %>% distinct()

fbs_set <- split(fbs_by_season$team, fbs_by_season$season)
to_fbs <- function(season, team) {
  ifelse(mapply(function(s, t) {
    st <- fbs_set[[as.character(s)]]
    !is.null(st) && t %in% st
  }, season, team), team, "FCS")
}

# ----------------------------------------------------------------------------
# 4. Drive-level table: one row per (season, game_id, drive_id)
#    drive_pts is constant within a drive; play-derived fields use min/mean.
# ----------------------------------------------------------------------------
cat("  aggregating plays -> drives...\n")
drives <- pbp %>%
  filter(!is.na(drive_id), !is.na(pos_team), !is.na(def_pos_team)) %>%
  group_by(season, game_id, drive_id) %>%
  summarise(
    week          = dplyr::first(week),
    pos_team      = dplyr::first(pos_team),
    def_pos_team  = dplyr::first(def_pos_team),
    is_home_off   = as.integer(dplyr::first(drive_is_home_offense)),
    drive_result  = dplyr::first(drive_result),
    # net points from the offense's perspective (neg = def/ST score allowed)
    net_pts       = coalesce(dplyr::first(drive_pts), dplyr::first(new_drive_pts), 0),
    start_ytg     = dplyr::first(drive_start_yards_to_goal),
    drive_yards   = dplyr::first(drive_yards),
    min_ytg       = suppressWarnings(min(yards_to_goal, na.rm = TRUE)),
    n_plays       = n(),
    mean_epa      = mean(EPA, na.rm = TRUE),
    pass_plays    = sum(pass == 1, na.rm = TRUE),
    rush_plays    = sum(rush == 1, na.rm = TRUE),
    wp_start      = dplyr::first(wp_before),
    start_period  = dplyr::first(period),
    start_margin  = dplyr::first(pos_score_diff),
    .groups = "drop"
  ) %>%
  mutate(
    min_ytg  = ifelse(is.finite(min_ytg), min_ytg, NA_real_),
    pos_team = to_fbs(season, pos_team),
    def_pos_team = to_fbs(season, def_pos_team),
    reached_rz = as.integer(!is.na(min_ytg) & min_ytg <= RZ_YTG),
    # score/period-based garbage flag (drive is "garbage" if the game is already
    # decided when it starts): 2nd half & |margin|>28, or 4th quarter & |margin|>21
    garbage    = as.integer(
      !is.na(start_period) & !is.na(start_margin) &
      ((start_period >= 3 & abs(start_margin) > GARB_H2_MARGIN) |
       (start_period >= 4 & abs(start_margin) > GARB_Q4_MARGIN))
    )
  )

# meaningful offensive possessions only
BAD_RESULTS <- c("END OF HALF","END OF GAME","END OF 4TH QUARTER",
                 "KICKOFF","Uncategorized")
meaningful <- drives %>%
  filter(!is.na(drive_result), !(drive_result %in% BAD_RESULTS))
if (GARBAGE_FILTER) meaningful <- meaningful %>% filter(garbage == 0)

cat(sprintf("  drives: %d total, %d meaningful%s\n",
            nrow(drives), nrow(meaningful),
            if (GARBAGE_FILTER) " (garbage-time removed)" else ""))

# ----------------------------------------------------------------------------
# 5. Team-game PPP: offense side (by pos_team) + defense side (by def_pos_team)
# ----------------------------------------------------------------------------
tg_off <- meaningful %>%
  group_by(season, game_id, week, team = pos_team) %>%
  summarise(
    off_drives     = n(),
    off_ppd        = mean(net_pts),
    off_pts        = sum(net_pts),
    off_pts_pos    = sum(pmax(net_pts, 0)),   # gross offensive points (for reconciliation)
    off_td_rate    = mean(net_pts >= 6),
    off_score_rate = mean(net_pts > 0),
    off_neg_rate   = mean(net_pts < 0),
    off_start_ytg  = mean(start_ytg, na.rm = TRUE),
    off_rz_trips   = sum(reached_rz),
    off_rz_ppd     = ifelse(sum(reached_rz) > 0,
                            sum(net_pts[reached_rz == 1]) / sum(reached_rz), NA_real_),
    off_epa        = mean(mean_epa, na.rm = TRUE),
    .groups = "drop"
  )

tg_def <- meaningful %>%
  group_by(season, game_id, week, team = def_pos_team) %>%
  summarise(
    def_drives     = n(),
    def_ppd        = mean(net_pts),          # points allowed per opp possession
    def_pts        = sum(net_pts),
    def_td_rate    = mean(net_pts >= 6),
    def_stop_rate  = mean(net_pts <= 0),
    def_rz_trips   = sum(reached_rz),
    def_rz_ppd     = ifelse(sum(reached_rz) > 0,
                            sum(net_pts[reached_rz == 1]) / sum(reached_rz), NA_real_),
    def_epa        = mean(mean_epa, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------------------------------------------------------
# 6. Play-level pass/rush efficiency splits (for the ML approach later)
# ----------------------------------------------------------------------------
tg_eff_off <- pbp %>%
  filter(!is.na(EPA), !is.na(pos_team)) %>%
  mutate(pos_team = to_fbs(season, pos_team)) %>%
  group_by(season, game_id, team = pos_team) %>%
  summarise(
    off_pass_epa = mean(EPA[pass == 1], na.rm = TRUE),
    off_rush_epa = mean(EPA[rush == 1], na.rm = TRUE),
    off_pass_sr  = mean(EPA[pass == 1] > 0, na.rm = TRUE),
    off_rush_sr  = mean(EPA[rush == 1] > 0, na.rm = TRUE),
    pass_rate    = mean(pass == 1, na.rm = TRUE),
    .groups = "drop"
  )
tg_eff_def <- pbp %>%
  filter(!is.na(EPA), !is.na(def_pos_team)) %>%
  mutate(def_pos_team = to_fbs(season, def_pos_team)) %>%
  group_by(season, game_id, team = def_pos_team) %>%
  summarise(
    def_pass_epa = mean(EPA[pass == 1], na.rm = TRUE),
    def_rush_epa = mean(EPA[rush == 1], na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------------------------------------------------------
# 7. Assemble team-game master + attach opponent / home-away / scores
# ----------------------------------------------------------------------------
home_long <- game_dates %>%
  transmute(season, game_id, week, team = home_team, opponent = away_team,
            is_home = 1L, team_score = home_score, opp_score = away_score, total_points)
away_long <- game_dates %>%
  transmute(season, game_id, week, team = away_team, opponent = home_team,
            is_home = 0L, team_score = away_score, opp_score = home_score, total_points)
tg_meta <- bind_rows(home_long, away_long) %>%
  mutate(team = to_fbs(season, team), opponent = to_fbs(season, opponent))

team_game_ppp <- tg_meta %>%
  left_join(tg_off %>% select(-week), by = c("season","game_id","team")) %>%
  left_join(tg_def %>% select(-week), by = c("season","game_id","team")) %>%
  left_join(tg_eff_off,               by = c("season","game_id","team")) %>%
  left_join(tg_eff_def,               by = c("season","game_id","team")) %>%
  # a team-game is usable only if it has both offensive and defensive drives
  filter(!is.na(off_drives), !is.na(def_drives)) %>%
  arrange(season, game_id, desc(is_home))

# ----------------------------------------------------------------------------
# 8. Diagnostics / sanity checks
# ----------------------------------------------------------------------------
cat("\n=== SANITY CHECKS ===\n")
cat(sprintf("  team-game rows: %d  (games: %d)\n",
            nrow(team_game_ppp), n_distinct(paste(team_game_ppp$season, team_game_ppp$game_id))))
cat(sprintf("  league mean off_ppd : %.3f  (expect ~1.9-2.2)\n", mean(team_game_ppp$off_ppd, na.rm=TRUE)))
cat(sprintf("  league mean def_ppd : %.3f  (should ~= off_ppd)\n", mean(team_game_ppp$def_ppd, na.rm=TRUE)))
cat(sprintf("  mean off_drives/tm  : %.2f  (expect ~11-13)\n", mean(team_game_ppp$off_drives, na.rm=TRUE)))

# reconcile GROSS offensive points vs final scores. Remaining gap should be
# small (~2-4 pts): defensive/special-teams TDs the team scored, which live as
# negative net_pts in the OPPONENT's drives, plus 2pt/edge cases.
recon <- team_game_ppp %>%
  filter(!is.na(team_score)) %>%
  mutate(diff = team_score - off_pts_pos)
cat(sprintf("  score vs gross off pts: mean diff %.2f, median %.2f (expect small +; = def/ST TDs scored)\n",
            mean(recon$diff, na.rm=TRUE), median(recon$diff, na.rm=TRUE)))

cat("\n  off_ppd by season:\n")
print(team_game_ppp %>% group_by(season) %>%
        summarise(mean_off_ppd = round(mean(off_ppd, na.rm=TRUE),3),
                  mean_drives  = round(mean(off_drives, na.rm=TRUE),2),
                  n = n(), .groups="drop"))

# ----------------------------------------------------------------------------
# 9. Save
# ----------------------------------------------------------------------------
saveRDS(drives,        file.path(CACHE, "drive_data.rds"))
saveRDS(team_game_ppp, file.path(CACHE, "team_game_ppp.rds"))
cat("\n✓ saved data_cache/drive_data.rds and data_cache/team_game_ppp.rds\n")
