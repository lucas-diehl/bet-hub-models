source("R/utilities.R")
source("R/odds_api.R")  # nfl_kickoff_utc(), used to find the target week
source("R/sleeper_api.R")
source("R/dfs_value_backtest.R")  # dfs_player_name_key(), used only as a
                                  # fallback when a Sleeper player has no
                                  # gsis_id (mostly true rookies) - see below.
assert_packages()
ensure_directories()

# Weekly waiver-wire pickup list, scored against each of a Sleeper user's
# real leagues rather than a generic depth-based proxy.
#
# The method this is built on is validated: scripts/47_streaming_validation.R
# shows the weekly model beats "best season-to-date scorer" as a streaming
# pick, with the interval excluding zero for the positions tested. That
# validation used a synthetic waiver pool (everyone outside a fixed
# rostered-depth cutoff). This script replaces the synthetic cutoff with real
# roster ownership pulled from Sleeper, and replaces the assumed full-PPR
# scoring with each league's actual scoring_settings - the ranking itself
# (highest model-projected points among available players) is unchanged.
#
#   & $rscript scripts/75_sleeper_waiver_pickups.R --username=<name>
#   & $rscript scripts/75_sleeper_waiver_pickups.R --username=<name> --week=2
#   & $rscript scripts/75_sleeper_waiver_pickups.R --username=<name> --top=15

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", flag, "="), "", hit[[1]])
}

username <- arg_value("--username")
if (is.null(username)) {
  stop("Usage: --username=<sleeper username> [--season=2026] [--week=N] [--top=10]",
       call. = FALSE)
}
season <- arg_value("--season", "2026")
top_n <- as.integer(arg_value("--top", "10"))

# --------------------------------------------------------------------------
# Which week. Default: the earliest week with a game still ahead of it -
# i.e. this week, whether or not it has already started, never a week that's
# fully in the past.
# --------------------------------------------------------------------------

target_week <- as.integer(arg_value("--week", NA))
if (is.na(target_week)) {
  schedule_path <- if (file.exists("data/raw/schedules_2026.rds")) {
    "data/raw/schedules_2026.rds"
  } else {
    "data/raw/schedules.rds"
  }
  sched <- readRDS(schedule_path) |>
    dplyr::filter(.data$season == as.integer(season), .data$game_type == "REG")
  now_utc <- lubridate::now(tzone = "UTC")
  upcoming <- sched |>
    dplyr::mutate(
      kickoff = lubridate::ymd_hms(
        nfl_kickoff_utc(.data$gameday, .data$gametime), tz = "UTC"
      )
    ) |>
    dplyr::filter(.data$kickoff >= now_utc)
  target_week <- if (nrow(upcoming)) min(upcoming$week) else max(sched$week)
}
cat("Season:", season, " Target week:", target_week, "\n")

proj_path <- sprintf("outputs/fantasy_prop_%s_week%d_projections.csv",
                     season, target_week)
long_path <- sprintf("outputs/fantasy_prop_%s_week%d_long.csv",
                     season, target_week)
if (!file.exists(proj_path) || !file.exists(long_path)) {
  stop(
    "No projections for week ", target_week, " (looked for ", proj_path,
    "). Run scripts/17_build_fantasy_prop_models.R for this week first.",
    call. = FALSE
  )
}
wide <- readr::read_csv(proj_path, show_col_types = FALSE)
long <- readr::read_csv(long_path, show_col_types = FALSE)

cat("Player-weeks projected this week:", nrow(wide), "\n\n")

# --------------------------------------------------------------------------
# Sleeper: who is the user, what leagues, what's on each roster
# --------------------------------------------------------------------------

user <- sleeper_user_id(username)
cat("Sleeper user:", user$display_name, "(", user$user_id, ")\n")

leagues <- sleeper_user_leagues(user$user_id, season)
if (!nrow(leagues)) {
  stop("No ", season, " NFL leagues found for '", username, "'.", call. = FALSE)
}
cat("Leagues found:", nrow(leagues), "-",
    paste(leagues$league_name, collapse = ", "), "\n\n")

directory <- sleeper_player_directory()
cat("Sleeper player directory:", nrow(directory), "entries,",
    sum(!is.na(directory$player_id)), "with a GSIS id.\n")

# For the sliver of players with no gsis_id in Sleeper's feed (overwhelmingly
# incoming rookies who haven't been cross-referenced yet), fall back to a
# name+position match against this week's own projection roster rather than
# leaving them unmatched. This can only ever ADD an "unavailable" player to
# the rostered set (never remove one), so a bad fallback match makes a real
# waiver-eligible player look falsely owned - conservative in the direction
# that matters, i.e. it costs a suggestion rather than risks recommending a
# player who's actually rostered.
directory <- directory |>
  dplyr::mutate(name_key = dfs_player_name_key(.data$full_name))
name_fallback <- wide |>
  dplyr::transmute(
    player_id, name_key = dfs_player_name_key(.data$player), position
  ) |>
  dplyr::distinct()

# --------------------------------------------------------------------------
# Scoring: translate each league's scoring_settings onto the model's own
# component predictions (outputs/..._long.csv), rather than assuming full PPR.
# --------------------------------------------------------------------------

# Sleeper stat key -> this project's target name. Two-point conversions are
# split three ways in Sleeper's schema (pass/rush/rec) but the model has one
# aggregate component, so all three map onto it and their point values are
# averaged (they're equal in every league seen so far; if a league ever set
# them unequally this understates precision on a rare component, not the
# ranking - two-point conversions are a small fraction of total points).
sleeper_stat_map <- c(
  pass_yd = "passing_yards", pass_td = "passing_tds", pass_int = "interceptions",
  rush_yd = "rushing_yards", rush_td = "rushing_tds",
  rec = "receptions", rec_yd = "receiving_yards", rec_td = "receiving_tds",
  fum_lost = "fumbles_lost"
)
two_pt_keys <- c("pass_2pt", "rush_2pt", "rec_2pt")

# Yardage/reception bonus thresholds (bonus_rush_yd_100, bonus_pass_yd_300,
# etc.) need a probability of crossing the threshold, not an expected value,
# to score correctly - not supported here. Flagged per-league below rather
# than silently under-scoring a league that uses them.
unsupported_keys <- function(scoring_settings) {
  names(scoring_settings)[stringr::str_detect(names(scoring_settings), "^bonus_")]
}

league_points_per_target <- function(scoring_settings) {
  pts <- setNames(rep(0, length(sleeper_stat_map)), unname(sleeper_stat_map))
  for (key in names(sleeper_stat_map)) {
    if (!is.null(scoring_settings[[key]])) {
      target <- sleeper_stat_map[[key]]
      pts[[target]] <- pts[[target]] + as.numeric(scoring_settings[[key]])
    }
  }
  two_pt_vals <- unlist(scoring_settings[intersect(two_pt_keys, names(scoring_settings))])
  two_pt_pts <- if (length(two_pt_vals)) mean(as.numeric(two_pt_vals)) else 0
  c(pts, two_point_conversions = two_pt_pts)
}

score_players_for_league <- function(long_df, points_per_target) {
  long_df |>
    dplyr::filter(.data$target %in% names(points_per_target)) |>
    dplyr::mutate(pts = .data$prediction * points_per_target[.data$target]) |>
    dplyr::group_by(.data$player_id) |>
    dplyr::summarise(league_projected_points = sum(.data$pts), .groups = "drop")
}

# --------------------------------------------------------------------------
# Per-league waiver board
# --------------------------------------------------------------------------

all_boards <- list()

for (i in seq_len(nrow(leagues))) {
  lg <- leagues[i, ]
  cat("\n---", lg$league_name, "---\n")

  settings <- sleeper_league_settings(lg$league_id)
  rosters <- sleeper_league_rosters(lg$league_id)
  cat("Rostered slots (active+taxi+IR):", nrow(rosters), "\n")

  bad_keys <- unsupported_keys(settings$scoring_settings)
  if (length(bad_keys)) {
    cat("Note: this league scores", paste(bad_keys, collapse = ", "),
        "(yardage/reception bonus thresholds) - not applied below;",
        "points shown are a floor, not the exact league total.\n")
  }

  rostered_sleeper_ids <- sleeper_rostered_ids(rosters)
  rostered_gsis <- directory |>
    dplyr::filter(.data$sleeper_player_id %in% rostered_sleeper_ids) |>
    dplyr::pull(.data$player_id) |>
    stats::na.omit() |>
    as.character()

  # Fallback for rostered players with no gsis_id in Sleeper's directory:
  # match by name+position within the rostered Sleeper rows themselves.
  # rostered_no_gsis's own player_id is entirely NA (that's why it's in this
  # fallback branch) but dropping it matters, not just tidying it: joining two
  # frames that both carry a same-named non-key column doesn't overwrite one
  # with the other, it silently suffixes both to player_id.x/player_id.y, and
  # the plain `player_id` pull() below would find neither.
  rostered_no_gsis <- directory |>
    dplyr::filter(.data$sleeper_player_id %in% rostered_sleeper_ids,
                  is.na(.data$player_id)) |>
    dplyr::select(-"player_id")
  rostered_gsis_fallback <- rostered_no_gsis |>
    dplyr::inner_join(name_fallback, by = c("name_key", "position")) |>
    dplyr::pull(.data$player_id)

  rostered_all <- union(rostered_gsis, rostered_gsis_fallback)
  cat("Rostered players matched to a GSIS id:", length(rostered_all),
      "of", length(rostered_sleeper_ids), "roster slots.\n")

  points_per_target <- league_points_per_target(settings$scoring_settings)
  league_scores <- score_players_for_league(long, points_per_target)

  available <- wide |>
    dplyr::filter(!.data$player_id %in% rostered_all) |>
    dplyr::left_join(league_scores, by = "player_id") |>
    dplyr::filter(is.finite(.data$league_projected_points)) |>
    dplyr::select(
      "player_id", "player", "position", "team", "opponent_team",
      "total_line", "team_spread", "implied_team_total",
      "league_projected_points", "projected_ppr",
      "projection_status", "role_status"
    )

  top_by_position <- available |>
    dplyr::group_by(.data$position) |>
    dplyr::slice_max(.data$league_projected_points, n = top_n,
                     with_ties = FALSE) |>
    dplyr::mutate(position_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$position, .data$position_rank) |>
    dplyr::mutate(
      league_id = lg$league_id, league_name = lg$league_name,
      season = as.integer(season), week = target_week
    )

  cat("Available candidates scored:", nrow(available), "| Top",
      top_n, "per position kept:", nrow(top_by_position), "\n")

  all_boards[[lg$league_id]] <- top_by_position
}

combined <- dplyr::bind_rows(all_boards) |>
  dplyr::select(
    "league_id", "league_name", "season", "week",
    "position", "position_rank", "player", "player_id", "team",
    "opponent_team", "total_line", "team_spread", "implied_team_total",
    "league_projected_points", "projected_ppr",
    "projection_status", "role_status"
  )

out_csv <- sprintf("outputs/sleeper_waiver_pickups_%s_week%d.csv", season, target_week)
readr::write_csv(combined, out_csv)
jsonlite::write_json(
  combined, sub("\\.csv$", ".json", out_csv),
  auto_unbox = TRUE, pretty = TRUE, na = "null"
)
readr::write_csv(combined, "outputs/sleeper_waiver_pickups_latest.csv")

cat("\nWrote", nrow(combined), "rows to", out_csv, "\n")
cat("\n=== Top 5 overall by league, for a quick look ===\n")
print(as.data.frame(
  combined |>
    dplyr::group_by(.data$league_name) |>
    dplyr::slice_max(.data$league_projected_points, n = 5, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select("league_name", "position", "player", "team",
                  "league_projected_points")
), digits = 3)
