source("R/utilities.R")
source("R/odds.R")
source("R/features.R")
assert_packages()
ensure_directories()
cfg <- read_config()

schedules <- readRDS("data/raw/schedules.rds")
team_games <- readRDS("data/raw/team_games.rds")
odds <- read_rotowire("data/raw/rotowire_games_archive.json")

games <- schedule_games(schedules, cfg$data$regular_season_only)
team_games <- add_team_context(team_games, games)
rolled <- add_rolling_features(team_games, unlist(cfg$features$rolling_windows))
features <- build_game_features(
  games,
  rolled,
  odds,
  cfg$features$minimum_prior_games
)

saveRDS(features, "data/processed/game_features.rds")

join_audit <- games |>
  join_odds(odds) |>
  dplyr::count(
    matched = !is.na(.data$home_line) & !is.na(.data$total_line),
    name = "games"
  )
readr::write_csv(join_audit, "outputs/odds_join_audit.csv")
message("Feature rows: ", nrow(features))
print(join_audit)
