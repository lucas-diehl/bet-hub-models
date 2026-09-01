source("R/utilities.R")
source("R/odds.R")
source("R/features.R")
assert_packages()
ensure_directories()
cfg <- read_config()

seasons <- seq(cfg$data$start_season, cfg$data$end_season)
schedules <- nflfastR::load_schedules(seasons)
saveRDS(schedules, "data/raw/schedules.rds")

team_game_files <- character(length(seasons))
for (i in seq_along(seasons)) {
  season <- seasons[i]
  destination <- file.path("data", "raw", paste0("team_games_", season, ".rds"))
  team_game_files[i] <- destination
  if (file.exists(destination)) {
    message("Using cached team-game aggregation for ", season)
    next
  }
  message("Loading and aggregating nflfastR play-by-play for ", season)
  pbp_season <- nflfastR::load_pbp(season)
  team_games_season <- summarize_team_games(
    pbp_season,
    cfg$data$regular_season_only
  )
  saveRDS(team_games_season, destination)
  rm(pbp_season, team_games_season)
  invisible(gc())
}

team_games <- purrr::map_dfr(team_game_files, readRDS)
saveRDS(team_games, "data/raw/team_games.rds")

download_rotowire(
  cfg$data$rotowire_url,
  "data/raw/rotowire_games_archive.json"
)
message("Raw inputs saved.")
