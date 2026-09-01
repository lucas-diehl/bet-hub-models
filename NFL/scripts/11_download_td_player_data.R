source("R/utilities.R")
assert_packages()
ensure_directories()

seasons <- 2021:2025
destination <- "data/raw/player_stats_2021_2025.rds"

player_stats <- nflreadr::load_player_stats(
  seasons = seasons,
  summary_level = "week"
)
saveRDS(player_stats, destination)
players <- nflreadr::load_players()
saveRDS(players, "data/raw/players.rds")

cat("Player-week rows:", nrow(player_stats), "\n")
cat("Seasons:", paste(sort(unique(player_stats$season)), collapse = ", "), "\n")
cat("Saved:", destination, "\n")
cat("Player directory rows:", nrow(players), "\n")
