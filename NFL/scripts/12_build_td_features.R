source("R/utilities.R")
source("R/odds.R")
source("R/touchdown_features.R")
assert_packages()
ensure_directories()

player_stats <- readRDS("data/raw/player_stats_2021_2025.rds")
player_directory <- readRDS("data/raw/players.rds")
schedules <- readRDS("data/raw/schedules.rds")
rotowire <- read_rotowire("data/raw/rotowire_games_archive.json")
td_odds <- readr::read_csv(
  "data/processed/anytime_td_odds_early_2023_2025.csv",
  show_col_types = FALSE
)

game_context <- build_td_game_context(
  schedules,
  rotowire,
  seasons = 2021:2025
)
player_features <- build_td_player_features(player_stats, game_context)
prop_board <- build_td_prop_board(td_odds, player_features, player_directory)

saveRDS(game_context, "data/processed/td_game_context.rds")
saveRDS(player_features, "data/processed/td_player_features.rds")
saveRDS(prop_board, "data/processed/td_prop_board.rds")

audit <- prop_board |>
  dplyr::summarise(
    offered_player_games = dplyr::n(),
    name_matched = sum(!is.na(.data$player_id)),
    feature_matched = sum(!is.na(.data$anytime_td)),
    match_rate = mean(!is.na(.data$anytime_td)),
    qb_rows = sum(.data$position == "QB", na.rm = TRUE)
  )
readr::write_csv(audit, "outputs/td_name_match_audit.csv")

cat("Game contexts:", nrow(game_context), "\n")
cat("Player-game feature rows:", nrow(player_features), "\n")
cat("Offered player-games:", nrow(prop_board), "\n")
print(audit)
