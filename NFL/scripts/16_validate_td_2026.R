source("R/utilities.R")
source("R/odds_api.R")
source("R/touchdown_features.R")
source("R/touchdown_model.R")
source("R/touchdown_backtest.R")
source("R/touchdown_deploy.R")
assert_packages()

config <- yaml::read_yaml("config/td_2026.yml")
stopifnot(
  config$season == 2026,
  config$mode == "paper",
  config$bankroll$starting_bankroll == 1000,
  config$bankroll$unit_fraction == 0.01,
  config$bankroll$minimum_units == 3,
  config$bankroll$maximum_units == 10,
  is.null(config$bankroll$daily_or_weekly_cap)
)

walk_forward <- readRDS("data/processed/td_walk_forward_model.rds")
predictions <- walk_forward$predictions
historical_keys <- paste(predictions$game_id, predictions$player_id)
stopifnot(!anyDuplicated(historical_keys))

deployment <- readRDS("data/processed/td_deployment_model.rds")

# The board scorer only touches the booster once a game enters the refresh
# window, so a dead handle would otherwise stay invisible until game week.
if (!booster_is_live(deployment$fundamental_fit, deployment$features)) {
  stop(
    "The touchdown fundamental booster cannot score after reload. ",
    "Retrain with scripts/13_train_td_model.R.",
    call. = FALSE
  )
}

weather_features <- c(
  "temperature", "wind_speed", "precip_probability",
  "is_dome", "cold_index", "high_wind_index",
  "wind_receiving_role", "precip_rushing_role", "qb_rush_weather"
)
stopifnot(
  !is.null(deployment$market_fit),
  all(weather_features %in% deployment$features)
)

schedules <- readRDS("data/raw/schedules_2026.rds")
rosters <- readRDS("data/raw/rosters_2026.rds")
event_map <- match_td_events_to_schedule(
  readRDS("data/raw/td_2026_current_events.rds"),
  schedules
)
game_lines <- flatten_current_td_game_lines(
  readRDS("data/raw/td_2026_current_game_lines.rds")
)
stopifnot(
  nrow(dplyr::filter(
    schedules,
    .data$season == 2026,
    .data$game_type == "REG"
  )) == 272,
  nrow(rosters) > 0,
  nrow(event_map) > 0,
  nrow(game_lines) == nrow(event_map),
  all(is.finite(game_lines$total_line)),
  all(is.finite(game_lines$home_spread))
)

card <- readr::read_csv(
  "outputs/td_2026_bet_card.csv",
  show_col_types = FALSE
)
if (nrow(card)) {
  stopifnot(!anyDuplicated(paste(card$game_id, card$player_id)))
}

status <- readr::read_csv(
  "outputs/td_2026_refresh_status.csv",
  show_col_types = FALSE
)
stopifnot(
  nrow(status) == 1,
  status$execution_mode[[1]] == "paper",
  status$current_nfl_events[[1]] == nrow(event_map)
)

cat("2026 touchdown setup validation: OK\n")
cat("Historical prediction rows:", nrow(predictions), "\n")
cat("Unique 2026 events matched:", nrow(event_map), "\n")
cat("Current bet-card rows:", nrow(card), "\n")
