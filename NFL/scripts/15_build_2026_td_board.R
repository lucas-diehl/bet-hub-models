source("R/utilities.R")
source("R/dashboard_feed.R")
source("R/odds_api.R")
source("R/touchdown_features.R")
source("R/touchdown_model.R")
source("R/touchdown_backtest.R")
source("R/touchdown_deploy.R")
assert_packages()
ensure_directories()

args <- commandArgs(trailingOnly = TRUE)
execute <- "--execute" %in% args
refresh_stats <- "--refresh-stats" %in% args
config <- yaml::read_yaml("config/td_2026.yml")
now_utc <- lubridate::now(tzone = "UTC")

schedule_path <- "data/raw/schedules_2026.rds"
roster_path <- "data/raw/rosters_2026.rds"
players_path <- "data/raw/players_2026.rds"
stats_path <- "data/raw/player_stats_2021_2026.rds"
events_path <- "data/raw/td_2026_current_events.rds"
lines_path <- "data/raw/td_2026_current_game_lines.rds"
props_dir <- "data/raw/odds_api_td_2026_current"
dir.create(props_dir, recursive = TRUE, showWarnings = FALSE)

if (execute || !file.exists(schedule_path)) {
  schedules <- nflreadr::load_schedules(2026)
  rosters <- nflreadr::load_rosters(2026)
  players <- nflreadr::load_players()
  if (!nrow(schedules) || !nrow(rosters)) {
    stop("Current 2026 schedule or roster download returned no rows.", call. = FALSE)
  }
  saveRDS(schedules, schedule_path)
  saveRDS(rosters, roster_path)
  saveRDS(players, players_path)
} else {
  schedules <- readRDS(schedule_path)
  rosters <- readRDS(roster_path)
  players <- if (file.exists(players_path)) {
    readRDS(players_path)
  } else {
    readRDS("data/raw/players.rds")
  }
}

if (refresh_stats || !file.exists(stats_path)) {
  historical_stats <- readRDS("data/raw/player_stats_2021_2025.rds")
  current_stats <- tryCatch(
    nflreadr::load_player_stats(2026, summary_level = "week"),
    error = function(e) tibble::tibble()
  )
  player_stats <- dplyr::bind_rows(historical_stats, current_stats) |>
    dplyr::distinct(.data$player_id, .data$game_id, .keep_all = TRUE)
  saveRDS(player_stats, stats_path)
} else {
  player_stats <- readRDS(stats_path)
}

quota_used <- NA_real_
quota_remaining <- NA_real_
if (execute) {
  events_result <- odds_api_current_events()
  lines_result <- odds_api_current_game_lines(config$market$region)
  saveRDS(events_result$data, events_path)
  saveRDS(lines_result$data, lines_path)
  quota_used <- lines_result$quota$last
  quota_remaining <- lines_result$quota$remaining
} else {
  if (!file.exists(events_path) || !file.exists(lines_path)) {
    stop(
      "No cached 2026 market snapshot. Run with --execute once.",
      call. = FALSE
    )
  }
  events_result <- list(data = readRDS(events_path))
  lines_result <- list(data = readRDS(lines_path))
}

event_map <- match_td_events_to_schedule(events_result$data, schedules)
game_lines <- flatten_current_td_game_lines(lines_result$data)

window_end <- now_utc + lubridate::days(config$market$refresh_window_days)
upcoming_events <- event_map |>
  dplyr::mutate(
    commence = lubridate::ymd_hms(.data$commence_time, tz = "UTC")
  ) |>
  dplyr::filter(
    .data$commence >= now_utc - lubridate::hours(6),
    .data$commence <= window_end
  )

if (execute && nrow(upcoming_events)) {
  for (i in seq_len(nrow(upcoming_events))) {
    event <- upcoming_events[i, ]
    result <- odds_api_current_event_props(
      event$event_id,
      market = "player_anytime_td",
      region = config$market$region
    )
    saveRDS(
      result$data,
      file.path(props_dir, paste0(event$event_id, ".rds"))
    )
    quota_used <- result$quota$last
    quota_remaining <- result$quota$remaining
  }
}

prop_rows <- purrr::map_dfr(seq_len(nrow(upcoming_events)), function(i) {
  event <- upcoming_events[i, ]
  prop_path <- file.path(props_dir, paste0(event$event_id, ".rds"))
  if (!file.exists(prop_path)) return(tibble::tibble())
  schedule_row <- schedules |>
    dplyr::filter(.data$game_id == event$game_id) |>
    dplyr::slice_head(n = 1L)
  game <- list(
    game_id = as.character(schedule_row$game_id[[1]]),
    season = as.integer(schedule_row$season[[1]]),
    week = as.integer(schedule_row$week[[1]]),
    snapshot_utc = format(now_utc, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    kickoff_utc = as.character(event$schedule_kickoff[[1]])
  )
  flatten_current_anytime_td(readRDS(prop_path), game)
})

# Restrict to the books the feed is allowed to quote, before the board picks a
# best price. Filtering afterwards would drop any bet whose best price happened
# to sit at an excluded book; filtering here re-prices it at the best available
# permitted book instead, which is what is actually bettable.
#
# This matters more for touchdowns than anything else in the project. Priced at
# the best of eight books the core tier returned +13.75% over 2023-25; at
# DraftKings and FanDuel alone it collapses to +0.83%. Adding Caesars and BetMGM
# recovers +11.60%, so the four-book set is what makes the tier viable at all.
permitted_books <- feed_books()
if (nrow(prop_rows)) {
  before <- nrow(prop_rows)
  prop_rows <- prop_rows |>
    dplyr::filter(tolower(.data$bookmaker_key) %in% names(permitted_books))
  cat("Price rows at permitted books:", nrow(prop_rows), "of", before, "\n")
}

game_overrides <- readr::read_csv(
  "config/td_2026_game_overrides.csv",
  show_col_types = FALSE
)
player_overrides <- readr::read_csv(
  "config/td_2026_player_overrides.csv",
  show_col_types = FALSE
)

if (!nrow(prop_rows)) {
  card <- empty_td_2026_board()
} else {
  upcoming_context <- build_td_2026_game_context(
    schedules,
    upcoming_events,
    game_lines,
    game_overrides
  )
  historical_context <- readRDS("data/processed/td_game_context.rds")
  player_features <- build_td_2026_player_features(
    player_stats,
    rosters,
    historical_context,
    upcoming_context
  )
  prop_board <- build_td_prop_board(prop_rows, player_features, players)
  deployment <- readRDS("data/processed/td_deployment_model.rds")
  scored <- score_td_deployment_board(
    prop_board |>
      dplyr::filter(!is.na(.data$player_id), .data$current_game_match),
    deployment
  )

  ledger_path <- "outputs/td_2026_bankroll_ledger.csv"
  bankroll <- config$bankroll$starting_bankroll
  if (file.exists(ledger_path)) {
    ledger <- readr::read_csv(ledger_path, show_col_types = FALSE)
    if (nrow(ledger) && "bankroll_after" %in% names(ledger)) {
      last_value <- suppressWarnings(as.numeric(tail(ledger$bankroll_after, 1)))
      if (is.finite(last_value)) bankroll <- last_value
    }
  }
  card <- prepare_td_2026_bet_card(
    scored,
    rosters,
    config,
    bankroll,
    player_overrides
  )
}

board_columns <- names(empty_td_2026_board())
for (column in setdiff(board_columns, names(card))) card[[column]] <- NA
card_output <- card |>
  dplyr::select(dplyr::all_of(board_columns))
readr::write_csv(card_output, "outputs/td_2026_bet_card.csv")

ledger_path <- "outputs/td_2026_bankroll_ledger.csv"
if (!file.exists(ledger_path)) {
  readr::write_csv(
    tibble::tibble(
      settled_date = as.Date(character()),
      game_id = character(),
      player_id = character(),
      player = character(),
      strategy_tier = character(),
      american_odds = double(),
      units = double(),
      unit_value = double(),
      stake = double(),
      result = character(),
      profit = double(),
      bankroll_before = double(),
      bankroll_after = double(),
      notes = character()
    ),
    ledger_path
  )
}

refresh_status <- tibble::tibble(
  refresh_timestamp_utc = format(
    now_utc,
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  ),
  execution_mode = config$mode,
  api_execute = execute,
  current_nfl_events = nrow(event_map),
  games_inside_refresh_window = nrow(upcoming_events),
  player_price_rows = nrow(prop_rows),
  model_qualified_bets = sum(
    card_output$strategy_tier %in% c("CORE", "EXPANDED")
  ),
  ready_bets = sum(stringr::str_detect(
    card_output$execution_status,
    "READY$"
  )),
  odds_api_last_cost = quota_used,
  odds_api_remaining = quota_remaining,
  note = dplyr::case_when(
    !nrow(upcoming_events) ~
      "No regular-season game is inside the configured refresh window.",
    !nrow(prop_rows) ~
      "Games are in range, but anytime-touchdown markets are not posted.",
    TRUE ~ "Board refreshed; verify weather and game-day active status."
  )
)
readr::write_csv(refresh_status, "outputs/td_2026_refresh_status.csv")

cat("2026 NFL events matched:", nrow(event_map), "\n")
cat("Games inside refresh window:", nrow(upcoming_events), "\n")
cat("Anytime-TD price rows:", nrow(prop_rows), "\n")
cat("Model-qualified bets:", refresh_status$model_qualified_bets, "\n")
cat("Ready bets:", refresh_status$ready_bets, "\n")
cat(refresh_status$note, "\n")
