source("R/utilities.R")
source("R/odds_api.R")
source("R/features.R")
source("R/models.R")
source("R/backtest.R")
assert_packages()
ensure_directories()
cfg <- read_config()

# Scores the posted 2026 lines with the two funded game-level models.
#
# The backtest pipeline builds features only for games that have already been
# played - build_game_features() drops anything without a final score. For live
# picks the 2026 schedule is appended to the team-game table as rows with no
# stats, so add_rolling_features() computes each team's lagged form from its
# last completed games and carries it into the new season.

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", flag, "="), "", hit[[1]])
}
max_week <- as.integer(arg_value("--max-week", "3"))

team_games <- readRDS("data/raw/team_games.rds")
schedules_hist <- readRDS("data/raw/schedules.rds")
schedules_2026 <- readRDS("data/raw/schedules_2026.rds")

games_hist <- schedule_games(schedules_hist, regular_season_only = TRUE)

future_games <- schedules_2026 |>
  dplyr::filter(.data$game_type == "REG", .data$week <= max_week) |>
  dplyr::transmute(
    .data$game_id,
    season = as.integer(.data$season),
    week = as.integer(.data$week),
    game_date = as.Date(.data$gameday),
    home_team = normalize_team(.data$home_team),
    away_team = normalize_team(.data$away_team)
  )
cat("2026 games in scope:", nrow(future_games), "\n")

# Stat-free rows for the upcoming games, one per team, so the rolling window
# sees them as "next game" rather than as history.
stat_cols <- setdiff(
  names(team_games),
  c("game_id", "season", "week", "game_date", "team", "opponent")
)
future_team_rows <- dplyr::bind_rows(
  future_games |>
    dplyr::transmute(.data$game_id, .data$season, .data$week, .data$game_date,
                     team = .data$home_team, opponent = .data$away_team),
  future_games |>
    dplyr::transmute(.data$game_id, .data$season, .data$week, .data$game_date,
                     team = .data$away_team, opponent = .data$home_team)
)
for (column in stat_cols) future_team_rows[[column]] <- NA_real_

combined <- dplyr::bind_rows(
  team_games |> add_team_context(games_hist),
  future_team_rows
) |>
  dplyr::arrange(.data$team, .data$game_date, .data$game_id)

rolled <- add_rolling_features(combined, unlist(cfg$features$rolling_windows))

feature_cols <- names(rolled)[
  stringr::str_detect(names(rolled), "_r\\d+$") |
    names(rolled) %in% c("prior_games", "rest_days", "pythagorean_win_pct")
]
side <- function(games, team_column, prefix) {
  games |>
    dplyr::select("game_id", team = dplyr::all_of(team_column)) |>
    dplyr::left_join(rolled, by = c("game_id", "team")) |>
    dplyr::select("game_id", dplyr::all_of(feature_cols)) |>
    dplyr::rename_with(~ paste0(prefix, .x), -dplyr::all_of("game_id"))
}

# --------------------------------------------------------------------------
# Current market
# --------------------------------------------------------------------------

abbrev <- stats::setNames(
  names(odds_api_team_names()), unname(odds_api_team_names())
)
market_raw <- odds_api_current_game_lines("us")
cat("Odds credits used:", market_raw$quota$last,
    " remaining:", market_raw$quota$remaining, "\n")

events <- market_raw$data
market <- purrr::map_dfr(seq_len(nrow(events)), function(i) {
  home <- abbrev[[as.character(events$home_team[[i]])]] %||% NA_character_
  away <- abbrev[[as.character(events$away_team[[i]])]] %||% NA_character_
  if (is.na(home) || is.na(away)) return(tibble::tibble())
  books <- events$bookmakers[[i]]
  if (is.null(books) || !nrow(books)) return(tibble::tibble())
  purrr::map_dfr(seq_len(nrow(books)), function(b) {
    markets <- books$markets[[b]]
    if (is.null(markets) || !nrow(markets)) return(tibble::tibble())
    purrr::map_dfr(seq_len(nrow(markets)), function(m) {
      outcomes <- markets$outcomes[[m]]
      if (is.null(outcomes) || !nrow(outcomes)) return(tibble::tibble())
      tibble::tibble(
        home_team = normalize_team(home), away_team = normalize_team(away),
        book = as.character(books$key[[b]]),
        market = as.character(markets$key[[m]]),
        outcome = as.character(outcomes$name),
        point = suppressWarnings(as.numeric(outcomes$point)),
        price = suppressWarnings(as.numeric(outcomes$price)),
        home_full = as.character(events$home_team[[i]])
      )
    })
  })
})

consensus <- market |>
  dplyr::filter(!is.na(.data$point)) |>
  dplyr::mutate(
    side = dplyr::case_when(
      .data$market == "totals" ~ tolower(.data$outcome),
      .data$outcome == .data$home_full ~ "home",
      TRUE ~ "away"
    )
  ) |>
  dplyr::group_by(.data$home_team, .data$away_team, .data$market, .data$side) |>
  dplyr::summarise(
    line = stats::median(.data$point),
    best_price = max(.data$price),
    books = dplyr::n(),
    .groups = "drop"
  )

lines <- consensus |>
  dplyr::filter(
    (.data$market == "spreads" & .data$side == "home") |
      (.data$market == "totals" & .data$side == "over")
  ) |>
  dplyr::select("home_team", "away_team", "market", "line", "books") |>
  tidyr::pivot_wider(names_from = "market", values_from = c("line", "books")) |>
  dplyr::rename(home_line = "line_spreads", total_line = "line_totals")

# --------------------------------------------------------------------------
# Predict and grade against the frozen thresholds
# --------------------------------------------------------------------------

train <- readRDS("data/processed/game_features.rds")
test <- future_games |>
  dplyr::left_join(side(future_games, "home_team", "home_"), by = "game_id") |>
  dplyr::left_join(side(future_games, "away_team", "away_"), by = "game_id") |>
  dplyr::inner_join(lines, by = c("home_team", "away_team")) |>
  dplyr::filter(!is.na(.data$home_line), !is.na(.data$total_line)) |>
  dplyr::mutate(
    market_margin = -.data$home_line,
    market_total = .data$total_line,
    home_margin = NA_real_, game_total = NA_real_,
    neutral_temperature = 70, neutral_wind = 0
  )
cat("Games with both features and lines:", nrow(test), "\n")
if (!nrow(test)) quit(save = "no", status = 0)

shared <- intersect(feature_names(train), names(test))
set.seed(cfg$backtest$seed)
total_pred <- fit_predict_model("forward_linear", train, test, "game_total",
                                cfg, features = shared)
set.seed(cfg$backtest$seed)
margin_pred <- fit_predict_model("random_forest", train, test, "home_margin",
                                 cfg, features = shared)

picks <- test |>
  dplyr::mutate(
    projected_total = total_pred,
    projected_margin = margin_pred,
    total_edge = .data$projected_total - .data$total_line,
    margin_edge = .data$projected_margin - .data$market_margin
  ) |>
  dplyr::select(
    "game_id", "week", "game_date", "home_team", "away_team",
    "total_line", "projected_total", "total_edge",
    "home_line", "projected_margin", "margin_edge"
  )
readr::write_csv(picks, "outputs/live_game_picks_2026.csv")

total_bets <- picks |> dplyr::filter(abs(.data$total_edge) >= 5)
spread_bets <- picks |> dplyr::filter(.data$margin_edge >= 6)

cat("\n=== Totals qualifying at 5+ points ===\n")
if (nrow(total_bets)) {
  print(as.data.frame(
    total_bets |>
      dplyr::transmute(
        .data$week, .data$away_team, .data$home_team,
        line = .data$total_line,
        model = round(.data$projected_total, 1),
        edge = round(.data$total_edge, 1),
        side = dplyr::if_else(.data$total_edge > 0, "OVER", "UNDER")
      ) |>
      dplyr::arrange(dplyr::desc(abs(.data$edge)))
  ))
} else cat("None.\n")

cat("\n=== Home spreads qualifying at 6+ points ===\n")
if (nrow(spread_bets)) {
  print(as.data.frame(
    spread_bets |>
      dplyr::transmute(
        .data$week, .data$away_team, .data$home_team,
        line = .data$home_line,
        model = round(.data$projected_margin, 1),
        edge = round(.data$margin_edge, 1)
      ) |>
      dplyr::arrange(dplyr::desc(.data$edge))
  ))
} else cat("None.\n")

cat("\nEdge distribution (all games in scope):\n")
cat("  total  |edge| >= 3:", sum(abs(picks$total_edge) >= 3),
    " >= 4:", sum(abs(picks$total_edge) >= 4),
    " >= 5:", nrow(total_bets), "\n")
cat("  margin  edge  >= 4:", sum(picks$margin_edge >= 4),
    " >= 5:", sum(picks$margin_edge >= 5),
    " >= 6:", nrow(spread_bets), "\n")
