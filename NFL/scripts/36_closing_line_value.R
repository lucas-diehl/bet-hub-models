source("R/utilities.R")
source("R/odds_api.R")
assert_packages()
ensure_directories()

# Extracts an early and a late line for every 2025 game from the purchased
# snapshots, then asks the only question that matters at these sample sizes:
# does the model's disagreement with the early number predict which way the
# market subsequently moves?
#
# Closing-line value is a far more efficient signal than profit. At roughly 55
# wagers a season, results take many years to separate a real edge from noise,
# while CLV is measurable on every game whether or not it was bet.

season <- 2025
raw_dir <- file.path("data/raw/odds_api_line_movement", season)
files <- list.files(raw_dir, pattern = "\\.json$", full.names = TRUE)
if (!length(files)) stop("No snapshots. Run scripts/35 first.", call. = FALSE)

abbreviations <- setNames(
  names(odds_api_team_names()), unname(odds_api_team_names())
)

# Median across books, so one outlier shop cannot define the market number.
#
# The snapshot's own week is carried through deliberately. A Tuesday pull
# returns every game the books have posted, including ones months away, so
# taking each game's globally earliest snapshot would compare the model against
# a line set before the season had happened. That is not closing-line value, it
# is a 67-day information advantage. Only same-week snapshots are used.
extract_snapshot <- function(path) {
  payload <- jsonlite::read_json(path, simplifyVector = FALSE)
  stamp <- as.character(payload$timestamp)
  snapshot_week <- as.integer(sub("^(\\d+)_.*$", "\\1", basename(path)))
  purrr::map_dfr(payload$data %||% list(), function(event) {
    home <- abbreviations[[as.character(event$home_team)]] %||% NA_character_
    away <- abbreviations[[as.character(event$away_team)]] %||% NA_character_
    if (is.na(home) || is.na(away)) return(tibble::tibble())

    spreads <- c()
    totals <- c()
    for (book in event$bookmakers %||% list()) {
      for (market in book$markets %||% list()) {
        if (identical(market$key, "spreads")) {
          for (o in market$outcomes %||% list()) {
            if (identical(as.character(o$name), as.character(event$home_team))) {
              spreads <- c(spreads, suppressWarnings(as.numeric(o$point)))
            }
          }
        }
        if (identical(market$key, "totals")) {
          for (o in market$outcomes %||% list()) {
            if (tolower(as.character(o$name)) == "over") {
              totals <- c(totals, suppressWarnings(as.numeric(o$point)))
            }
          }
        }
      }
    }
    tibble::tibble(
      snapshot_utc = stamp,
      snapshot_week = snapshot_week,
      commence_time = as.character(event$commence_time),
      home_team = home, away_team = away,
      home_line = if (length(spreads)) stats::median(spreads, na.rm = TRUE) else NA_real_,
      total_line = if (length(totals)) stats::median(totals, na.rm = TRUE) else NA_real_,
      books = length(event$bookmakers %||% list())
    )
  })
}

snapshots <- purrr::map_dfr(files, extract_snapshot) |>
  dplyr::filter(!is.na(.data$home_line) | !is.na(.data$total_line))

schedules <- readRDS("data/raw/schedules.rds") |>
  dplyr::filter(.data$season == !!season, .data$game_type == "REG") |>
  dplyr::transmute(
    .data$game_id, .data$season, .data$week,
    home_team = normalize_team(.data$home_team),
    away_team = normalize_team(.data$away_team),
    kickoff = lubridate::ymd_hms(
      nfl_kickoff_utc(.data$gameday, .data$gametime), tz = "UTC"
    )
  )

matched <- snapshots |>
  dplyr::mutate(
    home_team = normalize_team(.data$home_team),
    away_team = normalize_team(.data$away_team),
    stamp = lubridate::ymd_hms(.data$snapshot_utc, tz = "UTC")
  ) |>
  dplyr::inner_join(schedules, by = c("home_team", "away_team")) |>
  dplyr::filter(
    .data$stamp < .data$kickoff,
    .data$snapshot_week == .data$week
  ) |>
  dplyr::mutate(
    lead_hours = as.numeric(difftime(.data$kickoff, .data$stamp, units = "hours"))
  )

movement <- matched |>
  dplyr::group_by(.data$game_id, .data$season, .data$week) |>
  dplyr::arrange(.data$stamp, .by_group = TRUE) |>
  dplyr::summarise(
    snapshots = dplyr::n(),
    open_home_line = dplyr::first(.data$home_line),
    close_home_line = dplyr::last(.data$home_line),
    open_total = dplyr::first(.data$total_line),
    close_total = dplyr::last(.data$total_line),
    open_lead_hours = dplyr::first(.data$lead_hours),
    close_lead_hours = dplyr::last(.data$lead_hours),
    .groups = "drop"
  ) |>
  dplyr::filter(.data$snapshots >= 2)

readr::write_csv(movement, "outputs/line_movement_2025.csv")

cat("=== Snapshot coverage ===\n")
cat("Snapshot files read:", length(files), "\n")
cat("Games with 2+ usable snapshots:", nrow(movement), "of 272\n")
cat("Median hours before kickoff, first snapshot:",
    round(stats::median(movement$open_lead_hours), 1), "\n")
cat("Median hours before kickoff, last snapshot:",
    round(stats::median(movement$close_lead_hours), 1), "\n")
cat("Games whose last snapshot is within 6h of kickoff:",
    sum(movement$close_lead_hours <= 6), "\n")

# Model predictions for 2025, from the walk-forward backtest.
predictions <- readr::read_csv("outputs/predictions.csv", show_col_types = FALSE) |>
  dplyr::filter(
    .data$season == !!season,
    (.data$target == "game_total" & .data$model == "forward_linear") |
      (.data$target == "home_margin" & .data$model == "random_forest")
  )

clv <- predictions |>
  dplyr::inner_join(movement, by = c("game_id", "season")) |>
  dplyr::mutate(
    open_market = dplyr::if_else(
      .data$target == "game_total", .data$open_total, -.data$open_home_line
    ),
    close_market = dplyr::if_else(
      .data$target == "game_total", .data$close_total, -.data$close_home_line
    ),
    edge = .data$prediction - .data$open_market,
    movement = .data$close_market - .data$open_market,
    # Positive when the market moved toward the side the model favoured.
    clv_points = dplyr::if_else(
      .data$edge > 0, .data$movement, -.data$movement
    )
  ) |>
  dplyr::filter(is.finite(.data$edge), is.finite(.data$movement))

readr::write_csv(clv, "outputs/closing_line_value_2025.csv")

cat("\n=== Does model edge predict market movement? ===\n")
print(as.data.frame(
  clv |>
    dplyr::group_by(.data$target) |>
    dplyr::summarise(
      games = dplyr::n(),
      mean_abs_movement = mean(abs(.data$movement)),
      correlation = stats::cor(.data$edge, .data$movement),
      mean_clv_points = mean(.data$clv_points),
      pct_positive_clv = mean(.data$clv_points > 0),
      .groups = "drop"
    )
), digits = 4)

cat("\n=== CLV on bets that clear the frozen thresholds ===\n")
bet_clv <- clv |>
  dplyr::filter(
    (.data$target == "game_total" & abs(.data$edge) >= 5) |
      (.data$target == "home_margin" & .data$edge >= 6)
  )
if (nrow(bet_clv)) {
  print(as.data.frame(
    bet_clv |>
      dplyr::group_by(.data$target) |>
      dplyr::summarise(
        bets = dplyr::n(),
        mean_clv_points = mean(.data$clv_points),
        pct_positive_clv = mean(.data$clv_points > 0),
        .groups = "drop"
      )
  ), digits = 4)
} else {
  cat("No qualifying bets.\n")
}

cat("\n=== CLV by model edge band (all games) ===\n")
print(as.data.frame(
  clv |>
    dplyr::mutate(band = cut(
      abs(.data$edge), c(0, 2, 4, 6, 8, Inf),
      labels = c("0-2", "2-4", "4-6", "6-8", "8+"), include.lowest = TRUE
    )) |>
    dplyr::group_by(.data$target, .data$band) |>
    dplyr::summarise(
      games = dplyr::n(),
      mean_clv_points = mean(.data$clv_points),
      pct_positive = mean(.data$clv_points > 0),
      .groups = "drop"
    )
), digits = 4)
