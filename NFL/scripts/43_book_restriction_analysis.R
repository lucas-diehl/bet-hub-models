source("R/utilities.R")
source("R/odds_api.R")
source("R/dashboard_feed.R")
assert_packages()
ensure_directories()

# What the two-book feed restriction costs on spreads and totals.
#
# The touchdown tier lost almost all of its edge when priced at DraftKings or
# FanDuel instead of the best of eight books, because longshot prices disperse
# widely. Spreads and totals should behave differently: they sit near -110 and
# the dispersion is small in price terms. But they have a second axis the
# touchdown market does not - the line itself - and half a point matters more
# than five cents. Both are measured here.

season <- 2025
snapshot_dir <- file.path("data/raw/odds_api_line_movement", season)
files <- list.files(snapshot_dir, pattern = "\\.json$", full.names = TRUE)
if (!length(files)) stop("No snapshots. Run scripts/35 first.", call. = FALSE)

allowed <- names(feed_books())
abbrev <- stats::setNames(
  names(odds_api_team_names()), unname(odds_api_team_names())
)

read_all_books <- function(path) {
  payload <- jsonlite::read_json(path, simplifyVector = FALSE)
  week <- as.integer(sub("^(\\d+)_.*$", "\\1", basename(path)))
  label <- sub("^\\d+_(.*)\\.json$", "\\1", basename(path))
  stamp <- as.character(payload$timestamp)
  purrr::map_dfr(payload$data %||% list(), function(event) {
    home <- abbrev[[as.character(event$home_team)]] %||% NA_character_
    away <- abbrev[[as.character(event$away_team)]] %||% NA_character_
    if (is.na(home) || is.na(away)) return(tibble::tibble())
    purrr::map_dfr(event$bookmakers %||% list(), function(book) {
      purrr::map_dfr(book$markets %||% list(), function(market) {
        purrr::map_dfr(market$outcomes %||% list(), function(o) {
          tibble::tibble(
            snapshot_week = week, snapshot_label = label,
            snapshot_utc = stamp,
            home_team = normalize_team(home), away_team = normalize_team(away),
            book_key = as.character(book$key),
            book_title = as.character(book$title),
            market_key = as.character(market$key),
            outcome = as.character(o$name),
            is_home_side = identical(
              as.character(o$name), as.character(event$home_team)
            ),
            point = suppressWarnings(as.numeric(o$point)),
            price = suppressWarnings(as.numeric(o$price))
          )
        })
      })
    })
  })
}

quotes <- purrr::map_dfr(files, read_all_books) |>
  dplyr::filter(!is.na(.data$price))

cat("=== Sportsbooks present in the 2025 spread/total snapshots ===\n")
inventory <- quotes |>
  dplyr::group_by(.data$book_key, .data$book_title) |>
  dplyr::summarise(quotes = dplyr::n(), .groups = "drop") |>
  dplyr::mutate(
    in_feed = dplyr::if_else(
      .data$book_key %in% allowed, "FEED-ELIGIBLE", ""
    )
  ) |>
  dplyr::arrange(dplyr::desc(.data$quotes))
print(as.data.frame(inventory))
readr::write_csv(inventory, "outputs/book_inventory_2025.csv")

# Use the snapshot closest to kickoff for each game: that is the price you
# would actually be taking.
raw_schedule <- readRDS("data/raw/schedules.rds") |>
  dplyr::filter(.data$season == !!season, .data$game_type == "REG")
schedules <- raw_schedule |>
  dplyr::transmute(
    .data$game_id, .data$week,
    home_team = normalize_team(.data$home_team),
    away_team = normalize_team(.data$away_team),
    home_score = as.numeric(.data$home_score),
    away_score = as.numeric(.data$away_score),
    kickoff = lubridate::ymd_hms(
      nfl_kickoff_utc(.data$gameday, .data$gametime), tz = "UTC"
    )
  )

# Snapshots taken after kickoff carry live in-play prices, which are not
# bettable pregame numbers and are wildly dispersed (a +1800 spread once a game
# has gone sideways). Including them would make the multi-book universe look
# far better than it is.
latest <- quotes |>
  dplyr::inner_join(schedules, by = c("home_team", "away_team")) |>
  dplyr::filter(
    .data$snapshot_week == .data$week,
    lubridate::ymd_hms(.data$snapshot_utc, tz = "UTC") < .data$kickoff
  ) |>
  dplyr::group_by(.data$game_id) |>
  dplyr::filter(
    .data$snapshot_utc == max(.data$snapshot_utc)
  ) |>
  dplyr::ungroup()

# --------------------------------------------------------------------------
# Axis 1: price dispersion at a fixed line
# --------------------------------------------------------------------------

consensus <- latest |>
  dplyr::group_by(.data$game_id, .data$market_key, .data$outcome) |>
  dplyr::summarise(consensus_point = stats::median(.data$point), .groups = "drop")

at_line <- latest |>
  dplyr::inner_join(consensus, by = c("game_id", "market_key", "outcome")) |>
  dplyr::filter(.data$point == .data$consensus_point)

price_split <- at_line |>
  dplyr::group_by(.data$game_id, .data$market_key, .data$outcome) |>
  dplyr::summarise(
    books = dplyr::n(),
    best_all = max(.data$price),
    best_feed = suppressWarnings(max(.data$price[.data$book_key %in% allowed])),
    feed_has_best = any(
      .data$book_key %in% allowed & .data$price == max(.data$price)
    ),
    .groups = "drop"
  ) |>
  dplyr::filter(is.finite(.data$best_feed))

# American odds are discontinuous at +/-100, so a move from -105 to +100 reads
# as "205 cents" while being worth barely a point of implied probability.
# Differences are therefore reported on the implied-probability scale, with the
# median American gap kept only as a familiar reference.
cat("\n=== Axis 1: best price at the consensus line ===\n")
print(as.data.frame(
  price_split |>
    dplyr::mutate(
      prob_all = american_to_prob(.data$best_all),
      prob_feed = american_to_prob(.data$best_feed)
    ) |>
    dplyr::group_by(market = .data$market_key) |>
    dplyr::summarise(
      selections = dplyr::n(),
      mean_books = mean(.data$books),
      median_best_all = stats::median(.data$best_all),
      median_best_feed = stats::median(.data$best_feed),
      median_cents_given_up = stats::median(.data$best_all - .data$best_feed),
      mean_breakeven_given_up = mean(.data$prob_feed - .data$prob_all),
      pct_feed_has_best = mean(.data$feed_has_best),
      .groups = "drop"
    )
), digits = 4)

cat("\nWho supplies the best price when it beats DK/FD by 20+ cents:\n")
print(
  at_line |>
    dplyr::inner_join(
      price_split |> dplyr::filter(.data$best_all - .data$best_feed >= 20) |>
        dplyr::select("game_id", "market_key", "outcome", "best_all"),
      by = c("game_id", "market_key", "outcome")
    ) |>
    dplyr::filter(.data$price == .data$best_all) |>
    dplyr::count(.data$book_title, sort = TRUE) |>
    as.data.frame()
)

# --------------------------------------------------------------------------
# Axis 2: line availability. Half a point is worth far more than five cents.
# --------------------------------------------------------------------------

line_split <- latest |>
  dplyr::group_by(.data$game_id, .data$market_key, .data$outcome) |>
  dplyr::summarise(
    # For a total Over and for a spread side, the bettor wants the lowest
    # number; for an Under, the highest. Handle both directions.
    best_all_low = min(.data$point),
    best_all_high = max(.data$point),
    best_feed_low = suppressWarnings(min(.data$point[.data$book_key %in% allowed])),
    best_feed_high = suppressWarnings(max(.data$point[.data$book_key %in% allowed])),
    .groups = "drop"
  ) |>
  dplyr::filter(is.finite(.data$best_feed_low)) |>
  dplyr::mutate(
    wants_low = .data$outcome %in% c("Over") | .data$market_key == "spreads",
    line_given_up = dplyr::if_else(
      .data$wants_low,
      .data$best_feed_low - .data$best_all_low,
      .data$best_all_high - .data$best_feed_high
    )
  )

cat("\n=== Axis 2: best available number ===\n")
print(as.data.frame(
  line_split |>
    dplyr::group_by(market = .data$market_key) |>
    dplyr::summarise(
      selections = dplyr::n(),
      mean_points_given_up = mean(.data$line_given_up),
      pct_feed_matches_best = mean(.data$line_given_up <= 0),
      pct_half_point_worse = mean(.data$line_given_up >= 0.5),
      .groups = "drop"
    )
), digits = 4)

# --------------------------------------------------------------------------
# Strategy impact: grade the actual 2025 qualifying bets both ways
# --------------------------------------------------------------------------

predictions <- readr::read_csv("outputs/predictions.csv", show_col_types = FALSE) |>
  dplyr::filter(
    .data$season == !!season,
    (.data$target == "game_total" & .data$model == "forward_linear") |
      (.data$target == "home_margin" & .data$model == "random_forest")
  ) |>
  dplyr::select("game_id", "target", "prediction") |>
  tidyr::pivot_wider(names_from = "target", values_from = "prediction")

# Best (line, price) pair per side, within each book universe.
side_quotes <- latest |>
  dplyr::mutate(
    side = dplyr::case_when(
      .data$market_key == "totals" ~ tolower(.data$outcome),
      .data$is_home_side ~ "home",
      TRUE ~ "away"
    )
  ) |>
  dplyr::select(
    "game_id", "market_key", "side", "book_key", "point", "price"
  )

grade_universe <- function(universe_label, keys) {
  q <- side_quotes |> dplyr::filter(.data$book_key %in% keys)

  totals <- q |>
    dplyr::filter(.data$market_key == "totals") |>
    dplyr::inner_join(predictions, by = "game_id") |>
    dplyr::mutate(
      edge = dplyr::if_else(
        .data$side == "over",
        .data$game_total - .data$point, .data$point - .data$game_total
      )
    ) |>
    dplyr::filter(.data$edge >= 5) |>
    dplyr::group_by(.data$game_id) |>
    dplyr::arrange(dplyr::desc(.data$edge), dplyr::desc(.data$price),
                   .by_group = TRUE) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::inner_join(schedules, by = "game_id") |>
    dplyr::mutate(
      actual = .data$home_score + .data$away_score,
      result = dplyr::case_when(
        .data$actual == .data$point ~ 0,
        (.data$actual > .data$point) == (.data$side == "over") ~ 1,
        TRUE ~ -1
      ),
      market = "total"
    )

  spreads <- q |>
    dplyr::filter(.data$market_key == "spreads", .data$side == "home") |>
    dplyr::inner_join(predictions, by = "game_id") |>
    dplyr::mutate(edge = .data$home_margin + .data$point) |>
    dplyr::filter(.data$edge >= 6) |>
    dplyr::group_by(.data$game_id) |>
    dplyr::arrange(dplyr::desc(.data$edge), dplyr::desc(.data$price),
                   .by_group = TRUE) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::inner_join(schedules, by = "game_id") |>
    dplyr::mutate(
      margin = .data$home_score - .data$away_score,
      result = dplyr::case_when(
        .data$margin + .data$point == 0 ~ 0,
        .data$margin + .data$point > 0 ~ 1,
        TRUE ~ -1
      ),
      market = "spread"
    )

  dplyr::bind_rows(
    totals |> dplyr::select("game_id", "market", "point", "price", "result"),
    spreads |> dplyr::select("game_id", "market", "point", "price", "result")
  ) |>
    dplyr::mutate(
      profit = dplyr::case_when(
        .data$result > 0 ~ american_payout(.data$price),
        .data$result < 0 ~ -1, TRUE ~ 0
      ),
      universe = universe_label
    )
}

graded <- dplyr::bind_rows(
  grade_universe("all books", unique(quotes$book_key)),
  grade_universe("DK/FD only", allowed)
)
readr::write_csv(graded, "outputs/book_restriction_bets_2025.csv")

cat("\n=== Strategy impact, 2025 qualifying bets ===\n")
print(as.data.frame(
  graded |>
    dplyr::group_by(.data$universe, .data$market) |>
    dplyr::summarise(
      bets = dplyr::n(),
      wins = sum(.data$result > 0),
      pushes = sum(.data$result == 0),
      mean_price = mean(.data$price),
      profit_units = sum(.data$profit),
      roi = sum(.data$profit) / dplyr::n(),
      .groups = "drop"
    )
), digits = 4)

cat("\n=== Combined portfolio ===\n")
print(as.data.frame(
  graded |>
    dplyr::group_by(.data$universe) |>
    dplyr::summarise(
      bets = dplyr::n(),
      wins = sum(.data$result > 0),
      win_rate = sum(.data$result > 0) / sum(.data$result != 0),
      mean_price = mean(.data$price),
      profit_units = sum(.data$profit),
      roi = sum(.data$profit) / dplyr::n(),
      .groups = "drop"
    )
), digits = 4)
