source("R/utilities.R")
source("R/odds_api.R")
source("R/features.R")
source("R/models.R")
source("R/backtest.R")
source("R/dashboard_feed.R")
assert_packages()
ensure_directories()
cfg <- read_config()

# Weekly Tuesday publish for the Bet Hub feed.
#
# Timing: Tuesday, not game day. Tested on the 2025 snapshots, a Tuesday line
# (median 123 hours before kickoff) retains 75% of the bets that still qualify
# at the close, with zero side flips in 267 games, and the early number returned
# +17.1% on totals and +19.3% on home spreads against +20.6% and +1.8% at the
# close. Publishing an hour before kickoff would be operationally useless and is
# not better on the evidence.
#
# Append-only. A bet that has been published is frozen: its price, line, stake
# and id never change on a later run, even if the market moves or it would no
# longer qualify. Re-running is safe and idempotent; only genuinely new
# qualifying bets are added.
#
#   --week=3        which regular-season week to publish (default: next unplayed)
#   --execute       write the feed files (otherwise dry run)
#   --mode=PAPER    feed mode

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", flag, "="), "", hit[[1]])
}
execute <- "--execute" %in% args
mode <- arg_value("--mode", "PAPER")
ledger_path <- "data/processed/published_picks_ledger.csv"

schedules_2026 <- readRDS("data/raw/schedules_2026.rds") |>
  dplyr::filter(.data$game_type == "REG") |>
  dplyr::mutate(gameday = as.Date(.data$gameday))

target_week <- arg_value("--week")
if (is.null(target_week)) {
  upcoming <- schedules_2026 |>
    dplyr::filter(.data$gameday >= Sys.Date()) |>
    dplyr::arrange(.data$gameday)
  if (!nrow(upcoming)) stop("No upcoming games on the schedule.", call. = FALSE)
  target_week <- upcoming$week[[1]]
}
target_week <- as.integer(target_week)
cat("Publishing week:", target_week, "\n")

week_games <- schedules_2026 |>
  dplyr::filter(.data$week == target_week) |>
  dplyr::transmute(
    .data$game_id, season = as.integer(.data$season),
    week = as.integer(.data$week), game_date = .data$gameday,
    home_team = normalize_team(.data$home_team),
    away_team = normalize_team(.data$away_team),
    kickoff_utc = nfl_kickoff_utc(.data$gameday, .data$gametime)
  )
cat("Games this week:", nrow(week_games), "\n")

# A pick on a game that has already started is unplaceable and would pollute
# both the ledger and the graded record. This also makes a late re-run safe:
# it simply stops offering the games that have gone off.
started <- lubridate::ymd_hms(week_games$kickoff_utc, tz = "UTC") <= Sys.time()
if (any(started)) {
  cat("Skipping", sum(started), "game(s) already kicked off.\n")
  week_games <- week_games[!started, ]
}
if (!nrow(week_games)) {
  cat("Every game this week has started. Nothing to publish.\n")
  quit(save = "no", status = 0)
}

# --------------------------------------------------------------------------
# Team form carried into the upcoming week
# --------------------------------------------------------------------------

team_games <- readRDS("data/raw/team_games.rds")
games_hist <- schedule_games(readRDS("data/raw/schedules.rds"), TRUE)

stat_cols <- setdiff(
  names(team_games),
  c("game_id", "season", "week", "game_date", "team", "opponent")
)
future_rows <- dplyr::bind_rows(
  week_games |> dplyr::transmute(
    .data$game_id, .data$season, .data$week, .data$game_date,
    team = .data$home_team, opponent = .data$away_team),
  week_games |> dplyr::transmute(
    .data$game_id, .data$season, .data$week, .data$game_date,
    team = .data$away_team, opponent = .data$home_team)
)
for (column in stat_cols) future_rows[[column]] <- NA_real_

rolled <- dplyr::bind_rows(
  team_games |> add_team_context(games_hist), future_rows
) |>
  dplyr::arrange(.data$team, .data$game_date, .data$game_id) |>
  add_rolling_features(unlist(cfg$features$rolling_windows))

feature_cols <- names(rolled)[
  stringr::str_detect(names(rolled), "_r\\d+$") |
    names(rolled) %in% c("prior_games", "rest_days", "pythagorean_win_pct")
]
side_features <- function(team_column, prefix) {
  week_games |>
    dplyr::select("game_id", team = dplyr::all_of(team_column)) |>
    dplyr::left_join(rolled, by = c("game_id", "team")) |>
    dplyr::select("game_id", dplyr::all_of(feature_cols)) |>
    dplyr::rename_with(~ paste0(prefix, .x), -dplyr::all_of("game_id"))
}

# --------------------------------------------------------------------------
# Market, restricted to the four permitted books
# --------------------------------------------------------------------------

book_map <- feed_books()
abbrev <- stats::setNames(
  names(odds_api_team_names()), unname(odds_api_team_names())
)
market_raw <- odds_api_current_game_lines("us")
cat("Odds credits used:", market_raw$quota$last,
    " remaining:", market_raw$quota$remaining, "\n")

events <- market_raw$data
quotes <- purrr::map_dfr(seq_len(nrow(events)), function(i) {
  home <- abbrev[[as.character(events$home_team[[i]])]] %||% NA_character_
  away <- abbrev[[as.character(events$away_team[[i]])]] %||% NA_character_
  if (is.na(home) || is.na(away)) return(tibble::tibble())
  books <- events$bookmakers[[i]]
  if (is.null(books) || !nrow(books)) return(tibble::tibble())
  purrr::map_dfr(seq_len(nrow(books)), function(b) {
    if (!books$key[[b]] %in% names(book_map)) return(tibble::tibble())
    markets <- books$markets[[b]]
    if (is.null(markets) || !nrow(markets)) return(tibble::tibble())
    purrr::map_dfr(seq_len(nrow(markets)), function(m) {
      outcomes <- markets$outcomes[[m]]
      if (is.null(outcomes) || !nrow(outcomes)) return(tibble::tibble())
      tibble::tibble(
        home_team = normalize_team(home), away_team = normalize_team(away),
        book = unname(book_map[[books$key[[b]]]]),
        market = as.character(markets$key[[m]]),
        outcome = as.character(outcomes$name),
        point = suppressWarnings(as.numeric(outcomes$point)),
        price = suppressWarnings(as.numeric(outcomes$price)),
        home_full = as.character(events$home_team[[i]])
      )
    })
  })
}) |>
  dplyr::filter(!is.na(.data$point), !is.na(.data$price))

if (!nrow(quotes)) stop("No quotes from the permitted books.", call. = FALSE)

quotes <- quotes |>
  dplyr::mutate(
    side = dplyr::case_when(
      .data$market == "totals" ~ tolower(.data$outcome),
      .data$outcome == .data$home_full ~ "home",
      TRUE ~ "away"
    )
  )

consensus <- quotes |>
  dplyr::group_by(.data$home_team, .data$away_team, .data$market) |>
  dplyr::summarise(line = stats::median(.data$point[.data$side %in%
    c("over", "home")]), .groups = "drop")

best_price <- quotes |>
  dplyr::group_by(.data$home_team, .data$away_team, .data$market, .data$side) |>
  dplyr::arrange(dplyr::desc(.data$price), .by_group = TRUE) |>
  dplyr::summarise(
    price = dplyr::first(.data$price), book = dplyr::first(.data$book),
    point = dplyr::first(.data$point), .groups = "drop"
  )

lines <- consensus |>
  tidyr::pivot_wider(names_from = "market", values_from = "line") |>
  dplyr::rename(home_line = "spreads", total_line = "totals") |>
  dplyr::filter(!is.na(.data$home_line), !is.na(.data$total_line))

# --------------------------------------------------------------------------
# Score
# --------------------------------------------------------------------------

train <- readRDS("data/processed/game_features.rds")
test <- week_games |>
  dplyr::left_join(side_features("home_team", "home_"), by = "game_id") |>
  dplyr::left_join(side_features("away_team", "away_"), by = "game_id") |>
  dplyr::inner_join(lines, by = c("home_team", "away_team")) |>
  dplyr::mutate(
    market_margin = -.data$home_line, market_total = .data$total_line,
    home_margin = NA_real_, game_total = NA_real_,
    neutral_temperature = 70, neutral_wind = 0
  )
cat("Games priced and featured:", nrow(test), "\n")
if (!nrow(test)) quit(save = "no", status = 0)

shared <- intersect(feature_names(train), names(test))
set.seed(cfg$backtest$seed)
total_pred <- fit_predict_model("forward_linear", train, test, "game_total",
                                cfg, features = shared)
set.seed(cfg$backtest$seed)
margin_pred <- fit_predict_model("random_forest", train, test, "home_margin",
                                 cfg, features = shared)

teams <- feed_team_lookup()
cover_table <- feed_empirical_cover_prob()

scored <- test |>
  dplyr::mutate(
    projected_total = total_pred, projected_margin = margin_pred,
    total_edge = .data$projected_total - .data$total_line,
    margin_edge = .data$projected_margin - .data$market_margin
  )

price_for <- function(home, away, market, side) {
  hit <- best_price |>
    dplyr::filter(.data$home_team == home, .data$away_team == away,
                  .data$market == !!market, .data$side == !!side)
  if (!nrow(hit)) {
    return(list(price = NA_real_, book = NA_character_, point = NA_real_))
  }
  # point is carried too: a spread bet without its number is unusable, and the
  # feed spec requires the signed line on the selection.
  list(price = hit$price[[1]], book = hit$book[[1]], point = hit$point[[1]])
}

candidates <- list()
for (i in seq_len(nrow(scored))) {
  row <- scored[i, ]
  event <- paste(unname(teams$nick[row$away_team]), "@",
                 unname(teams$nick[row$home_team]))
  slate <- format(row$game_date)

  if (abs(row$total_edge) >= 5) {
    side <- if (row$total_edge > 0) "over" else "under"
    quote <- price_for(row$home_team, row$away_team, "totals", side)
    if (!is.na(quote$price)) {
      candidates[[length(candidates) + 1L]] <- tibble::tibble(
        bet_id = sprintf("nfl-modeling-%s-%s-%s-total", slate,
                         feed_slug(row$away_team), feed_slug(row$home_team)),
        slate_date = slate, week = row$week, event = event,
        event_start = row$kickoff_utc, market = "total",
        selection = paste(if (side == "over") "Over" else "Under",
                          row$total_line),
        side = side, line = row$total_line, edge = row$total_edge,
        odds_american = as.integer(quote$price), book = quote$book,
        model_prob = feed_lookup_cover_prob("total", row$total_edge, cover_table),
        strategies = "total_fl5,portfolio"
      )
    }
  }

  if (row$margin_edge >= 6) {
    quote <- price_for(row$home_team, row$away_team, "spreads", "home")
    if (!is.na(quote$price)) {
      candidates[[length(candidates) + 1L]] <- tibble::tibble(
        bet_id = sprintf("nfl-modeling-%s-%s-%s-spread", slate,
                         feed_slug(row$away_team), feed_slug(row$home_team)),
        slate_date = slate, week = row$week, event = event,
        event_start = row$kickoff_utc, market = "spread",
        selection = unname(teams$full[row$home_team]),
        side = "home", line = quote$point, edge = row$margin_edge,
        odds_american = as.integer(quote$price), book = quote$book,
        model_prob = feed_lookup_cover_prob("spread", row$margin_edge,
                                            cover_table),
        strategies = "spread_rf6,spread_home,portfolio"
      )
    }
  }
}

# --------------------------------------------------------------------------
# Anytime touchdown, core tier only
#
# The board is refreshed by scripts/15, which prices against the four permitted
# books and applies its own 10-day window. Only the core tier is published: the
# expanded tier's validation ROI went negative once the model was seeded
# reproducibly, so it is scored and tracked but never staked.
# --------------------------------------------------------------------------

td_card_path <- "outputs/td_2026_bet_card.csv"
td_candidates <- list()
if (file.exists(td_card_path)) {
  card <- readr::read_csv(td_card_path, show_col_types = FALSE)
  core <- if (nrow(card)) {
    card |>
      dplyr::filter(
        grepl("core", tolower(dplyr::coalesce(.data$strategy_tier, ""))),
        .data$week == target_week,
        is.finite(.data$best_american_odds),
        tolower(.data$best_book) %in% tolower(unname(feed_books()))
      )
  } else card

  cat("Touchdown core-tier bets on the card:", nrow(core), "\n")
  if (nrow(core)) {
    proper_book <- stats::setNames(
      unname(feed_books()), tolower(unname(feed_books()))
    )
    for (i in seq_len(nrow(core))) {
      row <- core[i, ]
      slate <- format(as.Date(row$game_date))
      td_candidates[[length(td_candidates) + 1L]] <- tibble::tibble(
        bet_id = sprintf("nfl-modeling-%s-%s-anytimetd", slate,
                         feed_slug(row$player)),
        slate_date = slate, week = as.integer(row$week),
        event = paste(row$opponent_team, "@", row$team),
        event_start = NA_character_, market = "prop",
        market_label = "Anytime TD",
        selection = paste(row$player, "Anytime TD"),
        side = "yes", line = NA_real_,
        edge = row$relative_edge,
        odds_american = as.integer(row$best_american_odds),
        book = unname(proper_book[tolower(row$best_book)]),
        model_prob = row$model_probability,
        stat = "anytime_td", player = row$player, team = row$team,
        strategies = "td_core_upgraded,td_core_baseline"
      )
    }
  }
} else {
  cat("No touchdown bet card found; run scripts/15 first.\n")
}

fresh <- dplyr::bind_rows(c(candidates, td_candidates))
cat("Qualifying bets this run:", nrow(fresh), "\n")

# --------------------------------------------------------------------------
# Append-only ledger
# --------------------------------------------------------------------------

ledger <- if (file.exists(ledger_path)) {
  readr::read_csv(ledger_path, col_types = readr::cols(
    .default = readr::col_character(),
    week = readr::col_integer(), line = readr::col_double(),
    edge = readr::col_double(), odds_american = readr::col_integer(),
    model_prob = readr::col_double(), stake_units = readr::col_double(),
    # Declared explicitly: the character default would collide with the
    # integer column built for new rows when the two are bound together.
    best_tier = readr::col_integer()
  ))
} else {
  tibble::tibble()
}

already <- if (nrow(ledger)) ledger$bet_id else character(0)
new_bets <- fresh |> dplyr::filter(!.data$bet_id %in% already)

cat("Already published:", sum(fresh$bet_id %in% already), "\n")
cat("New this run:", nrow(new_bets), "\n")

if (nrow(new_bets)) {
  new_bets <- new_bets |>
    dplyr::mutate(
      # Touchdown props are tier 2 (positive in both validation windows but the
      # interval still spans zero); the game-level portfolio is tier 1.
      best_tier = dplyr::if_else(.data$market == "prop", 2L, 1L),
      stake_units = round(feed_stake_units(
        dplyr::if_else(.data$market == "prop", "td", .data$market),
        .data$edge, .data$best_tier
      ), 2),
      confidence = feed_confidence(.data$best_tier),
      published_at = format(Sys.time(), tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")
    )
  print(as.data.frame(
    new_bets |> dplyr::select("slate_date", "event", "market", "selection",
                              "odds_american", "book", "edge", "stake_units")
  ), digits = 4)
} else {
  cat("Nothing new to publish.\n")
}

if (!execute) {
  cat("\nDry run. Add --execute to write the feed and update the ledger.\n")
  quit(save = "no", status = 0)
}

ledger <- dplyr::bind_rows(ledger, new_bets)
readr::write_csv(ledger, ledger_path)

# Every bet for a slate is written from the ledger, so previously published
# bets keep the exact price, line and stake they were sent with.
published <- ledger |> dplyr::filter(.data$week == target_week)

# Ledger rows written before touchdown support existed have no prop columns.
# Adding them here keeps an older ledger readable instead of forcing a reset,
# which would mean republishing bets that were already sent.
for (column in c("market_label", "stat", "player", "team")) {
  if (!column %in% names(published)) published[[column]] <- NA_character_
}
for (slate in sort(unique(published$slate_date))) {
  rows <- published |> dplyr::filter(.data$slate_date == slate)
  bets <- rows |>
    dplyr::mutate(
      game_id = NA_character_,
      market_label = dplyr::coalesce(.data$market_label, NA_character_),
      stat = dplyr::coalesce(.data$stat, NA_character_),
      player = dplyr::coalesce(.data$player, NA_character_),
      team = dplyr::coalesce(.data$team, NA_character_),
      market_prob = american_to_prob(.data$odds_american),
      ev_pct = .data$model_prob * american_payout(.data$odds_american) -
        (1 - .data$model_prob),
      strategy_count = 1L
    )
  path <- write_picks_file(bets, slate, target_week, mode = mode)
  cat("Wrote", nrow(bets), "bets to", path, "\n")
}
