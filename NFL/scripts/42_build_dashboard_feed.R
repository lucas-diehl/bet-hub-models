source("R/utilities.R")
source("R/odds_api.R")
source("R/touchdown_features.R")
source("R/dashboard_feed.R")
assert_packages()
ensure_directories()

# Driver for the Bet Hub feed.
#
#   --slate=YYYY-MM-DD   game day to emit (required)
#   --results            also grade and write results_<date>.json
#   --mode=PAPER|LIVE    defaults to PAPER
#   --yardage-props      opt in to receiving/reception/rushing prop markets
#
# The feed carries totals, spreads and anytime touchdown. Yardage and reception
# props are built but off by default: priced at the four permitted books they
# run negative (all positive-EV bets pool to -2.77%), and the best cut, tight
# ends, reaches only +0.85% on an interval spanning zero. Strategies 12-14 stay
# implemented so the decision can be revisited without rebuilding them.
#
# Prices are re-derived from the raw per-book payloads and restricted to
# DraftKings and FanDuel, because those are the only two books the feed
# accepts. Quoting a best-of-eight price that cannot be taken at either would
# overstate every edge in the file. Model probabilities are unchanged; edge and
# EV are recomputed against the price actually offered.

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", flag, "="), "", hit[[1]])
}
slate_date <- arg_value("--slate")
if (is.null(slate_date)) stop("--slate=YYYY-MM-DD is required.", call. = FALSE)
emit_results <- "--results" %in% args
mode <- arg_value("--mode", "PAPER")
include_yardage <- "--yardage-props" %in% args

teams <- feed_team_lookup()
book_map <- feed_books()

# The historical archive stops at 2025; the current season lives in its own
# file. Both are read so a mid-season call and a backfill use the same driver.
schedule_files <- c("data/raw/schedules.rds", "data/raw/schedules_2026.rds")
schedules <- purrr::map_dfr(
  schedule_files[file.exists(schedule_files)],
  function(p) {
    readRDS(p) |>
      dplyr::filter(.data$game_type == "REG") |>
      dplyr::transmute(
        .data$game_id, .data$season, .data$week, .data$gameday, .data$gametime,
        home_team = normalize_team(.data$home_team),
        away_team = normalize_team(.data$away_team),
        home_score = as.numeric(.data$home_score),
        away_score = as.numeric(.data$away_score)
      )
  }
) |>
  dplyr::distinct(.data$game_id, .keep_all = TRUE) |>
  dplyr::mutate(kickoff_utc = nfl_kickoff_utc(.data$gameday, .data$gametime))

slate <- schedules |> dplyr::filter(as.character(.data$gameday) == slate_date)
if (!nrow(slate)) stop("No regular-season games on ", slate_date, ".", call. = FALSE)
season <- slate$season[[1]]
week <- slate$week[[1]]

slate_meta <- slate |>
  dplyr::transmute(
    .data$game_id, .data$home_team, .data$away_team,
    event = paste(
      unname(teams$nick[.data$away_team]), "@",
      unname(teams$nick[.data$home_team])
    ),
    event_start = .data$kickoff_utc
  )

cat("Slate:", slate_date, " season", season, "week", week,
    " games:", nrow(slate), "\n")

# --------------------------------------------------------------------------
# Game lines, priced at DraftKings/FanDuel from the purchased snapshots
# --------------------------------------------------------------------------

read_book_lines <- function(path) {
  payload <- jsonlite::read_json(path, simplifyVector = FALSE)
  abbrev <- stats::setNames(
    names(odds_api_team_names()), unname(odds_api_team_names())
  )
  purrr::map_dfr(payload$data %||% list(), function(event) {
    home <- abbrev[[as.character(event$home_team)]] %||% NA_character_
    away <- abbrev[[as.character(event$away_team)]] %||% NA_character_
    if (is.na(home) || is.na(away)) return(tibble::tibble())
    purrr::map_dfr(event$bookmakers %||% list(), function(book) {
      key <- as.character(book$key)
      if (!key %in% names(book_map)) return(tibble::tibble())
      rows <- list()
      for (market in book$markets %||% list()) {
        for (o in market$outcomes %||% list()) {
          rows[[length(rows) + 1L]] <- tibble::tibble(
            home_team = normalize_team(home), away_team = normalize_team(away),
            book = unname(book_map[[key]]),
            market_key = as.character(market$key),
            outcome = as.character(o$name),
            point = suppressWarnings(as.numeric(o$point)),
            price = suppressWarnings(as.numeric(o$price)),
            home_full = as.character(event$home_team)
          )
        }
      }
      dplyr::bind_rows(rows)
    })
  })
}

snapshot_dir <- file.path("data/raw/odds_api_line_movement", season)
game_bets_ready <- FALSE
if (dir.exists(snapshot_dir)) {
  wk <- sprintf("%02d", week)
  open_path <- file.path(snapshot_dir, paste0(wk, "_open_tue.json"))
  close_paths <- list.files(
    snapshot_dir, pattern = paste0("^", wk, "_close_"), full.names = TRUE
  )
  if (file.exists(open_path) && length(close_paths)) {
    open_lines <- read_book_lines(open_path)
    close_lines <- purrr::map_dfr(close_paths, read_book_lines)
    game_bets_ready <- nrow(open_lines) > 0
  }
}

pick_price <- function(lines, side_key) {
  # Best available price at DK or FD for one side of one market.
  lines |>
    dplyr::filter(!is.na(.data$price)) |>
    dplyr::group_by(.data$home_team, .data$away_team) |>
    dplyr::arrange(dplyr::desc(.data$price), .by_group = TRUE) |>
    dplyr::summarise(
      "{side_key}_odds" := dplyr::first(.data$price),
      "{side_key}_book" := dplyr::first(.data$book),
      "{side_key}_point" := dplyr::first(.data$point),
      .groups = "drop"
    )
}

games <- tibble::tibble()
if (game_bets_ready) {
  predictions <- readr::read_csv("outputs/predictions.csv", show_col_types = FALSE) |>
    dplyr::filter(
      .data$season == !!season,
      (.data$target == "game_total" & .data$model == "forward_linear") |
        (.data$target == "home_margin" & .data$model == "random_forest")
    ) |>
    dplyr::select("game_id", "target", "prediction") |>
    tidyr::pivot_wider(names_from = "target", values_from = "prediction")

  totals_over <- open_lines |>
    dplyr::filter(.data$market_key == "totals", .data$outcome == "Over") |>
    pick_price("total_over")
  totals_under <- open_lines |>
    dplyr::filter(.data$market_key == "totals", .data$outcome == "Under") |>
    pick_price("total_under")
  spread_home <- open_lines |>
    dplyr::filter(
      .data$market_key == "spreads", .data$outcome == .data$home_full
    ) |>
    pick_price("spread_home")

  games <- slate_meta |>
    dplyr::inner_join(predictions, by = "game_id") |>
    dplyr::left_join(totals_over, by = c("home_team", "away_team")) |>
    dplyr::left_join(totals_under, by = c("home_team", "away_team")) |>
    dplyr::left_join(spread_home, by = c("home_team", "away_team")) |>
    dplyr::mutate(
      total_line = .data$total_over_point,
      home_line = .data$spread_home_point,
      total_prediction = .data$game_total,
      margin_prediction = .data$home_margin,
      total_odds_over = .data$total_over_odds,
      total_odds_under = .data$total_under_odds,
      total_book_over = .data$total_over_book,
      total_book_under = .data$total_under_book,
      spread_odds_home = .data$spread_home_odds,
      spread_book_home = .data$spread_home_book
    ) |>
    dplyr::filter(!is.na(.data$total_line), !is.na(.data$home_line))
}
cat("Games priced at DK/FD:", nrow(games), "\n")

# --------------------------------------------------------------------------
# Anytime touchdown, priced at DK/FD from the raw odds archive
# --------------------------------------------------------------------------

td <- tibble::tibble()
td_path <- "data/processed/anytime_td_odds_early_2023_2025.csv"
if (file.exists(td_path)) {
  td_prices <- readr::read_csv(td_path, show_col_types = FALSE) |>
    dplyr::filter(
      .data$game_id %in% slate_meta$game_id,
      tolower(.data$outcome) == "yes",
      tolower(.data$bookmaker_key) %in% names(book_map)
    ) |>
    dplyr::mutate(
      name_key = normalize_prop_player_name(.data$player),
      # Single-bracket lookup: it is vectorised and yields NA for an unmapped
      # key, where [[ ]] errors on the empty group dplyr evaluates for types.
      book = unname(book_map[tolower(.data$bookmaker_key)])
    ) |>
    dplyr::group_by(.data$game_id, .data$name_key) |>
    dplyr::arrange(dplyr::desc(.data$american_odds), .by_group = TRUE) |>
    dplyr::summarise(
      best_american_odds = dplyr::first(.data$american_odds),
      best_book = dplyr::first(.data$book),
      .groups = "drop"
    )

  td_model <- readr::read_csv(
    "outputs/td_walk_forward_predictions.csv", show_col_types = FALSE
  ) |>
    dplyr::filter(.data$game_id %in% slate_meta$game_id) |>
    dplyr::mutate(name_key = normalize_prop_player_name(.data$player)) |>
    dplyr::select(
      "game_id", "name_key", "player", "position", "team",
      "model_probability", "total_line", "anytime_td"
    )

  td <- td_model |>
    dplyr::inner_join(td_prices, by = c("game_id", "name_key")) |>
    dplyr::inner_join(slate_meta, by = "game_id") |>
    dplyr::mutate(
      relative_edge = .data$model_probability /
        american_to_prob(.data$best_american_odds) - 1
    )
}
cat("Touchdown candidates priced at DK/FD:", nrow(td), "\n")

# --------------------------------------------------------------------------
# Yardage and reception props, priced at DK/FD
# --------------------------------------------------------------------------

props <- tibble::tibble()
prop_model_path <- "outputs/player_prop_upgraded_bets.csv"
if (include_yardage && file.exists(prop_model_path)) {
  model <- readr::read_csv(prop_model_path, show_col_types = FALSE) |>
    dplyr::filter(.data$arm == "full", .data$game_id %in% slate_meta$game_id)

  if (nrow(model)) {
    raw <- readr::read_csv(
      "data/processed/player_prop_odds.csv", show_col_types = FALSE
    ) |>
      dplyr::filter(
        .data$game_id %in% slate_meta$game_id,
        tolower(.data$bookmaker_key) %in% names(book_map)
      ) |>
      dplyr::mutate(
        target = dplyr::case_when(
          .data$market_key == "player_reception_yds" ~ "receiving_yards",
          .data$market_key == "player_receptions" ~ "receptions",
          .data$market_key == "player_rush_yds" ~ "rushing_yards",
          TRUE ~ NA_character_
        ),
        side = tolower(.data$side),
        name_key = normalize_prop_player_name(.data$player)
      ) |>
      dplyr::filter(!is.na(.data$target))

    positions <- readRDS("data/processed/fantasy_prop_features_augmented.rds") |>
      dplyr::select("game_id", "player_id", "position", "team") |>
      dplyr::distinct()

    props <- model |>
      dplyr::mutate(name_key = normalize_prop_player_name(.data$player)) |>
      dplyr::inner_join(
        raw |>
          dplyr::filter(.data$line == .data$line) |>
          dplyr::select(
            "game_id", "name_key", "target", "side", "line",
            "american_odds", "bookmaker_key"
          ),
        by = c("game_id", "name_key", "target", "side",
               "consensus_line" = "line")
      ) |>
      dplyr::group_by(.data$game_id, .data$name_key, .data$target, .data$side) |>
      dplyr::arrange(dplyr::desc(.data$american_odds), .by_group = TRUE) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        odds_american = .data$american_odds,
        book = unname(book_map[tolower(.data$bookmaker_key)]),
        p_side = dplyr::if_else(.data$side == "over", .data$p_over, 1 - .data$p_over),
        ev = .data$p_side * american_payout(.data$odds_american) -
          (1 - .data$p_side)
      ) |>
      dplyr::inner_join(positions, by = c("game_id", "player_id")) |>
      dplyr::inner_join(slate_meta, by = "game_id")
  }
}
if (include_yardage) {
  cat("Yardage prop candidates priced:", nrow(props), "\n")
} else {
  cat("Yardage props: disabled (negative at the permitted books)\n")
}

# --------------------------------------------------------------------------
# Candidates -> deduped bets -> file
# --------------------------------------------------------------------------

candidates <- dplyr::bind_rows(
  build_game_bet_candidates(games, teams, slate_date),
  build_td_bet_candidates(td, slate_date),
  build_prop_bet_candidates(props, slate_date)
)

cat("\nRaw strategy candidates:", nrow(candidates), "\n")
if (nrow(candidates)) {
  print(as.data.frame(candidates |> dplyr::count(.data$strategy, name = "candidates")))
}

bets <- dedupe_feed_bets(candidates)
cat("\nDeduped bets:", nrow(bets), "\n")
if (nrow(bets)) {
  cat("Collapsed from", nrow(candidates), "candidates;",
      sum(bets$strategy_count > 1), "bets had 2+ strategies agree.\n")
  print(as.data.frame(
    bets |>
      dplyr::count(.data$market, .data$confidence, name = "bets")
  ))
}

path <- write_picks_file(bets, slate_date, week, mode = mode)
cat("\nWrote:", path, "\n")

if (emit_results && nrow(bets)) {
  finals <- schedules |>
    dplyr::filter(.data$game_id %in% slate_meta$game_id) |>
    dplyr::transmute(
      .data$game_id,
      final_margin = as.numeric(.data$home_score) - as.numeric(.data$away_score),
      total_points = as.numeric(.data$home_score) + as.numeric(.data$away_score)
    )

  # Typed empty frames, so a disabled market still joins cleanly instead of
  # failing on a rename of columns that were never created.
  empty_actual <- tibble::tibble(
    bet_key = character(), value = numeric(), stat = character()
  )

  td_actual <- if (nrow(td)) {
    td |> dplyr::transmute(
      bet_key = paste0(.data$game_id, "|td|", feed_slug(.data$player)),
      value = as.numeric(.data$anytime_td), stat = "tds"
    )
  } else empty_actual

  prop_actual <- if (nrow(props)) {
    props |> dplyr::transmute(
      bet_key = paste0(
        .data$game_id, "|prop|", feed_slug(.data$player), "|",
        feed_prop_stat(.data$target)
      ),
      value = as.numeric(.data$actual), stat = feed_prop_stat(.data$target)
    )
  } else empty_actual

  # Closing price for CLV: the same selection at DK/FD in the closing snapshot.
  closing <- tibble::tibble()
  if (game_bets_ready) {
    c_over <- close_lines |>
      dplyr::filter(.data$market_key == "totals", .data$outcome == "Over") |>
      pick_price("c_over")
    c_under <- close_lines |>
      dplyr::filter(.data$market_key == "totals", .data$outcome == "Under") |>
      pick_price("c_under")
    c_home <- close_lines |>
      dplyr::filter(.data$market_key == "spreads", .data$outcome == .data$home_full) |>
      pick_price("c_home")
    closing <- slate_meta |>
      dplyr::left_join(c_over, by = c("home_team", "away_team")) |>
      dplyr::left_join(c_under, by = c("home_team", "away_team")) |>
      dplyr::left_join(c_home, by = c("home_team", "away_team"))
  }

  results <- bets |>
    dplyr::left_join(finals, by = "game_id") |>
    dplyr::left_join(
      td_actual |> dplyr::rename(td_value = "value", td_stat = "stat"),
      by = "bet_key"
    ) |>
    dplyr::left_join(
      prop_actual |> dplyr::rename(prop_value = "value", prop_stat = "stat"),
      by = "bet_key"
    ) |>
    dplyr::left_join(
      closing |> dplyr::select(-"event", -"event_start", -"home_team", -"away_team"),
      by = "game_id"
    ) |>
    dplyr::mutate(
      margin_result = dplyr::case_when(
        .data$market != "spread" ~ NA_character_,
        is.na(.data$final_margin) ~ "pending",
        .data$final_margin + .data$line > 0 ~ "win",
        .data$final_margin + .data$line < 0 ~ "loss",
        TRUE ~ "push"
      ),
      total_result = dplyr::case_when(
        .data$market != "total" ~ NA_character_,
        is.na(.data$total_points) ~ "pending",
        .data$total_points > .data$line ~ dplyr::if_else(.data$side == "over", "win", "loss"),
        .data$total_points < .data$line ~ dplyr::if_else(.data$side == "over", "loss", "win"),
        TRUE ~ "push"
      ),
      td_result = dplyr::case_when(
        is.na(.data$td_value) ~ NA_character_,
        .data$td_value > 0 ~ "win",
        TRUE ~ "loss"
      ),
      prop_result = dplyr::case_when(
        is.na(.data$prop_value) ~ NA_character_,
        .data$prop_value > .data$line ~ dplyr::if_else(.data$side == "over", "win", "loss"),
        .data$prop_value < .data$line ~ dplyr::if_else(.data$side == "over", "loss", "win"),
        TRUE ~ "push"
      ),
      result = dplyr::coalesce(
        .data$margin_result, .data$total_result, .data$td_result,
        .data$prop_result, "pending"
      ),
      closing_odds_american = dplyr::case_when(
        .data$market == "spread" ~ .data$c_home_odds,
        .data$market == "total" & .data$side == "over" ~ .data$c_over_odds,
        .data$market == "total" ~ .data$c_under_odds,
        TRUE ~ NA_real_
      ),
      pnl_units = dplyr::case_when(
        .data$result == "win" ~ .data$stake_units *
          american_payout(.data$odds_american),
        .data$result == "loss" ~ -.data$stake_units,
        TRUE ~ 0
      )
    )

  results$actual <- lapply(seq_len(nrow(results)), function(i) {
    row <- results[i, ]
    if (row$market == "spread") return(list(final_margin = row$final_margin))
    if (row$market == "total") return(list(total_points = row$total_points))
    if (!is.na(row$td_value)) return(stats::setNames(list(row$td_value), "tds"))
    if (!is.na(row$prop_value)) {
      return(stats::setNames(list(row$prop_value), row$prop_stat))
    }
    list()
  })

  rpath <- write_results_file(results, slate_date)
  cat("Wrote:", rpath, "\n")
  print(as.data.frame(results |> dplyr::count(.data$market, .data$result)))
  cat("Net units:", round(sum(results$pnl_units, na.rm = TRUE), 3), "\n")
}
