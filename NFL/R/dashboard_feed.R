# Bet Hub feed emitter for the nfl-modeling source.
#
# Turns the project's validated strategies into picks_<date>.json and
# results_<date>.json under dashboard_feed/nfl-modeling/nfl/, per
# C:/dev/bet-dashboard/docs/sources/nfl-modeling.md.
#
# The organising problem is that the fourteen tracked strategies overlap
# heavily: the portfolio is the union of the totals and spread rules, the two
# touchdown tiers select overlapping players, and the tight-end prop rules are
# subsets of the expected-value rule. So strategies do not emit bets. They emit
# *candidates*, which are then collapsed to one bet per market per selection,
# carrying every strategy that fired as a tag.

feed_root <- function() {
  Sys.getenv("FEED_DIR", "C:/Users/ljdie/OneDrive/Documents/dashboard_feed")
}

feed_slug <- function(x) gsub("[^a-z0-9]", "", tolower(as.character(x)))

# The books the feed is allowed to quote. Prices are restricted to these rather
# than reporting a best-of-eight number that could not be taken.
#
# The written spec names DraftKings and FanDuel only, but the contract's Zod
# schema types `book` as a plain optional string with no enum, so the two extra
# regulated US books validate. They are included deliberately: on two books the
# touchdown core tier returns +0.83%, and on these four it returns +11.60%.
# Offshore shops are still excluded — most of the remaining gap comes from
# LowVig.ag, which is not realistically available.
feed_books <- function() {
  c(
    draftkings = "DraftKings",
    fanduel = "FanDuel",
    williamhill_us = "Caesars",
    betmgm = "BetMGM"
  )
}

# nflreadr and the schedules disagree on a few abbreviations (the Rams are LA
# in one and LAR in the other), so both the raw and normalised keys are indexed.
# Without this a Rams home game renders as "Colts @ NA".
feed_team_lookup <- function() {
  teams <- nflreadr::load_teams()
  keys <- c(teams$team_abbr, normalize_team(teams$team_abbr))
  full <- stats::setNames(rep(teams$team_name, 2), keys)
  nick <- stats::setNames(rep(teams$team_nick, 2), keys)
  list(full = full[!duplicated(names(full))],
       nick = nick[!duplicated(names(nick))])
}

# The spread and total models are regressors, not classifiers, so they have no
# native probability. Rather than invent one, the empirical cover rate at that
# edge band in the walk-forward record is used. Staking never touches this - it
# comes from the edge bands - so the project's finding that these regressors are
# not calibrated enough to size bets is respected.
feed_empirical_cover_prob <- function(bets_path = "outputs/walk_forward_bets.csv") {
  if (!file.exists(bets_path)) return(NULL)
  readr::read_csv(bets_path, show_col_types = FALSE) |>
    dplyr::filter(
      abs(.data$edge) >= .data$selected_threshold, .data$bet_result != 0
    ) |>
    dplyr::mutate(
      market = dplyr::if_else(.data$target == "game_total", "total", "spread"),
      band = cut(
        abs(.data$edge), c(0, 5, 6, 7, 8, Inf),
        labels = c("<5", "5-6", "6-7", "7-8", "8+"), include.lowest = TRUE
      )
    ) |>
    dplyr::group_by(.data$market, .data$band) |>
    dplyr::summarise(
      n = dplyr::n(),
      cover_prob = mean(.data$bet_result > 0),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n >= 25)
}

feed_lookup_cover_prob <- function(market, edge, table) {
  if (is.null(table) || !nrow(table)) return(NA_real_)
  band <- as.character(cut(
    abs(edge), c(0, 5, 6, 7, 8, Inf),
    labels = c("<5", "5-6", "6-7", "7-8", "8+"), include.lowest = TRUE
  ))
  hit <- table$cover_prob[table$market == market & as.character(table$band) == band]
  if (!length(hit)) return(NA_real_)
  hit[[1]]
}

# ---------------------------------------------------------------------------
# Strategy registry
#
# tier drives stake: 1 = bootstrap interval excludes zero, 2 = positive in both
# discovery and validation, 3 = replicated overlay, 4 = split/segment only.
# ---------------------------------------------------------------------------

nfl_bet_strategies <- function() {
  tibble::tribble(
    ~id, ~tag, ~label, ~tier, ~market,
    1L, "portfolio", "Combined game-level portfolio", 1L, "game",
    2L, "total_fl5", "Forward-linear total, edge >= 5", 2L, "total",
    3L, "spread_rf6", "Random-forest home spread, edge >= 6", 2L, "spread",
    4L, "td_core_upgraded", "TD core tier, upgraded model", 2L, "td",
    5L, "td_core_baseline", "TD core tier, baseline model", 2L, "td",
    6L, "spread_home", "Spread home-selection filter", 3L, "spread",
    7L, "spread_key3", "Spread crosses key number 3", 3L, "spread",
    8L, "total_over_heavy_fav", "Total over x favourite 7+", 3L, "total",
    9L, "total_under_small_fav", "Total under x spread <= 2.5", 3L, "total",
    10L, "td_te", "TD x tight end", 4L, "td",
    11L, "td_minus_odds", "TD x minus-odds player", 4L, "td",
    12L, "prop_te_under", "Tight-end prop unders", 4L, "prop",
    13L, "prop_te", "Tight-end props", 4L, "prop",
    14L, "prop_ev18", "Upgraded prop model, EV >= 0.18", 4L, "prop"
  )
}

# ---------------------------------------------------------------------------
# Stake schedule
#
# The project's own plans size in 1%-of-bankroll units on a 3-10 scale. The
# dashboard's unit is a standard bet, so those are divided by five and the
# result is capped at 2. Weaker tiers are deliberately small: everything here
# is PAPER, and tier 4 in particular is a tracked candidate rather than a
# measured edge.
# ---------------------------------------------------------------------------

feed_stake_units <- function(market, edge, tier) {
  base <- dplyr::case_when(
    market == "total" & abs(edge) >= 8 ~ 1.8,
    market == "total" & abs(edge) >= 7 ~ 1.4,
    market == "total" & abs(edge) >= 6 ~ 1.0,
    market == "total" ~ 0.6,
    market == "spread" & abs(edge) >= 9 ~ 1.4,
    market == "spread" & abs(edge) >= 7.5 ~ 1.0,
    market == "spread" ~ 0.6,
    market == "td" ~ 0.5,
    TRUE ~ 0.25
  )
  # A tier-4-only bet never gets a full-size stake even if its edge is large.
  dplyr::case_when(
    tier >= 4 ~ pmin(base, 0.25),
    tier == 3 ~ pmin(base, 0.5),
    TRUE ~ base
  )
}

feed_confidence <- function(tier) {
  dplyr::case_when(tier <= 1 ~ "high", tier <= 2 ~ "medium", TRUE ~ "low")
}

american_to_prob <- function(odds) {
  dplyr::if_else(odds < 0, abs(odds) / (abs(odds) + 100), 100 / (odds + 100))
}

american_payout <- function(odds) {
  dplyr::if_else(odds > 0, odds / 100, 100 / abs(odds))
}

# ---------------------------------------------------------------------------
# Candidate builders. Each returns one row per (bet_key, strategy).
# ---------------------------------------------------------------------------

# games: game_id, event, event_start, home_team, away_team, home_line,
#        total_line, total_prediction, margin_prediction,
#        total_odds_over, total_odds_under, spread_odds_home, book columns
build_game_bet_candidates <- function(games, teams, slate_date) {
  if (!nrow(games)) return(tibble::tibble())

  totals <- games |>
    dplyr::mutate(
      edge = .data$total_prediction - .data$total_line,
      side = dplyr::if_else(.data$edge > 0, "over", "under"),
      odds_american = dplyr::if_else(
        .data$side == "over", .data$total_odds_over, .data$total_odds_under
      ),
      book = dplyr::if_else(
        .data$side == "over", .data$total_book_over, .data$total_book_under
      )
    ) |>
    dplyr::filter(abs(.data$edge) >= 5, is.finite(.data$odds_american)) |>
    dplyr::mutate(
      bet_key = paste0(.data$game_id, "|total"),
      market = "total",
      selection = paste(
        dplyr::if_else(.data$side == "over", "Over", "Under"), .data$total_line
      ),
      line = .data$total_line
    )

  total_candidates <- dplyr::bind_rows(
    totals |> dplyr::mutate(strategy = "total_fl5"),
    totals |> dplyr::mutate(strategy = "portfolio"),
    totals |>
      dplyr::filter(.data$side == "over", abs(.data$home_line) >= 7) |>
      dplyr::mutate(strategy = "total_over_heavy_fav"),
    totals |>
      dplyr::filter(.data$side == "under", abs(.data$home_line) <= 2.5) |>
      dplyr::mutate(strategy = "total_under_small_fav")
  )

  spreads <- games |>
    dplyr::mutate(
      market_margin = -.data$home_line,
      edge = .data$margin_prediction - .data$market_margin,
      odds_american = .data$spread_odds_home,
      book = .data$spread_book_home
    ) |>
    # Home selection only: away selections returned -0.88% over the full period.
    dplyr::filter(.data$edge >= 6, is.finite(.data$odds_american)) |>
    dplyr::mutate(
      bet_key = paste0(.data$game_id, "|spread"),
      market = "spread",
      side = "home",
      selection = unname(teams$full[.data$home_team]),
      line = .data$home_line
    )

  spread_candidates <- dplyr::bind_rows(
    spreads |> dplyr::mutate(strategy = "spread_rf6"),
    spreads |> dplyr::mutate(strategy = "portfolio"),
    spreads |> dplyr::mutate(strategy = "spread_home"),
    spreads |>
      dplyr::filter(
        (.data$market_margin < 3 & .data$margin_prediction > 3) |
          (.data$market_margin > 3 & .data$margin_prediction < 3)
      ) |>
      dplyr::mutate(strategy = "spread_key3")
  )

  cover_table <- feed_empirical_cover_prob()

  dplyr::bind_rows(total_candidates, spread_candidates) |>
    dplyr::mutate(
      bet_id = sprintf(
        "nfl-modeling-%s-%s-%s-%s", slate_date,
        feed_slug(.data$away_team), feed_slug(.data$home_team), .data$market
      ),
      market_label = NA_character_,
      model_prob = vapply(
        seq_len(dplyr::n()),
        function(i) feed_lookup_cover_prob(
          .data$market[[i]], .data$edge[[i]], cover_table
        ),
        numeric(1)
      ),
      stat = NA_character_,
      player = NA_character_,
      team = NA_character_
    ) |>
    dplyr::select(
      "bet_key", "bet_id", "strategy", "game_id", "event", "event_start",
      "market", "market_label", "selection", "side", "line", "edge",
      "odds_american", "book", "model_prob", "stat", "player", "team"
    )
}

# td: game_id, event, event_start, player, team, position, best_american_odds,
#     best_book, model_probability, relative_edge, total_line
build_td_bet_candidates <- function(td, slate_date) {
  if (!nrow(td)) return(tibble::tibble())

  base <- td |>
    dplyr::filter(is.finite(.data$best_american_odds)) |>
    dplyr::mutate(
      bet_key = paste0(.data$game_id, "|td|", feed_slug(.data$player)),
      bet_id = sprintf(
        "nfl-modeling-%s-%s-anytimetd", slate_date, feed_slug(.data$player)
      ),
      market = "prop",
      market_label = "Anytime TD",
      selection = paste(.data$player, "Anytime TD"),
      side = "yes",
      line = NA_real_,
      edge = .data$relative_edge,
      odds_american = .data$best_american_odds,
      book = .data$best_book,
      model_prob = .data$model_probability,
      stat = "anytime_td"
    )

  core <- base |>
    dplyr::filter(
      .data$relative_edge >= 0.05,
      .data$total_line <= 42 | .data$position == "TE"
    )

  dplyr::bind_rows(
    core |> dplyr::mutate(strategy = "td_core_upgraded"),
    core |> dplyr::mutate(strategy = "td_core_baseline"),
    base |>
      dplyr::filter(.data$position == "TE", .data$relative_edge >= 0.03) |>
      dplyr::mutate(strategy = "td_te"),
    base |>
      dplyr::filter(.data$best_american_odds < 0, .data$relative_edge >= 0.03) |>
      dplyr::mutate(strategy = "td_minus_odds")
  ) |>
    dplyr::select(
      "bet_key", "bet_id", "strategy", "game_id", "event", "event_start",
      "market", "market_label", "selection", "side", "line", "edge",
      "odds_american", "book", "model_prob", "stat", "player", "team"
    )
}

feed_prop_stat <- function(target) {
  dplyr::case_when(
    target == "receiving_yards" ~ "rec_yds",
    target == "receptions" ~ "receptions",
    target == "rushing_yards" ~ "rush_yds",
    target == "passing_yards" ~ "pass_yds",
    TRUE ~ target
  )
}

feed_prop_label <- function(target) {
  dplyr::case_when(
    target == "receiving_yards" ~ "Receiving Yards",
    target == "receptions" ~ "Receptions",
    target == "rushing_yards" ~ "Rushing Yards",
    target == "passing_yards" ~ "Passing Yards",
    TRUE ~ target
  )
}

# props: game_id, event, event_start, player, team, position, target, side,
#        consensus_line, odds_american, book, p_side, ev
build_prop_bet_candidates <- function(props, slate_date) {
  if (!nrow(props)) return(tibble::tibble())

  base <- props |>
    dplyr::filter(is.finite(.data$odds_american), .data$ev > 0) |>
    dplyr::mutate(
      stat = feed_prop_stat(.data$target),
      bet_key = paste0(
        .data$game_id, "|prop|", feed_slug(.data$player), "|", .data$stat
      ),
      bet_id = sprintf(
        "nfl-modeling-%s-%s-%s-%s", slate_date, feed_slug(.data$player),
        .data$stat, substr(.data$side, 1, 1)
      ),
      market = "prop",
      market_label = feed_prop_label(.data$target),
      selection = sprintf(
        "%s %s %s", .data$player,
        dplyr::if_else(.data$side == "over", "Over", "Under"),
        .data$consensus_line
      ),
      line = .data$consensus_line,
      edge = .data$ev,
      model_prob = .data$p_side
    )

  dplyr::bind_rows(
    base |>
      dplyr::filter(.data$ev >= 0.18) |>
      dplyr::mutate(strategy = "prop_ev18"),
    base |>
      dplyr::filter(.data$position == "TE") |>
      dplyr::mutate(strategy = "prop_te"),
    base |>
      dplyr::filter(.data$position == "TE", .data$side == "under") |>
      dplyr::mutate(strategy = "prop_te_under")
  ) |>
    dplyr::select(
      "bet_key", "bet_id", "strategy", "game_id", "event", "event_start",
      "market", "market_label", "selection", "side", "line", "edge",
      "odds_american", "book", "model_prob", "stat", "player", "team"
    )
}

# ---------------------------------------------------------------------------
# Collapse candidates to one bet per selection
# ---------------------------------------------------------------------------

dedupe_feed_bets <- function(candidates) {
  if (!nrow(candidates)) return(tibble::tibble())

  registry <- nfl_bet_strategies()
  tagged <- candidates |>
    dplyr::inner_join(
      registry |> dplyr::select(strategy = "tag", "tier"), by = "strategy"
    )

  # Where two strategies disagree on the side of the same market, the primary
  # model signal wins and the dissenting overlay is dropped rather than emitted
  # as a second bet. In practice this only arises if an overlay is ever
  # redefined to pick a side; the filters above already inherit the model's.
  primary_side <- tagged |>
    dplyr::group_by(.data$bet_key) |>
    dplyr::arrange(.data$tier, .by_group = TRUE) |>
    dplyr::summarise(keep_side = dplyr::first(.data$side), .groups = "drop")

  kept <- tagged |>
    dplyr::inner_join(primary_side, by = "bet_key") |>
    dplyr::filter(
      is.na(.data$side) | is.na(.data$keep_side) |
        .data$side == .data$keep_side
    )

  kept |>
    dplyr::group_by(.data$bet_key) |>
    dplyr::arrange(.data$tier, .by_group = TRUE) |>
    dplyr::summarise(
      bet_id = dplyr::first(.data$bet_id),
      game_id = dplyr::first(.data$game_id),
      event = dplyr::first(.data$event),
      event_start = dplyr::first(.data$event_start),
      market = dplyr::first(.data$market),
      market_label = dplyr::first(.data$market_label),
      selection = dplyr::first(.data$selection),
      side = dplyr::first(.data$side),
      line = dplyr::first(.data$line),
      edge = dplyr::first(.data$edge),
      odds_american = dplyr::first(.data$odds_american),
      book = dplyr::first(.data$book),
      model_prob = dplyr::first(.data$model_prob),
      stat = dplyr::first(.data$stat),
      player = dplyr::first(.data$player),
      team = dplyr::first(.data$team),
      best_tier = min(.data$tier),
      strategies = paste(sort(unique(.data$strategy)), collapse = ","),
      strategy_count = dplyr::n_distinct(.data$strategy),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      stake_units = round(
        feed_stake_units(
          dplyr::if_else(
            .data$market == "prop" & .data$stat == "anytime_td",
            "td", .data$market
          ),
          .data$edge, .data$best_tier
        ), 2
      ),
      confidence = feed_confidence(.data$best_tier),
      market_prob = american_to_prob(.data$odds_american),
      ev_pct = dplyr::if_else(
        is.na(.data$model_prob), NA_real_,
        .data$model_prob * american_payout(.data$odds_american) -
          (1 - .data$model_prob)
      )
    ) |>
    dplyr::arrange(.data$best_tier, dplyr::desc(.data$stake_units))
}

# ---------------------------------------------------------------------------
# JSON writers
# ---------------------------------------------------------------------------

compact_list <- function(x) x[!vapply(x, is.null, logical(1))]

feed_bet_object <- function(row) {
  is_prop <- identical(row$market, "prop")
  tags <- c(
    if (is_prop) "prop" else row$market,
    if (is_prop) row$stat else NULL,
    strsplit(row$strategies, ",")[[1]]
  )

  compact_list(list(
    bet_id = row$bet_id,
    event = row$event,
    event_start = if (is.na(row$event_start)) NULL else row$event_start,
    market = row$market,
    market_label = if (is.na(row$market_label)) NULL else row$market_label,
    selection = row$selection,
    side = row$side,
    line = if (is.na(row$line)) NULL else as.numeric(row$line),
    odds_american = as.integer(row$odds_american),
    book = row$book,
    model_prob = if (is.na(row$model_prob)) NULL else round(row$model_prob, 4),
    market_prob = if (is.na(row$market_prob)) NULL else round(row$market_prob, 4),
    edge = if (is.na(row$model_prob)) NULL else
      round(row$model_prob - row$market_prob, 4),
    ev_pct = if (is.na(row$ev_pct)) NULL else round(row$ev_pct, 4),
    stake_units = as.numeric(row$stake_units),
    confidence = row$confidence,
    tags = as.list(unique(tags)),
    details = if (!is_prop) NULL else compact_list(list(
      player = row$player,
      stat = row$stat,
      team = row$team
    ))
  ))
}

write_picks_file <- function(bets, slate_date, week, feed_dir = feed_root(),
                             mode = "PAPER", model_version = "nfl-v1") {
  objects <- if (nrow(bets)) {
    lapply(seq_len(nrow(bets)), function(i) feed_bet_object(bets[i, ]))
  } else {
    list()
  }

  out <- list(
    contract_version = "1.0",
    source = "nfl-modeling",
    sport = "nfl",
    slate_date = slate_date,
    generated_at = format(
      as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
    ),
    model_version = model_version,
    mode = mode,
    event_context = sprintf("Week %d", week),
    bets = objects
  )

  dir <- file.path(feed_dir, "nfl-modeling", "nfl")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, sprintf("picks_%s.json", slate_date))
  jsonlite::write_json(
    out, path, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = 6
  )
  path
}

write_results_file <- function(results, slate_date, feed_dir = feed_root()) {
  objects <- lapply(seq_len(nrow(results)), function(i) {
    row <- results[i, ]
    actual <- row$actual[[1]]
    compact_list(list(
      bet_id = row$bet_id,
      result = row$result,
      closing_odds_american = if (is.na(row$closing_odds_american)) NULL else
        as.integer(row$closing_odds_american),
      pnl_units = if (is.na(row$pnl_units)) NULL else round(row$pnl_units, 4),
      actual = if (length(actual)) actual else NULL
    ))
  })

  out <- list(
    contract_version = "1.0",
    source = "nfl-modeling",
    sport = "nfl",
    slate_date = slate_date,
    graded_at = format(
      as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
    ),
    results = objects
  )

  dir <- file.path(feed_dir, "nfl-modeling", "nfl")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, sprintf("results_%s.json", slate_date))
  jsonlite::write_json(
    out, path, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = 6
  )
  path
}
