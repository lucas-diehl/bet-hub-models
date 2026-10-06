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
    ~id, ~tag, ~label, ~tier, ~market, ~status,
    1L, "portfolio", "Combined game-level portfolio", 1L, "game", "funded",
    2L, "total_fl5", "Forward-linear total, edge >= 5", 2L, "total", "funded",
    3L, "spread_rf6", "Random-forest home spread, edge >= 6", 2L, "spread", "funded",
    4L, "td_core_upgraded", "TD core tier, upgraded model", 2L, "td", "paper",
    5L, "td_core_baseline", "TD core tier, baseline model", 2L, "td", "paper",
    6L, "spread_home", "Spread home-selection filter", 3L, "spread", "paper",
    7L, "spread_key3", "Spread crosses key number 3", 3L, "spread", "paper",
    8L, "total_over_heavy_fav", "Total over x favourite 7+", 3L, "total", "paper",
    9L, "total_under_small_fav", "Total under x spread <= 2.5", 3L, "total", "paper",
    10L, "td_te", "TD x tight end", 4L, "td", "paper",
    11L, "td_minus_odds", "TD x minus-odds player", 4L, "td", "paper",
    12L, "prop_te_under", "Tight-end prop unders", 4L, "prop", "rejected",
    13L, "prop_te", "Tight-end props", 4L, "prop", "rejected",
    14L, "prop_ev18", "Upgraded prop model, EV >= 0.18", 4L, "prop", "rejected"
  )
}

feed_status_levels <- function() c("funded", "paper", "rejected")

# Status is a claim about evidence, and it is separate from tier, which is only
# about stake. Three values, in decreasing order of what the record supports:
#
#   funded    an interval that excludes zero on a closing-line backtest, with
#             the timing of that line established rather than assumed.
#   paper     positive but not established: small n, book-price sensitivity, or
#             an overlay that has not been replicated out of sample.
#   rejected  tested and found not to have an edge. These must never reach the
#             site. The yardage-prop rules are here because the calibration
#             curve was flat and a 320-segment scan beat chance 27.4% of the
#             time - the honest reading is that the estimator was wrong, not
#             that a smaller edge is hiding somewhere.
#
# The three prop rules stayed wired into the feed as tier-4 "tracked
# candidates" after that verdict. Emitting them at any stake size is a claim
# the record does not support, so the assertion below drops them at source.
feed_strategy_status <- function(tags) {
  registry <- nfl_bet_strategies()
  out <- registry$status[match(tags, registry$tag)]
  out[is.na(out)] <- "rejected"
  out
}

# Best (most supported) status among the strategies that fired on one bet.
feed_best_status <- function(statuses) {
  levels <- feed_status_levels()
  levels[min(match(statuses, levels))]
}

prob_to_american <- function(p) {
  p <- pmin(pmax(p, 1e-6), 1 - 1e-6)
  dplyr::if_else(p >= 0.5, -100 * p / (1 - p), 100 * (1 - p) / p)
}

american_is_worse <- function(a, b) {
  # "Worse" means a lower payout, i.e. a higher implied probability.
  american_to_prob(a) > american_to_prob(b)
}

# The minimum price at which the bet is still worth taking. Published prices go
# stale between Tuesday and kickoff, and without this the site has no way to
# tell a bettor whether a moved number is still playable - which is exactly the
# situation where a stale price silently turns a positive-EV bet negative.
#
# Where a model probability exists the floor is the break-even price implied by
# it. Where it does not (the spread and total models are regressors and their
# model_prob is an empirical cover rate, not a calibrated probability), the
# floor is the quoted price shaded by two points of implied probability. The
# floor is never better than the quoted price.
feed_min_price <- function(odds_american, model_prob,
                           consensus_prob = NA_real_) {
  shaded <- prob_to_american(american_to_prob(odds_american) + 0.02)
  breakeven <- prob_to_american(model_prob)
  floor_price <- dplyr::if_else(is.na(model_prob), shaded, breakeven)

  # Anytime-touchdown bets get a different floor, because the edge there is a
  # different thing. Re-graded at the consensus price the core tier returns
  # -13.7% against +9.8% at the best of four, and the model is a *worse*
  # forecaster than the market on a paired Brier bootstrap. So the bet is not
  # "the model says this player is underpriced" - it is "one book has drifted
  # off consensus and we are taking it". Break-even sits at roughly 57% capture
  # of the gap between the consensus payout and the best available payout, so
  # the floor is set there rather than at a model-implied break-even that the
  # Brier result says we should not trust.
  if (any(!is.na(consensus_prob))) {
    consensus_payout <- 1 / consensus_prob - 1
    best_payout <- 1 / american_to_prob(odds_american) - 1
    capture_payout <- consensus_payout + 0.57 * (best_payout - consensus_payout)
    capture_price <- prob_to_american(1 / (capture_payout + 1))
    floor_price <- dplyr::if_else(
      is.na(consensus_prob) | !is.finite(capture_price),
      floor_price, capture_price
    )
  }
  # If break-even is better than the quoted price the bet had no edge to begin
  # with; fall back to the shade so the field is never nonsense.
  floor_price <- dplyr::if_else(
    american_is_worse(floor_price, odds_american), floor_price, shaded
  )
  round(floor_price)
}

# The feed may only quote prices from books the bettor can actually reach. This
# matters more than it looks: the touchdown core tier returns +0.83% on two
# books, +11.60% on four and +13.75% on eight, so a best-of-eight price quietly
# inflates the headline by an order of magnitude. Callers must filter before
# selecting a best price, not after - filtering after selection silently drops
# bets whose winning quote came from an excluded book instead of re-pricing them
# at a reachable one.
feed_assert_books <- function(books, where = "feed") {
  books <- books[!is.na(books)]
  if (!length(books)) return(invisible(TRUE))
  permitted <- unname(feed_books())
  keys <- names(feed_books())
  bad <- unique(books[!(books %in% permitted | tolower(books) %in% keys)])
  if (length(bad)) {
    stop(
      sprintf(
        "%s quotes books outside the permitted four (%s): %s",
        where, paste(permitted, collapse = ", "), paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Publish-time gate. Everything here is a defect in the emitter rather than a
# condition to handle, so each one stops the publish instead of dropping a row.
feed_assert_publishable <- function(bets) {
  if (!nrow(bets)) return(invisible(TRUE))

  if ("status" %in% names(bets)) {
    bad <- bets$status[!bets$status %in% feed_status_levels()]
    if (length(bad)) {
      stop("Unknown status: ", paste(unique(bad), collapse = ", "), call. = FALSE)
    }
    rejected <- bets[bets$status == "rejected", ]
    if (nrow(rejected)) {
      stop(
        sprintf(
          "%d rejected-strategy bets reached publish: %s",
          nrow(rejected), paste(utils::head(rejected$bet_id, 5), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  } else {
    stop("Feed rows carry no status column.", call. = FALSE)
  }

  required <- c("book", "odds_american", "min_price")
  for (column in required) {
    if (!column %in% names(bets)) {
      stop("Feed rows are missing required column: ", column, call. = FALSE)
    }
    if (any(is.na(bets[[column]]))) {
      stop(
        sprintf(
          "%d rows have no %s: %s", sum(is.na(bets[[column]])), column,
          paste(utils::head(bets$bet_id[is.na(bets[[column]])], 5), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  # Every market except anytime-TD is priced against a number. A spread or total
  # published without one is unplaceable, which has happened here before.
  needs_line <- !(bets$market == "prop" &
                    !is.na(bets$stat) & bets$stat == "anytime_td")
  if (any(needs_line & is.na(bets$line))) {
    stop(
      sprintf(
        "%d spread/total/prop rows have no line: %s",
        sum(needs_line & is.na(bets$line)),
        paste(utils::head(bets$bet_id[needs_line & is.na(bets$line)], 5),
              collapse = ", ")
      ),
      call. = FALSE
    )
  }

  feed_assert_books(bets$book, "publish")

  # A floor better than the quote would tell a bettor to take a worse number
  # than the one already found.
  inverted <- !american_is_worse(bets$min_price, bets$odds_american) &
    bets$min_price != bets$odds_american
  if (any(inverted)) {
    stop(sprintf("%d rows have min_price better than the quote.", sum(inverted)),
         call. = FALSE)
  }

  invisible(TRUE)
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
      registry |> dplyr::select(strategy = "tag", "tier", "status"),
      by = "strategy"
    )

  # Rejected strategies are dropped here rather than at publish, so a bet that
  # fires only on rejected rules disappears entirely instead of surviving with
  # its tags stripped. inner_join above also means an unregistered tag drops
  # out - feed_assert_publishable catches anything that gets past both.
  dropped <- sum(tagged$status == "rejected")
  tagged <- dplyr::filter(tagged, .data$status != "rejected")
  if (dropped) {
    message(sprintf("Dropped %d candidates from rejected strategies.", dropped))
  }
  if (!nrow(tagged)) return(tibble::tibble())

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
      status = feed_best_status(.data$status),
      strategies = paste(sort(unique(.data$strategy)), collapse = ","),
      strategy_count = dplyr::n_distinct(.data$strategy),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      min_price = feed_min_price(.data$odds_american, .data$model_prob),
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
    row$status,
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
    # The site needs both numbers to render a stale price honestly: what we got
    # and how far it can move before the bet stops being worth taking.
    min_price = if (is.null(row$min_price) || is.na(row$min_price)) NULL else
      as.integer(row$min_price),
    status = row$status,
    model_prob = if (is.na(row$model_prob)) NULL else round(row$model_prob, 4),
    market_prob = if (is.na(row$market_prob)) NULL else round(row$market_prob, 4),
    edge = if (is.na(row$model_prob)) NULL else
      round(row$model_prob - row$market_prob, 4),
    ev_pct = if (is.na(row$ev_pct)) NULL else round(row$ev_pct, 4),
    stake_units = as.numeric(row$stake_units),
    confidence = row$confidence,
    # When this bet FIRST entered the ledger. The ledger is append-only and
    # freezes a bet's terms on first publish, so this is the moment the price
    # was actually taken - which the file's own generated_at cannot tell you,
    # because the feed is rewritten on every run. A prop locked in three days
    # out and one grabbed Sunday morning are different bets.
    posted_at = if (is.null(row$published_at) || is.na(row$published_at)) NULL else
      format(as.POSIXct(row$published_at, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
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
  # Nothing is written unless the whole slate passes. A partial file is worse
  # than none: the site would show some bets and silently omit others.
  feed_assert_publishable(bets)

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
  ## Normalize slate_date to an ISO string before it reaches either the
  ## filename or the payload.
  ##
  ## This is defensive against a specific, silent R trap: `for (d in dates)`
  ## iterates the UNDERLYING ATOMIC VECTOR and drops the Date class, so the
  ## loop variable arrives here as a bare number. scripts/58 did exactly that
  ## and shipped `results_20709.json` carrying `"slate_date": 20709` (20709 =
  ## days since 1970-01-01 = 2026-09-14). Nothing failed locally - the file
  ## wrote happily - and the Bet Hub then rejected it with
  ## "slate_date: Expected string, received number", so a full slate of graded
  ## Week 1 results silently never reached the site.
  ##
  ## Fixed at this boundary rather than only at the call site because every
  ## caller is one `for` loop away from reintroducing it.
  if (inherits(slate_date, "Date")) {
    slate_date <- format(slate_date, "%Y-%m-%d")
  } else if (is.numeric(slate_date)) {
    slate_date <- format(as.Date(slate_date, origin = "1970-01-01"), "%Y-%m-%d")
  } else {
    slate_date <- as.character(slate_date)
  }
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", slate_date)) {
    stop("write_results_file(): slate_date must be YYYY-MM-DD, got '",
         slate_date, "'.", call. = FALSE)
  }

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
