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

# The total model is a forward-selected linear fit. lm() and step() carry no
# RNG, so one fit is the whole distribution.
set.seed(cfg$backtest$seed)
total_pred <- fit_predict_model("forward_linear", train, test, "game_total",
                                cfg, features = shared)

# The spread model is not. Across 15 seeds the same rule ranged from -0.00% to
# +9.73% ROI with an SD of 2.99pp, and the seed that happened to be frozen in
# config sat at the 93rd percentile of that distribution - so a single fit is
# one draw, not a measurement. Averaging the predictions across seeds cuts that
# variance and backtests better than the mean single seed (+7.98% against
# +5.47%), which makes this the deployment, not a diagnostic.
#
# Averaging predictions, not bets: the threshold is applied once, to the mean,
# so a game does not qualify on a minority of seeds.
margin_seeds <- as.integer(cfg$backtest$seed) + (seq_len(15L) - 1L) * 7919L
margin_draws <- vapply(margin_seeds, function(s) {
  set.seed(s)
  fit_predict_model("random_forest", train, test, "home_margin", cfg,
                    features = shared, seed = s)
}, numeric(nrow(test)))
if (is.null(dim(margin_draws))) margin_draws <- matrix(margin_draws, nrow = 1)
margin_pred <- rowMeans(margin_draws)
cat(sprintf(
  "Spread model averaged over %d seeds; per-game prediction SD %.2f points.\n",
  length(margin_seeds), mean(apply(margin_draws, 1, stats::sd))
))

teams <- feed_team_lookup()
cover_table <- feed_empirical_cover_prob()

scored <- test |>
  dplyr::mutate(
    projected_total = total_pred, projected_margin = margin_pred,
    total_edge = .data$projected_total - .data$total_line,
    margin_edge = .data$projected_margin - .data$market_margin
  )

# Full slate, every game with a line - not just the ones that clear a betting
# threshold. Persisted so scripts/78_nfl_write_slate.R (the Extras-tab board,
# mirroring cfb-modeling's 08_write_slate.R) can show a model lean on every
# game without re-fitting the seed-averaged spread model, which is the
# expensive step here (~15-20 min for 15 ranger fits). Written unconditionally,
# even on a dry run, since it's informational and costs nothing extra to save.
slate_out_path <- sprintf("outputs/nfl_full_slate_%d_wk%d.csv", schedules_2026$season[[1]], target_week)
readr::write_csv(scored, slate_out_path)
readr::write_csv(scored, "outputs/nfl_full_slate_latest.csv")
cat("Wrote full-slate table (", nrow(scored), "games ) to", slate_out_path, "\n")

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
# Anytime touchdown: CORE and EXPANDED tiers
#
# The board is refreshed by scripts/15, which prices against the four permitted
# books and applies its own 10-day window.
#
# EXPANDED was previously withheld because "its validation ROI went negative
# once the model was seeded reproducibly". Re-measured 2026-09-29 against the
# current model - which now carries the fantasy TD signal - and with the QB,
# +400-band and -150 price-floor exclusions applied, that is no longer true:
#
#   CORE      318 bets  ROI +51.1%  +162.4u  P(ROI<=0) 0.000
#   EXPANDED  230 bets  ROI +30.7%   +70.7u  P(ROI<=0) 0.003
#   combined  548 bets  ROI +42.5%  +233.0u  P(ROI<=0) 0.000
#
# CAVEAT, recorded rather than buried: EXPANDED is materially weaker in the
# more recent season (2024 +44.1%, P=0.004; 2025 +13.9%, CI spans zero,
# P=0.144), and its heavy-favorite-only subset is marginal (+27.9%, P=0.054).
# CORE is the robust tier; EXPANDED is staked at the same per-bet size but
# should be the first thing dropped if live results disappoint.
# --------------------------------------------------------------------------

td_card_path <- "outputs/td_2026_bet_card.csv"
td_candidates <- list()
if (file.exists(td_card_path)) {
  card <- readr::read_csv(td_card_path, show_col_types = FALSE)
  core <- if (nrow(card)) {
    card |>
      dplyr::filter(
        tolower(dplyr::coalesce(.data$strategy_tier, "")) %in% c("core", "expanded"),
        .data$week == target_week,
        is.finite(.data$best_american_odds),
        tolower(.data$best_book) %in% tolower(unname(feed_books()))
      )
  } else card

  cat("Touchdown bets on the card (core + expanded):", nrow(core), "\n")
  if (nrow(core)) print(table(tolower(core$strategy_tier)))
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
        # Carried so the price floor can be set from the consensus rather than
        # from a model probability the Brier test says is worse than the
        # market's. See scripts/69.
        consensus_prob = if ("consensus_probability" %in% names(row)) {
          row$consensus_probability
        } else {
          NA_real_
        },
        stat = "anytime_td", player = row$player, team = row$team,
        strategies = "td_core_upgraded,td_core_baseline"
      )
    }
  }
} else {
  cat("No touchdown bet card found; run scripts/15 first.\n")
}

fresh <- dplyr::bind_rows(c(candidates, td_candidates))

# bind_rows() of an empty list returns a frame with no columns, so a week where
# nothing qualifies used to blow up on the ledger join rather than reporting a
# quiet week. Give it the schema so the empty path behaves like the full one -
# weeks with no qualifying bets are normal and must not look like a failure.
if (!nrow(fresh)) {
  fresh <- tibble::tibble(
    bet_id = character(), slate_date = character(), week = integer(),
    event = character(), event_start = character(), market = character(),
    market_label = character(), selection = character(), side = character(),
    line = numeric(), edge = numeric(), odds_american = integer(),
    book = character(), model_prob = numeric(), consensus_prob = numeric(),
    stat = character(), player = character(), team = character(),
    strategies = character()
  )
}
cat("Qualifying bets this run:", nrow(fresh), "\n")

# --------------------------------------------------------------------------
# Append-only ledger
# --------------------------------------------------------------------------

ledger <- if (file.exists(ledger_path)) {
  # Types are declared rather than guessed: the character default would collide
  # with the integer columns built for new rows when the two are bound
  # together. Only columns actually present are named, because readr warns
  # about parsers that match nothing, and an older ledger legitimately predates
  # some of these.
  declared <- list(
    week = readr::col_integer(), line = readr::col_double(),
    edge = readr::col_double(), odds_american = readr::col_integer(),
    model_prob = readr::col_double(), stake_units = readr::col_double(),
    best_tier = readr::col_integer(), min_price = readr::col_integer()
  )
  header <- names(readr::read_csv(ledger_path, n_max = 0,
                                  show_col_types = FALSE))
  spec <- c(list(.default = readr::col_character()),
            declared[intersect(names(declared), header)])
  readr::read_csv(ledger_path, col_types = do.call(readr::cols, spec))
} else {
  tibble::tibble()
}

# Rows written before the status/min_price/system columns existed are filled in
# from their recorded strategies rather than reset. Resetting would republish
# bets that were already sent, which is the one thing an append-only ledger is
# for. A row whose strategies cannot be resolved keeps NA and is caught at
# publish rather than guessed at.
if (nrow(ledger)) {
  if (!"system" %in% names(ledger)) {
    ledger$system <- dplyr::if_else(
      ledger$market == "prop", "touchdown", "game-markets"
    )
  }
  if (!"status" %in% names(ledger)) {
    ledger$status <- vapply(
      strsplit(dplyr::coalesce(ledger$strategies, ""), ","),
      function(tags) {
        tags <- trimws(tags[nzchar(tags)])
        if (!length(tags)) return(NA_character_)
        feed_best_status(feed_strategy_status(tags))
      },
      character(1)
    )
  }
  if (!"min_price" %in% names(ledger)) {
    ledger$min_price <- feed_min_price(ledger$odds_american, ledger$model_prob)
  }
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
      system = dplyr::if_else(.data$market == "prop", "touchdown", "game-markets"),
      status = vapply(
        strsplit(.data$strategies, ","),
        function(tags) feed_best_status(feed_strategy_status(trimws(tags))),
        character(1)
      ),
      min_price = feed_min_price(
        .data$odds_american, .data$model_prob,
        if ("consensus_prob" %in% names(new_bets)) .data$consensus_prob
        else NA_real_
      ),
      published_at = format(Sys.time(), tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")
    )

  # The price a bet was published at is frozen, so its floor must be frozen with
  # it. Catch a rejected strategy here, before it is written to the ledger and
  # becomes permanent.
  if (any(new_bets$status == "rejected")) {
    stop("Rejected-strategy bets reached the ledger; check the registry.",
         call. = FALSE)
  }
  feed_assert_books(new_bets$book, "ledger")

  print(as.data.frame(
    new_bets |> dplyr::select("slate_date", "event", "market", "selection",
                              "odds_american", "min_price", "book", "status",
                              "edge", "stake_units")
  ), digits = 4)
} else {
  cat("Nothing new to publish.\n")
}

if (!execute) {
  cat("\nDry run. Add --execute to write the feed and update the ledger.\n")
  quit(save = "no", status = 0)
}

# Harmonize column types before binding.
#
# The declared-types list above names only the columns that existed when it was
# written, and defaults everything else to character. That is fine until a new
# MARKET introduces a numeric column the ledger has never carried: every
# touchdown bet sets `consensus_prob`, which no spread/total bet ever does, so
# the column sat all-NA (read back as character) and the first TD bet died on
# "Can't combine `consensus_prob` <character> and <double>".
#
# That is not hypothetical - it is why the Tuesday publish task has been exiting
# 1, and why no touchdown bet has ever reached the feed despite the model
# qualifying them.
#
# Enumerating the missing names would just move the problem to the next market
# that adds a column, so instead: for any shared column where the new rows are
# numeric and the stored ones are not, coerce the stored side. Only ever widens
# character/logical -> numeric, and only when every existing value survives the
# cast, so a genuinely textual column is left alone.
# NOT gated on nrow(new_bets): vctrs type-checks the bind even when the new
# frame is EMPTY, so a run with nothing to add still died on the character /
# double clash. The ledger is read back with `.default = col_character()`, so
# consensus_prob returns as character on every single run regardless of what
# was written - which means this harmonization has to be unconditional.
if (nrow(ledger)) {
  for (nm in intersect(names(ledger), names(new_bets))) {
    if (is.numeric(new_bets[[nm]]) && !is.numeric(ledger[[nm]])) {
      old <- ledger[[nm]]
      cast <- suppressWarnings(as.numeric(as.character(old)))
      lost <- !is.na(as.character(old)) & is.na(cast)
      if (!any(lost)) {
        ledger[[nm]] <- cast
      } else {
        stop("Ledger column '", nm, "' is numeric in new bets but holds ",
             sum(lost), " non-numeric stored value(s); refusing to coerce.",
             call. = FALSE)
      }
    }
  }
}
ledger <- dplyr::bind_rows(ledger, new_bets)
readr::write_csv(ledger, ledger_path)

# Every bet for a slate is written from the ledger, so previously published
# bets keep the exact price, line and stake they were sent with.
published <- ledger |> dplyr::filter(.data$week == target_week)

# Ledger rows written before touchdown support existed have no prop columns.
# Adding them here keeps an older ledger readable instead of forcing a reset,
# which would mean republishing bets that were already sent.
for (column in c("market_label", "stat", "player", "team", "status", "system")) {
  if (!column %in% names(published)) published[[column]] <- NA_character_
}
if (!"min_price" %in% names(published)) published$min_price <- NA_integer_
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
