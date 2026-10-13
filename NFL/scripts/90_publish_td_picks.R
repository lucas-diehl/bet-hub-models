source("R/utilities.R")
source("R/dashboard_feed.R")
assert_packages()
ensure_directories()

## Day-scoped anytime-TD publish for the Bet Hub feed.
##
## Split out of scripts/57_publish_weekly_picks.R on 2026-10-13. That script's
## TD block staked every qualifying prop for the whole WEEK in one shot,
## whenever script 57 happened to run (Tue + Sun). The site is now posting TD
## picks on their own cadence (Thu 9am / Sun 6am / Mon 9am ET), one game-day at
## a time, so a Sunday prop must not get staked three days early on a Thursday
## run. This script owns that: it touches ONLY outputs/td_2026_bet_card.csv and
## the published-picks ledger, and does not touch the spread/total game-markets
## model at all (no ~15-20 min seed-averaged RF refit), so it is safe and cheap
## to run on its own schedule.
##
## Append-only, same as scripts/57: a bet already in the ledger is frozen (its
## price/line/stake/id never change on a later run); re-running the same day is
## safe and idempotent.
##
##   --day=2026-10-16   which calendar day's games to stake (default: today,
##                       America/New_York)
##   --execute           write the feed and update the ledger (otherwise dry run)
##   --mode=PAPER         feed mode

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", flag, "="), "", hit[[1]])
}
execute <- "--execute" %in% args
mode <- arg_value("--mode", "PAPER")
ledger_path <- "data/processed/published_picks_ledger.csv"

target_day <- arg_value("--day")
if (is.null(target_day)) {
  target_day <- format(lubridate::with_tz(Sys.time(), "America/New_York"), "%Y-%m-%d")
}
cat("Publishing anytime-TD picks for game day:", target_day, "\n")

card_path <- "outputs/td_2026_bet_card.csv"
if (!file.exists(card_path)) {
  stop("No touchdown bet card at ", card_path, " - run scripts/15 first.", call. = FALSE)
}
card <- readr::read_csv(card_path, show_col_types = FALSE)
if (nrow(card)) card <- card |> dplyr::filter(as.character(as.Date(.data$game_date)) == target_day)
cat("Card rows for this game day:", nrow(card), "\n")

# --------------------------------------------------------------------------
# Anytime touchdown candidates: CORE and EXPANDED tiers only, this day only.
#
# strategy_tier already encodes every selection rule (core_edge/expanded_edge,
# the low-total/TE/heavy-favorite/WR situational gates, and the WR odds-cap /
# RB edge-floor added 2026-10-13) - see R/touchdown_deploy.R prepare_td_2026_
# bet_card(). This script applies no additional filtering beyond the day scope
# and the permitted-book check.
# --------------------------------------------------------------------------

td_candidates <- list()
core <- if (nrow(card)) {
  card |>
    dplyr::filter(
      tolower(dplyr::coalesce(.data$strategy_tier, "")) %in% c("core", "expanded"),
      is.finite(.data$best_american_odds),
      tolower(.data$best_book) %in% tolower(unname(feed_books()))
    )
} else card

cat("Touchdown bets qualifying this game day (core + expanded):", nrow(core), "\n")
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

fresh <- dplyr::bind_rows(td_candidates)
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
cat("Qualifying TD bets this run:", nrow(fresh), "\n")

# --------------------------------------------------------------------------
# Append-only ledger (shared with scripts/57 - same file, same schema)
# --------------------------------------------------------------------------

ledger <- if (file.exists(ledger_path)) {
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
      best_tier = 2L,
      stake_units = round(feed_stake_units("td", .data$edge, .data$best_tier), 2),
      confidence = feed_confidence(.data$best_tier),
      system = "touchdown",
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

  if (any(new_bets$status == "rejected")) {
    stop("Rejected-strategy bets reached the ledger; check the registry.",
         call. = FALSE)
  }
  feed_assert_books(new_bets$book, "ledger")

  print(as.data.frame(
    new_bets |> dplyr::select("slate_date", "event", "selection",
                              "odds_american", "min_price", "book", "status",
                              "edge", "stake_units")
  ), digits = 4)
} else {
  cat("Nothing new to publish for this game day.\n")
}

if (!execute) {
  cat("\nDry run. Add --execute to write the feed and update the ledger.\n")
  quit(save = "no", status = 0)
}

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

# Only THIS game day's props, from the ledger - so a slate keeps the exact
# price/line/stake it was published with even if this script reruns later.
published <- ledger |>
  dplyr::filter(.data$market == "prop", .data$slate_date == target_day)

for (column in c("market_label", "stat", "player", "team", "status", "system")) {
  if (!column %in% names(published)) published[[column]] <- NA_character_
}
if (!"min_price" %in% names(published)) published$min_price <- NA_integer_

if (nrow(published)) {
  bets <- published |>
    dplyr::mutate(
      game_id = NA_character_,
      market_prob = american_to_prob(.data$odds_american),
      ev_pct = .data$model_prob * american_payout(.data$odds_american) -
        (1 - .data$model_prob),
      strategy_count = 1L
    )
  target_week <- if (!all(is.na(bets$week))) stats::na.omit(bets$week)[[1]] else NA_integer_
  path <- write_picks_file(bets, target_day, target_week, mode = mode)
  cat("Wrote", nrow(bets), "bets to", path, "\n")
} else {
  cat("Nothing published for", target_day, "yet - no picks file written.\n")
}
