## ---------------------------------------------------------------------------
## 88_emit_td_prop_board.R
##
## Publishes the FULL anytime-touchdown field to the Bet Hub as a
## props_<date>.json feed file, rendered on the /touchdowns tab.
##
## Why the whole field and not just the bets: the picks feed answers "what did
## we stake", this answers "what did the model think about everyone the book
## priced". A player who missed the threshold is still worth seeing next to his
## number, and when he was skipped for a STRUCTURAL reason rather than a thin
## edge - QB, or the +400..+499 band - that reason is written onto the row
## instead of leaving an unexplained gap in the list.
##
## Contract: packages/contract/src/schema.ts, PropsFileSchema / PropPlayerSchema.
## Zod strips unknown keys silently, so every field below is copied from the
## schema rather than invented - the same mistake the board feed made once.
##
##   --week=4     which week to publish (default: the card's own week)
##   --execute    write the feed file (otherwise dry run)
## ---------------------------------------------------------------------------

source("R/utilities.R")
source("R/dashboard_feed.R")
assert_packages()
ensure_directories()
suppressPackageStartupMessages(library(dplyr))

CONTRACT_VERSION <- "1.0"
args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", flag, "="), "", hit[[1]])
}
execute <- "--execute" %in% args

card_path <- "outputs/td_2026_bet_card.csv"
if (!file.exists(card_path)) {
  stop("No bet card at ", card_path, " - run scripts/15 first.", call. = FALSE)
}
card <- readr::read_csv(card_path, show_col_types = FALSE)
if (!nrow(card)) stop("Bet card is empty.", call. = FALSE)

target_week <- as.integer(arg_value("--week", card$week[[1]]))
card <- card |> filter(.data$week == target_week)
if (!nrow(card)) stop("No rows for week ", target_week, call. = FALSE)

## Defensive de-dup: one row per (player, team), best price only.
##
## scripts/15 already collapses to one row per player with the best-of-permitted
## -books price pre-selected (verified: 0 of 401 week-4 card rows duplicated by
## player+team), so this should be a no-op today. Kept as an explicit guard
## rather than trusting that invariant silently, since a player appearing twice
## with two different books on the live site is a visible, confusing failure
## mode and cheap to rule out here regardless of whether the upstream card ever
## stays clean. "Best" = lowest implied probability = most favourable payout to
## the bettor, the same convention best_american_odds/best_book already use.
n_before_dedup <- nrow(card)
card <- card |>
  group_by(.data$player, .data$team) |>
  arrange(.data$best_implied_probability, .by_group = TRUE) |>
  slice_head(n = 1) |>
  ungroup()
if (nrow(card) != n_before_dedup) {
  message(sprintf(
    "De-duplicated %d duplicate player+team row(s) (kept best price each): %d -> %d.",
    n_before_dedup - nrow(card), n_before_dedup, nrow(card)
  ))
}

cfg <- yaml::read_yaml("config/td_2026.yml")
excl_pos <- toupper(as.character(cfg$strategy$exclude_positions))
lo <- suppressWarnings(as.numeric(cfg$strategy$exclude_odds_low))
hi <- suppressWarnings(as.numeric(cfg$strategy$exclude_odds_high))
floor_odds <- suppressWarnings(as.numeric(cfg$market$minimum_american_odds))
cap_odds <- suppressWarnings(as.numeric(cfg$market$maximum_american_odds))

teams <- feed_team_lookup()
nick <- function(x) {
  out <- unname(teams$nick[x])
  ifelse(is.na(out), x, out)
}

## Which bets actually shipped, so the board and the slip agree. Read back from
## the picks feed rather than recomputed, same discipline as scripts/78.
## Only THIS week's picks files. Scanning the whole feed marked a player as
## staked because he was bet in an earlier week - the first run reported 4
## staked against 3 actually published.
posted <- list()
outdir <- file.path(feed_root(), "nfl-modeling", "nfl")
week_dates <- unique(as.character(card$game_date))
week_files <- file.path(outdir, sprintf("picks_%s.json", week_dates))
for (f in week_files[file.exists(week_files)]) {
  pj <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(pj)) next
  for (b in pj$bets) {
    if (!identical(b$market, "prop")) next
    sel <- if (is.null(b$selection)) "" else as.character(b$selection)
    key <- tolower(gsub("[^a-z0-9]", "", tolower(sel)))
    posted[[key]] <- b
  }
}
posted_for <- function(player) {
  key <- tolower(gsub("[^a-z0-9]", "", tolower(paste0(player, " Anytime TD"))))
  posted[[key]]
}

d <- card |>
  mutate(
    .excluded_reason = case_when(
      toupper(.data$position) %in% excl_pos ~ paste0(toupper(.data$position), " excluded"),
      is.finite(lo) & is.finite(hi) &
        .data$best_american_odds >= lo & .data$best_american_odds <= hi ~
        sprintf("+%d-+%d band excluded", as.integer(lo), as.integer(hi)),
      is.finite(floor_odds) & .data$best_american_odds < floor_odds ~
        sprintf("shorter than %d", as.integer(floor_odds)),
      is.finite(cap_odds) & .data$best_american_odds > cap_odds ~
        sprintf("longer than +%d", as.integer(cap_odds)),
      TRUE ~ NA_character_
    ),
    .ev = .data$model_probability * (
      ifelse(.data$best_american_odds > 0,
             1 + .data$best_american_odds / 100,
             1 + 100 / abs(.data$best_american_odds))
    ) - 1
  ) |>
  arrange(desc(.data$model_probability - .data$best_implied_probability))

num <- function(x) { x <- suppressWarnings(as.numeric(x)); if (length(x) && is.finite(x)) round(x, 5) else NULL }
chr <- function(x) if (length(x) && !is.na(x)) as.character(x) else NULL
compact <- function(l) l[!vapply(l, is.null, logical(1))]

props <- lapply(seq_len(nrow(d)), function(i) {
  r <- d[i, ]
  pb <- posted_for(r$player)
  compact(list(
    player = chr(r$player),
    player_id = chr(r$player_id),
    position = chr(r$position),
    team = chr(nick(r$team)),
    opponent = chr(nick(r$opponent_team)),
    game_id = chr(r$game_id),
    event = paste(nick(r$team), "vs", nick(r$opponent_team)),
    event_start = chr(r$kickoff_utc),
    best_odds = num(r$best_american_odds),
    best_book = chr(r$best_book),
    market_prob = num(r$best_implied_probability),
    consensus_prob = num(r$consensus_probability),
    books = if (is.finite(r$books_available)) as.integer(r$books_available) else NULL,
    model_prob = num(r$model_probability),
    edge = num(r$model_probability - r$best_implied_probability),
    relative_edge = num(r$relative_edge),
    ev = num(r$.ev),
    tier = chr(r$strategy_tier),
    is_bet = !is.null(pb),
    stake = if (!is.null(pb)) num(pb$stake_units) else NULL,
    ## The price and time the bet was actually LOCKED IN, carried straight from
    ## the picks feed. This board is rebuilt twice a day against the live
    ## market, so best_odds above moves while these do not - the gap between
    ## them is exactly how stale a staked price has gone.
    posted_at = if (!is.null(pb)) chr(pb$posted_at) else NULL,
    posted_odds = if (!is.null(pb)) num(pb$odds_american) else NULL,
    excluded_reason = chr(r$.excluded_reason)
  ))
})

slate_date <- as.character(names(sort(table(as.character(d$game_date)), decreasing = TRUE))[1])
out <- list(
  contract_version = CONTRACT_VERSION,
  source = "nfl-modeling",
  sport = "nfl",
  market = "anytime_td",
  market_label = "Anytime Touchdown",
  slate_date = slate_date,
  generated_at = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  model_version = "td-v2-fantasy",
  mode = "PAPER",
  event_context = sprintf("Week %d", target_week),
  notes = "Every player the permitted books priced. Model probability blends the touchdown model, the market line and the fantasy projection.",
  props = props
)

cat("=== Touchdown prop board ===\n")
cat("week:", target_week, "| slate:", slate_date, "| players:", length(props), "\n")
cat("positive edge:", sum(d$model_probability > d$best_implied_probability, na.rm = TRUE), "\n")
cat("staked:", sum(vapply(props, function(p) isTRUE(p$is_bet), logical(1))), "\n")
cat("excluded for a structural reason:", sum(!is.na(d$.excluded_reason)), "\n")
if (any(!is.na(d$.excluded_reason))) print(table(d$.excluded_reason))

if (!execute) {
  cat("\nDry run. Add --execute to write the feed file.\n")
  quit(save = "no", status = 0)
}
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
path <- file.path(outdir, sprintf("props_%s.json", slate_date))
jsonlite::write_json(out, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
cat("\nWrote", length(props), "players to", path, "\n")
