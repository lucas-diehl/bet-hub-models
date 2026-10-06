source("R/utilities.R")
source("R/dashboard_feed.R")
assert_packages()
ensure_directories()

# Full-slate model board for the Bet Hub "Extras" tab, ported from the
# cfb-modeling project's 08_write_slate.R. Writes ONE row per game for the
# target week with the model's numbers for EVERY game, not just the ones that
# clear a betting threshold: kickoff, market line, projected margin/total,
# projected score, the model's spread & total lean, edge, and a confidence
# bucket. Display-only - never grades or stakes. scripts/57 remains the sole
# source of truth for actual bets.
#
# Consistency rule (the exact bug CFB's own header warns about, so replicated
# here deliberately): for any game that HAS a posted bet, this reads the
# already-written picks_<date>.json back and uses ITS fields (price, book,
# stake, status, edge) rather than recomputing them - so a threshold or price
# change in scripts/57 can never desync the board from the bet slip. The
# model's raw lean is shown for every OTHER game from the persisted full-slate
# table (outputs/nfl_full_slate_latest.csv), which scripts/57 writes as a
# side effect specifically so this script never needs to re-fit the
# seed-averaged spread model (~15-20 min) just to draw a board.
#
#   --week=3      which regular-season week (default: matches scripts/57's
#                 own "next unplayed" logic against the persisted slate)
#   --execute     write the board files (otherwise dry run, prints only)

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", flag, "="), "", hit[[1]])
}
execute <- "--execute" %in% args
feed_dir <- feed_root()
outdir <- file.path(feed_dir, "nfl-modeling", "nfl")

slate_path <- "outputs/nfl_full_slate_latest.csv"
if (!file.exists(slate_path)) {
  stop("No persisted slate at ", slate_path,
       ". Run scripts/57_publish_weekly_picks.R first (it writes this as a side effect).",
       call. = FALSE)
}
# kickoff_utc is an ISO8601-looking string ("2026-09-13T20:25:00Z"); read_csv's
# type guesser parses that as a datetime column rather than character unless
# told otherwise, which later breaks the character-vector ordering below with
# a "result is type 'double'" error (POSIXct's storage is numeric) - the same
# class of guessing bug this project has hit before with a Date-shaped column.
slate <- readr::read_csv(
  slate_path, show_col_types = FALSE,
  col_types = readr::cols(kickoff_utc = readr::col_character(), .default = readr::col_guess())
)

target_week <- as.integer(arg_value("--week", slate$week[[1]]))
slate <- dplyr::filter(slate, .data$week == target_week)
if (!nrow(slate)) {
  stop("No rows for week ", target_week, " in ", slate_path,
       ". Re-run scripts/57 for that week first.", call. = FALSE)
}
cat("Writing slate board for week", target_week, "-", nrow(slate), "games\n")

teams <- feed_team_lookup()

mkconf_spread <- function(m) dplyr::case_when(m >= 6 ~ "high", m >= 3 ~ "medium", TRUE ~ "low")
mkconf_total  <- function(m) dplyr::case_when(m >= 5 ~ "high", m >= 2.5 ~ "medium", TRUE ~ "low")

slate <- slate |>
  dplyr::mutate(
    event = paste(unname(teams$nick[.data$away_team]), "@", unname(teams$nick[.data$home_team])),
    slate_date = format(as.Date(.data$game_date)),
    proj_home_score = (.data$projected_total + .data$projected_margin) / 2,
    proj_away_score = (.data$projected_total - .data$projected_margin) / 2,
    spread_pick = dplyr::if_else(.data$margin_edge > 0, .data$home_team, .data$away_team),
    spread_side = dplyr::if_else(.data$margin_edge > 0, "home", "away"),
    spread_line = dplyr::if_else(.data$margin_edge > 0, .data$home_line, -.data$home_line),
    spread_mag  = abs(.data$margin_edge),
    total_pick  = dplyr::if_else(.data$total_edge > 0, "Over", "Under"),
    total_mag   = abs(.data$total_edge)
  )

# ---- read the already-posted picks back, per game day, so the board and the
# bet slip cannot disagree (see header) ------------------------------------
read_posted <- function(slate_date) {
  pf <- file.path(outdir, sprintf("picks_%s.json", slate_date))
  if (!file.exists(pf)) return(list())
  pj <- tryCatch(jsonlite::fromJSON(pf, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(pj)) return(list())
  out <- list()
  for (b in pj$bets) {
    key <- paste(b$event, b$market, sep = "|")
    out[[key]] <- b
  }
  out
}
posted_cache <- list()
pget <- function(row, market) {
  d <- row$slate_date
  if (is.null(posted_cache[[d]])) posted_cache[[d]] <<- read_posted(d)
  posted_cache[[d]][[paste(row$event, market, sep = "|")]]
}

compact <- function(l) l[!vapply(l, is.null, logical(1))]

mk_game <- function(row) {
  pb <- pget(row, "spread"); tb <- pget(row, "total")
  compact(list(
    game_id     = as.character(row$game_id),
    event       = row$event,
    event_start = row$kickoff_utc,
    home_team   = row$home_team, away_team = row$away_team,
    spread      = if (is.na(row$home_line)) NULL else round(row$home_line, 1),
    total       = if (is.na(row$total_line)) NULL else round(row$total_line, 1),
    proj_total  = round(row$projected_total, 1),
    proj_margin = round(row$projected_margin, 1),
    proj_home_score = round(row$proj_home_score, 1),
    proj_away_score = round(row$proj_away_score, 1),
    # Field names below match packages/contract/src/schema.ts's BoardGameSchema
    # exactly (ats_pick/ats_line/ats_edge/ats_conf/ats_play/ats_book/ats_stake,
    # total_pick/total_edge/total_conf/total_play/total_stake) - NOT invented
    # names. Zod's default z.object() silently STRIPS unrecognized keys rather
    # than rejecting the file, so the first version of this script (spread_*,
    # *_pts, *_price, *_status) passed ingest validation while quietly losing
    # edge/book/stake for every game - "0 problems" in the ingest log looked
    # like success and wasn't. The schema has no slot for price or status at
    # all (odds_american/status aren't part of the board contract, only
    # book/stake/line are) - caught before shipping a frontend that would have
    # referenced fields that were never actually stored.
    #
    # A posted bet always wins over the raw model lean for SELECTION, carrying
    # the FROZEN terms it was actually bet at (never the current/live line
    # shown in `spread`/`total` above). Edge/confidence stay the model's own
    # points-edge for every game, bet or not - the picks feed's own `edge`
    # field is probability-space (model_prob - market_prob), a different unit
    # that would silently vary by bet status if reused here.
    ats_pick = if (!is.null(pb)) pb$selection else unname(teams$nick[row$spread_pick]),
    ats_line = if (!is.null(pb)) pb$line else round(row$spread_line, 1),
    ats_edge = round(row$spread_mag, 1),
    ats_conf = mkconf_spread(row$spread_mag),
    ats_play = if (!is.null(pb)) TRUE else NULL,
    ats_book = if (!is.null(pb)) pb$book else NULL,
    ats_stake = if (!is.null(pb)) pb$stake_units else NULL,
    total_pick  = if (!is.null(tb)) tb$selection else paste(row$total_pick, round(row$total_line, 1)),
    total_edge = round(row$total_mag, 1),
    total_conf  = mkconf_total(row$total_mag),
    total_play  = if (!is.null(tb)) TRUE else NULL,
    total_stake = if (!is.null(tb)) tb$stake_units else NULL
  ))
}

games <- lapply(seq_len(nrow(slate)), function(i) {
  g <- mk_game(slate[i, ]); g$.date <- slate$slate_date[[i]]; g
})

dates <- sort(unique(vapply(games, function(g) g$.date, character(1))))
now_iso <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

cat("\n=== Preview ===\n")
for (d in dates) {
  day <- Filter(function(g) identical(g$.date, d), games)
  n_spread_play <- sum(vapply(day, function(g) isTRUE(g$spread_play), logical(1)))
  n_total_play  <- sum(vapply(day, function(g) isTRUE(g$total_play), logical(1)))
  cat(sprintf("  %s: %d games, %d spread play(s), %d total play(s)\n",
              d, length(day), n_spread_play, n_total_play))
}

if (!execute) {
  cat("\nDry run. Add --execute to write board files.\n")
  quit(save = "no", status = 0)
}

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
for (d in dates) {
  day <- Filter(function(g) identical(g$.date, d), games)
  ord <- order(vapply(day, function(g) if (is.null(g$event_start)) "9" else g$event_start, character(1)))
  day <- day[ord]
  out <- list(
    contract_version = "1.0", source = "nfl-modeling", sport = "nfl",
    slate_date = d, generated_at = now_iso, model_version = "nfl-v1",
    mode = "PAPER", event_context = sprintf("Week %d", target_week),
    notes = "Model total/spread lean shown for every game; a posted bet (spread_play/total_play: true) carries the frozen terms it was actually bet at.",
    games = lapply(day, function(g) g[!startsWith(names(g), ".")])
  )
  path <- file.path(outdir, sprintf("board_%s.json", d))
  jsonlite::write_json(out, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  cat("Wrote", length(day), "games to", path, "\n")
}
