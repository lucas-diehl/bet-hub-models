# ==============================================================================
# DFS ENGINE — Vegas odds feed (FREE, no key)
# Pulls game totals + spreads from ESPN's public scoreboard API (odds sourced from
# DraftKings), stores them in the `games` table, and exposes them to the sport
# plugins. Market lines are the single most valuable external signal for in-house
# projections + game-environment correlation — and this feed generalizes across
# sports (WNBA now; NBA/NFL/NCAAF when those plugins land).
#
# Vegas feeds the JOINT distribution (game total -> correlation/ceiling, spread ->
# blowout risk), NOT the marginal mean, so it never overrides a validated projection.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# our sport key -> ESPN scoreboard path
.ESPN_SPORT <- c(wnba = "basketball/wnba", nba = "basketball/nba",
                 nfl = "football/nfl", ncaaf = "football/college-football")

.espn_scoreboard <- function(sport, date) {
  path <- .ESPN_SPORT[[sport]]; if (is.null(path)) return(NULL)
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/%s/scoreboard?dates=%s",
                 path, format(as.Date(date), "%Y%m%d"))
  resp <- tryCatch(httr2::request(url) |> httr2::req_user_agent("DFS-ENGINE/1.0") |>
                     httr2::req_timeout(30) |> httr2::req_perform(), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) != 200) return(NULL)
  tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL)
}

# Fetch + persist today's (or a date's) game lines for a sport. Returns a data.table
# (sport, game_id "AWY@HOM", home, away, vegas_total, spread [home line], home_total,
# away_total). game_id mirrors DK's "AWY@HOM" so sports can match on the team pair.
# ESPN REMOVED the `odds` block from the scoreboard payload. The scoreboard still
# returns the games -- 65 NCAAF events on 2026-09-26, every one with odds = length 0
# -- so `vegas_fetch` returned NULL for NFL *and* NCAAF and nothing surfaced it. That
# is the single root cause behind BOTH the long-standing "NFL Vegas caching gap" and
# NCAAF's hardcoded `exp_margin := 18.8`; they were never two bugs.
#
# The lines moved to the core API, one call per competition, still DraftKings-sourced.
# VERIFIED on 6 real games that `spread` keeps the SAME convention as the old
# scoreboard field -- the HOME line, negative = home favored (TEX@TENN: "TEX -5.5"
# -> spread +5.5 because the HOME side is the underdog). So the arithmetic below is
# unchanged; only where the numbers come from is different.
.ESPN_CORE_LEAGUE <- c(wnba = "basketball/leagues/wnba", nba = "basketball/leagues/nba",
                       nfl = "football/leagues/nfl", ncaaf = "football/leagues/college-football")

.espn_core_odds <- function(sport, event_id, comp_id) {
  lg <- .ESPN_CORE_LEAGUE[[sport]]; if (is.null(lg)) return(NULL)
  url <- sprintf("https://sports.core.api.espn.com/v2/sports/%s/events/%s/competitions/%s/odds",
                 lg, event_id, comp_id)
  resp <- tryCatch(httr2::request(url) |> httr2::req_user_agent("DFS-ENGINE/1.0") |>
                     httr2::req_timeout(20) |> httr2::req_perform(), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) != 200) return(NULL)
  j <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL)
  items <- j$items %||% list(); if (!length(items)) return(NULL)
  # prefer DraftKings (matches the book we actually play), else the first with a total
  pick <- NULL
  for (it in items) if (identical(it$provider$name %||% "", "DraftKings") && !is.null(it$overUnder)) { pick <- it; break }
  if (is.null(pick)) for (it in items) if (!is.null(it$overUnder)) { pick <- it; break }
  pick
}

vegas_fetch <- function(sport, date = Sys.Date()) {
  j <- .espn_scoreboard(sport, date)
  if (is.null(j) || is.null(j$events) || !length(j$events)) return(NULL)
  n_core <- 0L
  rows <- rbindlist(lapply(j$events, function(ev) {
    comp <- ev$competitions[[1]]; if (is.null(comp)) return(NULL)
    home <- away <- NA_character_
    for (cc in (comp$competitors %||% list())) {
      ab <- cc$team$abbreviation %||% NA_character_
      if (identical(cc$homeAway, "home")) home <- ab else if (identical(cc$homeAway, "away")) away <- ab
    }
    od <- NULL
    for (o in (comp$odds %||% list())) if (!is.null(o$overUnder)) { od <- o; break }
    # scoreboard carried nothing -> ask the core API for this competition
    if (is.null(od) && !is.null(ev$id) && !is.null(comp$id)) {
      od <- .espn_core_odds(sport, ev$id, comp$id)
      if (!is.null(od)) n_core <<- n_core + 1L
    }
    if (is.null(od)) return(NULL)
    total  <- suppressWarnings(as.numeric(od$overUnder))
    spread <- suppressWarnings(as.numeric(od$spread))          # home spread (neg = home fav)
    ok <- is.finite(total)
    data.table(sport = sport, game_date = as.character(as.Date(date)),
               home = home, away = away, vegas_total = total, spread = spread,
               home_total = if (ok && is.finite(spread)) (total - spread) / 2 else NA_real_,
               away_total = if (ok && is.finite(spread)) (total + spread) / 2 else NA_real_,
               details = od$details %||% NA_character_)
  }), fill = TRUE)
  if (is.null(rows) || !nrow(rows)) return(NULL)
  rows <- rows[!is.na(home) & !is.na(away) & is.finite(vegas_total)]
  if (!nrow(rows)) return(NULL)
  rows[, game_id := paste0(away, "@", home)]
  df <- data.frame(sport = rows$sport, game_id = rows$game_id, game_date = rows$game_date,
                   home = rows$home, away = rows$away, vegas_total = rows$vegas_total,
                   spread = rows$spread, home_total = rows$home_total, away_total = rows$away_total,
                   pace = NA_real_, updated_ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  tryCatch(db_upsert("games", df, keys = c("sport", "game_id")), error = function(e) NULL)
  msg(sprintf("  vegas: %d %s game line(s) for %s%s", nrow(rows), toupper(sport),
              as.character(as.Date(date)),
              if (n_core > 0L) sprintf(" (%d via core API -- scoreboard carried no odds)", n_core) else ""))
  rows[]
}

# Read a sport's game lines for a date (from the games table); fetch if absent.
vegas_games <- function(sport, date = Sys.Date(), fetch = TRUE) {
  read <- function() tryCatch(as.data.table(db_query(
    "SELECT sport, game_id, home, away, vegas_total, spread, home_total, away_total
       FROM games WHERE sport = ? AND game_date = ? AND vegas_total IS NOT NULL",
    list(sport, as.character(as.Date(date))))), error = function(e) NULL)
  d <- read()
  if ((is.null(d) || !nrow(d)) && fetch) { tryCatch(vegas_fetch(sport, date), error = function(e) NULL); d <- read() }
  d
}
