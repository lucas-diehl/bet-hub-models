read_odds_api_key <- function(path = "config/secrets.yml") {
  environment_key <- Sys.getenv("THE_ODDS_API_KEY", unset = "")
  if (nzchar(environment_key)) return(environment_key)

  if (!file.exists(path)) {
    stop(
      "Odds API credentials not found. Add config/secrets.yml or set ",
      "THE_ODDS_API_KEY.",
      call. = FALSE
    )
  }

  secrets <- yaml::read_yaml(path)
  key <- secrets$the_odds_api_key
  if (is.null(key) || !is.character(key) || length(key) != 1L || !nzchar(key)) {
    stop(
      "the_odds_api_key is blank in config/secrets.yml.",
      call. = FALSE
    )
  }
  key
}

odds_api_base_url <- function() {
  "https://api.the-odds-api.com/v4"
}

odds_api_request <- function(path, query = list()) {
  key <- read_odds_api_key()
  request <- httr2::request(paste0(odds_api_base_url(), path)) |>
    httr2::req_url_query(!!!c(query, list(apiKey = key))) |>
    httr2::req_user_agent("nfl-touchdown-model/0.1") |>
    httr2::req_retry(max_tries = 10, retry_on_failure = TRUE)

  response <- tryCatch(
    httr2::req_perform(request),
    error = function(e) {
      stop(redact_api_key(conditionMessage(e)), call. = FALSE)
    }
  )
  headers <- httr2::resp_headers(response)
  list(
    data = httr2::resp_body_json(response, simplifyVector = TRUE),
    quota = list(
      used = suppressWarnings(as.numeric(headers[["x-requests-used"]])),
      remaining = suppressWarnings(as.numeric(headers[["x-requests-remaining"]])),
      last = suppressWarnings(as.numeric(headers[["x-requests-last"]]))
    )
  )
}

check_odds_api_access <- function() {
  result <- odds_api_request("/sports")
  sports <- result$data
  nfl_available <- any(sports$key == "americanfootball_nfl")
  list(nfl_available = nfl_available, quota = result$quota)
}

odds_api_current_events <- function() {
  odds_api_request(
    "/sports/americanfootball_nfl/events",
    list(dateFormat = "iso")
  )
}

odds_api_current_game_lines <- function(region = "us") {
  odds_api_request(
    "/sports/americanfootball_nfl/odds",
    list(
      regions = region,
      markets = "spreads,totals",
      oddsFormat = "american",
      dateFormat = "iso"
    )
  )
}

odds_api_current_event_props <- function(
  event_id,
  market = "player_anytime_td",
  region = "us"
) {
  if (!grepl("^[a-f0-9]{32}$", event_id)) {
    stop("Invalid Odds API event ID.", call. = FALSE)
  }
  odds_api_request(
    paste0(
      "/sports/americanfootball_nfl/events/",
      event_id,
      "/odds"
    ),
    list(
      regions = region,
      markets = market,
      oddsFormat = "american",
      dateFormat = "iso"
    )
  )
}

odds_api_historical_events <- function(snapshot_time) {
  odds_api_request(
    "/historical/sports/americanfootball_nfl/events",
    list(
      date = snapshot_time,
      dateFormat = "iso"
    )
  )
}

odds_api_historical_event_props <- function(
  event_id,
  snapshot_time,
  market = "player_anytime_td",
  region = "us"
) {
  if (!grepl("^[a-f0-9]{32}$", event_id)) {
    stop("Invalid Odds API event ID.", call. = FALSE)
  }
  odds_api_request(
    paste0(
      "/historical/sports/americanfootball_nfl/events/",
      event_id,
      "/odds"
    ),
    list(
      date = snapshot_time,
      regions = region,
      markets = market,
      oddsFormat = "american",
      dateFormat = "iso"
    )
  )
}

estimate_historical_prop_cost <- function(events, regions = 1L, markets = 1L) {
  as.integer(events) * 10L * as.integer(regions) * as.integer(markets)
}

# Over/under player markets, keyed by the label used in this project's outputs.
# The Odds API charges 10 credits per event per market per region regardless of
# how many player lines come back, so markets that quote a dozen players per
# game cost the same as one quoting two quarterbacks.
player_prop_market_keys <- function() {
  c(
    passing_yards = "player_pass_yds",
    receptions = "player_receptions",
    receiving_yards = "player_reception_yds",
    rushing_yards = "player_rush_yds",
    passing_tds = "player_pass_tds",
    rush_reception_yards = "player_rush_reception_yds"
  )
}

# Unlike anytime touchdown, these markets return paired Over/Under outcomes that
# each carry a handicap in `point`. Both sides and the line are kept so the
# backtest can grade either direction and measure closing-line value.
flatten_player_prop_payload <- function(payload, market_key, game = NULL) {
  event <- payload$data
  if (is.null(event)) return(tibble::tibble())

  rows <- list()
  row_number <- 0L
  for (bookmaker in event$bookmakers %||% list()) {
    # Pull these out before the tibble() call below: tibble exposes each column
    # it has already built to later arguments, so a column named `bookmaker`
    # would shadow this loop variable mid-construction.
    bookmaker_key_value <- as.character(bookmaker$key)
    bookmaker_title <- as.character(bookmaker$title)
    bookmaker_updated <- as.character(bookmaker$last_update)

    for (market in bookmaker$markets %||% list()) {
      if (!identical(market$key, market_key)) next
      outcomes <- market$outcomes %||% list()
      if (!length(outcomes)) next
      market_key_value <- as.character(market$key)

      points <- vapply(
        outcomes,
        function(x) suppressWarnings(as.numeric(x$point %||% NA_real_)),
        numeric(1)
      )
      prices <- vapply(
        outcomes,
        function(x) suppressWarnings(as.numeric(x$price %||% NA_real_)),
        numeric(1)
      )
      players <- vapply(
        outcomes,
        function(x) as.character(x$description %||% NA_character_),
        character(1)
      )
      sides <- vapply(
        outcomes,
        function(x) as.character(x$name %||% NA_character_),
        character(1)
      )

      row_number <- row_number + 1L
      rows[[row_number]] <- tibble::tibble(
        event_id = as.character(event$id),
        event_snapshot = as.character(payload$timestamp),
        commence_time = as.character(event$commence_time),
        home_team = as.character(event$home_team),
        away_team = as.character(event$away_team),
        bookmaker_key = bookmaker_key_value,
        bookmaker = bookmaker_title,
        bookmaker_last_update = bookmaker_updated,
        market_key = market_key_value,
        player = players,
        side = sides,
        line = points,
        american_odds = prices,
        implied_probability = dplyr::if_else(
          prices < 0,
          abs(prices) / (abs(prices) + 100),
          100 / (prices + 100)
        )
      )
    }
  }

  result <- dplyr::bind_rows(rows)
  if (!is.null(game) && nrow(result)) {
    result <- dplyr::bind_cols(
      tibble::tibble(
        game_id = as.character(game$game_id),
        season = as.integer(game$season),
        week = as.integer(game$week),
        requested_snapshot = as.character(game$snapshot_utc),
        scheduled_kickoff = as.character(game$kickoff_utc)
      )[rep(1L, nrow(result)), ],
      result
    ) |>
      dplyr::mutate(
        snapshot_lead_minutes = as.numeric(difftime(
          lubridate::ymd_hms(.data$scheduled_kickoff, tz = "UTC"),
          lubridate::ymd_hms(.data$event_snapshot, tz = "UTC"),
          units = "mins"
        ))
      )
  }
  result
}

redact_api_key <- function(x) {
  key <- tryCatch(read_odds_api_key(), error = function(e) "")
  if (!nzchar(key)) return(x)
  gsub(key, "[REDACTED]", x, fixed = TRUE)
}

odds_api_team_names <- function() {
  c(
    ARI = "Arizona Cardinals", ATL = "Atlanta Falcons",
    BAL = "Baltimore Ravens", BUF = "Buffalo Bills",
    CAR = "Carolina Panthers", CHI = "Chicago Bears",
    CIN = "Cincinnati Bengals", CLE = "Cleveland Browns",
    DAL = "Dallas Cowboys", DEN = "Denver Broncos",
    DET = "Detroit Lions", GB = "Green Bay Packers",
    HOU = "Houston Texans", IND = "Indianapolis Colts",
    JAX = "Jacksonville Jaguars", KC = "Kansas City Chiefs",
    LA = "Los Angeles Rams", LAC = "Los Angeles Chargers",
    LAR = "Los Angeles Rams", LV = "Las Vegas Raiders",
    MIA = "Miami Dolphins", MIN = "Minnesota Vikings",
    NE = "New England Patriots", NO = "New Orleans Saints",
    NYG = "New York Giants", NYJ = "New York Jets",
    PHI = "Philadelphia Eagles", PIT = "Pittsburgh Steelers",
    SEA = "Seattle Seahawks", SF = "San Francisco 49ers",
    TB = "Tampa Bay Buccaneers", TEN = "Tennessee Titans",
    WAS = "Washington Commanders"
  )
}

nfl_kickoff_utc <- function(gameday, gametime) {
  local <- lubridate::ymd_hm(
    paste(gameday, gametime),
    tz = "America/New_York",
    quiet = TRUE
  )
  format(
    lubridate::with_tz(local, "UTC"),
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
}

flatten_anytime_td_payload <- function(payload, game = NULL) {
  event <- payload$data
  if (is.null(event)) return(tibble::tibble())

  rows <- list()
  row_number <- 0L
  bookmakers <- event$bookmakers %||% list()
  for (bookmaker in bookmakers) {
    for (market in bookmaker$markets %||% list()) {
      if (!identical(market$key, "player_anytime_td")) next
      outcomes <- market$outcomes %||% list()
      if (!length(outcomes)) next
      row_number <- row_number + 1L
      prices <- vapply(
        outcomes,
        function(x) suppressWarnings(as.numeric(x$price)),
        numeric(1)
      )
      bookmaker_key_value <- as.character(bookmaker$key)
      bookmaker_title <- as.character(bookmaker$title)
      bookmaker_updated <- as.character(bookmaker$last_update)
      rows[[row_number]] <- tibble::tibble(
        event_id = as.character(event$id),
        event_snapshot = as.character(payload$timestamp),
        commence_time = as.character(event$commence_time),
        home_team = as.character(event$home_team),
        away_team = as.character(event$away_team),
        bookmaker_key = bookmaker_key_value,
        bookmaker = bookmaker_title,
        bookmaker_last_update = bookmaker_updated,
        market_last_update = as.character(market$last_update),
        player = vapply(
          outcomes,
          function(x) as.character(x$description),
          character(1)
        ),
        outcome = vapply(
          outcomes,
          function(x) as.character(x$name),
          character(1)
        ),
        american_odds = prices,
        implied_probability = dplyr::if_else(
          prices < 0,
          abs(prices) / (abs(prices) + 100),
          100 / (prices + 100)
        )
      )
    }
  }
  result <- dplyr::bind_rows(rows)
  if (!is.null(game) && nrow(result)) {
    result <- dplyr::bind_cols(
      tibble::tibble(
        game_id = as.character(game$game_id),
        season = as.integer(game$season),
        week = as.integer(game$week),
        requested_snapshot = as.character(game$snapshot_utc),
        scheduled_kickoff = as.character(game$kickoff_utc)
      )[rep(1L, nrow(result)), ],
      result
    ) |>
      dplyr::mutate(
        snapshot_lead_minutes = as.numeric(difftime(
          lubridate::ymd_hms(.data$scheduled_kickoff, tz = "UTC"),
          lubridate::ymd_hms(.data$event_snapshot, tz = "UTC"),
          units = "mins"
        ))
      )
  }
  result
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
