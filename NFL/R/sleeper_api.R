# %||% is also defined in R/odds_api.R with identical semantics; guarded so
# whichever file is sourced first wins rather than one silently overwriting
# the other's.
if (!exists("%||%")) `%||%` <- function(x, y) if (is.null(x)) y else x

# Sleeper league connectivity.
#
# Every endpoint here is public and read-only - no API key, no OAuth. Sleeper
# has no rate-limit documentation beyond "be reasonable," so nothing here
# polls in a loop; each call is a single request. This mirrors the pattern
# already used for Sleeper's global player feed in scripts/52 (search-rank /
# depth-chart-order enrichment for the draft tool) but adds the league-level
# endpoints that feed roster ownership, which that script never needed.

sleeper_base_url <- function() "https://api.sleeper.app/v1"

sleeper_request <- function(path) {
  httr2::request(paste0(sleeper_base_url(), path)) |>
    httr2::req_timeout(30) |>
    httr2::req_user_agent("nfl-modeling-waiver-tool/1.0") |>
    httr2::req_retry(max_tries = 3, max_seconds = 20) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

# Username -> user_id. Usernames are case-sensitive on Sleeper's side in
# practice; pass it through exactly as given rather than normalizing it.
sleeper_user_id <- function(username) {
  result <- tryCatch(sleeper_request(paste0("/user/", username)),
                     error = function(e) NULL)
  if (is.null(result) || is.null(result$user_id)) {
    stop("No Sleeper user found for username '", username, "'. ",
         "Check the spelling - this is the Sleeper login/display name, not ",
         "an email address.", call. = FALSE)
  }
  list(user_id = result$user_id, display_name = result$display_name %||% username)
}

# Every league a user belongs to for a season. NFL only; Sleeper also hosts
# other sports under the same account.
sleeper_user_leagues <- function(user_id, season) {
  result <- sleeper_request(
    paste0("/user/", user_id, "/leagues/nfl/", season)
  )
  purrr::map_dfr(result, function(lg) {
    tibble::tibble(
      league_id = as.character(lg$league_id),
      league_name = as.character(lg$name %||% NA_character_),
      season = as.character(lg$season %||% NA_character_),
      status = as.character(lg$status %||% NA_character_),
      total_rosters = suppressWarnings(as.integer(lg$total_rosters %||% NA))
    )
  })
}

# Scoring settings and roster shape for one league, as a flat named list -
# scoring_settings keys are Sleeper's stat abbreviations (pass_yd, rec, etc.),
# values are points per unit (or per occurrence for td/int/etc.).
sleeper_league_settings <- function(league_id) {
  lg <- sleeper_request(paste0("/league/", league_id))
  list(
    league_id = league_id,
    league_name = lg$name %||% NA_character_,
    scoring_settings = lg$scoring_settings %||% list(),
    roster_positions = unlist(lg$roster_positions %||% list()),
    total_rosters = lg$total_rosters %||% NA_integer_
  )
}

# One row per (roster, player). `status` distinguishes the active roster from
# taxi squad and IR - all three count as "owned, not available" for waiver
# purposes, but are worth keeping separate for anyone who wants to filter.
sleeper_league_rosters <- function(league_id) {
  result <- sleeper_request(paste0("/league/", league_id, "/rosters"))
  purrr::map_dfr(result, function(r) {
    roster_id <- r$roster_id
    owner_id <- r$owner_id %||% NA_character_
    active <- as.character(unlist(r$players %||% list()))
    taxi <- as.character(unlist(r$taxi %||% list()))
    reserve <- as.character(unlist(r$reserve %||% list()))
    dplyr::bind_rows(
      if (length(active)) tibble::tibble(
        roster_id = roster_id, owner_id = owner_id,
        sleeper_player_id = setdiff(active, c(taxi, reserve)),
        roster_status = "active"
      ) else tibble::tibble(),
      if (length(taxi)) tibble::tibble(
        roster_id = roster_id, owner_id = owner_id,
        sleeper_player_id = taxi, roster_status = "taxi"
      ) else tibble::tibble(),
      if (length(reserve)) tibble::tibble(
        roster_id = roster_id, owner_id = owner_id,
        sleeper_player_id = reserve, roster_status = "IR"
      ) else tibble::tibble()
    )
  })
}

# Maps roster_id -> the human/team name, so the output can say who owns
# whom rather than just a numeric roster id (not needed for the waiver list
# itself, but useful context and cheap to fetch alongside).
sleeper_league_users <- function(league_id) {
  users <- sleeper_request(paste0("/league/", league_id, "/users"))
  rosters <- sleeper_request(paste0("/league/", league_id, "/rosters"))
  user_names <- purrr::map_dfr(users, function(u) {
    tibble::tibble(
      owner_id = as.character(u$user_id),
      display_name = as.character(u$display_name %||% u$user_id),
      team_name = as.character((u$metadata %||% list())$team_name %||%
                                 u$display_name %||% u$user_id)
    )
  })
  roster_owner <- purrr::map_dfr(rosters, function(r) {
    tibble::tibble(roster_id = r$roster_id,
                   owner_id = as.character(r$owner_id %||% NA_character_))
  })
  roster_owner |> dplyr::left_join(user_names, by = "owner_id")
}

# Sleeper's global player directory, cached locally (it's a ~5MB single-file
# dump covering every NFL player Sleeper has ever tracked, not a per-request
# feed - refetching every run would be both slow and pointless). Returns BOTH
# id systems: sleeper_player_id (the key rosters are expressed in) and
# player_id (GSIS, this project's own join key), so roster ownership can be
# translated directly into GSIS without a name-matching step at all - the
# failure mode that bit the DK salary and TD board work earlier is a fuzzy
# name join; an id-to-id join has no such risk.
sleeper_player_directory <- function(path = "data/raw/sleeper_players.rds",
                                     max_age_days = 3) {
  fresh <- file.exists(path) &&
    difftime(Sys.time(), file.info(path)$mtime, units = "days") < max_age_days
  raw <- if (fresh) {
    readRDS(path)
  } else {
    out <- sleeper_request("/players/nfl")
    saveRDS(out, path)
    out
  }

  pick <- function(p, field) {
    v <- p[[field]]
    if (is.null(v) || length(v) != 1) NA else v
  }

  purrr::imap_dfr(raw, function(p, sleeper_id) {
    tibble::tibble(
      sleeper_player_id = as.character(sleeper_id),
      player_id = as.character(pick(p, "gsis_id")),
      full_name = as.character(pick(p, "full_name")),
      position = as.character(pick(p, "position")),
      team = as.character(pick(p, "team")),
      status = as.character(pick(p, "status")),
      injury_status = as.character(pick(p, "injury_status")),
      search_rank = suppressWarnings(as.integer(pick(p, "search_rank")))
    )
  }) |>
    dplyr::mutate(
      player_id = dplyr::na_if(.data$player_id, "NA"),
      search_rank = dplyr::if_else(.data$search_rank >= 9999, NA_integer_,
                                   .data$search_rank)
    )
}

# All (season, week) player-week ids from a set of leagues, i.e. everyone
# owned anywhere across whichever leagues the caller passed in. This is
# deliberately league-scoped, not merged into one global "owned" set, when
# called per league - see sleeper_available_players() for the per-league use.
sleeper_rostered_ids <- function(rosters) {
  unique(rosters$sleeper_player_id)
}
