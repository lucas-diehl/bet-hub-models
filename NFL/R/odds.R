download_rotowire <- function(url, destination, refresh = FALSE) {
  if (file.exists(destination) && !refresh) {
    message("Using cached RotoWire archive: ", destination)
    return(destination)
  }

  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  utils::download.file(
    url,
    destfile = tmp,
    mode = "wb",
    method = "libcurl",
    quiet = FALSE,
    headers = c(
      "User-Agent" = paste(
        "Mozilla/5.0 (compatible; academic NFL model;",
        "single cached request)"
      )
    )
  )
  if (file.info(tmp)$size < 1000) stop("RotoWire response was unexpectedly small.")
  if (!file.rename(tmp, destination)) {
    ok <- file.copy(tmp, destination, overwrite = TRUE)
    if (!ok) stop("Could not save RotoWire archive to ", destination)
  }
  destination
}

read_rotowire <- function(path) {
  odds <- jsonlite::fromJSON(path, flatten = TRUE) |>
    dplyr::as_tibble() |>
    dplyr::transmute(
      season = as.integer(.data$season),
      week = as.integer(.data$week),
      game_date = as.Date(.data$game_date),
      home_team = normalize_team(.data$home_team_stats_id),
      away_team = normalize_team(.data$visit_team_stats_id),
      home_score_rw = as.numeric(.data$home_team_score),
      away_score_rw = as.numeric(.data$visit_team_score),
      total_line = readr::parse_double(as.character(.data$game_over_under)),
      home_line = as.numeric(.data$line),
      surface = dplyr::na_if(as.character(.data$surface), ""),
      weather = dplyr::na_if(as.character(.data$weather_icon), ""),
      temperature = readr::parse_double(as.character(.data$temperature)),
      precip_probability = readr::parse_double(
        stringr::str_remove(as.character(.data$precip_probability), "%$")
      ),
      precip_type = dplyr::na_if(as.character(.data$precip_type), ""),
      wind_speed = readr::parse_double(as.character(.data$wind_speed))
    ) |>
    dplyr::filter(
      !is.na(.data$season), !is.na(.data$week),
      !is.na(.data$home_team), !is.na(.data$away_team)
    ) |>
    dplyr::distinct(
      .data$season, .data$week, .data$game_date,
      .data$home_team, .data$away_team, .keep_all = TRUE
    )

  odds
}

join_odds <- function(games, odds) {
  exact <- games |>
    dplyr::left_join(
      odds,
      by = c("season", "week", "game_date", "home_team", "away_team")
    )

  # Date fields occasionally differ because of rescheduled games. Only retry
  # unmatched rows on the unique season/week/team key.
  fallback <- odds |>
    dplyr::select(-"game_date") |>
    dplyr::group_by(.data$season, .data$week, .data$home_team, .data$away_team) |>
    dplyr::filter(dplyr::n() == 1L) |>
    dplyr::ungroup()

  missing <- which(is.na(exact$total_line) & is.na(exact$home_line))
  if (length(missing)) {
    retry <- games[missing, ] |>
      dplyr::left_join(
        fallback,
        by = c("season", "week", "home_team", "away_team")
      )
    odds_cols <- setdiff(names(fallback), c("season", "week", "home_team", "away_team"))
    for (nm in odds_cols) exact[[nm]][missing] <- retry[[nm]]
  }
  exact
}
