source("R/utilities.R")
source("R/dfs_salaries.R")
assert_packages()
ensure_directories()

or_else <- function(x, y) {
  if (is.null(x) || !length(x)) y else x
}

config <- yaml::read_yaml("config/dfs_salaries.yml")
archive_directory <- file.path(
  config$current$archive_directory,
  "draftkings_api"
)
dir.create(archive_directory, recursive = TRUE, showWarnings = FALSE)

lobby_url <- paste0(
  "https://www.draftkings.com/lobby/getcontests?sport=",
  config$current$sport
)
lobby_response <- dfs_request(lobby_url) |>
  httr2::req_user_agent("nfl-dfs-salary-research/1.0") |>
  httr2::req_timeout(30) |>
  httr2::req_retry(max_tries = 3) |>
  httr2::req_perform()
lobby <- httr2::resp_body_json(lobby_response, simplifyVector = FALSE)
draft_groups <- lobby$DraftGroups
if (!length(draft_groups)) {
  stop(
    "DraftKings returned no active NFL draft groups. Try again after 2026 slates open.",
    call. = FALSE
  )
}

timestamp <- format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%SZ")
writeLines(
  jsonlite::toJSON(lobby, auto_unbox = TRUE, pretty = TRUE, null = "null"),
  file.path(archive_directory, paste0(timestamp, "_lobby.json")),
  useBytes = TRUE
)

rows <- purrr::map_dfr(draft_groups, function(group) {
  group_id <- group$DraftGroupId
  url <- sprintf(
    "https://api.draftkings.com/draftgroups/v1/draftgroups/%s/draftables",
    group_id
  )
  response <- dfs_request(url) |>
    httr2::req_user_agent("nfl-dfs-salary-research/1.0") |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()
  payload <- httr2::resp_body_json(response, simplifyVector = FALSE)
  writeLines(
    jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    file.path(
      archive_directory,
      sprintf("%s_draft_group_%s.json", timestamp, group_id)
    ),
    useBytes = TRUE
  )

  purrr::map_dfr(payload$draftables, function(player) {
    competition <- player$competition
    tibble::tibble(
      season = as.integer(format(Sys.Date(), "%Y")),
      week = NA_integer_,
      site = "DK",
      slate_type = "DRAFT_GROUP",
      slate_name = or_else(group$Name, paste("Draft group", group_id)),
      player_id_site = as.character(player$playerId),
      player_name = or_else(player$displayName, NA_character_),
      position = toupper(or_else(player$position, NA_character_)),
      team = normalize_dfs_team(
        or_else(
          player$teamAbbreviation,
          or_else(player$teamName, NA_character_)
        )
      ),
      opponent = NA_character_,
      home_away = NA_character_,
      salary = as.numeric(or_else(player$salary, NA_real_)),
      fantasy_points = NA_real_,
      game_info = or_else(competition$name, NA_character_),
      source = "DraftKings API",
      source_reference = url,
      captured_at_utc = format(
        Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
      )
    )
  })
})

readr::write_csv(
  rows,
  file.path(archive_directory, paste0(timestamp, "_normalized.csv"))
)
cat(
  sprintf(
    "Captured %s rows from %s active NFL draft groups.\n",
    format(nrow(rows), big.mark = ","),
    length(draft_groups)
  )
)
