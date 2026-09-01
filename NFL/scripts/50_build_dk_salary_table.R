source("R/utilities.R")
source("R/dfs_salaries.R")
assert_packages()
ensure_directories()

# Builds the DraftKings salary table from the payloads cached by scripts/48.
#
# This exists as a separate pass because slate selection during the scan was
# wrong in a way that only showed up on inspection: DraftKings runs college
# football with the same positions as the NFL and far more games per week, so
# ranking candidates by game count systematically picked the college slate.
# The fix is a team check - NFL groups score 1.0 on the 32 team codes and
# college groups score 0.0, with nothing in between - and it can be applied to
# the cached payloads without re-fetching anything.

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

nfl_team_codes <- function() {
  c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN",
    "DET", "GB", "HOU", "IND", "JAX", "KC", "LAC", "LAR", "LV", "MIA",
    "MIN", "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", "SF", "TB",
    "TEN", "WAS")
}

payload_dir <- "data/raw/dfs_salaries/dk_draftgroups/payloads"
files <- list.files(payload_dir, full.names = TRUE)
cat("Cached payloads:", length(files), "\n")

describe <- function(path) {
  body <- readRDS(path)
  draftables <- body$draftables %||% list()
  if (!length(draftables)) return(tibble::tibble())
  teams <- unique(toupper(vapply(
    draftables, function(p) as.character(p$teamAbbreviation %||% ""),
    character(1)
  )))
  teams <- teams[nzchar(teams)]
  dates <- unique(substr(vapply(
    draftables,
    function(p) as.character((p$competition %||% list())$startTime %||% ""),
    character(1)
  ), 1, 10))
  dates <- dates[nzchar(dates)]
  salaries <- vapply(
    draftables, function(p) as.numeric(p$salary %||% NA_real_), numeric(1)
  )
  positions <- toupper(vapply(
    draftables, function(p) as.character(p$position %||% ""), character(1)
  ))
  tibble::tibble(
    id = as.integer(tools::file_path_sans_ext(basename(path))),
    nfl_share = if (length(teams)) mean(teams %in% nfl_team_codes()) else NA_real_,
    n_teams = length(teams),
    n_players = dplyr::n_distinct(vapply(
      draftables, function(p) as.character(p$playerId %||% ""), character(1)
    )),
    n_salaried = sum(!is.na(salaries)),
    max_salary = suppressWarnings(max(salaries, na.rm = TRUE)),
    # Classic NFL DFS rosters QB/RB/WR/TE/DST only. Kickers and individual
    # defensive players mean some other contest format.
    idp_share = mean(positions %in% c("CB", "DE", "DT", "LB", "S", "K")),
    first_date = if (length(dates)) min(dates) else NA_character_
  )
}

inventory <- purrr::map_dfr(files, describe) |>
  dplyr::filter(
    !is.na(.data$nfl_share), .data$nfl_share >= 0.9,
    # A genuine main slate never fields more than the 32 NFL teams; a higher
    # count means the group is mixing in something else.
    .data$n_teams >= 16, .data$n_teams <= 32,
    .data$n_salaried > 0,
    # Nine 2022 groups priced every single player at exactly $2,500 and
    # rostered kickers and defensive backs. A real classic slate always tops
    # out near $9,000-10,000 for the most expensive player.
    is.finite(.data$max_salary), .data$max_salary >= 7000,
    .data$idp_share < 0.05
  )
cat("NFL classic salaried slates:", nrow(inventory), "\n")

schedule <- readRDS("data/raw/schedules.rds") |>
  dplyr::filter(.data$game_type == "REG") |>
  dplyr::group_by(.data$season, .data$week) |>
  dplyr::summarise(
    first_day = min(as.Date(.data$gameday)),
    last_day = max(as.Date(.data$gameday)),
    .groups = "drop"
  )

mapped <- inventory |>
  dplyr::mutate(day = as.Date(.data$first_date)) |>
  dplyr::inner_join(schedule, by = dplyr::join_by(
    dplyr::between(x$day, y$first_day, y$last_day)
  )) |>
  dplyr::group_by(.data$season, .data$week) |>
  dplyr::slice_max(.data$n_players, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()

readr::write_csv(mapped, "data/raw/dfs_salaries/dk_nfl_main_slates.csv")

cat("\n=== Weeks resolved ===\n")
print(as.data.frame(
  mapped |> dplyr::group_by(.data$season) |>
    dplyr::summarise(weeks = dplyr::n(), .groups = "drop")
))

expected <- tidyr::expand_grid(season = 2022:2025, week = 1:18)
gaps <- expected |>
  dplyr::anti_join(mapped, by = c("season", "week"))
cat("\n=== Still missing ===\n")
print(as.data.frame(gaps))
readr::write_csv(gaps, "outputs/dk_salary_gaps.csv")

rows <- purrr::pmap_dfr(
  list(mapped$id, mapped$season, mapped$week),
  function(id, season, week) {
    body <- readRDS(file.path(payload_dir, sprintf("%d.rds", id)))
    purrr::map_dfr(body$draftables %||% list(), function(player) {
      competition <- player$competition %||% list()
      tibble::tibble(
        season = as.integer(season), week = as.integer(week), site = "DK",
        slate_type = "MAIN",
        slate_name = sprintf("%d Week %d main", season, week),
        player_id_site = as.character(player$playerId %||% NA),
        player_name = as.character(player$displayName %||% NA),
        position = toupper(as.character(player$position %||% NA)),
        team = normalize_dfs_team(player$teamAbbreviation %||% NA),
        opponent = NA_character_, home_away = NA_character_,
        salary = suppressWarnings(as.numeric(player$salary %||% NA)),
        fantasy_points = NA_real_,
        game_info = as.character(competition$name %||% NA),
        source = "DraftKings API",
        source_reference = sprintf(
          "https://api.draftkings.com/draftgroups/v1/draftgroups/%d/draftables",
          id
        ),
        captured_at_utc = format(Sys.time(), tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")
      )
    })
  }
) |>
  dplyr::filter(is.finite(.data$salary), .data$salary > 0) |>
  # Draftables repeat a player once per eligible roster slot.
  dplyr::distinct(
    .data$season, .data$week, .data$player_id_site, .data$position,
    .keep_all = TRUE
  )

saveRDS(rows, "data/processed/dfs_salaries_dk_2022_plus.rds")
readr::write_csv(rows, "outputs/dfs_salaries_dk_2022_plus.csv")

cat("\n=== Salary table ===\n")
print(as.data.frame(
  rows |> dplyr::group_by(.data$season) |>
    dplyr::summarise(
      weeks = dplyr::n_distinct(.data$week), rows = dplyr::n(),
      per_week = round(dplyr::n() / dplyr::n_distinct(.data$week)),
      min_salary = min(.data$salary),
      median_salary = stats::median(.data$salary),
      max_salary = max(.data$salary),
      .groups = "drop"
    )
))

cat("\n=== Highest paid per season (should be NFL stars) ===\n")
print(as.data.frame(
  rows |> dplyr::group_by(.data$season) |>
    dplyr::slice_max(.data$salary, n = 3, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select("season", "week", "player_name", "position", "team", "salary")
))
