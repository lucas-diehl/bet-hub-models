## ---------------------------------------------------------------------------
## 81_build_current_salary_table.R
##
## Turns scripts/21's raw DraftKings capture into a current-season main-slate
## salary table the projection model can actually join against.
##
## Why this is a separate script: scripts/21 captures every active draft group
## verbatim and writes week = NA. That raw capture is not usable as a salary
## signal for two reasons:
##
##   1. NO WEEK. Salary has to join to player-weeks on (season, week, team,
##      position, name). scripts/21 leaves week NA, so nothing can join.
##   2. SHOWDOWN CONTAMINATION. A DK single-game Showdown slate prices its
##      players on a completely different scale - the 2026 Week 2 capture has
##      Jahmyr Gibbs at $18,000 and Josh Allen at $17,100 in 92-row single-game
##      groups, against a ~$9-10k top end on the real main slate. Pooling them
##      does not add data, it destroys the signal: the same player carries two
##      incompatible prices in the same week.
##
## The classification rules below are lifted deliberately unchanged from
## scripts/50_build_dk_salary_table.R, which already solved this for the
## 2022-2025 backfill (n_teams 16-32 excludes single-game slates, max_salary
## >= 7000 excludes the flat-priced junk groups, idp_share < 0.05 excludes
## non-classic formats). Same rules here means the live table and the
## historical table mean the same thing - which is the whole point, since the
## model trains on one and projects with the other.
##
## Idempotent and accumulating: each run merges the latest capture into the
## table, keeping the most recent capture per (season, week, player), so
## running it repeatedly through a week is safe and picks up price moves.
##
## Run:  & $rscript scripts/21_capture_current_dk_salaries.R   # fetch first
##       & $rscript scripts/81_build_current_salary_table.R
## ---------------------------------------------------------------------------

source("R/utilities.R")
source("R/dfs_salaries.R")
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
})
assert_packages()

OUT_PATH <- "outputs/dfs_salaries_dk_current.csv"
OUT_RDS <- "data/processed/dfs_salaries_dk_current.rds"

## Local copy, mirroring scripts/48 and scripts/50 which each define their own.
## Kept identical to theirs on purpose: this script's whole value depends on
## classifying slates by the same rules the historical table was built with.
nfl_team_codes <- function() {
  c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN",
    "DET", "GB", "HOU", "IND", "JAX", "KC", "LAC", "LAR", "LV", "MIA",
    "MIN", "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", "SF", "TB",
    "TEN", "WAS")
}

config <- yaml::read_yaml("config/dfs_salaries.yml")
archive_directory <- file.path(config$current$archive_directory, "draftkings_api")
if (!dir.exists(archive_directory)) {
  stop("No capture archive at ", archive_directory,
       " - run scripts/21_capture_current_dk_salaries.R first.", call. = FALSE)
}

files <- list.files(archive_directory, pattern = "_draft_group_\\d+\\.json$",
                    full.names = TRUE)
if (!length(files)) {
  stop("No draft-group payloads in ", archive_directory,
       " - run scripts/21_capture_current_dk_salaries.R first.", call. = FALSE)
}
message("Draft-group payloads found: ", length(files))

## ---------------------------------------------------------------------------
## 1. Describe each captured draft group (same measures scripts/50 uses)
## ---------------------------------------------------------------------------

describe <- function(path) {
  body <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  draftables <- body$draftables %||% list()
  if (!length(draftables)) return(NULL)

  teams <- toupper(vapply(
    draftables, function(p) as.character(p$teamAbbreviation %||% ""), character(1)
  ))
  teams <- teams[nzchar(teams)]
  dates <- substr(vapply(
    draftables,
    function(p) as.character((p$competition %||% list())$startTime %||% ""),
    character(1)
  ), 1, 10)
  dates <- dates[nzchar(dates)]
  salaries <- vapply(
    draftables, function(p) as.numeric(p$salary %||% NA_real_), numeric(1)
  )
  positions <- toupper(vapply(
    draftables, function(p) as.character(p$position %||% ""), character(1)
  ))

  tibble::tibble(
    path = path,
    id = suppressWarnings(as.integer(sub(".*_draft_group_(\\d+)\\.json$", "\\1", path))),
    captured = sub("^(\\d{8}T\\d{6}Z)_.*$", "\\1", basename(path)),
    nfl_share = if (length(teams)) mean(teams %in% nfl_team_codes()) else NA_real_,
    n_teams = dplyr::n_distinct(teams),
    n_players = dplyr::n_distinct(vapply(
      draftables, function(p) as.character(p$playerId %||% ""), character(1)
    )),
    n_salaried = sum(!is.na(salaries)),
    max_salary = suppressWarnings(max(salaries, na.rm = TRUE)),
    idp_share = mean(positions %in% c("CB", "DE", "DT", "LB", "S", "K")),
    first_date = if (length(dates)) min(dates) else NA_character_
  )
}

inventory_all <- map_dfr(files, describe)
message("Groups described: ", nrow(inventory_all))

inventory <- inventory_all |>
  filter(
    !is.na(.data$nfl_share), .data$nfl_share >= 0.9,
    .data$n_teams >= 16, .data$n_teams <= 32,
    .data$n_salaried > 0,
    is.finite(.data$max_salary), .data$max_salary >= 7000,
    .data$idp_share < 0.05
  )

cat("\n-- classification --\n")
cat("  groups captured:        ", nrow(inventory_all), "\n")
cat("  classic main-slate:     ", nrow(inventory), "\n")
cat("  rejected (showdown/etc):", nrow(inventory_all) - nrow(inventory), "\n")
if (nrow(inventory_all) > nrow(inventory)) {
  cat("\n  rejected groups and why:\n")
  print(as.data.frame(
    inventory_all |>
      anti_join(inventory, by = "id") |>
      transmute(
        id = .data$id, n_teams = .data$n_teams, n_players = .data$n_players,
        max_salary = .data$max_salary, idp_share = round(.data$idp_share, 3),
        reason = case_when(
          .data$n_salaried == 0 ~ "not priced yet",
          is.na(.data$nfl_share) | .data$nfl_share < 0.9 ~ "not NFL",
          .data$n_teams < 16 ~ "single-game/showdown",
          .data$n_teams > 32 ~ "more than 32 teams",
          .data$max_salary < 7000 ~ "flat/low pricing",
          .data$idp_share >= 0.05 ~ "kickers/IDP format",
          TRUE ~ "other"
        )
      )
  ), row.names = FALSE)
}
if (!nrow(inventory)) {
  stop("No classic main slate found in the capture. If it is mid-week with no ",
       "slate posted yet, this is expected - re-run closer to the weekend.",
       call. = FALSE)
}

## ---------------------------------------------------------------------------
## 2. Resolve each slate to an NFL (season, week) by its first kickoff date
## ---------------------------------------------------------------------------

schedule_paths <- c("data/raw/schedules_2026.rds", "data/raw/schedules.rds")
schedule <- map_dfr(schedule_paths[file.exists(schedule_paths)], readRDS) |>
  filter(.data$game_type == "REG") |>
  group_by(season = as.integer(.data$season), week = as.integer(.data$week)) |>
  summarise(
    first_day = min(as.Date(.data$gameday)),
    last_day = max(as.Date(.data$gameday)),
    .groups = "drop"
  )

mapped <- inventory |>
  mutate(day = as.Date(.data$first_date)) |>
  filter(!is.na(.data$day)) |>
  inner_join(schedule, by = join_by(between(x$day, y$first_day, y$last_day))) |>
  ## One slate per (season, week): the biggest player pool is the main slate.
  group_by(.data$season, .data$week) |>
  slice_max(.data$n_players, n = 1, with_ties = FALSE) |>
  ungroup()

cat("\n-- resolved slates --\n")
print(as.data.frame(
  mapped |> select("season", "week", "id", "n_teams", "n_players", "max_salary",
                   "first_date")
), row.names = FALSE)
if (!nrow(mapped)) {
  stop("Captured a classic slate but could not map it to a scheduled NFL week.",
       call. = FALSE)
}

## ---------------------------------------------------------------------------
## 3. Flatten to the same schema as outputs/dfs_salaries_dk_2022_plus.csv
## ---------------------------------------------------------------------------

rows <- pmap_dfr(
  list(mapped$path, mapped$id, mapped$season, mapped$week),
  function(path, id, season, week) {
    body <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    map_dfr(body$draftables %||% list(), function(player) {
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
          "https://api.draftkings.com/draftgroups/v1/draftgroups/%d/draftables", id
        ),
        captured_at_utc = format(Sys.time(), tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")
      )
    })
  }
) |>
  filter(is.finite(.data$salary), .data$salary > 0) |>
  ## Draftables repeat a player once per eligible roster slot (FLEX etc).
  distinct(.data$season, .data$week, .data$player_id_site, .keep_all = TRUE)

## Merge with anything captured on earlier runs, newest capture winning.
if (file.exists(OUT_PATH)) {
  ## Force every id/text column to character on read. readr's type guesser
  ## mis-infers player_id_site as double (looks numeric) and captured_at_utc
  ## as a parsed datetime depending on what happened to be in the file, either
  ## of which breaks bind_rows() against this run's character/character
  ## columns the next time this script runs. `rows` below is written back out
  ## as plain character/character too, so this keeps both sides consistent.
  previous <- readr::read_csv(
    OUT_PATH, show_col_types = FALSE, guess_max = 50000,
    col_types = readr::cols(
      season = "i", week = "i", salary = "d", fantasy_points = "d",
      .default = "c"
    )
  )
  rows <- bind_rows(rows, previous) |>
    arrange(desc(.data$captured_at_utc)) |>
    distinct(.data$season, .data$week, .data$player_id_site, .keep_all = TRUE)
}
rows <- rows |> arrange(.data$season, .data$week, desc(.data$salary))

readr::write_csv(rows, OUT_PATH)
saveRDS(rows, OUT_RDS)

cat("\n================ CURRENT SALARY TABLE ================\n")
cat("\nrows:", nrow(rows), " ->  ", OUT_PATH, "\n")
cat("\n-- coverage --\n")
print(as.data.frame(rows |> count(season, week)), row.names = FALSE)
cat("\n-- salary distribution by position (offense) --\n")
print(as.data.frame(
  rows |> filter(.data$position %in% c("QB", "RB", "WR", "TE")) |>
    group_by(.data$position) |>
    summarise(n = n(), min = min(.data$salary), median = median(.data$salary),
              max = max(.data$salary), .groups = "drop")
), row.names = FALSE)
cat("\n  Sanity: a classic DK main slate tops out near $9,000-10,000. A max in\n",
    " the $15,000-18,000 range means a Showdown slate leaked through.\n")
cat("\n-- most expensive players (face validity) --\n")
print(as.data.frame(
  rows |> filter(.data$position %in% c("QB", "RB", "WR", "TE")) |>
    arrange(desc(.data$salary)) |>
    select("season", "week", "player_name", "position", "team", "salary") |>
    head(10)
), row.names = FALSE)
cat("\n=====================================================\n")
