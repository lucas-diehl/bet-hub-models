source("R/utilities.R")
source("R/dfs_salaries.R")
assert_packages()
ensure_directories()

config <- yaml::read_yaml("config/dfs_salaries.yml")
start_season <- as.integer(config$historical$start_season)
end_season <- as.integer(config$historical$end_season)
week_limits <- as.integer(unlist(config$historical$weeks))
sites <- unlist(config$historical$sites)
delay <- as.numeric(config$historical$request_delay_seconds)

historical <- backfill_rotoguru_salaries(
  seasons = seq.int(start_season, end_season),
  weeks = seq.int(week_limits[[1]], week_limits[[2]]),
  sites = sites,
  request_delay_seconds = delay
)
manual <- ingest_manual_dfs_salary_directory(
  config$current$manual_drop_directory
)
combined <- deduplicate_dfs_salaries(dplyr::bind_rows(historical, manual))

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(combined, "data/processed/dfs_salaries.rds")
readr::write_csv(combined, "outputs/dfs_salaries.csv")

coverage <- combined |>
  dplyr::group_by(.data$site, .data$season) |>
  dplyr::summarise(
    weeks = dplyr::n_distinct(.data$week),
    first_week = min(.data$week, na.rm = TRUE),
    last_week = max(.data$week, na.rm = TRUE),
    player_rows = dplyr::n(),
    unique_players = dplyr::n_distinct(.data$player_id_site),
    minimum_salary = min(.data$salary, na.rm = TRUE),
    median_salary = stats::median(.data$salary, na.rm = TRUE),
    maximum_salary = max(.data$salary, na.rm = TRUE),
    .groups = "drop"
  )
readr::write_csv(coverage, "outputs/dfs_salary_coverage.csv")

checks <- tibble::tibble(
  check = c(
    "rows",
    "duplicate rows",
    "missing season",
    "missing week",
    "missing player name",
    "missing salary",
    "nonpositive salary",
    "unknown team codes"
  ),
  value = c(
    nrow(combined),
    nrow(combined) - nrow(dplyr::distinct(
      combined,
      .data$season, .data$week, .data$site, .data$slate_name,
      .data$player_id_site, .data$player_name, .data$position
    )),
    sum(is.na(combined$season)),
    sum(is.na(combined$week)),
    sum(is.na(combined$player_name) | combined$player_name == ""),
    sum(is.na(combined$salary)),
    sum(combined$salary <= 0, na.rm = TRUE),
    sum(!combined$team %in% c(
      "ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
      "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
      "LAC", "LAR", "LV", "MIA", "MIN", "NE", "NO", "NYG",
      "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS"
    ), na.rm = TRUE)
  )
)
readr::write_csv(checks, "outputs/dfs_salary_checks.csv")

cat(
  sprintf(
    "Saved %s salary rows across %s site-seasons.\n",
    format(nrow(combined), big.mark = ","),
    nrow(coverage)
  )
)

