source("R/utilities.R")
source("R/dfs_salaries.R")
assert_packages()

salary_path <- "data/processed/dfs_salaries.rds"
if (!file.exists(salary_path)) {
  stop("Run scripts/19_backfill_dfs_salaries.R first.", call. = FALSE)
}
salaries <- readRDS(salary_path)

expected_columns <- dfs_salary_schema()
missing_columns <- setdiff(expected_columns, names(salaries))
if (length(missing_columns)) {
  stop(
    "Missing normalized columns: ", paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}
stopifnot(
  nrow(salaries) == 132784L,
  sum(salaries$site == "DK") == 54894L,
  sum(salaries$site == "FD") == 77890L,
  all(is.finite(salaries$salary)),
  all(salaries$salary > 0),
  !anyDuplicated(salaries[c(
    "season", "week", "site", "slate_name",
    "player_id_site", "player_name", "position"
  )])
)

sample_path <- tempfile(fileext = ".csv")
writeLines(
  c(
    paste(
      "Position", "Name + ID", "Name", "ID", "Roster Position",
      "Salary", "Game Info", "TeamAbbrev", "AvgPointsPerGame",
      sep = ","
    ),
    paste(
      "QB", "Patrick Mahomes (1)", "Patrick Mahomes", "1", "QB",
      "8000", "KC@BUF 01/01/2026", "KC", "25.5",
      sep = ","
    )
  ),
  sample_path
)
sample <- parse_manual_dfs_salary_file(sample_path, 2026L, 1L)
unlink(sample_path)
stopifnot(
  nrow(sample) == 1L,
  sample$site[[1]] == "DK",
  sample$salary[[1]] == 8000,
  sample$team[[1]] == "KC",
  sample$season[[1]] == 2026L,
  sample$week[[1]] == 1L
)

cat("DFS salary validation passed, including official DK CSV ingestion.\n")

