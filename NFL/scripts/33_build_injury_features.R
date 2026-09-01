source("R/utilities.R")
source("R/injury_features.R")
assert_packages()
ensure_directories()

seasons <- 2009:2025
raw_path <- "data/raw/injuries_2009_2025.rds"
team_path <- "data/processed/team_injury_features.rds"

if (!file.exists(raw_path)) {
  cat("Downloading nflverse injury reports", min(seasons), "-", max(seasons), "\n")
  injuries <- nflreadr::load_injuries(seasons)
  saveRDS(injuries, raw_path)
} else {
  injuries <- readRDS(raw_path)
}

team_injuries <- build_team_injury_features(injuries)
saveRDS(team_injuries, team_path)

cat("Injury report rows:", nrow(injuries), "\n")
cat("Team-weeks with a report:", nrow(team_injuries), "\n")
cat("Seasons:", paste(range(team_injuries$season), collapse = "-"), "\n")
cat("Team-weeks with the QB listed out:", sum(team_injuries$inj_qb_out), "\n")

cat("\nBurden distribution by season:\n")
print(as.data.frame(
  team_injuries |>
    dplyr::group_by(.data$season) |>
    dplyr::summarise(
      team_weeks = dplyr::n(),
      mean_burden = mean(.data$inj_burden),
      mean_out = mean(.data$inj_out_total),
      qb_out_rate = mean(.data$inj_qb_out),
      .groups = "drop"
    )
), digits = 4)
