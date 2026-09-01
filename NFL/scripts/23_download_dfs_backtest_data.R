source("R/utilities.R")
assert_packages()
ensure_directories()

seasons <- 2014:2021
destination <- "data/raw/player_stats_2014_2021.rds"

source_directory <- "data/raw/nflverse_player_stats"
source_paths <- file.path(
  source_directory,
  sprintf("stats_player_week_%d.rds", seasons)
)
if (!all(file.exists(source_paths))) {
  python <- file.path(
    Sys.getenv("USERPROFILE"),
    ".cache/codex-runtimes/codex-primary-runtime/dependencies/python/python.exe"
  )
  status <- system2(
    python,
    "scripts/download_nflverse_player_stats.py"
  )
  if (status != 0) {
    stop("nflverse player-stat download failed.", call. = FALSE)
  }
}
player_stats <- dplyr::bind_rows(lapply(source_paths, readRDS))
if (!nrow(player_stats)) {
  stop("Downloaded nflverse player-stat files contained no rows.", call. = FALSE)
}
saveRDS(player_stats, destination)

cat("Player-week rows:", nrow(player_stats), "\n")
cat("Seasons:", paste(sort(unique(player_stats$season)), collapse = ", "), "\n")
cat("Saved:", destination, "\n")
