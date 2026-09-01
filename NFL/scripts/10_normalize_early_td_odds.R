source("R/utilities.R")
source("R/odds_api.R")
assert_packages()
ensure_directories()

raw_dir <- "data/raw/odds_api_td_early"
manifest_path <- "data/raw/td_odds_early_manifest.csv"
output_path <- "data/processed/anytime_td_odds_early_2023_2025.csv"

if (!dir.exists(raw_dir) || !file.exists(manifest_path)) {
  stop("Early-market raw files or manifest are missing.", call. = FALSE)
}

manifest <- readr::read_csv(
  manifest_path,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

schedules <- readRDS("data/raw/schedules.rds") |>
  dplyr::filter(
    .data$season %in% 2023:2025,
    .data$game_type == "REG"
  ) |>
  dplyr::mutate(
    kickoff_utc = nfl_kickoff_utc(.data$gameday, .data$gametime)
  ) |>
  dplyr::left_join(
    dplyr::select(
      manifest,
      "game_id",
      snapshot_utc = "requested_snapshot"
    ),
    by = "game_id"
  ) |>
  dplyr::arrange(.data$season, .data$week, .data$kickoff_utc)

raw_paths <- file.path(raw_dir, paste0(schedules$game_id, ".json"))
available <- which(file.exists(raw_paths))
normalized <- purrr::map_dfr(available, function(i) {
  payload <- jsonlite::read_json(raw_paths[[i]], simplifyVector = FALSE)
  flatten_anytime_td_payload(payload, schedules[i, ])
}) |>
  dplyr::distinct()

readr::write_csv(normalized, output_path)
cat("Cached games normalized:", length(available), "\n")
cat("Normalized early-market price rows:", nrow(normalized), "\n")
cat("Saved:", output_path, "\n")
