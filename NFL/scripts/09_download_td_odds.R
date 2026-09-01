source("R/utilities.R")
source("R/odds_api.R")
assert_packages()
ensure_directories()

args <- commandArgs(trailingOnly = TRUE)
execute <- "--execute" %in% args
max_arg <- grep("^--max-events=", args, value = TRUE)
max_new_events <- if (length(max_arg)) {
  as.integer(sub("^--max-events=", "", max_arg[[1]]))
} else {
  Inf
}
if (is.na(max_new_events) || max_new_events < 1) {
  stop("--max-events must be a positive integer.", call. = FALSE)
}

seasons <- 2023:2025
snapshot_minutes <- 60L
quota_floor <- 500L
schedule_path <- "data/raw/schedules.rds"
manifest_path <- "data/raw/td_odds_close_manifest.csv"
event_ids_path <- "data/raw/td_odds_event_ids.csv"
normalized_path <- "data/processed/anytime_td_odds_close_2023_2025.csv"
raw_dir <- "data/raw/odds_api_td_close"
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(schedule_path)) {
  stop("Missing data/raw/schedules.rds. Run scripts/01_download.R first.", call. = FALSE)
}

schedules <- readRDS(schedule_path) |>
  dplyr::filter(
    .data$season %in% seasons,
    .data$game_type == "REG"
  ) |>
  dplyr::mutate(
    kickoff_utc = nfl_kickoff_utc(.data$gameday, .data$gametime),
    snapshot_utc = format(
      lubridate::ymd_hms(.data$kickoff_utc, tz = "UTC") -
        lubridate::minutes(snapshot_minutes),
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )
  ) |>
  dplyr::arrange(.data$season, .data$week, .data$kickoff_utc)

raw_paths <- file.path(raw_dir, paste0(schedules$game_id, ".json"))
cached <- file.exists(raw_paths)
new_events <- sum(!cached)
known_event_ids <- if (file.exists(event_ids_path)) {
  readr::read_csv(
    event_ids_path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ) |>
    dplyr::distinct(.data$game_id, .keep_all = TRUE)
} else {
  tibble::tibble(game_id = character(), event_id = character())
}
missing_id_count <- sum(
  !schedules$game_id[!cached] %in% known_event_ids$game_id
)
estimated_cost <- estimate_historical_prop_cost(new_events) + missing_id_count

cat("Regular-season games:", nrow(schedules), "\n")
cat("Already cached:", sum(cached), "\n")
cat("New event requests:", new_events, "\n")
cat("Event-ID lookups required:", missing_id_count, "\n")
cat("Estimated additional credits:", estimated_cost, "\n")
cat("Snapshot: closest available market at", snapshot_minutes, "minutes pre-kickoff\n")

if (!execute) {
  cat("Dry run only. Add --execute to make API requests.\n")
  quit(save = "no", status = 0)
}

manifest <- if (file.exists(manifest_path)) {
  readr::read_csv(
    manifest_path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
} else {
  tibble::tibble(
    game_id = character(),
    event_id = character(),
    requested_snapshot = character(),
    returned_snapshot = character()
  )
}

team_names <- odds_api_team_names()
new_calls <- 0L
last_remaining <- Inf

for (i in seq_len(nrow(schedules))) {
  game <- schedules[i, ]
  raw_path <- raw_paths[[i]]
  if (file.exists(raw_path)) next
  if (new_calls >= max_new_events) break
  if (is.finite(last_remaining) && last_remaining - 10L < quota_floor) {
    message("Stopping at quota floor of ", quota_floor, " credits.")
    break
  }

  known <- known_event_ids |>
    dplyr::filter(.data$game_id == game$game_id)
  if (nrow(known)) {
    event_id <- known$event_id[[1]]
  } else {
    events_result <- odds_api_historical_events(game$snapshot_utc)
    events <- events_result$data$data
    expected_home <- unname(team_names[[game$home_team]])
    expected_away <- unname(team_names[[game$away_team]])
    target <- events |>
      dplyr::filter(
        .data$home_team == expected_home,
        .data$away_team == expected_away
      )
    if (nrow(target) != 1L) {
      stop(
        "Could not resolve event uniquely for ", game$game_id,
        " (", expected_away, " at ", expected_home, ").",
        call. = FALSE
      )
    }
    event_id <- target$id[[1]]
    known_event_ids <- known_event_ids |>
      dplyr::filter(.data$game_id != game$game_id) |>
      dplyr::bind_rows(tibble::tibble(
        game_id = as.character(game$game_id),
        event_id = as.character(event_id)
      ))
    readr::write_csv(known_event_ids, event_ids_path)
  }

  result <- odds_api_historical_event_props(
    event_id = event_id,
    snapshot_time = game$snapshot_utc
  )
  jsonlite::write_json(
    result$data,
    raw_path,
    auto_unbox = TRUE,
    pretty = FALSE,
    na = "null"
  )
  new_calls <- new_calls + 1L
  last_remaining <- result$quota$remaining

  manifest <- manifest |>
    dplyr::filter(.data$game_id != game$game_id) |>
    dplyr::bind_rows(tibble::tibble(
      game_id = game$game_id,
      event_id = event_id,
      requested_snapshot = game$snapshot_utc,
      returned_snapshot = as.character(result$data$timestamp)
    ))
  readr::write_csv(manifest, manifest_path)
  message(
    "[", new_calls, "] Cached ", game$game_id,
    "; quota remaining: ", last_remaining
  )
}

available <- which(file.exists(raw_paths))
normalized <- purrr::map_dfr(available, function(i) {
  payload <- jsonlite::read_json(raw_paths[[i]], simplifyVector = FALSE)
  flatten_anytime_td_payload(payload, schedules[i, ])
}) |>
  dplyr::distinct()
readr::write_csv(normalized, normalized_path)

cat("New API event requests completed:", new_calls, "\n")
cat("Cached games normalized:", length(available), "\n")
cat("Normalized price rows:", nrow(normalized), "\n")
cat("Saved:", normalized_path, "\n")
