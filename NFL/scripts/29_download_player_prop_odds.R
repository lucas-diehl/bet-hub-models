source("R/utilities.R")
source("R/odds_api.R")
assert_packages()
ensure_directories()

# Historical over/under player props from The Odds API.
#
# Cost is 10 credits per event per market per region, charged whether or not
# the book quoted anything. Everything here is built around not wasting those:
# event IDs come from the cache built by scripts/09, each market is cached in
# its own directory so a partial pull is never repurchased, and the script
# refuses to start without --execute.
#
#   --execute            actually call the API (otherwise dry run only)
#   --markets=a,b        labels from player_prop_market_keys(); default four
#   --seasons=2024,2025  regular seasons to cover
#   --weeks=1,2          optional regular-season week filter, for cheap pilots
#   --max-events=N       stop after N new paid calls (per run, across markets)
#   --quota-floor=N      stop before the balance would drop below N

args <- commandArgs(trailingOnly = TRUE)
execute <- "--execute" %in% args

arg_value <- function(flag, default) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", flag, "="), "", hit[[1]])
}

market_labels <- strsplit(
  arg_value(
    "--markets",
    "passing_yards,receptions,receiving_yards,rushing_yards"
  ),
  ",", fixed = TRUE
)[[1]] |> trimws()
seasons <- as.integer(strsplit(
  arg_value("--seasons", "2024,2025"), ",", fixed = TRUE
)[[1]])
weeks_arg <- arg_value("--weeks", "")
weeks <- if (nzchar(weeks_arg)) {
  as.integer(strsplit(weeks_arg, ",", fixed = TRUE)[[1]])
} else {
  NULL
}
max_new_calls <- suppressWarnings(as.numeric(arg_value("--max-events", "Inf")))
quota_floor <- as.integer(arg_value("--quota-floor", "500"))

known_markets <- player_prop_market_keys()
unknown <- setdiff(market_labels, names(known_markets))
if (length(unknown)) {
  stop(
    "Unknown market label(s): ", paste(unknown, collapse = ", "),
    ". Available: ", paste(names(known_markets), collapse = ", "),
    call. = FALSE
  )
}
if (anyNA(seasons)) stop("--seasons must be integers.", call. = FALSE)
if (is.na(max_new_calls) || max_new_calls < 1) {
  stop("--max-events must be a positive integer.", call. = FALSE)
}

snapshot_minutes <- 60L
schedule_path <- "data/raw/schedules.rds"
event_ids_path <- "data/raw/td_odds_event_ids.csv"
raw_root <- "data/raw/odds_api_player_props"
manifest_path <- "data/raw/player_prop_odds_manifest.csv"
normalized_path <- "data/processed/player_prop_odds.csv"

if (!file.exists(schedule_path)) {
  stop("Missing data/raw/schedules.rds. Run scripts/01_download.R first.",
       call. = FALSE)
}
if (!file.exists(event_ids_path)) {
  stop(
    "Missing data/raw/td_odds_event_ids.csv. Event IDs are reused from the ",
    "touchdown pull so this script never pays to look them up again.",
    call. = FALSE
  )
}

schedules <- readRDS(schedule_path) |>
  dplyr::filter(.data$season %in% seasons, .data$game_type == "REG") |>
  (\(x) if (is.null(weeks)) x else dplyr::filter(x, .data$week %in% weeks))() |>
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

event_ids <- readr::read_csv(
  event_ids_path,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
) |>
  dplyr::distinct(.data$game_id, .keep_all = TRUE)

schedules <- schedules |>
  dplyr::left_join(event_ids, by = "game_id")
missing_ids <- sum(is.na(schedules$event_id))

# Build the full work list up front so the dry run reports the real cost.
# Game-major order so that a run capped by --max-events returns a balanced
# sample across markets rather than exhausting the first market alphabetically.
work <- tidyr::expand_grid(
  game_index = seq_len(nrow(schedules)),
  market_label = market_labels
) |>
  dplyr::mutate(
    game_id = schedules$game_id[.data$game_index],
    market_key = unname(known_markets[.data$market_label]),
    raw_path = file.path(
      raw_root, .data$market_label, paste0(.data$game_id, ".json")
    ),
    cached = file.exists(.data$raw_path)
  )

pending <- work |> dplyr::filter(!.data$cached)
cost_estimate <- estimate_historical_prop_cost(nrow(pending))

cat("Seasons:", paste(seasons, collapse = ", "), "\n")
cat("Games:", nrow(schedules), "\n")
cat("Markets:", paste(market_labels, collapse = ", "), "\n")
cat("Games missing a cached event ID:", missing_ids, "\n")
cat("Already cached game-markets:", sum(work$cached), "\n")
cat("Pending game-markets:", nrow(pending), "\n")
cat("Estimated credits for pending:", cost_estimate, "\n")
cat("Snapshot:", snapshot_minutes, "minutes before kickoff\n")
print(
  pending |>
    dplyr::count(.data$market_label, name = "pending_calls") |>
    dplyr::mutate(credits = .data$pending_calls * 10L)
)

if (!execute) {
  cat("\nDry run only. Add --execute to spend credits.\n")
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
    game_id = character(), market_label = character(),
    event_id = character(), requested_snapshot = character(),
    returned_snapshot = character()
  )
}

for (label in market_labels) {
  dir.create(file.path(raw_root, label), recursive = TRUE, showWarnings = FALSE)
}

new_calls <- 0L
last_remaining <- Inf
stop_run <- FALSE

for (i in seq_len(nrow(pending))) {
  if (stop_run) break
  row <- pending[i, ]
  game <- schedules[row$game_index, ]
  if (is.na(game$event_id)) {
    message("Skipping ", game$game_id, "; no cached event ID.")
    next
  }
  if (new_calls >= max_new_calls) {
    message("Reached --max-events limit of ", max_new_calls, ".")
    break
  }
  if (is.finite(last_remaining) && last_remaining - 10L < quota_floor) {
    message("Stopping at quota floor of ", quota_floor, " credits.")
    break
  }

  result <- tryCatch(
    odds_api_historical_event_props(
      event_id = game$event_id,
      snapshot_time = game$snapshot_utc,
      market = row$market_key
    ),
    error = function(e) {
      message("Request failed for ", game$game_id, " / ", row$market_label,
              ": ", conditionMessage(e))
      stop_run <<- TRUE
      NULL
    }
  )
  if (is.null(result)) next

  jsonlite::write_json(
    result$data, row$raw_path,
    auto_unbox = TRUE, pretty = FALSE, na = "null"
  )
  new_calls <- new_calls + 1L
  last_remaining <- result$quota$remaining

  manifest <- manifest |>
    dplyr::filter(
      !(.data$game_id == game$game_id & .data$market_label == row$market_label)
    ) |>
    dplyr::bind_rows(tibble::tibble(
      game_id = game$game_id,
      market_label = row$market_label,
      event_id = game$event_id,
      requested_snapshot = game$snapshot_utc,
      returned_snapshot = as.character(result$data$timestamp)
    ))

  if (new_calls %% 25L == 0L || new_calls == 1L) {
    readr::write_csv(manifest, manifest_path)
    message("[", new_calls, "/", nrow(pending), "] ", game$game_id,
            " / ", row$market_label, "; quota remaining: ", last_remaining)
  }
}
readr::write_csv(manifest, manifest_path)

# Normalize everything cached, including anything pulled by earlier runs.
all_cached <- work |> dplyr::mutate(cached = file.exists(.data$raw_path)) |>
  dplyr::filter(.data$cached)
normalized <- purrr::pmap_dfr(
  list(all_cached$raw_path, all_cached$market_key, all_cached$game_index),
  function(path, market_key, game_index) {
    payload <- jsonlite::read_json(path, simplifyVector = FALSE)
    flatten_player_prop_payload(payload, market_key, schedules[game_index, ])
  }
) |>
  dplyr::distinct()

readr::write_csv(normalized, normalized_path)

cat("\nNew paid requests this run:", new_calls, "\n")
cat("Quota remaining:", last_remaining, "\n")
cat("Cached game-markets normalized:", nrow(all_cached), "\n")
cat("Normalized price rows:", nrow(normalized), "\n")
cat("Saved:", normalized_path, "\n")
if (nrow(normalized)) {
  print(
    normalized |>
      dplyr::group_by(.data$market_key) |>
      dplyr::summarise(
        rows = dplyr::n(),
        games = dplyr::n_distinct(.data$game_id),
        players = dplyr::n_distinct(.data$player),
        books = dplyr::n_distinct(.data$bookmaker_key),
        .groups = "drop"
      )
  )
}
