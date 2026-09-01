source("R/utilities.R")
source("R/odds_api.R")
assert_packages()
ensure_directories()

# Buys repeated spread/total snapshots so each game has an early line and a late
# line, which is what closing-line value needs.
#
# The bulk historical /odds endpoint returns every not-yet-started game in one
# response for a flat 10 x markets x regions, so a whole slate costs 20 credits
# rather than 20 per game. Four snapshots a week covers the opening number plus
# a near-close for Thursday, the Sunday afternoon slate, and Sunday night.
# Monday night games inherit the Sunday-night snapshot, so their lead time is
# longer; the extracted table records lead time per game so that is visible
# rather than assumed.

args <- commandArgs(trailingOnly = TRUE)
execute <- "--execute" %in% args
arg_value <- function(flag, default) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", flag, "="), "", hit[[1]])
}
season <- as.integer(arg_value("--season", "2025"))
quota_floor <- as.integer(arg_value("--quota-floor", "1800"))

raw_dir <- file.path("data/raw/odds_api_line_movement", season)
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

# The historical archive stops at 2025; the current season lives in its own file
# (schedules_<season>.rds). Read both so a backfill and an in-season capture use
# the same driver (mirrors scripts/42_build_dashboard_feed.R).
schedule_files <- c("data/raw/schedules.rds", sprintf("data/raw/schedules_%d.rds", season))
schedules <- purrr::map_dfr(schedule_files[file.exists(schedule_files)], readRDS) |>
  dplyr::filter(.data$season == !!season, .data$game_type == "REG") |>
  dplyr::distinct(.data$game_id, .keep_all = TRUE) |>
  dplyr::mutate(kickoff_utc = nfl_kickoff_utc(.data$gameday, .data$gametime))

# Snapshot times are anchored to the Tuesday before each week's first kickoff.
weeks <- schedules |>
  dplyr::group_by(.data$week) |>
  dplyr::summarise(
    first_kick = min(lubridate::ymd_hms(.data$kickoff_utc, tz = "UTC")),
    .groups = "drop"
  )

snapshot_times <- purrr::pmap(
  list(weeks$week, weeks$first_kick),
  function(wk, first_kick) {
    monday <- lubridate::floor_date(first_kick, "week", week_start = 2)
    tibble::tibble(
      week = wk,
      label = c("open_tue", "close_thu", "close_sun_early", "close_sun_late"),
      snapshot = c(
        monday + lubridate::hours(15),
        monday + lubridate::days(2) + lubridate::hours(23) +
          lubridate::minutes(45),
        monday + lubridate::days(5) + lubridate::hours(16) +
          lubridate::minutes(45),
        monday + lubridate::days(5) + lubridate::hours(23) +
          lubridate::minutes(45)
      )
    )
  }
) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    snapshot_utc = format(.data$snapshot, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    path = file.path(
      raw_dir, paste0(sprintf("%02d", .data$week), "_", .data$label, ".json")
    ),
    cached = file.exists(.data$path)
  )

pending <- dplyr::filter(snapshot_times, !.data$cached)
cat("Season:", season, "\n")
cat("Games:", nrow(schedules), "\n")
cat("Snapshots planned:", nrow(snapshot_times), "\n")
cat("Already cached:", sum(snapshot_times$cached), "\n")
cat("Pending snapshots:", nrow(pending), "\n")
cat("Estimated credits:", nrow(pending) * 20L, "\n")

if (!execute) {
  cat("\nDry run only. Add --execute to spend credits.\n")
  quit(save = "no", status = 0)
}

last_remaining <- Inf
for (i in seq_len(nrow(pending))) {
  row <- pending[i, ]
  if (is.finite(last_remaining) && last_remaining - 20L < quota_floor) {
    message("Stopping at quota floor of ", quota_floor, ".")
    break
  }
  result <- tryCatch(
    odds_api_request(
      "/historical/sports/americanfootball_nfl/odds",
      list(
        date = row$snapshot_utc, regions = "us", markets = "spreads,totals",
        oddsFormat = "american", dateFormat = "iso"
      )
    ),
    error = function(e) {
      message("Failed ", row$path, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(result)) break
  jsonlite::write_json(
    result$data, row$path, auto_unbox = TRUE, pretty = FALSE, na = "null"
  )
  last_remaining <- result$quota$remaining
  message("[", i, "/", nrow(pending), "] week ", row$week, " ", row$label,
          "; remaining: ", last_remaining)
}

cat("\nQuota remaining:", last_remaining, "\n")
cat("Cached snapshots:", sum(file.exists(snapshot_times$path)), "\n")
