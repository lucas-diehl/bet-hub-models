source("R/utilities.R")
source("R/odds_api.R")
assert_packages()
ensure_directories()

# P0: what is the RotoWire archive actually a snapshot of?
#
# The whole backtest treats it as a closing line. If it is an opener or a
# midweek number then +9.18% measures "model beats an early line", which is an
# upper bound rather than a deployable figure. Two signals already point that
# way: early-line returns beat closing-line returns on spreads (+19.3% vs
# +1.8%), and model MAE is worse than market MAE, so the whole edge sits in the
# disagreement tail where timing bites hardest.
#
# The 72 purchased 2025 snapshots give a direct answer. For every game, compare
# the archived number against the market number at each snapshot and find which
# lead time it most resembles.

season <- 2025
snapshot_dir <- file.path("data/raw/odds_api_line_movement", season)
files <- list.files(snapshot_dir, pattern = "\\.json$", full.names = TRUE)
if (!length(files)) stop("No snapshots. Run scripts/35 first.", call. = FALSE)

abbrev <- stats::setNames(
  names(odds_api_team_names()), unname(odds_api_team_names())
)

read_snapshot <- function(path) {
  payload <- jsonlite::read_json(path, simplifyVector = FALSE)
  stamp <- as.character(payload$timestamp)
  week <- as.integer(sub("^(\\d+)_.*$", "\\1", basename(path)))
  purrr::map_dfr(payload$data %||% list(), function(event) {
    home <- abbrev[[as.character(event$home_team)]] %||% NA_character_
    away <- abbrev[[as.character(event$away_team)]] %||% NA_character_
    if (is.na(home) || is.na(away)) return(tibble::tibble())
    spreads <- c(); totals <- c()
    for (book in event$bookmakers %||% list()) {
      for (market in book$markets %||% list()) {
        for (o in market$outcomes %||% list()) {
          if (identical(market$key, "spreads") &&
                identical(as.character(o$name), as.character(event$home_team))) {
            spreads <- c(spreads, suppressWarnings(as.numeric(o$point)))
          }
          if (identical(market$key, "totals") &&
                tolower(as.character(o$name)) == "over") {
            totals <- c(totals, suppressWarnings(as.numeric(o$point)))
          }
        }
      }
    }
    tibble::tibble(
      snapshot_week = week,
      snapshot_utc = stamp,
      home_team = normalize_team(home), away_team = normalize_team(away),
      snap_home_line = if (length(spreads)) stats::median(spreads, na.rm = TRUE) else NA_real_,
      snap_total = if (length(totals)) stats::median(totals, na.rm = TRUE) else NA_real_
    )
  })
}

snapshots <- purrr::map_dfr(files, read_snapshot)

schedules <- readRDS("data/raw/schedules.rds") |>
  dplyr::filter(.data$season == !!season, .data$game_type == "REG") |>
  dplyr::transmute(
    .data$game_id, .data$week,
    home_team = normalize_team(.data$home_team),
    away_team = normalize_team(.data$away_team),
    kickoff = lubridate::ymd_hms(
      nfl_kickoff_utc(.data$gameday, .data$gametime), tz = "UTC"
    )
  )

# Same-week snapshots only, and strictly before kickoff. A Tuesday pull returns
# every posted game, so without the week filter a game's earliest observation
# can be two months old, and post-kickoff rows carry live in-play prices.
matched <- snapshots |>
  dplyr::inner_join(schedules, by = c("home_team", "away_team")) |>
  dplyr::filter(.data$snapshot_week == .data$week) |>
  dplyr::mutate(
    stamp = lubridate::ymd_hms(.data$snapshot_utc, tz = "UTC"),
    lead_hours = as.numeric(difftime(.data$kickoff, .data$stamp, units = "hours"))
  ) |>
  dplyr::filter(.data$lead_hours > 0)

archive <- readRDS("data/processed/game_features.rds") |>
  dplyr::filter(.data$season == !!season) |>
  dplyr::select("game_id", arc_home_line = "home_line", arc_total = "total_line")

cat("Snapshot observations:", nrow(matched), "\n")
cat("Games in archive:", nrow(archive), "\n")

joined <- matched |>
  dplyr::inner_join(archive, by = "game_id") |>
  dplyr::mutate(
    diff_total = abs(.data$snap_total - .data$arc_total),
    diff_spread = abs(.data$snap_home_line - .data$arc_home_line),
    bucket = cut(
      .data$lead_hours,
      breaks = c(0, 2, 4, 8, 16, 30, 54, 78, 102, 126, 200),
      labels = c("0-2h", "2-4h", "4-8h", "8-16h", "16-30h", "30-54h",
                 "54-78h", "78-102h", "102-126h", "126h+")
    )
  )

cat("Joined observations:", nrow(joined), "\n\n")

summary_table <- joined |>
  dplyr::group_by(.data$bucket) |>
  dplyr::summarise(
    obs = dplyr::n(),
    games = dplyr::n_distinct(.data$game_id),
    median_hours = stats::median(.data$lead_hours),
    median_diff_total = stats::median(.data$diff_total, na.rm = TRUE),
    exact_total = mean(.data$diff_total == 0, na.rm = TRUE),
    median_diff_spread = stats::median(.data$diff_spread, na.rm = TRUE),
    exact_spread = mean(.data$diff_spread == 0, na.rm = TRUE),
    .groups = "drop"
  )

cat("=== Archive vs market, by lead time ===\n")
print(as.data.frame(summary_table), digits = 4)
readr::write_csv(summary_table, "outputs/line_provenance_buckets.csv")

# Per game, which snapshot is closest to the archived number.
closest <- joined |>
  dplyr::filter(!is.na(.data$diff_total)) |>
  dplyr::group_by(.data$game_id) |>
  dplyr::slice_min(.data$diff_total, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()

cat("\n=== Per game, lead time of the closest-matching snapshot (totals) ===\n")
print(as.data.frame(
  tibble::tibble(
    games = nrow(closest),
    median_lead_hours = stats::median(closest$lead_hours),
    q25 = stats::quantile(closest$lead_hours, 0.25),
    q75 = stats::quantile(closest$lead_hours, 0.75),
    share_within_6h = mean(closest$lead_hours <= 6),
    share_beyond_48h = mean(closest$lead_hours >= 48)
  )
), digits = 4)

closest_spread <- joined |>
  dplyr::filter(!is.na(.data$diff_spread)) |>
  dplyr::group_by(.data$game_id) |>
  dplyr::slice_min(.data$diff_spread, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()

cat("\n=== Per game, lead time of the closest-matching snapshot (spreads) ===\n")
print(as.data.frame(
  tibble::tibble(
    games = nrow(closest_spread),
    median_lead_hours = stats::median(closest_spread$lead_hours),
    q25 = stats::quantile(closest_spread$lead_hours, 0.25),
    q75 = stats::quantile(closest_spread$lead_hours, 0.75),
    share_within_6h = mean(closest_spread$lead_hours <= 6),
    share_beyond_48h = mean(closest_spread$lead_hours >= 48)
  )
), digits = 4)

# Direct comparison against the two anchors used elsewhere in the project.
anchors <- joined |>
  dplyr::group_by(.data$game_id) |>
  dplyr::summarise(
    earliest = .data$lead_hours[which.max(.data$lead_hours)],
    latest = .data$lead_hours[which.min(.data$lead_hours)],
    diff_total_earliest = .data$diff_total[which.max(.data$lead_hours)],
    diff_total_latest = .data$diff_total[which.min(.data$lead_hours)],
    diff_spread_earliest = .data$diff_spread[which.max(.data$lead_hours)],
    diff_spread_latest = .data$diff_spread[which.min(.data$lead_hours)],
    .groups = "drop"
  )

cat("\n=== Archive against the week's earliest vs latest snapshot ===\n")
print(as.data.frame(tibble::tibble(
  anchor = c("earliest (Tue)", "latest (~kickoff)"),
  median_lead_hours = c(stats::median(anchors$earliest),
                        stats::median(anchors$latest)),
  median_diff_total = c(stats::median(anchors$diff_total_earliest, na.rm = TRUE),
                        stats::median(anchors$diff_total_latest, na.rm = TRUE)),
  median_diff_spread = c(stats::median(anchors$diff_spread_earliest, na.rm = TRUE),
                         stats::median(anchors$diff_spread_latest, na.rm = TRUE)),
  exact_total = c(mean(anchors$diff_total_earliest == 0, na.rm = TRUE),
                  mean(anchors$diff_total_latest == 0, na.rm = TRUE)),
  exact_spread = c(mean(anchors$diff_spread_earliest == 0, na.rm = TRUE),
                   mean(anchors$diff_spread_latest == 0, na.rm = TRUE))
)), digits = 4)
