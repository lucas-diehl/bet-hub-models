source("R/utilities.R")
source("R/odds_api.R")
source("R/dashboard_feed.R")
assert_packages()
ensure_directories()

# Closes the loop on published picks: captures a near-kickoff line snapshot for
# closing-line value, then grades finished games and writes results files.
#
# Two modes, because they run at different times:
#
#   --capture-close   run Sunday morning (and before any standalone kickoff).
#                     Caches the current line for every published bet so CLV can
#                     be measured later. Costs 2 API credits.
#   --grade           run Monday/Tuesday. Grades anything with a final score and
#                     writes results_<date>.json per slate.
#
# Grading is append-only in the same sense as publishing: a bet already graded
# keeps its recorded result. Only newly finished games are added.

args <- commandArgs(trailingOnly = TRUE)
capture_close <- "--capture-close" %in% args
do_grade <- "--grade" %in% args || !capture_close
execute <- "--execute" %in% args

`%|%` <- function(x, y) if (is.na(x) || is.null(x) || !nzchar(x)) y else x

ledger_path <- "data/processed/published_picks_ledger.csv"
close_path <- "data/processed/closing_line_capture.csv"
graded_path <- "data/processed/graded_picks.csv"

if (!file.exists(ledger_path)) {
  cat("No published picks yet.\n")
  quit(save = "no", status = 0)
}
ledger <- readr::read_csv(ledger_path, show_col_types = FALSE)
cat("Published bets in ledger:", nrow(ledger), "\n")

schedules <- readRDS("data/raw/schedules_2026.rds") |>
  dplyr::filter(.data$game_type == "REG") |>
  dplyr::transmute(
    .data$game_id, week = as.integer(.data$week),
    gameday = as.Date(.data$gameday),
    home_team = normalize_team(.data$home_team),
    away_team = normalize_team(.data$away_team),
    home_score = suppressWarnings(as.numeric(.data$home_score)),
    away_score = suppressWarnings(as.numeric(.data$away_score))
  )

# The ledger keys bets by slate date and team slugs rather than game_id, so the
# link back to the schedule is rebuilt here.
ledger <- ledger |>
  dplyr::mutate(
    slug_pair = sub("^nfl-modeling-\\d{4}-\\d{2}-\\d{2}-", "", .data$bet_id),
    slug_pair = sub("-(total|spread)$", "", .data$slug_pair)
  )
schedule_keyed <- schedules |>
  dplyr::mutate(
    slug_pair = paste0(feed_slug(.data$away_team), "-", feed_slug(.data$home_team))
  )
ledger <- ledger |>
  dplyr::left_join(
    schedule_keyed |> dplyr::select("slug_pair", "game_id", "home_score",
                                    "away_score", "gameday"),
    by = "slug_pair"
  )

# --------------------------------------------------------------------------
# Closing-line capture
# --------------------------------------------------------------------------

close_window_hours <- as.numeric(
  sub("^--close-window=", "",
      grep("^--close-window=", args, value = TRUE)[1]) %|% "36"
)

if (capture_close) {
  # Only games kicking off inside the window are captured. "First capture wins"
  # is what keeps a later, worse snapshot from overwriting a good one - but
  # without this filter the first capture could be taken weeks out, which is
  # not a closing line in any useful sense and would make CLV meaningless.
  now <- Sys.time()
  pending <- ledger |>
    dplyr::filter(
      is.na(.data$home_score),
      !is.na(.data$event_start),
      difftime(
        lubridate::ymd_hms(.data$event_start, tz = "UTC"), now, units = "hours"
      ) <= close_window_hours,
      lubridate::ymd_hms(.data$event_start, tz = "UTC") > now
    )
  cat("Published bets kicking off within", close_window_hours, "hours:",
      nrow(pending), "\n")
  if (!nrow(pending)) {
    cat("Nothing close enough to kickoff to capture. Run again nearer game day.\n")
  } else {
    capture_slugs <- unique(pending$slug_pair)
    book_map <- feed_books()
    abbrev <- stats::setNames(
      names(odds_api_team_names()), unname(odds_api_team_names())
    )
    res <- odds_api_current_game_lines("us")
    cat("Credits used:", res$quota$last, " remaining:", res$quota$remaining, "\n")
    events <- res$data

    snap <- purrr::map_dfr(seq_len(nrow(events)), function(i) {
      home <- abbrev[[as.character(events$home_team[[i]])]] %||% NA_character_
      away <- abbrev[[as.character(events$away_team[[i]])]] %||% NA_character_
      if (is.na(home) || is.na(away)) return(tibble::tibble())
      books <- events$bookmakers[[i]]
      if (is.null(books) || !nrow(books)) return(tibble::tibble())
      purrr::map_dfr(seq_len(nrow(books)), function(b) {
        if (!books$key[[b]] %in% names(book_map)) return(tibble::tibble())
        markets <- books$markets[[b]]
        if (is.null(markets) || !nrow(markets)) return(tibble::tibble())
        purrr::map_dfr(seq_len(nrow(markets)), function(m) {
          out <- markets$outcomes[[m]]
          if (is.null(out) || !nrow(out)) return(tibble::tibble())
          tibble::tibble(
            slug_pair = paste0(feed_slug(normalize_team(away)), "-",
                               feed_slug(normalize_team(home))),
            market = dplyr::if_else(markets$key[[m]] == "totals",
                                    "total", "spread"),
            outcome = as.character(out$name),
            point = suppressWarnings(as.numeric(out$point)),
            price = suppressWarnings(as.numeric(out$price)),
            home_full = as.character(events$home_team[[i]])
          )
        })
      })
    }) |>
      dplyr::filter(!is.na(.data$price), .data$slug_pair %in% capture_slugs) |>
      dplyr::mutate(
        side = dplyr::case_when(
          .data$market == "total" ~ tolower(.data$outcome),
          .data$outcome == .data$home_full ~ "home",
          TRUE ~ "away"
        )
      ) |>
      dplyr::group_by(.data$slug_pair, .data$market, .data$side) |>
      dplyr::summarise(
        close_price = max(.data$price),
        close_line = stats::median(.data$point),
        captured_at = format(Sys.time(), tz = "UTC", "%Y-%m-%dT%H:%M:%SZ"),
        .groups = "drop"
      )

    existing <- if (file.exists(close_path)) {
      readr::read_csv(close_path, show_col_types = FALSE)
    } else tibble::tibble()

    # First capture wins: a later run must not overwrite a snapshot taken
    # closer to kickoff for a game that has since started.
    combined <- dplyr::bind_rows(existing, snap) |>
      dplyr::distinct(.data$slug_pair, .data$market, .data$side,
                      .keep_all = TRUE)
    if (execute) {
      readr::write_csv(combined, close_path)
      cat("Closing snapshot rows stored:", nrow(combined), "\n")
    } else {
      cat("Dry run. Would store", nrow(combined), "snapshot rows.\n")
    }
  }
}

if (!do_grade) quit(save = "no", status = 0)

# --------------------------------------------------------------------------
# Grading
# --------------------------------------------------------------------------

closing <- if (file.exists(close_path)) {
  readr::read_csv(close_path, show_col_types = FALSE)
} else {
  tibble::tibble(slug_pair = character(), market = character(),
                 side = character(), close_price = numeric(),
                 close_line = numeric())
}

finished <- ledger |>
  dplyr::filter(!is.na(.data$home_score), !is.na(.data$away_score)) |>
  dplyr::left_join(
    closing |> dplyr::select("slug_pair", "market", "side", "close_price"),
    by = c("slug_pair", "market", "side")
  ) |>
  dplyr::mutate(
    final_margin = .data$home_score - .data$away_score,
    total_points = .data$home_score + .data$away_score,
    result = dplyr::case_when(
      .data$market == "spread" & .data$final_margin + .data$line > 0 ~ "win",
      .data$market == "spread" & .data$final_margin + .data$line < 0 ~ "loss",
      .data$market == "spread" ~ "push",
      .data$total_points > .data$line ~
        dplyr::if_else(.data$side == "over", "win", "loss"),
      .data$total_points < .data$line ~
        dplyr::if_else(.data$side == "over", "loss", "win"),
      TRUE ~ "push"
    ),
    pnl_units = dplyr::case_when(
      .data$result == "win" ~ .data$stake_units *
        american_payout(.data$odds_american),
      .data$result == "loss" ~ -.data$stake_units,
      TRUE ~ 0
    )
  )

cat("Finished bets to grade:", nrow(finished), "\n")
if (!nrow(finished)) {
  cat("Nothing has finished yet.\n")
  quit(save = "no", status = 0)
}

print(as.data.frame(
  finished |> dplyr::select("slate_date", "event", "market", "selection",
                            "line", "result", "pnl_units")
), digits = 4)
cat("\nNet units:", round(sum(finished$pnl_units), 3), "\n")
cat("Record:", sum(finished$result == "win"), "-",
    sum(finished$result == "loss"), "-",
    sum(finished$result == "push"), "\n")

if (!execute) {
  cat("\nDry run. Add --execute to write results files.\n")
  quit(save = "no", status = 0)
}

for (slate in sort(unique(finished$slate_date))) {
  rows <- finished |> dplyr::filter(.data$slate_date == slate)
  results <- rows |>
    dplyr::mutate(
      closing_odds_american = .data$close_price,
      actual = lapply(seq_len(dplyr::n()), function(i) {
        if (rows$market[[i]] == "spread") {
          list(final_margin = rows$final_margin[[i]])
        } else {
          list(total_points = rows$total_points[[i]])
        }
      })
    )
  path <- write_results_file(results, slate)
  cat("Wrote", nrow(results), "grades to", path, "\n")
}

readr::write_csv(finished, graded_path)
