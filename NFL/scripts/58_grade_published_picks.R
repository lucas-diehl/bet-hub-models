source("R/utilities.R")
source("R/odds_api.R")
source("R/dashboard_feed.R")
source("R/touchdown_features.R")  # normalize_prop_player_name(), for prop grading
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

# Pinned so captured_at round-trips as character every time. Without this,
# readr::read_csv() auto-detects an ISO8601-looking column as <datetime<UTC>>
# on read-back, while the freshly built snapshot always builds it as plain
# character (via format()) - bind_rows() then throws a hard type-mismatch
# error the moment there's existing data to combine with, which silently
# killed every capture-close run past the first (confirmed in the cloud
# pipeline's run logs: "Can't combine `captured_at` <datetime<UTC>> and
# <character>" after a successful API fetch, so nothing was ever saved).
close_col_types <- readr::cols(
  slug_pair = readr::col_character(),
  market = readr::col_character(),
  side = readr::col_character(),
  close_price = readr::col_double(),
  close_line = readr::col_double(),
  captured_at = readr::col_character()
)

if (!file.exists(ledger_path)) {
  cat("No published picks yet.\n")
  quit(save = "no", status = 0)
}
ledger <- readr::read_csv(ledger_path, show_col_types = FALSE)
cat("Published bets in ledger:", nrow(ledger), "\n")

# Schema backfill: a ledger written before prop support existed (the cloud
# pipeline ran for weeks without scripts/88 or this file's prop-grading code)
# has no player/team/stat columns at all. Those bets are real spread/total
# rows and grade fine without them - only prop rows ever read these three -
# so backfilling NA here is correct, not a cover-up: there is no prop bet in
# an old-schema ledger for these columns to be wrong about.
for (col in c("player", "team", "stat")) {
  if (!col %in% names(ledger)) ledger[[col]] <- NA_character_
}

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
# link back to the schedule is rebuilt here. Prop bet_ids end in "-anytimetd"
# after a PLAYER slug, not a team-pair slug (e.g.
# nfl-modeling-2026-10-01-romanwilson-anytimetd) - stripping only
# "-(total|spread)$" left that suffix attached and would have produced a
# slug_pair that could never match a real team pairing, so props never game-
# score-joined at all. Stripped here too, and props are joined to the schedule
# by (team, week) below instead of by slug_pair, since a player's game is
# identified by his team, not a reconstructed matchup slug.
ledger <- ledger |>
  dplyr::mutate(
    slug_pair = sub("^nfl-modeling-\\d{4}-\\d{2}-\\d{2}-", "", .data$bet_id),
    slug_pair = sub("-(total|spread|anytimetd)$", "", .data$slug_pair)
  )
schedule_keyed <- schedules |>
  dplyr::mutate(
    slug_pair = paste0(feed_slug(.data$away_team), "-", feed_slug(.data$home_team))
  )

is_prop <- ledger$market == "prop"
ledger_game <- ledger[!is_prop, ] |>
  dplyr::left_join(
    schedule_keyed |> dplyr::select("slug_pair", "game_id", "home_score",
                                    "away_score", "gameday"),
    by = "slug_pair"
  )

# Props: match by TEAM + week instead of slug_pair, since the "slug_pair" value
# for a prop row is a player slug, not a matchup. normalize_team() first so a
# ledger team abbreviation that differs from the schedule's own (the project
# has hit this before - LA/LAR) still joins.
team_games <- dplyr::bind_rows(
  schedules |> dplyr::transmute(.data$game_id, .data$week, .data$gameday,
                                team = .data$home_team, .data$home_score,
                                .data$away_score),
  schedules |> dplyr::transmute(.data$game_id, .data$week, .data$gameday,
                                team = .data$away_team, .data$home_score,
                                .data$away_score)
)
ledger_prop <- ledger[is_prop, ] |>
  dplyr::mutate(team = normalize_team(.data$team)) |>
  dplyr::left_join(team_games, by = c("team", "week"))

ledger <- dplyr::bind_rows(ledger_game, ledger_prop)

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
      readr::read_csv(close_path, col_types = close_col_types)
    } else {
      tibble::tibble()
    }

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
  readr::read_csv(close_path, col_types = close_col_types)
} else {
  tibble::tibble(slug_pair = character(), market = character(),
                 side = character(), close_price = numeric(),
                 close_line = numeric())
}

## Anytime-TD prop outcomes. These never had any grading support at all - the
## case_when below fell through every prop bet to the catch-all "push" (0 PnL,
## no win or loss ever recorded), because the branches only test spread/total
## conditions and `line` is NA for a prop (it has no number, only a player).
## Joined here, against the SAME player_stats file the live board itself reads,
## by (season, week, normalized name, team) - the team is included because
## player-name collisions are not actually rare across 32 rosters.
td_outcomes <- NULL
if (any(ledger$market == "prop", na.rm = TRUE)) {
  stats_path <- if (file.exists("data/raw/player_stats_2021_2026.rds")) {
    "data/raw/player_stats_2021_2026.rds"
  } else {
    "data/raw/player_stats_2021_2025.rds"
  }
  td_outcomes <- readRDS(stats_path) |>
    dplyr::filter(.data$season == 2026, .data$season_type == "REG") |>
    dplyr::transmute(
      week = as.integer(.data$week),
      team = normalize_team(.data$team),
      .name_key = normalize_prop_player_name(.data$player_display_name),
      scored_td = (dplyr::coalesce(.data$rushing_tds, 0) +
                     dplyr::coalesce(.data$receiving_tds, 0)) > 0,
      rushing_tds = dplyr::coalesce(.data$rushing_tds, 0),
      receiving_tds = dplyr::coalesce(.data$receiving_tds, 0)
    ) |>
    dplyr::distinct(.data$week, .data$team, .data$.name_key, .keep_all = TRUE)
}

finished <- ledger |>
  dplyr::filter(!is.na(.data$home_score), !is.na(.data$away_score)) |>
  dplyr::left_join(
    closing |> dplyr::select("slug_pair", "market", "side", "close_price",
                             "close_line"),
    by = c("slug_pair", "market", "side")
  )

if (!is.null(td_outcomes)) {
  finished <- finished |>
    dplyr::mutate(.name_key = dplyr::if_else(
      .data$market == "prop", normalize_prop_player_name(.data$player), NA_character_
    )) |>
    dplyr::left_join(
      td_outcomes |> dplyr::select("week", "team", ".name_key", "scored_td",
                                   "rushing_tds", "receiving_tds"),
      by = c("week", "team", ".name_key")
    ) |>
    dplyr::select(-".name_key")
} else {
  finished$scored_td <- NA
  finished$rushing_tds <- NA_real_
  finished$receiving_tds <- NA_real_
}

n_prop_unmatched <- sum(finished$market == "prop" & is.na(finished$scored_td))
if (n_prop_unmatched > 0) {
  cat("WARNING:", n_prop_unmatched,
      "finished prop bet(s) could not be matched to a player_stats row",
      "(name mismatch, or the player did not appear in the box score at all",
      "- which for an anytime-TD bet correctly means he did not score, but is",
      "indistinguishable here from a bad join without checking by hand):\n")
  print(as.data.frame(
    finished |> dplyr::filter(.data$market == "prop", is.na(.data$scored_td)) |>
      dplyr::select("slate_date", "event", "player", "team")
  ))
}

finished <- finished |>
  dplyr::mutate(
    final_margin = .data$home_score - .data$away_score,
    total_points = .data$home_score + .data$away_score,
    result = dplyr::case_when(
      .data$market == "prop" ~
        dplyr::if_else(dplyr::coalesce(.data$scored_td, FALSE), "win", "loss"),
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

# --------------------------------------------------------------------------
# Closing-line value
#
# CLV is the primary KPI, ahead of realised ROI, and the reason is sample size.
# At roughly 65 qualifying bets a season it takes years for ROI to separate a
# real edge from variance - the backtest's own interval spans +1.0% to +17.2%
# on 525 bets. CLV resolves per bet instead of per outcome, so a season's worth
# of it says more about whether the model is finding value than a season of
# win-loss does.
#
# Two components, reported separately because they are not interchangeable:
#
#   line CLV (points)  did the number move toward us. This is the primary
#                      figure. A spread taken at -3.5 that closes -4.5 is worth
#                      +1.0 whatever the eventual result.
#   price CLV (prob)   did the juice move toward us, in points of implied
#                      probability. Secondary for spreads and totals, primary
#                      for anytime-TD props, which have no line.
# --------------------------------------------------------------------------

clv <- finished |>
  dplyr::mutate(
    clv_points = dplyr::case_when(
      is.na(.data$close_line) ~ NA_real_,
      # Spread bets are home-side only, and `line` is the home number, so a
      # more negative close means the number moved toward us.
      .data$market == "spread" ~ .data$line - .data$close_line,
      .data$side == "over" ~ .data$close_line - .data$line,
      .data$side == "under" ~ .data$line - .data$close_line,
      TRUE ~ NA_real_
    ),
    # Falling implied probability at the close means we took the better price.
    clv_prob = dplyr::if_else(
      is.na(.data$close_price), NA_real_,
      american_to_prob(.data$odds_american) - american_to_prob(.data$close_price)
    )
  )

with_clv <- dplyr::filter(clv, !is.na(.data$clv_points) | !is.na(.data$clv_prob))
cat("\n=== Closing-line value ===\n")
cat("Bets with a captured close:", nrow(with_clv), "of", nrow(clv), "\n")

if (nrow(with_clv)) {
  print(as.data.frame(
    with_clv |>
      dplyr::group_by(.data$market) |>
      dplyr::summarise(
        bets = dplyr::n(),
        mean_clv_points = mean(.data$clv_points, na.rm = TRUE),
        beat_close = mean(.data$clv_points > 0, na.rm = TRUE),
        mean_clv_prob = mean(.data$clv_prob, na.rm = TRUE),
        roi = sum(.data$pnl_units) / sum(.data$stake_units),
        .groups = "drop"
      )
  ), digits = 4, row.names = FALSE)

  # A real edge should get stronger as the model disagrees with the market
  # harder. A flat gradient across edge buckets is the signature of a
  # threshold that is selecting noise rather than value - it is the same test
  # that sank the yardage props, applied to live bets.
  cat("\n=== CLV and ROI by edge bucket ===\n")
  print(as.data.frame(
    with_clv |>
      dplyr::mutate(bucket = cut(
        abs(.data$edge), c(0, 6, 7, 8, Inf),
        labels = c("<6", "6-7", "7-8", "8+"), include.lowest = TRUE
      )) |>
      dplyr::group_by(.data$bucket) |>
      dplyr::summarise(
        bets = dplyr::n(),
        mean_clv_points = mean(.data$clv_points, na.rm = TRUE),
        beat_close = mean(.data$clv_points > 0, na.rm = TRUE),
        roi = sum(.data$pnl_units) / sum(.data$stake_units),
        .groups = "drop"
      )
  ), digits = 4, row.names = FALSE)

  # Kill criterion. Stated in advance so it cannot be renegotiated during a
  # losing run, which is exactly when it would be. Rolling over the most recent
  # 50 graded bets with a captured close:
  #
  #   mean line CLV >= 0      keep betting
  #   mean line CLV <  0      review before the next publish
  #   mean line CLV < -0.25   suspend the arm; publish to paper only
  #
  # Below -0.25 points the model is systematically on the wrong side of the
  # move, which is a stronger signal than any run of results at this sample
  # size, and -0.25 is roughly the point where the move eats a normal edge.
  window <- utils::tail(
    with_clv[order(with_clv$slate_date), ], 50
  )
  rolling <- mean(window$clv_points, na.rm = TRUE)
  cat(sprintf("\nRolling CLV over the last %d graded bets: %+.3f points\n",
              nrow(window), rolling))
  cat("Status: ", if (is.na(rolling)) "no closes captured yet"
      else if (rolling < -0.25) "SUSPEND - move this arm to paper"
      else if (rolling < 0) "REVIEW before the next publish"
      else "OK - continue", "\n", sep = "")

  # Expected drawdown, so a normal losing run is not mistaken for a broken
  # model. At a 57% win rate on -110, a 20-bet losing stretch is unremarkable.
  wins <- sum(with_clv$result == "win")
  decided <- sum(with_clv$result != "push")
  if (decided >= 20) {
    p <- wins / decided
    set.seed(1L)
    worst <- vapply(seq_len(2000L), function(i) {
      draws <- sample(c(1, -1), decided, replace = TRUE, prob = c(p, 1 - p))
      equity <- cumsum(dplyr::if_else(draws > 0, 10 / 11, -1))
      min(equity - cummax(equity))
    }, numeric(1))
    cat(sprintf(
      "Expected worst drawdown at this win rate over %d bets: %.1f units median, %.1f units at the 5th percentile.\n",
      decided, -stats::median(worst), -stats::quantile(worst, 0.05)
    ))
  }

  readr::write_csv(clv, "outputs/clv_record.csv")
}
finished <- clv

if (!execute) {
  cat("\nDry run. Add --execute to write results files.\n")
  quit(save = "no", status = 0)
}

## as.character() before the loop: `for (x in <Date vector>)` iterates the
## underlying numeric and silently drops the Date class, which is how
## results_20709.json got written instead of results_2026-09-14.json.
for (slate in sort(unique(as.character(finished$slate_date)))) {
  rows <- finished |> dplyr::filter(as.character(.data$slate_date) == slate)
  results <- rows |>
    dplyr::mutate(
      closing_odds_american = .data$close_price,
      ## Three markets now, not two - a prop falling into the old if/else
      ## "spread, else total" would have been labelled with a total_points
      ## value that has nothing to do with whether the player scored.
      actual = lapply(seq_len(dplyr::n()), function(i) {
        if (rows$market[[i]] == "spread") {
          list(final_margin = rows$final_margin[[i]])
        } else if (rows$market[[i]] == "prop") {
          list(
            scored_td = isTRUE(rows$scored_td[[i]]),
            rushing_tds = rows$rushing_tds[[i]],
            receiving_tds = rows$receiving_tds[[i]]
          )
        } else {
          list(total_points = rows$total_points[[i]])
        }
      })
    )
  path <- write_results_file(results, slate)
  cat("Wrote", nrow(results), "grades to", path, "\n")
}

readr::write_csv(finished, graded_path)
