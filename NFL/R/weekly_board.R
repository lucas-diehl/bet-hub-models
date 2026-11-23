## Weekly fantasy board persistence (added 2026-11-23).
##
## scripts/17 used to (a) pick the week as "first REG game with gameday >=
## Sys.Date()" and (b) project only the games present in the Odds API /events
## snapshot - which drops every game once it has been played. Each run then
## OVERWROTE outputs/fantasy_prop_2026_week<N>_projections.csv with whatever
## subset was left. Two confirmed casualties:
##
##   week 10, Mon 2026-11-16  the MNF-only run replaced the 323-row Sunday board
##                            with 24 LAC/BAL rows (commit a2fee71).
##   week 11, Fri 2026-11-20  every run after TNF kicked off dropped IND/HOU
##                            (324 -> 299 rows, commit 6ead85c onward); the Sun
##                            board, the DFS JSON and the TD fantasy signal all
##                            lost the Thursday game.
##
## Same root cause: the week file was treated as "games still on the board",
## not "the week". The helpers below make it the week:
##   * the game set comes from the SCHEDULE; the event snapshot only supplies
##     lines. A not-yet-started game missing from the snapshot is still
##     projected (on sign-converted nflverse lines) instead of silently skipped.
##   * games that have kicked off are never re-projected (their rows freeze);
##     games not yet started are re-projected every run.
##   * the fresh rows are MERGED into the existing week file - a game's rows are
##     replaced only when this run produced rows for it; every other game of the
##     week is carried forward untouched.

## Every REG game of `season`, with a UTC kickoff and a `started` flag. A game
## counts as started once it has a score, or its kickoff has passed, or (no
## usable kickoff time) its ET game day is before today.
fantasy_week_schedule <- function(schedules, season = 2026L,
                                  now_utc = lubridate::now(tzone = "UTC")) {
  games <- schedules |>
    dplyr::filter(.data$season == !!season, .data$game_type == "REG")
  kickoff <- lubridate::ymd_hms(
    nfl_kickoff_utc(games$gameday, games$gametime), tz = "UTC", quiet = TRUE
  )
  today_et <- as.Date(lubridate::with_tz(now_utc, "America/New_York"))
  scored <- if ("home_score" %in% names(games)) {
    is.finite(suppressWarnings(as.numeric(games$home_score)))
  } else {
    rep(FALSE, nrow(games))
  }
  tibble::tibble(
    game_id = as.character(games$game_id),
    week = as.integer(games$week),
    gameday = as.Date(games$gameday),
    kickoff_utc = kickoff,
    started = scored |
      (!is.na(kickoff) & kickoff <= now_utc) |
      (is.na(kickoff) & as.Date(games$gameday) < today_et)
  ) |>
    dplyr::arrange(.data$kickoff_utc, .data$gameday)
}

## The week to project: the week of the earliest game that has not started.
## (Replaces `gameday >= Sys.Date()`, which used the runner's UTC date and the
## day rather than the kickoff.)
fantasy_target_week <- function(week_schedule) {
  upcoming <- week_schedule[!week_schedule$started, ]
  if (!nrow(upcoming)) stop("No unplayed REG games on the schedule.", call. = FALSE)
  upcoming$week[[1]]
}

## Restrict the matched Odds API events to `game_ids`, and add an
## event_id = NA row for any of them the snapshot does not list, so
## build_td_2026_game_context() still builds them (lines then fall back to the
## sign-converted nflverse schedule line).
fantasy_projection_event_map <- function(event_map, game_ids) {
  kept <- event_map |>
    dplyr::filter(.data$game_id %in% game_ids) |>
    dplyr::distinct(.data$game_id, .keep_all = TRUE)
  missing <- setdiff(game_ids, kept$game_id)
  if (length(missing)) {
    message(sprintf(
      "::warning::%d unplayed game(s) are not in the Odds API event snapshot; projecting them on nflverse schedule lines: %s",
      length(missing), paste(missing, collapse = ", ")
    ))
    kept <- dplyr::bind_rows(
      kept,
      tibble::tibble(event_id = NA_character_, game_id = missing)
    )
  }
  kept
}

## Coerce `value` to the type of `like`.
coerce_like <- function(value, like) {
  if (inherits(like, "Date")) {
    as.Date(value)
  } else if (is.integer(like)) {
    suppressWarnings(as.integer(value))
  } else if (is.numeric(like)) {
    suppressWarnings(as.numeric(value))
  } else if (is.logical(like)) {
    as.logical(value)
  } else {
    as.character(value)
  }
}

## Make shared columns of `existing` (read back from CSV) and `fresh` the same
## type so they bind. Fresh's type wins, EXCEPT where one side is an all-NA
## logical (readr's guess for an empty column, e.g. injury_status on a run with
## no injury report) - that side takes the other's type, so real values on the
## other side are never coerced to NA.
harmonize_board_types <- function(existing, fresh) {
  all_na_lgl <- function(x) is.logical(x) && all(is.na(x))
  for (column in intersect(names(existing), names(fresh))) {
    a <- existing[[column]]
    b <- fresh[[column]]
    if (identical(class(a), class(b))) next
    if (all_na_lgl(b) && !all_na_lgl(a)) {
      fresh[[column]] <- coerce_like(b, a)
    } else {
      existing[[column]] <- coerce_like(a, b)
    }
  }
  list(existing = existing, fresh = fresh)
}

## Merge a fresh projection run into the existing same-week board.
##   fresh          rows produced by this run
##   existing       the current week file (NULL / 0 rows on the first run)
##   week_game_ids  every game_id of the target week (rows outside it are dropped)
##   drop_columns   derived columns to strip from carried rows (re-ranked later)
## A game is REPLACED only if `fresh` has rows for it; every other game of the
## week that `existing` has is carried forward.
merge_fantasy_week_board <- function(fresh, existing, week_game_ids,
                                     drop_columns = c("overall_rank", "position_rank")) {
  fresh_games <- unique(as.character(fresh$game_id))
  if (is.null(existing) || !nrow(existing)) {
    return(list(board = fresh, replaced = fresh_games, carried = character()))
  }
  existing$game_id <- as.character(existing$game_id)
  carried <- existing |>
    dplyr::filter(
      .data$game_id %in% week_game_ids,
      !.data$game_id %in% fresh_games
    ) |>
    dplyr::select(-dplyr::any_of(drop_columns))
  types <- harmonize_board_types(carried, fresh)
  board <- dplyr::bind_rows(types$fresh, types$existing)

  ## Guard: a merge can only ever ADD games. Losing one means the logic above
  ## regressed - stop before anything is written.
  lost <- setdiff(intersect(unique(existing$game_id), week_game_ids),
                  unique(board$game_id))
  if (length(lost)) {
    stop("merge_fantasy_week_board(): merge would drop game(s) already on the ",
         "week board: ", paste(lost, collapse = ", "), call. = FALSE)
  }
  list(
    board = board,
    replaced = intersect(fresh_games, unique(existing$game_id)),
    carried = unique(carried$game_id)
  )
}

## One-line coverage log for a week board + a loud warning for any scheduled
## game with no rows. Returns the missing game_ids invisibly.
report_week_board_coverage <- function(board, week_game_ids, label = "board") {
  counts <- table(factor(as.character(board$game_id), levels = week_game_ids))
  missing <- names(counts)[counts == 0]
  message(sprintf(
    "%s: %d of %d scheduled games present, %d rows (%s).",
    label, sum(counts > 0), length(week_game_ids), nrow(board),
    paste(sprintf("%s=%d", names(counts), as.integer(counts)), collapse = " ")
  ))
  thin <- names(counts)[counts > 0 & counts < 10]
  if (length(thin)) {
    message("::warning::", label, ": unusually few players (<10) for: ",
            paste(thin, collapse = ", "))
  }
  if (length(missing)) {
    message("::warning::", label, ": ", length(missing),
            " scheduled game(s) have NO projections: ",
            paste(missing, collapse = ", "))
  }
  invisible(missing)
}
