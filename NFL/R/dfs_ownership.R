# Uses first_matching_column() from R/dfs_salaries.R; scripts must source that
# file first (every script that needs this one already does, for the salary
# pool it also depends on).
if (!exists("first_matching_column")) source("R/dfs_salaries.R")

# Ownership: heuristic projection now, real capture pipeline for later.
#
# No historical DraftKings contest ownership exists anywhere in this project's
# data - the same gap already flagged for contest fields and payouts. DK's
# public draftgroups API (used for salary recovery, scripts/48/50) lists
# slates and prices; it does not expose who rostered whom in a contest. That
# information only exists in the per-contest "download results" export DK
# gives an *entrant* after a contest locks, which means it cannot be backfilled
# for years of history the way salaries were - it can only be captured going
# forward, one contest the user actually enters at a time.
#
# So this file has two halves that must not be confused with each other:
#
#   dfs_ownership_heuristic()   a transparent, unvalidated proxy computed from
#                                signals every DFS site uses when it has no
#                                ground truth: value, salary tier, game
#                                environment. It is a heuristic, not a
#                                statistical model, and must be labelled that
#                                way wherever it is surfaced.
#
#   parse_dk_contest_standings_file() / ingest_dk_contest_standings_directory()
#                                the capture side. Drop a DK contest-standings
#                                export into the manual-upload directory after
#                                a contest locks and this reconstructs real
#                                ownership percentages from it. Once enough
#                                slates accumulate this real data should
#                                replace the heuristic - that is future work,
#                                this just starts the collection.

# ---------------------------------------------------------------------------
# Heuristic ownership
# ---------------------------------------------------------------------------

# `pool` needs season, week, position, salary, projected_ppr, and one of
# implied_team_total / team_spread for the game-environment signal.
#
# Three signals, each rank-percentiled within its own slate and position so the
# heuristic is scale-free and comparable across weeks:
#
#   value       projected_ppr per $1,000 of salary. The single best-documented
#               driver of public ownership in DFS - obvious value chalks up.
#   game env    implied team total. Public money follows shootouts and
#               favourites; a player in a low-total, low-implied-total game
#               gets faded even at a good salary.
#   salary tier a mild U-shape: rock-bottom "punt" salaries at a thin position
#               draw ownership from lineup-construction necessity (someone has
#               to fill the cheap slot), and the single highest-salary "obvious
#               stud" at a position draws ownership from being the safe,
#               unimaginative play. Middling salaries get neither push.
#
# The blend (55/30/15) is not fitted to anything - there is no ownership data
# to fit it to. It encodes value as the dominant, most defensible signal and
# treats the other two as secondary nudges. This weighting is a starting
# point to be replaced once dfs_actual_ownership.rds has enough rows to fit
# something real; it is not itself a claim of accuracy.
dfs_ownership_heuristic <- function(pool) {
  required <- c("season", "week", "position", "salary", "projected_ppr")
  missing <- setdiff(required, names(pool))
  if (length(missing)) {
    stop("dfs_ownership_heuristic: missing columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  has_team_total <- "implied_team_total" %in% names(pool) &&
    any(!is.na(pool$implied_team_total))

  pool <- dplyr::mutate(
    pool,
    .value = .data$projected_ppr / pmax(.data$salary / 1000, 1),
    .team_total = if (has_team_total) .data$implied_team_total else NA_real_
  )

  pool <- pool |>
    dplyr::group_by(.data$season, .data$week, .data$position) |>
    dplyr::mutate(
      value_pctile = dplyr::percent_rank(.data$.value),
      # Missing team totals fall back to the group's own median before
      # ranking, so one unfilled row doesn't null out its whole percentile;
      # a position group with no team-total data at all falls back to neutral
      # further down rather than computing percent_rank on an all-NA vector.
      .team_total_filled = dplyr::coalesce(
        .data$.team_total, stats::median(.data$.team_total, na.rm = TRUE)
      ),
      env_pctile = if (has_team_total && any(!is.na(.data$.team_total))) {
        dplyr::percent_rank(.data$.team_total_filled)
      } else {
        0.5
      },
      # U-shape: distance from the middle of the salary range, folded so both
      # the cheapest and the priciest player score high.
      salary_pctile = dplyr::percent_rank(.data$salary),
      salary_extremity_pctile = abs(.data$salary_pctile - 0.5) * 2
    ) |>
    dplyr::ungroup()

  pool |>
    dplyr::mutate(
      ownership_heuristic_score = round(
        100 * (0.55 * .data$value_pctile + 0.30 * .data$env_pctile +
                 0.15 * .data$salary_extremity_pctile),
        1
      ),
      ownership_tier = dplyr::case_when(
        .data$ownership_heuristic_score >= 70 ~ "chalk",
        .data$ownership_heuristic_score >= 45 ~ "medium",
        .data$ownership_heuristic_score >= 20 ~ "low",
        TRUE ~ "contrarian"
      ),
      ownership_source = "heuristic_v1"
    ) |>
    dplyr::select(
      -".value", -".team_total", -".team_total_filled", -"value_pctile",
      -"env_pctile", -"salary_pctile", -"salary_extremity_pctile"
    )
}

# ---------------------------------------------------------------------------
# Real ownership capture, from DK's own post-lock contest-standings export
# ---------------------------------------------------------------------------

dk_ownership_schema <- function() {
  c("season", "week", "contest_id", "contest_name", "site", "entries",
    "player_name", "roster_count", "roster_pct", "source_file",
    "captured_at_utc")
}

empty_dk_ownership <- function() {
  tibble::tibble(
    season = integer(), week = integer(), contest_id = character(),
    contest_name = character(), site = character(), entries = integer(),
    player_name = character(), roster_count = integer(),
    roster_pct = double(), source_file = character(),
    captured_at_utc = character()
  )
}

# Player-name extraction from a lineup cell. DK sometimes appends a
# parenthetical id ("Josh Allen (12345678)") and sometimes doesn't; this
# strips it either way. A cell can hold one player (per-slot column layout) or
# several separated by a common delimiter (single combined "Lineup" column,
# usually space-separated after each roster-slot label) - callers split first.
dk_strip_player_id <- function(x) {
  x <- stringr::str_remove(x, "\\s*\\(\\d+\\)\\s*$")
  trimws(x)
}

dk_roster_slot_labels <- function() {
  c("QB", "RB", "WR", "TE", "FLEX", "DST", "DEF", "CPT", "UTIL", "FLEX1",
    "FLEX2")
}

# Parses one DK "contest standings" / "results" export. This format has not
# been exercised against a real downloaded file - DK does not expose a sample
# without an actual entered contest - so it is written defensively against the
# two shapes DK is known to use (a single combined Lineup column, or one
# column per roster slot) and fails loudly with a description of what it found
# rather than guessing silently. The first real file run through this should
# be spot-checked by hand: does roster_pct for a few well-known chalk plays
# look like what the contest lobby showed.
parse_dk_contest_standings_file <- function(path, season = NA_integer_,
                                            week = NA_integer_) {
  raw <- suppressMessages(readr::read_csv(
    path, show_col_types = FALSE, name_repair = "minimal"
  ))
  names_lower <- tolower(trimws(names(raw)))

  entries <- nrow(raw)
  if (!entries) {
    stop("parse_dk_contest_standings_file: ", basename(path), " has no rows.",
         call. = FALSE)
  }

  lineup_col <- first_matching_column(
    names_lower, c("lineup", "roster", "line up")
  )
  slot_cols <- which(toupper(names_lower) %in% dk_roster_slot_labels())

  player_lists <- if (!is.na(lineup_col)) {
    # A single free-text cell. DK typically prefixes each player with the
    # roster-slot label (e.g. "QB Josh Allen RB Saquon Barkley ..."); strip
    # the known slot labels as delimiters and split on the remainder.
    cells <- as.character(raw[[lineup_col]])
    pattern <- paste0("\\b(", paste(dk_roster_slot_labels(), collapse = "|"),
                      ")\\b")
    lapply(cells, function(cell) {
      pieces <- strsplit(gsub(pattern, "|", cell), "\\|")[[1]]
      pieces <- trimws(pieces)
      dk_strip_player_id(pieces[nzchar(pieces)])
    })
  } else if (length(slot_cols)) {
    lapply(seq_len(nrow(raw)), function(i) {
      values <- as.character(unlist(raw[i, slot_cols]))
      dk_strip_player_id(values[!is.na(values) & nzchar(trimws(values))])
    })
  } else {
    stop(
      "parse_dk_contest_standings_file: ", basename(path), " has neither a ",
      "'Lineup' column nor per-slot roster columns (looked for: ",
      paste(dk_roster_slot_labels(), collapse = ", "), "). Columns found: ",
      paste(names(raw), collapse = ", "), call. = FALSE
    )
  }

  players <- unlist(player_lists, use.names = FALSE)
  if (!length(players)) {
    stop("parse_dk_contest_standings_file: parsed zero player names from ",
         basename(path), ". The lineup format may differ from what this ",
         "parser expects - inspect the file and adjust.", call. = FALSE)
  }

  counts <- table(players)

  contest_meta <- stringr::str_match(
    basename(path),
    stringr::regex("(20\\d{2}).*?(?:week|wk)[-_ ]?(\\d{1,2})", ignore_case = TRUE)
  )
  if (is.na(season) && !is.na(contest_meta[1, 2])) {
    season <- as.integer(contest_meta[1, 2])
  }
  if (is.na(week) && !is.na(contest_meta[1, 3])) {
    week <- as.integer(contest_meta[1, 3])
  }

  tibble::tibble(
    season = as.integer(season), week = as.integer(week),
    contest_id = tools::file_path_sans_ext(basename(path)),
    contest_name = tools::file_path_sans_ext(basename(path)),
    site = "DK", entries = as.integer(entries),
    player_name = names(counts),
    roster_count = as.integer(counts),
    roster_pct = round(100 * as.integer(counts) / entries, 2),
    source_file = basename(path),
    captured_at_utc = format(Sys.time(), tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")
  ) |>
    dplyr::arrange(dplyr::desc(.data$roster_pct))
}

# Mirrors ingest_manual_dfs_salary_directory(): drop exports in, get one table
# out. Existing (season, week, contest_id) combinations are not re-parsed
# unless the source file is newer, so this is safe to re-run every week as new
# exports arrive.
ingest_dk_contest_standings_directory <- function(
    directory = "data/raw/dfs_contest_standings",
    existing_path = "data/processed/dfs_actual_ownership.rds") {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  paths <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  if (!length(paths)) return(empty_dk_ownership())

  existing <- if (file.exists(existing_path)) readRDS(existing_path) else
    empty_dk_ownership()
  done <- if (nrow(existing)) unique(existing$source_file) else character()

  new_paths <- setdiff(basename(paths), done)
  if (!length(new_paths)) {
    message("No new contest-standings files to ingest.")
    return(existing)
  }

  parsed <- purrr::map_dfr(file.path(directory, new_paths), function(path) {
    tryCatch(
      parse_dk_contest_standings_file(path),
      error = function(error) {
        warning(conditionMessage(error), call. = FALSE)
        empty_dk_ownership()
      }
    )
  })

  combined <- dplyr::bind_rows(existing, parsed)
  saveRDS(combined, existing_path)
  message(sprintf(
    "Ingested %d new file(s), %d ownership rows added. Total: %d rows across %d contests.",
    length(new_paths), nrow(parsed), nrow(combined),
    dplyr::n_distinct(combined$contest_id)
  ))
  combined
}
