source("R/utilities.R")
source("R/dfs_salaries.R")
assert_packages()
ensure_directories()

# Backfills DraftKings NFL salaries for 2022 onward from the draftgroups API.
#
# RotoGuru, the source behind the 2011-2021 archive, stops returning NFL tables
# in 2022 (verified: 2021 week 5 gives 439 rows, every season after gives zero).
# DraftKings still serves historical draft groups from
#   /draftgroups/v1/draftgroups/{id}/draftables
# so the salaries are recoverable, but there is no endpoint that lists groups by
# sport or date and no lightweight metadata route. Draft group ids are shared
# across every sport and are only loosely chronological, so the ids for a given
# NFL week have to be found by scanning a neighbourhood.
#
# Strategy: anchor on a known id/date pair, interpolate to the target week,
# scan a window, and keep the NFL group covering the most games - the main
# slate. Every probe is cached, so re-runs cost nothing and the job is
# resumable after an interruption.
#
#   --seasons=2022,2023   seasons to backfill (default 2022:2025)
#   --weeks=1,2           optional week filter
#   --window=80           ids to scan either side of the estimate
#   --execute             actually call the API (otherwise plan only)

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", flag, "="), "", hit[[1]])
}
execute <- "--execute" %in% args
seasons <- as.integer(strsplit(arg_value("--seasons", "2022,2023,2024,2025"),
                               ",", fixed = TRUE)[[1]])
weeks_arg <- arg_value("--weeks", "")
weeks <- if (nzchar(weeks_arg)) {
  as.integer(strsplit(weeks_arg, ",", fixed = TRUE)[[1]])
} else 1:18
window <- as.integer(arg_value("--window", "80"))

cache_dir <- "data/raw/dfs_salaries/dk_draftgroups"
# v3. Two things the earlier versions got wrong:
#   - salary presence separates the classic salary-cap slate from the Tiers /
#     Pick6 group that covers the same games with no salaries at all;
#   - kickoff dates must come from each draftable's own `competition`, not the
#     top-level `competitions` array, whose start times can be a day off (group
#     114257 reports 2024-10-05 for a slate that actually plays on 10-06).
summary_dir <- file.path(cache_dir, "summaries_v3")
payload_dir <- file.path(cache_dir, "payloads")
dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(payload_dir, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

# College football on DraftKings uses the same positions as the NFL (QB, RB,
# WR, TE, DST, K), so a position check cannot tell the two apart. It also runs
# far more games per week, which means ranking candidates by game count
# actively prefers the college slate. Team abbreviations are the discriminator
# and they separate cleanly: real NFL groups score 1.0 here, college groups 0.0.
nfl_team_codes <- function() {
  c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN",
    "DET", "GB", "HOU", "IND", "JAX", "KC", "LAC", "LAR", "LV", "MIA",
    "MIN", "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", "SF", "TB",
    "TEN", "WAS")
}

# Returns NA when the payload is absent, otherwise the three things that
# together identify a genuine classic NFL slate: the share of teams that are
# NFL, the top salary, and the share of draftables at positions classic DFS
# does not roster. Nine 2022 groups priced every player at exactly $2,500 and
# included kickers and defensive backs; they pass a team check but are not
# salary-cap slates.
payload_quality <- function(id, payload_dir) {
  path <- file.path(payload_dir, sprintf("%d.rds", id))
  blank <- list(nfl_share = NA_real_, max_salary = NA_real_,
                idp_share = NA_real_, n_teams = NA_integer_)
  if (!file.exists(path)) return(blank)
  body <- readRDS(path)
  draftables <- body$draftables %||% list()
  if (!length(draftables)) return(blank)
  teams <- unique(toupper(vapply(
    draftables, function(p) as.character(p$teamAbbreviation %||% ""),
    character(1)
  )))
  teams <- teams[nzchar(teams)]
  positions <- toupper(vapply(
    draftables, function(p) as.character(p$position %||% ""), character(1)
  ))
  salaries <- vapply(
    draftables, function(p) as.numeric(p$salary %||% NA_real_), numeric(1)
  )
  list(
    nfl_share = if (length(teams)) mean(teams %in% nfl_team_codes()) else NA_real_,
    max_salary = suppressWarnings(max(salaries, na.rm = TRUE)),
    idp_share = mean(positions %in% c("CB", "DE", "DT", "LB", "S", "K")),
    n_teams = length(teams)
  )
}

dk_get <- function(id) {
  url <- sprintf(
    "https://api.draftkings.com/draftgroups/v1/draftgroups/%d/draftables", id
  )
  response <- tryCatch(
    dfs_request(url) |>
      httr2::req_user_agent("nfl-dfs-salary-research/1.0 (historical backfill)") |>
      httr2::req_timeout(30) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(response) || httr2::resp_status(response) != 200) return(NULL)
  tryCatch(
    httr2::resp_body_json(response, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

# Light index row per id, cached. Full payloads are only kept for the groups
# that turn out to be main slates.
probe_id <- function(id) {
  path <- file.path(summary_dir, sprintf("%d.rds", id))
  if (file.exists(path)) return(readRDS(path))

  body <- dk_get(id)
  out <- tibble::tibble(
    id = id, ok = FALSE, is_nfl = FALSE, n_games = NA_integer_,
    n_players = NA_integer_, n_with_salary = NA_integer_,
    game_dates = NA_character_
  )
  if (!is.null(body)) {
    draftables <- body$draftables %||% list()
    if (length(draftables)) {
      positions <- unique(vapply(
        draftables,
        function(p) toupper(as.character(p$position %||% "")), character(1)
      ))
      competitions <- body$competitions %||% list()
      starts <- vapply(
        draftables,
        function(p) substr(
          as.character((p$competition %||% list())$startTime %||% ""), 1, 10
        ),
        character(1)
      )
      out <- tibble::tibble(
        id = id, ok = TRUE,
        is_nfl = any(c("QB", "RB", "WR", "TE", "DST") %in% positions),
        n_games = length(competitions),
        n_players = dplyr::n_distinct(vapply(
          draftables, function(p) as.character(p$playerId %||% ""), character(1)
        )),
        n_with_salary = sum(vapply(
          draftables, function(p) !is.null(p$salary), logical(1)
        )),
        game_dates = paste(sort(unique(starts[nzchar(starts)])), collapse = ",")
      )
      if (out$is_nfl && out$n_games >= 8L && out$n_with_salary > 0L) {
        saveRDS(body, file.path(payload_dir, sprintf("%d.rds", id)))
      }
    }
  }
  saveRDS(out, path)
  Sys.sleep(0.3)
  out
}

schedules <- readRDS("data/raw/schedules.rds") |>
  dplyr::filter(.data$game_type == "REG", .data$season %in% seasons) |>
  dplyr::mutate(gameday = as.Date(.data$gameday))

targets <- schedules |>
  dplyr::filter(.data$week %in% weeks) |>
  dplyr::group_by(.data$season, .data$week) |>
  dplyr::summarise(
    first_day = min(.data$gameday), last_day = max(.data$gameday),
    games = dplyr::n(), .groups = "drop"
  ) |>
  dplyr::arrange(.data$season, .data$week)

# Anchors calibrate ids to dates. These pairs were measured directly and must
# span the whole backfill range: estimate_id() extrapolates with rule = 2, so
# any date earlier than the first anchor clamps to that anchor's id. Seeding
# only from 2024 sent every 2022 and 2023 week to id 100000 and found nothing.
# Each slate located during a run is appended, so estimates tighten as it goes.
anchors <- tibble::tibble(
  id = c(
    50000, 60000, 70000, 80000, 90000, 100000, 114257, 120000,
    124000, 130000, 132000, 134000, 136000, 138000, 140000, 144000, 146163
  ),
  day = as.Date(c(
    "2021-05-16", "2021-11-28", "2022-06-08", "2023-01-01", "2023-07-23",
    "2024-02-08", "2024-10-06", "2025-01-09",
    "2025-03-21", "2025-07-02", "2025-08-12", "2025-09-19", "2025-10-25",
    "2025-11-30", "2026-01-10", "2026-03-23", "2026-09-09"
  ))
)
anchor_path <- file.path(cache_dir, "anchors.rds")
if (file.exists(anchor_path)) {
  anchors <- dplyr::bind_rows(anchors, readRDS(anchor_path)) |>
    dplyr::distinct(.data$id, .keep_all = TRUE) |>
    dplyr::arrange(.data$day)
}

# Confirmed classic NFL slates are by far the best anchors available: they are
# the exact thing being searched for, so interpolating between them lands much
# closer than generic id/date pairs taken from whatever sport happened to sit
# at that id.
confirmed_path <- "data/raw/dfs_salaries/dk_nfl_main_slates.csv"
if (file.exists(confirmed_path)) {
  confirmed <- readr::read_csv(confirmed_path, show_col_types = FALSE)
  if (nrow(confirmed) && all(c("id", "first_date") %in% names(confirmed))) {
    anchors <- dplyr::bind_rows(
      anchors,
      tibble::tibble(
        id = as.integer(confirmed$id),
        day = as.Date(confirmed$first_date)
      )
    ) |>
      dplyr::filter(!is.na(.data$id), !is.na(.data$day)) |>
      dplyr::distinct(.data$id, .keep_all = TRUE) |>
      dplyr::arrange(.data$day)
  }
}

estimate_id <- function(day) {
  a <- dplyr::arrange(anchors, .data$day)
  round(stats::approx(
    as.numeric(a$day), a$id, xout = as.numeric(day), rule = 2
  )$y)
}

cat("Seasons:", paste(seasons, collapse = ", "), "\n")
cat("Target weeks:", nrow(targets), "\n")
cat("Scan window: +/-", window, "ids\n")
cat("Cached probes:", length(list.files(summary_dir)), "\n")
cat("Estimated new calls (worst case):",
    nrow(targets) * (2 * window + 1), "\n")

if (!execute) {
  cat("\nPlan only. Add --execute to call the API.\n")
  print(as.data.frame(
    targets |> dplyr::mutate(estimated_id = estimate_id(.data$first_day)) |>
      head(10)
  ))
  quit(save = "no", status = 0)
}

found <- list()
for (i in seq_len(nrow(targets))) {
  target <- targets[i, ]
  centre <- estimate_id(target$first_day)

  # Draft group ids drift against the calendar, so a fixed window either misses
  # slates or wastes thousands of calls. Start narrow and widen only when a
  # week comes up empty; cached probes make the re-scan free for ids already
  # seen, so the expansion only pays for genuinely new ground.
  share_in_week <- function(dates) {
    vapply(dates, function(value) {
      if (is.na(value) || !nzchar(value)) return(NA_real_)
      days <- suppressWarnings(as.Date(strsplit(value, ",")[[1]]))
      days <- days[!is.na(days)]
      if (!length(days)) return(NA_real_)
      mean(days >= target$first_day - 1 & days <= target$last_day + 1)
    }, numeric(1), USE.NAMES = FALSE)
  }

  select_slate <- function(index) {
    if (!nrow(index)) return(index)
    candidates <- index |>
      dplyr::filter(
        .data$ok, .data$is_nfl, .data$n_games >= 8L, .data$n_with_salary > 0L
      ) |>
      dplyr::mutate(dates_in_week = share_in_week(.data$game_dates)) |>
      dplyr::filter(!is.na(.data$dates_in_week), .data$dates_in_week >= 0.75)
    if (!nrow(candidates)) return(candidates)
    # Every candidate already has its payload cached, so these checks cost
    # only a local read.
    quality <- purrr::map_dfr(
      candidates$id,
      function(id) tibble::as_tibble(payload_quality(id, payload_dir))
    )
    candidates |>
      dplyr::bind_cols(quality) |>
      dplyr::filter(
        !is.na(.data$nfl_share), .data$nfl_share >= 0.9,
        .data$n_teams >= 16, .data$n_teams <= 32,
        is.finite(.data$max_salary), .data$max_salary >= 7000,
        .data$idp_share < 0.05
      )
  }

  # Widen until a slate matching *this week's dates* appears. Testing only for
  # "any salaried NFL group" would stop early on a neighbouring week's slate,
  # which is what happened before. Cached probes make re-scanning free.
  in_week <- tibble::tibble()
  for (radius in c(window, window * 2L, window * 4L)) {
    ids <- seq(centre - radius, centre + radius)
    index <- purrr::map_dfr(ids, probe_id)
    in_week <- select_slate(index)
    if (nrow(in_week)) break
    message(sprintf("  widening %d week %02d to +/-%d",
                    target$season, target$week, radius * 2L))
  }

  if (!nrow(in_week)) {
    message(sprintf("%d week %02d: no main slate found near id %d",
                    target$season, target$week, centre))
    next
  }
  # Most games wins; ties break on the fuller player pool, which distinguishes
  # the main slate from a same-size but thinner variant.
  best <- in_week |>
    dplyr::arrange(dplyr::desc(.data$n_games), dplyr::desc(.data$n_players)) |>
    dplyr::slice_head(n = 1)
  found[[length(found) + 1L]] <- best |>
    dplyr::mutate(season = target$season, week = target$week)
  anchors <- dplyr::bind_rows(
    anchors, tibble::tibble(id = best$id, day = target$first_day)
  ) |>
    dplyr::distinct(.data$id, .keep_all = TRUE)
  saveRDS(anchors, anchor_path)
  message(sprintf("%d week %02d: id %d, %d games, %d players",
                  target$season, target$week, best$id, best$n_games,
                  best$n_players))
}

slates_path <- "data/raw/dfs_salaries/dk_main_slates.csv"
cat("\nMain slates found this run:", length(found), "of", nrow(targets), "\n")

# Accumulate across runs. Gap-filling passes target only the weeks that failed
# earlier, so normalising just this run's finds would overwrite the archive
# with a handful of weeks.
slates <- dplyr::bind_rows(found)
if (file.exists(slates_path)) {
  # game_dates is a comma-joined list of dates. Left to type inference, a file
  # whose rows all happen to hold a single date parses as <date> and then fails
  # to bind against the character column built in this run.
  previous <- readr::read_csv(
    slates_path,
    col_types = readr::cols(game_dates = readr::col_character()),
    show_col_types = FALSE
  )
  slates <- dplyr::bind_rows(slates, previous)
}
slates <- slates |>
  dplyr::distinct(.data$season, .data$week, .keep_all = TRUE) |>
  dplyr::arrange(.data$season, .data$week)

if (!nrow(slates)) {
  cat("Nothing to normalise. Widen --window or check the anchors.\n")
  quit(save = "no", status = 0)
}
readr::write_csv(slates, slates_path)
cat("Main slates known in total:", nrow(slates), "\n")

# Normalise the cached payloads into the project's salary schema.
rows <- purrr::pmap_dfr(
  list(slates$id, slates$season, slates$week),
  function(id, season, week) {
    path <- file.path(payload_dir, sprintf("%d.rds", id))
    if (!file.exists(path)) return(empty_dfs_salaries())
    body <- readRDS(path)
    competitions <- body$competitions %||% list()
    comp_lookup <- stats::setNames(
      lapply(competitions, identity),
      vapply(competitions, function(c) as.character(c$competitionId %||% ""),
             character(1))
    )
    purrr::map_dfr(body$draftables %||% list(), function(player) {
      comp <- player$competition %||% list()
      tibble::tibble(
        season = as.integer(season),
        week = as.integer(week),
        site = "DK",
        slate_type = "MAIN",
        slate_name = sprintf("%d Week %d main", season, week),
        player_id_site = as.character(player$playerId %||% NA),
        player_name = as.character(player$displayName %||% NA),
        position = toupper(as.character(player$position %||% NA)),
        team = normalize_dfs_team(player$teamAbbreviation %||% NA),
        opponent = NA_character_,
        home_away = NA_character_,
        salary = suppressWarnings(as.numeric(player$salary %||% NA)),
        fantasy_points = NA_real_,
        game_info = as.character(comp$name %||% NA),
        source = "DraftKings API",
        source_reference = sprintf(
          "https://api.draftkings.com/draftgroups/v1/draftgroups/%d/draftables", id
        ),
        captured_at_utc = format(Sys.time(), tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")
      )
    })
  }
) |>
  dplyr::filter(is.finite(.data$salary), .data$salary > 0) |>
  # Draftables repeat a player once per eligible roster slot; one row per
  # player per slate is what the salary archive expects.
  dplyr::distinct(
    .data$season, .data$week, .data$site, .data$player_id_site, .data$position,
    .keep_all = TRUE
  )

saveRDS(rows, "data/processed/dfs_salaries_dk_2022_plus.rds")
readr::write_csv(rows, "outputs/dfs_salaries_dk_2022_plus.csv")

cat("Salary rows recovered:", nrow(rows), "\n")
print(as.data.frame(
  rows |> dplyr::group_by(.data$season) |>
    dplyr::summarise(
      weeks = dplyr::n_distinct(.data$week),
      players = dplyr::n(),
      median_salary = stats::median(.data$salary),
      .groups = "drop"
    )
))
