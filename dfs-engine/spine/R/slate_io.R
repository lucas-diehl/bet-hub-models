# ==============================================================================
# DFS ENGINE — slate I/O (DK salary CSV reader + slate persistence)
# There is no public DK API for the slate/player pool, so the realistic input is
# the "DKSalaries.csv" export you download per slate. This reader normalizes it
# into the spine's salaries table and returns the player pool every sport's
# project_players() builds on. Sport-agnostic.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# Default location for a downloaded DK salaries export.
dk_salary_path <- function(sport, date, slate = "main", site = "dk") {
  dfs_path("data", "slates", sport, paste0(site, "_", as.character(date), "_", slate, ".csv"))
}

# Parse a DKSalaries.csv. DK columns: Position, "Name + ID", Name, ID,
# "Roster Position", Salary, "Game Info", TeamAbbrev, AvgPointsPerGame.
# Game Info looks like "LAS@NYL 06/22/2026 07:00PM ET" -> we derive game_id + opp.
read_dk_salary_csv <- function(path, sport, slate_id = NULL) {
  if (!file.exists(path)) stop("DK salaries CSV not found: ", path,
                               "\n  Download the slate's DKSalaries.csv and place it there.")
  d <- fread(path, check.names = FALSE, na.strings = c("", "NA"))
  nm <- names(d); low <- tolower(trimws(nm))
  g <- function(cands) { hit <- nm[low %in% cands]; if (length(hit)) hit[1] else NA_character_ }
  c_pos <- g(c("position", "roster position")); c_name <- g(c("name"))
  c_id  <- g(c("id")); c_sal <- g(c("salary")); c_team <- g(c("teamabbrev", "team"))
  c_game<- g(c("game info", "gameinfo")); c_avg <- g(c("avgpointspergame", "avg points per game"))
  c_event <- g(c("event"))               # readable event name (tennis: tournament -> surface)
  if (anyNA(c(c_name, c_sal))) stop("Unrecognized DK salaries format (need Name, Salary): ", path)

  out <- data.table(
    player_name = as.character(d[[c_name]]),
    dk_id       = if (!is.na(c_id)) as.character(d[[c_id]]) else NA_character_,
    position    = if (!is.na(c_pos)) as.character(d[[c_pos]]) else NA_character_,
    salary      = suppressWarnings(as.integer(d[[c_sal]])),
    team        = if (!is.na(c_team)) as.character(d[[c_team]]) else NA_character_,
    dk_avg      = if (!is.na(c_avg)) suppressWarnings(as.numeric(d[[c_avg]])) else NA_real_,
    event       = if (!is.na(c_event)) as.character(d[[c_event]]) else NA_character_)
  out <- out[!is.na(salary) & salary > 0]
  # derive game_id from "AWY@HOM date time"
  if (!is.na(c_game)) {
    gi <- as.character(d[[c_game]])[!is.na(d[[c_sal]]) & d[[c_sal]] > 0]
    matchup <- sub("\\s.*$", "", gi)                       # "AWY@HOM"
    out[, game_id := matchup]
  } else out[, game_id := NA_character_]
  out[, norm := norm_name(player_name)]
  out[, player_id := surrogate_player_id(norm)]            # stable id until mapped to source ids
  out[, sport := sport]
  if (!is.null(slate_id)) out[, slate_id := slate_id]
  out[]
}

# Count priced-player rows in a saved DK salary CSV (0 if missing/empty/malformed).
dk_salary_csv_rows <- function(path) {
  if (!file.exists(path)) return(0L)
  as.integer(tryCatch(nrow(fread(path, na.strings = c("", "NA"), showProgress = FALSE)),
                      error = function(e) 0L))
}

# Is a saved DK salary CSV usable as a ready slate? It must exist AND carry at least
# one priced player row. A header-only/empty CSV (e.g. scraped before DK posted the
# pool) must NOT count as ready, or it poisons every downstream read. For a "today"
# slate we also re-scrape when the file is stale (DK keeps adding/repricing players
# and scratching late inactives up to lock); historical (past-date) CSVs never expire.
dk_salary_csv_ready <- function(path, date = NULL, max_age_hours = 12) {
  if (dk_salary_csv_rows(path) < 1L) return(FALSE)
  if (!is.null(date) && as.Date(date) == Sys.Date()) {
    age_h <- as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "hours"))
    if (is.finite(age_h) && age_h > max_age_hours) return(FALSE)
  }
  TRUE
}

# Write a normalized pool (from a DK scrape/contest) as a DKSalaries-style CSV so
# the sport projection models (read_dk_salary_csv) can consume it.
write_dk_snapshot_csv <- function(pool, sport, date, slate = "main", site = "dk") {
  pool <- as.data.table(pool)
  snap <- data.table(
    Position = pool$position, `Name + ID` = paste0(pool$player_name, " (", pool$dk_id, ")"),
    Name = pool$player_name, ID = pool$dk_id, `Roster Position` = pool$position,
    Salary = pool$salary, `Game Info` = if ("game_id" %in% names(pool)) pool$game_id else "",
    TeamAbbrev = pool$team, AvgPointsPerGame = if ("dk_avg" %in% names(pool)) pool$dk_avg else NA_real_)
  path <- dk_salary_path(sport, date, slate, site)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  fwrite(snap, path)
  path
}

# Auto-log OUR projected ownership for a slate (contest_id "_projected"). Builds the
# ownership dataset alongside salaries every run; later the logger fills actual_pct
# from settled contests, and the join (projected vs actual) trains the ownership model.
persist_projected_ownership <- function(pool, slate_id, sport) {
  pool <- as.data.table(pool)
  if (!"own" %in% names(pool)) return(invisible(0L))
  df <- data.frame(slate_id = slate_id, sport = sport, player_id = pool$player_id,
                   contest_id = "_projected", projected_pct = round(100 * pool$own, 2),
                   actual_pct = NA_real_, captured_ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  db_upsert("ownership", df, keys = c("slate_id", "player_id", "contest_id"))
}

# Persist a normalized pool's salaries to the store (idempotent).
persist_salaries <- function(pool, slate_id, sport) {
  pool <- as.data.table(pool)
  sal <- data.frame(
    slate_id = slate_id, sport = sport, player_id = pool$player_id,
    salary = as.integer(pool$salary),
    position = if ("position" %in% names(pool)) pool$position else NA_character_,
    roster_slot = NA_character_,
    game_id = if ("game_id" %in% names(pool)) pool$game_id else NA_character_,
    team = if ("team" %in% names(pool)) pool$team else NA_character_)
  db_upsert("salaries", sal, keys = c("slate_id", "player_id"))
  # also register players
  pl <- data.frame(sport = sport, player_id = pool$player_id,
                   name = pool$player_name, team = if ("team" %in% names(pool)) pool$team else NA,
                   ext_ids = NA_character_, updated_ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  db_upsert("players", unique(pl), keys = c("sport", "player_id"))
  invisible(nrow(sal))
}
