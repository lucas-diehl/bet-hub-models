# ==============================================================================
# DFS ENGINE — FanDuel salary ingest (CSV-drop, since FD has no open API)
# FanDuel doesn't expose a public no-auth salary endpoint like DraftKings, so we use
# FD's own "Download players list" CSV (available on every FD contest). Drop it in
# data/fd_inbox/ (any name) OR pass a path; this normalizes it to the pipeline's pool
# shape, writes the DKSalaries-style snapshot at the site='fd' path (so the rest of the
# pipeline reads FD and DK uniformly), persists salaries, and registers the slate.
#
#   fd_ingest_csv("nfl", "data/fd_inbox/FanDuel-NFL-....csv")   # explicit file
#   fd_ingest_latest("golf")                                    # newest CSV in fd_inbox
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

fd_inbox_dir <- function() { d <- dfs_path("data", "fd_inbox"); dir.create(d, recursive = TRUE, showWarnings = FALSE); d }

# Parse FanDuel's player-list export into the normalized pool shape (mirrors dk_draftables).
# FD columns: Id, Position, First Name, Last Name, FPPG, Played, Salary, Game, Team,
# Opponent, Injury Indicator, Injury Details, Tier, Roster Position.
read_fd_players_csv <- function(path, sport) {
  d <- as.data.table(fread(path)); nm <- names(d)
  g <- function(cands, default = NA) { h <- intersect(cands, nm); if (length(h)) d[[h[1]]] else rep(default, nrow(d)) }
  nick  <- as.character(g(c("Nickname")))
  first <- as.character(g(c("First Name", "first_name"))); last <- as.character(g(c("Last Name", "last_name")))
  pname <- ifelse(!is.na(nick) & nzchar(nick), nick, trimws(paste(first, last)))
  gm <- as.character(g(c("Game", "game")))                                  # e.g. "KC@BUF"
  team <- as.character(g(c("Team", "team"))); opp <- as.character(g(c("Opponent", "opponent")))
  gid <- ifelse(!is.na(gm) & nzchar(gm), gsub("\\s+", "", gm),              # prefer FD's own "AWY@HOM"
                ifelse(!is.na(team) & !is.na(opp), apply(cbind(team, opp), 1, function(x) paste(sort(x), collapse = "@")), NA))
  out <- data.table(
    player_name = pname,
    fd_id    = as.character(g(c("Id", "id"))),
    position = as.character(g(c("Position", "position"))),
    salary   = suppressWarnings(as.integer(g(c("Salary", "salary")))),
    team     = team, game_id = gid,
    fd_avg   = suppressWarnings(as.numeric(g(c("FPPG", "fppg")))),
    status   = toupper(trimws(as.character(g(c("Injury Indicator", "injury_indicator"), "")))))
  out <- out[!is.na(salary) & salary > 0]
  if (!nrow(out)) stop("FanDuel CSV has no priced players: ", path)
  out <- out[!(status %in% c("O", "OUT", "IR", "SUSP", "D"))]              # drop Out/IR/Doubtful/Suspended
  out[, norm := norm_name(player_name)]
  out[, player_id := surrogate_player_id(norm)]
  out[, sport := sport]
  unique(out, by = "fd_id")[]
}

# Ingest a specific FD CSV -> normalized snapshot + DB (salaries + slate registration).
fd_ingest_csv <- function(sport, raw_csv, date = Sys.Date(), slate = "main") {
  if (!file.exists(raw_csv)) stop("FanDuel CSV not found: ", raw_csv)
  pool <- read_fd_players_csv(raw_csv, sport)
  slate_id <- make_slate_id(sport, "fd", date, slate); pool[, slate_id := slate_id]
  snap <- data.table(
    Position = pool$position, `Name + ID` = paste0(pool$player_name, " (", pool$fd_id, ")"),
    Name = pool$player_name, ID = pool$fd_id, `Roster Position` = pool$position,
    Salary = pool$salary, `Game Info` = pool$game_id, TeamAbbrev = pool$team,
    AvgPointsPerGame = pool$fd_avg)
  csv_path <- dk_salary_path(sport, date, slate, "fd")
  dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE); fwrite(snap, csv_path)
  db_upsert("slates", data.frame(
    slate_id = slate_id, sport = sport, site = "fd", slate_date = as.character(date),
    name = slate, lock_ts = NA_character_, updated_ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S")), keys = "slate_id")
  persist_salaries(pool, slate_id, sport)
  msg(sprintf("FanDuel ingest: %d players -> %s (+ DB).", nrow(pool), csv_path))
  invisible(list(slate_id = slate_id, draft_group_id = NA, pool = pool, csv = csv_path))
}

# Ingest the newest FD CSV dropped in data/fd_inbox/ whose name matches the sport.
# FanDuel names files by its own league label (golf files say "PGA", not "golf").
.FD_NAME <- c(golf = "golf|pga", nfl = "nfl", wnba = "wnba", tennis = "tennis|\\bten\\b",
              ncaaf = "cfb|ncaaf|college", nba = "\\bnba\\b")
fd_ingest_latest <- function(sport, date = Sys.Date(), slate = "main") {
  fs <- list.files(fd_inbox_dir(), pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  pat <- .FD_NAME[[sport]] %||% sport
  if (sport != "") fs <- fs[grepl(pat, basename(fs), ignore.case = TRUE)]
  if (!length(fs)) { msg("FanDuel: no CSV for", sport, "in", fd_inbox_dir()); return(invisible(NULL)) }
  fd_ingest_csv(sport, fs[which.max(file.mtime(fs))], date, slate)
}
