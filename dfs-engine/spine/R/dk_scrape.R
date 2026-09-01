# ==============================================================================
# DFS ENGINE — DraftKings salary scraper (free, repeatable, backtestable)
# Pulls the player pool + salaries from DraftKings' public JSON endpoints (no auth,
# no cost), the same source free DFS tools use. Storing each day's pull is what
# BUILDS the historical-salary record the contest backtest needs (free feeds don't
# carry it). Replaces the manual DKSalaries.csv download.
#
# Endpoints (public):
#   lobby/getcontests?sport=WNBA           -> DraftGroups (slate ids + metadata)
#   draftgroups/v1/draftgroups/{id}/draftables -> players: salary, pos, team, game, fppg, status
#
# Also writes a DKSalaries-style CSV snapshot to data/slates/<sport>/ so the rest of
# the pipeline (read_dk_salary_csv) and the backtest history both consume it.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# our sport key -> DK lobby sport identifier
.DK_SPORT <- c(golf = "GOLF", wnba = "WNBA", tennis = "TEN",
               nfl = "NFL", ncaaf = "CFB", nba = "NBA")

.dk_get_json <- function(url) {
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) DFS-ENGINE/1.0") |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_timeout(30) |>
      httr2::req_perform(),
    error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) != 200) return(NULL)
  tryCatch(httr2::resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
}

# DK files WNBA under Sport="NBA", so the Sport field is unreliable. Identify a
# contest's sport from its NAME / attr.League instead (authoritative).
DK_SPORT_MATCH <- list(
  wnba  = function(n, lg) grepl("wnba", n, ignore.case = TRUE) | toupper(lg) %in% "WNBA",
  nba   = function(n, lg) (grepl("\\bnba\\b", n, ignore.case = TRUE) & !grepl("wnba", n, ignore.case = TRUE)) | toupper(lg) %in% "NBA",
  nfl   = function(n, lg) grepl("\\bnfl\\b", n, ignore.case = TRUE) | toupper(lg) %in% "NFL",
  ncaaf = function(n, lg) grepl("cfb|college foot|ncaaf", n, ignore.case = TRUE) | toupper(lg) %in% c("CFB","NCAAF"),
  # DK names tennis contests "TEN $..." (abbrev prefix) and the tennis lobby has no
  # attr.League column — so match the \bTEN\b token, not just the word "tennis".
  tennis= function(n, lg) grepl("\\bten\\b|tennis", n, ignore.case = TRUE) | toupper(lg) %in% c("TEN","TENNIS"),
  golf  = function(n, lg) grepl("pga|golf", n, ignore.case = TRUE) | toupper(lg) %in% c("GOLF","PGA"))

# Find a sport's available slates today by scanning the Contests list (robust to the
# WNBA-under-NBA quirk). Per draft group, returns a representative guaranteed GPP
# (for the real payout) + game count. Sorted: classic multi-game first, then prize.
dk_find_slates <- function(sport) {
  dks <- .DK_SPORT[[sport]] %||% toupper(sport)
  j <- .dk_get_json(paste0("https://www.draftkings.com/lobby/getcontests?sport=", dks))
  if (is.null(j) || is.null(j$Contests)) return(NULL)
  ct <- as.data.table(j$Contests)
  lg <- if ("attr.League" %in% names(ct)) ct[["attr.League"]] else rep(NA_character_, nrow(ct))
  mf <- DK_SPORT_MATCH[[sport]] %||% function(n, l) grepl(sport, n, ignore.case = TRUE)
  ct <- ct[which(mf(ct$n, lg))]
  if (!nrow(ct)) return(NULL)
  ct[, `:=`(fee = suppressWarnings(as.numeric(a)), field = suppressWarnings(as.integer(m)),
            prize = suppressWarnings(as.numeric(po)))]
  du   <- if ("attr.IsDoubleUp" %in% names(ct)) ct[["attr.IsDoubleUp"]] %in% TRUE else rep(FALSE, nrow(ct))
  ff   <- if ("attr.IsFiftyfifty" %in% names(ct)) ct[["attr.IsFiftyfifty"]] %in% TRUE else rep(FALSE, nrow(ct))
  ct[, gpp := !du & !ff & !grepl("satellite|qualifier|winner take all|single stat", n, ignore.case = TRUE)]
  dgm <- as.data.table(j$DraftGroups)[, .(dg = DraftGroupId, GameCount,
              Suffix = ContestStartTimeSuffix, StartDateEst)]
  slates <- ct[, { g <- .SD[gpp == TRUE]; if (!nrow(g)) g <- .SD; g <- g[order(-prize)]
    .(top_contest_id = g$id[1], top_name = g$n[1], top_fee = g$fee[1],
      top_field = g$field[1], top_prize = g$prize[1], n_contests = .N) }, by = dg]
  slates <- merge(slates, dgm, by = "dg", all.x = TRUE)
  setorder(slates, -GameCount, -top_prize)
  slates[]
}

# The main slate to play for a sport: classic multi-game if available, else the
# (single-game) Showdown. Detects Showdown authoritatively from the draftables.
dk_main_slate <- function(sport) {
  s <- dk_find_slates(sport); if (is.null(s) || !nrow(s)) return(NULL)
  classic <- s[GameCount > 1 & !grepl("showdown|captain", top_name, ignore.case = TRUE)]
  pick <- if (nrow(classic)) classic[1] else s[1]
  showdown <- tryCatch({
    j <- .dk_get_json(sprintf("https://api.draftkings.com/draftgroups/v1/draftgroups/%s/draftables", pick$dg))
    uniqueN(as.data.table(j$draftables)$rosterSlotId) == 2
  }, error = function(e) grepl("showdown|captain", pick$top_name, ignore.case = TRUE))
  list(draft_group_id = pick$dg, contest_id = pick$top_contest_id, name = pick$top_name,
       game_count = pick$GameCount, is_showdown = isTRUE(showdown),
       fee = pick$top_fee, field = pick$top_field, prize = pick$top_prize)
}

# List DK draft groups (slates) for a sport. Returns data.table sorted by game count.
dk_draftgroups <- function(sport) {
  dks <- .DK_SPORT[[sport]]; if (is.null(dks)) stop("No DK sport mapping for: ", sport)
  j <- .dk_get_json(paste0("https://www.draftkings.com/lobby/getcontests?sport=", dks))
  if (is.null(j) || is.null(j$DraftGroups)) stop("DK lobby returned no draft groups for ", dks,
    " (no slate live right now, or DK blocked the request).")
  dg <- as.data.table(j$DraftGroups)
  keep <- intersect(c("DraftGroupId","ContestStartTimeSuffix","StartDate","StartDateEst",
                      "GameCount","ContestTypeId","Sport"), names(dg))
  dg <- dg[, ..keep]
  # getcontests ignores the sport= param and returns ALL sports — filter by the
  # Sport field (also our guard against ever pulling MLB/baseball).
  if ("Sport" %in% names(dg)) dg <- dg[toupper(Sport) == dks]
  if (!nrow(dg)) stop("No live ", dks, " slate on DK right now.")
  setorder(dg, -GameCount)
  dg[]
}

# Pick the "main" slate: most games, no time-suffix (afternoon/night sub-slates).
dk_main_draftgroup <- function(sport) {
  dg <- dk_draftgroups(sport)
  main <- dg[is.na(ContestStartTimeSuffix) | ContestStartTimeSuffix == ""]
  if (!nrow(main)) main <- dg
  main$DraftGroupId[1]
}

# Pull the draftables (player pool) for a draft group -> normalized pool data.table.
dk_draftables <- function(draft_group_id, sport) {
  url <- sprintf("https://api.draftkings.com/draftgroups/v1/draftgroups/%s/draftables",
                 as.character(draft_group_id))
  j <- .dk_get_json(url)
  if (is.null(j) || is.null(j$draftables)) stop("No draftables for draft group ", draft_group_id)
  d <- as.data.table(j$draftables)
  if (!nrow(d)) stop("Draft group ", draft_group_id, " has no players yet (slate not populated).")

  # fppg lives in draftStatAttributes (id == -2 is AvgPointsPerGame on DK)
  fppg <- vapply(seq_len(nrow(d)), function(i) {
    a <- d$draftStatAttributes[[i]]
    if (is.null(a) || !length(a)) return(NA_real_)
    a <- as.data.table(a)
    v <- if ("id" %in% names(a) && any(a$id == -2)) a$value[a$id == -2][1] else a$value[1]
    suppressWarnings(as.numeric(v))
  }, numeric(1))

  # Match/game key. as.data.table() flattens DK's nested `competition` object into
  # dotted columns (competition.name / competition.competitionId), so d$competition is
  # usually NULL — read the flattened columns too. For TEAM sports competition.name is
  # the readable matchup ("DAL @ LAV") that both sides share; for TENNIS it's only the
  # TOURNAMENT ("ATP Eastbourne ... Men Singles"), so the per-MATCH pairing must come
  # from competition.competitionId (exactly 2 players per id). Prefer the readable
  # "AWY@HOM" when present (team_env.R parses it), else fall back to the competition id.
  pick <- function(nested, dotted) {
    if (!is.null(comp) && is.data.frame(comp) && nested %in% names(comp)) as.character(comp[[nested]])
    else if (dotted %in% names(d)) as.character(d[[dotted]])
    else NA_character_
  }
  comp <- d$competition
  game_name <- pick("name", "competition.name")
  comp_id   <- pick("competitionId", "competition.competitionId")
  is_matchup <- !is.na(game_name) & grepl("@| vs\\.? | v\\.? ", game_name, ignore.case = TRUE)
  game_key  <- ifelse(is_matchup, gsub("\\s+", "", game_name), comp_id)

  out <- data.table(
    player_name = as.character(d$displayName),
    dk_id       = as.character(if ("playerDkId" %in% names(d)) d$playerDkId else d$playerId),
    position    = as.character(d$position),
    salary      = suppressWarnings(as.integer(d$salary)),
    team        = as.character(if ("teamAbbreviation" %in% names(d)) d$teamAbbreviation else NA),
    game_id     = game_key,                                    # "DAL@LAV" or per-match comp id
    event       = as.character(game_name),                     # readable event (tennis: tournament)
    dk_avg      = fppg,
    status      = as.character(if ("status" %in% names(d)) d$status else NA))
  out <- out[!is.na(salary) & salary > 0]
  # rows can exist with no salaries set yet (slate posted, pool not priced) — treat
  # the same as "not populated" so callers degrade gracefully instead of writing a
  # header-only CSV that poisons later runs (read_dk_salary_csv would re-read empty).
  if (!nrow(out)) stop("Draft group ", draft_group_id, " has no priced players yet (slate not populated).")
  out <- unique(out, by = "dk_id")                            # dedupe FLEX/CPT duplicate slots
  # Drop players DK has ruled OUT/injured so they are never projected or rostered.
  # status CSV column is dropped downstream, so this is the one place to enforce it.
  if ("status" %in% names(out)) {
    n0 <- nrow(out)
    out <- out[is.na(status) | !toupper(trimws(status)) %in% c("OUT","O","IL","INJ","SUSP","DNP","NA")]
    if (nrow(out) < n0) msg(sprintf("  dropped %d %s player(s) ruled OUT", n0 - nrow(out), sport))
  }
  out[, norm := norm_name(player_name)]
  out[, player_id := surrogate_player_id(norm)]
  out[, sport := sport]
  out[]
}

# Ensure a slate CSV exists for (sport,date); auto-scrape today's DK slate if not.
# Returns TRUE if a slate is available, FALSE if none is posted yet (e.g. early in
# the day before DK puts the slate up). Golf uses DataGolf, so always "available".
ensure_dk_slate <- function(sport, date = Sys.Date(), slate = "main", site = "dk", max_age_days = 0) {
  if (sport == "golf") return(TRUE)
  p <- dk_salary_path(sport, date, slate, site)
  if (dk_salary_csv_ready(p, date)) return(TRUE)            # exists, non-empty, not stale
  # past-date CSV that's empty/missing has no live draft group to re-pull
  if (as.Date(date) != Sys.Date()) return(file.exists(p) && dk_salary_csv_rows(p) >= 1L)
  ok <- tryCatch({ scrape_dk_salaries(sport, date = date, slate = slate, site = site); TRUE },
                 error = function(e) { msg("  no live", sport, "slate:", conditionMessage(e)); FALSE })
  ok && file.exists(p)
}

# Full daily scrape: discover main slate, pull players, persist raw + salaries, and
# write the DKSalaries-style CSV snapshot the pipeline/backtest read. Idempotent.
scrape_dk_salaries <- function(sport, date = Sys.Date(), slate = "main",
                               draft_group_id = NULL, site = "dk") {
  if (is.null(draft_group_id)) draft_group_id <- dk_main_draftgroup(sport)
  msg("DK draft group for", sport, ":", draft_group_id)
  pool <- dk_draftables(draft_group_id, sport)
  slate_id <- make_slate_id(sport, site, date, slate)
  pool[, slate_id := slate_id]

  # raw landing (re-derivable features later)
  raw_dir <- dfs_path("data", "raw", "dk_salaries"); dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(pool, file.path(raw_dir, paste0(slate_id, "_", draft_group_id, ".rds")))

  # CSV snapshot in DKSalaries shape (read_dk_salary_csv consumes this)
  snap <- data.table(
    Position = pool$position, `Name + ID` = paste0(pool$player_name, " (", pool$dk_id, ")"),
    Name = pool$player_name, ID = pool$dk_id, `Roster Position` = pool$position,
    Salary = pool$salary, `Game Info` = pool$game_id, TeamAbbrev = pool$team,
    AvgPointsPerGame = pool$dk_avg,
    Event = if ("event" %in% names(pool)) pool$event else NA_character_)  # surface inference (tennis)
  csv_path <- dk_salary_path(sport, date, slate, site)
  dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
  fwrite(snap, csv_path)

  # register the slate + persist salaries (builds backtest history)
  db_upsert("slates", data.frame(
    slate_id = slate_id, sport = sport, site = site, slate_date = as.character(date),
    name = slate, lock_ts = NA_character_, updated_ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    keys = "slate_id")
  persist_salaries(pool, slate_id, sport)

  msg(sprintf("Scraped %d players -> %s (+ DB). %d listed OUT/injured.",
              nrow(pool), csv_path, sum(!is.na(pool$status) & pool$status != "None", na.rm = TRUE)))
  invisible(list(slate_id = slate_id, draft_group_id = draft_group_id, pool = pool, csv = csv_path))
}
