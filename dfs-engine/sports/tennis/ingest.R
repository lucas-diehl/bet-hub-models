# ==============================================================================
# Tennis plugin — ingestion (ATP match history -> training store)
#
# DATA SOURCE NOTE (2026): the long-standing JeffSackmann/tennis_atp + tennis_wta
# GitHub repos were REMOVED (his account now exposes only tennis_MatchChartingProject;
# the old raw CSV URLs 404). We migrated to msolonskyi/ManTennisData, an actively
# maintained mirror of ATP match data WITH per-match serve stats (1999-present).
# Its schema differs from Sackmann's, so .mtd_standardize() maps it back to the
# Sackmann-style columns the model (model.R) consumes — model code is unchanged.
#   - matches_<year>.csv : one row per match, winner/loser + serve stats
#   - tournaments.csv    : surface + start date + category (joined by tournament_id)
# WTA has no maintained free mirror right now, so wta ingest is a documented no-op
# (WTA players on a DK slate fall to the season-avg baseline in project.R).
#
# DATA GAP (unchanged): historical DK salaries/ownership aren't here. The contest
# backtest needs saved DKSalaries exports + logged ownership — the logger handles that.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# raw-CSV mirror; override via env TENNIS_MTD_BASE if the mirror ever moves.
.mtd_base <- function() Sys.getenv("TENNIS_MTD_BASE",
  "https://raw.githubusercontent.com/msolonskyi/ManTennisData/master")
# WTA: JustSkadi's mirror is the ORIGINAL Sackmann wta_matches schema (date/surface/
# score/serve stats inline) -> usable as-is. 2019-2024 (recency gap; established field).
.wta_url <- function() Sys.getenv("TENNIS_WTA_URL",
  "https://raw.githubusercontent.com/JustSkadi/WTA-Matches-2019-2024-Analysis/main/wta_matches_combined.csv")
TENNIS_TOURS_LIVE <- c("atp", "wta")       # tours with a maintained free source

tennis_matches_path <- function(tour) dfs_path("data", "raw", paste0("tennis_", tour, "_matches.rds"))
tennis_src_dir      <- function() dfs_path("data", "raw", "tennis_src")
tennis_src_csv      <- function(tour, year) file.path(tennis_src_dir(), sprintf("%s_matches_%d.csv", tour, year))

# Robust cache-or-download of a CSV from a full URL. prefer_cache=TRUE (immutable past
# data) reads the cache if present; FALSE re-downloads but falls back to a stale cache.
.tennis_dl <- function(url, cache_file, prefer_cache = TRUE) {
  local <- file.path(tennis_src_dir(), cache_file)
  if (prefer_cache && file.exists(local)) return(fread(local, showProgress = FALSE))
  resp <- tryCatch(httr2::request(url) |> httr2::req_user_agent("DFS-ENGINE/1.0") |>
                     httr2::req_timeout(120) |> httr2::req_retry(max_tries = 3) |>
                     httr2::req_perform(), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) != 200) {
    if (file.exists(local)) { msg("  ", cache_file, "unreachable -> using cached copy"); return(fread(local, showProgress = FALSE)) }
    msg("  skip", cache_file, "(unreachable, no cache)"); return(NULL)
  }
  txt <- httr2::resp_body_string(resp)
  dir.create(tennis_src_dir(), recursive = TRUE, showWarnings = FALSE)
  writeLines(txt, local)
  fread(text = txt, showProgress = FALSE)
}
.mtd_fetch <- function(relpath, cache_file, prefer_cache = TRUE)
  .tennis_dl(paste0(.mtd_base(), "/", relpath), cache_file, prefer_cache)

# "64 64" / "76(4) 36 64" -> Sackmann-style "6-4 6-4" / "7-6(4) 3-6 6-4" (winner POV).
.mtd_convert_score <- function(s) {
  if (is.na(s) || !nzchar(trimws(s))) return(NA_character_)
  toks <- strsplit(trimws(s), "\\s+")[[1]]
  out <- vapply(toks, function(t) {
    tb <- regmatches(t, regexpr("\\(\\d+\\)$", t)); tb <- if (length(tb)) tb else ""
    g  <- sub("\\(.*$", "", t)                                    # games digits, e.g. "76"
    if (!grepl("^\\d{2,}$", g)) return(NA_character_)
    a <- if (startsWith(g, "10")) "10" else substr(g, 1, 1)       # tolerate 10-x super sets
    b <- substr(g, nchar(a) + 1L, nchar(g))
    if (!nzchar(b)) return(NA_character_)
    paste0(a, "-", b, tb)
  }, character(1))
  out <- out[!is.na(out)]
  if (!length(out)) return(NA_character_)
  paste(out, collapse = " ")
}

# Map a ManTennisData (matches + tournaments) pair to Sackmann-style columns.
.mtd_standardize <- function(M, TT, tour) {
  M <- as.data.table(M); TT <- as.data.table(TT)
  tt <- TT[, .(tournament_id = id, t_surface = surface, t_date = start_dtm,
               t_name = name, t_cat = series_category_id)]
  M <- merge(M, tt, by = "tournament_id", all.x = TRUE)
  num <- function(col) if (col %in% names(M)) suppressWarnings(as.numeric(M[[col]])) else rep(NA_real_, nrow(M))
  slam <- (!is.na(M$t_cat) & M$t_cat == "gs") |
          M$t_name %in% c("Australian Open", "Roland Garros", "Wimbledon", "US Open", "French Open")
  data.table(
    tour         = tour,
    tourney_date = as.character(M$t_date),                         # YYYYMMDD
    surface      = fifelse(M$t_surface %in% c("Hard","Clay","Grass","Carpet"), M$t_surface, "Hard"),
    best_of      = fifelse(slam, 5L, 3L),
    score        = vapply(as.character(M$match_score), .mtd_convert_score, character(1)),
    winner_id    = as.character(M$winner_code),  winner_name = as.character(M$winner_name),
    loser_id     = as.character(M$loser_code),   loser_name  = as.character(M$loser_name),
    w_ace = num("win_aces"),            w_df = num("win_double_faults"),
    l_ace = num("los_aces"),            l_df = num("los_double_faults"),
    w_bpFaced = num("win_break_points_serve_total"), w_bpSaved = num("win_break_points_saved"),
    l_bpFaced = num("los_break_points_serve_total"), l_bpSaved = num("los_break_points_saved"),
    w_svpt = num("win_service_points_total"),  w_SvGms = num("win_service_games_played"),
    l_svpt = num("los_service_points_total"),  l_SvGms = num("los_service_games_played"))
}

# ATP via ManTennisData (matches_<yr>.csv + tournaments.csv -> Sackmann-style cols).
.atp_ingest <- function(years) {
  yr <- as.integer(format(Sys.Date(), "%Y"))
  TT <- .mtd_fetch("atp/tournaments.csv", "atp_tournaments.csv", prefer_cache = FALSE)
  if (is.null(TT)) { msg("No ATP tournaments file (network blocked? drop CSVs in ", tennis_src_dir(), ")"); return(NULL) }
  rbindlist(lapply(years, function(y) {
    M <- .mtd_fetch(sprintf("atp/matches_%d.csv", y), sprintf("atp_matches_%d.csv", y), prefer_cache = (y < yr))
    if (is.null(M) || !nrow(M)) return(NULL)
    d <- .mtd_standardize(M, TT, "atp"); d[, season := y]; d
  }), fill = TRUE)
}

# WTA via the JustSkadi mirror: already the original Sackmann schema, so just tag the
# tour + season and coerce ids to character (ATP uses string codes). One combined file.
.wta_ingest <- function() {
  W <- .tennis_dl(.wta_url(), "wta_matches_combined.csv", prefer_cache = FALSE)
  if (is.null(W) || !nrow(W)) return(NULL)
  W <- as.data.table(W)
  W[, `:=`(tour = "wta", season = as.integer(year),
           winner_id = as.character(winner_id), loser_id = as.character(loser_id),
           tourney_date = as.character(tourney_date))]
  W[!is.na(surface) & nzchar(surface)]
}

# Download + store match results for one or both tours. years (ATP) default last 5;
# current year is re-fetched (mutable), past years cached immutably. WTA is one file.
tennis_ingest <- function(tours = c("atp", "wta"), years = NULL) {
  yr <- as.integer(format(Sys.Date(), "%Y"))
  if (is.null(years)) years <- (yr - 4):yr
  for (tour in tours) {
    got <- switch(tour, atp = .atp_ingest(years), wta = .wta_ingest(),
                  { msg("No maintained free", toupper(tour), "source -> skipping (DK baseline)."); NULL })
    if (is.null(got) || !nrow(got)) { msg("No ", toupper(tour), " data (network blocked? drop CSVs in ", tennis_src_dir(), ")"); next }
    dir.create(dirname(tennis_matches_path(tour)), recursive = TRUE, showWarnings = FALSE)
    saveRDS(got, tennis_matches_path(tour))
    msg("Stored", nrow(got), toupper(tour), "matches ->", tennis_matches_path(tour))
  }
  invisible(TRUE)
}

# Normalize an event/tournament string to a city key for surface lookup.
# "ATP Eastbourne, Great Britain Men Singles" -> "eastbourne"; "Eastbourne" -> "eastbourne".
tennis_event_key <- function(x) {
  x <- as.character(x)
  x <- sub("^\\s*(ATP|WTA)\\b\\s*(Challenger)?\\s*", "", x, ignore.case = TRUE)  # drop tour prefix
  x <- sub(",.*$", "", x)                                          # drop ", Country ..."
  x <- sub("\\s+(Men|Women)\\s+Singles.*$", "", x, ignore.case = TRUE)
  tolower(gsub("[^a-z]+", "", tolower(trimws(x))))                 # keep letters only
}

# Tournament-city -> surface lookup so projection can infer a DK match's surface (the
# DK feed omits it). Built from BOTH tours so mixed ATP+WTA slates resolve: ATP from
# ManTennisData tournaments.csv (name+surface), WTA from the WTA matches file
# (tourney_name+surface, inline). Most-recent surface per city wins. Returns a named
# character vector key->surface (or NULL). `tour` arg kept for back-compat (ignored).
tennis_surface_lookup <- function(tour = NULL) {
  parts <- list()
  fa <- file.path(tennis_src_dir(), "atp_tournaments.csv")
  if (file.exists(fa)) { TT <- tryCatch(fread(fa, showProgress = FALSE), error = function(e) NULL)
    if (!is.null(TT) && all(c("name", "surface", "year") %in% names(TT)))
      parts[["atp"]] <- as.data.table(TT)[, .(name, surface, year)] }
  fw <- tennis_matches_path("wta")
  if (file.exists(fw)) { W <- tryCatch(as.data.table(readRDS(fw)), error = function(e) NULL)
    if (!is.null(W) && all(c("tourney_name", "surface", "season") %in% names(W)))
      parts[["wta"]] <- W[, .(name = tourney_name, surface, year = season)] }
  if (!length(parts)) return(NULL)
  TT <- rbindlist(parts, fill = TRUE)[surface %in% c("Hard", "Clay", "Grass", "Carpet")]
  if (!nrow(TT)) return(NULL)
  TT[, key := tennis_event_key(name)]
  setorder(TT, -year)
  TT <- unique(TT[key != ""], by = "key")
  stats::setNames(TT$surface, TT$key)
}
