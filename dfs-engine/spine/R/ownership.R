# ==============================================================================
# DFS ENGINE — ownership logging (the gating, un-backfillable asset)
# Parses a DK "Contest Standings" CSV -> actual %Drafted, and the inbox auto-
# importer that derives ALL metadata from the contest id in the filename (via the
# contest API), so logging is "drop the file in a folder" — no flags to type.
# (Shared by jobs/log_ownership.R and jobs/import_ownership.R.)
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# Parse the ownership block (Player / %Drafted) from a DK standings export.
parse_dk_standings <- function(csv_path) {
  # DK standings have two side-by-side blocks (entrants | players). Plain fread stops
  # once the shorter (player) block ends — which is fine, every player row comes first
  # so none are lost. (fill=TRUE is WRONG here: it reads the full 70k-row entrant list
  # and mis-detects the header off the blank inter-block column.)
  raw <- fread(csv_path, check.names = FALSE, na.strings = c("", "NA"))
  nm  <- names(raw)
  pick <- function(c) { hit <- nm[tolower(trimws(nm)) %in% c]; if (length(hit)) hit[1] else NA_character_ }
  col_player <- pick(c("player")); col_pct <- pick(c("%drafted", "drafted", "% drafted", "pct_drafted"))
  col_pos <- pick(c("roster position", "position", "roster_position"))
  # DK standings have TWO score columns side-by-side: "Points" = each ENTRANT's total
  # lineup score (left block), "FPTS" = each PLAYER's fantasy points (right block, the
  # one we want). Prefer FPTS; only fall back to Points if there's no separate FPTS.
  col_fpts <- pick(c("fpts")); if (is.na(col_fpts)) col_fpts <- pick(c("points"))
  if (is.na(col_player) || is.na(col_pct))
    stop("Not a DK Contest Standings export (need Player + %Drafted): ", basename(csv_path))
  d <- raw[, c(col_player, col_pct, col_pos, col_fpts)[!is.na(c(col_player, col_pct, col_pos, col_fpts))], with = FALSE]
  setnames(d, c(col_player, col_pct), c("player_name", "pct_drafted"))
  if (!is.na(col_pos))  setnames(d, col_pos, "dk_position")
  if (!is.na(col_fpts)) setnames(d, col_fpts, "fpts")
  d <- d[!is.na(player_name) & nchar(trimws(player_name)) > 0]
  p <- suppressWarnings(as.numeric(gsub("%", "", as.character(d$pct_drafted))))
  # %Drafted is a percentage (0-100). Only rescale if the values are clearly 0-1
  # FRACTIONS (MAX <= 1). Using the median wrongly fired on large-field slates where
  # most players are legitimately <1% owned, inflating actual ownership ~100x.
  if (isTRUE(max(p, na.rm = TRUE) <= 1)) p <- p * 100
  d[, pct_drafted := p][, norm := norm_name(player_name)]
  unique(d, by = "norm")
}

resolve_player_ids <- function(d, sport) {
  ref <- tryCatch(db_query("SELECT player_id, name FROM players WHERE sport = ?", list(sport)),
                  error = function(e) data.frame())
  d[, player_id := NA_real_]
  if (nrow(ref)) { ref <- as.data.table(ref)[, norm := norm_name(name)]; ref <- unique(ref, by = "norm")
    d[ref, on = "norm", player_id := i.player_id] }
  miss <- is.na(d$player_id); if (any(miss)) d[miss, player_id := surrogate_player_id(norm)]
  d
}

# Which slate_id for (sport, date) do our PROJECTIONS actually live under? Multi-layout
# sports (nfl, ncaaf) no longer have a slate tagged plain "main" — dk_slate_options()
# tags them main1/main2/showdown<n> — so logging actuals against "main" produced an
# orphan slate_id that joins to nothing. That silently starved the NFL/NCAAF ownership
# models: ownership_training_data() inner-joins actuals to projections+salaries, so every
# captured %Drafted row was discarded and predict_ownership() fell back to the value-rank
# heuristic forever. Pick the layout whose player pool best matches this contest's CSV.
.resolve_logged_slate <- function(sport, date, slate, site, player_ids) {
  want <- make_slate_id(sport, site, date, slate)
  cand <- tryCatch(as.data.table(db_query(sprintf(
    "SELECT DISTINCT slate_id FROM salaries WHERE slate_id LIKE '%s-%s-%s-%%'",
    sport, site, format(as.Date(date), "%Y-%m-%d")))), error = function(e) NULL)
  if (is.null(cand) || !nrow(cand)) return(want)
  if (want %in% cand$slate_id) return(want)                  # single-layout sport: unchanged
  pid <- unique(player_ids[!is.na(player_ids)])
  if (!length(pid)) return(want)
  best <- NULL; best_n <- -1L
  for (sid in cand$slate_id) {
    have <- tryCatch(db_query(sprintf(
      "SELECT DISTINCT player_id FROM salaries WHERE slate_id='%s'", sid))$player_id,
      error = function(e) NULL)
    n <- length(intersect(pid, have))
    if (n > best_n) { best_n <- n; best <- sid }
  }
  if (is.null(best) || best_n <= 0) return(want)
  msg(sprintf("  ownership: '%s' has no slate; attaching to %s (%d/%d players matched)",
              slate, best, best_n, length(pid)))
  best
}

# Log one DK standings CSV -> raw landing (always) + ownership table (resolved).
log_ownership_csv <- function(csv, sport, date, slate = "main", contest = NULL,
                              type = NA, fee = NA, field = NA, site = "dk") {
  stopifnot(file.exists(csv))
  if (is.null(contest) || is.na(contest)) contest <- sub("\\.csv$", "", basename(csv))
  slate_id <- make_slate_id(sport, site, date, slate)
  cap <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  d <- parse_dk_standings(csv); msg("Parsed", nrow(d), "players from", basename(csv))

  land <- dfs_path("data", "ownership"); dir.create(land, showWarnings = FALSE, recursive = TRUE)
  fwrite(copy(d)[, `:=`(sport = sport, slate_id = slate_id, contest_id = as.character(contest), captured_ts = cap)],
         file.path(land, paste0(gsub("[^A-Za-z0-9]+", "_", contest), ".csv")))

  d <- resolve_player_ids(d, sport)
  # NOTE: the "(n resolved)" count logged below counts only players matched by NAME against
  # the `players` table. A 0 there is NOT a failure — resolve_player_ids() falls back to
  # surrogate_player_id(norm), a deterministic NEGATIVE id derived from the normalized
  # name, and the slate pool assigns ids the same way (dk_contest.R), so the two sides
  # still join. Don't gate importing on that count.
  # re-point at the layout our projections/salaries actually used (see above); the raw
  # landing CSV above keeps the as-requested id, this only affects the joinable table
  slate_id <- .resolve_logged_slate(sport, date, slate, site, d$player_id)
  own <- as.data.table(data.frame(slate_id = slate_id, sport = sport, player_id = d$player_id,
           contest_id = as.character(contest), projected_pct = NA_real_,
           actual_pct = d$pct_drafted, captured_ts = cap))[
           , .(projected_pct = NA_real_, actual_pct = max(actual_pct, na.rm = TRUE), captured_ts = cap[1]),
             by = .(slate_id, sport, player_id, contest_id)]
  db_upsert("ownership", own, keys = c("slate_id", "player_id", "contest_id"))
  db_upsert("contest_results", data.frame(contest_id = as.character(contest), slate_id = slate_id,
    sport = sport, site = site, name = slate, contest_type = if (is.na(type)) NA_character_ else type,
    entry_fee = suppressWarnings(as.numeric(fee)), field_size = suppressWarnings(as.integer(field)),
    max_entries = NA_integer_, total_prizes = NA_real_, payout_curve_id = NA_character_,
    cash_line = NA_real_, captured_ts = cap), keys = "contest_id")
  n_res <- sum(d$player_id > 0)
  msg(sprintf("Logged ownership: %d players (%d resolved) -> %s", nrow(own), n_res, slate_id))
  invisible(list(slate_id = slate_id, contest_id = contest, n = nrow(own)))
}

# AUTO-IMPORT: scan an inbox folder for DK standings CSVs, derive sport/date/fee/
# field from the contest id in each filename (via the contest API), log, and move
# the file to processed/. Minimal-manual: just drop the downloaded CSV in the folder.
auto_import_ownership <- function(dir = dfs_path("data", "ownership_inbox")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  if (!length(files)) { msg("ownership inbox empty:", dir); return(invisible(0L)) }
  done <- file.path(dir, "processed"); dir.create(done, showWarnings = FALSE)
  n <- 0L
  for (f in files) {
    cid <- gsub("\\D", "", basename(f))
    if (!nchar(cid)) { msg("  skip (no contest id in filename):", basename(f)); next }
    ct <- tryCatch(dk_contest(cid), error = function(e) NULL)
    sport <- if (!is.null(ct)) infer_sport_from_contest(ct) else ""
    if (is.na(sport) || sport == "" || sport %in% c("mlb","baseball","softball")) {
      msg("  could not infer a valid sport for", basename(f)); next }
    # Log against the day's MAIN slate (not a contest-specific slate) so actual
    # %Drafted joins the projected ownership + salaries on (slate_id, player_id);
    # contest_id still distinguishes each contest. (Showdown sub-slates are the one
    # case this over-attaches — those players just won't match the main pool.)
    ok <- tryCatch({ log_ownership_csv(f, sport = sport,
            date = if (!is.null(ct)) ct$date else Sys.Date(), slate = "main", contest = cid,
            type = "gpp", fee = if (!is.null(ct)) ct$entry_fee else NA,
            field = if (!is.null(ct)) ct$field_size else NA); TRUE },
          error = function(e) { msg("  import failed:", conditionMessage(e)); FALSE })
    if (ok) { file.rename(f, file.path(done, basename(f))); n <- n + 1L }
  }
  msg("auto-imported", n, "ownership file(s)")
  invisible(n)
}
