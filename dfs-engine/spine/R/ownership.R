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
  # so none are lost. (fill=TRUE is WRONG as the FIRST read here: it reads the full
  # 70k-row entrant list and mis-detects the header off the blank inter-block column.)
  #
  # BUT on Showdown-format exports the entrant block's unquoted "Lineup" text column
  # (e.g. "CPT Jaxon Smith-Njigba FLEX A.J. Brown FLEX ...") can make a row ragged well
  # BEFORE the player block ends, which makes plain fread bail out early with "Stopped
  # early on line N" and silently drop real player/FPTS rows past that point — the
  # "player rows come first" assumption above doesn't hold for Showdown exports. Detect
  # that via the warning and retry with fill=TRUE, reusing the header names the strict
  # pass already found correctly (so the retry can't mis-detect the header either).
  warned <- FALSE
  raw <- withCallingHandlers(
    fread(csv_path, check.names = FALSE, na.strings = c("", "NA")),
    warning = function(w) { if (grepl("Stopped early", conditionMessage(w))) warned <<- TRUE; invokeRestart("muffleWarning") })
  nm <- names(raw)
  if (warned) {
    # Take the player block as the TRAILING columns, parsed per line.
    #
    # The layout is: Rank,EntryId,EntryName,TimeRemaining,Points,Lineup,,Player,
    # Roster Position,%Drafted,FPTS -- the entrant block on the left, a blank spacer,
    # then the player block as the last 4 columns. Raggedness only ever comes from
    # UNQUOTED commas on the LEFT (EntryName like "foo (3/20)" or Showdown Lineup
    # text), which shifts the player block right but never off the end.
    #
    # Two earlier attempts both failed on this:
    #   col.names = nm  -> "Can't assign 11 names to a 30-column data.table" (the
    #                      strict pass only saw 11 columns before bailing).
    #   fill=Inf + skip -> parsed, but column N is a DIFFERENT field on ragged rows, so
    #                      Lineup tokens landed in Player and we hashed ids for "FLEX",
    #                      "DK", "Deshaun". That is worse than failing: it wrote
    #                      plausible garbage that then refused to join (2/31 overlap).
    # Anchoring on the END is immune to left-side shifting.
    ln <- readLines(csv_path, warn = FALSE); ln <- ln[nzchar(ln)]
    hdr <- trimws(strsplit(ln[1], ",", fixed = TRUE)[[1]])
    tailn <- utils::tail(hdr, 4L)
    if (length(tailn) == 4L && any(grepl("^player$", tailn, ignore.case = TRUE)) &&
        any(grepl("drafted", tailn, ignore.case = TRUE))) {
      parts <- lapply(ln[-1], function(x) {
        f <- strsplit(x, ",", fixed = TRUE)[[1]]
        if (length(f) < 4L) rep(NA_character_, 4L) else trimws(utils::tail(f, 4L))
      })
      raw <- as.data.table(do.call(rbind, parts))
      setnames(raw, tailn); nm <- tailn
    } else {
      # unexpected shape -- fail loudly rather than invent rows
      stop("Ragged standings export with an unrecognised trailing player block: ",
           basename(csv_path))
    }
  }
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
  if (any(is.finite(p)) && max(p[is.finite(p)]) <= 1) p <- p * 100
  d[, pct_drafted := p][, norm := norm_name(player_name)]
  unique(d, by = "norm")
}

# max() over an all-NA group returns -Inf (with a warning), not NA. Two players whose
# %Drafted failed to parse were stored as actual_pct = -Inf, which then propagated into
# every correlation and sum that touched their slate -- `sum(actual_pct)` came back -Inf
# for 13 of 15 NFL slates, silently poisoning the accuracy report rather than failing.
# Collapse to NA so a missing value stays missing.
.max_finite <- function(x) { x <- x[is.finite(x)]; if (!length(x)) NA_real_ else max(x) }

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
  # Search a DATE WINDOW, not just the exact date. The slate is built when DK posts it,
  # which is routinely the day BEFORE kickoff, while the standings capture is keyed to
  # the contest/settlement date. Confirmed live: ownership for contest 196151358 landed
  # on nfl-dk-2026-10-04-main while every salary row for those games sat under
  # nfl-dk-2026-10-03-*. The exact-date LIKE then returned zero candidates and this
  # function bailed on the line below BEFORE any matching logic ran, orphaning the
  # capture. That is why NFL ownership stayed unjoinable (and unmeasurable) even after
  # the cookie and the parser were fixed -- 7 of 7 recent NFL captures orphaned this way.
  # +/-2 days covers Thu/Sun/Mon builds without reaching a different slate of games.
  dts <- format(as.Date(date) + (-2:2), "%Y-%m-%d")
  like <- paste(sprintf("slate_id LIKE '%s-%s-%s-%%'", sport, site, dts), collapse = " OR ")
  cand <- tryCatch(as.data.table(db_query(sprintf(
    "SELECT DISTINCT slate_id FROM salaries WHERE %s", like))), error = function(e) NULL)
  if (is.null(cand) || !nrow(cand)) return(want)
  if (want %in% cand$slate_id) return(want)                  # single-layout sport: unchanged
  pid <- unique(player_ids[!is.na(player_ids)])
  if (!length(pid)) return(want)
  # A candidate slate is a plausible home only if it can actually HOST this contest:
  # nearly all of the contest's drafted players must exist in the slate's pool, AND the
  # pool must be large enough to seat them.
  #
  # The previous test scored candidates by the fraction of the CANDIDATE'S OWN pool that
  # matched (n / pool). That is direction-safe in one case only -- a 50-player Showdown
  # CSV against a 748-player main slate -- and badly wrong in the other. A 619-player
  # CLASSIC contest fully contains a 30-player Showdown pool, so it scored n/pool = 1.00,
  # the maximum possible, and beat the true ~700-player main slate's 0.88. Eleven NFL
  # captures were re-keyed onto single-game Showdown slates that way, which then compared
  # classic actuals against CPT-multiplied Showdown projections and dragged measured NFL
  # ownership correlation NEGATIVE (-0.066 median) while the correctly-keyed contests sat
  # at +0.40..+0.61. NFL ownership was never broken -- the slate keying was.
  #
  # So gate on CONTAINMENT OF THE CONTEST (n / |contest|), which is direction-safe, plus
  # the SIZE GUARD the old test lacked: a contest that drafted 619 distinct players cannot
  # be a 30-man Showdown, whatever the overlap fraction says. Among qualifying slates
  # prefer the SMALLEST pool, so a Showdown still wins over the main slate it is a
  # subset of. A Showdown game that never got its own slate built now stays correctly
  # unresolved rather than being silently mis-attached.
  MIN_COVER <- 0.60   # share of the CONTEST's players that must exist in the slate
  MIN_POOL  <- 0.90   # slate pool must be >= this x the contest's drafted-player count
  # Try the EXACT requested date's candidates first, and only widen to the +/-2 window
  # if none of them qualify. The NFL player universe barely changes week to week, so a
  # WRONG-DATE main slate (e.g. the week before) also scores high cover against this
  # contest's players -- confirmed live: a 2026-09-15 contest's true home (main1, pool
  # 725, cover .868) lost the smallest-pool tiebreak to 2026-09-14's OWN main2 (pool
  # 602, cover .767), a different week's games entirely, because both independently
  # cleared the gate and the wrong one happened to be smaller. Scoping to same-date
  # candidates first removes the ambiguity before the tiebreak ever has to run.
  exact_dt <- format(as.Date(date), "%Y-%m-%d")
  cand_exact <- cand[grepl(sprintf("^%s-%s-%s-", sport, site, exact_dt), slate_id)]
  .pick_best <- function(sids) {
    best <- NULL; best_pool <- Inf; best_n <- 0L; best_cover <- 0
    for (sid in sids) {
      have <- tryCatch(db_query(sprintf(
        "SELECT DISTINCT player_id FROM salaries WHERE slate_id='%s'", sid))$player_id,
        error = function(e) NULL)
      if (is.null(have) || !length(have)) next
      n <- length(intersect(pid, have)); cover <- n / length(pid)
      if (n < 8L || cover < MIN_COVER) next                    # contest not contained here
      if (length(have) < MIN_POOL * length(pid)) next           # pool too small to seat it
      if (length(have) < best_pool) {
        best <- sid; best_pool <- length(have); best_n <- n; best_cover <- cover
      }
    }
    list(best = best, pool = best_pool, n = best_n, cover = best_cover)
  }
  r <- .pick_best(cand_exact$slate_id)
  if (is.null(r$best) && nrow(cand_exact) < nrow(cand)) r <- .pick_best(cand$slate_id)
  if (is.null(r$best)) return(want)
  msg(sprintf("  ownership: '%s' has no slate; attaching to %s (%d/%d of the contest's players, slate pool %d)",
              slate, r$best, r$n, length(pid), r$pool))
  r$best
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
           , .(projected_pct = NA_real_, actual_pct = .max_finite(actual_pct), captured_ts = cap[1]),
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
