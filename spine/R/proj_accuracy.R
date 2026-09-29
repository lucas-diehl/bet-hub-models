# ==============================================================================
# DFS ENGINE — projection ACCURACY scorecard (the SaberSim/Stokastic discipline)
# The single metric that decides whether the engine can win: how close our projected
# DK points are to what players ACTUALLY scored. Joins every archived projection to the
# real result (from downloaded standings) and reports, per sport:
#   corr  — rank skill (are the right players on top?)     higher is better
#   rmse  — typical error magnitude in DK points
#   bias  — mean(proj - actual); + means we systematically OVER-project (inflates the
#           sim, cash lines and EV — the silent killer)
#   salary_corr — a naive baseline (does our model beat just using salary?)
# Everything is recomputed from raw each run, so it is always current and grows as more
# slates settle. Use projection_misses(sport) to see WHICH players we most mis-projected.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# actual player DK points from a settled standings CSV, resolved to OUR player_id
# (same resolver the ownership import uses, so it joins our stored projections).
actual_player_points <- function(csv_path, sport) {
  d <- tryCatch(parse_dk_standings(csv_path), error = function(e) NULL)
  if (is.null(d) || !nrow(d) || !"fpts" %in% names(d)) return(NULL)
  d <- resolve_player_ids(d[!is.na(fpts)], sport)
  d[!is.na(player_id), .(player_id, actual = fpts, act_own = pct_drafted)]
}

# our archived projection (latest version) for a slate, joined to salary
slate_projection <- function(slate_id) {
  p <- as.data.table(db_query(sprintf(
    "SELECT player_id, proj_mean AS proj, ceil, version_ts FROM projections WHERE slate_id='%s'", slate_id)))
  if (!nrow(p)) return(p)
  p <- p[order(version_ts)][, .SD[.N], by = player_id]           # newest per player
  s <- tryCatch(as.data.table(db_query(sprintf(
    "SELECT player_id, salary FROM salaries WHERE slate_id='%s'", slate_id))), error = function(e) NULL)
  if (!is.null(s) && nrow(s)) p <- merge(p, unique(s, by = "player_id"), by = "player_id", all.x = TRUE)
  if (!"salary" %in% names(p)) p[, salary := NA_real_]
  p[, .(player_id, proj, ceil, salary)]
}

# join projections to actuals across every settled slate -> tidy per-player table.
# A slate can have SEVERAL entered contests (some a different sub-slate entirely, e.g. a
# specific-games contest that shares almost no players with our main pool). We pick the
# contest whose players best MATCH our projection and reject weak matches, so a mismatched
# contest never pollutes the accuracy metric. `min_frac` = share of the contest's players
# our projection must cover; `min_match` = absolute floor.
# ── SCORING-SCALE GUARD ───────────────────────────────────────────────────────
# The roster-overlap test below (`min_frac`) catches a contest drawn from a
# DIFFERENT PLAYER POOL. It cannot catch a contest drawn from the SAME pool but
# scored on a different NUMBER OF ROUNDS -- and in golf that is the common case:
# DK runs "PGA TOUR Showdown ... (Round 2 TOUR)" on the same 150 golfers as the
# 4-round main event. Grading a 4-round projection (65-102 pts) against 1-round
# actuals (15-25 pts) passed every existing guard and produced a completely fake
# scorecard: 1.95x "over-projection" and corr 0.06 across all three settled golf
# slates, which is what the handoff doc's "golf runs ~2x too high" was built on.
# Nothing was wrong with the model.
#
# So classify BOTH sides by how many rounds they score and require them to agree.
# Unknown-vs-known is rejected, not assumed compatible: a missing measurement is
# recoverable, a confidently wrong one sends us fixing a model that is fine.
.scale_class_contest <- function(name) {
  n <- tolower(name %||% "")
  if (!nzchar(n)) return(NA_character_)
  # Cup FIRST: "Presidents Cup Showdown" is match-play scoring, not a round slate,
  # and testing "showdown" before "cup" would file it as one.
  if (grepl("presidents cup|ryder cup", n)) return("cup")      # match play, own scale
  # DK golf single-round products: "Showdown", "Captain", or an explicit "Round N"
  if (grepl("showdown|captain|\\bround\\s*[1-4]\\b|\\br[1-4]\\s*tour\\b", n)) return("round")
  "event"
}
.scale_class_slate <- function(slate_id) {
  s <- tolower(slate_id %||% "")
  if (grepl("presidents|cup", s)) return("cup")
  if (grepl("golf-round|captain|showdown|_r[1-4]\\b", s)) return("round")
  "event"
}

.slate_date <- function(slate_id) {
  m <- regmatches(slate_id, regexpr("\\d{4}-\\d{2}-\\d{2}", slate_id))
  if (!length(m)) return(as.Date(NA)) else as.Date(m)
}

# Re-point a scale-mismatched contest at the RIGHT slate instead of discarding it.
# `ownership` links a contest to whichever slate was being built when the standings
# were imported, which for golf is routinely the `-main` tournament slate even when
# the contest is a Round-2 showdown. But we DO archive round-scale projections under
# their own `-golf-round` slate id, so the correct comparison usually exists -- it is
# just filed under a sibling slate. Look for one of the contest's own scale on the
# same date (a round contest is drafted the day before it is played, so allow +/-1).
# NOTE: args are `sport_x` / `scale_x`, NOT `sport` / `scale` -- those are COLUMN
# names in all_slates, and data.table would compare the column to itself (always
# TRUE) rather than to the argument. Same shadowing trap that made an earlier
# starter-resolver silently match every row.
.slate_for_scale <- function(all_slates, sport_x, near_date, scale_x) {
  if (!nrow(all_slates) || is.na(near_date)) return(character(0))
  cand <- all_slates[sport == sport_x & scale == scale_x & !is.na(sdate)]
  if (!nrow(cand)) return(character(0))
  cand <- cand[abs(as.numeric(sdate - near_date)) <= 1]
  if (!nrow(cand)) return(character(0))
  cand[order(abs(as.numeric(sdate - near_date))), slate_id]
}

projection_actuals <- function(min_frac = 0.40, min_match = 8L, strict_scale = TRUE) {
  sdir <- dfs_path("data", "ownership_inbox", "processed")
  sl <- tryCatch(as.data.table(db_query(
    "SELECT DISTINCT sport, slate_id, contest_id FROM ownership WHERE contest_id <> '_projected'")),
    error = function(e) NULL)
  if (is.null(sl) || !nrow(sl)) return(NULL)
  # contest names, for the scale guard (best-effort: absent name -> unknown class)
  cn <- tryCatch(as.data.table(db_query(
    "SELECT contest_id, name FROM contest_results")), error = function(e) NULL)
  cmap <- if (!is.null(cn) && nrow(cn)) setNames(cn$name, as.character(cn$contest_id)) else character(0)
  # every slate that has archived projections, with its date + scale class, so a
  # mismatched contest can be re-pointed at a correctly-scaled sibling slate
  allsl <- tryCatch(as.data.table(db_query(
    "SELECT DISTINCT sport, slate_id FROM projections")), error = function(e) NULL)
  if (!is.null(allsl) && nrow(allsl)) {
    allsl[, sdate := .slate_date(slate_id), by = slate_id]
    allsl[, scale := .scale_class_slate(slate_id), by = slate_id]
  } else allsl <- data.table(sport = character(0), slate_id = character(0),
                             sdate = as.Date(character(0)), scale = character(0))

  # Re-pointing means a slate's projections can be asked for from several contests
  # (and from other slates' loops), so memoize -- without this the DB is queried once
  # per contest instead of once per slate and the scorecard stops finishing.
  .prcache <- new.env(parent = emptyenv())
  slate_projection_cached <- function(x) {
    k <- as.character(x)
    if (!is.null(.prcache[[k]])) return(.prcache[[k]])
    v <- tryCatch(slate_projection(x), error = function(e) data.table())
    assign(k, v, envir = .prcache); v
  }

  keys <- unique(sl[, .(sport, slate_id)])
  rbindlist(lapply(seq_len(nrow(keys)), function(i) {
    sp <- keys$sport[i]; sid <- keys$slate_id[i]
    sdte <- .slate_date(sid)
    best <- NULL; bestn <- 0L; best_sid <- sid
    for (cid in sl[sport == sp & slate_id == sid, contest_id]) {
      f <- file.path(sdir, sprintf("contest-standings-%s.csv", cid)); if (!file.exists(f)) next
      use_sid <- sid
      if (strict_scale) {
        got <- .scale_class_contest(cmap[[as.character(cid)]] %||% NA_character_)
        if (is.na(got)) {
          msg(sprintf("  accuracy: skipping contest %s (no name -> scoring scale unverifiable)", cid))
          next
        }
        if (!identical(got, .scale_class_slate(sid))) {
          alt <- .slate_for_scale(allsl, sp, sdte, got)
          if (!length(alt)) {
            msg(sprintf("  accuracy: skipping contest %s ('%s' scale) - no '%s'-scale slate archived near %s",
                        cid, got, got, as.character(sdte)))
            next
          }
          use_sid <- alt[1]
          msg(sprintf("  accuracy: contest %s is '%s' scale -> grading against %s (not %s)",
                      cid, got, use_sid, sid))
        }
      }
      pr <- slate_projection_cached(use_sid); if (!nrow(pr)) next
      a <- actual_player_points(f, sp); if (is.null(a) || !nrow(a)) next
      m <- merge(pr, a, by = "player_id")
      if (nrow(m) >= min_frac * nrow(a) && nrow(m) > bestn) {
        best <- m; bestn <- nrow(m); best_sid <- use_sid                 # best well-matching contest
      }
    }
    if (is.null(best) || nrow(best) < min_match) return(NULL)            # no matching contest -> skip
    best[, `:=`(sport = sp, slate_id = best_sid)]; best
  }), fill = TRUE)
}

# the scorecard: per-sport accuracy + calibration + naive-baseline comparison
projection_accuracy <- function(print = TRUE) {
  M <- projection_actuals()
  if (is.null(M) || !nrow(M)) { if (print) msg("No joined projection/actual data yet (need settled slates with stored projections)."); return(invisible(NULL)) }
  acc <- M[, .(slates = uniqueN(slate_id), n = .N,
    corr = round(cor(proj, actual), 2),
    rmse = round(sqrt(mean((proj - actual)^2)), 1),
    mae  = round(mean(abs(proj - actual)), 1),
    bias = round(mean(proj - actual), 1),                       # + = OVER-project
    over_pct = round(100 * mean(proj - actual) / max(mean(actual), 1e-6), 0),
    salary_corr = round(if (sum(!is.na(salary)) > 2) suppressWarnings(cor(salary, actual, use = "complete.obs")) else NA_real_, 2)),
    by = sport][order(-n)]
  if (print) {
    cat("\n===== PROJECTION ACCURACY — proj vs ACTUAL DK points =====\n")
    cat("corr=rank skill (higher better) | bias += we OVER-project (inflates sim/EV) | salary_corr=naive baseline\n\n")
    print(acc)
    beat <- acc[!is.na(salary_corr) & corr > salary_corr, sport]
    if (length(beat)) cat("\nbeats the salary baseline:", paste(beat, collapse = ", "), "\n")
  }
  invisible(list(by_sport = acc, data = M))
}

# WHICH players we most mis-projected for a sport (names via the players dimension).
projection_misses <- function(sport, n = 12L) {
  spx <- sport
  M <- projection_actuals(); if (is.null(M)) return(invisible(NULL))
  M <- M[sport == spx]; if (!nrow(M)) { msg("no data for", spx); return(invisible(NULL)) }
  nm <- tryCatch(as.data.table(db_query("SELECT player_id, name FROM players WHERE sport = ?", list(spx))),
                 error = function(e) NULL)
  if (!is.null(nm) && nrow(nm)) M <- merge(M, unique(nm, by = "player_id"), by = "player_id", all.x = TRUE)
  if (!"name" %in% names(M)) M[, name := as.character(player_id)]
  M[, err := proj - actual]
  cat(sprintf("\n== %s: biggest OVER-projections (proj >> actual) ==\n", toupper(sport)))
  print(head(M[order(-err), .(name, proj = round(proj, 1), actual = round(actual, 1), err = round(err, 1), slate_id)], n))
  cat(sprintf("\n== %s: biggest UNDER-projections (actual >> proj) ==\n", toupper(sport)))
  print(head(M[order(err), .(name, proj = round(proj, 1), actual = round(actual, 1), err = round(err, 1), slate_id)], n))
  invisible(M)
}
