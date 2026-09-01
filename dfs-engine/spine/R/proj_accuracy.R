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
projection_actuals <- function(min_frac = 0.40, min_match = 8L) {
  sdir <- dfs_path("data", "ownership_inbox", "processed")
  sl <- tryCatch(as.data.table(db_query(
    "SELECT DISTINCT sport, slate_id, contest_id FROM ownership WHERE contest_id <> '_projected'")),
    error = function(e) NULL)
  if (is.null(sl) || !nrow(sl)) return(NULL)
  keys <- unique(sl[, .(sport, slate_id)])
  rbindlist(lapply(seq_len(nrow(keys)), function(i) {
    sp <- keys$sport[i]; sid <- keys$slate_id[i]
    pr <- slate_projection(sid); if (!nrow(pr)) return(NULL)
    best <- NULL; bestn <- 0L
    for (cid in sl[sport == sp & slate_id == sid, contest_id]) {
      f <- file.path(sdir, sprintf("contest-standings-%s.csv", cid)); if (!file.exists(f)) next
      a <- actual_player_points(f, sp); if (is.null(a) || !nrow(a)) next
      m <- merge(pr, a, by = "player_id")
      if (nrow(m) >= min_frac * nrow(a) && nrow(m) > bestn) { best <- m; bestn <- nrow(m) }  # best well-matching contest
    }
    if (is.null(best) || nrow(best) < min_match) return(NULL)                                 # no matching contest -> skip
    best[, `:=`(sport = sp, slate_id = sid)]; best
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
