# ==============================================================================
# DFS ENGINE — portfolio REPORT CARD + build alerts (2026-11-13)
#
# Diagnostics in the style MobyWhaler posts for his own 150-lineup builds, attached to every
# build's pool feed (pool_*.json -> `report`) and printed to the build log:
#   * selection DEPTH   - where the picks sit in the projection-ranked candidate pool
#                         (avg / median / worst rank, first skipped). Picks reaching very deep
#                         usually means caps/mins are binding hard.
#   * positions used    - distinct QBs / DSTs (etc.) across the set
#   * overlap profile   - how many lineup PAIRS share 0..n players
#   * leverage / fades  - exposure minus projected ownership, top 5 each way
#   * shapes            - sport hook rr$shape (NFL: naked QB, QB+2, bring-back, RB+own DST)
#   * vacated bumps     - teammates boosted by an injury (NFL vacated.R)
#   * second opinion    - our projection vs an outside source (NFL: Sleeper), big gaps only
# and two build ALERTS:
#   * stale lineups     - lineups published by an EARLIER build of this slate that contain a
#                         player who has since left the pool (ruled out / scratched)
#   * provisional       - built on an earlier day than the slate (Thursday sims move a lot by
#                         Sunday); the feed carries `provisional: true`
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

.lu_key <- function(ix) paste(sort(ix), collapse = "-")

portfolio_report <- function(r, top_k = 5L) {
  picks <- r$picks; if (!length(picks)) return(NULL)
  pool <- as.data.table(r$pool); res <- r$res; n <- length(picks)
  nm <- pool$player_name; pos <- toupper(as.character(pool$position %||% rep(NA, nrow(pool))))
  ix_list <- lapply(picks, function(p) p$idx[[1]])
  # --- depth in the proj-ranked candidate pool -------------------------------------------
  depth <- NULL
  if (!is.null(res) && nrow(res) && "idx" %in% names(res)) {
    ranked <- res[order(-proj)]; rk <- match(vapply(ix_list, .lu_key, ""), vapply(ranked$idx, .lu_key, ""))
    got <- sort(rk[!is.na(rk)])
    depth <- list(pool_size = nrow(res), avg_rank = if (length(got)) round(mean(got)) else NA,
                  median_rank = if (length(got)) stats::median(got) else NA,
                  worst_rank = if (length(got)) max(got) else NA,
                  first_skipped = if (length(got)) setdiff(seq_len(max(got) + 1L), got)[1] else NA,
                  direct_solves = sum(is.na(rk)))
  }
  # --- exposure, leverage, fades -----------------------------------------------------------
  ex  <- tabulate(unlist(ix_list), nbins = nrow(pool)) / n
  own <- if ("own" %in% names(pool)) pool$own else rep(NA_real_, nrow(pool))
  row <- function(j) list(name = nm[j], pos = pos[j], exposure = round(ex[j], 3), own = round(own[j], 3),
                          lev = round(ex[j] - own[j], 3))
  used <- which(ex > 0)
  most <- used[order(-ex[used])][seq_len(min(top_k, length(used)))]
  levj <- if (all(is.na(own))) integer(0) else { u <- used[is.finite(own[used])]; u[order(-(ex[u] - own[u]))][seq_len(min(top_k, length(u)))] }
  fadej <- if (all(is.na(own))) integer(0) else { f <- which(is.finite(own) & own >= 0.08); f[order(ex[f] - own[f])][seq_len(min(top_k, length(f)))] }
  # --- positions used / overlap profile -----------------------------------------------------
  used_by_pos <- lapply(split(unlist(ix_list), pos[unlist(ix_list)]), function(v) length(unique(v)))
  ov <- if (n > 1) { cmb <- utils::combn(n, 2); vapply(seq_len(ncol(cmb)), function(k)
          length(intersect(ix_list[[cmb[1, k]]], ix_list[[cmb[2, k]]])), integer(1)) } else integer(0)
  overlap <- if (length(ov)) as.list(table(factor(ov, levels = 0:max(ov)))) else list()
  # --- shapes (sport hook) -------------------------------------------------------------------
  shapes <- NULL
  if (is.function(r$rr$shape)) {
    S <- rbindlist(lapply(ix_list, function(ix) as.list(r$rr$shape(ix, pool))))
    shapes <- lapply(S, function(v) sum(as.numeric(v), na.rm = TRUE))
  }
  # --- injury redistribution + second opinion ----------------------------------------------
  vac <- if ("vac_bump" %in% names(pool)) {
    v <- pool[is.finite(vac_bump) & vac_bump >= 0.75][order(-vac_bump)][seq_len(min(.N, 8L))]
    if (nrow(v)) lapply(seq_len(nrow(v)), function(i) list(name = v$player_name[i], pos = v$position[i],
      bump = round(v$vac_bump[i], 1), proj = round(v$proj[i], 1),
      sim_coupled = !is.na(v$cuff_of[i] %||% NA))) else NULL } else NULL
  alt <- if ("alt_proj" %in% names(pool)) {
    a <- pool[(is.finite(alt_proj) | proj >= 6) & pmax(proj, alt_proj, na.rm = TRUE) >= 6]
    a[, alt0 := fifelse(is.finite(alt_proj), alt_proj, 0)]
    a[, gap := alt0 - proj]
    a <- a[abs(gap) >= pmax(4, 0.35 * pmax(proj, alt0))][order(-abs(gap))][seq_len(min(.N, 10L))]
    if (nrow(a)) lapply(seq_len(nrow(a)), function(i) list(name = a$player_name[i], pos = a$position[i],
      ours = round(a$proj[i], 1), other = if (is.finite(a$alt_proj[i])) round(a$alt_proj[i], 1) else NA,
      note = if (!is.finite(a$alt_proj[i])) "other source does not project him (likely out)" else NULL)) else NULL } else NULL
  list(lineups = n, depth = depth, positions_used = used_by_pos, overlap_pairs = overlap,
       most_exposed = lapply(most, row), leverage = lapply(levj, row), fades = lapply(fadej, row),
       shapes = shapes, vacated_bumps = vac, second_opinion = alt,
       hidden_gems = tryCatch(hidden_gems(res, pool), error = function(e) NULL))
}

# HIDDEN GEMS (MobyWhaler, 2026-11-12: two chalky-looking lineups whose exact player combos
# were at the 83rd percentile for uniqueness in his pool -- "could likely penetrate through to
# the top easier"). near_overlap = mean shared players with the 10 NEAREST lineups of the
# simulated field; lower = a rarer combination even if every player is popular.
combo_near_overlap <- function(cand_idx, field_idx, P, k = 10L) {
  if (!length(cand_idx) || !length(field_idx)) return(rep(NA_real_, length(cand_idx)))
  ind <- function(L) { M <- matrix(0, length(L), P); for (i in seq_along(L)) M[i, L[[i]]] <- 1; M }
  O <- ind(cand_idx) %*% t(ind(field_idx))
  k <- min(k, ncol(O))
  apply(O, 1, function(v) mean(sort(v, decreasing = TRUE)[seq_len(k)]))
}

# Top-projection candidates that are chalky by player but unique by combination.
hidden_gems <- function(res, pool, top_frac = 0.10, max_n = 5L) {
  if (is.null(res) || !"near" %in% names(res) || all(is.na(res$near))) return(NULL)
  R <- res[order(-proj)][seq_len(max(1L, ceiling(top_frac * .N)))]
  R[, uniq_pct := 1 - frank(near) / .N]                     # 1 = most unique within the top slice
  own_med <- stats::median(R$avg_own, na.rm = TRUE)
  G <- R[avg_own >= own_med & uniq_pct >= 0.80][order(-uniq_pct)][seq_len(min(.N, max_n))]
  if (!nrow(G)) return(NULL)
  lapply(seq_len(nrow(G)), function(i) list(proj = round(G$proj[i], 1), avg_own = round(G$avg_own[i], 3),
    uniqueness_pct = round(G$uniq_pct[i], 2), near_overlap = round(G$near[i], 2),
    players = pool$player_name[G$idx[[i]]]))
}

# Print a compact version of the report to the build log.
log_report <- function(rep, sport = "") {
  if (is.null(rep)) return(invisible())
  d <- rep$depth
  if (!is.null(d)) msg(sprintf("  report[%s]: picks from a %d-candidate pool -- avg rank %s, median %s, worst %s, first skipped #%s, %d direct-solve",
                               sport, d$pool_size, d$avg_rank, d$median_rank, d$worst_rank, d$first_skipped, d$direct_solves))
  if (length(rep$positions_used)) msg("  report: distinct players by position:", paste(names(rep$positions_used), unlist(rep$positions_used), sep = "=", collapse = " "))
  if (length(rep$shapes)) msg("  report: shapes:", paste(names(rep$shapes), unlist(rep$shapes), sep = "=", collapse = " "), sprintf("(of %d)", rep$lineups))
  f <- function(l) paste(vapply(l, function(x) sprintf("%s %+.0f%%", x$name, 100 * x$lev), ""), collapse = ", ")
  if (length(rep$leverage)) msg("  report: leverage:", f(rep$leverage))
  if (length(rep$fades)) msg("  report: fades:", f(rep$fades))
  if (length(rep$second_opinion)) msg("  report: big gaps vs second source:", paste(vapply(rep$second_opinion, function(x)
    sprintf("%s ours %.1f / other %s", x$name, x$ours, if (is.na(x$other)) "none" else sprintf("%.1f", x$other)), ""), collapse = "; "))
  invisible()
}

# Lineups from the most recent EARLIER build of this slate that include a player who is no
# longer in the pool (ruled out / scratched since). Reads recommended_lineups BEFORE this
# build overwrites today's rows. Returns a list (possibly empty); never errors.
stale_lineup_alerts <- function(slate_id, pool) {
  rows <- tryCatch(db_query("SELECT build_date, n, players, created_ts FROM recommended_lineups WHERE slate_id = ?",
                            list(slate_id)), error = function(e) NULL)
  if (is.null(rows) || !nrow(rows)) return(list())
  rows <- as.data.table(rows)[order(-as.character(created_ts))]
  last_ts <- rows$created_ts[1]; rows <- rows[created_ts == last_ts | build_date == rows$build_date[1]]
  live <- norm_name(pool$player_name)
  out <- list()
  for (i in seq_len(nrow(rows))) {
    pl <- tryCatch(jsonlite::fromJSON(rows$players[i]), error = function(e) NULL)
    if (is.null(pl) || !"name" %in% names(pl)) next
    gone <- pl$name[!norm_name(pl$name) %in% live]
    if (length(gone)) out[[length(out) + 1L]] <- list(lineup = rows$n[i], built = as.character(rows$created_ts[i]), players_out = gone)
  }
  if (length(out)) msg(sprintf("  ALERT: %d lineup(s) from the previous build include player(s) no longer in the pool: %s",
                               length(out), paste(unique(unlist(lapply(out, `[[`, "players_out"))), collapse = ", ")))
  out
}

# TRUE when the build is happening on an earlier (America/New_York) day than the slate.
is_provisional_build <- function(slate_date) {
  today_et <- as.Date(format(Sys.time(), tz = "America/New_York", "%Y-%m-%d"))
  isTRUE(as.Date(slate_date) > today_et)
}
