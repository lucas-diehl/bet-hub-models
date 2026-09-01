# ==============================================================================
# DFS ENGINE — loss-attribution diagnostic (the "why did we lose?" autopsy)
# Good projections don't win GPPs — beating the FIELD does. This decomposes past
# results, per sport, into the levers that actually drive ROI so we fix the right
# one instead of guessing:
#   proj_corr  — projection rank skill vs ACTUAL DK points (is the projection wrong?)
#   own_corr   — did we predict CHALK? (projected vs actual ownership; leverage input)
#   chalk_gap  — our lineups' actual ownership MINUS the field's (>0 = we're chalkier
#                than opponents -> we tie the pack; the #1 GPP leak)
#   avg/best_pctile — where our lineups land vs a realistic field on the ACTUAL
#                outcome (0.5 = mid-pack; ~0.999 needed to win a large GPP)
# Reuses projection_actuals() (proj+actual+actual-own per settled slate), the stored
# recommended_lineups, and the validated optimizer-based field sim. Read-only.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# read-only query (concurrent-safe; avoids the exclusive-lock self-conflict that
# db_query's read-write connection hits when called many times in one process).
.dq <- function(sql) as.data.table(with_db(function(con) DBI::dbGetQuery(con, sql), read_only = TRUE))

# per-sport autopsy over every settled slate we both projected AND built lineups for.
.autopsy_sport <- function(sport, M_all, proj_own, field_n = 800L) {
  dfs_load_sport(sport); rr <- get_sport(sport)$roster_rules
  spx <- sport; M <- M_all[sport == spx]                          # spx avoids the col/arg name clash
  if (!nrow(M)) return(NULL)
  NM <- .dq(sprintf("SELECT player_id, name FROM players WHERE sport='%s'", sport))
  RL <- .dq(sprintf("SELECT slate_id, players FROM recommended_lineups WHERE sport='%s'", sport))
  slates <- intersect(unique(M$slate_id), unique(RL$slate_id))
  rows <- list()
  for (sid in slates) {
    m  <- M[slate_id == sid]
    sa <- .dq(sprintf("SELECT player_id, position, team, game_id FROM salaries WHERE slate_id='%s'", sid))
    pool <- merge(m, unique(sa, by = "player_id"), by = "player_id", all.x = TRUE)
    pool <- merge(pool, proj_own[slate_id == sid, .(player_id, proj_own = projected_pct)], by = "player_id", all.x = TRUE)
    pool <- merge(pool, unique(NM, by = "player_id"), by = "player_id", all.x = TRUE)
    pool <- pool[is.finite(proj) & is.finite(actual) & is.finite(salary) & salary > 0]
    if (nrow(pool) < rr$n + 3) next
    pool[, own := pmax(fcoalesce(act_own, 0), 1e-3) / 100]        # ACTUAL ownership (0-100 -> frac)
    pool[, nkey := norm_name(name)]
    rl <- RL[slate_id == sid]
    ours <- lapply(rl$players, function(pj) {
      pl <- tryCatch(jsonlite::fromJSON(pj), error = function(e) NULL)
      if (is.null(pl) || !nrow(pl)) return(NULL)
      ix <- match(norm_name(pl$name), pool$nkey); ix[!is.na(ix)] })
    ours <- ours[vapply(ours, function(x) length(x) == rr$n, logical(1))]
    if (!length(ours)) next
    our_tot <- vapply(ours, function(ix) sum(pool$actual[ix]), 0)
    our_own <- vapply(ours, function(ix) mean(pool$act_own[ix]), 0)
    fld <- simulate_field(pool, rr, field_n = field_n, own = pool$own)   # realistic field, ACTUAL own
    fld_tot <- vapply(fld$idx, function(ix) sum(pool$actual[ix]), 0)     # scored on ACTUAL points
    our_pct <- vapply(our_tot, function(t) mean(fld_tot < t), 0)
    rows[[length(rows) + 1]] <- data.table(slate = sid,
      proj_corr = suppressWarnings(cor(pool$proj, pool$actual)),
      own_corr  = suppressWarnings(cor(pool$proj_own, pool$act_own, use = "complete.obs")),
      our_own = mean(our_own), field_own = 100 * mean(field_ownership(fld, nrow(pool))),
      avg_pctile = mean(our_pct), best_pctile = max(our_pct))
  }
  A <- rbindlist(rows); if (!nrow(A)) return(NULL)
  data.table(sport = sport, slates = nrow(A),
    proj_corr = round(mean(A$proj_corr, na.rm = TRUE), 3),
    own_corr  = round(mean(A$own_corr,  na.rm = TRUE), 3),
    our_own   = round(mean(A$our_own), 1), field_own = round(mean(A$field_own), 1),
    chalk_gap = round(mean(A$our_own) - mean(A$field_own), 1),
    avg_pctile = round(mean(A$avg_pctile), 3), best_pctile = round(mean(A$best_pctile), 3))
}

# main entry: run the autopsy for each sport and print an attributed verdict.
loss_autopsy <- function(sports = c("golf", "wnba", "tennis"), field_n = 800L, print = TRUE) {
  M_all <- tryCatch(as.data.table(projection_actuals()), error = function(e) NULL)
  if (is.null(M_all) || !nrow(M_all)) { if (print) msg("loss_autopsy: no settled proj/actual data yet."); return(invisible(NULL)) }
  proj_own <- .dq("SELECT slate_id, player_id, projected_pct FROM ownership WHERE contest_id='_projected'")
  res <- rbindlist(lapply(sports, function(s)
    tryCatch(.autopsy_sport(s, M_all, proj_own, field_n), error = function(e) { msg("autopsy", s, "error:", conditionMessage(e)); NULL })), fill = TRUE)
  if (!nrow(res)) { if (print) msg("loss_autopsy: no slates with both projections and recommended lineups."); return(invisible(NULL)) }
  if (print) {
    cat("\n===== LOSS AUTOPSY — why GPP ROI lags (per sport) =====\n")
    cat("proj_corr=projection rank skill | own_corr=predict chalk | chalk_gap=our own MINUS field's (>0 = too chalky)\n")
    cat("avg/best_pctile=our lineups vs a realistic field on ACTUAL outcomes (~0.999 needed to win a large GPP)\n\n")
    print(res, row.names = FALSE)
    cat("\n-- verdict --\n")
    for (i in seq_len(nrow(res))) { r <- res[i]; flags <- character(0)
      if (isTRUE(r$chalk_gap > 3))   flags <- c(flags, sprintf("CHALK-DUPLICATION (+%.1f%% vs field) -> leverage/uniqueness (P2)", r$chalk_gap))
      if (isTRUE(r$own_corr < 0.55)) flags <- c(flags, sprintf("OWNERSHIP WEAK (%.2f) -> tune ownership (P1)", r$own_corr))
      if (isTRUE(r$best_pctile < 0.9)) flags <- c(flags, sprintf("CEILING SHORT (best %.2f) -> higher-variance/ceiling builds", r$best_pctile))
      cat(sprintf("  %-7s %s\n", r$sport, if (length(flags)) paste(flags, collapse = "; ") else "no dominant leak flagged"))
    }
  }
  invisible(res)
}
