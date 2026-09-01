# ==============================================================================
# DFS ENGINE — pipeline orchestration (shared by run_sport.R and daily_report.R)
# run_slate(): project -> correlate -> slate sim -> field sim -> candidates ->
# EV grade -> portfolio, returning everything downstream consumers need.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# Persist the slate's projections (versioned) so the web app + audits have history.
persist_projections <- function(pool, slate_id, sport, source = "model") {
  pool <- as.data.table(pool)
  if (!"player_id" %in% names(pool) || !nrow(pool)) return(invisible(0L))
  g <- function(col, default = NA_real_) if (col %in% names(pool)) pool[[col]] else rep(default, nrow(pool))
  df <- data.frame(slate_id = slate_id, sport = sport, player_id = pool$player_id,
    version_ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), source = source,
    proj_mean = g("proj"), sim_sd = g("sim_sd"), ceil = g("ceil"),
    floor = g("floor"), p_zero = g("p_zero"))
  db_upsert("projections", df, keys = c("slate_id", "player_id", "version_ts", "source"))
}

# Archive the recommended lineups (one immutable row per lineup per build-day) so the
# app can show history and score the picks against actual results later.
persist_recommended_lineups <- function(picks, pool, slate_id, sport, date) {
  if (!length(picks)) return(invisible(0L))
  P <- as.data.table(pool); bd <- as.character(as.Date(date))
  ceilcol <- if ("ceil" %in% names(P)) P$ceil else P$proj
  rows <- rbindlist(lapply(seq_along(picks), function(i) {
    p <- picks[[i]]; ix <- p$idx[[1]]
    players <- lapply(ix, function(j) list(name = P$player_name[j],
      pos = (P$position[j] %||% ""), salary = as.integer(P$salary[j]),
      proj = round(P$proj[j], 1), own = round((if ("own" %in% names(P)) P$own[j] else 0), 4)))
    data.table(lineup_uid = paste(slate_id, bd, i, sep = "|"), slate_id = slate_id,
      sport = sport, build_date = bd, n = i, role = p$role %||% "",
      salary = as.integer(p$salary), proj = round(p$proj, 1),
      ceil = round(sum(ceilcol[ix]), 1), own = round(p$avg_own %||% 0, 4),
      reason = p$reason %||% "", captain = NA_character_,
      players = jsonlite::toJSON(players, auto_unbox = TRUE),
      created_ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  }), fill = TRUE)
  db_upsert("recommended_lineups", as.data.frame(rows), keys = "lineup_uid")
}

run_slate <- function(sport, date = Sys.Date(), slate = "main", site = "dk",
                      n_sims = 10000L, n_cand = 400L, n_lineups = 8L, field_n = 1500L,
                      seed = 2026L, extra = list(), persist = TRUE, gpp_mode = NULL) {
  set.seed(seed)
  dfs_load_sport(sport)
  spec  <- get_sport(sport); rr <- sport_roster(sport, site)   # DK or FanDuel roster rules
  gates <- load_gates(sport)
  if (is.null(spec$project_players)) stop("Sport '", sport, "' has no project_players() yet.")

  slate_obj <- c(list(sport = sport, site = site, date = as.character(date), name = slate,
                      slate_id = make_slate_id(sport, site, date, slate)), extra)
  pool <- validate_projection(as.data.table(spec$project_players(slate_obj)))
  # SAFEGUARD: drop DK-slate players the freshest feed says won't play + discount questionable
  # (the #1 live error — WNBA scratches / NFL inactives). No-op for golf/tennis (no feed).
  pool <- tryCatch(apply_inactives(pool, sport, date), error = function(e) pool)
  if (persist) tryCatch(persist_projected_ownership(pool, slate_obj$slate_id, sport), error = function(e) NULL)
  loadings <- get_loadings(pool, sport)
  sim   <- slate_sim(pool, loadings, team_loadings = get_team_loadings(pool, sport),
                     n_sims = n_sims, seed = seed)
  own   <- if ("own" %in% names(pool)) pmax(pool$own, 1e-3) else rep(1, nrow(pool))
  field <- simulate_field(pool, rr, field_n = field_n, own = own)
  cands <- make_candidates(pool, rr, n_cand = n_cand)
  res   <- grade_candidates(cands, sim, field, curve_gpp = make_gpp(), curve_cash = make_double_up())
  picks <- if (identical(gpp_mode, "gpp20"))
             build_gpp20(res, gates, n = n_lineups, pool = pool,
                         caps = tryCatch(load_exposure_overrides(sport), error = function(e) NULL))
           else build_portfolio(res, gates, n = n_lineups)
  # archive projections + recommended lineups (best-effort; never fail the build).
  # persist=FALSE for read-only consumers (e.g. the /dfs exposure optimizer) so they
  # never overwrite the dashboard's archived lineups.
  if (persist) {
    tryCatch(persist_projections(pool, slate_obj$slate_id, sport), error = function(e) NULL)
    tryCatch(persist_recommended_lineups(picks, pool, slate_obj$slate_id, sport, date), error = function(e) NULL)
    tryCatch(mark_slate_built(slate_obj$slate_id), error = function(e) NULL)
  }
  list(sport = sport, slate_id = slate_obj$slate_id, pool = pool, sim = sim,
       field = field, res = res, picks = picks, gates = gates, rr = rr)
}
