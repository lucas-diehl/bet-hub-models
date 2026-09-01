#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS v2 — engine/project.R  (Phase 4b: the orchestrator)
#
# Chains the whole v2 projection: skill -> course fit -> market blend ->
# tournament simulation (with cut) -> DK-point distribution, plus an affine LEVEL
# calibration so the projection is unbiased in DK points.
#
#   train_v2(master)            -> a model bundle (skill + coursefit + calib + cal_dk)
#   project_pool(bundle, pool)  -> per-player proj/floor/ceil/sim_sd/make_cut/own
#   project_live(bundle, tour)  -> live DK slate projection (DataGolf field + odds)
#   project_event(bundle, M, ev)-> historical event projection (for backtest)
#
# Source: Sys.setenv(ENGINE_SOURCE_ONLY="1"); source("engine/project.R")
# Run:    Rscript engine/project.R   (trains bundle, projects newest event, prints board)
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
Sys.setenv(ENGINE_SOURCE_ONLY="1", SKILL_SOURCE_ONLY="1",
           COURSEFIT_SOURCE_ONLY="1", MARKET_SOURCE_ONLY="1", SIMULATE_SOURCE_ONLY="1")
for (f in c("data","skill","coursefit","market","simulate"))
  if (!exists(c(data="build_master", skill="train_skill", coursefit="train_coursefit",
                market="load_win_odds", simulate="simulate_dk")[[f]]))
    source(file.path("engine", paste0(f, ".R")))
if (!exists("blend_dg_skill")) source("engine/enrich.R")   # live DataGolf enrichments
if (!exists("blend_ownership")) source("engine/ownership.R")  # our ownership model + blend
emsg <- get0("emsg", ifnotfound = function(...) cat(..., "\n"))
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
OUT  <- "golf_picks"
MARKET_W_DEFAULT <- 0.5     # backtest-tunable; market is a strong prior (P3)

# ── train the full bundle ─────────────────────────────────────────────────────
train_v2 <- function(master, market_w = MARKET_W_DEFAULT, calibrate_level = TRUE) {
  M   <- as.data.table(master)
  sk  <- train_skill(M)
  cf  <- train_coursefit(project_skill(sk, M))
  cal <- calibrate_dk()
  cal_fd <- tryCatch(calibrate_dk(site = "fd"), error = function(e) NULL)  # FanDuel re-score
  odds <- load_win_odds()
  bundle <- list(skill = sk, coursefit = cf, sim_cal = cal, sim_cal_fd = cal_fd, odds = odds,
                 market_w = market_w, level = list(a = 0, b = 1),
                 # win-tail calibration (engine/calibrate_sim.R): sim spread multiplier
                 spread_scale = tryCatch(readRDS(file.path(OUT,"v2_spread_scale.rds")),
                                         error = function(e) 1.0),
                 trained_through = max(M$year))
  if (calibrate_level) {
    bundle$level    <- .fit_level(bundle, M)
    bundle$level_fd <- tryCatch(.fit_level_fd(bundle, M), error = function(e) list(a = 0, b = 1))
  }
  bundle
}

# realized FanDuel points per player-event (from round-outcome counts + FD finish table),
# the FD analog of the DK `total_pts` target. Micro (streak/bounce) is omitted here (no
# hole sequence in the data) so it's a slight UNDER-count; the sim adds micro, so fitting
# real_fd ~ sim_proj still lands the level right.
.realized_fd <- function() {
  R <- as.data.table(readRDS(file.path(OUT, "v2_round_shapes.rds")))
  pe <- R[, .(nr = .N,
              score_pts = sum(hole_pts_fd) + 5 * sum(bogey_free) + 4 * sum(bird5),
              tot_topar = sum(to_par)), by = .(player_id, event_id, year)]
  pe[, made := nr >= 3L]
  pe[made == TRUE, rnk := frank(tot_topar, ties.method = "min"), by = .(event_id, year)]
  pe[, fin_pts := 0]; pe[made == TRUE, fin_pts := fd_finish_pts(rnk)]
  pe[, real_fd := score_pts + fin_pts][, .(player_id, event_id, year, real_fd)]
}

# affine FD-level fit: real_fd = a + b*sim_fd_proj, so FD projections read as honest
# FanDuel points (same idea as .fit_level for DK, different target + FD scoring).
.fit_level_fd <- function(bundle, M, n_events = 16L, n_sims = 1200L) {
  rf <- .realized_fd(); if (!nrow(rf)) return(list(a = 0, b = 1))
  ev <- unique(rf[, .(event_id, year)])
  ev <- ev[unique(M[, .(event_id, year)]), on = c("event_id", "year"), nomatch = 0L]
  set.seed(5); ev <- ev[sample(.N, min(n_events, .N))]
  rows <- rbindlist(lapply(seq_len(nrow(ev)), function(i) {
    pj <- tryCatch(project_pool(bundle, M[event_id == ev$event_id[i] & year == ev$year[i]],
                                n_sims = n_sims, calibrate = FALSE, site = "fd"),
                   error = function(e) NULL)
    if (is.null(pj)) return(NULL)
    merge(pj[, .(player_id, proj)],
          rf[event_id == ev$event_id[i] & year == ev$year[i], .(player_id, real_fd)],
          by = "player_id")
  }))
  rows <- rows[is.finite(real_fd) & is.finite(proj)]
  if (nrow(rows) < 100) return(list(a = 0, b = 1))
  fit <- lm(real_fd ~ proj, data = rows)
  list(a = coef(fit)[[1]], b = coef(fit)[[2]])
}

# affine calibration proj_final = a + b*sim_proj, fit on a sample of events so the
# projection is unbiased vs realised DK points (the sim is faithful in SHAPE; this
# fixes the ~6% level bias without distorting ceilings/floors).
.fit_level <- function(bundle, M, n_events = 16L, n_sims = 1200L) {
  ev <- unique(M[!is.na(total_pts), .(event_id, year)])
  set.seed(5); ev <- ev[sample(.N, min(n_events, .N))]
  rows <- rbindlist(lapply(seq_len(nrow(ev)), function(i) {
    pj <- tryCatch(project_pool(bundle, M[event_id==ev$event_id[i] & year==ev$year[i]],
                                n_sims = n_sims, calibrate = FALSE), error=function(e) NULL)
    if (is.null(pj)) return(NULL)
    merge(pj[, .(player_id, proj)],
          M[event_id==ev$event_id[i] & year==ev$year[i], .(player_id, total_pts)],
          by="player_id")
  }))
  rows <- rows[is.finite(total_pts) & is.finite(proj)]
  if (nrow(rows) < 100) return(list(a = 0, b = 1))
  # MSE-optimal linear fit (b~1.2): the raw sim proj is already DK-scale, so it needs
  # a gentle fit, NOT a spread-match. Moment/IQR matching stretches an expected-value
  # projection to REALISED (high-variance) outcomes -> over-projects the top. The proj
  # floor (pmax(proj,5) in project_pool) handles lm's slightly-negative intercept.
  fit <- lm(total_pts ~ proj, data = rows)
  list(a = coef(fit)[[1]], b = coef(fit)[[2]])
}

# skill -> coursefit -> market blend, returning the per-round SG mu stage (no sim).
# Used by the full projection and by single-round (engine/round_sim.R).
project_mu <- function(bundle, pool, live_market = NULL, dg_skill = NULL) {
  P <- project_skill(bundle$skill, pool)
  P <- project_coursefit(bundle$coursefit, P)
  odds <- if (!is.null(live_market)) {
    lm2 <- as.data.table(live_market)[, .(player_id, p_win)]
    lm2[, `:=`(event_id = P$event_id[1], year = P$year[1])]; lm2
  } else bundle$odds
  Pm <- market_skill(P, odds); Pm <- blend_market(Pm, bundle$market_w)
  Pm[, mu := final_mu]
  if (!is.null(dg_skill)) Pm <- blend_dg_skill(Pm, dg_skill)   # DataGolf per-cat skill prior
  Pm[]
}

# ── project any feature pool -> DK-point projection ───────────────────────────
# pool must carry the skill/coursefit feature columns + player_id + course_par
# (+ optional own, salary, player_name). live_market: data.table(player_id, p_win).
project_pool <- function(bundle, pool, n_sims = 4000L, cut_n = 65L,
                         live_market = NULL, wave = NULL, dg_skill = NULL,
                         calibrate = TRUE, site = "dk") {
  Pm <- project_mu(bundle, pool, live_market, dg_skill)  # skill->coursefit->market(->DG skill)
  if (!is.null(wave)) Pm <- apply_conditions(Pm, wave)  # live weather/wave shift
  # site scoring: DK uses the trained level calibration; FanDuel (fd) re-scores the SAME
  # per-round SG with FD's point map. FD has no fitted level yet -> identity (each FD
  # component is calibrated directly to FD points, so the raw sim is ~unbiased).
  fd <- tolower(site) %in% c("fd", "fanduel")
  cal <- if (fd) (bundle$sim_cal_fd %||% calibrate_dk(site = "fd")) else bundle$sim_cal
  # simulate_dk returns the pool WITH all its columns (s_*, salary, own, ...) plus
  # proj/floor/ceil/sim_sd/make_cut -- no re-merge needed.
  out <- simulate_dk(Pm, cal, n_sims = n_sims, cut_n = cut_n,
                     spread_scale = bundle$spread_scale %||% 1.0)$players
  lvl <- if (fd) (bundle$level_fd %||% list(a = 0, b = 1)) else bundle$level
  if (calibrate) { a <- lvl$a; b <- lvl$b
    out[, `:=`(proj = a + b*proj, floor = a + b*floor, ceil = a + b*ceil, sim_sd = b*sim_sd)] }
  out[, floor := pmax(floor, 0)]        # a rostered golfer can't net negative over a tourney
  out[, proj  := pmax(proj, 5)]         # no full-tournament projection below ~5 DK pts
  out[, ceil  := pmax(ceil, proj)]
  setorder(out, -proj)
  out[]
}

# ── live DK slate ─────────────────────────────────────────────────────────────
project_live <- function(bundle, tour = "pga", slate = "main", n_sims = 4000L,
                         cut_n = 65L, master = NULL, site = "dk") {
  if (is.null(master)) master <- as.data.table(readRDS(file.path(OUT,"v2_master.rds"))$master)
  sl <- assemble_live_slate(tour, slate, master)
  pool <- as.data.table(sl$players)
  pool[, `:=`(event_id = "live", year = as.integer(format(Sys.Date(), "%Y")),
              course_par = 71L)]
  # LIVE COURSE FIX: attach the CURRENT event's course profile + per-player history
  # at THIS course (else the snapshot carries each player's last event's course).
  cn <- tryCatch({ fu <- .dg("field-updates", list(tour = tour))
    resolve_course_num(as.character(fu$event_id), master) }, error = function(e) NA_integer_)
  if (!is.na(cn)) { pool <- attach_current_course(pool, cn, master)
    emsg("  course fit: attached course ", cn, " profile + player history") }
  # live enrichments: course-history-&-fit-adjusted market + DataGolf per-cat skill
  mkt <- tryCatch(get_dg_pretourn(tour, "baseline_history_fit"), error=function(e) data.table())
  if (!nrow(mkt)) mkt <- tryCatch(get_live_market(tour), error=function(e) data.table())
  sr  <- tryCatch(get_dg_skill_ratings(), error=function(e) data.table())
  pj <- project_pool(bundle, pool, n_sims = n_sims, cut_n = cut_n,
                     live_market = if (nrow(mkt)) mkt else NULL,
                     dg_skill    = if (nrow(sr)) sr else NULL, site = site)
  # OWNERSHIP EVERYWHERE: blend our trained model (cor .81) with DataGolf's feed.
  # Provisionally 90% DataGolf / 10% ours until the DG-vs-actual A/B (dg_ownership_log)
  # tells us which is sharper and we can tune this weight with data.
  if ("own" %in% names(pj))
    pj[, own := tryCatch(blend_ownership(pj, dg_own = own, w_ours = 0.1), error = function(e) own)]
  attr(pj, "event") <- sl$event
  pj
}

# ── historical event (backtest / study) ───────────────────────────────────────
project_event <- function(bundle, M, ev_id, yr, n_sims = 3000L, cut_n = 65L) {
  e <- as.data.table(M)[event_id == ev_id & year == yr]
  if (!nrow(e)) stop("event not found")
  if (!"course_par" %in% names(e)) e[, course_par := 71L]
  project_pool(bundle, e, n_sims = n_sims, cut_n = cut_n)
}

# ── demo: train + project newest event, print the board ───────────────────────
if (identical(environment(), globalenv()) && !nzchar(Sys.getenv("PROJECT_SOURCE_ONLY"))) {
  emsg("=== engine/project.R — train bundle + project newest event ===")
  M <- as.data.table(readRDS(file.path(OUT,"v2_master.rds"))$master)
  bundle <- train_v2(M[year < max(M$year)])   # hold out newest year for honesty
  saveRDS(bundle, file.path(OUT, "v2_bundle.rds"))
  emsg("level calibration: proj = ", round(bundle$level$a,2), " + ",
       round(bundle$level$b,3), " * sim_proj")
  ev <- M[year == max(year), .N, by=.(event_id,year)][order(-N)][1]
  pj <- project_event(bundle, M, ev$event_id, ev$year)
  cat(sprintf("\n== projection board: event %s/%s (%d players) ==\n", ev$event_id, ev$year, nrow(pj)))
  print(head(pj[, .(player = if("player_name" %in% names(pj)) player_name else player_id,
                    proj=round(proj,1), floor=round(floor,1), ceil=round(ceil,1),
                    mc=round(100*make_cut), ott=round(s_ott,2), app=round(s_app,2),
                    putt=round(s_putt,2))], 15), row.names=FALSE)
  # sanity vs actual
  act <- M[event_id==ev$event_id & year==ev$year, .(player_id, total_pts)]
  chk <- merge(pj[,.(player_id,proj)], act, by="player_id")
  cat(sprintf("\nvs actual: cor %.3f | mean proj %.1f vs actual %.1f\n",
      cor(chk$proj, chk$total_pts, use="complete.obs"), mean(chk$proj), mean(chk$total_pts, na.rm=TRUE)))
  emsg("saved v2_bundle.rds")
}
