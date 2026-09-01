#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS v2 — engine/export.R  (Phase 5: bridge to the DFS ENGINE + lineups)
#
# Writes golf_picks/dfs_projections*.rds in the EXACT schema the DFS ENGINE golf
# adapter (sports/golf/adapter.R) consumes: list(projections = dt[dg_id, player_name,
# salary, proj, sim_sd, ceil, floor, own], meta). v2 is 100% ours -> meta$engine="v2",
# blend_w_model=1. Keeps the 4 variant filenames the dashboard expects
# (default/_m80/_opp/_opp_m80) so nothing downstream breaks.
#
# Also a STANDALONE lineup board (reuses v1's ILP optimizer + field sim + portfolio
# from dfs_pipeline_v2.R) so golf-modeling still emits its own xlsx plan.
#
#   export_v2(tour, variant)   -> writes one projections file (live DataGolf slate)
#   run_export_v2()            -> all tours x variants (what dfs_export.R calls)
#   v2_lineups(pj)             -> portfolio lineups from a v2 projection
#
# Run: Rscript engine/export.R           (live: writes dfs_projections*.rds)
#      Rscript engine/export.R --test    (offline: projects a historical event)
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
Sys.setenv(ENGINE_SOURCE_ONLY="1", SKILL_SOURCE_ONLY="1", COURSEFIT_SOURCE_ONLY="1",
           MARKET_SOURCE_ONLY="1", SIMULATE_SOURCE_ONLY="1", PROJECT_SOURCE_ONLY="1")
if (!exists("project_pool")) source("engine/project.R")
emsg <- get0("emsg", ifnotfound = function(...) cat(..., "\n"))
OUT  <- "golf_picks"

.golf_file <- function(tour = "main", variant = "default", site = "dk")
  sprintf("dfs_projections%s%s%s.rds",
          if (identical(tour, "opp")) "_opp" else "",
          if (identical(variant, "m80")) "_m80" else "",
          if (tolower(site) %in% c("fd", "fanduel")) "_fd" else "")

# load a trained bundle (retrain if missing/stale > 7 days)
load_bundle <- function(retrain_days = 7L) {
  f <- file.path(OUT, "v2_bundle.rds")
  if (file.exists(f) && difftime(Sys.time(), file.info(f)$mtime, units = "days") < retrain_days)
    return(readRDS(f))
  emsg("training v2 bundle (no fresh cache)...")
  M <- as.data.table(readRDS(file.path(OUT, "v2_master.rds"))$master)
  b <- train_v2(M); saveRDS(b, f); b
}

# map a v2 projection -> the adapter schema and write it
.write_projections <- function(pj, event, file, meta_extra = list()) {
  d <- as.data.table(copy(pj))
  if (!"own" %in% names(d))         d[, own := NA_real_]          # historical: no feed own
  if (!"player_name" %in% names(d)) d[, player_name := as.character(player_id)]
  out <- d[is.finite(proj) & !is.na(salary) & salary > 0, .(
    dg_id = as.integer(player_id),
    player_name,
    salary = as.integer(salary),
    proj   = round(proj, 2),
    ceil   = round(ceil, 2),
    floor  = round(pmax(floor, 0), 2),
    sim_sd = round(pmax(sim_sd, 4), 3),
    own    = round(fcoalesce(as.numeric(own), 0.001), 4),
    make_cut = round(make_cut, 3))]
  meta <- c(list(event = event, generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                 date = as.character(Sys.Date()), n = nrow(out),
                 blend_w_model = 1.0, engine = "v2"), meta_extra)
  saveRDS(list(projections = out, meta = meta), file.path(OUT, file))
  emsg(sprintf("wrote %s: %s -- %d players (engine v2)", file, event, nrow(out)))
  invisible(out)
}

# ── live export: project the current DataGolf slate + write files ─────────────
export_v2 <- function(tour = "pga", bundle = NULL, n_sims = 4000L) {
  if (is.null(bundle)) bundle <- load_bundle()
  key <- if (identical(tour, "opp")) "opp" else "main"
  pj <- tryCatch(project_live(bundle, tour = if (key=="opp") "opp" else "pga",
                              n_sims = n_sims),
                 error = function(e) { emsg("  no slate for tour '", tour, "': ", conditionMessage(e)); NULL })
  if (is.null(pj) || !nrow(pj)) return(invisible(NULL))
  ev <- attr(pj, "event"); if (is.null(ev)) ev <- "live"
  # v2 has no DG-blend variants; write default + an m80 alias so both tabs render.
  .write_projections(pj, ev, .golf_file(key, "default"), list(market_w = bundle$market_w))
  .write_projections(pj, ev, .golf_file(key, "m80"),     list(market_w = bundle$market_w, alias = "default"))
  # FanDuel re-score: SAME field/skill, FD point map. The DFS ENGINE golf adapter reads
  # this when a slate is FanDuel and joins FD salaries from the ingested CSV. Best-effort:
  # an FD failure never blocks the DK export above.
  tryCatch({
    pj_fd <- project_live(bundle, tour = if (key == "opp") "opp" else "pga",
                          n_sims = n_sims, site = "fd")
    if (!is.null(pj_fd) && nrow(pj_fd))
      .write_projections(pj_fd, ev, .golf_file(key, "default", "fd"),
                         list(market_w = bundle$market_w, site = "fanduel"))
  }, error = function(e) emsg("  FanDuel re-score skipped: ", conditionMessage(e)))
  invisible(pj)
}

run_export_v2 <- function(n_sims = 4000L) {
  bundle <- load_bundle()
  for (t in c("pga", "opp")) export_v2(if (t=="opp") "opp" else "pga", bundle, n_sims)
}

# ── standalone lineup board (reuse v1's optimizer/sim/portfolio) ──────────────
# adapts a v2 projection into the `sl` structure dfs_pipeline_v2.R's builders expect.
v2_lineups <- function(pj, event = "event", n_lineups = 8L) {
  Sys.setenv(GOLF_DFS_SOURCE_ONLY = "1")
  if (!exists("make_candidates")) source("dfs_pipeline_v2.R")   # ilp/candidates/sim/portfolio
  sl <- as.data.table(copy(pj))
  sl[, `:=`(model_mean = proj, model_ceil = ceil,
            ceil_prem = pmax(ceil - proj, 0),
            dg_proj = proj)]
  if (!"leverage" %in% names(sl))
    sl[, leverage := frank(model_ceil)/.N - fcoalesce(own, 0.001)]
  cands <- make_candidates(sl)
  res   <- simulate_slate(sl, cands)
  gates <- list(cash_enabled = FALSE, gpp_enabled = FALSE)  # spine gate is separate
  picks <- build_portfolio(res, sl, gates, n_lineups)
  write_output(picks, sl, event, gates)
  invisible(picks)
}

# ── main ──────────────────────────────────────────────────────────────────────
if (identical(environment(), globalenv()) && !nzchar(Sys.getenv("EXPORT_SOURCE_ONLY"))) {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--test" %in% args) {
    emsg("=== engine/export.R --test (offline historical event) ===")
    M <- as.data.table(readRDS(file.path(OUT, "v2_master.rds"))$master)
    bundle <- load_bundle()
    ev <- M[year == max(year), .N, by=.(event_id, year)][order(-N)][1]
    pj <- project_event(bundle, M, ev$event_id, ev$year)
    o <- .write_projections(pj, paste0("TEST_", ev$event_id, "_", ev$year),
                            "dfs_projections_v2test.rds")
    cat("\n== sample of exported schema ==\n")
    print(head(o[order(-proj)], 8), row.names = FALSE)
  } else {
    emsg("=== engine/export.R — live v2 export ===")
    run_export_v2()
  }
}
