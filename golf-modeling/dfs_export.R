#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS PROJECTION EXPORT — bridge to the DFS ENGINE
# Runs the DFS model (dfs_pipeline_v2.R) for the CURRENT DataGolf slate and writes
# a clean per-player projections file the DFS ENGINE's golf adapter reads. This
# keeps the two projects decoupled: golf-modeling produces projections here; the
# spine just consumes golf_picks/dfs_projections.rds.
#   Run from the golf-modeling root:  Rscript dfs_export.R
# ==============================================================================
suppressPackageStartupMessages({ library(data.table) })
# run from the golf-modeling root regardless of launch dir (relative paths below)
if (dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")

# The pipeline errors at source time if DATAGOLF_API_KEY isn't set, so load it first
# (env -> golf-modeling/.Renviron -> the sibling DFS ENGINE/.Renviron).
if (!nzchar(Sys.getenv("DATAGOLF_API_KEY"))) {
  for (p in c(".Renviron", "../DFS ENGINE/.Renviron", "~/.Renviron"))
    if (file.exists(p)) { readRenviron(p); if (nzchar(Sys.getenv("DATAGOLF_API_KEY"))) break }
}

# v2 ENGINE CUTOVER: when GOLF_ENGINE_V2 is set (or gates$engine=="v2"), the 100%-ours
# simulation-first engine (engine/*.R) produces the projections instead of v1. v1 stays
# the default until v2 wins the backtest; flip the flag (or set it in dfs_gates.rds) to
# cut over. Writes the SAME dfs_projections*.rds files the DFS ENGINE adapter reads.
.v2_on <- nzchar(Sys.getenv("GOLF_ENGINE_V2")) ||
  tryCatch(isTRUE(readRDS("golf_picks/dfs_gates.rds")$engine == "v2"), error = function(e) FALSE)
if (.v2_on) {
  cat("[dfs_export] GOLF ENGINE v2 active -> engine/export.R\n")
  Sys.setenv(EXPORT_SOURCE_ONLY = "1")
  source("engine/export.R")
  run_export_v2()
  quit(save = "no", status = 0)
}

Sys.setenv(GOLF_DFS_SOURCE_ONLY = "1")     # source functions only; don't run main()
source("dfs_pipeline_v2.R")

gates  <- load_gates()
D      <- load_training()
models <- train_models(D)                                       # trained once, shared across events
dir.create("golf_picks", showWarnings = FALSE)

# Fresh live DataGolf projections + ownership per player, blended with the model.
write_variant <- function(slate, sl_base, blend, file) {
  sl <- score_slate(copy(sl_base), models, blend)
  out <- sl[, .(dg_id = as.integer(player_id), player_name = player_name,
                salary = as.integer(salary),
                proj = round(proj, 2), model_mean = round(model_mean, 2),
                dg_proj = round(dg_proj, 2), ceil = round(model_ceil, 2),
                sim_sd = round(sim_sd, 3), own = round(own, 4),
                floor = round(pmax(proj - 1.5 * sim_sd, 0), 2))]
  out <- out[!is.na(salary) & salary > 0 & is.finite(proj)]
  meta <- list(event = slate$event, generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
               date = as.character(Sys.Date()), n = nrow(out), blend_w_model = blend)
  saveRDS(list(projections = out, meta = meta), file.path("golf_picks", file))
  cat(sprintf("Wrote golf_picks/%s: %s — %d players (blend %.0f%% model)\n",
              file, slate$event, nrow(out), 100 * blend))
}

# Export EVERY DK-eligible tour this week: the main PGA event + the opposite-field
# event ("opp"). Each -> a default-blend file + an 80%-model file. Missing tours skip.
tours <- list(list(tour = "pga", suffix = ""), list(tour = "opp", suffix = "_opp"))
for (t in tours) {
  slate <- tryCatch(get_slate(t$tour), error = function(e) NULL)
  if (is.null(slate) || !length(slate$players) || !nrow(slate$players)) {
    cat("  no DataGolf slate for tour '", t$tour, "' — skipping\n", sep = ""); next
  }
  sl_base <- attach_features(slate$players, D)
  write_variant(slate, sl_base, gates$blend_w_model, sprintf("dfs_projections%s.rds", t$suffix))
  write_variant(slate, sl_base, 0.80,                sprintf("dfs_projections%s_m80.rds", t$suffix))
}
