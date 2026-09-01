#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — DO EVERYTHING for today (the one command to schedule).
#   1. auto-import any DK standings CSVs dropped in data/ownership_inbox/ (actual ownership)
#   2. build the interactive dashboard for all sports + any saved contests
#      (auto-scrapes today's DK slates, auto-logs salaries + projected ownership,
#       builds lineups / captain boards)
# Nothing to type. Point Windows Task Scheduler at this (see scripts/schedule_windows.ps1).
#
#   Rscript jobs/run_all.R [--bankroll 300] [--sports wnba,tennis,golf,nfl,ncaaf] [--contests id,id]
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine()

parse_args <- function(args = commandArgs(TRUE)) { out <- list(); i <- 1L
  while (i <= length(args)) { if (startsWith(args[i], "--")) { out[[sub("^--", "", args[i])]] <- args[i + 1L]; i <- i + 2L } else i <- i + 1L }; out }

if (!interactive()) {
  a <- parse_args()
  msg("=== DFS ENGINE — daily run ===")
  # actual ownership: download standings for your entered+settled contests (if DK
  # session configured), then import everything in the inbox.
  tryCatch(sync_ownership(), error = function(e) msg("ownership sync error:", conditionMessage(e)))
  tryCatch(auto_import_ownership(), error = function(e) msg("ownership import error:", conditionMessage(e)))
  # P&L: log the user's own settled entries (rank/points/prize -> profit) into the ledger
  tryCatch(ingest_all_results(), error = function(e) msg("results ingest error:", conditionMessage(e)))
  # accuracy: how close projections were to actuals per sport (the north-star scorecard)
  tryCatch(projection_accuracy(), error = function(e) msg("accuracy error:", conditionMessage(e)))
  sports   <- if (!is.null(a$sports)) strsplit(a$sports, ",")[[1]] else c("wnba","tennis","golf","golf_round","golf_captain","golf_m80","golf_opp","golf_opp_m80","nfl","ncaaf")
  contests <- if (!is.null(a$contests)) strsplit(a$contests, ",")[[1]] else NULL
  # log today's projections FIRST (fast) so accuracy grading has them even if the heavy
  # dashboard build below is slow / fails / hits a DB lock.
  tryCatch(log_projections(date = Sys.Date()), error = function(e) msg("projection log error:", conditionMessage(e)))
  f <- build_dashboard(sports = sports, contests = contests, bankroll = a$bankroll)
  # publish the interactive simulator to the bet-site feed (served behind a password, auto-rebuilt)
  tryCatch(publish_simulator(f), error = function(e) msg("simulator publish error:", conditionMessage(e)))
  # golf betting: refresh the H2H matchup picks feed the bet site reads (no-op if no live event)
  tryCatch({
    a2 <- commandArgs(FALSE); m2 <- grep("^--file=", a2, value = TRUE)
    r2 <- if (length(m2)) dirname(dirname(normalizePath(sub("^--file=", "", m2[1])))) else getwd()
    bh <- file.path(dirname(r2), "golf-modeling", "engine", "bet_hub.R")
    if (file.exists(bh)) {
      rs <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
      msg("golf bet hub: refreshing H2H picks feed ...")
      out <- system2(rs, shQuote(bh), stdout = TRUE, stderr = TRUE)
      msg("golf bet hub:", if (length(out)) tail(out, 1) else "(no output)")
    } else msg("golf bet hub: script not found at", bh)
  }, error = function(e) msg("bet hub error:", conditionMessage(e)))
  # golf ownership A/B: snapshot DataGolf's projected ownership each slate (vs our model + actual)
  tryCatch({
    a2 <- commandArgs(FALSE); m2 <- grep("^--file=", a2, value = TRUE)
    r2 <- if (length(m2)) dirname(dirname(normalizePath(sub("^--file=", "", m2[1])))) else getwd()
    lg <- file.path(dirname(r2), "golf-modeling", "log_dg_own.R")
    if (file.exists(lg)) {
      rs <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
      out <- system2(rs, shQuote(lg), stdout = TRUE, stderr = TRUE)
      msg("dg-own log:", if (length(out)) tail(out, 1) else "(done)")
    }
  }, error = function(e) msg("dg-own log error:", conditionMessage(e)))
  # golf Elo snapshot -> bet dashboard feed (elo_<date>.json)
  tryCatch({
    a3 <- commandArgs(FALSE); m3 <- grep("^--file=", a3, value = TRUE)
    r3 <- if (length(m3)) dirname(dirname(normalizePath(sub("^--file=", "", m3[1])))) else getwd()
    el <- file.path(dirname(r3), "golf-modeling", "emit_elo.R")
    if (file.exists(el)) {
      rs <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
      eo <- system2(rs, shQuote(el), stdout = TRUE, stderr = TRUE)
      msg("elo feed:", if (length(eo)) tail(eo, 1) else "(done)")
    }
  }, error = function(e) msg("elo feed error:", conditionMessage(e)))
  # DFS value feed: write top-10 value names per sport for the bet site's /dfs page
  tryCatch({
    Sys.setenv(DFS_VALUES_SOURCE_ONLY = "1")
    source(dfs_path("jobs", "dfs_values.R")); write_dfs_values()
  }, error = function(e) msg("dfs values error:", conditionMessage(e)))
  # golf ROUND-specific DFS feed (auto-detects the live DK single-round slate; no-op otherwise)
  tryCatch({
    rj <- dfs_path("jobs", "round_dfs.R")
    if (file.exists(rj)) {
      rs2 <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
      out <- system2(rs2, shQuote(rj), stdout = TRUE, stderr = TRUE)
      msg("round DFS feed:", if (length(out)) tail(out, 1) else "(done)")
    }
  }, error = function(e) msg("round DFS feed error:", conditionMessage(e)))
  msg("Done. Open:", f)
}
