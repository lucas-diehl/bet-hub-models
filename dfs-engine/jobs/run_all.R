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
  # actual ownership: download standings for your entered+settled contests (if DK session
  # configured). The IMPORT is deliberately deferred until after build_dashboard() below —
  # attributing a capture to the right slate layout (main1/main2/showdown) needs that
  # day's salaries to already be in the DB, and the scrape that writes them happens in
  # build_dashboard(). Importing here attributed same-day contests to a plain "-main"
  # slate_id that joins to no projections, which is exactly what starved the NFL/NCAAF
  # ownership models. Downloading early is still fine: it only fills the inbox.
  tryCatch(sync_ownership(), error = function(e) msg("ownership sync error:", conditionMessage(e)))
  # P&L: log the user's own settled entries (rank/points/prize -> profit) into the ledger
  tryCatch(ingest_all_results(), error = function(e) msg("results ingest error:", conditionMessage(e)))
  # accuracy: how close projections were to actuals per sport (the north-star scorecard)
  tryCatch(projection_accuracy(), error = function(e) msg("accuracy error:", conditionMessage(e)))
  sports   <- if (!is.null(a$sports)) strsplit(a$sports, ",")[[1]] else c("wnba","tennis","golf","golf_round","golf_captain","presidents_cup","golf_m80","golf_opp","golf_opp_m80","nfl","ncaaf")
  contests <- if (!is.null(a$contests)) strsplit(a$contests, ",")[[1]] else NULL
  # log today's projections FIRST (fast) so accuracy grading has them even if the heavy
  # dashboard build below is slow / fails / hits a DB lock.
  # sports= was omitted here, so this always fell back to log_projections()'s own
  # default (wnba/tennis/golf/golf_opp only) even though `sports` above already lists
  # nfl/ncaaf/golf_round/etc. — the accuracy scorecard silently never had NFL/NCAAF's
  # daily projection snapshot to grade against (they're still separately archived by
  # build_dashboard()'s persist_projections(), so this only affects the fast/early log).
  tryCatch(log_projections(sports = sports, date = Sys.Date()), error = function(e) msg("projection log error:", conditionMessage(e)))
  f <- build_dashboard(sports = sports, contests = contests, bankroll = a$bankroll)
  # NOW import the standings downloaded above — today's salaries exist, so
  # .resolve_logged_slate() can attach each capture to the layout our projections used.
  # Anything that still fails stays in the inbox and is retried on the next run.
  tryCatch(auto_import_ownership(), error = function(e) msg("ownership import error:", conditionMessage(e)))
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
  # Contest selection (P3, 2026-08-18) — WHERE to enter, not who to play. Built and
  # validated live back in August (confirmed 150-max mega-GPPs run 15-16% rake vs
  # 5-11% for single-/small-max) but never actually wired into anything run daily
  # until now — it only ever ran if you remembered to type the command yourself.
  # Same system2 pattern as the other auxiliary jobs above; writes a dated, readable
  # report next to the HTML dashboard rather than editing dashboard.html's JS to add
  # a section (lower risk, and the output is a plain ranked list, not a chart).
  tryCatch({
    cs <- dfs_path("jobs", "contest_select.R")
    if (file.exists(cs)) {
      rs3 <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
      cs_sports <- intersect(sports, c("nfl", "ncaaf", "wnba", "golf", "tennis"))
      if (length(cs_sports)) {
        out <- system2(rs3, c(shQuote(cs), "--sports", paste(cs_sports, collapse = ",")),
                       stdout = TRUE, stderr = TRUE)
        outdir <- dfs_path("data", "reports"); dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
        cf <- file.path(outdir, sprintf("contest_selection_%s.txt", Sys.Date()))
        writeLines(out, cf)
        msg("Contest selection ->", cf)
      }
    }
  }, error = function(e) msg("contest selection error:", conditionMessage(e)))
  msg("Done. Open:", f)
}
