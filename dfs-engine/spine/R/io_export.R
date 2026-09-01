# ==============================================================================
# DFS ENGINE — output (xlsx report + DK/FD CSV export)
# Generalizes Golf/dfs_pipeline_v2.R write_output(): a Lineups tab (the portfolio,
# one row each, with reasoning + LIVE/PAPER), a Rankings tab (full board to build
# your own), and a DK-upload CSV. Sport-agnostic — driven by roster_slots order.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table); library(openxlsx) })

# Ordered slot labels for the DK upload header (e.g. c("PG","SG","SF","PF","C","G","F","UTIL")).
# Falls back to generic P1..Pn when a sport has no explicit slot template.
roster_slot_labels <- function(rr) {
  if (!is.null(rr$slot_labels)) return(rr$slot_labels)
  if (!is.null(rr$slots)) {
    lab <- unlist(lapply(names(rr$slots), function(p) rep(p, rr$slots[[p]])))
    if (!is.null(rr$flex$count) && rr$flex$count > 0) lab <- c(lab, rep("FLEX", rr$flex$count))
    return(lab)
  }
  paste0("P", seq_len(rr$n))
}

write_report <- function(picks, pool, sport, event, gates,
                         rr = get_sport(sport)$roster_rules, outdir = NULL) {
  pool <- as.data.table(pool)
  if (!"player_name" %in% names(pool)) pool[, player_name := as.character(player_id)]
  if (is.null(outdir)) outdir <- dfs_path("data", "lineups", sport)
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  n_lu <- length(picks)
  expo <- tabulate(unlist(lapply(picks, function(p) p$idx[[1]])), nbins = nrow(pool))
  pool[, lineup_exposure := expo]
  ceil_col <- if ("ceil" %in% names(pool)) pool$ceil else pool$proj

  wb  <- createWorkbook()
  hdr <- createStyle(fontColour = "#FFFFFF", fgFill = "#1E4D78", textDecoration = "bold")

  # 1. LINEUPS
  addWorksheet(wb, "Lineups")
  lu <- rbindlist(lapply(seq_along(picks), function(i) { p <- picks[[i]]; ix <- p$idx[[1]]
    data.table(
      Lineup = i, Role = p$role,
      Live = ifelse(isTRUE(p$live), "LIVE - gate passed", "PAPER ONLY"),
      Players = paste(pool$player_name[ix], collapse = ", "),
      Salary = p$salary, Proj = round(p$proj, 1), Ceiling = round(sum(ceil_col[ix]), 1),
      AvgOwn = round(100 * p$avg_own, 1), P_Cash = round(100 * p$p_cash, 1),
      GPP_EV = round(100 * p$gpp_ev, 1), Top1_pct = round(100 * p$p_top1, 1),
      Reasoning = p$reason) }))
  writeData(wb, "Lineups", lu, headerStyle = hdr); setColWidths(wb, "Lineups", 1:ncol(lu), "auto")

  # 2. RANKINGS — full board
  addWorksheet(wb, "Rankings")
  sim_sd <- if ("sim_sd" %in% names(pool)) pool$sim_sd else rep(0, nrow(pool))
  rk <- pool[order(-proj), .(
    Player = player_name, Team = if ("team" %in% names(pool)) team else NA,
    Pos = if ("position" %in% names(pool)) position else NA,
    Salary = salary, Proj = round(proj, 1),
    Value = round(proj / pmax(salary / 1000, 0.1), 2),
    Ceiling = round(ceil_col[order(-proj)], 1),
    Floor = round(pmax(proj - 1.5 * sim_sd, 0)[order(-proj)], 1),
    Own_pct = round(100 * (if ("own" %in% names(pool)) own else NA), 1),
    Exposure_pct = round(100 * lineup_exposure / max(1L, n_lu)))]
  writeData(wb, "Rankings", rk, headerStyle = hdr); setColWidths(wb, "Rankings", 1:ncol(rk), "auto")

  # event/slate_id is already sport-prefixed and unique; don't double-prefix
  stamp <- gsub("[^A-Za-z0-9]+", "_", if (startsWith(as.character(event), sport)) event else paste(sport, event))
  xlsx <- file.path(outdir, paste0("dfs_plan_", stamp, "_", Sys.Date(), ".xlsx"))
  saveWorkbook(wb, xlsx, overwrite = TRUE); msg("Saved report:", xlsx)

  # 3. DK upload CSV (player names per slot; swap to DK ids if pool has dk_id)
  slot_lab <- roster_slot_labels(rr)
  id_col <- if ("dk_id" %in% names(pool)) "dk_id" else "player_name"
  live_picks <- Filter(function(p) isTRUE(p$live), picks)
  emit <- if (length(live_picks)) live_picks else picks    # paper still exported, flagged in xlsx
  csv <- rbindlist(lapply(emit, function(p) {
    ix <- p$idx[[1]]
    vals <- as.list(pool[[id_col]][ix]); names(vals) <- slot_lab[seq_along(ix)]
    as.data.table(vals) }), fill = TRUE)
  csv_path <- file.path(outdir, paste0("dk_upload_", stamp, "_", Sys.Date(), ".csv"))
  fwrite(csv, csv_path); msg("Saved DK upload CSV:", csv_path,
                             if (!length(live_picks)) "(PAPER — no gate passed)" else "")

  # console summary
  cat("\n========== DFS PORTFOLIO —", event, " (", n_lu, "lineups) ==========\n")
  if (!isTRUE(gates$cash_enabled) && !isTRUE(gates$gpp_enabled))
    cat("** ALL CONTESTS GATED OFF — run the backtest; until gates pass these are PAPER only.\n\n")
  for (i in seq_along(picks)) { p <- picks[[i]]; ix <- p$idx[[1]]
    cat(sprintf("#%d [%s] %s\n   %s\n   $%d | proj %.1f | ceil %.1f | own %.0f%%\n",
        i, ifelse(isTRUE(p$live), "LIVE", "PAPER"), p$role,
        paste(pool$player_name[ix], collapse = ", "),
        p$salary, p$proj, sum(ceil_col[ix]), 100 * p$avg_own)) }
  invisible(list(xlsx = xlsx, csv = csv_path))
}
