# ==============================================================================
# DFS ENGINE — daily report  ("what do I enter today, how much, what lineups")
# Produces a self-contained HTML one-pager (open in a browser) + a console card:
#   - Today's plan: contest types, # entries, $/entry, total, LIVE/PAPER
#   - Bankroll exposure summary
#   - The lineups (players, salary, proj, ceiling, ownership) with reasoning
#   - Key plays: top projected / best leverage / best value
# Sport-agnostic. Honest: PAPER unless a gate passed.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

.h <- function(x) { x <- as.character(x)
  x <- gsub("&","&amp;",x); x <- gsub("<","&lt;",x); gsub(">","&gt;",x) }

# leverage = ceiling rank - ownership rank (positive = field underweights the upside)
.player_table <- function(pool) {
  P <- as.data.table(copy(pool))
  if (!"own" %in% names(P)) P[, own := NA_real_]
  P[, lev := frank(if ("ceil" %in% names(P)) ceil else proj) / .N - frank(-own) / .N]
  P[, value := proj / pmax(salary / 1000, 0.1)]
  P
}

write_daily_report <- function(sport, slate_id, pool, picks, res, gates,
                               plan = NULL, opts = load_bankroll_opts(), outdir = NULL) {
  P <- .player_table(pool)
  if (is.null(plan)) plan <- recommend_plan(picks, gates, opts)
  if (is.null(outdir)) outdir <- dfs_path("data", "reports")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  live_any <- isTRUE(gates$cash_enabled) || isTRUE(gates$gpp_enabled)
  status <- if (live_any) "LIVE (gate passed)" else "PAPER — gates not passed"
  ceil_of <- function(ix) sum((if ("ceil" %in% names(P)) P$ceil else P$proj)[ix])

  css <- "body{font-family:-apple-system,Segoe UI,Roboto,Arial,sans-serif;margin:0;background:#0f1115;color:#e6e8eb}
  .wrap{max-width:1000px;margin:0 auto;padding:24px}
  h1{font-size:22px;margin:0 0 2px} .sub{color:#9aa3ad;font-size:13px;margin-bottom:18px}
  .badge{display:inline-block;padding:3px 10px;border-radius:999px;font-size:12px;font-weight:600}
  .live{background:#10391f;color:#54e08a} .paper{background:#3a2a10;color:#f0b54a}
  table{width:100%;border-collapse:collapse;margin:8px 0 22px;font-size:13px}
  th,td{text-align:left;padding:7px 9px;border-bottom:1px solid #242833}
  th{color:#9aa3ad;font-weight:600} h2{font-size:15px;margin:18px 0 6px;color:#cfd4da}
  .lu{background:#161922;border:1px solid #242833;border-radius:10px;padding:12px 14px;margin:10px 0}
  .lu .role{font-weight:700} .lu .meta{color:#9aa3ad;font-size:12px;margin:3px 0 6px}
  .lu .why{color:#aab2bd;font-size:12px;margin-top:6px;font-style:italic}
  .tag{font-size:11px;padding:1px 7px;border-radius:6px;margin-left:6px}
  .disc{color:#7a828c;font-size:11px;margin-top:24px;border-top:1px solid #242833;padding-top:12px}
  .num{font-variant-numeric:tabular-nums}"

  H <- c(sprintf("<!doctype html><html><head><meta charset='utf-8'><title>DFS %s %s</title><style>%s</style></head><body><div class='wrap'>",
                 .h(toupper(sport)), .h(slate_id), css))
  H <- c(H, sprintf("<h1>%s — daily plan <span class='badge %s'>%s</span></h1>",
                    .h(toupper(sport)), if (live_any) "live" else "paper", .h(status)))
  H <- c(H, sprintf("<div class='sub'>Slate %s · generated %s</div>", .h(slate_id), .h(format(Sys.time(), "%Y-%m-%d %H:%M"))))

  # --- plan ---
  bk <- attr(plan, "bankroll"); live_total <- attr(plan, "live_total")
  H <- c(H, "<h2>Today's plan</h2><table><tr><th>Contest</th><th>Type</th><th>Entries</th><th>$/entry</th><th>Total</th><th>Status</th><th>Why</th></tr>")
  for (i in seq_len(nrow(plan))) { r <- plan[i]
    H <- c(H, sprintf("<tr><td>%s</td><td>%s</td><td class='num'>%d</td><td class='num'>$%s</td><td class='num'>$%s</td><td><span class='badge %s'>%s</span></td><td>%s</td></tr>",
      .h(r$Contest), .h(r$Type), r$Entries, .h(r$FeePerEntry), .h(r$TotalStake),
      if (r$Status=="LIVE") "live" else "paper", r$Status, .h(r$Rationale))) }
  H <- c(H, "</table>")
  H <- c(H, sprintf("<div class='sub'>Bankroll $%s · daily cap $%s (%.0f%%) · <b>real money at risk today: $%s</b></div>",
                    .h(bk), .h(round(attr(plan,"daily_budget"),2)), 100*opts$daily_max_frac, .h(round(live_total,2))))

  # --- lineups ---
  H <- c(H, sprintf("<h2>Lineups (%d)</h2>", length(picks)))
  for (i in seq_along(picks)) { p <- picks[[i]]; ix <- p$idx[[1]]
    pl <- P[ix][order(-proj)]
    chips <- paste(sprintf("%s <span class='num'>($%s, %.0f)</span>",
                  .h(pl$player_name), .h(pl$salary), pl$proj), collapse = " &nbsp;·&nbsp; ")
    H <- c(H, sprintf("<div class='lu'><div class='role'>#%d %s <span class='tag %s'>%s</span></div><div class='meta'>$%s salary · %.1f proj · %.1f ceil · %.0f%% avg own</div><div>%s</div><div class='why'>%s</div></div>",
      i, .h(p$role), if (isTRUE(p$live)) "live" else "paper", if (isTRUE(p$live)) "LIVE" else "PAPER",
      .h(p$salary), p$proj, ceil_of(ix), 100*p$avg_own, chips, .h(p$reason))) }

  # --- key plays ---
  topn <- function(dt, col, n=6) dt[order(-get(col))][seq_len(min(n,.N))]
  mk <- function(title, dt, extra) { c(sprintf("<h2>%s</h2><table><tr><th>Player</th><th>Pos</th><th>$</th><th>Proj</th>%s</tr>", title, extra)) }
  H <- c(H, mk("Top projected", NULL, "<th>Own%</th>"))
  for (i in seq_len(min(6,nrow(P)))) { r <- topn(P,"proj")[i]
    H <- c(H, sprintf("<tr><td>%s</td><td>%s</td><td class='num'>$%s</td><td class='num'>%.1f</td><td class='num'>%.0f%%</td></tr>",
      .h(r$player_name), .h(r$position %||% ""), .h(r$salary), r$proj, 100*(r$own %||% 0))) }
  H <- c(H, "</table>")
  H <- c(H, mk("Best leverage (ceiling the field underweights)", NULL, "<th>Ceil</th><th>Own%</th>"))
  for (i in seq_len(min(6,nrow(P)))) { r <- topn(P,"lev")[i]
    H <- c(H, sprintf("<tr><td>%s</td><td>%s</td><td class='num'>$%s</td><td class='num'>%.1f</td><td class='num'>%.1f</td><td class='num'>%.0f%%</td></tr>",
      .h(r$player_name), .h(r$position %||% ""), .h(r$salary), r$proj,
      (if ("ceil" %in% names(r)) r$ceil else r$proj), 100*(r$own %||% 0))) }
  H <- c(H, "</table>")

  H <- c(H, "<div class='disc'>DFS is negative-EV after rake for most entries. This tool improves your edge (distributions, ownership, leverage) — it does not guarantee profit. Stakes are conservative fixed-fraction; contest types stay PAPER until their field-graded backtest gate passes. Bet within your bankroll.</div></div></body></html>")

  f <- file.path(outdir, sprintf("plan_%s_%s.html", sport, Sys.Date()))
  writeLines(paste(H, collapse = "\n"), f)
  msg("Daily report ->", f)

  # --- console card ---
  cat("\n================  ", toupper(sport), " — DAILY PLAN  (", status, ")  ================\n", sep="")
  cat(sprintf("Bankroll $%s | daily cap $%s | REAL MONEY TODAY: $%s\n",
              bk, round(attr(plan,"daily_budget"),2), round(live_total,2)))
  cat("\nCONTESTS:\n")
  for (i in seq_len(nrow(plan))) { r <- plan[i]
    cat(sprintf("  %-20s %-5s %2d entries x $%-3s = $%-4s  [%s]\n",
      r$Contest, r$Type, r$Entries, r$FeePerEntry, r$TotalStake, r$Status)) }
  cat("\nLINEUPS:\n")
  for (i in seq_along(picks)) { p <- picks[[i]]; ix <- p$idx[[1]]
    cat(sprintf("  #%d [%s] %-13s $%s | %.0f proj | %.0f ceil | %.0f%% own\n     %s\n",
      i, if (isTRUE(p$live)) "LIVE" else "PAPER", p$role, p$salary, p$proj, ceil_of(ix),
      100*p$avg_own, paste(P$player_name[ix], collapse=", "))) }
  cat("\nOpen the HTML for the full card:", f, "\n")
  invisible(f)
}
