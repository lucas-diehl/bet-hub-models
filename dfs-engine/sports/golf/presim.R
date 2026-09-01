# ==============================================================================
# Golf pre-contest SIMULATOR — maximize your entries
# Runs the full correlated slate sim + realistic field + EV grading for a golf
# tournament and returns a BIG portfolio of optimized lineups with their simulated
# outcomes (cash %, top-1% / boom %, GPP EV, dupe risk) plus player EXPOSURE across
# the portfolio. Use it to pick which lineups to enter and how much to expose each
# player. Works for either tournament (main / opposite-field) and either blend.
#   golf_presim("main")            # Scottish Open, default blend, 20 lineups
#   golf_presim("opp", n=30)       # ISCO, 30 lineups
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

golf_presim <- function(tournament = "main", variant = "default", n = 20L,
                        date = Sys.Date(), n_sims = 15000L, field_n = 2500L, write = TRUE) {
  sport_key <- paste0("golf", if (identical(tournament, "opp")) "_opp" else "",
                      if (identical(variant, "m80")) "_m80" else "")
  slate <- if (sport_key == "golf") "main" else sport_key
  r <- run_slate("golf", date = date, slate = slate, n_lineups = n, n_sims = n_sims,
                 field_n = field_n, extra = list(variant = variant, tournament = tournament))
  P <- as.data.table(r$pool)
  ceilcol <- if ("ceil" %in% names(P)) P$ceil else P$proj

  # one row per recommended lineup, with its simulated outcome distribution
  lineups <- rbindlist(lapply(seq_along(r$picks), function(i) {
    p <- r$picks[[i]]; ix <- p$idx[[1]]
    data.table(lineup = i, role = p$role %||% "",
      players = paste(P$player_name[ix], collapse = ", "),
      salary = as.integer(p$salary), proj = round(p$proj, 1), ceil = round(sum(ceilcol[ix]), 1),
      avg_own = round(100 * (p$avg_own %||% 0), 1),
      cash_pct = round(100 * (p$p_cash %||% NA_real_), 1),
      boom_top1_pct = round(100 * (p$p_top1 %||% NA_real_), 1),
      gpp_ev_pct = round(100 * (p$gpp_ev %||% NA_real_), 0),
      dupe_risk = round(p$dupe_idx %||% NA_real_, 2))
  }), fill = TRUE)

  # player exposure across the portfolio (how many of the N lineups each player is in)
  cnt <- tabulate(unlist(lapply(r$picks, function(p) p$idx[[1]])), nbins = nrow(P))
  expo <- P[, .(player = player_name, salary = as.integer(salary), proj = round(proj, 1),
                own = round(100 * (if ("own" %in% names(P)) own else 0), 1))]
  expo[, `:=`(lineups = cnt, exposure_pct = round(100 * cnt / max(1L, length(r$picks)), 0))]
  expo <- expo[lineups > 0][order(-lineups)]

  event <- attr(r$pool, "event") %||% tournament
  cat(sprintf("\n===== GOLF PRE-CONTEST SIM — %s  (%s blend, %d lineups, %d sims, field %d) =====\n",
              event, variant, length(r$picks), n_sims, field_n))
  cat("gates: cash", isTRUE(r$gates$cash_enabled), " gpp", isTRUE(r$gates$gpp_enabled),
      "  (real $ only where a gate passed)\n\n")
  print(lineups)
  cat("\n-- top exposures --\n"); print(head(expo, 15))

  if (write) {
    dir <- dfs_path("data", "reports"); dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    tag <- gsub("[^A-Za-z0-9]+", "_", paste0(event, "_", variant))
    fwrite(lineups, file.path(dir, sprintf("golfsim_lineups_%s_%s.csv", tag, date)))
    fwrite(expo,    file.path(dir, sprintf("golfsim_exposure_%s_%s.csv", tag, date)))
    msg("wrote golfsim_lineups/exposure CSVs ->", dir)
  }
  invisible(list(lineups = lineups, exposure = expo, event = event, gates = r$gates))
}
