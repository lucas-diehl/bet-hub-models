# ==============================================================================
# DFS ENGINE — bankroll & staking (how much, how many, which contests)
# Turns the graded portfolio + gates + your bankroll into a concrete daily plan:
# which contest TYPES to enter, how many entries, and $ per entry. Honest by
# construction: real-money stakes only for contest types whose backtest gate
# passed; everything else is PAPER ($0 real) but still shown so you see the plan.
#
# Staking is conservative fixed-fraction (not Kelly) until a gate reports a
# trustworthy measured edge — DFS is -EV after rake for most, so we cap exposure.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# defaults; override via config/bankroll.yml or the `opts` arg.
staking_defaults <- function() list(
  bankroll       = 200,     # your roll (set in config/bankroll.yml)
  daily_max_frac = 0.15,    # never risk more than this fraction of roll per day
  cash_frac      = 0.60,    # split of the daily budget to cash vs gpp
  gpp_frac       = 0.40,
  cash_fee       = 5,       # typical per-entry fee you play (cash)
  gpp_fee        = 3,       # typical per-entry fee you play (gpp)
  min_fee        = 1)

load_bankroll_opts <- function() {
  o <- staking_defaults()
  f <- dfs_path("config", "bankroll.yml")
  if (file.exists(f) && requireNamespace("yaml", quietly = TRUE)) {
    y <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    for (k in names(y)) o[[k]] <- y[[k]]
  }
  o
}

# Build the daily contest plan. picks = portfolio (list from build_portfolio).
# Returns a data.table: Contest, Type, Entries, FeePerEntry, TotalStake, Status, Rationale.
recommend_plan <- function(picks, gates, opts = load_bankroll_opts()) {
  roles <- vapply(picks, function(p) p$role, character(1))
  n_cash <- sum(roles == "Cash anchor")
  n_gpp  <- sum(roles != "Cash anchor")
  daily_budget <- opts$bankroll * opts$daily_max_frac

  rows <- list()
  add <- function(contest, type, n, fee, live, why) {
    if (n <= 0) return()
    stake <- if (live) round(n * fee, 2) else 0
    rows[[length(rows) + 1]] <<- data.table(
      Contest = contest, Type = type, Entries = n,
      FeePerEntry = if (live) fee else 0, TotalStake = stake,
      Status = if (live) "LIVE" else "PAPER", Rationale = why)
  }

  # CASH bucket
  cash_live <- isTRUE(gates$cash_enabled)
  cash_budget <- daily_budget * opts$cash_frac
  cash_fee <- if (n_cash > 0) max(opts$min_fee, round(cash_budget / n_cash)) else opts$cash_fee
  add("Double-Ups / 50-50", "Cash", n_cash, cash_fee, cash_live,
      if (cash_live) sprintf("Cash gate PASSED (validated %s). Floor-leaning builds.", gates$validated_on %||% "")
      else "Cash gate NOT passed -> PAPER. Track results; do not risk real money yet.")

  # GPP bucket
  gpp_live <- isTRUE(gates$gpp_enabled)
  gpp_budget <- daily_budget * opts$gpp_frac
  gpp_fee <- if (n_gpp > 0) max(opts$min_fee, round(gpp_budget / n_gpp)) else opts$gpp_fee
  add("Tournaments (GPP)", "GPP", n_gpp, gpp_fee, gpp_live,
      if (gpp_live) "GPP gate PASSED. Ceiling/leverage builds — expect high variance, min stakes."
      else "GPP gate NOT passed -> PAPER. Leverage is indicative until the field-graded backtest clears.")

  plan <- rbindlist(rows)
  attr(plan, "bankroll") <- opts$bankroll
  attr(plan, "daily_budget") <- daily_budget
  attr(plan, "live_total") <- sum(plan$TotalStake)
  plan
}

# Staking plan for a SPECIFIC DK contest (real entry fee + per-user entry cap).
recommend_contest_plan <- function(contest, picks, gates, opts = load_bankroll_opts()) {
  fee <- contest$entry_fee %||% NA
  live <- isTRUE(gates$gpp_enabled)                 # showdown / large-field = tournament
  daily_budget <- opts$bankroll * opts$daily_max_frac
  cap_user <- contest$max_entries_per_user %||% length(picks)
  max_e <- suppressWarnings(min(length(picks), cap_user, floor(daily_budget / max(fee, 0.1))))
  if (!is.finite(max_e) || max_e < 1) max_e <- length(picks)
  entries <- if (live) max(1L, as.integer(max_e)) else length(picks)
  stake <- if (live) round(entries * fee, 2) else 0
  plan <- data.table(
    Contest = contest$name %||% "Contest",
    Type = if (isTRUE(contest$is_showdown)) "Showdown GPP" else "GPP",
    Entries = entries, FeePerEntry = if (live) fee else 0, TotalStake = stake,
    Status = if (live) "LIVE" else "PAPER",
    Rationale = if (live)
      sprintf("GPP gate PASSED. Enter up to %d (DK cap %s/user). Min stakes — high variance.", entries, cap_user)
      else "GPP gate NOT passed -> PAPER. Showdown is high-variance; paper-track before risking money.")
  attr(plan, "bankroll") <- opts$bankroll
  attr(plan, "daily_budget") <- daily_budget
  attr(plan, "live_total") <- sum(plan$TotalStake)
  plan
}
