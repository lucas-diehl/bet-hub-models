# ==============================================================================
# DFS ENGINE — validation harness + gates  (build in parallel; no metric, no merge)
# Three falsifiable layers, generalized from Golf/dfs_backtest_v2.R:
#   L1 accuracy    : RMSE/MAE/corr of proj vs actual, vs a Vegas/salary baseline.
#   L2 calibration : are stated quantiles true? reliability of P(actual >= proj).
#   L3 contest ROI : lineups vs the field reconstructed from ACTUAL ownership under
#                    REAL payouts -> cash win rate + GPP ROI; writes per-sport gates.
#
# Operates on a STANDARDIZED historical table so it stays sport-agnostic. Each sport
# supplies it via an optional plugin field `backtest_table(years)` returning columns:
#   slate_id, year, player_id, salary, position, team, game_id,
#   proj, sim_sd, ceil, own_actual (0-1 or 0-100), total_pts   [+ optional base_proj]
# Projections MUST be walk-forward (trained only on data before each slate's year).
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# ── L1: projection accuracy ───────────────────────────────────────────────────
validate_accuracy <- function(H) {
  H <- as.data.table(H)
  by_pos <- function(d) d[, .(
    n = .N, rmse = sqrt(mean((proj - total_pts)^2)),
    mae = mean(abs(proj - total_pts)), corr = suppressWarnings(cor(proj, total_pts))),
    by = .(position = if ("position" %in% names(d)) position else "ALL")]
  model <- by_pos(H)
  base_col <- if ("base_proj" %in% names(H)) "base_proj" else NULL
  out <- list(model = model)
  if (!is.null(base_col)) {
    Hb <- copy(H)[, proj := get(base_col)]
    out$baseline <- by_pos(Hb)
    out$beats_baseline <- sqrt(mean((H$proj - H$total_pts)^2)) <
                          sqrt(mean((H[[base_col]] - H$total_pts)^2))
  }
  out
}

# ── L2: distribution calibration ──────────────────────────────────────────────
# Each player implies P(actual >= x) = 1 - Phi((x - proj)/sim_sd). Bin predicted
# probabilities and compare to empirical hit rate (reliability). A '90th pct'
# outcome must occur ~10% of the time.
validate_calibration <- function(H, thresholds = NULL) {
  H <- as.data.table(H)[sim_sd > 0]
  if (is.null(thresholds)) thresholds <- quantile(H$total_pts, c(.5, .7, .8, .9, .95), names = FALSE)
  rel <- rbindlist(lapply(thresholds, function(x) {
    p_pred <- 1 - pnorm((x - H$proj) / H$sim_sd)
    data.table(threshold = x, pred = mean(p_pred), emp = mean(H$total_pts >= x))
  }))
  rel[, abs_err := abs(pred - emp)]
  rel
}

# ── L3 data-quality gate (run BEFORE any contest backtest) ────────────────────
# A gate that can move real money must be earned on real, complete data. This
# refuses to grade unless the standardized history H clears coverage + provenance
# thresholds; otherwise the caller keeps gates FALSE with an "insufficient_data"
# reason. Returns list(pass, reasons, summary). No silent fallbacks.
validate_data_quality <- function(H, rr, min_slates = 30L, min_pool_mult = 3,
                                  min_own_coverage = 0.80) {
  H <- as.data.table(H); reasons <- character(0)
  n_slates <- uniqueN(H$slate_id)
  if (n_slates < min_slates)
    reasons <- c(reasons, sprintf("only %d slates (need >= %d) — insufficient_data", n_slates, min_slates))

  # player coverage: every slate needs a pool big enough to build distinct lineups
  per <- H[, .(np = .N), by = slate_id]
  thin <- per[np < rr$n * min_pool_mult]
  if (nrow(thin))
    reasons <- c(reasons, sprintf("%d/%d slates have < %d players", nrow(thin), n_slates, rr$n * min_pool_mult))

  # salary coverage: no missing / non-positive salary rows
  n_bad_sal <- sum(is.na(H$salary) | H$salary <= 0)
  if (n_bad_sal > 0)
    reasons <- c(reasons, sprintf("%d rows with missing/zero salary", n_bad_sal))

  # ACTUAL ownership coverage + provenance (no projected/fallback ownership allowed)
  own_real <- !is.na(H$own_actual) & H$own_actual > 0
  cov <- mean(own_real)
  if (cov < min_own_coverage)
    reasons <- c(reasons, sprintf("actual-ownership coverage %.0f%% (need >= %.0f%%)",
                                  100 * cov, 100 * min_own_coverage))
  if ("own_source" %in% names(H)) {
    if (any(H$own_source[own_real] != "actual", na.rm = TRUE))
      reasons <- c(reasons, "ownership contains non-actual (projected/imputed) rows")
  } else reasons <- c(reasons, "no own_source provenance column — cannot certify ownership is actual")

  # payout-source provenance: a real per-contest payout table must be present
  if (!"payout_source" %in% names(H) || any(H$payout_source != "table", na.rm = TRUE))
    reasons <- c(reasons, "no real DK payout table linked (payout_source != 'table') — representative curve not gate-eligible")

  summary <- list(n_slates = n_slates, thin_slates = nrow(thin), bad_salary_rows = n_bad_sal,
                  own_coverage = round(cov, 3))
  list(pass = length(reasons) == 0L, reasons = reasons, summary = summary)
}

# ── L3: contest-result backtest (field from actual ownership, real payouts) ────
# strict_own = TRUE (gated path): never fabricate ownership. A slate missing any
# actual ownership is SKIPPED (returns NULL) rather than imputed to 0.1.
grade_history_slate <- function(sl, rr, lev_gamma, curve_gpp, curve_cash, field_n = 1200L,
                                strict_own = TRUE) {
  sl <- as.data.table(sl)
  if (strict_own && any(is.na(sl$own_actual) | sl$own_actual <= 0)) return(NULL)
  own <- pmax(ifelse(is.na(sl$own_actual) | sl$own_actual <= 0, 0.1,
                     ifelse(sl$own_actual > 1, sl$own_actual / 100, sl$own_actual)), 1e-3)
  ceil_prem <- pmax((if ("ceil" %in% names(sl)) sl$ceil else sl$proj) - sl$proj, 0)
  builds <- list(
    cash     = sl$proj,
    balanced = sl$proj + 0.5 * ceil_prem,
    leverage = sl$proj + 0.8 * ceil_prem - lev_gamma * own * mean(sl$proj))
  con <- build_constraints(sl, rr)
  idxs <- lapply(builds, function(o) ilp_solve(o, sl, rr, con))
  if (any(vapply(idxs, is.null, logical(1)))) return(NULL)

  act <- sl$total_pts
  # field from actual ownership, scored on ACTUAL points (golf's approach)
  fld <- simulate_field(sl, rr, field_n = field_n, own = own)
  if (length(fld$idx) < 200) return(NULL)
  ftot  <- vapply(fld$idx, function(ix) sum(act[ix]), numeric(1))
  fkeys <- fld$keys
  cash_line <- quantile(ftot, 0.5, names = FALSE)
  field_size <- length(ftot)

  rbindlist(lapply(names(idxs), function(nm) {
    ix <- idxs[[nm]]; tot <- sum(act[ix]); key <- paste(sort(ix), collapse = "-")
    dupes <- sum(fkeys == key) + 1L
    pct <- mean(tot >= ftot); rank <- max(1L, round((1 - pct) * field_size))
    data.table(family = nm, actual = tot, cashed = as.integer(tot >= cash_line),
               cash_roi = grade_roi(rank, curve_cash, field_size),
               gpp_roi  = grade_roi(rank, curve_gpp, field_size, dupes), pctile = pct)
  }))
}

# Full L3 sweep: tune lev_gamma on early years, validate on the latest, write gates.
validate_contest <- function(H, sport, rr = get_sport(sport)$roster_rules,
                             tune_years = NULL, holdout_year = NULL,
                             cash_min_slates = 30L, cash_win_gate = 0.58) {
  H <- as.data.table(H)

  # DATA-QUALITY GATE: never flip a money gate on thin/synthetic/fallback data.
  dq <- validate_data_quality(H, rr, min_slates = cash_min_slates)
  if (!dq$pass) {
    cat("\n========== ", toupper(sport), "L3 BLOCKED: insufficient_data ==========\n")
    for (r in dq$reasons) cat("  -", r, "\n")
    gates <- list(sport = sport, cash_enabled = FALSE, gpp_enabled = FALSE,
                  lev_gamma = 6, validated_on = NA, n_slates = dq$summary$n_slates,
                  status = "insufficient_data", reasons = dq$reasons,
                  run_date = as.character(Sys.Date()))
    gates_path <- dfs_path("data", paste0("gates_", sport, ".rds"))
    dir.create(dirname(gates_path), showWarnings = FALSE, recursive = TRUE)
    saveRDS(gates, gates_path)
    cat("\nGates kept FALSE (PAPER) ->", gates_path, "\n")
    return(invisible(list(gates = gates, validation = NULL, data_quality = dq)))
  }

  yrs <- sort(unique(H$year))
  if (is.null(holdout_year)) holdout_year <- max(yrs)
  if (is.null(tune_years))   tune_years   <- setdiff(yrs, holdout_year)
  curve_gpp <- make_gpp(); curve_cash <- make_double_up()

  run_cfg <- function(years, lev_gamma) {
    sl_ids <- unique(H[year %in% years, .(slate_id, year)])
    rbindlist(lapply(seq_len(nrow(sl_ids)), function(i) {
      sl <- H[slate_id == sl_ids$slate_id[i]]
      if (nrow(sl) < rr$n * 3) return(NULL)
      g <- grade_history_slate(sl, rr, lev_gamma, curve_gpp, curve_cash)
      if (!is.null(g)) g[, slate_id := sl_ids$slate_id[i]]; g
    }))
  }

  msg("Tuning lev_gamma on years:", paste(tune_years, collapse = ","))
  grid <- c(2, 4, 6, 8)
  tune <- rbindlist(lapply(grid, function(lg) {
    r <- run_cfg(tune_years, lg)
    data.table(lev_gamma = lg,
               cash_win = mean(r[family == "cash"]$cashed),
               lev_gpp  = mean(r[family == "leverage"]$gpp_roi))
  }))
  print(tune)
  best <- tune[which.max(0.5 * cash_win + 0.5 * pmin(lev_gpp, 0.5))]
  msg("Chosen lev_gamma =", best$lev_gamma)

  msg("Validating on holdout year:", holdout_year)
  V <- run_cfg(holdout_year, best$lev_gamma)
  summ <- V[, .(slates = .N, cash_win = round(100 * mean(cashed), 1),
                cash_roi = round(100 * mean(cash_roi), 1),
                gpp_roi  = round(100 * mean(gpp_roi), 1),
                mean_pctile = round(mean(pctile), 3)), by = family]
  cat("\n========== ", toupper(sport), holdout_year, "VALIDATION ==========\n"); print(summ)

  n_cash <- nrow(V[family == "cash"])
  gates <- list(
    sport = sport,
    cash_enabled = n_cash >= cash_min_slates && summ[family == "cash"]$cash_win >= 100 * cash_win_gate,
    gpp_enabled  = summ[family == "leverage"]$gpp_roi > 0,
    lev_gamma    = best$lev_gamma, validated_on = holdout_year,
    n_slates = n_cash, run_date = as.character(Sys.Date()))
  gates_path <- dfs_path("data", paste0("gates_", sport, ".rds"))
  dir.create(dirname(gates_path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(gates, gates_path)
  cat("\nGates written ->", gates_path, "\n"); str(gates)
  cat("\nNote: GPP ROI uses a representative payout + a simulated field — directional.",
      "\nThe cash gate is the trustworthy one. Lineups stay PAPER until a gate passes.\n")
  invisible(list(gates = gates, validation = V, tune = tune))
}

# Load a sport's gates (paper-trade defaults if none yet).
load_gates <- function(sport) {
  f <- dfs_path("data", paste0("gates_", sport, ".rds"))
  if (file.exists(f)) readRDS(f)
  else list(sport = sport, cash_enabled = FALSE, gpp_enabled = FALSE, lev_gamma = 6,
            note = "No backtest gates found -> PAPER TRADE ONLY")
}
