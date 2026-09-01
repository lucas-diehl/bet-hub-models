#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS PIPELINE v2 — live lineup generator
#
# Design contract: this script uses the SAME feature snapshots, SAME model
# spec, and SAME lineup strategies that dfs_backtest_v2.R validates. The
# backtest writes golf_picks/dfs_gates.rds; this script reads it and only
# recommends real-money entry for contest types that passed their gate.
# No gates file (or failed gates) => lineups are emitted as PAPER TRADE.
#
# Fixes vs dfs_model.R:
#   * Chronological, lagged (pre-event) features via wf_top20.R snapshots
#     — no frank(string) ordering, no current-event rounds in features.
#   * Real ceiling model: XGBoost quantile regression (alpha = 0.80) on DK
#     points, not a capped copy of the mean target.
#   * Ownership used for leverage scoring and duplication risk, not decoration.
#   * Monte Carlo slate simulation grades every candidate lineup on
#     P(beat field median), GPP percentile distribution, and dupe risk.
#   * Salary floor (>= $49,400 default), pairwise overlap <= 4 between
#     emitted lineups, ILP optimizer with floor+cap+count constraints.
#   * Survivorship fix: keep all finishers incl. low/zero-point weeks.
#   * Sane imputations (field-relative, not zeros).
#   * Model/DG blend weight comes from the backtest (whatever won on the
#     validation year), defaulting to 35% model / 65% DG until proven.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table); library(xgboost); library(openxlsx)
})

msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S]"), ..., "\n", sep = " ")

# ── CONFIG ────────────────────────────────────────────────────────────────────
API_KEY <- Sys.getenv("DATAGOLF_API_KEY")   # no hardcoded fallback. Set the env var.
stopifnot("Set DATAGOLF_API_KEY env var (and rotate the keys that were in old scripts)" =
          nchar(API_KEY) > 0)

OUTPUT_DIR   <- "golf_picks"
SNAPSHOTS    <- file.path(OUTPUT_DIR, "rich_snapshots.rds")      # rich features (list: snap, feats)
DFS_RAW      <- file.path(OUTPUT_DIR, "dfs_raw_data.rds")
GATES_FILE   <- file.path(OUTPUT_DIR, "dfs_gates.rds")           # from dfs_backtest_v2.R
SALARY_CAP   <- 50000L
SALARY_FLOOR <- 49400L
N_PLAYERS    <- 6L
N_SIMS       <- 8000L
N_CANDIDATES <- 400L
MAX_OVERLAP  <- 4L          # max shared players between any two portfolio lineups
N_LINEUPS    <- 8L          # portfolio size (target 7-10 multi-entry builds)
set.seed(2026)

Sys.setenv(WF_SOURCE_ONLY = "1")
source("claude/wf_top20.R")   # sourced for helpers; FEATS overridden below

# === model now runs on the RICH feature set (cor 0.32->0.38 vs the old wf FEATS) ===
FEATS <- readRDS(file.path(OUTPUT_DIR, "rich_selected_feats.rds"))  # 42 selected rich features
fill_consistent <- function(D) {   # generic median fill on whatever FEATS are present
  for (f in intersect(FEATS, names(D))) {
    md <- median(D[[f]], na.rm = TRUE); if (is.na(md)) md <- 0
    D[is.na(get(f)), (f) := md]
  }
  D
}

# ── API ───────────────────────────────────────────────────────────────────────
dg_get <- function(endpoint, params = list()) {
  params$key <- API_KEY; params$file_format <- "json"
  url <- paste0("https://feeds.datagolf.com/", endpoint)
  resp <- tryCatch(httr2::request(url) |> httr2::req_url_query(!!!params) |>
                     httr2::req_perform(), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) != 200) return(NULL)
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

# ── 1. TRAINING DATA (shared with backtest) ───────────────────────────────────
load_training <- function() {
  s <- as.data.table(readRDS(SNAPSHOTS)$snap)   # rich snapshot list -> $snap table
  d <- as.data.table(readRDS(DFS_RAW))
  s[, `:=`(event_id = as.character(event_id), year = as.integer(year))]
  d[, `:=`(event_id = as.character(event_id), year = as.integer(year),
           player_id = as.integer(dg_id))]
  # SURVIVORSHIP FIX: keep all rows with a recorded salary, incl. <=0 points
  dfs <- d[!is.na(total_pts) & !is.na(salary) & salary > 0,
           .(event_id, year, player_id, salary = as.numeric(salary),
             total_pts = as.numeric(total_pts))]
  dfs <- dfs[, .SD[1], by = .(event_id, year, player_id)]
  D <- merge(dfs, s[, c("event_id","year","player_id", FEATS), with = FALSE],
             by = c("event_id","year","player_id"))
  D <- fill_consistent(D)
  msg("Training rows: ", nrow(D), " (", uniqueN(paste(D$event_id, D$year)), " slates)")
  D
}

# ── 2. MODELS: mean + true quantile ceiling ───────────────────────────────────
xgb_mat <- function(dt) xgb.DMatrix(as.matrix(dt[, FEATS, with = FALSE]),
                                    label = dt$total_pts)

train_models <- function(D) {
  dtrain <- xgb_mat(D)
  base <- list(eta = 0.04, max_depth = 5, min_child_weight = 12,
               subsample = 0.8, colsample_bytree = 0.8, nthread = 2)
  msg("Training mean model...")
  m_mean <- xgb.train(c(base, objective = "reg:squarederror"), dtrain, nrounds = 600)
  msg("Training ceiling model (quantile alpha = 0.80)...")
  m_ceil <- tryCatch(
    xgb.train(c(base, objective = "reg:quantileerror", quantile_alpha = 0.80),
              dtrain, nrounds = 600),
    error = function(e) { msg("  quantile objective unavailable (need xgboost >= 2.0); ",
                              "falling back to mean + 0.84*residual_sd"); NULL })
  # residual sd by salary tier (for simulation + ceiling fallback)
  D[, pred_mean := predict(m_mean, xgb_mat(D))]
  D[, sal_tier := cut(salary, c(0, 6500, 7500, 8500, 9500, Inf))]
  tier_sd <- D[, .(resid_sd = sd(total_pts - pred_mean)), by = sal_tier]
  list(mean = m_mean, ceil = m_ceil, tier_sd = tier_sd)
}

# ── 3. CURRENT SLATE ──────────────────────────────────────────────────────────
get_slate <- function(tour = "pga", slate = "main") {
  raw <- dg_get("preds/fantasy-projection-defaults",
                list(tour = tour, site = "draftkings", slate = slate))
  stopifnot("Could not fetch current DFS slate" = !is.null(raw) && "projections" %in% names(raw))
  p <- as.data.table(raw$projections)
  p[, `:=`(player_id = as.integer(dg_id), salary = as.numeric(salary),
           dg_proj  = as.numeric(proj_points_total),
           # amateurs/qualifiers (majors) can have NA ownership -> default, not NA
           own      = pmax(fcoalesce(as.numeric(proj_ownership), 0.1), 0.1) / 100,
           dg_sd    = as.numeric(std_dev))]
  list(event = raw$event_name,
       players = p[!is.na(salary) & salary > 0,
                   .(player_id, player_name, salary, dg_proj, own, dg_sd)])
}

attach_features <- function(slate_players, D) {
  # latest pre-event snapshot per player = most recent training-row features.
  # Snapshots are already lagged (pre-event), so the latest row's features
  # include that player's most recent COMPLETED event. No off-by-one here:
  # wf snapshots store state entering each event; we take the newest and the
  # current event is not in them by construction.
  latest <- D[order(year, event_id), .SD[.N], by = player_id][, c("player_id", FEATS), with = FALSE]
  sl <- merge(slate_players, latest, by = "player_id", all.x = TRUE)
  n_missing <- sum(is.na(sl$sg_last_24))
  if (n_missing) msg("  ", n_missing, " slate players lack feature history -> field-relative fill")
  # field-relative imputation (NOT zeros): unknown players ~ weak-field level
  med <- function(col) median(sl[[col]], na.rm = TRUE)
  for (f in FEATS) {
    fallback <- if (grepl("^sg_", f)) med(f) - 0.25 else med(f)
    sl[is.na(get(f)), (f) := fallback]
  }
  sl
}

score_slate <- function(sl, models, blend_w_model) {
  X <- xgb.DMatrix(as.matrix(sl[, FEATS, with = FALSE]))
  sl[, model_mean := predict(models$mean, X)]
  sl[, sal_tier := cut(salary, c(0, 6500, 7500, 8500, 9500, Inf))]
  sl <- merge(sl, models$tier_sd, by = "sal_tier", all.x = TRUE)
  sl[is.na(resid_sd), resid_sd := mean(models$tier_sd$resid_sd)]
  sl[, model_ceil := if (!is.null(models$ceil)) predict(models$ceil, X)
                     else model_mean + 0.84 * resid_sd]
  # Blend with DG (weight set by backtest gates; conservative default)
  sl[, proj := ifelse(!is.na(dg_proj) & dg_proj > 0,
                      blend_w_model * model_mean + (1 - blend_w_model) * dg_proj,
                      model_mean)]
  sl[, sim_sd := fifelse(!is.na(dg_sd) & dg_sd > 0, 0.5 * dg_sd + 0.5 * resid_sd, resid_sd)]
  sl[, ceil_prem := pmax(model_ceil - model_mean, 0)]
  # Guarantee finite values reach the ILP / simulator (no NA/NaN/Inf in obj).
  fixc <- function(col, fb) { v <- sl[[col]]; if (any(!is.finite(v))) sl[!is.finite(get(col)), (col) := fb] }
  fixc("model_mean", median(sl$model_mean[is.finite(sl$model_mean)]))
  fixc("model_ceil", median(sl$model_ceil[is.finite(sl$model_ceil)]))
  fixc("proj",       median(sl$proj[is.finite(sl$proj)]))
  fixc("ceil_prem", 0); fixc("own", 0.001); fixc("sim_sd", median(sl$sim_sd[is.finite(sl$sim_sd)]))
  # leverage: ceiling rank vs ownership rank (positive = field underweights upside)
  sl[, leverage := frank(model_ceil) / .N - own]
  sl[!is.finite(leverage), leverage := 0]
  sl[]
}

# ── 4. OPTIMIZER (ILP: cap + floor + count) ───────────────────────────────────
ilp_lineup <- function(obj, salary, cap = SALARY_CAP, floor = SALARY_FLOOR, n = N_PLAYERS) {
  if (!requireNamespace("lpSolve", quietly = TRUE)) stop("install.packages('lpSolve')")
  sol <- lpSolve::lp("max", obj,
                     const.mat = rbind(salary, salary, rep(1, length(obj))),
                     const.dir = c("<=", ">=", "="),
                     const.rhs = c(cap, floor, n), all.bin = TRUE)
  if (sol$status != 0) return(NULL)
  which(as.logical(sol$solution))
}

# Candidate pool: perturbed objectives spanning cash -> leverage space.
# Noise is sized to actually move the ILP optimum so we get a DIVERSE pool even
# when ownership isn't published yet (early week) and the leverage term is muted.
make_candidates <- function(sl, n_cand = N_CANDIDATES) {
  cands <- list(); seen <- character(0)
  P <- nrow(sl); mp <- mean(sl$proj)
  base_objs <- list(
    cash      = sl$proj,
    balanced  = sl$proj + 0.5 * sl$ceil_prem,
    leverage  = sl$proj + 0.9 * sl$ceil_prem - 6.0 * sl$own * mp,
    value     = sl$proj / (sl$salary / 1000)
  )
  # noise grows across attempts so later draws explore further from the optimum
  per <- ceiling(n_cand / length(base_objs))
  for (nm in names(base_objs)) {
    s_obj <- sd(base_objs[[nm]])
    for (k in seq_len(per)) {
      scale <- if (k == 1) 0 else min(0.35, 0.06 + 0.02 * (k %% 12))
      noise <- rnorm(P, 0, scale * s_obj)
      # occasionally fade a random chunk of the chalk to force contrarian players in
      if (k > 1 && k %% 3 == 0) {
        fade <- sample.int(P, max(2L, P %/% 12))
        noise[fade] <- noise[fade] - 0.20 * s_obj
      }
      idx <- ilp_lineup(base_objs[[nm]] + noise, sl$salary)
      if (is.null(idx)) next
      key <- paste(sort(idx), collapse = "-")
      if (key %in% seen) next
      seen <- c(seen, key)
      cands[[length(cands) + 1]] <- list(idx = idx, family = nm)
    }
  }
  msg("  Candidate lineups: ", length(cands))
  cands
}

# ── 5. SLATE SIMULATION ──────────────────────────────────────────────────────
# Player scores ~ Normal(proj, sim_sd). Field lineups sampled by ownership
# under the cap. Each candidate graded on: P(beat field median) [cash],
# percentile distribution + invented-payout EV [GPP, indicative], dupe risk.
simulate_slate <- function(sl, cands, n_sims = N_SIMS, field_n = 1500L) {
  P <- nrow(sl)
  scores <- matrix(rnorm(P * n_sims, sl$proj, sl$sim_sd), nrow = P)
  # field lineups
  fld <- vector("list", field_n); got <- 0L; tries <- 0L
  while (got < field_n && tries < field_n * 8L) {
    pick <- sample.int(P, N_PLAYERS, prob = sl$own); tries <- tries + 1L
    if (sum(sl$salary[pick]) <= SALARY_CAP) { got <- got + 1L; fld[[got]] <- pick }
  }
  fld <- fld[seq_len(got)]
  fld_mat <- vapply(fld, function(ix) colSums(scores[ix, , drop = FALSE]), numeric(n_sims))
  fld_med <- apply(fld_mat, 1, median)                       # per-sim cash line
  payout <- function(pct) fifelse(pct >= .999, 100, fifelse(pct >= .99, 20,
            fifelse(pct >= .97, 6, fifelse(pct >= .93, 3,
            fifelse(pct >= .85, 1.8, fifelse(pct >= .78, 1.2, 0))))))
  res <- rbindlist(lapply(cands, function(cd) {
    tot <- colSums(scores[cd$idx, , drop = FALSE])
    # fraction of field beaten, per sim
    pct <- vapply(seq_len(n_sims), function(s) mean(tot[s] >= fld_mat[s, ]), numeric(1))
    data.table(family   = cd$family,
               idx      = list(cd$idx),
               proj     = sum(sl$proj[cd$idx]),
               salary   = sum(sl$salary[cd$idx]),
               p_cash   = mean(tot >= fld_med),
               gpp_ev   = mean(payout(pct)) - 1,             # indicative only
               p_top1   = mean(pct >= 0.99),
               dupe_idx = prod(pmax(sl$own[cd$idx], 0.01)) * 1e6,  # relative dupe risk
               avg_own  = mean(sl$own[cd$idx]))
  }))
  res
}

# ── 6. CONTEST ROUTER ────────────────────────────────────────────────────────
load_gates <- function() {
  if (file.exists(GATES_FILE)) readRDS(GATES_FILE)
  else list(cash_enabled = FALSE, gpp_enabled = FALSE, blend_w_model = 0.35,
            note = "No backtest gates found -> PAPER TRADE ONLY")
}

# Build a DIVERSIFIED PORTFOLIO of N lineups spanning cash -> leverage, with a
# pairwise overlap cap so multi-entry players get genuinely different builds.
build_portfolio <- function(res, sl, gates, n = N_LINEUPS) {
  picks <- list()
  is_dupe   <- function(idx) any(vapply(picks, function(p)
                 setequal(p$idx[[1]], idx), logical(1)))
  overlap_ok<- function(idx) all(vapply(picks, function(p)
                 length(intersect(idx, p$idx[[1]])) <= MAX_OVERLAP, logical(1)))
  take <- function(pool, role, live, reason_fn) {
    for (i in seq_len(nrow(pool))) {
      row <- pool[i]
      if (is_dupe(row$idx[[1]]) || !overlap_ok(row$idx[[1]])) next
      picks[[length(picks) + 1]] <<- c(as.list(row), role = role,
                                       live = live, reason = reason_fn(row))
      return(TRUE)
    }
    FALSE
  }
  # role allocation across the portfolio: ~25% cash, ~37% leverage, rest balanced
  n_cash <- max(1L, round(n * 0.25))
  n_lev  <- max(1L, round(n * 0.375))
  n_bal  <- n - n_cash - n_lev

  cashpool <- res[order(-p_cash)]
  for (k in seq_len(n_cash)) take(cashpool, "Cash anchor", gates$cash_enabled, function(r) sprintf(
    "Cash build: beats the field median in %.0f%% of sims (breakeven ~55.6%% after rake). %s.",
    100 * r$p_cash, if (gates$cash_enabled) "Cash gate PASSED" else "paper only"))

  balpool <- res[family %in% c("balanced","cash","value")][order(-gpp_ev)]
  for (k in seq_len(n_bal)) take(balpool, "Balanced GPP", gates$gpp_enabled, function(r) sprintf(
    "Balanced GPP: sim EV %.0f%% (indicative), avg ownership %.0f%%, dupe index %.1f, top-1%% in %.1f%% of sims. %s.",
    100 * r$gpp_ev, 100 * r$avg_own, r$dupe_idx, 100 * r$p_top1,
    if (gates$gpp_enabled) "GPP gate PASSED" else "paper only"))

  levpool <- res[order(-p_top1)]
  for (k in seq_len(n_lev)) take(levpool, "Leverage GPP", gates$gpp_enabled, function(r) sprintf(
    "Leverage build: wins top-1%% in %.1f%% of sims at avg ownership %.0f%% (contrarian). High variance — min stakes. %s.",
    100 * r$p_top1, 100 * r$avg_own, if (gates$gpp_enabled) "GPP gate PASSED" else "paper only"))

  # backfill (if diversity blocked some roles) to reach N
  backfill <- res[order(-gpp_ev)]
  for (i in seq_len(nrow(backfill))) {
    if (length(picks) >= n) break
    take(backfill[i], "Balanced GPP", gates$gpp_enabled, function(r) sprintf(
      "Portfolio fill: sim EV %.0f%%, top-1%% %.1f%%. %s.",
      100 * r$gpp_ev, 100 * r$p_top1, if (gates$gpp_enabled) "GPP gate PASSED" else "paper only"))
  }
  msg("  Portfolio lineups built: ", length(picks))
  picks
}

# ── 7. OUTPUT ────────────────────────────────────────────────────────────────
write_output <- function(picks, sl, event, gates) {
  wb  <- createWorkbook()
  hdr <- createStyle(fontColour = "#FFFFFF", fgFill = "#1E4D78", textDecoration = "bold")
  n_lu <- length(picks)
  # portfolio exposure: how many of the N lineups each player appears in
  expo <- tabulate(unlist(lapply(picks, function(p) p$idx[[1]])), nbins = nrow(sl))
  sl[, lineup_exposure := expo]

  # 1. LINEUPS — the 7-10 builds, one row each
  addWorksheet(wb, "Lineups")
  lu <- rbindlist(lapply(seq_along(picks), function(i) { p <- picks[[i]]; data.table(
    Lineup   = i, Role = p$role,
    Live     = ifelse(isTRUE(p$live), "LIVE — gate passed", "PAPER ONLY"),
    Players  = paste(sl$player_name[p$idx[[1]]], collapse = ", "),
    Salary   = p$salary, Proj = round(p$proj, 1),
    Ceiling  = round(sum(sl$model_ceil[p$idx[[1]]]), 1),
    AvgOwn   = round(100 * p$avg_own, 1), P_Cash = round(100 * p$p_cash, 1),
    GPP_EV   = round(100 * p$gpp_ev, 1),  Top1_pct = round(100 * p$p_top1, 1),
    Reasoning = p$reason) }))
  writeData(wb, "Lineups", lu, headerStyle = hdr); setColWidths(wb, "Lineups", 1:ncol(lu), "auto")

  # 2. RANKINGS — full board so you can build your own
  addWorksheet(wb, "Rankings")
  rk <- sl[order(-proj), .(
    Player = player_name, Salary = salary, Proj = round(proj, 1),
    Value = round(proj / (salary / 1000), 2),          # points per $1k
    Ceiling = round(model_ceil, 1), CeilPrem = round(ceil_prem, 1),
    Floor = round(pmax(proj - 1.5 * sim_sd, 0), 1),
    ModelMean = round(model_mean, 1), DG_Proj = round(dg_proj, 1),
    Own_pct = round(100 * own, 1), Leverage = round(leverage, 3),
    Exposure_pct = round(100 * lineup_exposure / max(1L, n_lu)))]
  writeData(wb, "Rankings", rk, headerStyle = hdr); setColWidths(wb, "Rankings", 1:ncol(rk), "auto")

  f <- file.path(OUTPUT_DIR, paste0("dfs_plan_", gsub("[^A-Za-z0-9]", "_", event),
                                    "_", Sys.Date(), ".xlsx"))
  saveWorkbook(wb, f, overwrite = TRUE)
  msg("Saved: ", f)
  # console summary
  cat("\n================ DFS PORTFOLIO —", event, "  (", n_lu, "lineups) ================\n")
  if (!gates$cash_enabled && !gates$gpp_enabled)
    cat("** ALL CONTESTS GATED OFF — run dfs_backtest_v2.R; until gates pass,\n",
        "** these lineups are for paper tracking only.\n\n")
  for (i in seq_along(picks)) { p <- picks[[i]]
    cat(sprintf("#%d [%s] %s\n   %s\n   $%d | proj %.1f | ceil %.1f | own %.0f%%\n",
        i, ifelse(isTRUE(p$live), "LIVE", "PAPER"), p$role,
        paste(sl$player_name[p$idx[[1]]], collapse = ", "),
        p$salary, p$proj, sum(sl$model_ceil[p$idx[[1]]]), 100 * p$avg_own))
  }
  cat("\nFull player rankings on the 'Rankings' tab (build-your-own).\n")
}

# ── MAIN ──────────────────────────────────────────────────────────────────────
main <- function() {
  gates <- load_gates()
  msg("Gates: cash=", gates$cash_enabled, " gpp=", gates$gpp_enabled,
      " blend_w_model=", gates$blend_w_model)
  D      <- load_training()
  models <- train_models(D)
  slate  <- get_slate()
  msg("Slate: ", slate$event, " (", nrow(slate$players), " players)")
  sl     <- attach_features(slate$players, D)
  sl     <- score_slate(sl, models, gates$blend_w_model)
  cands  <- make_candidates(sl)
  res    <- simulate_slate(sl, cands)
  picks  <- build_portfolio(res, sl, gates, N_LINEUPS)
  write_output(picks, sl, slate$event, gates)
  invisible(picks)
}
# GOLF_DFS_SOURCE_ONLY=1 lets another script (e.g. the DFS ENGINE export) source
# these functions without running the full lineup pipeline (mirrors WF_SOURCE_ONLY).
if (!interactive() && !nzchar(Sys.getenv("GOLF_DFS_SOURCE_ONLY"))) main()
