# ==============================================================================
# DFS ENGINE — portfolio construction
# Select a DIVERSIFIED set of N lineups spanning cash -> leverage, with a pairwise
# overlap cap so multi-entry players get genuinely different builds. Carries the
# gates (cash_enabled / gpp_enabled): lineups are LIVE only for contest types that
# passed their backtest gate, else PAPER. Lifted from Golf/dfs_pipeline_v2.R
# build_portfolio() and made sport-agnostic.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# res: from grade_candidates(). gates: list(cash_enabled, gpp_enabled). n: portfolio
# size. max_overlap: max shared players between any two emitted lineups.
build_portfolio <- function(res, gates = list(cash_enabled = FALSE, gpp_enabled = FALSE),
                            n = 8L, max_overlap = NULL) {
  if (is.null(max_overlap)) {
    rsize <- length(res$idx[[1]]); max_overlap <- max(1L, rsize - 2L)
  }
  picks <- list()
  is_dupe    <- function(idx) any(vapply(picks, function(p) setequal(p$idx[[1]], idx), logical(1)))
  overlap_ok <- function(idx) all(vapply(picks, function(p)
                  length(intersect(idx, p$idx[[1]])) <= max_overlap, logical(1)))
  take <- function(pool, role, live, reason_fn) {
    for (i in seq_len(nrow(pool))) {
      row <- pool[i]
      if (is_dupe(row$idx[[1]]) || !overlap_ok(row$idx[[1]])) next
      picks[[length(picks) + 1]] <<- c(as.list(row), role = role, live = live,
                                       reason = reason_fn(row))
      return(TRUE)
    }
    FALSE
  }

  n_cash <- max(1L, round(n * 0.25))
  n_lev  <- max(1L, round(n * 0.375))
  n_bal  <- n - n_cash - n_lev

  # NOTE (2026-08-17): a hard low-ownership cap on GPP builds was tested and REVERTED —
  # it tanked EV (WNBA field-sim GPP EV +2.55 -> -0.27, top-1% 4.3% -> 1.1%) because chalk
  # studs ARE the ceiling and grade_candidates' gpp_ev already trades ownership vs ceiling
  # optimally (it penalises duplication). Blanket "go contrarian" is wrong; we trust gpp_ev.
  cashpool <- res[order(-p_cash)]
  for (k in seq_len(n_cash)) take(cashpool, "Cash anchor", gates$cash_enabled, function(r) sprintf(
    "Cash build: beats the field median in %.0f%% of sims (breakeven ~55.6%% after rake). %s.",
    100 * r$p_cash, if (isTRUE(gates$cash_enabled)) "Cash gate PASSED" else "paper only"))

  balpool <- res[family %in% c("balanced", "cash", "value")][order(-gpp_ev)]
  for (k in seq_len(n_bal)) take(balpool, "Balanced GPP", gates$gpp_enabled, function(r) sprintf(
    "Balanced GPP: sim EV %.0f%% (indicative), avg own %.0f%%, dupe idx %.1f, top-1%% in %.1f%% of sims. %s.",
    100 * r$gpp_ev, 100 * r$avg_own, r$dupe_idx, 100 * r$p_top1,
    if (isTRUE(gates$gpp_enabled)) "GPP gate PASSED" else "paper only"))

  levpool <- res[order(-p_top1)]
  for (k in seq_len(n_lev)) take(levpool, "Leverage GPP", gates$gpp_enabled, function(r) sprintf(
    "Leverage build: wins top-1%% in %.1f%% of sims at avg own %.0f%% (contrarian). High variance. %s.",
    100 * r$p_top1, 100 * r$avg_own, if (isTRUE(gates$gpp_enabled)) "GPP gate PASSED" else "paper only"))

  backfill <- res[order(-gpp_ev)]
  for (i in seq_len(nrow(backfill))) {
    if (length(picks) >= n) break
    take(backfill[i], "Balanced GPP", gates$gpp_enabled, function(r) sprintf(
      "Portfolio fill: sim EV %.0f%%, top-1%% %.1f%%. %s.",
      100 * r$gpp_ev, 100 * r$p_top1, if (isTRUE(gates$gpp_enabled)) "GPP gate PASSED" else "paper only"))
  }
  msg("  Portfolio lineups built:", length(picks))
  picks
}

# 20-ENTRY LARGE-GPP mode. Backtest-tuned on ACTUAL results (2026-08-23, 24 settled golf
# slates): for a fully-entered 20-lineup set, ranking by PROJECTION with a moderate per-player
# exposure cap reaches the highest actual ceiling — ceiling/leverage weighting HURT (−2.3%,
# chases noise) and the exposure cap barely mattered. Unlike the 150-max spread, a 20-set
# concentrates on the best projections with only light self-uniqueness (no forced contrarian
# fades — those can't be validated and cost EV, per P2b). Returns the same shape as build_portfolio.
build_gpp20 <- function(res, gates = list(gpp_enabled = FALSE), n = 20L, exp_cap = 0.50,
                        pool = NULL, caps = NULL) {
  R <- res[order(-proj)]
  base_cap <- max(1L, floor(n * exp_cap))
  # PER-PLAYER cap overrides (injury/news): caps = named vector player_name -> max exposure frac.
  # Order-insensitive name match (First Last vs "Last, First"). Only lowers a player below base_cap.
  .nkey <- function(x) vapply(as.character(x), function(s) {
    t <- strsplit(gsub("[^a-z ]", " ", tolower(s)), "\\s+")[[1]]; t <- t[nzchar(t)]
    paste(sort(t), collapse = " ") }, character(1))
  # exposure KEY per pool row: the underlying player. For Showdown/Captain pools the same
  # golfer appears as a CPT row AND a FLEX row (distinct player_id) but shares `base_id` —
  # key on base_id so a per-player cap counts CPT+FLEX together (one golfer, one exposure).
  P0   <- if (!is.null(pool)) as.data.table(pool) else NULL
  ekey <- if (!is.null(P0) && "base_id" %in% names(P0)) as.character(P0$base_id) else NULL
  keyf <- function(j) if (is.null(ekey)) as.character(j) else ekey[j]
  pcap <- integer(0)   # exposure key -> override cap count
  if (!is.null(pool) && !is.null(caps) && length(caps)) {
    P <- P0 %||% as.data.table(pool); nmcol <- if ("player_name" %in% names(P)) "player_name" else names(P)[1]
    ck <- .nkey(names(caps)); cv <- as.numeric(caps); pk <- .nkey(P[[nmcol]])
    for (j in seq_len(nrow(P))) { m <- match(pk[j], ck)
      if (!is.na(m)) pcap[keyf(j)] <- max(1L, floor(n * cv[m])) }
    if (length(pcap)) msg("  20-max GPP: player exposure caps applied to", length(pcap), "player(s)")
  }
  cap_of <- function(k) { v <- pcap[k]; if (is.na(v)) base_cap else v }
  picks <- list(); expo <- integer(0)
  cnt <- function(k) { v <- expo[k]; if (is.na(v)) 0L else v }
  is_dupe <- function(idx) any(vapply(picks, function(p) setequal(p$idx[[1]], idx), logical(1)))
  for (i in seq_len(nrow(R))) {
    if (length(picks) >= n) break
    idx <- R$idx[[i]]
    if (is_dupe(idx)) next
    if (!all(vapply(idx, function(j) cnt(keyf(j)) < cap_of(keyf(j)), logical(1)))) next
    picks[[length(picks) + 1]] <- c(as.list(R[i]), role = "20-max GPP", live = gates$gpp_enabled,
      reason = sprintf("20-entry GPP set (proj-ranked, %.0f%% max exposure): projected %.1f, avg own %.0f%%. %s.",
                       100 * exp_cap, R$proj[i], 100 * (R$avg_own[i] %||% 0),
                       if (isTRUE(gates$gpp_enabled)) "GPP gate PASSED" else "paper only"))
    for (j in idx) { k <- keyf(j); expo[k] <- cnt(k) + 1L }
  }
  msg("  20-max GPP lineups built:", length(picks))
  picks
}

# Load per-player exposure caps for the gpp20 build from config/exposure_overrides.json.
# Returns a named numeric vector (player_name -> max exposure frac) for `sport`, or NULL.
load_exposure_overrides <- function(sport) {
  f <- dfs_path("config", "exposure_overrides.json")
  if (!file.exists(f)) return(NULL)
  j <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  o <- if (!is.null(j)) j[[sport]] else NULL
  if (is.null(o) || !length(o)) return(NULL)
  v <- suppressWarnings(as.numeric(unlist(o))); names(v) <- names(o)
  v <- v[is.finite(v) & v > 0 & v <= 1]
  if (length(v)) v else NULL
}
