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
#
# Default changed 2026-10-01: a bare `rsize-2` let two 9-man classic entries differ
# by only 2 of 9 players. Replaced with the SAME ~50%-shared cap validated on real
# showdown results (2026-09-28 ledger: median pair shared 43.8% of a 6-man roster,
# median max pair overlap exactly 5 of 6) -- generalized as floor(rsize/2) so every
# roster size inherits the same validated tightness from one place.
build_portfolio <- function(res, gates = list(cash_enabled = FALSE, gpp_enabled = FALSE),
                            n = 8L, max_overlap = NULL) {
  rsize <- length(res$idx[[1]])
  if (is.null(max_overlap)) max_overlap <- max(1L, floor(rsize / 2))
  cur_cap <- max_overlap
  picks <- list()
  is_dupe    <- function(idx) any(vapply(picks, function(p) setequal(p$idx[[1]], idx), logical(1)))
  overlap_ok <- function(idx) all(vapply(picks, function(p)
                  length(intersect(idx, p$idx[[1]])) <= cur_cap, logical(1)))
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

  # GUARANTEE the full portfolio size. A tight max_overlap combined with a thin
  # candidate pool can run this backfill dry before reaching `n` -- this is exactly
  # what happened on the real PIT@CLE showdown (2026-10-01): asked for 20, silently
  # got 15, nothing in the log said why, and 5 of those 15 were unplayable punt-only
  # lineups because the only remaining distinct combinations left in the pool were
  # built from scrubs. Relax the cap one player at a time and keep backfilling;
  # never relax past rsize-1 (that would let is_dupe() start blocking a genuine
  # duplicate instead of a merely-too-similar lineup).
  while (length(picks) < n && cur_cap < rsize - 1L) {
    cur_cap <- cur_cap + 1L
    before <- length(picks)
    for (i in seq_len(nrow(backfill))) {
      if (length(picks) >= n) break
      take(backfill[i], "Balanced GPP", gates$gpp_enabled, function(r) sprintf(
        "Portfolio fill (overlap relaxed to %d/%d): sim EV %.0f%%, top-1%% %.1f%%. %s.",
        cur_cap, rsize, 100 * r$gpp_ev, 100 * r$p_top1,
        if (isTRUE(gates$gpp_enabled)) "GPP gate PASSED" else "paper only"))
    }
    if (length(picks) > before) {
      msg(sprintf(
        "  Portfolio: relaxed max_overlap %d -> %d/%d to backfill %d more (candidate pool too thin at the tighter cap -- consider raising n_cand)",
        max_overlap, cur_cap, rsize, length(picks) - before))
    } else {
      # A LOOSER cap found nothing new in this SAME static pool -- looser still
      # won't help either (the pool is exhausted, not the cap too tight), and
      # silently continuing would keep ratcheting cur_cap up to rsize-2 with
      # nothing logged. Stop here so cur_cap reflects the tightest cap that
      # actually did any work, which is what the log above claims it is.
      cur_cap <- cur_cap - 1L
      break
    }
  }
  if (length(picks) < n)
    msg(sprintf(
      "  Portfolio: only %d of %d requested lineups exist as distinct combinations in the candidate pool -- raise n_cand.",
      length(picks), n))

  msg("  Portfolio lineups built:", length(picks))
  picks
}

# 20-ENTRY LARGE-GPP mode. Backtest-tuned on ACTUAL results (2026-08-23, 24 settled golf
# slates): for a fully-entered 20-lineup set, ranking by PROJECTION with a moderate per-player
# exposure cap reaches the highest actual ceiling — ceiling/leverage weighting HURT (−2.3%,
# chases noise) and the exposure cap barely mattered. Unlike the 150-max spread, a 20-set
# concentrates on the best projections with only light self-uniqueness (no forced contrarian
# fades — those can't be validated and cost EV, per P2b). Returns the same shape as build_portfolio.
## DIVERSIFICATION LEVERS (max_overlap, min_sd_pct).
##
## min_sd_pct stays OFF by default -- tested 2026-09, counterproductive: it cut a
## portfolio to 10 of 20 lineups and pushed overlap UP to 3.84 while narrowing the
## projection range to 1.1. Do not re-enable without new evidence.
##
## max_overlap now defaults ON (changed 2026-10-01, by explicit direction, applied
## to every sport's regular-slate 20-entry build, not just showdown). Measured
## 2026-09-28 on the real entry ledger: across NFL contests the median pair of our
## own entries shared 43.8% of the roster and the median MAXIMUM pair overlap was 5
## of 6 players -- two entries routinely differing by one player. Outcomes agree: 8
## entries in the ATL@GB showdown finished inside a 25.7-point range, and 10 WNBA
## entries inside 1.5 points. That is one bet wearing several hats. The showdown fix
## (cap = 3 of 6, i.e. ~50% shared) already shipped and is generalized here as
## floor(rsize/2) so every roster size inherits the same validated tightness.
##
## This does NOT touch the validated ranking: the 24-settled-golf-slate backtest
## (2026-08-23) that showed ceiling/leverage ranking LOSES to proj-ranking never
## varied pairwise overlap, so this floor rides alongside proj-ranking, not instead
## of it. Pass max_overlap = Inf to disable and get the old unconstrained behaviour.
##
## Hunter/Vielma/Zaman (arXiv 1604.01455) build each successive entry by maximising
## expected score subject to (a) an upper bound on correlation with already-chosen
## entries and (b) a lower bound on its own variance. `max_overlap` is (a) in its
## combinatorial form; `min_sd_pct` is (b), as a quantile of the candidate pool's own
## sim_sd so it travels across sports and slate sizes.
build_gpp20 <- function(res, gates = list(gpp_enabled = FALSE), n = 20L, exp_cap = 0.50,
                        pool = NULL, caps = NULL, max_overlap = NULL, min_sd_pct = NULL,
                        rr = NULL) {
  R <- res[order(-proj)]
  rsize <- length(R$idx[[1]])
  if (!is.null(min_sd_pct) && "sim_sd" %in% names(R)) {
    floor_sd <- stats::quantile(R$sim_sd, min_sd_pct, na.rm = TRUE)
    keep <- !is.finite(R$sim_sd) | R$sim_sd >= floor_sd
    msg(sprintf("  20-max GPP: variance floor at p%02d drops %d of %d candidates",
                round(100 * min_sd_pct), sum(!keep), nrow(R)))
    R <- R[keep]
  }
  if (is.null(max_overlap)) max_overlap <- max(1L, floor(rsize / 2))
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
  cur_cap <- max_overlap
  overlap_ok <- function(idx) all(vapply(picks, function(p)
                  length(intersect(idx, p$idx[[1]])) <= cur_cap, logical(1)))

  try_fill <- function(label_suffix = "") {
    added <- 0L
    for (i in seq_len(nrow(R))) {
      if (length(picks) >= n) break
      idx <- R$idx[[i]]
      if (is_dupe(idx)) next
      if (!overlap_ok(idx)) next
      if (!all(vapply(idx, function(j) cnt(keyf(j)) < cap_of(keyf(j)), logical(1)))) next
      picks[[length(picks) + 1]] <<- c(as.list(R[i]), role = "20-max GPP", live = gates$gpp_enabled,
        reason = sprintf("20-entry GPP set (proj-ranked, %.0f%% max exposure%s): projected %.1f, avg own %.0f%%. %s.",
                         100 * exp_cap, label_suffix, R$proj[i], 100 * (R$avg_own[i] %||% 0),
                         if (isTRUE(gates$gpp_enabled)) "GPP gate PASSED" else "paper only"))
      for (j in idx) { k <- keyf(j); expo[k] <<- cnt(k) + 1L }
      added <- added + 1L
    }
    added
  }

  try_fill()

  # GUARANTEE the full portfolio size. Never relax the exposure caps (those are real
  # risk limits, e.g. "don't play this QB in more than half the entries") -- only the
  # pairwise-overlap floor, one player at a time, logging every relaxation so a thin
  # slate is visible instead of silently shipping fewer entries than requested.
  while (length(picks) < n && cur_cap < rsize - 1L) {
    cur_cap <- cur_cap + 1L
    added <- try_fill(sprintf(", overlap relaxed to %d/%d", cur_cap, rsize))
    if (added > 0L) {
      msg(sprintf(
        "  20-max GPP: relaxed max_overlap %d -> %d/%d to backfill %d more (candidate pool too thin at the tighter cap -- consider raising n_cand)",
        max_overlap, cur_cap, rsize, added))
    } else {
      # A LOOSER cap found nothing new in this SAME static pool (the pool is
      # exhausted, not the cap too tight) -- stop ratcheting cur_cap rather than
      # silently climbing to rsize-2 with nothing logged. The exposure-aware
      # direct solve below picks up from here at the tightest cap that actually
      # helped, not the loosest one the loop happened to reach.
      cur_cap <- cur_cap - 1L
      break
    }
  }
  # DETERMINISTIC exposure-aware backfill, the final guarantee. The static candidate
  # pool above can run dry on EXPOSURE (not overlap) even though legal lineups still
  # exist, because random perturbation sampling has no way to guarantee it tries the
  # SPECIFIC remaining combination once several players are already at cap. Measured
  # on a real NFL classic slate (427 players): 7 players appeared in literally every
  # one of the top-20 proj-ranked candidates, all hit cap by the 10th lineup, and
  # 100% of the other 290 "distinct" candidates STILL touched one of them -- no
  # amount of resampling that static pool would have reached 20. So when the pool
  # runs dry and `rr` (roster rules) is available, stop resampling and SOLVE the
  # actual remaining problem directly: re-run the ILP with every already-capped
  # player's row forced to zero. This is guaranteed correct whenever a legal lineup
  # still exists; the only failure mode is genuine infeasibility, which means no
  # more exposure-legal lineups exist at all -- a real answer, not a bug.
  truly_infeasible <- FALSE
  if (length(picks) < n && !is.null(pool) && !is.null(rr)) {
    Pz <- P0 %||% as.data.table(pool)
    base_con <- build_constraints(Pz, rr)
    added <- 0L; tries <- 0L; dup_streak <- 0L
    max_tries <- max(60L, (n - length(picks)) * 12L)
    rediscover_budget <- 30L; n_rediscovered <- 0L
    while (length(picks) < n && tries < max_tries) {
      tries <- tries + 1L
      maxed <- names(expo)[vapply(names(expo), function(k) cnt(k) >= cap_of(k), logical(1))]
      con2 <- base_con
      if (length(maxed)) {
        excl <- which(keyf(seq_len(nrow(Pz))) %in% maxed)
        if (length(excl)) {
          ind <- rep(0, nrow(Pz)); ind[excl] <- 1
          con2$mat <- rbind(con2$mat, ind); con2$dir <- c(con2$dir, "="); con2$rhs <- c(con2$rhs, 0)
        }
      }
      # Enforce the overlap cap DIRECTLY in the solve (one row per existing pick)
      # instead of solving unconstrained and rejecting afterward -- every solution
      # is then overlap-legal by construction, so no try is ever wasted on an
      # overlap violation (only genuine exposure exhaustion can still waste one).
      # SKIP when cur_cap is Inf (the explicit disable sentinel) -- lpSolve cannot
      # accept Inf as a constraint RHS (crashes: "NA/NaN/Inf in foreign function
      # call"), and no constraint is exactly the semantics overlap_ok(idx) already
      # gives for Inf (<= Inf is always TRUE).
      if (is.finite(cur_cap)) {
        for (p in picks) {
          ind <- rep(0, nrow(Pz)); ind[p$idx[[1]]] <- 1
          con2$mat <- rbind(con2$mat, ind); con2$dir <- c(con2$dir, "<="); con2$rhs <- c(con2$rhs, cur_cap)
        }
      }
      # escalating jitter, same idea as make_candidates' dup_streak scaling -- a
      # fixed tiny jitter can get stuck rediscovering the same few alternatives
      # when many players are already excluded and the feasible region is small.
      scale <- min(0.5, 0.02 + 0.04 * dup_streak)
      noise <- rnorm(nrow(Pz), 0, scale * stats::sd(Pz$proj))
      idx <- ilp_solve(Pz$proj + noise, Pz, rr, con2)
      if (is.null(idx)) { truly_infeasible <- TRUE; break }   # no more exposure+overlap-legal lineup exists
      if (is_dupe(idx)) {
        dup_streak <- dup_streak + 1L
        # the jitter alone didn't escape an already-rejected combo -- force it, same
        # fix as make_candidates' EXCLUDE_BUDGET, so tries are never wasted on a
        # lineup already known to be unusable.
        if (n_rediscovered < rediscover_budget) {
          ind <- rep(0, nrow(Pz)); ind[idx] <- 1
          base_con$mat <- rbind(base_con$mat, ind); base_con$dir <- c(base_con$dir, "<=")
          base_con$rhs <- c(base_con$rhs, rr$n - 1L)
          n_rediscovered <- n_rediscovered + 1L
        }
        next
      }
      picks[[length(picks) + 1]] <- list(idx = list(idx), role = "20-max GPP",
        live = gates$gpp_enabled, salary = sum(Pz$salary[idx]), proj = sum(Pz$proj[idx]),
        avg_own = mean((if ("own" %in% names(Pz)) Pz$own else rep(0, nrow(Pz)))[idx]),
        reason = sprintf("20-entry GPP set (exposure-aware direct solve -- the static candidate pool ran dry on exposure caps): projected %.1f. %s.",
                         sum(Pz$proj[idx]), if (isTRUE(gates$gpp_enabled)) "GPP gate PASSED" else "paper only"))
      for (j in idx) { k <- keyf(j); expo[k] <- cnt(k) + 1L }
      added <- added + 1L; dup_streak <- 0L
    }
    if (added > 0L)
      msg(sprintf("  20-max GPP: exposure-aware direct solve backfilled %d more (static candidate pool was exhausted, not actually infeasible)", added))
  }

  if (length(picks) < n) {
    msg(sprintf(
      if (truly_infeasible)
        "  20-max GPP: only %d of %d -- the ILP itself is infeasible under the current exposure caps and overlap cap (genuine structural limit, confirmed, not a retry-budget issue)."
      else
        "  20-max GPP: only %d of %d -- stopped after exhausting the retry budget without a solver infeasibility, so a LARGER budget might find more (raise n_cand, or this slate's true ceiling may just be close to here).",
      length(picks), n))
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
