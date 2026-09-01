# ==============================================================================
# DFS ENGINE — lineup evaluation (EV against the simulated field + a payout curve)
# For each candidate lineup, compute its outcome distribution across sims and its
# finish AGAINST the simulated field, then apply a payout curve to get expected ROI
# (not expected points). Cash: P(beat the cash line). GPP: tournament EV from field
# rank, with duplication splitting prizes.
# NOTE: the curve is whatever the caller passes. A specific DK contest supplies the
# REAL prize-by-rank table (make_table_payout); the generic daily card defaults to
# REPRESENTATIVE curves (make_gpp/make_double_up), so its EV is INDICATIVE, not a
# real-payout claim — and stays PAPER until a backtest gate passes.
# Generalizes the grading in Golf/dfs_pipeline_v2.R simulate_slate().
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# cands: list(list(idx, family)). sim: from slate_sim(). field: from simulate_field().
# curve_gpp / curve_cash: dfs_payout objects. field_size: real contest size for ROI
# (defaults to the simulated field size).
grade_candidates <- function(cands, sim, field, curve_gpp = make_gpp(),
                             curve_cash = make_double_up(), field_size = NULL) {
  scores <- sim$scores; pool <- sim$pool
  n_sims <- ncol(scores)
  field_mat <- score_field(field, scores)             # field_n × n_sims
  fld_med   <- apply(field_mat, 2, median)            # per-sim cash line
  if (is.null(field_size)) field_size <- length(field$idx)
  own <- if ("own" %in% names(pool)) pmax(pool$own, 1e-3) else rep(0.05, nrow(pool))

  rbindlist(lapply(cands, function(cd) {
    ix  <- cd$idx
    tot <- colSums(scores[ix, , drop = FALSE])        # n_sims
    # fraction of field beaten, per sim (vectorized)
    beaten <- field_mat <= matrix(tot, nrow(field_mat), n_sims, byrow = TRUE)
    pct <- colMeans(beaten)
    rank <- pmax(1L, round((1 - pct) * field_size))
    key  <- paste(ix, collapse = "-")
    dupes <- sum(field$keys == key) + 1L              # we are one of the dupes
    data.table(
      family   = cd$family,
      idx      = list(ix),
      proj     = sum(pool$proj[ix]),
      salary   = sum(pool$salary[ix]),
      ceil     = sum((if ("ceil" %in% names(pool)) pool$ceil else pool$proj)[ix]),
      avg_own  = mean(own[ix]),
      dupe_idx = prod(own[ix]) * 1e6,                 # relative dupe risk (golf metric)
      p_cash   = mean(tot >= fld_med),
      gpp_ev   = mean(grade_roi(rank, curve_gpp, field_size, dupes)),
      cash_ev  = mean(grade_roi(rank, curve_cash, field_size)),
      p_top1   = mean(pct >= 0.99))
  }))
}
