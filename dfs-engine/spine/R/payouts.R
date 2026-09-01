# ==============================================================================
# DFS ENGINE — payout curves
# Lineup EV must be graded in ROI against a REAL contest payout structure, not in
# points (the thing homebrew tools skip). A payout curve maps a finishing RANK
# (1 = best) over a field of N entries to a prize as a MULTIPLE of the entry fee.
#
# Built-ins are representative DK structures; when a logged contest_results row
# has a real payout, prefer that (load_contest_curve). Replaces the *invented*
# curve in the golf scripts (Golf/dfs_pipeline_v2.R `payout()`).
# ==============================================================================

DFS_RAKE <- 0.15   # typical DK GPP rake; double-ups closer to ~0.10

# ── cash / double-up ──────────────────────────────────────────────────────────
# Top `cash_frac` of the field each receive `payout` x entry; everyone else 0.
make_double_up <- function(cash_frac = 0.44, payout = 1.8) {
  structure(list(type = "cash", cash_frac = cash_frac, payout = payout),
            class = "dfs_payout")
}

# ── GPP / top-heavy ───────────────────────────────────────────────────────────
# Power-law prize pool over the paid top `paid_frac`, normalized so the pool pays
# out (1 - rake) of entries. `alpha` controls top-heaviness (higher = spikier 1st).
make_gpp <- function(paid_frac = 0.20, alpha = 1.0, rake = DFS_RAKE, min_cash = 1.5) {
  structure(list(type = "gpp", paid_frac = paid_frac, alpha = alpha,
                 rake = rake, min_cash = min_cash),
            class = "dfs_payout")
}

# Real DK payout: explicit prize-by-rank table (the trustworthy curve).
# prizes: data.frame(min_rank, max_rank, prize). field_size + entry_fee from contest.
make_table_payout <- function(prizes, field_size, entry_fee) {
  N <- as.integer(field_size); mult <- numeric(N)
  for (i in seq_len(nrow(prizes))) {
    rng <- seq.int(prizes$min_rank[i], min(prizes$max_rank[i], N))
    mult[rng] <- prizes$prize[i] / entry_fee
  }
  structure(list(type = "table", mult = mult, field_size = N, entry_fee = entry_fee),
            class = "dfs_payout")
}

# Vector of prize multiples indexed by rank 1..N (0 for non-cashers).
payout_multipliers <- function(curve, field_size) {
  N <- as.integer(field_size)
  if (curve$type == "table") {
    m <- curve$mult
    if (length(m) >= N) return(m[seq_len(N)])
    return(c(m, numeric(N - length(m))))
  }
  mult <- numeric(N)
  if (curve$type == "cash") {
    k <- max(1L, floor(curve$cash_frac * N))
    mult[seq_len(k)] <- curve$payout
    return(mult)
  }
  # gpp
  K <- max(1L, floor(curve$paid_frac * N))
  w <- 1 / (seq_len(K))^curve$alpha            # raw top-heavy weights
  pool <- N * (1 - curve$rake)                 # total multiples to distribute
  m <- w / sum(w) * pool
  # enforce a sane min-cash floor, then renormalize to the pool
  m <- pmax(m, curve$min_cash)
  m <- m / sum(m) * pool
  mult[seq_len(K)] <- m
  mult
}

# ROI (= prize multiple - 1) for a lineup finishing at integer `rank` in `field_size`.
# Vectorized over rank. Duplicate lineups split the prize -> pass n_dupes.
grade_roi <- function(rank, curve, field_size, n_dupes = 1) {
  mult <- payout_multipliers(curve, field_size)
  rank <- pmin(pmax(as.integer(rank), 1L), length(mult))
  mult[rank] / n_dupes - 1
}

# Look up a real payout for a logged contest; fall back to a representative curve.
load_contest_curve <- function(contest_id = NULL, type = c("gpp", "cash")) {
  type <- match.arg(type)
  # (placeholder for real payout ingestion from contest_results / DK structure CSVs;
  #  until then, representative curves keyed by type)
  if (type == "cash") make_double_up() else make_gpp()
}
