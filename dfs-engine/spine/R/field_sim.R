# ==============================================================================
# DFS ENGINE — field simulation (realistic opponent field)
# The part homebrew tools skip — and the reason their GPP numbers are fiction.
# A real GPP field is NOT random ownership draws: every entrant is TRYING to build a
# strong lineup. So we model each opponent as an OPTIMIZER of its own noised view of
# projections (proj x (1 + noise)) under the roster rules, with a pull toward popular
# (high-owned) players. Skill TIERS set the noise: sharps disagree little and cluster
# near the true optimum (they are hard to beat); casuals are noisy and off-optimal.
#
# Why this matters: if the field is a bag of random lineups, ANY decent lineup "wins"
# constantly and GPP EV/finish come out absurdly high. A field that also builds strong
# lineups is what makes cash%, top-1%, win% and ROI realistic — you must beat opponents
# who are near-optimal too. Duplication + marginal ownership EMERGE from the optimization
# (validated: golf dupe ~0.034 vs a real 63k-entry DK contest's 0.034), and chalk-optimal
# lineups correctly grade as NEGATIVE GPP EV (heavily duplicated), leverage as higher.
# Generalizes the field block of Golf/dfs_pipeline_v2.R simulate_slate().
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# Entrant tiers: (share of field, projection-noise sd as a FRACTION of projection,
# ownership `pull`). Each entrant optimizes proj*(1+N(0,noise)) plus a popularity term
# `pull*own` (in projection points). Lower noise = sharper (near-optimal, hard to beat).
# `pull` sign models STRATEGY: sharps FADE chalk (negative pull -> leverage builds, so the
# contrarian space is populated and a user's leverage lineup isn't uniquely unopposed);
# casuals PILE ON popular names (positive pull -> the duplicated chalk that leverage fades).
# This mix is what makes both cash% AND GPP EV realistic — you compete against near-optimal
# lineups AND against other leverage plays. Validated: golf dupe ~ real 63k-entry contest;
# chalk-max grades NEGATIVE GPP EV, leverage modestly positive (not the old +900% fiction).
FIELD_TIERS <- list(
  list(w = 0.18, noise = 0.05, pull = -0.4),   # sharp / contrarian — near-optimal, fades chalk
  list(w = 0.34, noise = 0.11, pull =  0.3),   # good
  list(w = 0.28, noise = 0.18, pull =  0.6),   # mid — chases popular names
  list(w = 0.20, noise = 0.30, pull =  0.9))   # casual — piles on chalk (drives duplication)

# Build ONE competitive, roster-valid lineup by optimizing the entrant's value vector
# `val` (a noised projection) under the roster rules: greedy top-by-value per slot, then
# cheapest-value-loss swaps down to the salary cap. Honors positional slots + FLEX,
# team_limit, max_per_game and group_unique (Showdown CPT/FLEX same-player) constraints.
# Returns sorted row indices, or NULL if it can't build a legal lineup.
field_opt_lineup <- function(pool, rr, val) {
  P <- nrow(pool); sal <- pool$salary; cap <- rr$cap
  team <- if ("team" %in% names(pool)) pool$team else rep(NA_character_, P)
  gid  <- if ("game_id" %in% names(pool)) pool$game_id else rep(NA_character_, P)
  grp  <- if (!is.null(rr$group_unique) && rr$group_unique %in% names(pool)) as.character(pool[[rr$group_unique]]) else NULL
  tlim <- rr$team_limit; glim <- rr$max_per_game
  has_con <- !is.null(grp) || !is.null(tlim) || !is.null(glim)

  # roles = ordered list of (eligible indices, count needed)
  roles <- list()
  if (is.null(rr$slots)) roles[[1L]] <- list(elig = seq_len(P), need = rr$n)
  else {
    for (pos in names(rr$slots)) roles[[length(roles) + 1L]] <-
      list(elig = which(!is.na(pool$position) & pool$position == pos), need = rr$slots[[pos]])
    if (!is.null(rr$flex) && (rr$flex$count %||% 0) > 0)
      roles[[length(roles) + 1L]] <- list(
        elig = which(!is.na(pool$position) & pool$position %in% rr$flex$positions), need = rr$flex$count)
  }

  sel <- integer(0); role_of <- integer(0); used_grp <- character(0)
  ok_add <- function(i) {
    if (!is.null(grp) && grp[i] %in% used_grp) return(FALSE)
    if (!is.null(tlim) && !is.na(team[i]) && sum(team[sel] == team[i], na.rm = TRUE) >= tlim) return(FALSE)
    if (!is.null(glim) && !is.na(gid[i])  && sum(gid[sel]  == gid[i],  na.rm = TRUE) >= glim) return(FALSE)
    TRUE
  }
  for (ri in seq_along(roles)) {
    cand <- roles[[ri]]$elig; cand <- cand[order(-val[cand])]
    need <- roles[[ri]]$need; filled <- 0L
    for (i in cand) {
      if (i %in% sel) next
      if (has_con && !ok_add(i)) next
      sel <- c(sel, i); role_of <- c(role_of, ri)
      if (!is.null(grp)) used_grp <- c(used_grp, grp[i])
      filled <- filled + 1L; if (filled == need) break
    }
    if (filled < need) return(NULL)
  }

  # repair down to the cap: swap a selected player for the highest-value cheaper one in
  # the same role (least projection lost per dollar saved), keeping all constraints.
  it <- 0L
  while (sum(sal[sel]) > cap && it < 100L) {
    it <- it + 1L; best <- NULL
    for (p in seq_along(sel)) {
      i <- sel[p]; ri <- role_of[p]
      cand <- roles[[ri]]$elig
      cand <- cand[!(cand %in% sel) & sal[cand] < sal[i]]
      if (!length(cand)) next
      if (has_con) {
        others <- sel[-p]
        keep <- vapply(cand, function(j) {
          if (!is.null(grp) && grp[j] %in% grp[others]) return(FALSE)
          if (!is.null(tlim) && !is.na(team[j]) && sum(team[others] == team[j], na.rm = TRUE) >= tlim) return(FALSE)
          if (!is.null(glim) && !is.na(gid[j])  && sum(gid[others]  == gid[j],  na.rm = TRUE) >= glim) return(FALSE)
          TRUE }, logical(1))
        cand <- cand[keep]; if (!length(cand)) next
      }
      j <- cand[which.max(val[cand])]
      ratio <- (val[i] - val[j]) / (sal[i] - sal[j])       # proj lost per $ saved (minimize)
      if (is.null(best) || ratio < best$r) best <- list(p = p, j = j, r = ratio)
    }
    if (is.null(best)) break
    sel[best$p] <- best$j
    if (!is.null(grp)) used_grp <- grp[sel]
  }
  if (sum(sal[sel]) > cap) return(NULL)
  sort(sel)
}

# Build a realistic field of `field_n` valid lineups as a mixture of skill tiers, each
# entrant optimizing its own noised projections (+ popularity pull). Returns
# list(idx = list of index vecs, keys = char keys for duplication tracking).
simulate_field <- function(pool, rr, field_n = 1500L, own = NULL, tiers = FIELD_TIERS) {
  pool <- as.data.table(pool)
  P <- nrow(pool); proj <- as.numeric(pool$proj)
  if (is.null(own)) own <- if ("own" %in% names(pool)) pmax(pool$own, 1e-3) else rep(0, P)
  own <- pmax(as.numeric(own), 0)
  med <- stats::median(proj[proj > 0], na.rm = TRUE); if (!is.finite(med) || med <= 0) med <- 1

  ws <- vapply(tiers, function(t) t$w, numeric(1)); ws <- ws / sum(ws)
  ns <- as.integer(round(field_n * ws)); ns[1] <- ns[1] + (field_n - sum(ns))   # fix rounding
  out <- vector("list", field_n); got <- 0L
  for (ti in seq_along(tiers)) {
    tau  <- tiers[[ti]]$noise %||% 0.15
    bonus <- (tiers[[ti]]$pull %||% 0) * own * med             # per-tier popularity pull (proj pts)
    made <- 0L; tries <- 0L; target <- ns[ti]
    while (made < target && tries < target * 8L + 50L) {
      tries <- tries + 1L
      val <- proj * (1 + rnorm(P, 0, tau)) + bonus
      ix <- field_opt_lineup(pool, rr, val)
      if (is.null(ix)) next
      got <- got + 1L; made <- made + 1L; out[[got]] <- ix
    }
  }
  idx <- out[seq_len(got)]
  list(idx = idx, keys = vapply(idx, paste, character(1), collapse = "-"))
}

# Realized ownership per pool row = fraction of field lineups containing it (for
# validation against actual %Drafted, and diagnostics).
field_ownership <- function(field, n_pool) {
  tab <- tabulate(unlist(field$idx), nbins = n_pool)
  tab / max(1L, length(field$idx))
}

# Score the field on the cached sim. Returns field_n × n_sims matrix of totals.
score_field <- function(field, scores) {
  if (length(field$idx) == 0) stop("empty field")
  t(vapply(field$idx, function(ix) colSums(scores[ix, , drop = FALSE]), numeric(ncol(scores))))
}
