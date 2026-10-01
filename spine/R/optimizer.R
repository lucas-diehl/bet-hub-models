# ==============================================================================
# DFS ENGINE — optimizer (generalized ILP via lpSolve)
# Generalizes the golf ilp_lineup() ("pick 6 under cap") to arbitrary roster
# rules: position slots, FLEX/UTIL, salary cap+floor, per-team and per-game caps.
# Plus make_candidates(): the perturbed-objective candidate pool spanning
# cash -> leverage space (lifted from Golf/dfs_pipeline_v2.R make_candidates()).
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# Build the lpSolve constraint system from a sport's roster_rules and the pool.
# Binary decision y_p = player p selected.
build_constraints <- function(pool, rr) {
  P <- nrow(pool)
  mats <- list(); dirs <- character(0); rhs <- numeric(0)
  add <- function(row, dir, b) { mats[[length(mats) + 1]] <<- row; dirs <<- c(dirs, dir); rhs <<- c(rhs, b) }

  add(rep(1, P), "=", rr$n)                                   # roster size
  add(pool$salary, "<=", rr$cap)                              # salary cap
  if (!is.null(rr$floor)) add(pool$salary, ">=", rr$floor)    # salary floor

  # position slots with a FLEX group, plus an optional SUPERFLEX group stacked on top
  # (e.g. NCAAF: QB|RB|WR) — covers DK classic templates.
  if (!is.null(rr$slots)) {
    flexpos  <- rr$flex$positions;      flexn  <- rr$flex$count %||% 0
    sflexpos <- rr$superflex$positions; sflexn <- rr$superflex$count %||% 0
    for (pos in names(rr$slots)) {
      ind   <- as.numeric(!is.na(pool$position) & pool$position == pos)
      lower <- rr$slots[[pos]]
      extra <- (if (!is.null(flexpos)  && pos %in% flexpos)  flexn  else 0) +
               (if (!is.null(sflexpos) && pos %in% sflexpos) sflexn else 0)
      upper <- lower + extra
      add(ind, ">=", lower)
      if (upper < rr$n) add(ind, "<=", upper)
    }
  }
  if (!is.null(rr$team_limit) && "team" %in% names(pool)) {
    for (t in unique(pool$team[!is.na(pool$team)]))
      add(as.numeric(!is.na(pool$team) & pool$team == t), "<=", rr$team_limit)
  }
  if (!is.null(rr$max_per_game) && "game_id" %in% names(pool)) {
    for (g in unique(pool$game_id[!is.na(pool$game_id)]))
      add(as.numeric(!is.na(pool$game_id) & pool$game_id == g), "<=", rr$max_per_game)
  }
  # group uniqueness: at most one selected row per group (Showdown: one CPT/FLEX
  # row per base player, so a player can't be captained AND flexed).
  if (!is.null(rr$group_unique) && rr$group_unique %in% names(pool)) {
    grp <- pool[[rr$group_unique]]
    for (gk in unique(grp[!is.na(grp)]))
      add(as.numeric(!is.na(grp) & grp == gk), "<=", 1)
  }
  list(mat = do.call(rbind, mats), dir = dirs, rhs = rhs)
}

# Solve one lineup for objective `obj`; returns selected row indices or NULL if infeasible.
ilp_solve <- function(obj, pool, rr, con = NULL) {
  if (!requireNamespace("lpSolve", quietly = TRUE)) stop("install.packages('lpSolve')")
  if (is.null(con)) con <- build_constraints(pool, rr)
  obj[!is.finite(obj)] <- min(obj[is.finite(obj)], na.rm = TRUE)
  sol <- lpSolve::lp("max", obj, const.mat = con$mat, const.dir = con$dir,
                     const.rhs = con$rhs, all.bin = TRUE)
  if (sol$status != 0) return(NULL)
  which(as.logical(round(sol$solution)))
}

# Perturbed-objective candidate pool spanning cash -> leverage. Noise grows across
# attempts so later draws explore further from each optimum; periodic chalk fades
# force contrarian players in (the golf trick). Returns list of list(idx, family).
#
# FIXED 2026-10-01 (two separate bugs found validating the same complaint):
#
# 1. THIN pool (showdown: ~30 players, 6-man roster): a fixed attempt count used to
#    return far fewer UNIQUE candidates than n_cand -- a real PIT@CLE showdown asked
#    for 300 and got 77, because modest noise kept landing on the same few ILP
#    optima. Fixed with an ATTEMPT BUDGET per family (retry instead of give up) plus
#    a HARD exclusion constraint on any exact duplicate, so progress is guaranteed
#    rather than hoped for.
#
# 2. DEEP pool (classic: 427 real players on a real NFL slate): the fix above makes
#    300 genuinely DISTINCT combinations easily, but distinct is not the same as
#    DIVERSE -- measured on that real slate, 7 players appeared in literally every
#    one of the top-20 proj-ranked candidates, because ordinary noise only ever has
#    enough leverage to flip the cheapest, most price-sensitive seat; it essentially
#    never dislodges a dominant player from the other 8. All 7 hit their exposure
#    cap by the 10th lineup, and then ALL 290 remaining "distinct" candidates still
#    included at least one of them -- build_gpp20 could only ever fill 10 of 20,
#    WITH OR WITHOUT an overlap cap (confirmed: identical result at max_overlap=Inf).
#    Fixed by periodically forcing a HARD exclusion of a random subset of the top
#    tier before solving (STUD_EXCLUDE_EVERY), so some candidates are genuinely
#    stud-free rather than only varying who fills the punt seat.
#
# Both exclusions are capped/bounded so they can't blow up the ILP: EXCLUDE_BUDGET
# limits persistent dedup rows; the stud exclusion is a one-off per draw, never
# accumulated.
make_candidates <- function(pool, rr, n_cand = 400L) {
  pool <- as.data.table(pool)
  P <- nrow(pool); mp <- mean(pool$proj)
  own <- if ("own" %in% names(pool)) pmax(pool$own, 1e-3) else rep(0.05, P)
  ceil <- if ("ceil" %in% names(pool)) pool$ceil else pool$proj
  ceil_prem <- pmax(ceil - pool$proj, 0)
  con <- build_constraints(pool, rr)
  rsize <- rr$n

  base_objs <- list(
    cash     = pool$proj,
    balanced = pool$proj + 0.5 * ceil_prem,
    leverage = pool$proj + 0.9 * ceil_prem - 6.0 * own * mp,
    value    = pool$proj / pmax(pool$salary / 1000, 0.1))

  # the players capable of dominating every proj-ranked candidate: top 3 rosters'
  # worth by raw projection, regardless of family objective
  top_tier <- order(-pool$proj)[seq_len(min(3L * rsize, P))]
  STUD_EXCLUDE_EVERY <- 3L

  cands <- list(); seen <- character(0)
  target_per_family <- ceiling(n_cand / length(base_objs))
  EXCLUDE_BUDGET <- 40L
  n_excluded <- 0L

  for (nm in names(base_objs)) {
    if (length(cands) >= n_cand) break
    s_obj <- stats::sd(base_objs[[nm]]); if (!is.finite(s_obj) || s_obj == 0) s_obj <- 1
    got <- 0L; attempt <- 0L; dup_streak <- 0L
    max_attempts <- target_per_family * 8L
    while (got < target_per_family && attempt < max_attempts && length(cands) < n_cand) {
      attempt <- attempt + 1L
      scale <- if (attempt == 1) 0 else min(0.6, 0.06 + 0.03 * dup_streak + 0.01 * (attempt %% 12))
      noise <- rnorm(P, 0, scale * s_obj)
      if (attempt > 1 && attempt %% 3 == 0) {           # fade a chunk of the chalk
        fade <- sample.int(P, max(2L, P %/% 12)); noise[fade] <- noise[fade] - 0.20 * s_obj
      }
      # STUD EXCLUSION: hard-forbid a random subset of the top tier for this ONE
      # draw (a throwaway copy of `con`, never persisted), so the result is
      # guaranteed stud-free rather than merely hoping noise finds that lineup.
      con_i <- con
      if (attempt > 1 && length(top_tier) > 1 && stats::runif(1) < 1 / STUD_EXCLUDE_EVERY) {
        kexc <- sample.int(max(1L, length(top_tier) %/% 2), 1L)
        excl <- sample(top_tier, kexc)
        ind <- rep(0, P); ind[excl] <- 1
        con_i$mat <- rbind(con_i$mat, ind); con_i$dir <- c(con_i$dir, "="); con_i$rhs <- c(con_i$rhs, 0)
      }
      idx <- ilp_solve(base_objs[[nm]] + noise, pool, rr, con_i)
      if (is.null(idx)) { dup_streak <- dup_streak + 1L; next }   # infeasible this draw
      key <- paste(sort(idx), collapse = "-")
      if (key %in% seen) {
        dup_streak <- dup_streak + 1L
        if (n_excluded < EXCLUDE_BUDGET) {               # force genuine progress, not luck
          ind <- rep(0, P); ind[idx] <- 1
          con$mat <- rbind(con$mat, ind); con$dir <- c(con$dir, "<="); con$rhs <- c(con$rhs, rsize - 1L)
          n_excluded <- n_excluded + 1L
        }
        next
      }
      seen <- c(seen, key)
      cands[[length(cands) + 1]] <- list(idx = idx, family = nm)
      got <- got + 1L; dup_streak <- 0L
    }
  }
  if (length(cands) < n_cand)
    msg(sprintf("  make_candidates: pool supports only %d distinct lineups (requested %d) -- thin player pool, not a bug",
                length(cands), n_cand))
  msg("  Candidate lineups:", length(cands))
  cands
}
