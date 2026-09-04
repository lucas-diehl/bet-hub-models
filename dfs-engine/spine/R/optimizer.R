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
make_candidates <- function(pool, rr, n_cand = 400L) {
  pool <- as.data.table(pool)
  P <- nrow(pool); mp <- mean(pool$proj)
  own <- if ("own" %in% names(pool)) pmax(pool$own, 1e-3) else rep(0.05, P)
  ceil <- if ("ceil" %in% names(pool)) pool$ceil else pool$proj
  ceil_prem <- pmax(ceil - pool$proj, 0)
  con <- build_constraints(pool, rr)

  base_objs <- list(
    cash     = pool$proj,
    balanced = pool$proj + 0.5 * ceil_prem,
    leverage = pool$proj + 0.9 * ceil_prem - 6.0 * own * mp,
    value    = pool$proj / pmax(pool$salary / 1000, 0.1))

  cands <- list(); seen <- character(0)
  per <- ceiling(n_cand / length(base_objs))
  for (nm in names(base_objs)) {
    s_obj <- stats::sd(base_objs[[nm]]); if (!is.finite(s_obj) || s_obj == 0) s_obj <- 1
    for (k in seq_len(per)) {
      scale <- if (k == 1) 0 else min(0.35, 0.06 + 0.02 * (k %% 12))
      noise <- rnorm(P, 0, scale * s_obj)
      if (k > 1 && k %% 3 == 0) {                       # fade a chunk of the chalk
        fade <- sample.int(P, max(2L, P %/% 12)); noise[fade] <- noise[fade] - 0.20 * s_obj
      }
      idx <- ilp_solve(base_objs[[nm]] + noise, pool, rr, con)
      if (is.null(idx)) next
      key <- paste(sort(idx), collapse = "-")
      if (key %in% seen) next
      seen <- c(seen, key)
      cands[[length(cands) + 1]] <- list(idx = idx, family = nm)
    }
  }
  msg("  Candidate lineups:", length(cands))
  cands
}
