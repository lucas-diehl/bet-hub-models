# ==============================================================================
# NFL plugin — classic-slate STACKING rules (lineup construction)
#
# Until 2026-11-13 the classic NFL ILP had NO stacking logic at all: build_constraints()
# only knew roster slots + salary, and make_candidates() perturbed raw projections. Any
# QB stack in our lineups was an accident, and nothing stopped a lineup from rostering a
# RB against its own DST or two RBs from one backfield. Correlation lived only in the
# slate sim (correlate.R), which build_gpp20() never reads (it ranks by projection).
#
# Two rule sets, wired through the roster rules (rr$stack_hard / rr$stack_draw) so the
# spine stays sport-agnostic:
#
#   HARD (every lineup, incl. build_gpp20's direct-solve backfill) -- each one is either
#   evidence-backed or removes a structurally negative-correlation build:
#     1. the QB has >= 1 same-team WR/TE. ETR/Levitan, 452 Milly Maker top-10 lineups:
#        naked QB 6.4% of top-10 lineups vs 17.4% of the field.
#        https://establishtherun.com/levitan-winning-draftkings-milly-maker-trends/
#     2. <= 4 offensive players from any one team ("no top-10 lineup had more than 3
#        teammates with its QB").
#     3. no offensive player facing our own DST. QB vs opposing DST is the most negative
#        pair on the board (-0.46 RotoViz; our own calibration in correlate.R has
#        DST game load -0.663 vs QB +0.408).
#     4. never two RBs from the same team (they split one backfield's touches).
#     5. a team that is NOT the QB's team supplies at most 2 skill players (keeps
#        secondary "stacks" from crowding out the primary one).
#
#   DRAW (randomised per candidate, so the pool spans the stack shapes winners use):
#     - QB + 2 pass-catchers in ~60% of draws, QB + 1 otherwise (top-10 lineups: QB+2
#       41% vs field 29%, QB+1 49% vs 49%).
#     - a bring-back from the QB's actual opponent in ~50% of draws, ~70% when that game's
#       total is in the slate's top third (top-10: one bring-back 36% vs field 31%).
#     - RUSHING QBs (>= 30 proj rush yds) may go naked in 25% of draws (rule 1 exemption),
#       with a PORTFOLIO quota of <= 10% naked-QB lineups (rr$quota, enforced in build_gpp20).
#     - RB + own DST in 50% of draws when that team is favoured by >= 6 implied points.
#   Backtest 2026-11-13 (30 DK main slates 2023-25, 20 lineups, actual points): the two
#   2026-11-13 additions moved best lineup +1.9 / top-3 +1.5 vs the first version (same
#   sign all 3 seasons, t~1.1 -- positive, not proven); naked QBs landed at 8.8%.
#
# Rules are linear rows appended to the lpSolve system; y_p = 1 if player p is rostered.
# Opponent is parsed from DK's "AWY@HOM" game_id. If game_id/team can't be parsed for a
# player, that player simply isn't covered by the cross-team rules (never an error).
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

NFL_STACK <- list(
  max_team_offense = 4L,     # rule 2
  max_other_team   = 2L,     # rule 5
  p_qb2            = 0.60,   # draw: share of QB+2 candidates
  p_bringback      = 0.50,   # draw: share with a bring-back ...
  p_bringback_high = 0.70,   # ... in a top-third game total
  # RUSHING-QB exemption from rule 1 (added 2026-11-13). Naked QBs are 6.4% of Milly
  # top-10 lineups (vs 17.4% of the field) and the recent Week-1 Milly winners were naked
  # rushing QBs -- a rushing QB's points don't need a pass-catcher to land. A QB projected
  # for >= rush_qb_yds rushing yards may go naked in p_naked_rush of candidate draws.
  rush_qb_yds      = 30,
  p_naked_rush     = 0.25,
  # RB + own DST (added 2026-11-13): when a DST's team is favoured by >= rb_dst_margin
  # implied points, require one of its RBs in p_rb_dst of draws (game script: a leading
  # team runs, and its defence faces a trailing, passing opponent).
  rb_dst_margin    = 6,
  p_rb_dst         = 0.50,
  # QB+2 CAP (added 2026-11-14, MobyWhaler's generator cap; Milly top-10 QB+3 was 3.3%):
  # at most this many of the QB's own WR/TE in one lineup.
  max_qb_pc        = 2L)

# USER lists from config/exposure_overrides.json (added 2026-11-14, MobyWhaler practices):
#   "nfl_stack_only": players legal ONLY when their own QB is in the lineup (chalky pass
#                     catchers you'd only play inside a stack)
#   "nfl_naked_ok":   QBs allowed to go naked (cheap QBs whose receivers the field stacks),
#                     on top of the automatic rushing-QB exemption
.nfl_user_lists <- function() {
  f <- tryCatch(dfs_path("config", "exposure_overrides.json"), error = function(e) "")
  j <- if (nzchar(f) && file.exists(f)) tryCatch(jsonlite::fromJSON(f, simplifyVector = TRUE), error = function(e) NULL) else NULL
  g <- function(k) { v <- j[[k]]; if (is.null(v) || !length(v)) character(0) else norm_name(as.character(unlist(v))) }
  list(stack_only = g("nfl_stack_only"), naked_ok = g("nfl_naked_ok"))
}
.nfl_norm_col <- function(pool) if ("norm" %in% names(pool)) pool$norm else norm_name(pool$player_name)

# PORTFOLIO quotas (read by build_gpp20 via rr$quota). An unconstrained (naked) build always
# out-projects a constrained one, so a proj-ranked selection would otherwise fill the
# portfolio with naked rushing-QB lineups (measured: 35% on a test slate). Cap the share at
# ~10% -- winners run 6.4%.
nfl_is_naked <- function(idx, pool) {
  pos <- .nfl_pos_key(pool[idx]); team <- toupper(as.character(pool$team[idx]))
  q <- which(pos == "QB"); if (!length(q)) return(FALSE)
  !any(pos %in% c("WR", "TE") & team == team[q[1]])
}
NFL_QUOTA <- list(list(name = "naked QB", max_frac = 0.10, test = nfl_is_naked))

# Lineup shape flags for the build report card (rr$shape, spine/R/report_card.R).
nfl_lineup_shape <- function(idx, pool) {
  P <- pool[idx]; pos <- .nfl_pos_key(P); team <- toupper(as.character(P$team))
  opp <- if ("game_id" %in% names(P)) .nfl_opponent(P) else rep(NA_character_, nrow(P))
  q <- which(pos == "QB")[1]; d <- which(pos == "DST")[1]
  mates <- if (is.na(q)) 0L else sum(pos %in% c("WR", "TE") & team == team[q])
  c(naked_qb = as.integer(mates == 0L), qb_plus_1 = as.integer(mates == 1L), qb_plus_2 = as.integer(mates >= 2L),
    bring_back = as.integer(!is.na(q) && any(pos %in% c("RB", "WR", "TE") & team == opp[q], na.rm = TRUE)),
    rb_own_dst = as.integer(!is.na(d) && any(pos == "RB" & team == team[d])))
}

.nfl_rushing_qb <- function(pool, pos, opts = NFL_STACK) {
  rush <- if ("rush_yds" %in% names(pool)) pos == "QB" & is.finite(pool$rush_yds) & pool$rush_yds >= opts$rush_qb_yds
          else rep(FALSE, nrow(pool))
  ok <- .nfl_user_lists()$naked_ok                                       # user-listed naked QBs count too
  if (length(ok)) rush <- rush | (pos == "QB" & .nfl_norm_col(pool) %in% ok)
  rush
}

.nfl_pos_key <- function(pool) {
  p <- toupper(as.character(pool$position))
  fifelse(p %in% c("D", "DEF", "DST"), "DST", p)
}

# opponent team per row from the "AWY@HOM" game key (NA when unparseable)
.nfl_opponent <- function(pool) {
  t <- toupper(as.character(pool$team))
  g <- toupper(gsub("\\s+", "", as.character(pool$game_id)))
  parts <- strsplit(g, "@", fixed = TRUE)
  vapply(seq_along(t), function(i) {
    p <- parts[[i]]
    if (is.na(t[i]) || length(p) != 2L) return(NA_character_)
    if (p[1] == t[i]) p[2] else if (p[2] == t[i]) p[1] else NA_character_
  }, character(1))
}

.nfl_rows <- function() {
  rows <- list(); dirs <- character(0); rhs <- numeric(0)
  list(add = function(r, d, b) { rows[[length(rows) + 1L]] <<- r; dirs <<- c(dirs, d); rhs <<- c(rhs, b) },
       get = function() if (!length(rows)) NULL else list(mat = do.call(rbind, rows), dir = dirs, rhs = rhs))
}

nfl_stack_hard <- function(pool, opts = NFL_STACK) {
  pool <- as.data.table(pool)
  if (!all(c("team", "position") %in% names(pool))) return(NULL)
  pos <- .nfl_pos_key(pool); team <- toupper(as.character(pool$team))
  opp <- if ("game_id" %in% names(pool)) .nfl_opponent(pool) else rep(NA_character_, nrow(pool))
  off <- pos %in% c("QB", "RB", "WR", "TE"); skill <- pos %in% c("RB", "WR", "TE")
  pc  <- pos %in% c("WR", "TE")
  R <- .nfl_rows(); P <- nrow(pool)
  rq <- .nfl_rushing_qb(pool, pos, opts)                                 # rushing QBs: rule 1 moves to the draw
  for (q in which(pos == "QB" & !is.na(team) & !rq)) {                  # 1. QB + >=1 WR/TE
    r <- as.numeric(pc & team == team[q]); r[q] <- -1
    if (sum(r > 0)) R$add(r, ">=", 0) else R$add(replace(numeric(P), q, 1), "=", 0)  # no pass-catcher in pool -> QB unusable
  }
  for (t in unique(team[off & !is.na(team)])) {
    R$add(as.numeric(off & team %in% t), "<=", opts$max_team_offense)  # 2. <=4 from a team
    rb <- pos == "RB" & team %in% t
    if (sum(rb) > 1) R$add(as.numeric(rb), "<=", 1)                     # 4. one RB per team
    r <- as.numeric(skill & team %in% t); r[pos == "QB" & team %in% t] <- -opts$max_other_team
    R$add(r, "<=", opts$max_other_team)                                 # 5. <=2 unless it's the QB's team
  }
  for (d in which(pos == "DST" & !is.na(team))) {                       # 3. nobody vs own DST
    if (is.na(opp[d])) next
    r <- as.numeric(off & !is.na(team) & team == opp[d])
    if (any(r > 0)) { r[d] <- 9; R$add(r, "<=", 9) }
  }
  if (is.finite(opts$max_qb_pc %||% Inf)) for (q in which(pos == "QB" & !is.na(team))) {   # 6. QB+2 cap
    r <- as.numeric(pc & team == team[q])
    if (sum(r) > opts$max_qb_pc) { r[q] <- 9; R$add(r, "<=", opts$max_qb_pc + 9) }
  }
  so <- .nfl_user_lists()$stack_only                                     # 7. stack-only players
  if (length(so)) for (p in which(.nfl_norm_col(pool) %in% so & skill & !is.na(team))) {
    r <- -as.numeric(pos == "QB" & team == team[p]); r[p] <- 1
    R$add(r, "<=", 0)
  }
  R$get()
}

# Per-candidate randomised stack shape. `u` lets callers make it reproducible.
nfl_stack_draw <- function(pool, opts = NFL_STACK) {
  pool <- as.data.table(pool)
  if (!all(c("team", "position", "game_id") %in% names(pool))) return(NULL)
  pos <- .nfl_pos_key(pool); team <- toupper(as.character(pool$team)); opp <- .nfl_opponent(pool)
  pc <- pos %in% c("WR", "TE"); skill <- pos %in% c("RB", "WR", "TE")
  qb2 <- stats::runif(1) < opts$p_qb2
  hi_total <- if ("game_total_z" %in% names(pool)) pool$game_total_z > stats::quantile(pool$game_total_z, 2/3, na.rm = TRUE) else rep(FALSE, nrow(pool))
  R <- .nfl_rows(); P <- nrow(pool)
  bb <- stats::runif(1)                                   # one draw per candidate, shared by every QB
  naked_ok <- stats::runif(1) < opts$p_naked_rush         # this draw lets a RUSHING QB go naked
  rq <- .nfl_rushing_qb(pool, pos, opts)
  for (q in which(pos == "QB" & !is.na(team))) {
    if (rq[q] && !naked_ok) { r <- as.numeric(pc & team == team[q]); r[q] <- -1   # rule 1 for rushing QBs
      if (sum(r > 0)) R$add(r, ">=", 0) }
    if (qb2 && !(rq[q] && naked_ok)) { r <- as.numeric(pc & team == team[q]); r[q] <- -2
      if (sum(r > 0) >= 2) R$add(r, ">=", 0) }
    p_bb <- if (isTRUE(hi_total[q])) opts$p_bringback_high else opts$p_bringback
    if (bb < p_bb && !is.na(opp[q])) { r <- as.numeric(skill & !is.na(team) & team == opp[q]); r[q] <- -1
      if (sum(r > 0)) R$add(r, ">=", 0) }
  }
  # RB + own DST when that team is a big favourite (soft: only in p_rb_dst of draws)
  if (all(c("team_implied", "opp_implied") %in% names(pool)) && stats::runif(1) < opts$p_rb_dst) {
    fav <- which(pos == "DST" & is.finite(pool$team_implied) & is.finite(pool$opp_implied) &
                 pool$team_implied - pool$opp_implied >= opts$rb_dst_margin)
    for (d in fav) { r <- as.numeric(pos == "RB" & !is.na(team) & team == team[d]); r[d] <- -1
      if (sum(r > 0)) R$add(r, ">=", 0) }
  }
  R$get()
}
