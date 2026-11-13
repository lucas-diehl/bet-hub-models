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
  p_bringback_high = 0.70)   # ... in a top-third game total

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
  for (q in which(pos == "QB" & !is.na(team))) {                        # 1. QB + >=1 WR/TE
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
  for (q in which(pos == "QB" & !is.na(team))) {
    if (qb2) { r <- as.numeric(pc & team == team[q]); r[q] <- -2
      if (sum(r > 0) >= 2) R$add(r, ">=", 0) }
    p_bb <- if (isTRUE(hi_total[q])) opts$p_bringback_high else opts$p_bringback
    if (bb < p_bb && !is.na(opp[q])) { r <- as.numeric(skill & !is.na(team) & team == opp[q]); r[q] <- -1
      if (sum(r > 0)) R$add(r, ">=", 0) }
  }
  R$get()
}
