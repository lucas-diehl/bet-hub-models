# ==============================================================================
# Golf plugin — DraftKings MATCH PLAY scoring (Presidents Cup / Ryder Cup format)
#
# DK's Presidents Cup contest (DraftGroup 153817, GameTypeId 129, GameType name
# "Cup") is a SEPARATE scoring system from DK's usual stroke-play/finish-position
# golf scoring: points are earned per HOLE and per MATCH, not per stroke or finish
# position. This file is the single source of truth for that point table so a
# settled real match and a simulated match are graded identically.
#
# Point table (sourced from a 2023 thegolfnewsnet.com article; DK's official 2026
# rules page was not reachable via automation this session — proceeding on this
# basis was explicitly approved by the user; re-verify against DK's live rules
# page before trusting this for real money if that becomes possible):
#   Hole won                              +3
#   Hole halved                           +0.75
#   Hole lost                             -0.75
#   Hole NOT PLAYED (winning side only, when a match closes out early)  +1.6
#   Match won                             +5
#   Match halved                          +2
#   Match lost                             0
#   Bonus: 3 consecutive holes won in a single match   +5  (max once per match)
#   Bonus: zero holes LOST in a match (min 10-hole match length)  +7.5
#
# Foursomes/Fourball: BOTH partners on a team get the IDENTICAL match score (it's
# a team result, not an individual one) -- this file scores ONE side's hole
# sequence; the caller (grading code or the Monte Carlo simulator in
# golf-modeling/engine/matchplay_sim.R) is responsible for assigning the same
# returned total to both partners on a team match.
#
# Deliberately ZERO dependencies on spine helpers (no msg()/dfs_path()/data.table
# required) so this file can be `source()`d standalone from golf-modeling's
# simulator (a separate R project/process) -- see engine/matchplay_sim.R's
# `.mp_locate_scoring_file()`, which sources THIS exact file when the two
# projects are colocated (the normal setup), falling back to an embedded copy of
# the same logic otherwise so golf-modeling still works run standalone.
# ==============================================================================

DK_MATCHPLAY_POINTS <- list(
  hole_won            = 3,
  hole_halved         = 0.75,
  hole_lost           = -0.75,
  hole_unplayed_win   = 1.6,     # per unplayed hole, winning side only
  match_won           = 5,
  match_halved        = 2,
  match_lost          = 0,
  bonus_streak3       = 5,       # 3 consecutive holes won, once per match
  bonus_zero_lost     = 7.5,     # zero holes lost, match played >= 10 holes
  bonus_zero_lost_min_holes = 10L
)

# ── (a) exact scoring from a full hole-by-hole sequence ──────────────────────
# hole_results : character vector, one entry per hole ACTUALLY PLAYED by this
#                side, in order, each "won"/"halved"/"lost" (case-insensitive).
#                Do NOT include unplayed holes here -- pass their count instead.
# match_result : "win"/"halve"/"loss" -- this side's final match outcome.
# holes_unplayed: holes not played because the match closed out early (e.g. a
#                4&3 win leaves 3 holes unplayed). Only paid to the WINNING side
#                per DK's rule ("holes not played by the WINNING side").
#
# Returns a list with `total` plus the breakdown (useful for auditing/debugging
# a graded match or a single simulated draw).
dk_matchplay_points <- function(hole_results, match_result = c("win", "halve", "loss"),
                                 holes_unplayed = 0L) {
  match_result <- match.arg(match_result)
  hr <- tolower(trimws(as.character(hole_results)))
  if (length(hr) && !all(hr %in% c("won", "halved", "lost")))
    stop("hole_results must be one of 'won'/'halved'/'lost', got: ",
         paste(unique(hr[!hr %in% c("won", "halved", "lost")]), collapse = ", "))
  holes_unplayed <- as.integer(holes_unplayed %||% 0L)
  P <- DK_MATCHPLAY_POINTS

  n_played <- length(hr)
  pts_hole <- sum(c(won = P$hole_won, halved = P$hole_halved, lost = P$hole_lost)[hr])
  if (!n_played) pts_hole <- 0

  pts_unplayed <- if (match_result == "win") holes_unplayed * P$hole_unplayed_win else 0
  pts_match <- switch(match_result, win = P$match_won, halve = P$match_halved, loss = P$match_lost)

  # bonus: 3+ consecutive "won" holes anywhere in the sequence (cap once)
  bonus_streak <- 0
  if (n_played >= 3L) {
    run <- 0L; hit <- FALSE
    for (r in hr) { run <- if (r == "won") run + 1L else 0L; if (run >= 3L) hit <- TRUE }
    if (hit) bonus_streak <- P$bonus_streak3
  }

  # bonus: zero holes lost, match length played >= 10 holes (unplayed holes
  # don't count toward "length played" -- the bonus is about holes actually
  # contested, not the theoretical 18)
  bonus_zero_lost <- if (n_played >= P$bonus_zero_lost_min_holes && !any(hr == "lost"))
    P$bonus_zero_lost else 0

  total <- pts_hole + pts_unplayed + pts_match + bonus_streak + bonus_zero_lost
  list(total = total, pts_hole = pts_hole, pts_unplayed = pts_unplayed, pts_match = pts_match,
       bonus_streak3 = bonus_streak, bonus_zero_lost = bonus_zero_lost,
       n_played = n_played, holes_unplayed = holes_unplayed, match_result = match_result)
}

# ── (b) approximate scoring from a MATCH-LEVEL SUMMARY (no hole order known) ──
# For grading a settled real match when only the box-score summary is available
# (holes won/halved/lost counts, final result, unplayed holes on a closeout) --
# NOT the hole-by-hole sequence. Everything is exact EXCEPT the 3-consecutive-
# holes-won streak bonus, which fundamentally requires hole ORDER: pass
# `streak_bonus = TRUE/FALSE` if you know it from an external box score/recap,
# otherwise it defaults to FALSE (conservative -- undercounts rather than
# fabricates a bonus) and the result carries `streak_bonus_known = FALSE` so
# callers can tell the total is a floor, not necessarily exact.
dk_matchplay_points_summary <- function(holes_won, holes_halved, holes_lost,
                                         match_result = c("win", "halve", "loss"),
                                         holes_unplayed = 0L, streak_bonus = NA) {
  match_result <- match.arg(match_result)
  P <- DK_MATCHPLAY_POINTS
  holes_won <- as.integer(holes_won); holes_halved <- as.integer(holes_halved)
  holes_lost <- as.integer(holes_lost); holes_unplayed <- as.integer(holes_unplayed %||% 0L)
  n_played <- holes_won + holes_halved + holes_lost

  pts_hole <- holes_won * P$hole_won + holes_halved * P$hole_halved + holes_lost * P$hole_lost
  pts_unplayed <- if (match_result == "win") holes_unplayed * P$hole_unplayed_win else 0
  pts_match <- switch(match_result, win = P$match_won, halve = P$match_halved, loss = P$match_lost)

  bonus_zero_lost <- if (n_played >= P$bonus_zero_lost_min_holes && holes_lost == 0)
    P$bonus_zero_lost else 0
  streak_known <- !is.na(streak_bonus)
  bonus_streak <- if (isTRUE(streak_bonus)) P$bonus_streak3 else 0

  total <- pts_hole + pts_unplayed + pts_match + bonus_streak + bonus_zero_lost
  list(total = total, pts_hole = pts_hole, pts_unplayed = pts_unplayed, pts_match = pts_match,
       bonus_streak3 = bonus_streak, bonus_zero_lost = bonus_zero_lost,
       n_played = n_played, holes_unplayed = holes_unplayed, match_result = match_result,
       streak_bonus_known = streak_known)
}

`%||%` <- get0("%||%", ifnotfound = function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a)
