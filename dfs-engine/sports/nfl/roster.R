# ==============================================================================
# NFL plugin — roster rules + DK scoring + registration  (Phase 4 foundation)
#
# DK NFL Classic: QB / RB / RB / WR / WR / WR / TE / FLEX(RB|WR|TE) / DST = 9,
# $50,000 cap. Full-PPR scoring with yardage bonuses. The projection MODEL +
# correlation (QB-stack / bring-back / RB game-script) land next; this file is the
# testable scoring + roster contract the rest of the spine builds on.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

NFL_ROSTER <- list(
  n          = 9L,
  cap        = 50000L,
  floor      = NULL,
  slots      = c(QB = 1L, RB = 2L, WR = 3L, TE = 1L, DST = 1L),
  flex       = list(count = 1L, positions = c("RB", "WR", "TE")),  # 1 FLEX from RB/WR/TE
  slot_labels= c("QB", "RB", "RB", "WR", "WR", "WR", "TE", "FLEX", "DST"),
  team_limit = NULL, max_per_game = NULL)

# DK NFL scoring. box: named stat columns (offense and/or DST). Vectorized.
#   passing 0.04/yd + 4/TD - 1/INT (+3 @300);  rushing 0.1/yd + 6/TD (+3 @100)
#   receiving 1/rec + 0.1/yd + 6/TD (+3 @100);  -1 fumble lost;  2 per 2-pt
#   DST: sack 1, INT 2, fum-rec 2, TD 6, safety 2, block 2 + points-allowed tiers
nfl_dk_scoring <- function(box) {
  g <- function(col) if (col %in% names(box)) as.numeric(box[[col]]) else 0
  pa_pts <- function(pa) fifelse(pa <= 0, 10, fifelse(pa <= 6, 7, fifelse(pa <= 13, 4,
            fifelse(pa <= 20, 1, fifelse(pa <= 27, 0, fifelse(pa <= 34, -1, -4))))))
  off <- 0.04 * g("pass_yds") + 4 * g("pass_td") - 1 * g("interceptions") +
         0.1  * g("rush_yds") + 6 * g("rush_td") +
         1    * g("receptions") + 0.1 * g("rec_yds") + 6 * g("rec_td") +
         2    * g("two_pt") - 1 * g("fumbles_lost") + 6 * g("fumble_td") +
         3 * (g("pass_yds") >= 300) + 3 * (g("rush_yds") >= 100) + 3 * (g("rec_yds") >= 100)
  dst <- 1 * g("sacks") + 2 * g("def_int") + 2 * g("fumble_rec") + 6 * g("def_td") +
         2 * g("safeties") + 2 * g("blocked_kicks") +
         (if ("points_allowed" %in% names(box)) pa_pts(g("points_allowed")) else 0)
  off + dst
}

# FanDuel NFL Classic: QB / RB / RB / WR / WR / WR / TE / FLEX(RB|WR|TE) / DEF = 9,
# $60,000 cap. Differs from DK: HALF-PPR (0.5/rec), NO yardage bonuses, -2 per fumble
# lost, DEF slot label (not DST). Passing/rushing/TD values match DK.
NFL_ROSTER_FD <- list(
  n          = 9L,
  cap        = 60000L,
  floor      = NULL,
  slots      = c(QB = 1L, RB = 2L, WR = 3L, TE = 1L, DEF = 1L),
  flex       = list(count = 1L, positions = c("RB", "WR", "TE")),
  slot_labels= c("QB", "RB", "RB", "WR", "WR", "WR", "TE", "FLEX", "DEF"),
  team_limit = NULL, max_per_game = NULL)

nfl_fd_scoring <- function(box) {
  g <- function(col) if (col %in% names(box)) as.numeric(box[[col]]) else 0
  pa_pts <- function(pa) fifelse(pa <= 0, 10, fifelse(pa <= 6, 7, fifelse(pa <= 13, 4,
            fifelse(pa <= 20, 1, fifelse(pa <= 27, 0, fifelse(pa <= 34, -1, -4))))))
  off <- 0.04 * g("pass_yds") + 4 * g("pass_td") - 1 * g("interceptions") +
         0.1  * g("rush_yds") + 6 * g("rush_td") +
         0.5  * g("receptions") + 0.1 * g("rec_yds") + 6 * g("rec_td") +   # HALF-PPR, no bonuses
         2    * g("two_pt") - 2 * g("fumbles_lost") + 6 * g("fumble_td")   # FD: -2 per fumble lost
  dst <- 1 * g("sacks") + 2 * g("def_int") + 2 * g("fumble_rec") + 6 * g("def_td") +
         2 * g("safeties") + 2 * g("blocked_kicks") +
         (if ("points_allowed" %in% names(box)) pa_pts(g("points_allowed")) else 0)
  off + dst
}

# Classic-slate stacking rules (lineup_rules.R). DFS_NFL_STACK=0 turns them off (A/B, or
# an emergency revert to the pre-2026-11-13 unconstrained build without a code change).
if (exists("nfl_stack_hard") && Sys.getenv("DFS_NFL_STACK", "1") != "0") {
  NFL_ROSTER$stack_hard    <- nfl_stack_hard;  NFL_ROSTER$stack_draw    <- nfl_stack_draw
  NFL_ROSTER_FD$stack_hard <- nfl_stack_hard;  NFL_ROSTER_FD$stack_draw <- nfl_stack_draw
  NFL_ROSTER$quota <- NFL_QUOTA;                NFL_ROSTER_FD$quota <- NFL_QUOTA
  NFL_ROSTER$shape <- nfl_lineup_shape;         NFL_ROSTER_FD$shape <- nfl_lineup_shape
}
# SALARY FLOOR (added 2026-11-14; ETR, 452 Milly top-10 lineups: only 6% left >$500 unused
# vs 12% of the field; MobyWhaler's generator enforces a salary minimum). DK $49,000 /
# FD $59,000 by default; DFS_NFL_SAL_FLOOR=0 disables (FD = DK value + 10,000).
local({
  fl <- suppressWarnings(as.integer(Sys.getenv("DFS_NFL_SAL_FLOOR", "49000")))
  if (is.finite(fl) && fl > 0) { NFL_ROSTER$floor <<- fl; NFL_ROSTER_FD$floor <<- fl + 10000L }
})
# Conviction caps (portfolio.R) replace the flat per-player cap in the 20-max / large
# presets. Backtest 2026-11-13, 51 real DK main slates 2023-25, 20 lineups each, scored
# on ACTUAL points vs the old build: stacking + conviction was the best of 6 variants on
# best-lineup (+2.0 pts), top-3 (+1.4) and mean (+0.5) -- directionally consistent but
# NOT statistically significant (t < 1), so it ships on structure + external evidence,
# not on a proven lift. DFS_NFL_CONVICTION=0 reverts to the flat cap.
if (Sys.getenv("DFS_NFL_CONVICTION", "1") != "0") {
  NFL_ROSTER$conviction <- TRUE; NFL_ROSTER_FD$conviction <- TRUE
}
# SIM-OPTIMAL candidates (optimizer.R make_candidates_sim) added on top of the perturbed pool.
# Backtest 2026-11-14 (51 DK main slates 2023-25, 20 lineups, actual points): with R1 caps +
# salary floor + QB+2 cap, best lineup +4.2 (t 2.2) / top-3 +2.6 (t 2.3) vs the 2026-11-13
# build, holding in the 2025 holdout; pool lineups within 75% of the hindsight optimum
# 0.3 -> 1.7 per slate -- the same ceiling as a 1,500 perturbed pool at ~1/5 of the build
# time. QB+2 share 30% -> 42% (Milly top-10: 41%). DFS_NFL_CAND_SIM overrides (0 = off).
# Ranking stays projection-based: blending in the sim ceiling rate (rank_mode "blend") did
# NOT help (+0.4, inconsistent across the holdout).
local({
  ns <- suppressWarnings(as.integer(Sys.getenv("DFS_NFL_CAND_SIM", "1500")))
  if (is.finite(ns) && ns > 0) { NFL_ROSTER$cand_sim <<- ns; NFL_ROSTER_FD$cand_sim <<- ns }
  NFL_ROSTER$rank_mode <<- Sys.getenv("DFS_NFL_RANK", "proj"); NFL_ROSTER_FD$rank_mode <<- NFL_ROSTER$rank_mode
})

register_sport("nfl", list(
  ingest          = if (exists("nfl_ingest")) nfl_ingest else NULL,
  project_players = if (exists("nfl_project_players")) nfl_project_players else NULL,  # live DK-slate -> project.R
  correlation     = if (exists("nfl_correlation")) nfl_correlation else NULL,  # QB-stack / bring-back / game-script
  roster_rules    = NFL_ROSTER,
  roster_rules_fd = NFL_ROSTER_FD,
  dk_scoring      = nfl_dk_scoring,
  fd_scoring      = nfl_fd_scoring))
