# ==============================================================================
# NFL plugin — correlation (the GPP edge: stacking)
# Two factors (uses the extended slate_sim):
#   GAME factor  — shootout/pace: a high game total lifts everyone in the game
#                  (positive load for all -> QB + bring-back correlate).
#   TEAM factor  — the passing STACK: QB + WR/TE on the SAME team share it strongly
#                  (positive), so a same-team stack correlates MORE than a bring-back;
#                  RB gets a NEGATIVE team load (game script — teams run when ahead,
#                  pass when behind), so it decouples from its own QB.
# Net same-team corr = load_i*load_j + team_load_i*team_load_j (> cross-team = game only).
# Requires the pool to carry `team` + `game_id` (from the DK slate).
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

nfl_correlation <- function(pool) {
  pool <- as.data.table(pool)
  if (!"game_id" %in% names(pool)) return(NULL)
  pos <- toupper(as.character(pool$position))
  game_ld <- fifelse(pos == "QB", 0.50,
             fifelse(pos %in% c("WR", "TE"), 0.45,
             fifelse(pos == "RB", 0.42,
             fifelse(pos == "DST", 0.35, 0.40))))                  # shootout: all positive
  team_ld <- fifelse(pos == "QB", 0.58,                            # stack anchor
             fifelse(pos == "WR", 0.50,
             fifelse(pos == "TE", 0.44,
             fifelse(pos == "RB", -0.28,                           # game-script: decouple from QB
             fifelse(pos == "DST", 0.20, 0.0)))))
  data.table(player_id = pool$player_id, game_id = pool$game_id,
             load = game_ld, team_load = team_ld)
}
