# ==============================================================================
# Tennis plugin — roster rules + DK scoring + registration
#
# !!! VERIFY against a live DK Tennis slate before trusting validity/scoring. !!!
# DK Tennis Classic (as encoded): 6 players, $50,000 cap, flat (no positions).
# Both sides of a match ARE rosterable on DK; max_per_game left NULL. The scoring
# below is the best-of-3 structure (set the bonuses per the current DK rules).
# ==============================================================================

TENNIS_ROSTER <- list(
  n          = 6L,
  cap        = 50000L,
  floor      = 47000L,
  slots      = NULL,                 # flat-n
  slot_labels= paste0("P", 1:6),
  team_limit = NULL,
  max_per_game = NULL                # DK allows rostering both players in a match
)

# DK Tennis fantasy scoring (best-of-3). box columns: games_won, games_lost,
# sets_won, sets_lost, match_won, aces, dfs (double faults), breaks, clean_set,
# no_df (no double fault), straight_sets. VERIFY point values vs current DK rules.
tennis_dk_scoring <- function(box) {
  g <- function(col) if (col %in% names(box)) as.numeric(box[[col]]) else 0
  30 * g("match_played") +
    2.5  * g("games_won")   - 2.0 * g("games_lost") +
    6.0  * g("sets_won")    - 3.0 * g("sets_lost") +
    6.0  * g("match_won")   +
    0.4  * g("aces")        - 1.0 * g("dfs") +
    0.75 * g("breaks")      +
    2.5  * g("clean_set")   + 2.5 * g("no_df") + 6.0 * g("straight_sets")
}

# ── register the plugin (sourced last in the sport dir) ───────────────────────
register_sport("tennis", list(
  ingest          = if (exists("tennis_ingest")) tennis_ingest else NULL,
  project_players = if (exists("tennis_project_players")) tennis_project_players else NULL,
  correlation     = if (exists("tennis_correlation")) tennis_correlation else NULL,
  roster_rules    = TENNIS_ROSTER,
  dk_scoring      = tennis_dk_scoring
))
