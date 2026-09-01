# ==============================================================================
# WNBA plugin — roster rules + DK scoring + registration
#
# DK WNBA Classic: 6 players, $50,000 cap. Roster = 2 G + 3 F + 1 UTIL (UTIL = any G/F).
# Multi-position eligibility ("G/F") is simplified to the FIRST listed slot for now —
# a full multi-slot assignment ILP is a later upgrade (noted in plan).
# ==============================================================================

WNBA_ROSTER <- list(
  n          = 6L,
  cap        = 50000L,
  floor      = 49000L,                       # optional; keeps lineups near the cap
  slots      = c(G = 2L, F = 3L),            # dedicated guard/forward slots: 2 G, 3 F
  flex       = list(count = 1L, positions = c("G", "F")),   # 1 UTIL fillable by G or F
  slot_labels= c("G", "G", "F", "F", "F", "UTIL"),
  team_limit = NULL,
  max_per_game = NULL
)

# DK WNBA fantasy scoring (same as DK NBA). box: data.frame/list of stat columns.
wnba_dk_scoring <- function(box) {
  g <- function(col) if (col %in% names(box)) as.numeric(box[[col]]) else 0
  pts <- g("pts"); fg3 <- g("fg3m"); reb <- g("reb"); ast <- g("ast")
  stl <- g("stl"); blk <- g("blk"); tov <- g("tov")
  base <- pts + 0.5 * fg3 + 1.25 * reb + 1.5 * ast + 2 * stl + 2 * blk - 0.5 * tov
  # double-double / triple-double bonuses
  cats <- cbind(pts, reb, ast, stl, blk)
  n10  <- rowSums(cats >= 10)
  base + ifelse(n10 >= 3, 3, 0) + ifelse(n10 == 2, 1.5, 0)
}

# ── register the plugin (this file is sourced last in the sport dir) ───────────
register_sport("wnba", list(
  ingest          = if (exists("wnba_ingest")) wnba_ingest else NULL,
  project_players = if (exists("wnba_project_players")) wnba_project_players else NULL,
  correlation     = if (exists("wnba_correlation")) wnba_correlation else NULL,
  roster_rules    = WNBA_ROSTER,
  dk_scoring      = wnba_dk_scoring
))
