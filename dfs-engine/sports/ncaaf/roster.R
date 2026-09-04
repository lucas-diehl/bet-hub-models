# ==============================================================================
# NCAAF (CFB) plugin — roster rules + registration
# DK CFB Classic: QB / RB / RB / WR / WR / WR / FLEX(RB|WR) / SUPERFLEX(QB|RB|WR) = 8,
# $50,000 cap. NO DST (unlike NFL). Scoring is identical to DK NFL (cfb_dk_scoring,
# defined in ingest.R since ingest needs it to compute training dk_pts).
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

NCAAF_ROSTER <- list(
  n           = 8L,
  cap         = 50000L,
  floor       = NULL,
  slots       = c(QB = 1L, RB = 2L, WR = 3L),
  flex        = list(count = 1L, positions = c("RB", "WR")),
  superflex   = list(count = 1L, positions = c("QB", "RB", "WR")),
  slot_labels = c("QB", "RB", "RB", "WR", "WR", "WR", "FLEX", "SUPERFLEX"),
  team_limit  = NULL, max_per_game = NULL)

register_sport("ncaaf", list(
  ingest          = if (exists("cfb_ingest")) cfb_ingest else NULL,
  project_players = if (exists("ncaaf_project_players")) ncaaf_project_players else NULL,
  correlation     = NULL,
  roster_rules    = NCAAF_ROSTER,
  dk_scoring      = cfb_dk_scoring
))
