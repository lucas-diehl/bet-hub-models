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

register_sport("nfl", list(
  ingest          = if (exists("nfl_ingest")) nfl_ingest else NULL,
  project_players = if (exists("nfl_project_players")) nfl_project_players else NULL,  # live DK-slate -> project.R
  correlation     = if (exists("nfl_correlation")) nfl_correlation else NULL,  # QB-stack / bring-back / game-script
  roster_rules    = NFL_ROSTER,
  roster_rules_fd = NFL_ROSTER_FD,
  dk_scoring      = nfl_dk_scoring,
  fd_scoring      = nfl_fd_scoring))
