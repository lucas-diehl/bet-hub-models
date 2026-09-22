# ==============================================================================
# 2026 Presidents Cup — DK match-play PAIRINGS (hand-maintained -- update daily)
#
# WHY THIS FILE EXISTS: DraftKings' Presidents Cup contest (DraftGroup 153817,
# GameTypeId 129, "Cup") scores hole-by-hole match play across the WHOLE week
# (see ../../DFS ENGINE/sports/golf/matchplay.R for the point table). There is
# no live pairings API -- the PGA TOUR / Presidents Cup releases each session's
# pairings on a ROLLING basis, the evening/day before that session is played:
#
#   Wed evening (~Sep 23)   -> Thursday Fourball pairings
#   Thu evening (after R1)  -> Friday Foursomes pairings
#   Fri evening             -> Saturday AM Fourball pairings
#   Sat midday              -> Saturday PM Foursomes pairings
#   Sat evening             -> Sunday Singles pairings (all 12 vs 12)
#
# HOW TO UPDATE (do this each day this week, 2026-09-23 through 2026-09-27):
#   1. Find that session's list() in PRESIDENTS_CUP_PAIRINGS$sessions below.
#   2. Set status = "announced".
#   3. Fill `matches` with one list(team_usa = c(...), team_intl = c(...)) per
#      match. Singles: ONE name per side. Fourball/Foursomes: TWO names per
#      side (the pairing). Use the EXACT names from PRESIDENTS_CUP_ROSTER_USA /
#      _INTL below (name-matching is exact-ish but why risk it).
#   4. Re-run:  Rscript engine/matchplay_sim.R --export
#      (or let sports/golf/adapter.R's golf_presidents_cup_pool() auto-
#      regenerate it on next build -- same auto-export pattern as the
#      single-round v2 export in engine/round_sim.R).
#
# UNTIL a session is "announced": engine/matchplay_sim.R assumes EVERY one of
# the 24 rostered players plays that session and applies a neutral
# field-average expected-points estimate for it (mp_neutral_session_estimate()
# in matchplay_sim.R) -- keeps the projection USABLE all week, just less
# precise for sessions that haven't been announced. This also means: once you
# mark a session "announced", any of the 24 players who do NOT appear in any
# of that session's matches are correctly scored as sitting it out (+0 for
# that session, no fallback) -- captains really do rest players, and this file
# is the only place that fact can be recorded.
#
# Sourced by:
#   - engine/matchplay_sim.R's export_presidents_cup_projection() (this project)
#   - indirectly by DFS ENGINE's sports/golf/adapter.R via that export's output
#     file (golf_picks/dfs_presidents_cup_projections.rds) -- adapter.R does
#     NOT read this file directly.
# ==============================================================================

# ---- ROSTERS (stable, confirmed both full 12-man teams announced 2026-09-22; captains
#      Brandt Snedeker (USA) / Geoff Ogilvy (Intl) are NON-PLAYING, excluded here) ----
PRESIDENTS_CUP_ROSTER_USA <- c(
  "Scottie Scheffler", "Cameron Young", "Wyndham Clark", "Sam Burns", "Russell Henley",
  "Collin Morikawa", "Chris Gotterup", "Xander Schauffele", "Justin Thomas", "Patrick Cantlay",
  "Jacob Bridgeman", "Jackson Koivun")

PRESIDENTS_CUP_ROSTER_INTL <- c(
  "Si Woo Kim", "Ryan Fox", "Hideki Matsuyama", "Min Woo Lee", "Tom Kim", "Adam Scott",
  "Corey Conners", "Nicolas Echavarria", "Sungjae Im", "Ryo Hisatsune", "Nick Taylor", "Christiaan Bezuidenhout")
  # NOTE: DK's live draftables feed spells him "Nicolas Echavarria" (confirmed live
  # 2026-09-22, DraftGroup 153817) -- used here so the DK-salary <-> projection name
  # join in sports/golf/adapter.R's golf_presidents_cup_pool() matches directly. The
  # underlying data (golf_picks/v2_master.rds, player_id 22833) inconsistently spells
  # him "Nico" in most rows and "Nicolas" in a few -- engine/matchplay_sim.R's
  # mp_player_snapshot() resolves EITHER spelling to the same player_id, so this
  # isn't fragile to which variant is used here.

PRESIDENTS_CUP_ROSTER <- c(PRESIDENTS_CUP_ROSTER_USA, PRESIDENTS_CUP_ROSTER_INTL)   # all 24 playing golfers

# ---- PAIRINGS (update this block every day as sessions are announced) ----
# status: "pending" (no real pairings yet -> neutral fallback used for everyone)
#      or "announced" (matches[] below is the real, official pairing sheet).
PRESIDENTS_CUP_PAIRINGS <- list(
  meta = list(event = "2026 Presidents Cup", venue = "Medinah Country Club",
              dates = "2026-09-24 to 2026-09-27",
              updated = "2026-09-22 -- NO sessions announced yet (rosters only)"),
  sessions = list(
    list(session = "Thursday Fourball",     type = "fourball",  status = "pending", matches = list()),
    list(session = "Friday Foursomes",      type = "foursomes", status = "pending", matches = list()),
    list(session = "Saturday AM Fourball",  type = "fourball",  status = "pending", matches = list()),
    list(session = "Saturday PM Foursomes", type = "foursomes", status = "pending", matches = list()),
    list(session = "Sunday Singles",        type = "singles",   status = "pending", matches = list())
  )
)

# ---- EXAMPLE ONLY -- NOT REAL PAIRINGS, shown for shape/testing purposes only ----
# Do NOT uncomment this into the live PRESIDENTS_CUP_PAIRINGS object above without
# replacing it with the actual announced pairing sheet. This illustrates a fully
# filled-in Thursday Fourball session (5 matches, all 12 USA + all 12 Intl playing
# -- real sessions before Sunday usually rest 2 players per side instead):
#
# PRESIDENTS_CUP_PAIRINGS$sessions[[1]] <- list(
#   session = "Thursday Fourball", type = "fourball", status = "announced",
#   matches = list(
#     list(team_usa = c("Scottie Scheffler", "Justin Thomas"),    team_intl = c("Hideki Matsuyama", "Adam Scott")),
#     list(team_usa = c("Xander Schauffele", "Patrick Cantlay"),  team_intl = c("Si Woo Kim", "Tom Kim")),
#     list(team_usa = c("Collin Morikawa", "Wyndham Clark"),      team_intl = c("Min Woo Lee", "Ryan Fox")),
#     list(team_usa = c("Sam Burns", "Russell Henley"),           team_intl = c("Sungjae Im", "Corey Conners")),
#     list(team_usa = c("Cameron Young", "Chris Gotterup"),       team_intl = c("Ryo Hisatsune", "Nick Taylor"))
#   )
# )
