# bet-screen (NBA player props)

Posts NBA player-prop picks to the Bet Hub feed. Source key `bet-screen`, sport `nba`.

- `scripts/live_fetch_props_slate.py` -- fetches today's DraftKings/FanDuel player props,
  classifies each player's role (starter/bench) from their current minutes EWMA and
  tonight's game script (close/blowout-risk) from the spread, matches against the 11
  segments in `src/nba_model/segments.py`, and writes `picks_<date>.json` to `$FEED_DIR`
  (contract v1.0) plus an archival copy under `data/picks_archive/` (persisted via
  `actions/cache`, not committed) so the grading step the next day doesn't need its own
  Supabase read.
- `scripts/grade_props_slate.py` -- grades the prior day's archived picks against real box
  scores (free, `nba_api`) and writes `results_<date>.json` to `$FEED_DIR`.
- `scripts/download_boxscores.py` -- refreshes `data/processed/player_games_<season>.csv`
  (cached across runs; `data/processed/player_games_2026.csv` is checked in as a one-time
  seed so the minutes EWMA has a prior season to carry over from on day one of 2026-27).

## Segment validation status

This is a research-derived rule set, not a probability model. Full backtest history,
including a deeper mechanism investigation that complicates the simple "p<0.05" framing
`segments.py` encodes, lives in the `bet-screen` possessions repo's `nba/HANDOFF.md`.

Started at 11 candidate role x game-script segments from the 2025-26 diagnostic pass; 3
cleared the original significance screen. A follow-up investigation into *why*
rebounds-under works found the broader (non-role-conditioned) cut has more robust support
than the role-narrowed one two of those three segments ship, and recommended
forward-validation before real staking. Trimmed on 2026-10-20 (too many bets posting) to
just those 3 confirmed segments plus the next-best 2 by backtest_p_le0 among the original
11 -- 5 total. All 5 post as real (`mode: "LIVE"`) picks per an explicit owner decision,
not because every segment has cleared that bar independently -- forward results should be
read with that in mind.
