-- ============================================================================
-- DFS ENGINE — local store schema (DuckDB)
-- Every table carries a `sport` column so the spine is sport-agnostic.
-- Timestamp-versioned where projections/ownership change through the day.
-- Idempotent: all CREATE statements use IF NOT EXISTS.
-- ============================================================================

CREATE TABLE IF NOT EXISTS sports (
  sport         TEXT PRIMARY KEY,
  display_name  TEXT
);

-- players: identity per (sport, source player_id). ext_ids holds cross-source ids as JSON text.
CREATE TABLE IF NOT EXISTS players (
  sport         TEXT,
  player_id     BIGINT,
  name          TEXT,
  team          TEXT,
  ext_ids       TEXT,                      -- JSON text: {"dk":..., "dg":..., "espn":...}
  updated_ts    TIMESTAMP,
  PRIMARY KEY (sport, player_id)
);

-- games: one row per game/match, with Vegas environment (drives the correlation factors).
CREATE TABLE IF NOT EXISTS games (
  sport         TEXT,
  game_id       TEXT,
  game_date     DATE,
  home          TEXT,
  away          TEXT,
  vegas_total   DOUBLE,
  spread        DOUBLE,                     -- home spread (negative = home favored)
  home_total    DOUBLE,
  away_total    DOUBLE,
  pace          DOUBLE,
  updated_ts    TIMESTAMP,
  PRIMARY KEY (sport, game_id)
);

-- slates: a DK/FD contest slate (the daily anchor). slate_id is globally unique.
CREATE TABLE IF NOT EXISTS slates (
  slate_id      TEXT PRIMARY KEY,
  sport         TEXT,
  site          TEXT,                       -- 'dk' | 'fd'
  slate_date    DATE,
  name          TEXT,                       -- e.g. 'Main', 'Afternoon'
  lock_ts       TIMESTAMP,
  updated_ts    TIMESTAMP
);

-- salaries: the player pool for a slate (salary, position, roster slot, game link).
CREATE TABLE IF NOT EXISTS salaries (
  slate_id      TEXT,
  sport         TEXT,
  player_id     BIGINT,
  salary        INTEGER,
  position      TEXT,
  roster_slot   TEXT,
  game_id       TEXT,
  team          TEXT,
  PRIMARY KEY (slate_id, player_id)
);

-- projections: versioned by timestamp + source ('model','dg','blend','vegas_baseline').
CREATE TABLE IF NOT EXISTS projections (
  slate_id      TEXT,
  sport         TEXT,
  player_id     BIGINT,
  version_ts    TIMESTAMP,
  source        TEXT,
  proj_mean     DOUBLE,
  sim_sd        DOUBLE,
  ceil          DOUBLE,
  floor         DOUBLE,
  p_zero        DOUBLE,
  PRIMARY KEY (slate_id, player_id, version_ts, source)
);

-- ownership: projected (our model) + actual (logged from DK contest results).
-- THE gating asset. actual_pct cannot be backfilled — log every slate from day one.
CREATE TABLE IF NOT EXISTS ownership (
  slate_id      TEXT,
  sport         TEXT,
  player_id     BIGINT,
  contest_id    TEXT,
  projected_pct DOUBLE,
  actual_pct    DOUBLE,
  captured_ts   TIMESTAMP,
  PRIMARY KEY (slate_id, player_id, contest_id)
);

-- contest_results: the real contest structure (payout curve, field size, cash line).
CREATE TABLE IF NOT EXISTS contest_results (
  contest_id      TEXT PRIMARY KEY,
  slate_id        TEXT,
  sport           TEXT,
  site            TEXT,
  name            TEXT,
  contest_type    TEXT,                     -- 'cash' | 'gpp'
  entry_fee       DOUBLE,
  field_size      INTEGER,
  max_entries     INTEGER,
  total_prizes    DOUBLE,
  payout_curve_id TEXT,
  cash_line       DOUBLE,
  captured_ts     TIMESTAMP
);

-- sim_runs: provenance for each slate simulation (params as JSON text).
CREATE TABLE IF NOT EXISTS sim_runs (
  run_id        TEXT PRIMARY KEY,
  slate_id      TEXT,
  sport         TEXT,
  version_ts    TIMESTAMP,
  n_sims        INTEGER,
  params        TEXT,
  created_ts    TIMESTAMP
);

-- ============================================================================
-- USER-ACTION LAYER (the web app's persistent state: history, entries, P&L)
-- ============================================================================

-- slate_catalog: every DK slate detected per day (drives "all slates" browsing).
-- pool_scraped / lineups_built track what work is cached for each slate.
CREATE TABLE IF NOT EXISTS slate_catalog (
  slate_id        TEXT PRIMARY KEY,
  sport           TEXT,
  slate_date      DATE,
  draft_group_id  TEXT,
  name            TEXT,                     -- 'main','slate_2','showdown_<dg>'...
  game_count      INTEGER,
  is_showdown     BOOLEAN,
  top_contest_id  TEXT,
  pool_scraped    BOOLEAN,
  lineups_built   BOOLEAN,
  updated_ts      TIMESTAMP
);

-- recommended_lineups: immutable archive of what the engine recommended, so we can
-- review past picks and score them against actual results. players is JSON text.
CREATE TABLE IF NOT EXISTS recommended_lineups (
  lineup_uid    TEXT PRIMARY KEY,           -- slate_id|build_date|n
  slate_id      TEXT,
  sport         TEXT,
  build_date    DATE,
  n             INTEGER,                    -- lineup index within the build
  role          TEXT,                       -- 'Cash anchor','Balanced GPP','Leverage GPP'
  salary        INTEGER,
  proj          DOUBLE,
  ceil          DOUBLE,
  own           DOUBLE,
  reason        TEXT,
  captain       TEXT,                       -- showdown CPT (else NULL)
  players       TEXT,                       -- JSON: [{name,pos,salary,proj,own}]
  created_ts    TIMESTAMP
);

-- user_entries: the user's actual DK entries (auto-discovered from settled standings
-- by their display name, or marked manually). lineup is the DK lineup string.
CREATE TABLE IF NOT EXISTS user_entries (
  entry_id      TEXT PRIMARY KEY,           -- contest_id|entry_name
  contest_id    TEXT,
  slate_id      TEXT,
  sport         TEXT,
  entry_name    TEXT,
  entry_fee     DOUBLE,
  lineup        TEXT,
  source        TEXT,                       -- 'auto' | 'manual'
  entered_date  DATE,
  status        TEXT,                       -- 'pending' | 'settled'
  captured_ts   TIMESTAMP
);

-- entry_results: settled outcome + profit/loss per entry (the W/L + P&L ledger).
CREATE TABLE IF NOT EXISTS entry_results (
  entry_id      TEXT PRIMARY KEY,
  contest_id    TEXT,
  slate_id      TEXT,
  sport         TEXT,
  rank          INTEGER,
  field_size    INTEGER,
  points        DOUBLE,
  prize         DOUBLE,
  entry_fee     DOUBLE,
  pnl           DOUBLE,                      -- prize - entry_fee
  cashed        BOOLEAN,
  settled_ts    TIMESTAMP
);
