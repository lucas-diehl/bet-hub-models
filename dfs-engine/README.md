# DFS ENGINE

A simulation-first, multi-sport DFS engine (R, local-first). Not a projections
site — a **simulation engine**: per-player distribution → game-environment
correlation → joint Monte-Carlo slate sim → ownership-driven field sim → lineup EV
graded against a real payout curve. One sport-agnostic spine; each sport is a plugin.

**Sports in scope:** golf, WNBA, tennis (live now) · NFL, NCAAF (Sept) · NBA (Oct).
**Never:** baseball / softball.

> Honest pitch: DFS is −EV after rake for most entries. This sells *edge and
> tooling* — better distributions, ownership, leverage, speed — never guaranteed
> profit. Lineups stay **PAPER** until a sport's backtest gate passes.

## Layout

```
bootstrap.R          resolve project root (no hardcoded cwd) + load spine/secrets
setup.R              install deps (binary) + bootstrap DuckDB schema
run_sport.R          live pipeline orchestrator (CLI)
spine/R/             sport-agnostic core (db, plugin contract, sim, optimizer, ...)
sports/<sport>/      per-sport plugins: ingest / project / correlate / roster
jobs/log_ownership.R OWNERSHIP LOGGER — run every slate, every sport (build #1)
config/contests.yml  payout curves + roster templates (VERIFY tagged)
data/                dfs.duckdb · raw/ · slates/ · ownership/ (landing) · lineups/
```

## Quick start

```powershell
# 1. install deps + create the DB (one time)
Rscript setup.R

# 2. train the projection models (pulls the CURRENT season automatically; weekly)
Rscript jobs/train_models.R --sport wnba

# 3a. ONE COMMAND, FULLY AUTOMATIC — auto-imports any ownership CSVs, auto-pulls
#     today's DK slates (classic OR showdown), logs salaries + projected ownership,
#     builds the dashboard for every sport. Schedule this (scripts/schedule_windows.ps1).
Rscript jobs/run_all.R --bankroll 300
#     open data/reports/dashboard_<today>.html in a browser.

# 3b. set it and forget it — register Windows scheduled tasks (run once):
powershell -ExecutionPolicy Bypass -File scripts\schedule_windows.ps1 -Bankroll 300

# 3c. (one time) to auto-collect ACTUAL ownership: enter free/cheap contests for the
#     slates you want, then paste your DK cookie into config/dk_session.txt
#     (see config/dk_session.txt.example). run_all then downloads + logs %Drafted
#     nightly for your entered+settled contests. No cookie? It skips gracefully.

# 4. Analyze a specific DK contest (real payouts; Showdown -> CAPTAIN board).
#    Paste any DK contest URL or id — detects Classic vs Showdown automatically.
Rscript jobs/contest.R --url "https://www.draftkings.com/draft/contest/191610408"
#    ...or add contests as dashboard tabs:
Rscript jobs/dashboard.R --contests 191610408 --bankroll 300

# (optional) single-sport HTML one-pager + xlsx + console card:
Rscript jobs/daily_report.R --sport wnba --bankroll 300
# after contests settle, log ownership (the un-backfillable asset):
Rscript jobs/log_ownership.R --csv "contest-standings-123.csv" --sport wnba --date <today> --contest 123 --type gpp --fee 20 --field 11764
```

**Everything defaults to today** (`Sys.Date()`). The dashboard auto-scrapes today's
DK slates on demand — if a sport shows "no slate yet," DK just hasn't posted it
(slates go up a few hours before lock; re-run closer to game time). WNBA form
auto-refreshes from `wehoop` when stale; retrain weekly for model quality.

Salaries, contest details, and the real prize-by-rank payout all come from DK's
public JSON (free). **Showdown / Captain Mode** is fully supported: the engine
optimizes 1 CPT (1.5× points & salary) + 5 FLEX from one game and surfaces a
**captain board** ranking who to put in the 1.5× slot — the biggest leverage call
in DFS.

The report tells you, for the day: the lineups (players/salary/proj/ceiling/own),
the contest types to enter, **how many entries and $ per entry** (sized to your
bankroll), top plays, and best-leverage. Real-money stakes appear only for contest
types whose backtest gate has passed — otherwise it's **$0 / PAPER** by design.

Everything is **free, repeatable, backtestable**: salaries come from DK's public
JSON endpoints (no key, no cost); stats from `wehoop` / Sackmann; ownership from your
own logged contest CSVs. No paid feeds.

Outputs land in `data/lineups/<sport>/`: an xlsx (Lineups + Rankings tabs) and a DK
upload CSV. Until a gate passes everything is flagged **PAPER**.

## The plugin contract (the whole abstraction)

A sport registers `project_players`, `correlation`, `roster_rules`, `dk_scoring`,
`ingest`. The core (`slate_sim` / `field_sim` / `optimizer` / `validate`) calls
**only** these. If adding a sport forces a core change, the abstraction is wrong.
The factor correlation model handles every case: positive same-game loadings
(NBA/WNBA/NFL stacks), opposite-sign loadings (tennis opponents, verified at
−0.40 in sim), or zero (golf).

## Validation & gates

`spine/R/validate.R` — three falsifiable layers (accuracy vs a Vegas/salary
baseline · distribution calibration · contest ROI vs a field rebuilt from **actual
logged ownership** under real payouts). It writes `data/gates_<sport>.rds`;
`run_sport` reads it and only marks lineups LIVE for contest types that passed.

## Golf

Your mature engine stays in `golf-modeling/`. `sports/golf/adapter.R` exposes it
through the contract (live projections from DataGolf) so the spine never depends on
golf's working directory — the bug in the old detached copies. Requires
`DATAGOLF_API_KEY`. Needs `xgboost ≥ 2.0` for the true quantile ceiling (else a
documented fallback).

## Known gaps (honest)

- **WNBA & tennis now have real models** (`sports/*/model.R`): WNBA minutes-first
  (xgboost, beats baseline on a real walk-forward holdout); tennis surface-Elo →
  DK mixture (validated on synthetic; pulls real ATP/WTA via `tennis_train()` on an
  unfiltered network). Both fall back to the DK-season-avg baseline for unmatched
  players. Train: `Rscript jobs/train_models.R --sport wnba|tennis --backtest`.
- **Historical DK salaries/ownership** aren't in the free stat feeds → the contest
  backtest (L3) needs the daily DK scrape (`jobs/scrape_salaries.R`) + logged
  ownership accumulating from day one. **This is why those two jobs run every slate.**
- **Roster templates** for WNBA/tennis (and the deferred NFL/NCAAF/NBA) are tagged
  **VERIFY** in `config/contests.yml` and each `roster.R` — confirm against a live
  DK slate. Multi-position eligibility (NBA "G/F") is simplified to the first slot.
- **Vegas anchoring is optional and free**: game environment is derived from the
  stat feeds (pace/ratings for WNBA, surface Elo for tennis) — no paid odds API.
  DK Sportsbook lines can be scraped free later if a market prior is wanted.
