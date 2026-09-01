# bet-hub-models

The four models behind **Bet Hub**, wired to run on **GitHub Actions** so picks flow
to the site with the laptop off. Each model writes schema-compliant JSON to a scratch
`FEED_DIR`, then the shared ingest step upserts it into Supabase (which the site reads).

```
golf-modeling/   72-hole matchup + top-10 golf bets (DataGolf)      -> workflow: golf
cfb-modeling/    college-football weekly picks (CFBD)               -> workflow: cfb
NFL/             NFL weekly picks (The Odds API + nflverse)         -> workflow: nfl
dfs-engine/      DFS projections + the /dfs optimizer pool feed     -> workflow: dfs
.github/
  actions/ingest/   composite: checkout bet-dashboard -> pnpm ingest -> Supabase
  workflows/        one per model (schedule + manual dispatch)
```

## One-time setup

1. **Push this repo** to `github.com/lucas-diehl/bet-hub-models` (public is fine).
2. Add **Actions secrets** (repo → Settings → Secrets and variables → Actions → New):

   | Secret | Used by | What it is |
   |--------|---------|------------|
   | `DATABASE_URL` | all (ingest) | Supabase Postgres connection string (same one the site uses) |
   | `DATAGOLF_API_KEY` | golf, dfs | DataGolf API key |
   | `CFB_API_KEY` | cfb | CollegeFootballData (CFBD) API key |
   | `THE_ODDS_API_KEY` | nfl, dfs | The Odds API key |

3. Open the **Actions** tab and run each workflow once via **Run workflow**
   (`workflow_dispatch`) to verify, then let the crons take over.

## How it runs

- **Paths are env-driven.** `FEED_DIR` is set per job; the golf entry scripts self-locate
  their root on Linux; cfb/NFL guard their Windows `setwd` with `dir.exists`, so on the
  runner they simply stay in the checked-out model dir.
- **R packages** install from Posit Package Manager (binary, fast) and are cached per model.
- **Heavy data** (`data_cache`, nflverse `data`, `_targets`, `ownership_inbox`, the DuckDB)
  is git-ignored and rebuilt/cached in CI — see `.gitignore`.
- **Cron is best-effort** (GitHub may fire 5–20 min late); fine for daily/weekly picks.

## Local runs

Still work on Windows unchanged (the Windows paths are tried first). To run a model
locally against the live DB, set `FEED_DIR` + the relevant key in `.Renviron` (git-ignored).

## Notes

- The **pick-freeze** lives in the ingest (bet-dashboard): once a bet is posted, a re-emit
  can't change its line/odds/side/selection/stake — so re-running a workflow is safe.
- First CI runs of a new model often need a round of iteration (a missing package or data
  path shows up in the runner log); that's normal bring-up.
