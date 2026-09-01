# NFL pregame prediction and betting backtest

An R pipeline for predicting:

- home scoring margin (`home_score - away_score`);
- combined game total; and
- derived home win probability (diagnostic only).

The inputs are nflfastR play-by-play data and historical closing lines from
RotoWire's NFL game archive. The design adapts the supplied references while
using chronological, leakage-safe evaluation.

## Methodology

The shared pregame feature table contains only information available before
kickoff:

- rolling offensive and defensive EPA/play and success rate;
- pass EPA, rush EPA, yards per dropback, rushing yards per play;
- explosive-play, turnover, sack, and early-down pass rates;
- scoring, plays, rest, and season-to-date Pythagorean expectation;
- opponent counterparts for every team feature;
- RotoWire surface and weather fields.

The compact baseline follows the "five statistics" idea: passing production,
rushing production, giveaways, takeaways, and home field. Expanded models add
the efficiency and context fields above. Linear regression, random forest,
gradient boosting, and a small feed-forward neural network are compared.

Each backtest season is predicted using only earlier seasons. This walk-forward
design replaces random K-fold validation, which is unsuitable for a changing
league. The RotoWire closing line is always evaluated as a benchmark. Model
comparisons include MAE, RMSE, R-squared, ATS/total win rate, number of bets,
flat-stake profit, ROI, and 1,000 paired bootstrap confidence intervals.

Thresholds are measured in points of model edge, not percentages of the line.
They are selected only from prior validation seasons and must meet the configured
minimum bet count. Pushes return the stake. The default price is -110, so the
break-even win rate is 52.38%.

## Project layout

```text
config.yml
R/
  backtest.R
  features.R
  models.R
  odds.R
  utilities.R
scripts/
  00_install.R
  01_download.R
  02_build_features.R
  03_backtest.R
  04_report.R
  resolve_runtimes.ps1
run_pipeline.R
```

Generated data are written under `data/` and outputs under `outputs/`. Both are
ignored by Git except for their placeholder files.

## 2026 anytime-touchdown operation

The player-prop deployment is separate from the historical training pipeline.
It refreshes the current 2026 schedule, roster, consensus game lines, and
anytime-touchdown prices, then builds the forward bet card and operating
workbook:

```powershell
.\run_td_2026.ps1 -RefreshStats
```

Use `.\run_td_2026.ps1` without `-RefreshStats` when the current weekly player
statistics cache is already up to date.

The 2026 system is forward-locked in paper mode. Outdoor candidates remain
blocked until weather is verified in `config/td_2026_game_overrides.csv`, and
all candidates remain blocked until game-day active status is confirmed in
`config/td_2026_player_overrides.csv`.

## Fantasy and non-priced player-stat models

The separate fantasy projection suite models receptions, receiving yards,
rushing yards, passing yards, passing touchdowns, interceptions thrown,
rushing touchdowns, receiving touchdowns, and fumbles lost. It derives
standard full-PPR projections without sportsbook lines, prices, or betting
logic.

```powershell
.\run_fantasy_props.ps1
```

The workbook includes the full Week 1 PPR board, separate passing, receiving,
and rushing views, walk-forward metrics, feature importance, methodology, and
model checks.

## Over/under player props

Historical receiving-yard, reception, and rushing-yard lines for 2024-2025 are
cached under `data/raw/odds_api_player_props/`. The downloader always dry-runs
first and prints the exact credit cost before spending anything:

```powershell
. .\scripts\resolve_runtimes.ps1
& $rscript scripts/29_download_player_prop_odds.R                  # dry run
& $rscript scripts/29_download_player_prop_odds.R --execute        # spends
```

Useful flags: `--markets=`, `--seasons=`, `--weeks=` (cheap pilots),
`--max-events=` and `--quota-floor=`. Each market caches into its own directory,
so a partial pull is never repurchased and a market can be added later
independently.

Cost is 10 credits per game per market regardless of how many player lines come
back, which makes markets with few quoted players poor value — passing yards
returns about two players per game against twelve for receiving yards.

The backtest of the fantasy component projections against these lines is
[player_prop_backtest_report.md](outputs/player_prop_backtest_report.md). Its
conclusion is that the projections do **not** beat these markets, because a
conditional mean is the wrong statistic to compare with a two-sided price.

## Weekly live operation

Three scheduled jobs drive the 2026 season. All are safe to re-run.

```powershell
.\run_weekly_feed.ps1 -Job publish        # Tuesday
.\run_weekly_feed.ps1 -Job capture-close  # Sunday morning
.\run_weekly_feed.ps1 -Job grade          # Tuesday, after the week finishes
```

Register them with Task Scheduler:

```powershell
$root = "C:\Users\ljdie\OneDrive\Documents\NFL\run_weekly_feed.ps1"
schtasks /create /tn "NFL publish picks" /sc weekly /d TUE /st 10:00 `
  /tr "powershell -NoProfile -ExecutionPolicy Bypass -File $root -Job publish"
schtasks /create /tn "NFL capture close" /sc weekly /d SUN /st 09:00 `
  /tr "powershell -NoProfile -ExecutionPolicy Bypass -File $root -Job capture-close"
schtasks /create /tn "NFL grade picks" /sc weekly /d TUE /st 09:00 `
  /tr "powershell -NoProfile -ExecutionPolicy Bypass -File $root -Job grade"
```

**Why Tuesday.** Measured on the 2025 snapshots, a Tuesday line sits a median
123 hours before kickoff and still retains 75% of the bets that qualify at the
close, with zero side flips across 267 games. Betting the early number returned
+17.1% on totals and +19.3% on home spreads, against +20.6% and +1.8% at the
close. Publishing near kickoff would be operationally useless and is not better
on the evidence.

**Append-only.** `data/processed/published_picks_ledger.csv` records every bet
that has been sent. Its id, price, line and stake are then frozen — a later run
never revises them, even if the market moves or the bet would no longer qualify.
Only new bet ids are appended, so re-running is idempotent. Games that have
already kicked off are skipped.

**Closing-line capture** only snapshots games kicking off within 36 hours
(`--close-window=` to change). The first capture per bet wins, which is only
correct because of that window — an unfiltered capture run weeks early would
freeze a line that is nowhere near closing and make CLV meaningless.

Each publish or capture costs 2 Odds API credits.

## Bet Hub dashboard feed

Emits `picks_<date>.json` and `results_<date>.json` for the `nfl-modeling`
source, per `C:/dev/bet-dashboard/docs/sources/nfl-modeling.md`.

```powershell
. .\scripts\resolve_runtimes.ps1
& $rscript scripts/42_build_dashboard_feed.R --slate=2025-09-28 --results
cd C:\dev\bet-dashboard; pnpm ingest:dry
```

Output goes to `$FEED_DIR/nfl-modeling/nfl/`, defaulting to
`C:/Users/ljdie/OneDrive/Documents/dashboard_feed`.

**Markets carried:** totals, spreads, anytime touchdown. Yardage and reception
props are built but disabled — at the permitted books they run negative. Enable
with `--yardage-props` to revisit.

**Books:** DraftKings, FanDuel, Caesars, BetMGM, set in `feed_books()`. The
written spec names only the first two, but the contract types `book` as a plain
string with no enum, so all four validate. The two extra regulated books are
what make the touchdown tier viable: +0.83% on two books, +11.60% on four.
Offshore shops stay excluded.

All fourteen tracked strategies are registered in `nfl_bet_strategies()`. They
emit *candidates*, not bets: the portfolio is the union of the totals and
spread rules, the two touchdown tiers overlap, and the tight-end prop rules are
subsets of the expected-value rule. `dedupe_feed_bets()` collapses them to one
bet per market per selection, tagging every strategy that fired and sizing from
the strongest tier. Where an overlay would disagree with the primary model
signal the overlay is dropped rather than emitted as a second, opposing bet.

Prices are restricted to DraftKings and FanDuel because those are the only
books the feed accepts, and are re-derived from the raw per-book payloads
rather than reusing a best-of-eight price that could not be taken there. Edge
and EV are recomputed against the offered price. **This materially changes the
touchdown tier** — see the caveat in
[model_upgrade_report.md](outputs/model_upgrade_report.md).

## DFS salary history

The free salary pipeline uses RotoGuru for the historical DraftKings and
FanDuel backfill through 2021. RotoGuru's NFL tables are empty beginning in
2022, so the project does not silently fabricate or interpolate those salaries.

Run the one-time free backfill:

```powershell
.\run_dfs_salaries.ps1 -Backfill
```

For 2026 and later, save each official platform salary export under
`data/raw/dfs_salary_uploads/` with the season and week in its filename, such
as `DK_2026_week_01_main.csv`, then run:

```powershell
.\run_dfs_salaries.ps1 -IngestFiles
```

An optional current DraftKings capture is also available after NFL draft
groups open:

```powershell
.\run_dfs_salaries.ps1 -CaptureCurrent
```

That endpoint is undocumented and can change, so the official weekly CSV
archive remains the durable source of record.

## Run

The `.ps1` runners locate R and Node themselves via
`scripts/resolve_runtimes.ps1`, which checks the `NFL_RSCRIPT` and `NFL_NODE`
environment variables, then `PATH`, then the highest version under the standard
install roots. Nothing needs to be on `PATH` and no interpreter path is
hardcoded.

To drive the R pipeline directly, dot-source the resolver first:

```powershell
. .\scripts\resolve_runtimes.ps1
& $rscript scripts/00_install.R
& $rscript run_pipeline.R
```

Set `NFL_RSCRIPT` to pin a specific R build when several are installed.

For a quick smoke test, change `end_season` and `backtest_start` in
`config.yml` to cover fewer seasons.

## Important interpretation notes

- RotoWire labels its field `line` as the **home line**. A home line of `-3`
  implies a market home margin of `+3`.
- Per project specification, the archive's single line for each game is treated
  as the closing line.
- RotoWire's endpoint is public at the time of writing, but its terms and schema
  can change. The downloader caches the raw response and never hammers the site.
- Historical results do not establish future profitability. Report confidence
  intervals, bet count, drawdown, and performance by season—not only aggregate
  hit rate.

## Reference adaptations

- [Quinnipiac capstone](https://iq.qu.edu/experiential-learning/course-projects-and-capstones/student-projects/predicting-nfl-total-score-and-point-spread-bets/):
  separate margin/total regressions, model comparison, feature
  selection, betting edge thresholds, and profitability evaluation.
- [Weirich et al.](https://pmc.ncbi.nlm.nih.gov/articles/PMC12463883/):
  Pythagorean baseline, random forest, neural network,
  chronological evaluation, MAE/RMSE/R-squared, paired bootstrap comparisons,
  and feature importance.
- [Samford model](https://www.samford.edu/sports-analytics/fans/2023/How-I-Built-a-Competitive-NFL-Prediction-Model-with-Only-Five-Statistics):
  compact passing/rushing/turnover baseline and repeated game-outcome
  simulation logic (implemented here as residual simulation).
- [NFL Play Predictions](https://www.nflplaypredictions.com/model-build):
  careful feature audit, weather/context variables,
  multiple model families, and diagnostics beyond headline accuracy.
- [RotoWire NFL archive](https://www.rotowire.com/betting/nfl/archive.php):
  archived home lines, totals, scores, surface, and weather.
