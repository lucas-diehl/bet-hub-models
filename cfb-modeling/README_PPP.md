# CFB Points-Per-Possession (PPP) Betting Model

An opponent-adjusted **points-per-drive** projection model that estimates each
team's offensive & defensive efficiency (pace-normalized), projects a game's
margin and total, and bets when the projection diverges from the market.

Built alongside the older classifier (`complete_cfb_betting_model.R`), which is
kept untouched as a benchmark.

## Pipeline

| Script | Role | Reads | Writes |
|---|---|---|---|
| `01_build_possessions.R` | plays → drives → team-game PPP | `data_cache/pbp_data.rds` (519MB) | `drive_data.rds`, `team_game_ppp.rds` |
| `02_build_asof_ratings.R` | leak-free opponent-adjusted as-of ratings | `team_game_ppp.rds`, `sp_ratings.rds` | `asof_ratings.rds` |
| `00_ppp_common.R` | shared helpers (features, Approach A projection, eval) | — | — (sourced) |
| `03_models_backtest.R` | A/B/C bake-off + benchmark | ratings, betting lines | `ppp_backtest_results.csv`, `model_bakeoff_summary.csv` |
| `05_bet_slip.R` | per-year UNDER bet slip + flat/Kelly ROI | `ppp_backtest_results.csv` | `ppp_bet_slip_UNDER4.csv`, `ppp_bet_slip_UNDER3_DOG.csv` |
| `05_build_features.R` | coaching/sequencing PBP features (one 519MB pass) | `pbp_data.rds` | `team_game_features.rds` |
| `06_early_ats.R` | early-season 4th-down aggressiveness ATS bet slip | features, coaches, lines | `early_ats_bet_slip.csv` |
| `04_weekly_picks.R` | weekly UNDER recommendations (Kelly) | ratings, betting lines | `weekly_picks_<yr>_wk<w>.csv` |
| `07_write_feed.R` | write ATS+Totals picks/results to Bet Hub dashboard feed | ratings, features, lines | `<FEED_DIR>/cfb-modeling/cfb/picks_<date>.json`, `results_<date>.json` |
| `weekly_update.R` | hands-off driver: refresh CFBD -> rebuild -> auto-detect week -> write/grade feed | CFBD API | feed JSON + `weekly_update.log` |
| `run_weekly.bat` | launcher invoked by Windows Task Scheduler (`CFB-PPP-Weekly`, Mon+Thu 8am) | — | — |
| `run_all.R` | orchestrator (`--fast` skips 01/02) | — | — |
| `verify_pipeline.R` | correctness checks (sanity, leakage, headline) | caches, results | — |

**Run:** `Rscript run_all.R` (full, ~3-4 min) or `Rscript run_all.R --fast`
(reuse cached ratings). R 4.3.3; needs `dplyr, tidyr, xgboost, lme4, glmnet`.

## Method

- **Possessions:** one row per offensive drive; `net_pts` per drive (positive =
  offense scored, negative = defensive/ST score allowed). Meaningful drives only
  (kickoffs/end-of-half removed); optional score/period garbage-time toggle.
- **Ratings (the core):** iterative empirical-Bayes opponent adjustment
  (KenPom-style). `observed_off = adj_off_i + (adj_def_j − LG) + hfa·home`.
  Shrunk toward a preseason prior (prior-season carryover + SP+) with weight `K`
  games; recomputed **as-of each week** using only prior games (leakage-checked).
- **Approach A (winner):** opponent-adjusted PPD × expected possessions → points
  → projected margin/total; scale-calibrated on completed history (as-of).
- **Approaches B/C:** XGBoost regressions on PPP features (B market-blind, C
  market-aware). Both underperformed A → not used for betting.
- **Betting:** bet the total when `|proj_total − over_under| ≥ 4 pts`; quarter-Kelly
  staking (5% cap). Graded vs the **closing** line ("beat the close").

## Results (walk-forward 2021–2025, FBS-vs-FBS only)

### ⚠️ Headline: after fixing a data leak, there is NO reliable edge.

An earlier version of this model showed a strong totals edge (58.5% at ≥4 pts,
p<0.001). **That was largely a leakage artifact.** The preseason prior blended in
**same-season SP+ ratings**, and `cfbd_ratings_sp(year=Y)` returns *end-of-season*
SP+ — which already knows how season Y turned out. Because the rating shrinkage
(`K=5`) keeps the prior at ~30% weight *all season*, that lookahead contaminated
ratings every week.

Fixing it (prior-year SP+ only) collapsed the "edge":

| Totals, ≥3 pts | Leaky (same-season SP+) | **Fixed (prior-year SP+)** | SP+=0 (no prior) |
|---|---|---|---|
| Overall | 57.3% / +9.4% | **52.4% / −0.0%** | 52.1% / −0.6% |
| 2021 | 56.7% | 51.7% | 52.3% |
| 2022 | 56.0% | 49.5% | 50.4% |
| 2023 | 60.7% | 54.1% | 52.2% |
| 2024 | 60.9% | 57.2% | 55.7% |
| 2025 | 51.8% | 49.4% | 49.9% |

Both leak-free variants land at **~52–53% = breakeven** (−110 breakeven is 52.4%),
and Approach A's total RMSE (16.2) is now **worse** than the market's (15.7). The
"beat the close" result was the leak talking.

- **Recovery attempt (recency-weighted calibration, `PPP_RECENCY_HL`)** to fix the
  scoring-environment drift: **did not help** (still 52.4% overall, 2025 ~49.6%).
- **B / C (XGBoost) approaches:** no edge either.
- **Spreads:** ~50–52%, sign flips by season → noise. Disabled by default.

### The edge is on the UNDER side (asymmetry, not symmetry)
The symmetric model is breakeven, but that average hides a real asymmetry: the model
**over-projects totals every season** (proj − actual = +0.5 to +1.1), so its OVER
signals are biased noise and only its **UNDER** signals carry the edge. Betting
UNDER-only (leak-free backtest 2021–2025, `05_bet_slip.R`):

| Strategy | Bets | Win % | Flat ROI | Seasons positive |
|---|---|---|---|---|
| OVER ≥3 (for contrast) | 1,244 | 51.3% | −2.1% | 1/5 |
| **UNDER ≥4 pts** | 461 | **56.6%** | **+8.1%** | **5/5** |
| **UNDER ≥3 + model likes dog ATS** | 383 | **57.2%** | **+9.2%** | **5/5** |

UNDER-only is monotonic in conviction (≥2 53.7% → ≥6 58.3%) and — crucially —
positive in **all five seasons**, including 2025 (UNDER ≥4 = 66% there). 2025's
apparent "collapse" was entirely the OVER bets; the UNDER edge never decayed, it was
masked. The dog-ATS interaction (model also likes the market underdog to cover =
projecting a close, low-scoring game) tightens per-season consistency and lowers
drawdown.

**Caveats:** this is an in-sample subset (~20 combinations were tested; unadjusted
p≈0.03). What raises it above noise: structural rationale (the over-projection),
monotonicity, and 5/5 season consistency. The definitive test is **forward
performance on 2026** — treat the ROI above as an upper bound.

### Second edge: early-season 4th-down aggressiveness (SPREADS)
Coach-attributed 4th-down go-for-it rate (persisted across staff/seasons) predicts
ATS results **in the first weeks of the season only** — the market is slow to price
a stable coaching trait before it has current-season data. Bet the more-aggressive
team ATS when the go-rate gap is large (`06_early_ats.R`, leak-free, 2021–2025):

| Window (gap ≥0.10) | Bets | ATS Win % | ROI |
|---|---|---|---|
| Weeks 1–2 | 154 | 60.4% | +15.3% |
| Weeks 1–4 | 326 | 57.4% | +9.5% (5/5 seasons) |
| Weeks 5+ (control) | ~1,180 | ~50% | negative (no edge) |

Monotonic in conviction (gap ≥0.20 → 62%), specific to early weeks (clean weeks-5+
null), `lm(margin ~ -spread + agg_diff)` p=0.001 in wk1–4 vs p=0.56 after. Same
in-sample caveat — forward-test 2026. This is our only **spread** edge.

## Conclusion / going into 2026

Two plausible, season-consistent, in-sample edges: **UNDER totals** (`05_bet_slip.R`)
and **early-season 4th-down-aggressiveness ATS** (`06_early_ats.R`). Both need 2026
forward validation. What this project delivers:

1. A rigorous, reproducible, **leak-checked** PPP pipeline (a correct research
   platform — it prevented betting on a fake edge, which is itself valuable).
2. A clear negative result and a documented **UNDER-lean / over-projection**
   asymmetry as the only lead worth further, carefully-validated research.
3. Directions that could plausibly yield a *real* edge: model **totals earlier in
   the week vs the opening line** (softer than close); target **specific
   sub-markets** (tempo mismatches, extreme totals); add non-PPP signals (weather,
   injuries/QB availability, travel/rest); and validate any candidate on a **held-out
   future season**, never the tuning window.

## Honesty notes

- Backtest is walk-forward with as-of ratings AND as-of calibration; the leakage
  invariant is asserted by `verify_pipeline.R` (which now also guards against the
  win rate jumping back above ~56%, i.e. a leak returning).
- Graded against the **closing** line (hardest bar).
- Every number regenerates deterministically from `run_all.R` — unlike the older
  `bankroll_log_2025.csv`, which was stale and unreproducible.
- Known immaterial approximations: the league-mean centering constant `LG` and the
  SP+ scaling `sd` are computed over all seasons (fixed scale constants, not
  game-level predictors) — negligible, but noted for full disclosure.
