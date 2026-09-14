# CFB Game-Market Model — Agent Handoff

**Scope:** the college-football **spread (ATS)** and **totals (over/under)** picks produced
by this repo (`cfb-modeling`) and written to the Bet Hub dashboard feed. This is the
"game market" model (not player props / DFS). Read this before changing model logic.

**One-paragraph summary:** an opponent-adjusted **points-per-drive (PPD)** projection
model. It rates every team's offense and defense on a per-possession basis (leak-free,
as-of each week), projects each game's margin and total, and bets when the projection
diverges from the DraftKings line. Two edges survived honest, leakage-audited
walk-forward validation: **UNDER-only totals** and **early-season 4th-down-aggressiveness
ATS**. Everything is **PAPER** until it clears live 2026 forward validation.

---

## 1. TL;DR — what actually works (revised after the P0 review, 2026-09-01)

| Edge | Rule | Backtest (walk-forward 2021–2025, FBS-vs-FBS, push-excluded) | Status |
|---|---|---|---|
| **UNDER totals** | bet UNDER when `book_total − proj_total ≥ 4` (or ≥3 with dog-ATS) | **55.9% / ~+7% ROI** raw (≥4). Real but modest: **~54% after de-bias**; base rate ~50.7% so it *is* real selection; **season-inconsistent** (2021 below breakeven, carried by 2023 & 2025). | live, PAPER |
| **Early-season ATS** | weeks 1–4, more 4th-down-aggressive team when gap `≥0.10` | 57.4% / 326 bets, but **multiplicity-adjusted p≈0.8**, and **backtest CLV is FLAT (t=0.19)** — mechanism unconfirmed. On **CLV probation.** | PAPER, unconfirmed |

> **⚠️ Correction:** an earlier version of this doc headlined **58.5% / +11.7%** for the
> UNDER rule. That was an **error** — it was the *stale, leaky-era, both-sides* points-edge
> figure, not the deployed UNDER-only leak-fixed number. The canonical deployed figure is
> **55.9%** (push-corrected; 56.6% if pushes are miscounted as UNDER wins). See §5.1 and the
> P0 findings below. Always headline the **deployed slip** number, never the backtest table.

**Not edges (verified):** OVER totals (~50%, and the UNDER/OVER asymmetry survives de-bias),
spreads in general, tempo, play-call entropy, 2nd-and-short. See §7.

**Critical caveat:** both edges are **in-sample** (~20 combinations tested on 2021–2025).
The UNDER asymmetry survives a de-bias / matched-threshold test (§5.1) and rests on a ~50%
base rate, so it is real selection — but modest and season-dependent. The ATS edge rests on
one strong piece of evidence (the weeks-5+ specificity null) undercut by **flat CLV**; it is
**not confirmed**. **2026 forward CLV decides both** — see `FORWARD_TEST_2026.md` (rules
frozen). Treat all ROI figures as an upper bound.

### P0 review findings (2026-09-01) — read before trusting any number
- **P0.1 (reconciliation):** 58.5% was a leaked/both-sides doc error; canonical UNDER≥4 =
  55.9% push-corrected. **Push-handling bug:** `over_hit` counts a landed total (total==line)
  as an UNDER *win*; corrected grading is push-excluded (drops 2021 to 51.4%, below breakeven).
- **P0.2 (de-bias):** LOO per-season bias ≈ +0.75 pts. At matched thresholds on the de-biased
  projection, UNDER (54.2% @≥4) still clearly beats OVER (51.0%) → **asymmetry is real**, but
  the raw 56.6% partly rides the persistent over-projection bias (de-biased ≈54%).
- **P0.3 (base rate):** unconditional UNDER base rate ~50.7% pooled → real selection (+5.9 net).
  But net-of-base is **−2.0 in 2021**, +2.4 (2022), +13.4 (2023), +5.7 (2024), +13.8 (2025).
- **P0.4 (canary):** `verify_pipeline.R` VERIFY 3 is now **structural** (prior-year-SP+ code
  assertions), not an outcome band that watched the wrong population.
- **Pending (P1):** ATS coach-recency confound (career vs recency-weighted go-rate; tenure/tier
  controls), `model_prob` calibration plot, shrink the hard-coded ATS probs, multiplicity-
  adjusted p table, doc-vs-code segment-guard reconciliation, feed `status`/`prob_source`/line
  fields, DK-vs-consensus book check. None started; all gated after P0 (now done).

---

## 2. Repo map (execution order)

| File | Role | Key outputs |
|---|---|---|
| `01_build_possessions.R` | pbp → drives → team-game PPD + pace (garbage-time filtered) | `data_cache/{drive_data,team_game_ppp}.rds` |
| `02_build_asof_ratings.R` | iterative opponent-adjusted **as-of** ratings + **preseason seed** | `data_cache/asof_ratings.rds` |
| `05_build_features.R` | coaching/sequencing PBP features (entropy, 4th-down, 2nd-short) | `data_cache/team_game_features.rds` |
| `00_ppp_common.R` | **shared library** — everything below sources it | (functions) |
| `03_models_backtest.R` | A/B/C bake-off + benchmark, walk-forward | `ppp_backtest_results.csv`, `model_bakeoff_summary.csv` |
| `05_bet_slip.R` | per-year UNDER bet slip + flat/Kelly ROI | `ppp_bet_slip_*.csv` |
| `06_early_ats.R` | early-season ATS bet slip | `early_ats_bet_slip.csv` |
| `04_weekly_picks.R` | weekly UNDER recommendations (standalone) | `weekly_picks_*.csv` |
| `07_write_feed.R` | **production**: write picks/results JSON to the dashboard feed | `dashboard_feed/cfb-modeling/cfb/{picks,results}_<date>.json` |
| `weekly_update.R` + `run_weekly.bat` | hands-off driver (refresh → rebuild → detect week → 07) | `weekly_update.log` |
| `run_all.R` | full pipeline orchestrator (`--fast` skips heavy rebuild) | — |
| `verify_pipeline.R` | leakage/sanity asserts | — |

Legacy: `complete_cfb_betting_model.R` (old XGBoost cover/over classifier) is kept only as
a **benchmark** — it has no edge; do not build on it.

`README_PPP.md` is the user-facing overview; this file is the deeper methodology handoff.

---

## 3. Data & environment

- **Source:** `cfbfastR` / CFBD API. Cached in `data_cache/*.rds`: `pbp_data` (~519MB,
  2019–present), `game_info`, `betting_lines`, `sp_ratings`, `coaches`,
  `recruiting`, `returning_production`, `adv_stats`.
- **API key** in `.Renviron` (`CFB_API_KEY`). **The CFBD API is rate-limited — be sparing.**
  All model/feature work here can be done from cache; only `weekly_update.R`'s refresh hits
  the network.
- **R** at `C:\Program Files\R\R-4.3.3\bin\Rscript.exe` (not on PATH). Packages:
  `dplyr, tidyr, xgboost, jsonlite, glmnet, lme4, cfbfastR`.
- **Books:** cfbfastR carries **DraftKings** but **not FanDuel** — lines are DK-only in
  practice (`book_lines()` prefers DK, keeps a FanDuel branch if it ever appears). Games
  neither book prices are dropped.
- **Universe:** FBS-vs-FBS only (non-FBS teams collapse to `"FCS"` and are excluded).

---

## 4. Methodology

### 4.1 Possessions (`01`)
A **possession = one offensive drive**. Points per drive uses `net_pts` (negative when the
defense/ST scores). Only **meaningful drives** count (kickoffs / end-of-half removed).
**Garbage-time filter is score+period based** (defined *before* features, never tuned to
results): H2 margin >28 or Q4 margin >21. (Do **not** use win-probability for this —
favorites sit >0.95 WP from kickoff and it nukes the whole game.) Score reconciliation
verified: gross offensive drive points ≈ actual team points (median diff 0).

### 4.2 Opponent-adjusted as-of ratings (`02`)
Iterative empirical-Bayes (KenPom-style), 8 iterations:
```
observed_off(i vs j, home) = adj_off_i + (adj_def_j − LG) + hfa·home + resid
```
- Metrics adjusted (offense & defense each): **ppd**, **epa**, **pass_epa**, **rush_epa**,
  **td_rate** (explosiveness), **rz_ppd** (finishing). Plus an additive **pace** model
  (`total_drives = pace_i + pace_j`).
- **Shrinkage** `K=5` games toward a **preseason prior** = regressed prior-season carryover
  (`REG=0.5`) blended with **prior-year SP+** (`SP_WEIGHT=0.35`). Prior dominates early,
  data takes over by ~week 5.
- **Leak-free:** for each `(season, week)` only games with `week < w` are used (asserted in
  code). SP+ is **prior-year** — see the leakage lesson (§8).
- **Preseason seed** (cold-start fix): after the main loop, `02` emits weeks 1–4 as-of rows
  for the *next unplayed* season from `build_priors()` so openers are modelable before any
  games exist. Superseded automatically once real games arrive.

### 4.3 Projection — "Approach A" (the model that won the bake-off)
```
exp_drives = (pace_home + pace_away) / 2
home_ppd   = adj_off_ppd_home + adj_def_ppd_away − LG + hfa_ppd
away_ppd   = adj_off_ppd_away + adj_def_ppd_home − LG − hfa_ppd
proj_margin = (home_ppd − away_ppd) · exp_drives
proj_total  = (home_ppd + away_ppd) · exp_drives
```
Then a **scale calibration** `lm(actual ~ proj)` fit **as-of** on completed history (the raw
PPD projection is over-extreme — RMSE worse than naive without it). Probabilities via a
**normal residual model**: `P(under) = Φ((proj_total − OU)/σ_t)`, σ from calibration
residuals; `P(cover)` analogous with σ_m.

### 4.4 Bake-off result (`03`)
- **A** = pure opponent-adjusted PPD projection → **winner**.
- **B** = XGBoost regression on PPP matchup features (market-blind) → no edge.
- **C** = XGBoost market-aware → no edge (just relearns the line).
- **Benchmark** = old cover/over classifier → no edge.
Fitted-weight ML never beat the structured projection or the market; the edge lives in
the projection's **directional disagreements on totals**, not in point accuracy (A's totals
RMSE ~= the market's; it does **not** beat the market on RMSE).

---

## 5. The two edges in detail

> ### ⚠️ Forecast-encompassing finding (2026-09, decisive — read first)
> `lm(actual ~ market_line + projection)` on the walk-forward backtest:
> - **Margin:** `proj_margin` coef **0.011, p=0.76** → the market **fully encompasses** our
>   margin projection. **ATS is definitively closed by projection** (established, not inferred
>   from RMSE). There is no thin tail to exploit with the current projection.
> - **Totals:** `proj_total` coef **−0.035, p=0.65** → the market **also fully encompasses**
>   our totals projection. **The UNDER edge is therefore NOT projection skill** — it is a narrow
>   **large-disagreement / directional / post-2023-regime tail effect** (see §5.1), consistent
>   with the "artifact, not bias" alternative. Whatever edge exists lives in the *tail behavior
>   of big disagreements*, not in the projection carrying information the market lacks.
> - **A-prime tested and also dead (2026-09):** projecting from **component** ratings doesn't help
>   either — `actual_margin ~ −spread + net_pass + net_rush + net_expl + net_epa + net_fin` has
>   **every component p > 0.35** (net_pass, the lasso's #1 driver, p=0.84). The market encompasses
>   our aggregate *and* component ratings.
> - **Strategic implication (important):** **no projection improvement can create ATS edge** — not
>   A-prime, component ratings, per-metric shrinkage, mixed model, or a better QB-blind projection —
>   because the market already prices everything our efficiency ratings know (both markets:
>   proj_margin p=0.76, proj_total p=0.65, all components p>0.35). Edge, if it exists, can only come
>   from **(a) information the market lacks or prices bluntly** (QB *backup-quality* mispricing —
>   review §5, the one avenue with a mechanism) or **(b) transient tail/regime effects** (the totals
>   large-disagreement post-2023 inefficiency; early-ATS H2). RMSE-improving features (§9/§10) are
>   worth doing for **totals σ/calibration display**, not for creating edge.

### 5.1 UNDER-only totals  *(numbers revised after P0 review; edge is a tail effect, not projection skill)*
The model **over-projects totals every season** (LOO bias ≈ +0.75 pts). Deployed rule and
its honest qualifications:

| Rule (push-excluded) | Bets | Win% | ROI | Seasons ≥ breakeven |
|---|---|---|---|---|
| **UNDER ≥4 (deployed)** | 454 | **55.9%** | +6.8% | 4/5 (2021 = 51.4%, below) |
| UNDER ≥3 + dog-ATS | 379 | 56.7% | +8.3% | 4/5 |
| OVER ≥3 (contrast) | 1,244 | 51.3% | −2.1% | 1/5 |

- **De-bias test (P0.2):** removing the LOO per-season bias and testing both sides at
  *matched* thresholds → UNDER still beats OVER (≥4: 54.2% vs 51.0%; OVER ≤51%, 1/5 seasons).
  **The asymmetry is real**, but the raw 55.9% partly rides the persistent over-projection;
  the de-biased edge is ~**54%**.
- **Base rate (P0.3):** unconditional UNDER base rate ~**50.7%** pooled → real selection, but
  net-of-base is **−2.0 (2021)**, +2.4, +13.4, +5.7, +13.8 — concentrated in 2023 & 2025.
- **Segment observations — NOT deployed** (reconciling old §5.1 vs §6/§11): near-pick'em
  (`|spread|≤3` ~48%) and low totals (~50%) underperform, but adding those filters **did not
  survive per-season checks (overfit)**, so they are *observed variation only*, not part of
  the deployed rule. Do not deploy them without fresh validation.
- **`model_prob` is over-confident** (§7): calibration slope 0.13; `P_under≥0.60` predicts
  65% but realizes 56%. The *selection* uses the points-edge (fine); the displayed prob is not
  calibrated. See `outputs/totals_calibration.csv`.
- **⚠️ REGIME / decay risk (P1 §10b):** the edge is **heavily post-2023** — UNDER≥4 wins
  **61.6% (2023–25) vs 52.0% (2021–22)**, with projection bias *growing* (0.62→0.79). This
  looks like a **clock-rule transition inefficiency** (2023 running-clock rules cut pace; the
  model's carryover/prior pace lags → over-projects → unders hit). It is **strongest in weeks
  1–2 (64%)** — where the preseason pace estimate is stalest. Mechanism implication: "fixing"
  the pace lag would remove the bias *and* the edge. **Expect decay** as pace stabilizes / the
  market and model catch up. Watch the pre/post split live.
- **Robustness (P1 §10d, §5.1):** block-bootstrap (by season-week) 95% CI = **[50.7%, 61.2%]
  — includes breakeven**; after multiplicity adjustment UNDER≥4 (raw p=0.071) fails, only the
  higher-volume **UNDER≥3 survives** (BH≈0). The ≥4 vs ≥3 threshold is a real degree of freedom;
  ≥3 is statistically more robust but includes weaker bets. `UNDER3_DOG` earns its keep (its 99
  marginal bets hit 57.6%, 4/5 seasons).
- **Book selection (P1 §9, Odds API):** DK totals run only **+0.13 pts above consensus** (at
  consensus 73% of the time). So ~3% of a 4pt edge is fading DK; the rest is model skill — the
  UNDER edge is **not** primarily a "DK is off-consensus" artifact.

### 5.2 Early-season 4th-down-aggressiveness ATS  *(unconfirmed — on CLV probation)*
Coach-attributed cumulative **go-for-it rate** in decision-range 4th downs (persisted via
`coaches.rds`). Bet the **more-aggressive** team ATS, weeks 1–4.

| Window / conviction | Bets | Win% | ROI | p (raw) |
|---|---|---|---|---|
| Wk 1–2, gap ≥0.10 | 154 | 60.4% | +15.3% | 0.028 |
| **Wk 1–4, gap ≥0.10** | 326 | 57.4% | +9.5% | 0.041 |
| Wk 1–4, gap ≥0.20 | 92 | 63.0% | +20.4% | 0.026 |
| **Wk 5+ (control)** | — | ~50% | negative | — |

**For (two independent supports):** (1) the clean **weeks-5+ specificity null**
(`lm(margin ~ −spread + agg_diff)` p=0.001 wk1–4 vs 0.56 wk5+); (2) **recency confound
refuted (P1 §5.2):** `agg_diff` is uncorrelated with career size (r=0.00), tenure (−0.05),
P5 tier (−0.04), and **survives those controls (β=+12.4, p=0.003)** — it is *not* a
"bet newer coaches" proxy. Recency-weighting ≈ career (56.9% vs 57.4%), so career rate stays.

**Against / hypothesis (updated 2026-09):** the "market is slow" mechanism (**H1**) is **dead** —
backtest CLV is flat (+0.025, t=0.19), and the higher-powered test `lm(line_move ~ agg_diff)`
over *all* wk1–4 games is **p=0.46** (no line movement toward the aggressive team). What survives
is the bolder **H2: the closing line is persistently wrong on this dimension.** H2 predicts flat
CLV + positive results, so **CLV cannot test it** — only long-run results can, and at ~65 bets/yr
one season can't distinguish 57% from 52% (needs ~6 seasons). So this is a **multi-season paper
track with no short-horizon read**, not a "confirm-in-2026" item. Also: **multiplicity** (raw
p=0.041 → BH≈0.08). **Deployed expectation = the β-implied win rate, not the observed:** β=11.7
implies **~55.5%** at gap≥0.10 (observed 55.1% — honest) and **~58%** at gap≥0.20 (observed 63%
is luck-inflated). `model_prob` (0.593/0.546) is empirical/in-sample (`prob_source: empirical_insample`).

---

## 6. Production bet-selection rules (`07_write_feed.R`)

Config constants (top of `07`): `UNDER_EDGE=4, DOG_EDGE=3, AGG_GAP=0.10, N4_MIN=20,
EARLY_WEEKS=1:4, ODDS=-110`.

- **Totals (UNDER only):** take when `(book_total − proj_total) ≥ 4` **OR**
  (`≥3` **AND** model likes the dog ATS), and EV>0 at −110. Tags `UNDER4` / `UNDER3_DOG`.
- **ATS:** weeks 1–4, both teams `n4 ≥ 20` 4th-down history, `|go-rate gap| ≥ 0.10` → bet the
  more-aggressive team. Stake: gap ≥0.20 → 1u ("STRONG"), else 0.5u. `model_prob` = empirical
  tier win-rate (0.60 / 0.55) since the rule has no calibrated probability.
- **Grading & CLV:** post/grade split — picks are only written for **not-yet-started** games;
  results are graded from the **previously-posted** picks file (so we grade the number we
  actually bet), with `closing_odds_american` and `clv_pct = Φ((posted−closing)/σ) − 0.5`.
  **No backfill** (only games we posted get graded).
- `SIMULATE=1` replays historical open→close in one pass (for testing).

---

## 7. What was tested and REJECTED (honest negatives)

The bar is **beating the market** — a feature must be significant *beyond the line*
(`lm(target ~ line + feature)`), not merely correlated with outcomes.

| Idea | Result |
|---|---|
| Tempo Imposition (totals) | p=0.94 beyond `over_under` — market prices pace |
| Play-call entropy, 4th-down aggressiveness, 2nd-and-short (as line-beating features) | all p>0.5 on spreads |
| "Predictable offense → UNDER" | reversed & inconsistent |
| "Less-predictable team covers ATS" | 48.9% (false) |
| Feature-weight lasso (learn weights, CBB-style) | diagnostic only: passing matchup drives margin; explosiveness/rush/4th-down-agg drive totals; **pace = zero weight**. No ROI gain (RMSE worse than market). Did independently **replicate** the UNDER edge (55.5%, 5/5) — reassuring. |

Lesson: the "establishment" rate stats (efficiency, pace, explosiveness, success) are
efficiently priced. Coaching/behavioral signals are the whitespace, but only the
**early-season** 4th-down one cleared the bar so far.

---

## 8. The leakage lesson (read this before touching priors)

An early version showed a **58.5% totals edge** that was **mostly a data leak**: the
preseason prior blended **same-season** SP+, and `cfbd_ratings_sp(year=Y)` returns
**end-of-season** ratings (it knows how the year turned out). Because `K=5` shrinkage keeps
the prior at ~30% weight *all season*, that lookahead contaminated every week. Fixing it to
**prior-year** SP+ collapsed totals to **52.4% (breakeven)**; confirmed with `SP_WEIGHT=0`
(52.1%). The tell was in the pattern: strong early seasons, weak most-recent season (the
recent year benefits least from a prior that "knows the future").

**Guards now in place:** SP+ is prior-year only; `verify_pipeline.R` asserts the as-of
leakage invariant and flags if the totals win-rate jumps back above ~56% (a leak returning).
**Any new feature must be as-of (week `<w`) and validated with the beat-the-market test.**

---

## 9. Output contract & automation

- **Feed:** `dashboard_feed/cfb-modeling/cfb/picks_<date>.json` + `results_<date>.json`,
  one file per game day (games grouped by ET date), contract 1.0
  (`dashboard_feed/_schema/`). Fields include `book` (DraftKings), `model_prob`,
  `market_prob`, `edge`, `ev_pct`, `stake_units`, `tags`, `details`; results carry
  `closing_odds_american` + `clv_pct`. `mode: "PAPER"`.
- **Do NOT touch hosting** — Supabase + Vercel + the dashboard ingest are already wired and
  owned by the `bet-dashboard` repo. This model's only job is writing the feed JSON.
- **Automation:** `CFB-PPP-Weekly` scheduled task (Mon/Thu 8am, S4U so it runs logged-out) →
  `run_weekly.bat` → `weekly_update.R` → `07`. Hardened: idle gate (fast exit off-season),
  rebuild only on new completed games, heavy steps isolated as subprocesses with
  **sequential future plan** (no orphaned R workers), always reaches `07`.

---

## 10. Config / hyperparameters

| Param | Value | Where |
|---|---|---|
| `K_SHRINK` | 5 games | `02` |
| `ITERS` | 8 | `02` |
| `REG` (carryover regression) | 0.50 | `02` |
| `SP_WEIGHT` (prior-year SP+) | 0.35 | `02` (env `PPP_SP_WEIGHT`) |
| `UNDER_EDGE` / `DOG_EDGE` | 4 / 3 pts | `07` |
| `AGG_GAP` / `N4_MIN` / `EARLY_WEEKS` | 0.10 / 20 / 1:4 | `07` |
| odds assumption | −110 | throughout |

None of these were grid-searched hard; the totals/ATS thresholds were chosen from
conviction curves (monotonic), not knife-edge optima.

---

## 11. Known limitations & open work

- **In-sample edges** — 2026 is the real forward test. Track live ATS% / UNDER% / CLV.
- **Week-1 totals** lean on the preseason seed (noisy); real ratings kick in week 2+. ATS
  works from day one (coach-stable).
- **DraftKings-only** lines; odds modeled at −110 (no line-shopping, no live juice).
- **Machine-local pipeline** — the PC must be on for the model/ingest tasks to run.
- **Multiple-comparison risk** — ~20 combos tested; per-season consistency + mechanism +
  specificity checks are the discipline. Don't add filters that only help in aggregate
  (e.g., a totals-range filter degraded 2024 → overfit).
- **Open leads worth clean validation:** the dog/away ATS tilts; sharp-confirmation as an
  ATS booster; a de-biased totals projection (the over-projection is structural); the
  entropy×opponent-defense interaction and true WP-optimal 4th-down cost (team-level nulls
  make me skeptical). Player-EPA feature work (`team_epa_lookup`) is a side quest, not a
  blocker.

---

## 12. How to reproduce / run

```powershell
$rs = "C:\Program Files\R\R-4.3.3\bin\Rscript.exe"
cd C:\Users\ljdie\OneDrive\Documents\cfb-modeling

& $rs run_all.R                 # full rebuild (touches 519MB pbp; ~6-8 min)
& $rs run_all.R --fast          # reuse caches, just backtest + slips + picks
& $rs verify_pipeline.R         # leakage + sanity asserts

# one week's live feed (offline, cached lines):
$env:PPP_SEASON=2026; $env:PPP_WEEK=2; & $rs 07_write_feed.R

# backtest results live in ppp_backtest_results.csv; bet slips in ppp_bet_slip_*.csv /
# early_ats_bet_slip.csv
```

Everything is deterministic (`set.seed(42)`) and regenerates from cache — unlike the old,
unreproducible `bankroll_log_2025.csv`, which should be ignored.
