# CFB Betting Model — Complete Data-Scientist Handoff

**Repo:** `c:\Users\ljdie\OneDrive\Documents\cfb-modeling`
**Last updated:** 2026-09-03
**Audience:** an incoming data scientist taking full ownership. This is the single
self-contained reference — every data source, feature (tested and deployed), method,
interaction, result, and open lead. Companion docs (`HANDOFF.md`, `FEATURE_REGISTRY.md`,
`ROADMAP.md`, `ATS_GAP.md`, `FORWARD_TEST_2026.md`) are deeper dives on specific topics;
this doc supersedes and summarizes them.

---

## 0. Executive summary

The project is an opponent-adjusted **points-per-drive (PPD)** college-football model that
projects each game's margin and total, compares to the market line, and bets the
disagreement. It writes paper picks to a personal betting dashboard ("Bet Hub").

**Two bettable signals are live (both PAPER), plus one validated candidate:**
1. **UNDER totals** — bet the under when the model's total is ≥4 points below the book total.
   Backtest 55.9% (push-excluded, 2021–2025). *Executable today* (needs only lines + ratings).
2. **Backup-QB ATS fade** — fade a team forced to start a backup QB. Backtest: backup team
   covers ~45% (fade ~55%), strongest (59.5%) when the backup is a true unknown.
   *Not yet executable* — requires a **pregame availability feed** we don't ingest (we can
   currently only detect a backup start retrospectively). This is a #1 infrastructure unlock.
3. **`proj_vs_open` — projection beats the OPENING spread (DEPLOYED 2026-09-08, tag `OPEN_ATS`).**
   The margin projection is encompassed by the *closing* line but **not by the open**: on clean,
   timestamped Odds API DK opens (2023–25) `lm(actual ~ open + proj)` gives proj coef **0.138,
   p=0.004**; betting the disagreement at the open covers **~53–54%** (not decaying — 2025 is
   best at 56%), and forward CLV is the deciding metric (backtest +0.31/+0.43pt). *Executable* —
   the opening-line capture pipeline (`09_capture_open_lines.R`) is built and live from week 2,
   2026, feeding `07_write_feed.R`'s new Arm O. See §5.3 for the pipeline, and §9 for a scheduling
   gap disclosed there (capture timing currently needs the user logged in for full freshness).

**The single most important finding** (drives all strategy): the **closing** market line
**forecast-encompasses** every efficiency projection we have built. In `lm(actual ~ closing_line
+ our_projection)`, our projection's coefficient is insignificant on both markets (margin p=0.76,
total p=0.65, all rating components p>0.35, offense/defense ELO p=0.77, per-QB ELO p=0.39).
**Consequence: no team-efficiency rating — however clever — can create edge *against the close*.**
Edge can only come from (a) information the market lacks/prices bluntly (the availability family),
(b) directional/tail/regime effects the market shares (the UNDER edge), or (c) **beating a
*softer, earlier* number than the close** (`proj_vs_open` — the same ratings that are behind
Sunday's close are ahead of Sunday's open). **Caveat on the test itself (important):** the
encompassing regression measures the *conditional mean* and is *underpowered by collinearity*
(`cor(proj,spread)≈0.81`); its CI on margin was [−0.06, +0.08], whose upper bound implies ~1pt of
edge on large disagreements. So "encompassed" means "not a usable *close* edge here," **not**
"provably zero." Record borderline encompassing tests as *inconclusive*, report the CI, and
confirm with a direct selection/threshold test — that discipline is what surfaced `proj_vs_open`.

---

## 1. Current live state

- **Feed emits UNDER totals only.** ATS is **removed** from the live feed
  (`07_write_feed.R`, `EMIT_ATS` gate defaults off; re-enable with env `PPP_EMIT_ATS=1`).
  The early-season 4th-down ATS arm was removed after the encompassing tests (see §6.3).
- **Book:** DraftKings (cfbfastR carries DK, not FanDuel). Odds modeled at −110.
- **Universe:** FBS-vs-FBS only (non-FBS collapse to `"FCS"` and are excluded).
- **Automation:** scheduled task rebuilds ratings and writes the feed hands-off; a separate
  15-min `Bet-Ingest` task (in the `bet-dashboard` repo) upserts the feed into Supabase.
- **Everything is PAPER** pending live 2026 forward validation (`FORWARD_TEST_2026.md`).

---

## 2. Data sources

### 2.1 External APIs
| Source | Access | Use | Notes |
|---|---|---|---|
| **cfbfastR / CFBD API** | key `CFB_API_KEY` in `.Renviron` | play-by-play, games, lines, SP+, coaches, recruiting, returning production, advanced stats | **Rate-limited — be sparing.** All modeling runs from cache; only `weekly_update.R`'s refresh hits the network. |
| **The Odds API** | key `ODDS_API_KEY` in `.Renviron` | open/close lines from DK/FanDuel/Caesars for line-shopping work (Category C, not yet built) | **Token-limited — user asked to be careful.** Only touch when explicitly doing line-capture work. |

### 2.2 Cached data (`data_cache/*.rds`) — the working substrate
All modeling can and should be done from these caches (no API calls needed).

| File | Size | Shape | Contents / key columns |
|---|---|---|---|
| **pbp_data.rds** | 519 MB | 1,499,964 × 362 | Play-by-play 2019–2025. `EPA`, `wpa`, `success`, `pass_attempt`, `sack`, `down`/`distance`, `pos_team`/`def_pos_team`, `passer/rusher/receiver_player_name` + player IDs, `target_player`(17% pop)/`receiver_player_name`(83%), `fg_make_prob`/`fg_made`+`play_type` for FGs, `period`, win prob, timeouts, `home/away_pregame_elo`. **Read `names()` only — do not repeatedly load-and-process the full 519MB.** |
| **drive_data.rds** | 5 MB | 205,563 × 21 | One row per drive. `net_pts` (neg when defense/ST scores), `garbage` flag, `n_plays`, `mean_epa`, `start_period`, `reached_rz`, `pos_team`/`def_pos_team`. Basis of PPD + variance features. |
| **game_info.rds** | 0.8 MB | 22,971 × 32 | One row per game. Scores (`home_points`/`away_points`), `start_date` (ISO), `neutral_site`, `venue_id`, conference, `home/away_pregame_elo`, `excitement_index`. |
| **betting_lines.rds** | 0.4 MB | — | **Multi-provider** (Bovada, ESPN Bet, DraftKings, Caesars, William Hill, consensus; median 3, up to 6/game). `spread`, `over_under`, `spread_open`(~35% pop), `over_under_open`(~29%), moneylines(~33%). **31.6% of games have cross-book spread range ≥1pt; 18% totals ≥1.5.** Underused — enables the whole line-shopping category offline. |
| **team_info.rds** | — | 924 × 29 | `latitude`, `longitude`, `elevation`(**sparse/unreliable** — see B4), `timezone`, `dome`, `grass`, `venue_id`, conference, `classification` (FBS/FCS). |
| **asof_ratings.rds** | 1.2 MB | — | Output of `02` — leak-free opponent-adjusted ratings per (season, week). |
| **team_game_ppp.rds** | 0.9 MB | — | Output of `01` — team-game PPD + pace, garbage-filtered. |
| **team_game_features.rds** | 0.5 MB | — | Output of `05_build_features` — coaching/sequencing features. |
| adv_stats.rds / advanced_stats.rds | 4.1 MB | — | CFBD advanced season stats. |
| sp_ratings.rds, coaches.rds, recruiting.rds, returning_production.rds, team_records.rds, team_info.rds | small | — | CFBD reference tables. `coaches.rds` persists coach→team for the 4th-down feature. |
| model_data_{raw,complete,v2,v3}.rds | 0.7–3.5 MB | — | Legacy feature frames for the old XGBoost model (benchmark only). |

### 2.3 Derived caches created for the availability/ELO research (this session)
| File | Built by | Contents |
|---|---|---|
| qb_primary_passers.rds | pbp | primary passer per team-game (att≥8), ts-ordered |
| qb_game_table.rds | pbp | season-modal QB + starter-out flag |
| qb_elo_features.rds | derived | per-QB as-of ELO, `qb_surprise`, expected-starter rating |
| elo_features.rds | game_info | off/def team ELO + 1/2/4/6-game momentum deltas |
| wr_targets.rds / wr_out_features.rds | pbp | per-receiver targets; as-of top-receiver-out flag + target share |
| fg_attempts.rds | pbp | FG attempts with `made` + expected make prob (for FGOE) |

---

## 3. Repository map (execution order)

| File | Role | Key outputs |
|---|---|---|
| `01_build_possessions.R` | pbp → drives → team-game PPD + pace (garbage filtered) | `drive_data.rds`, `team_game_ppp.rds` |
| `02_build_asof_ratings.R` | iterative opponent-adjusted **as-of** ratings + preseason seed | `asof_ratings.rds` |
| `05_build_features.R` | coaching/sequencing PBP features (entropy, 4th-down, 2nd-short) | `team_game_features.rds` |
| `00_ppp_common.R` | **shared library** — sourced by everything below | (functions) |
| `03_models_backtest.R` | A/B/C bake-off + benchmark, walk-forward | `ppp_backtest_results.csv`, `model_bakeoff_summary.csv` |
| `05_bet_slip.R` | per-year UNDER bet slip + flat/Kelly ROI | `ppp_bet_slip_*.csv` |
| `06_early_ats.R` | early-season ATS bet slip (arm now off in feed) | `early_ats_bet_slip.csv` |
| `04_weekly_picks.R` | weekly UNDER recommendations (standalone) | `weekly_picks_*.csv` |
| `07_write_feed.R` | **production**: write picks/results JSON to dashboard feed (UNDER + Arm O `OPEN_ATS`) | `dashboard_feed/cfb-modeling/cfb/{picks,results}_<date>.json` |
| `08_write_slate.R` | full-slate model board for the Extras tab (every game, not just bets) | `dashboard_feed/cfb-modeling/cfb/board_<date>.json` |
| `09_capture_open_lines.R` | **capture & FREEZE early-week opening lines** (Odds API live endpoint, ~2 credits/call) for Arm O | `data_cache/opening_lines.csv` (append-only ledger) |
| `weekly_update.R` + `run_weekly.bat` | hands-off driver (refresh → rebuild → detect week → 09 backstop → 07 → 08) | `weekly_update.log` |
| `run_all.R` | full pipeline orchestrator (`--fast` skips heavy rebuild) | — |
| `verify_pipeline.R` | leakage/sanity asserts (run after any ratings change) | — |

**Legacy (benchmark only, do not build on):** `complete_cfb_betting_model.R` (57KB old
XGBoost cover/over classifier), `*.bin`/`*.model` xgb artifacts, `final_models.rds`,
`ultimate_model_complete.rds`, `bankroll_log_2025.csv` (unreproducible). The XGBoost approach
has **no edge** — it relearns the line.

---

## 4. Methodology

### 4.1 Possessions & PPD (`01`)
A **possession = one offensive drive.** Points per drive = `net_pts` (negative when the
defense/ST scores). Only meaningful drives count (kickoffs / end-of-half removed).
**Garbage-time filter is score+period based, defined before features and never tuned to
results:** H2 margin >28 or Q4 margin >21. (Do *not* use win-probability for garbage
detection — favorites sit >0.95 WP from kickoff and it deletes whole games.) Score
reconciliation verified: gross offensive drive points ≈ actual team points (median diff 0).

### 4.2 Opponent-adjusted as-of ratings (`02`)
Iterative empirical-Bayes (KenPom-style), **8 iterations**:
```
observed_off(i vs j, home) = adj_off_i + (adj_def_j − LG) + hfa·home + resid
```
- **Metrics adjusted** (offense & defense each): `ppd`, `epa`, `pass_epa`, `rush_epa`,
  `td_rate` (explosiveness), `rz_ppd` (finishing). Plus an additive **pace** model
  (`total_drives = pace_i + pace_j`).
- **Shrinkage** `K=5` games toward a preseason prior = regressed prior-season carryover
  (`REG=0.5`) blended with **prior-year SP+** (`SP_WEIGHT=0.35`, env `PPP_SP_WEIGHT`). Prior
  dominates early; data takes over by ~week 5.
- **Leak-free:** for each `(season, week)` only games with `week < w` are used (asserted in
  code). **SP+ is prior-year only** (see the leakage lesson, §8).
- **Preseason seed** (cold-start): after the main loop, `02` emits weeks 1–4 rows for the
  *next unplayed* season so openers are modelable before any games exist; superseded once
  real games arrive. (A latent bug where this collapsed coverage once new pbp published was
  fixed by filling all carryover teams every week.)

### 4.3 Projection — "Approach A" (won the bake-off)
```
exp_drives  = (pace_home + pace_away) / 2
home_ppd    = adj_off_ppd_home + adj_def_ppd_away − LG + hfa_ppd
away_ppd    = adj_off_ppd_away + adj_def_ppd_home − LG − hfa_ppd
proj_margin = (home_ppd − away_ppd) · exp_drives
proj_total  = (home_ppd + away_ppd) · exp_drives
```
Then a **scale calibration** `lm(actual ~ proj)` fit as-of on completed history (the raw PPD
projection is over-extreme; RMSE is worse than naive without it). Probabilities via a normal
residual model: `P(under) = Φ((proj_total − OU)/σ_t)`, σ from calibration residuals;
`P(cover)` analogous with σ_m.

### 4.4 Model bake-off (`03`)
- **A** = pure opponent-adjusted PPD projection → **winner.**
- **B** = XGBoost regression on PPP matchup features (market-blind) → no edge.
- **C** = XGBoost market-aware → no edge (relearns the line).
- **Benchmark** = old cover/over classifier → no edge.
The edge lives in the projection's **directional disagreements on totals**, not in point
accuracy — A's totals RMSE ≈ the market's and does **not** beat the market on RMSE.

### 4.5 Backtest mechanics
- **Walk-forward**, 2021–2025, FBS-vs-FBS, deterministic (`set.seed(42)`).
- **Push handling (critical):** a total landing exactly on the line is a **push**, not a win.
  An earlier bug (`over_hit` counting `total==line` as an UNDER win) inflated win rates;
  fixed in `05_bet_slip.R` and `verify_pipeline.R` (pushes excluded). This dropped 2021 to
  below breakeven and is why the canonical number is 55.9%, not the stale 58.5%.
- Main results file: `ppp_backtest_results.csv` (3,730 games, 2021–2025; columns include
  `spread`, `over_under`, `actual_margin`, `total_points`, `home_covered`, `over_hit`,
  `proj_margin_A`, `proj_total_A`, and A/B/C probabilities).

### 4.6 The evaluation bar — forecast encompassing (READ THIS)
**A feature is only credited if it beats the *line*, not if it correlates with outcomes.**
The test is:
```
lm(actual_target ~ market_line + candidate_feature)
```
If the candidate's coefficient is insignificant, the market **encompasses** it — the feature
carries no information the closing line lacks, regardless of how well it predicts outcomes on
its own. **RMSE improvement is not sufficient** (the market beats us on RMSE and still gets
encompassed). Every feature below was judged by this bar. This single principle explains
~90% of the rejections.

### 4.7 Statistical discipline
- **As-of everything:** every feature uses only `week < w` data; asserted in
  `verify_pipeline.R`. A same-season SP+ leak once faked a 58.5% edge (§8).
- **Multiplicity:** ~27 hypotheses tested → Benjamini-Hochberg adjustment. UNDER≥4 (raw
  p=0.071) fails BH; higher-volume UNDER≥3 survives. Any new survivor must be BH-adjusted
  against the running count. Every test — including rejections — is logged in
  `FEATURE_REGISTRY.md`.
- **Block bootstrap** by season-week for robustness CIs (UNDER≥4 95% CI [50.7%, 61.2%] —
  includes breakeven).
- **De-bias / matched thresholds, base-rate netting, calibration/reliability** — used to
  confirm the UNDER asymmetry is real selection, not just a projection bias artifact.
- **Cross-fit** discovery/calibration/threshold/report on disjoint data; require monotonic
  conviction and correct sign across most folds + a mechanism.

---

## 5. The two live betting signals

### 5.1 UNDER totals (deployed, executable)
The model over-projects totals every season (LOO bias ≈ +0.75 pts). Deployed rule
(`07_write_feed.R`, constants `UNDER_EDGE=4, DOG_EDGE=3, ODDS=−110`):
- Bet UNDER when `(book_total − proj_total) ≥ 4` (`UNDER4`), **OR** `≥3` **AND** the model
  likes the market underdog ATS (`UNDER3_DOG`); EV>0 at −110.

| Rule (push-excluded) | Bets | Win% | ROI | Seasons ≥ breakeven |
|---|---|---|---|---|
| **UNDER ≥4 (deployed)** | 454 | **55.9%** | +6.8% | 4/5 (2021 = 51.4%) |
| UNDER ≥3 + dog-ATS | 379 | 56.7% | +8.3% | 4/5 |
| OVER ≥3 (contrast) | 1,244 | 51.3% | −2.1% | 1/5 |

**Honest qualifications:**
- **It is NOT projection skill** — `proj_total` is encompassed (p=0.65). The edge is a
  **large-disagreement / directional / post-2023-regime tail effect.**
- **De-biased edge ≈ 54%** (raw 55.9% partly rides the persistent over-projection).
- **Regime/decay risk:** heavily post-2023 (61.6% 2023–25 vs 52.0% 2021–22); likely a
  **clock-rule transition inefficiency** (2023 running-clock rules cut pace; the model's
  carryover pace lags → over-projects → unders hit). Strongest in weeks 1–2 (stalest pace
  prior). **Expect decay** as pace stabilizes. "Fixing" the pace lag would remove the bias
  *and* the edge.
- **OT tax:** unders win only 30.5% in games that reach OT (~4% of games) vs 51.2%
  otherwise — but OT is not pregame-predictable (see B3), so it's an irreducible drag.
- **`model_prob` is over-confident** (calibration slope 0.13) — the *displayed* probability
  is not calibrated; the *selection* (points edge) is fine. Fix het-σ for display honesty.

### 5.2 Backup-QB ATS fade (validated, NOT yet executable)
**Fade a team forced to start a backup QB.** Retrospective test (2021–2024, one-sided
backup-start games, push-excluded):

| Definition | n | Backup team cover% | Fade% |
|---|---|---|---|
| Season-modal QB threw 0 passes | 603 | 44.8% | 55.2% |
| As-of (prior-weeks modal QB) | 564 | 45.2% | 54.8% |
| **As-of, backup = true unknown (0 prior FBS att)** | 153 | **40.5%** | **59.5%** |

**This is the availability family** — three independent confirmations of one mechanism:

| Signal | Result |
|---|---|
| QB starter-out | fade 55% (as-of confirmed, not a look-ahead artifact) |
| Top-receiver-out (0 catches/targets) | fade 56.8% (n=236), all 4 seasons <50% |
| **Unknown backup (monotonic in experience)** | cover% by prior att 0/1-50/51-200/200+ = 40.5/42.6/49.0/50.0 |

**Mechanism:** the market applies a *generic* availability downgrade that is **too small when
it cannot calibrate the replacement** (an unknown backup, a missing WR). It prices the
*reverse* correctly — a star QB's telegraphed **return** covers exactly 50.0% (A3). It also
prices the *magnitude* of a known downgrade correctly (the per-QB ELO gradient is
non-monotonic/insignificant) — **only the discrete, hard-to-value event is mispriced.**

**Why it's not executable yet:** we can only detect a backup start *after the game* (0 snaps
in the box score). To bet it we must know *pregame* that the starter is out — a
**depth-chart / injury / availability feed** we don't ingest. **This is the highest-value
infrastructure build** (conference availability reports + a depth-chart source, joined by
team+week). It unblocks all three availability edges at once.

### 5.3 `proj_vs_open` — beating the opening spread (DEPLOYED, PAPER, live from wk2 2026)
Every encompassing test in this project historically ran against the **closing** line — the
hardest benchmark and *not the number you have to bet*. Testing against the **open** flips the
result: the margin projection is encompassed by the close but **carries information beyond the
open**.

**Evidence (two independent datasets):**
- Cached `spread_open` (same-provider open+close): proj vs open coef 0.075, p=0.038; the line
  moves *toward* the model (cor +0.149). Within Bovada (the one provider spanning both eras) the
  effect is **post-2023 only** (2023–25 coef 0.128, p=0.007; 2021–22 encompassed/losing) — so it's
  a regime effect, not a book artifact.
- **Clean Odds API DK opens (2023–25, timestamped):** proj vs open coef **0.138, p=0.004**,
  CI [0.045, 0.232]. Betting the disagreement at the open covers **~53–54%** (mag≥3, n=1325);
  per season **50.6 → 53.0 → 56.1%** (2023→25) — **not decaying.**

**Mechanism — a time-to-kickoff continuum, not a binary.** The number is soft at the open (edge
largest), sharpens as sharp money arrives, and by the **true close** (cfbfastR) the model is fully
encompassed (p=0.57). Open→true-close drift is ~0.32 pts toward the model. (NB: Odds API snapshots
captured ~30–90 min pre-kickoff are *not* true closes — the model still beats those, p=0.001 — so
"close" quality depends on capture time.) This is the **ATS expression of the UNDER regime effect**:
the market's *opening* numbers lag the post-2023 pace regime, and our weekly-rebuilt ratings are
ahead of them. Same post-2023 dependence, same decay risk.

**Built (2026-09-08):** `09_capture_open_lines.R` pulls live spreads+totals from The Odds API
(NCAAF, DK/FanDuel/Caesars, **~2 credits/call** — the cheap live endpoint, NOT the 10-credit
historical one), matches events to CFBD `game_id` by team-name prefix + kickoff date, and
**appends only never-before-seen (game_id, book) rows** to `data_cache/opening_lines.csv` — a
FROZEN, append-only ledger that mirrors the dashboard's own "freeze posted terms" philosophy: a
later run can never overwrite an already-captured opening line. `07_write_feed.R` reads this
ledger (DraftKings preferred, FanDuel fallback — Caesars is captured but never used as a
bettable price, matching `BOOKS_ALLOWED` elsewhere) and emits Arm O (`OPEN_ATS` tag) bets **only
for games with a frozen ledger row** — no ledger entry means no bet; it never silently falls
back to reading the current/live spread, which would quietly re-introduce the closing-line
number the edge doesn't exist against.

**Scheduling (and its one open gap):** two dedicated tasks, `CFB-Open-Capture-Sun` (Sun 20:00)
and `CFB-Open-Capture-Mon` (Mon 06:00 — both before the main 08:00 weekly rebuild), aim to
freeze the number as early in the week as possible (the edge decays toward kickoff). **They
currently run as `InteractiveToken`, not `S4U`** — assigning S4U to a *new* task hit a genuine
Windows "Access is denied" wall this session couldn't clear non-interactively (confirmed not an
XML-encoding issue; retried with byte-exact UTF-16LE+BOM matching the working `CFB-PPP-Weekly`
task's own export, still denied). `weekly_update.R` (confirmed S4U) also calls
`09_capture_open_lines.R` as a backstop every Mon/Thu, so a week is only meaningfully degraded
(not fully missed) if the user is logged out at both early windows. **To finish the upgrade to
full hands-off:** from an **elevated** PowerShell, run
`schtasks /create /tn "CFB-Open-Capture" /xml "C:\Users\ljdie\OneDrive\Documents\cfb-modeling\cfb_open_capture_task.xml" /f`
— that XML is already correctly formatted (verified byte-for-byte against a known-working S4U
task) and should succeed under admin rights where the non-elevated session was denied; once
confirmed, delete the two `InteractiveToken` fallback tasks (`CFB-Open-Capture-Sun`/`-Mon`).

**Confirmed live** (first capture, week 2 2026): 85 events pulled, 185 (game, book) rows
matched and frozen, 5 games cleared the `mag≥3` threshold and were emitted as paper bets —
validated end-to-end through `pnpm ingest:dry` with zero contract rejections.

**Cheap follow-ups a new DS should run:** (1) once the S4U gap is closed, measure realized
capture timing vs the backtest's Monday-snapshot proxy (a tighter Sunday capture may widen the
edge further per the recency-lag mechanism, §1.1 of the reviewer's v3 doc); (2) the
identifiability gate on this signal to sharpen deciles; (3) track forward CLV weekly — it is the
deciding metric, not win rate (see `FORWARD_TEST_2026.md` Arm O).

---

## 6. Complete feature / hypothesis test log

The authoritative, append-only version with exact test statistics is `FEATURE_REGISTRY.md`.
Summary here. **Bar = beats the line (encompassing), as-of, with a mechanism.**

### 6.1 Deployed (survived, with caveats)
| id | market | rule | result |
|---|---|---|---|
| UNDER4 | total | proj_total ≥4 below line | 55.9% / 454 bets, tail effect |
| UNDER3_DOG | total | edge≥3 + model likes dog ATS | 57.6% / 99 marginal bets |
| early_ats | spread | more-aggressive team wk1–4 (gap≥0.10) | 57.4% but flat CLV + fails BH → **removed from feed**, backtest-research only |

### 6.2 The availability family (new information — the only live edge channel)
| id | market | result |
|---|---|---|
| qb_starter_out | spread | backup team covers 44.8%/45.2% as-of → fade ~55% |
| A4_unknown_backup | spread | **CONFIRMED monotonic** — unknown backup fade 59.5% |
| A1_wr_out | both | **SIGNAL** — WR-out team covers 43.2% (fade 56.8%), all seasons <50%, UNDER 52.6%; p=0.043, decaying 62→53% |
| qb_elo / qb_elo_gradient | both | per-QB ELO **encompassed** (level p=0.39, surprise p=0.38); no magnitude gradient (market prices the *size* of a known downgrade) |

### 6.3 Rejected / closed (efficiency & projection — all encompassed)
| id | market | result |
|---|---|---|
| encompass_margin | spread | proj_margin coef 0.011, **p=0.76** → ATS closed by projection |
| encompass_total | total | proj_total coef −0.035, **p=0.65** → UNDER edge is not projection skill |
| A_prime | spread | component matchups (pass/rush/expl/epa/fin) all **p>0.35** (net_pass p=0.84) |
| elo_offdef | both | split off/def team ELO **encompassed** (margin p=0.77, total p=0.35); real rating (cor .58) but worse than market |
| elo_momentum | both | rolling 1/2/4/6-game ELO deltas (form/trend) all **p>0.32**; \|cor with ATS cover\|≤0.016 → market not slow on form |
| ats_H1_slow | spread | market-slow mechanism dead: `line_move ~ agg_diff` p=0.46, CLV flat t=0.19 |
| tempo_imposition | total | pace beyond OU p=0.94 |
| entropy_under / entropy_ats | both | reversed / 48.9% |
| 4thdown_general | spread | p=0.56 wk5+ (priced outside opener) |
| playcall_2ndshort | spread | p>0.5 |
| OVER_edge | total | 51.3%, 1/5 seasons |
| B_xgb / C_xgb | both | ML on PPP/market features — no edge |
| totals_range_filter | total | degraded 2024 — overfit |
| ats_recency | spread | recency ≈ career (56.9 vs 57.4) — no gain |

### 6.4 The pre-registered Category A/B slate (tested this session)
Registered before results to keep multiplicity honest.
| id | cat | result |
|---|---|---|
| **A4_unknown_backup** | A | ✅ CONFIRMED monotonic (fade 59.5%) |
| **A1_wr_out** | A | 🟡 SIGNAL (fade 56.8%, decaying) |
| A3_qb_return | A | ❌ returning team covers 50.0% (n=320) — market prices the return |
| A2_kicker_fgoe | A | ❌ game FGOE encompassed p=0.55; bad-kicker UNDER 52.5%≈52.7% |
| B1_variance_tail | B | ❌ **reversed** — low-variance games go UNDER only 44.5% |
| B2_clock_backdoor | B | ❌ **reversed** — favorites cover *less* post-2023 (fav≥17 Δ−4.5) |
| B3_ot_trap | B | ❌ premise false — OT rate flat ~4%, not pregame-predictable |
| B4_altitude_travel | B | ⚠️ inconclusive — `elevation` too sparse (max Δ 2180ft); travel encompassed (p=0.74) |

### 6.5 Diagnostics (count toward the test budget)
RMSE-by-tier (no soft corner: best G5-G5 gap 0.90 still worse than market), β-implied ATS
(observed matches β-implied at gap 0.10, luck-inflated at 0.20), de-bias matched thresholds,
2023 regime split, recency confound refuted (agg_diff survives tenure/size/P5 controls
β=+12.4 p=0.003).

**Running test count ≈ 35.**

---

## 7. Interactions tested
- UNDER edge × era (pre/post-2023 regime — real, large).
- UNDER edge × market-total tercile (naïve "bet high totals" nearly matches model unders).
- UNDER edge × scoring variance (post-hoc, unconfirmed: better in high-variance, n=119 —
  needs its own pre-registration, not deployed).
- Backup fade × backup experience (**A4 — monotonic, confirmed**).
- Backup fade × per-QB ELO magnitude (no gradient).
- WR-out × target concentration (non-monotonic — binary event matters, not the gradient).
- 4th-down aggression × week (specificity: wk1–4 p=0.001 vs wk5+ p=0.56).
- Kicker FGOE × close-spread × low-total (n=30, noise).
- Altitude × visitor pace, travel × body-clock (data-limited).

**Recurring theme:** the market prices *magnitudes and gradients* well; it misprices only
discrete, hard-to-value binary events (a key player unexpectedly out). Interactions that
worked were all of the form "the binary event, conditioned on how hard it is to value."

---

## 8. Key findings & lessons

1. **The encompassing wall — real at the CLOSE, cracks at the OPEN.** The *closing* line prices
   all team efficiency (both markets, all components, ELO, momentum, per-QB rating). Do **not**
   propose more ratings/opponent-adjustment/momentum features to beat the *close* — dead on arrival
   (§4.6). BUT the same projection beats the *opening* line post-2023 (`proj_vs_open`, §5.3) — the
   wall is about the close, not about ATS in general. Also: the encompassing test is underpowered
   by collinearity — mark borderline results *inconclusive*, report the CI, confirm with a direct
   selection test (that's how `proj_vs_open` was found).
2. **The leakage lesson.** A 58.5% "edge" was mostly a data leak: the preseason prior blended
   *same-season* SP+, and `cfbd_ratings_sp(year=Y)` returns *end-of-season* ratings. With
   `K=5` keeping the prior at ~30% weight all season, the lookahead contaminated every week.
   Fixed to prior-year SP+ (collapsed to breakeven; confirmed with `SP_WEIGHT=0`). **Any new
   feature must be as-of and beat-the-market validated.** The tell was the *pattern* (strong
   early seasons, weak most-recent — the recent year benefits least from a prior that knows
   the future).
3. **Push handling.** Landed totals are pushes, not wins — miscounting inflated win rates.
4. **The availability theme is the only whitespace** (§5.2) — three confirmations, one
   mechanism, one blocker (pregame availability feed).
5. **Regime effects are real and decaying** — the UNDER edge is a 2023 clock-rule artifact;
   watch the pre/post split live.
6. **The `--reconcile` hazard** (§9) — never run it against this feed.

---

## 9. Infrastructure — dashboard feed & automation

- **Feed:** `dashboard_feed/cfb-modeling/cfb/picks_<date>.json` + `results_<date>.json`, one
  file per game day, contract 1.0 (schema in `dashboard_feed/_schema/`,
  `additionalProperties:false`). Fields: `book`, `model_prob`, `market_prob`, `edge`,
  `ev_pct`, `stake_units`, `tags`, `details`; results carry `closing_odds_american` +
  `clv_pct`. `mode:"PAPER"`.
- **Post/grade split:** picks are written only for **not-yet-started** games; results are
  graded from the **previously-posted** picks file (grade the number actually bet). No
  backfill.
- **Ingest (in the separate `bet-dashboard` repo, `C:\dev\bet-dashboard`):** pnpm monorepo,
  Next.js 15 + Supabase Postgres + Drizzle. `pnpm ingest` upserts feed JSON with **frozen
  posted terms** (a re-emitted pick keeps its original line). Scheduled `Bet-Ingest` task runs
  plain `pnpm ingest` every 15 min.
- **⚠️ NEVER run `pnpm ingest --reconcile` on this feed.** Reconcile deletes ungraded DB bets
  per `(source,sport,slate_date)` not in the *current* file — but `07` re-emits only
  currently-qualifying picks, so it deletes anything that dropped out (old ATS, moved-line
  UNDERs, shrunk golf boards). Plain upsert's freeze logic is what correctly preserves posted
  paper bets. Recovery from an accidental reconcile: plain `pnpm ingest` re-adds everything
  still in the feed files.
- **Do NOT touch hosting** (Supabase/Vercel/env vars) — owned by the `bet-dashboard` repo.
  This model's only job is writing feed JSON.
- **CFB automation:** `CFB-PPP-Weekly` scheduled task (Mon/Thu, confirmed S4U so it runs
  logged-out) → `run_weekly.bat` → `weekly_update.R` → `09` (capture backstop) → `07` → `08`.
  Hardened: idle gate (off-season fast exit), rebuild only on new completed games, heavy steps
  isolated as subprocesses with **sequential future plan** (`future::plan("sequential")` +
  `R_FUTURE_PLAN=sequential` + `setDTthreads(1)` — prevents orphaned R workers), always reaches
  `07`. **⚠️ Found DISABLED on 2026-09-08** (`schtasks /query` showed `Enabled: false`, last
  successful run 2026-08-27 — over a week of missed automated runs; everything since was this
  session's manual runs) — **re-enabled** via `schtasks /change /tn "CFB-PPP-Weekly" /enable`.
  If picks ever look stale again, check this first: `schtasks /query /tn "CFB-PPP-Weekly" /fo LIST`.
- **Opening-line capture:** `CFB-Open-Capture-Sun` (Sun 20:00) + `CFB-Open-Capture-Mon`
  (Mon 06:00) → `run_open_capture.bat` → `09_capture_open_lines.R`. **Currently `InteractiveToken`**
  (needs the user logged in) — see §5.3 for why (a Windows S4U-registration permission wall) and
  the exact elevated command to upgrade them. `weekly_update.R`'s Mon/Thu S4U run is the backstop.
- **Registering a NEW scheduled task with S4U logon from a non-elevated context reliably fails
  with "Access is denied" on this machine**, even with byte-exact XML matching a task that
  already has S4U live. *Modifying* an existing S4U task's other properties still works fine
  (e.g. `/enable` above). Budget for needing one elevated PowerShell command per NEW hands-off
  task going forward — this is now a known, recurring constraint, not a one-off.

---

## 10. Known limitations
- **In-sample edges** — 2026 is the real forward test (`FORWARD_TEST_2026.md`, rules frozen).
  Win rate is underpowered at ~267 UNDER + ~65 ATS bets/season; **CLV is the primary metric.**
- **DraftKings-only** lines; −110 assumed; no line-shopping, no live juice.
- **No availability / injury / depth-chart data** — the blocker for the QB/WR edges.
- **No weather, no referee-crew data** in cache (would need external feeds).
- **`team_info.elevation` unreliable** — altitude features untestable until fixed.
- **Machine-local** — the PC must be on for the scheduled tasks.

---

## 11. Open work / roadmap (prioritized)

**Target architecture** (evolve toward): `context/player-adjusted distribution [market-blind]`
+ `fixed-time market-residual model [what the line still misses]` + `exact line/price/PUSH
handling` → cross-fitted monotonic EV → bet. Replaces `ratings → mean → point disagreement →
threshold`.

1. **Pregame availability feed (highest value).** Unblocks the QB backup fade + WR-out +
   unknown-backup edges (all confirmed retrospectively). Sources: conference availability
   reports (Big Ten/Big 12/ACC/SEC), a depth-chart provider; join by team+week. Convert the
   0-snaps proxy into a pregame flag.
2. **Line-shopping / microstructure (Category C — deferred by user, but cached data is
   ready).** `betting_lines.rds` already has multi-book + partial openers + moneylines.
   Testable offline now: cross-book dispersion as a confidence gate for the UNDER; opener-vs-
   close CLV for the UNDER; DK-vs-consensus tail. Use the Odds API only to fill open/close
   gaps (sparingly — token-limited).
3. **Phase 0 data ledger (time-critical, irreplaceable).** Timestamped DK/FanDuel lines at
   fixed snapshots (T−36h primary), availability-report vintages, weather forecasts with
   issuance timestamps, model inputs+outputs for ALL games (incl. non-bets, for calibration &
   selection-bias protection). Every week not captured is lost forever.
4. **Display/calibration only (no edge):** het-σ to fix the over-confident `model_prob`
   (slope 0.13).
5. **Totals-only, could beat encompassing:** `weather × style` (wind non-linear) — needs an
   external weather feed.

**Do NOT prioritize** (encompassed / rejected): more efficiency ratings, ELO/momentum
variants, XGBoost on cover/over, threshold tuning on the current projection,
motivation/rivalry flags, generic team-quality features.

---

## 12. Reproducibility & environment
- **R 4.3.3** at `C:\Program Files\R\R-4.3.3\bin\Rscript.exe` (not on PATH). Packages:
  `dplyr, tidyr, xgboost, jsonlite, glmnet, lme4, cfbfastR`.
- **Run:**
  ```powershell
  $rs = "C:\Program Files\R\R-4.3.3\bin\Rscript.exe"
  cd C:\Users\ljdie\OneDrive\Documents\cfb-modeling
  & $rs run_all.R                 # full rebuild (touches 519MB pbp; ~6-8 min)
  & $rs run_all.R --fast          # reuse caches: backtest + slips + picks
  & $rs verify_pipeline.R         # leakage + sanity asserts
  $env:PPP_SEASON=2026; $env:PPP_WEEK=2; & $rs 07_write_feed.R   # one week's feed
  ```
- **Gotchas when scripting R from PowerShell:** write `.R` files as **UTF-8 without BOM**
  (a BOM breaks Rscript); avoid the token `rm(` in R source (the sandbox flags it as
  Remove-Item); the pbp cache is 519MB — cache heavy aggregations to a small `.rds` first,
  then analyze, to stay under command timeouts; watch for full-width `），` sneaking in via
  autocorrect.
- Everything is deterministic (`set.seed(42)`) and regenerates from cache. Ignore
  `bankroll_log_2025.csv` (unreproducible legacy).

---

## 13. First-week orientation for the new DS
1. Read this doc, then `FEATURE_REGISTRY.md` (the full test ledger) and `ROADMAP.md`.
2. Run `run_all.R --fast` + `verify_pipeline.R` to reproduce the backtest.
3. Internalize §4.6 (encompassing) — it's the filter that keeps the project honest.
4. The live research frontier is the **availability family** (§5.2). The highest-leverage
   build is the **pregame availability feed** (§11.1). The cheapest untapped data is the
   **multi-book lines already in cache** (§11.2).
5. Log every new hypothesis in `FEATURE_REGISTRY.md` *before* seeing its result.
