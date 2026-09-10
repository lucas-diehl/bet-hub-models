# DFS ENGINE — Model & System Handoff (for audit + upgrade)

**Audience:** an engineer/agent picking this up cold to **audit correctness** and **propose upgrades** across golf, tennis, WNBA, and the in-browser simulator.
**Last updated:** 2026-09-03.
**Golden rules (do not violate):** never build MLB / baseball / softball. DataGolf is the only *paid* feed — everything else must stay free / repeatable / backtestable. The DK session cookie (`config/dk_session.txt`) is a **secret** — never print, log, or commit it. Cash-game lineup builds are considered stable; be careful changing them. Validate every model change on a **walk-forward backtest AND actual contest results**, never on the field-sim's EV alone (see §10).

---

## 0. TL;DR system shape

- **Two codebases, one GitHub monorepo** (`lucas-diehl/bet-hub-models`, public):
  - `dfs-engine/` — the **spine** (sport-agnostic pipeline) + **sport plugins** (tennis, wnba, nfl, ncaaf) + the golf **adapter** that bridges to the golf engine. This repo = `C:\Users\ljdie\OneDrive\Documents\DFS ENGINE`.
  - `golf-modeling/` — the **golf v2 engine** (100% our own simulation-first model). Sibling dir `C:\Users\ljdie\OneDrive\Documents\golf-modeling`.
- **Storage:** local **DuckDB** at `dfs-engine/data/dfs.duckdb` (`spine/R/db.R`). Model artifacts are `.rds` files (golf: `golf-modeling/golf_picks/*.rds`; dfs: `dfs-engine/data/models/*.rds`).
- **Runtime:** R (Rscript 4.3.3 locally, **4.4 on CI**). Entry point for a full daily build: `jobs/run_all.R`.
- **Outputs (“feeds”):** JSON + one self-contained HTML simulator, written to `dashboard_feed/dfs-engine/{tennis,wnba,golf,pools,simulator}` and `dashboard_feed/golf-modeling/pga`, then ingested to Supabase and served by the bet site.
- **Automation:** GitHub Actions (`dfs.yml`, `golf.yml`, `train.yml`, `nfl.yml`, `cfb.yml`). Local Windows Task Scheduler tasks exist but are **Disabled** (superseded by Actions).

> **Read §12 (Known issues) first if you are auditing** — several things that look wired are not actually running in production.

---

## 1. Repository layout & key files

### 1.1 Spine (`dfs-engine/spine/R/`) — sport-agnostic
| File | Role |
|---|---|
| `plugin.R` | Sport plugin registry (`register_sport`, `get_sport`, `dfs_load_sport`). |
| `pipeline.R` | **`run_slate()`** — the core per-slate pipeline (project → sim → field → candidates → grade → build). |
| `slates.R`, `slate_io.R`, `slate_sim.R` | Slate objects, salary I/O, Monte-Carlo slate simulation (`slate_sim`). |
| `validate.R` | `validate_projection()` — schema/​sanity gate on every projection pool. |
| `dk_scrape.R`, `dk_contest.R`, `dk_auth.R` | DraftKings lobby/draftables scraping, contest metadata, session auth. |
| `fd_scrape.R` | FanDuel CSV-drop ingest (`fd_ingest_latest`, `fd_ingest_csv`). |
| `injuries.R` | `apply_inactives()` — drops OUT / discounts questionable. ESPN injury feed + **manual scratches** (`config/manual_scratches.json`) for tennis/golf (no auto feed). |
| `vegas.R` | Vegas lines/totals feed (WNBA team environment). |
| `ownership_model.R` | **`train_ownership_model(sport)`** — trained projected-ownership model per sport (tennis/wnba/nfl). |
| `ownership.R` | Ownership persistence + actual-ownership logging/join. |
| `field_sim.R` | **Opponent field simulation** — models the contest field as optimizers of noised projections + ownership pull; validated dupe ≈ 0.034 vs a real 63k-entry DK contest. |
| `lineup_eval.R` | `grade_candidates()` — GPP EV, `p_top1`, `p_cash`, `avg_own`, `dupe_idx` per candidate lineup. |
| `optimizer.R` | ILP/greedy lineup candidate generation (`make_candidates`). |
| `portfolio.R` | `build_portfolio()` (role-based cash/balanced/leverage) and **`build_gpp20()`** (the 20-entry format) + per-player exposure caps + `load_exposure_overrides()`. |
| `showdown.R` | **Showdown/Captain Mode** — `expand_showdown_pool` (1 CPT@1.5× + 5 FLEX), `captain_board`, `run_contest`. |
| `correlation.R` | Correlation/stacking loadings (`get_loadings`, `get_team_loadings`). |
| `payouts.R`, `staking.R`, `dk_contest.R` | Payout curves (`make_gpp`, `make_double_up`, `payout_multipliers`), bankroll/staking, contest caps. |
| `contest_select.R` | Score/rank live DK contests by rake/field/overlay (the “which contest to enter” layer). |
| `dashboard.R` | **Dashboard builder** — `build_dashboard`, `build_sport_card`, `build_contest_card`, `build_golf_captain_card`, `build_sim_payload`, `publish_simulator`, `dash_contest_options`, `dash_cand_metrics_hi`. |
| `diagnostics.R` | `loss_autopsy()` — attributes a losing result to projection vs ownership vs chalk-duplication vs rake vs variance. |
| `proj_accuracy.R`, `results.R`, `report.R`, `io_export.R` | Projection-accuracy scorecard, P&L ledger, reporting, feed export. |

### 1.2 Sport plugins (`dfs-engine/sports/<sport>/`)
- **`golf/adapter.R`** — bridges the spine to the golf engine; reads `golf-modeling/golf_picks/dfs_projections*.rds`; handles DK single-round + captain detection (`golf_dk_round_group`, `golf_live_round`, `golf_dk_captain_group`, `golf_captain_base`, `.golf_round_model_pool`). FanDuel via `_fd` files. `golf/presim.R` helper.
- **`tennis/`** — `model.R` (`tennis_train`, feature build, XGBoost/GLM projection), `project.R` (`tennis_project_players`, surface handling, baseline fallback), `ingest.R` (ATP/WTA match history), `roster.R`, `correlate.R`.
- **`wnba/`** — `model.R` (`wnba_train`), `project.R` (minutes/usage/pace projection, injuries, Vegas), `ingest.R` (box scores), `team_env.R`, `roster.R`, `correlate.R` (game stacks).
- **`nfl/`** — `model.R`, `project.R` (site-aware, external-source layering), `bestball.R`, etc. (In development; live for the 2026 season.)
- `nba/`, `ncaaf/` — placeholders.

### 1.3 Golf engine (`golf-modeling/engine/`) — the deepest model
| File | Role |
|---|---|
| `data.R` | **`build_master()`** → `golf_picks/v2_master.rds` (the training table: ~48 features per player-round, SG categories, DK/FD scoring identity, finish, made-cut). Also `build_round_shapes()`. Standalone: `Rscript engine/data.R`. |
| `skill.R` | **`train_v2()` / skill μ model** — linear model on SG target (OOS skill cor ≈ 0.328). |
| `coursefit.R` | Course-fit residual model (player × course history). |
| `market.R` | Market/odds blend (`market_w`). |
| `elo.R` | **Personalized multiplayer golf Elo** (leakage-free) → `golf_picks/v2_elo.rds`. Standalone: `Rscript engine/elo.R`. |
| `weather.R` | Per-tee-time wind (round-specific `WIND_K_ROUND`), heat/rain adjustments. |
| `simulate.R` | **SG → DK/FD points Monte-Carlo sim** (cut, finish, tail/skew). Site param for FD scoring. |
| `round_sim.R` | **Single-round** projection (`export_round_projection`, per-tee-time wind, FRL prob). |
| `project.R` | Orchestrator: `project_live`, `project_event` (assemble slate → μ → coursefit → market → sim). |
| `ownership.R` | Golf ownership model → `golf_picks/v2_own_model.rds` (DataGolf-anchored; golf own cor ≈ 0.81). |
| `export.R` | **`export_v2` / `run_export_v2`** → writes `dfs_projections*.rds` in the adapter schema (default / `_m80` / `_opp` / `_fd`). `load_bundle()` retrains the bundle if > 7 days old. |
| `backtest.R` | **Walk-forward backtest** — per-slate rank cor + contest ROI (the gate for shipping any change). |
| `bet_hub.R` | H2H matchup + top-10 picks feed for the bet site (also grading). |
| `matchups.R`, `bet_top20.R`, `eval_outrights.R` | Betting-market models (only top-10-favorites + matchups passed walk-forward; win/top-5/top-20/make-cut failed). |
| `emit_elo.R` (root) | **Snapshots** current Elo → `dashboard_feed/golf-modeling/pga/elo_<date>.json`. Does **not** compute Elo (reads `v2_elo.rds`). |

### 1.4 Jobs (`dfs-engine/jobs/`)
`run_all.R` (daily: everything), `refresh_dfs.R` (scoped tennis/wnba refresh + simulator republish), `round_dfs.R` (golf single-round feed), `dfs_values.R` (top-10 value feed), `train_models.R` (**per-sport** retrain: `--sport wnba|tennis|nfl` → projection **and** ownership), `log_projections.R`, `sync_ownership.R`, `import_ownership.R`, `pnl.R`, `accuracy.R`, `my_contests.R`, `contest_select.R`, `diagnose.R`, `bestball.R`, `golf_sim.R`.

### 1.5 Config (`dfs-engine/config/`)
`bankroll.yml`, `contests.yml`, `exposure_overrides.json` (per-player max exposure for the 20-entry build), `manual_scratches.json` (manual OUT list for tennis/golf), `dk_session.txt`/`dk_user.txt` (**secrets**).

---

## 2. Data sources & ingestion

| Sport | Primary data | Ingest path | Freshness |
|---|---|---|---|
| **Golf** | **DataGolf API** (paid; skill ratings, field, historical rounds/SG, odds), DK lobby/draftables (salaries) | `golf-modeling/engine/data.R` (history → master), `project.R` (live slate), `sports/golf/adapter.R` (DK salaries) | history rebuilt only when `data.R` runs; live slate every projection |
| **Tennis** | ATP/WTA match history (free), DK draftables | `sports/tennis/ingest.R`, `model.R`; DK via `spine/R/dk_scrape.R` | features live each slate; model weights only via `train_models.R` |
| **WNBA** | Box scores (free), ESPN injuries, Vegas lines/totals | `sports/wnba/ingest.R`, `injuries.R`, `vegas.R` | features live each slate; model weights only via `train_models.R` |
| **Ownership (actual)** | DK contest standings CSVs (session cookie) | `jobs/sync_ownership.R` → `data/ownership_inbox/` → `import_ownership.R` → `ownership.actual_pct` | as contests settle |

**Live inputs refresh every run** (DataGolf ratings, today's field, DK salaries, projected ownership). **Learned weights do not** unless a training job runs (see §12).

---

## 3. The core pipeline — `run_slate()` (spine/R/pipeline.R)

For one (sport, date, slate, site):
1. `dfs_load_sport(sport)` → plugin's `project_players(slate)` builds the **pool** (`player_name, salary, proj, sim_sd, ceil, floor, own, position, team, …`).
2. `validate_projection()` — schema/sanity gate.
3. `apply_inactives()` — drop OUT, discount questionable (ESPN feed + manual scratches).
4. `persist_projected_ownership()`.
5. `slate_sim()` — N-sim Monte-Carlo of player fantasy scores using correlation loadings.
6. `simulate_field()` — the opponent field (N optimizers of noised projections + ownership pull).
7. `make_candidates()` — candidate lineup pool (ILP/greedy under roster + salary rules).
8. `grade_candidates()` — GPP EV / p_top1 / cash / dupe per candidate vs the field.
9. **Build**: `build_gpp20()` for the 20-entry format (default on dashboard cards) or `build_portfolio()` (roles).
10. Persist projections + recommended lineups to DuckDB; return payload for the dashboard/simulator.

`run_contest(contest_id)` is the Showdown/Captain analog (spine/R/showdown.R): expands pool to CPT+FLEX, sims on base players, grades with group-uniqueness, emits a **captain board**.

---

## 4. Golf model (the flagship — golf-modeling v2)

**Philosophy:** 100% ours, simulation-first. Beats DK-salary rank OOS (~0.373 vs 0.352 per-slate cor) and beats the v1 blend in backtest.

**Chain:** `data.R` builds the master (per player-round SG categories + ~48 features + DK/FD scoring identity) → `skill.R` fits μ (expected SG, linear; OOS cor ≈ 0.328) → `coursefit.R` adds course-fit residual → `market.R` blends odds → `elo.R` supplies a leakage-free personalized Elo feature → `simulate.R` Monte-Carlos SG→points with proper tail/skew (cut + finish) → `export.R` writes `dfs_projections*.rds` in the adapter schema (`default`, `_m80` = 80% model/20% DataGolf, `_opp` = opposite field, `_fd` = FanDuel-scored). `load_bundle()` caches/retrains the trained bundle (`v2_bundle.rds`).

**Variants / tabs:** main PGA, opposite-field (`_opp`), 80%-model (`_m80`), FanDuel (`_fd`).
**Single-round** (`round_sim.R`): auto-detected DK round R1–R4 (`golf_live_round`), per-tee-time wind, FRL probability; **R4 blends finishing/placement points** (70% round + 30% finish). Feed: `round_values_<date>.json`.
**Captain Showdown** (`build_golf_captain_card`): detects DK GameTypeId 153 (`golf_dk_captain_group`), joins captain base salaries to the round model, runs the showdown pipeline. Exposure caps count a golfer across CPT+FLEX (`base_id`).
**Betting markets** (out of DFS scope but shared engine): only **top-10-favorites** and **matchups** pass walk-forward; win/top-5/top-20/make-cut fail; FRL untestable (no odds history).

---

## 5. Tennis model (sports/tennis)

- `tennis_train(refresh=TRUE)` pulls ATP/WTA history, builds features (recent form, surface, matchup, serve/return), fits the projection model → `data/models/tennis_models.rds`.
- `tennis_project_players()` maps the live DK slate to features and projects; **players it can't match fall back to a DK season-avg baseline** (`.tennis_baseline`). Surfaces detected per slate (Hard/Clay/Grass).
- No auto-injury feed → withdrawals handled via `config/manual_scratches.json` (see the Cilic WD workflow).
- **Ownership:** trained model, corr improved 0.16 → 0.58 vs heuristic on held-out slates.

**Upgrade recommendations (vetted, 2026-09-03 review) — HIGH value:**
1. **Replace features→points regression with a point-level Markov simulation** from serve/return hold probabilities. DK tennis scoring is dominated by games won, aces, breaks, and set bonuses — all of which fall out naturally from a point sim, and you get the **full distribution** instead of mean + assumed sd. Crucially it gives the **within-match correlation for free**: the two players are strongly *negatively* correlated (one gets the win bonus) and their game counts are *positively* correlated (long matches inflate both). This is the single biggest tennis upgrade.
2. **Model retirement/withdrawal probability explicitly.** It's a few % of matches, partially predictable from recent retirements, heat, and consecutive-day load, and essentially unpriced by the field — a real edge.
3. **Close the injury/WD production hole** (currently manual scratches only): a scheduled scrape of the **ATP/WTA order of play + live scoreboard** for WDs. This is a concrete near-term build, not a research project.

## 6. WNBA model (sports/wnba)

- `wnba_train(refresh=TRUE)` on box-score history → `data/models/wnba_models.rds`. Projects minutes/usage/efficiency → fantasy points.
- Live adjustments: **injuries** (`apply_inactives` — drop OUT, minutes-bump teammates), **Vegas** lines/totals (`team_env.R`, blowout/pace), **game-stack correlation** (`correlate.R`).
- **Ownership:** trained model (`train_ownership_model("wnba")`).

**Upgrade recommendations (vetted, 2026-09-03 review) — HIGH value:**
1. **Minutes are the whole game — build a dedicated minutes model that emits a DISTRIBUTION**, conditioned on the Vegas spread (blowout risk truncates starter minutes), rest/back-to-back, and roster depth. Currently minutes are projected but not as a first-class distribution.
2. **"Next-woman-up" depth-chart model:** rosters are only 11–12 deep, so an injury redistributes minutes+usage highly predictably. Reallocate the injured player's minutes/usage using historical **lineup-combination** data. This barely exists in public projections and is high value.

---

## 7. Ownership models (all sports)

- **Tennis/WNBA/NFL:** `spine/R/ownership_model.R::train_ownership_model(sport)` — regresses projected ownership on salary/value/proj/etc.; trained from **logged actual ownership** (DK standings imports). Falls back to a heuristic until ≥ 5 slates logged. Retrained **inline every dashboard build** and via `train_models.R`.
- **Golf:** `golf-modeling/engine/ownership.R::train_ownership_model(master)` → `v2_own_model.rds` (DataGolf-anchored, ~90/10). Golf ownership cor ≈ 0.81.
- Ownership is the **leverage foundation** — the field sim and every leverage number depend on it. **Audit priority: validate tennis/WNBA ownership calibration** the way golf is validated.

**Upgrade recommendations (vetted, 2026-09-03 review) — this is where the −0.35 likely hides:**
1. **Golf ownership cor 0.81 is a floor, not an edge.** At ~90/10 DataGolf-anchored you're mostly *reselling DataGolf's number* — fine as a floor, but not proprietary and it's a paid dependency. Build a genuinely independent ownership signal.
2. **Stop treating ownership as a point estimate. Leverage comes from ownership ERROR, not level.** Feed a **distribution over ownership** into `field_sim.R` instead of a point value. The field sim almost certainly **understates the variance in how the field constructs**, which mis-estimates both dupe and top-1% probability — a plausible root cause of the field-sim EV cor ≈ −0.35 (§10).
3. **Version the ownership model.** *(Maintainer note: the review's premise that `train_ownership_model` retrains on every build is **not** the case — `predict_ownership` already loads the pinned `own_model_<sport>.rds`. The valid part stands:)* pin + **version-stamp** the ownership artifact so every projection row is traceable, and confirm it's trained on a schedule (`train.yml`) rather than ad-hoc. The real serve-path retrain to fix is `load_bundle` (§17).
4. **Easy win — DK salary residual:** model DK salary from public inputs and use the **residual**. A player cheap relative to *public perception* drives both ownership and value; the residual is a clean feature for both models.

---

## 8. The simulator (in-browser, `spine/assets/dashboard.html`)

- A **self-contained HTML/JS app** baked by `dashboard.R`. Read once as a template (`build_sim_payload` injects a compact Monte-Carlo payload), published to `dashboard_feed/dfs-engine/simulator/dashboard.html` behind a password, auto-updated 7×/day.
- **Payload:** the slate sim is down-sampled to K columns; the field is summarized per sim as a **quantile grid** (`SIM_QLEVELS`, denser at the top to resolve top-1%/win%). A few hundred KB.
- **Client math mirrors `grade_candidates()` exactly:** lineup total = colSums(draws[picks]); pct-beaten = interpolate into the per-sim field grid; cash% = P(total ≥ per-sim field median); top1% = P(pct ≥ .99); EV via the chosen contest's payout multipliers.
- **Optimize panel** (`OPT_CONTESTS` in the HTML): contest-type toggles — **GPP (large-field)**, **20-entry GPP** (your format, backtest-tuned: concentrate + light self-diversify, don't over-fade chalk), **Cash/Double-up**, **Single-entry GPP** — each with objective + ROI-uncertainty / duplication / overlap knobs.
- **Contest dropdown:** `dash_contest_options()` — live DK contests + robust presets, each carrying a payout-multiplier vector at the sim's field resolution so the browser can re-grade EV/ROI with no re-run.
- **High-fidelity selection:** `dash_cand_metrics_hi()` grades the browser's candidate lineups on a full 50k-sim draw so the Optimize/Top-ROI *selection* rests on a stable basis (the live manual scorer uses the light down-sampled draws to keep the HTML small).

**Audit note:** the simulator's contest-ROI/EV is a *construction* aid, not ground truth — see §10.

**Upgrade (vetted):** add an **automated simulator/engine parity test** — a checked-in **golden slate payload**, run `grade_candidates()` in R and the JS math on it, assert agreement within tolerance in CI. The §13 "confirm the JS mirrors `grade_candidates()`" manual step will silently rot; make it a test.

---

## 9. Lineup construction & the 20-entry format

- **`build_gpp20(res, gates, n=20, exp_cap=0.5, pool, caps)`** — proj-ranked greedy fill with a per-player exposure cap. Backtest finding: for a fully-entered 20-lineup set, **proj-ranking + ~0.5 exposure reaches the highest actual ceiling**; ceiling/leverage weighting *hurt* (−2.3%, chases noise). Per-player caps via `config/exposure_overrides.json` (e.g., an injured player capped at 0.40). For showdown pools, exposure is keyed on the underlying golfer (`base_id`) so CPT+FLEX count together.
- **`build_portfolio()`** — role-based (cash/balanced/leverage) with exposure + pairwise-overlap caps (used for cash and multi-role sets).
- **Showdown/Captain** — `spine/R/showdown.R`; captain board ranks who to put in the 1.5× slot (ceiling + leverage).

---

## 10. Field simulation, grading & the EV-trust caveat (READ THIS)

- The field sim (`field_sim.R`) + `grade_candidates()` produce GPP EV, `p_top1`, dupe, cash%. **Duplication is validated** (~0.034 vs a real 63k DK contest).
- **BUT the field-sim's `gpp_ev` / `p_top1` do NOT predict actual GPP outcomes** (golf cor ≈ −0.35). A blanket contrarian/own-cap experiment **tanked** field-sim EV and was reverted; chalk studs are the ceiling.
- **Therefore:** validate any leverage/construction change on **realized dupe + ACTUAL contest results**, never on the sim's EV alone. Use `loss_autopsy()` (spine/R/diagnostics.R) to attribute results.
- Established leaks (from `loss_autopsy`): our lineups run **~+6–7% more owned than the field** (chalk-duplication), and the 150-max mega-GPPs the user plays are the **worst structure** (15–16% rake). Contest selection + format is a bigger lever than projection accuracy.

---

## 11. Feeds & the bet site

Written to `dashboard_feed/` then ingested to Supabase by the Actions `ingest` action:
- `dfs-engine/<sport>/values_<date>_<site>.json` — top-10 value/exposure names (the `/dfs` page).
- `dfs-engine/golf/round_values_<date>_<site>.json` — single-round (slate_type `single_round`, round N).
- `dfs-engine/simulator/dashboard.html` (+ `meta.json`) — the password-gated simulator.
- `dfs-engine/pools/pool_*.json` — optimizer pools.
- `golf-modeling/pga/elo_<date>.json` — Elo snapshot; also H2H picks, DataGolf-ownership A/B.

---

## 12. Automation & KNOWN ISSUES (audit starts here)

**GitHub Actions** (`.github/workflows/` in the monorepo):
- `dfs.yml` — 3×/day (12/16/20 UTC): `Rscript jobs/run_all.R --bankroll 300` (all sports) → publish feeds + simulator → Supabase. **Does not retrain projection models.**
- `golf.yml` — 09:00 + 18:00 ET + Mon/Tue grading: `engine/bet_hub.R` + `emit_elo.R`. **Does not run `data.R`/`elo.R`.**
- `train.yml` — **NEW (PR #1, pending merge)**: weekly (Tue) golf `data.R`→`elo.R`→bundle retrain + `train_models.R --sport {wnba,tennis,nfl}`, then **commits** the refreshed `.rds` back.
- `nfl.yml`, `cfb.yml` — football.

**Confirmed production gaps (as of this handoff):**
1. **Golf training data + Elo were frozen at 8/2** — `v2_master.rds`/`v2_elo.rds` are committed but never rebuilt; `emit_elo` only snapshots. New results (incl. FedExCup playoffs) were absent from training. → fixed by `train.yml` once merged + run.
2. **Tennis/WNBA/NFL trained models are gitignored** (`dfs-engine/.gitignore` has `data/models/`) → **not committed → absent on the runner → production projected on the season-avg BASELINE**, not the trained model. → `train.yml` force-commits them.
3. The bundle "retrains" weekly but on the frozen master (no new signal) until #1 is fixed.
4. Local Windows scheduled tasks are **Disabled** (expected — moved to Actions). Local `.rds` files are **stale copies**, not authoritative.

**Verify after `train.yml`'s first run:** golf Elo ratings actually change; a dfs run logs the tennis/WNBA **trained** model (not "baseline"); artifacts get committed.

---

## 13. How to validate any change (gates)

- **Golf:** `golf-modeling/engine/backtest.R` — walk-forward per-slate rank cor + cash win% + GPP ROI (2024–26). Ship only if it beats salary/v1 **OOS**.
- **Tennis/WNBA:** `jobs/train_models.R --sport X --backtest` (calls `<sport>_accuracy_backtest`) + `jobs/accuracy.R` (`projection_accuracy` scorecard vs actuals).
- **Field-beating / GPP:** realized dupe + `loss_autopsy` on entered contests over a **meaningful sample** (never one week — GPP variance dominates). Do **not** trust field-sim EV (§10).
- **North star:** actual contest ROI over many slates, reported with variance bands.

---

## 14. Suggested audit & upgrade priorities

> Items marked **[review]** come from the vetted 2026-09-03 external review (details in §5–§8, §17–§19). Roughly ordered by (impact × tractability).

**Correctness / plumbing first (do before model work):**
1. **Confirm the training loop actually closes** (merge/run `train.yml`; verify commits + downstream freshness). #1 correctness issue.
2. **[review] Separate training from serving** — `load_bundle()` (golf export) and `train_ownership_model` (dashboard build) retrain in the serve path → non-deterministic, slow, unversioned. Train only in `train.yml`; serve loads pinned/versioned artifacts (§17).
3. **[review] Add a test suite** — start with `validate_projection()` schema contract tests (would have caught the baseline-in-prod bug) + the simulator/engine **parity golden test** (§8, §17).
4. **[review] Persist raw slate inputs + version-stamp feeds** so any published number is reproducible and traceable (§18).

**Model upgrades (each gated by backtest + actual results):**
5. **[review] Tennis: point-level Markov sim** from serve/return hold probabilities — full distribution + free within-match correlation (win-bonus anti-correlation, game-count co-movement). Biggest tennis lever (§5).
6. **[review] WNBA: minutes distribution model + next-woman-up depth reallocation** (§6). Minutes are the whole game; injury redistribution is highly predictable on 11–12-deep rosters.
7. **[review] Ownership as a distribution into `field_sim.R`** (not a point estimate) — leverage = ownership *error*; likely root cause of the field-sim EV cor −0.35 (§7, §10). Plus a DK-salary-residual feature and an independent (non-DataGolf) ownership signal.
8. **Tennis/WNBA ownership calibration** — validate per-slate cor like golf (0.81); currently least-validated.
9. **Golf skill μ** — linear core (OOS cor 0.328); integrate the created-feature backlog (`spike_index`, `xSG`/`putt_luck`, `toughness_sg`, FedEx-playoff motivation, SG-trend) and test a GBM/ensemble μ on the SG target (gated by `backtest.R`).
10. **[review] Tennis retirement/WD model + OOP scrape** — unpriced edge + closes the manual-scratch production hole (§5).
11. **Sim tails** — top-finish probabilities + DK ceilings drive GPPs; verify `simulate.R` skew/tail vs realized (`sg_dist_model`).
12. **Contest selection + multi-entry differentiation** — biggest ROI lever per §10; `contest_select.R` built but under-wired.
13. ~~WNBA game-stack correlation — modeled vs realized~~ **DONE (2026-09)**: measured realized same-game correlation in real box scores (`tests/validate_wnba_correlation.R`, 2023-2026, 1,076 games) — opponent pairs ~0.016, same-team ~0.036 (high-minute-robust). The prior assumption (rho 0.20-0.45) overstated same-game co-movement by roughly an order of magnitude. Recalibrated `sports/wnba/correlate.R` to match (now also populates the previously-unused `team_load` second factor for the small same-team premium). **Re-run the validation script periodically** as `wnba_player_box.rds` grows — this was a point-in-time calibration, not a permanent constant.

## 15. From the field: external review (Alex Blickle thread, 2026-09) — sim correlation & field realism

[Blickle's thread](https://x.com/AlexBlickle1/status/2096644199198097515) argues a contest sim's reported ROI is only as trustworthy as two hard things: (1) **game realism** — not just marginal noise + a pairwise correlation coefficient, but *scenario-conditioned* inputs (his example: a discrete "20% chance the defense sells out to stop the run" regime that jointly shifts several players' inputs together — this is "hidden" correlation a simple factor model can't express), and (2) **field realism** — ownership percentages alone don't reproduce a real field; you need the *joint* structure of how people build lineups (stack rates: single/double/mini-stack, which pairs get stacked). Get either wrong and, his framing: "the ROI you see tells you more about the flaws in the sim than the quality of your lineup." He also describes **projection/ownership sensitivity** (slice sim iterations by which input-regime a player's assumptions were drawn from, report how the result moves) and **stack dependence/lift** (measured co-movement for specific stack types, calibrated against real data).

This is not a new topic for us — it's a sharper diagnosis of the exact failure already named in §10 (field-sim EV cor ≈ −0.35 vs actual outcomes). Status:

- **Done, cheap, real** (item 13 above): validated `correlation.R`'s assumed same-game rho against real WNBA box scores and recalibrated — a direct, evidenced fix using data we already had, no new architecture.
- **Idea #1 (scenario-conditioned correlation) — DONE (2026-09) for WNBA.** A full synthetic play-by-play scenario-injection sim (Blickle's own approach) needs play-by-play data we don't ingest. Built the tractable, evidenced version instead:
  1. **Found the evidence** (`tests/validate_game_script_correlation.R`): realized WNBA correlation **sign-flips** by actual game script.
     | | close game (≤8) | blowout (≥18) |
     |---|---|---|
     | opponent pairs | +0.041 | **−0.068** |
     | same-team pairs | −0.001 | **+0.076** |
     A single flat factor cannot represent a sign flip — concrete proof the "hidden, script-dependent correlation" Blickle describes is real for us.
  2. **Built the architecture** to actually represent it: `slate_sim.R` now accepts a `regime_loadings` object (`list(base, alt, p_alt)`, S3-tagged) from `get_loadings()`/`get_team_loadings()`. When present, it draws a **per-game, per-SIMULATION** Bernoulli regime (weighted by `p_alt`) and blends `load`/`team_load` per sim column — so different simulated draws of the same slate land in different correlation regimes, not one averaged constant. Plain numeric-vector loadings (every sport except WNBA) are **completely unaffected** — verified via a full regression pass (`tests/test_scoring_roster.R`, 37/37 passing) plus a dedicated backward-compat test confirming NFL's `get_loadings()` still returns a plain vector.
  3. **Calibrated WNBA's two regimes** (`sports/wnba/correlate.R`) directly to the measured targets (opponent close/blowout hit almost exactly in a 80k-sim isolation test; same-team-close is capped at the flat close-game value since the additive team-factor model can't go *below* the opponent-level correlation, and the measured −0.001 there is statistically indistinguishable from noise anyway — stated as an explicit, honest limitation rather than fabricated precision). `p_alt` (pre-game P(blowout)) uses a closed-form P(|Margin|≥18) with Margin ~ Normal(exp_margin, σ), **σ=14.13 measured directly from 2,268 real WNBA game margins** — not assumed. `exp_margin` (pregame expected |margin|, Vegas spread when available) now flows through `team_env.R` → `project.R` → the pool.
  4. **Verified end-to-end** on a real cached slate (`run_slate("wnba", date="2026-08-30", ...)`): runs clean, `exp_margin` present with a sensible range, lineups built normally.

  **Extended to NCAAF (2026-09), same session.** Turned out NCAAF was *more* ready than initially scoped — the cached raw CFBD JSON from the CFB DFS build (`data/raw/ncaaf_src/*.json`) already has team-level `points` sitting unused, so no new data pull was needed (`cfb_game_scores()` in `ingest.R` extracts it). 1,759 real games (2024-2025) — more than WNBA had. Measured the same phenomenon (`tests/validate_ncaaf_game_script_correlation.R`):
  | | close (≤7 margin) | blowout (≥28) |
  |---|---|---|
  | opponent pairs | +0.060 | **−0.082** |
  | same-team pairs | +0.036 | +0.059 |

  Same opponent-correlation sign flip. **Unlike WNBA**, same-team correlation sits *below* the opponent-level magnitude in both CFB regimes (real data doesn't support a positive same-team increment here) — so `sports/ncaaf/correlate.R` uses `team_load=0` in both regimes, a simpler model than WNBA's, chosen because that's what this sport's data actually supports (not copy-pasted from WNBA). σ=23.98 (much wider than WNBA's 14.13 — real college-football blowout culture, e.g. Alabama 63-0 in the sample). Verified: isolated regime targets hit within tolerance, `get_loadings("ncaaf")` returns `regime_loadings`, and a real production `run_slate("ncaaf", ...)` on a cached live slate (280 players) runs clean end-to-end.

  **One honest gap, deferred on purpose rather than shipped fragile:** live `exp_margin` (pregame P(blowout) input) currently uses a neutral empirical-mean default (~18.8), not real Vegas spreads. Real CFB spreads are available via the exact same `vegas_games()` infra WNBA uses, but joining them needs a DK-abbreviation ↔ ESPN/CFBD-full-school-name alias map (~130 FBS teams) that doesn't exist yet — a fuzzy join without a verified alias table risks silent mismatches, so it's flagged as a TODO in `project.R` rather than shipped unverified. The *correlation architecture itself* (the main ask) works correctly regardless; only the sharpness of the per-game P(blowout) estimate is affected.

  **Extended to NFL (2026-09), same session, per direct user request.** Pulled nflverse's free schedule/results dataset (`sports/nfl/ingest.R::nfl_game_scores()` — separate release from player stats, has real final scores + historical closing spreads, 100% coverage). 1,359 real games (2021-2025). Measured (`tests/validate_nfl_game_script_correlation.R`):
  | | close (≤6 margin) | blowout (≥21) |
  |---|---|---|
  | opponent pairs | +0.038 | +0.009 |
  | same-team pairs | +0.023 | **+0.075** |

  **Genuinely different pattern from WNBA/NCAAF: opponent correlation does NOT flip sign** — it shrinks toward independence instead. Real, sport-specific reason: NFL's well-known garbage-time dynamic (trailing teams pass more to catch up, inflating *both* sides' skill-position stats) — unlike basketball/CFB where a blown-out team's rotation just gets pulled, deflating them alone. NFL already had a validated, tested correlation model (`sports/nfl/correlate.R`'s QB-stack-positive/RB-decouple-negative team factor) — **left completely untouched**; only the GAME factor was made regime-conditional (shrinks by the measured ratio in blowouts, no sign flip), using the "mixed" case the architecture was designed to support (game factor regime-conditional, team factor stays a plain vector — confirmed with a dedicated test). Full existing NFL stacking tests (37/37) still pass unchanged.

  **Also disclosed, not fixed (out of today's scope):** the existing NFL `game_ld` values imply an opponent correlation of 0.12–0.25 — the *real* measured close-game value is ~0.038. Same overstated-assumption pattern found and fixed for WNBA. Left untouched since recalibrating the base stacking model is a separate, bigger task (would need position×script splits this aggregate sample can't support) from what was asked today.

  **exp_margin (live serving):** reuses NFL's existing, already-working `game_id`-matched Vegas join (the same one that already powers `game_total_z`) to also pull the real closing spread — more direct and reliable than NCAAF's deferred team-name join. **Caveat, stated honestly:** verified the code path and the safe fallback (empirical mean) work correctly on a live 2026-09-07 slate, but `vegas_games("nfl", ...)` returned empty for that date — a pre-existing gap (the *existing* `game_total_z` feature was already silently getting no data for this slate too, unrelated to this change) — so the *real-spread* path itself could not be observed firing end-to-end today, only the fallback. Worth checking why NFL Vegas lines aren't cached for current slates as a separate follow-up.

  **What this still does NOT cover:** golf/tennis (no team concept, don't need it). NCAAF's live exp_margin still uses the neutral default (see above — needs a team-name alias map). NFL's own base stacking calibration (see disclosure above).

- **Idea #2 (field lineup stack-pattern calibration) — real method built, real signal found, NOT yet wired into `field_sim.R`.** Found genuine per-entry lineup text (not just ownership %) sitting unused in `data/ownership_inbox/processed/*.csv` (real DK contest-standings exports). Built a co-occurrence **LIFT** metric (`tests/explore_field_stack_lift.R`): P(QB + skill-player both in an entry) / (P(QB) × P(skill)) — needs zero team-roster mapping, since real teammates simply co-occur far above chance. On 2 real CFB slates (4,178 entries): found genuine stacked pairs at 1.3×–5.2× the base rate. **Caveats, stated plainly:** only 2 slates / 1 sport so far (not enough to safely calibrate production field construction), and the derived stack-size breakdown (83% no-stack / 14% single / 2% double in this sample) is sensitive to an arbitrary `lift ≥ 3` "presumed teammate" threshold — treat as a first-pass estimate. Next step: extend to more slates, and ideally get real NFL/WNBA lineup exports (our more mature sports) before touching `field_sim.R`'s construction logic.

- Stack dependence/lift **reporting on our own sim draws** remains explicitly *not* worth building yet — it would just re-derive our own factor-model assumption circularly. It only becomes valuable once (1) and (2) above are actually wired into the simulator.

---

## 15. Environment & commands

- **R:** local `C:\Program Files\R\R-4.3.3\bin\Rscript.exe`; CI R 4.4, `use-public-rspm`.
- **Packages:** data.table, jsonlite, httr2, xgboost, openxlsx, DBI, duckdb, nnet.
- **Secrets (CI):** `DATAGOLF_API_KEY`, `THE_ODDS_API_KEY`, `DATABASE_URL`. Local: `config/dk_session.txt` (DK cookie — secret).
- **Common runs:**
  - Full daily: `Rscript jobs/run_all.R --bankroll 300 [--sports wnba,tennis,golf,...]`
  - Retrain a sport: `Rscript jobs/train_models.R --sport tennis`
  - Golf master/Elo rebuild: `cd golf-modeling && Rscript engine/data.R && Rscript engine/elo.R`
  - Golf backtest: `cd golf-modeling && Rscript engine/backtest.R`
  - Build/publish dashboard: `build_dashboard(sports=c("wnba","tennis","golf","golf_round","golf_captain")); publish_simulator(f)`
- **Golf env flags:** `GOLF_MODEL_AUTORUN=0` (use cached model, skip retrain), `ENGINE_SOURCE_ONLY`/`EXPORT_SOURCE_ONLY` (source without running a script's main block).

---

## 16. Glossary

- **Slate** — a set of players priced for one contest window. **Pool** — projected players for a slate.
- **μ (mu)** — expected value (SG for golf; fantasy points elsewhere). **SG** — strokes gained (golf skill unit).
- **gpp20 / 20-entry** — the user's primary format: 20 max-entry lineups in a large-field GPP.
- **Showdown / Captain Mode** — single-game contest: 1 captain (1.5×) + 5 flex.
- **Field sim** — simulated opponent lineups used to estimate rank/dupe/EV.
- **Dupe** — how many field entries duplicate a lineup (splits the prize).
- **`_m80` / `_opp` / `_fd`** — golf projection variants: 80% model, opposite-field event, FanDuel scoring.
- **Bundle** — the trained golf model object (`v2_bundle.rds`).

---

## 17. Engineering & testing gaps (vetted, 2026-09-03 review)

These are correctness/reproducibility issues, not model quality:
1. **Separate training from serving.** *(Corrected 2026-09-03 after code check:)* the **ownership** serve path is already clean — `predict_ownership()` (`spine/R/ownership_model.R`; golf `engine/ownership.R`) **loads the pinned `.rds` and predicts**; `train_ownership_model` runs only in `jobs/train_models.R`. The genuine retrain-in-serve issue is **`load_bundle()`** (`golf-modeling/engine/export.R`): it retrains the golf **bundle** if the committed one is > 7 days old, *inside the export/serve path* → non-deterministic + slow feed runs. Fix: make serving **load-only** (never retrain) and let `train.yml` own the bundle retrain. **Sequencing:** land this together with / after the `train.yml` PR, else the bundle can go stale with nothing refreshing it. Also stamp a version hash into every projection row + feed regardless.
2. **There is no test suite.** At minimum add **schema contract tests** on `validate_projection()` inputs/outputs — that's exactly the class of bug that produced gap #2 (baseline-in-prod went undetected). 
3. **Add the simulator/engine parity golden test** (§8).
4. **Version stamping.** Stamp a model/version hash (and ownership-model hash) into every projection row and feed, so any published number is traceable to the exact artifact that produced it.

## 18. Reproduce a feed (runbook — the "how do I rebuild yesterday" gap)

To reproduce a given day's outputs deterministically:
1. **Pin artifacts:** check out the repo at the commit whose `train.yml` produced that week's `.rds` (golf `golf_picks/v2_*.rds`, dfs `data/models/*.rds`). Today these are committed (golf) or gitignored (dfs — see §12); after PR #1 both are committed and thus pinnable.
2. **Set `GOLF_MODEL_AUTORUN=0`** so golf uses the cached bundle (no retrain).
3. **Rebuild:** `Rscript jobs/run_all.R --bankroll 300 --sports wnba,tennis,golf,golf_round,golf_captain` (or `build_dashboard(...)` + `publish_simulator(f)`), then `jobs/dfs_values.R` / `jobs/round_dfs.R` for the JSON feeds.
4. **Caveats to determinism:** live inputs (DataGolf ratings, DK salaries, injuries) are fetched at run time, so an exact byte-reproduction of a *past* day requires cached raw snapshots. Recommended upgrade: **persist the raw slate inputs** (DataGolf response, DK draftables, injury feed) per slate so a feed is fully reproducible from stored inputs + pinned artifacts. Seeds are fixed (`seed = 2026L` in `run_slate`/sims), so given identical inputs + artifacts the pipeline is deterministic.

## 19. Data dictionary — golf master (`v2_master.rds`) & μ model

`golf-modeling/engine/data.R::build_master()` writes `list(master, feats)`. The master is one row per **player-round** with the DK/FD scoring identity and ~48 columns. Key groups:

**μ-model features** (`skill.R::MU_FEATS`, the linear skill core; target = `t_sg` = that round's strokes-gained):
| Feature | Meaning |
|---|---|
| `sg_ott_24`, `sg_app_24`, `sg_arg_24`, `sg_putt_24` | Trailing-24-round strokes-gained by category (off-the-tee, approach, around-green, putting) |
| `career_sg` | Career strokes-gained baseline |
| `sg_last_50` | Strokes-gained over the last 50 rounds (medium-term form) |
| `pform_12` | Recent-form window (~12) |
| `elo_pre` | Pre-event **personalized Elo** (leakage-free; from `elo.R`) |
| **Interactions** | `sg_ott_24 × c_len_z` (bombers × course length, +.010 OOS), `elo_c × fasg_z` (Elo × field strength, +.0065) |

**Other master columns (by group):**
- **Targets/labels:** `t_sg` (round SG target), `sg_total/ott/app/arg/putt` (realized), `finish`, `made_cut`, DK/FD points (scoring identity in `data.R` + `simulate.R`).
- **Course fit:** `pc_prior_sg`, `pc_prior_n`, `pc_hist`, `course_fit`, course attributes (`coursefit.R`).
- **DataGolf enrich** (`enrich.R`): `dg_sg_ott/app/arg/putt/total`, `dgt_c` — DataGolf's regressed per-category true skill.
- **Field/context:** `field_avg_sg`, `c_len` (course length), centered vars `c_len_z`, `fasg_z`, `elo_c`.
- **Round shapes / weather:** per-tee-time wind + round-scoring shapes (`round_sim.R`, `weather.R`).
> **Action for the auditor:** there is no committed data dictionary in-repo — generate one programmatically from `attr(master,'feats')` + `names(master)` and check it in next to `data.R`. Enumerating all ~48 with provenance is a good first PR.

## 20. NFL: drop backup QBs (2026-09)

**Bug (real, user-reported):** a healthy benched backup QB (concrete case: Carson Wentz, MIN, depth-chart rank 3) isn't "injured," so `apply_inactives()`'s ESPN-injury-based safeguard can't catch him. He'd get a small nonzero salary-baseline projection (DK prices him very low, and the baseline formula scales ~linearly with salary), which a min-salary-hungry optimizer happily plugs in as "cheap value" despite his real expected output being ~0 — he's never actually going to play.

**Fix:** `sports/nfl/ingest.R::nfl_qb_starters()` pulls nflverse's free, no-key, ~daily-updated depth-chart dataset (`depth_charts_<year>.csv`), takes the latest snapshot, and returns the set of current QB1s (`pos_rank == 1`) per team. `sports/nfl/project.R` drops any pool row where `position == "QB"` and the player isn't in that set, right before the final column selection (after all projection sources — external model, built-in model, salary baseline — have already run, so it catches a backup regardless of which path produced his number). Fails safe: if the depth-chart pull is unavailable and there's no cache yet, the filter is skipped entirely rather than blocking the build or wrongly dropping every QB.

**Scope, deliberately narrow:** QB only, per the request — it's a clean 1-starter-per-team position, unlike RB/WR/TE where committee backfields make "backup" fuzzy and often still fantasy-relevant. Applying the same depth-chart signal to other positions would need a different bar (e.g. "gets meaningful snap share" rather than "is rank 1"), not attempted here.

**Verified:** live pull correctly excludes Wentz; a real slate (`run_slate("nfl", date="2026-09-07", ...)`) resolved to exactly 32 QBs, one per team, all recognizable current starters. Regression test: `tests/test_nfl_qb_starters.R`.

## 21. NFL: drop players not on an active roster — a DIFFERENT blind spot than §20 (2026-09)

**Bug (real, user-reported):** Ricky Pearsall (SF) was appearing in lineups despite being on **season-ending IR**. Root cause is *not* the same as §20's — `apply_inactives()`'s ESPN injury feed is a "status for THIS WEEK's game" report; a player placed on long-term IR weeks ago can fall off it entirely once he's no longer "in question" for the current week's game. Confirmed live: `injury_report("nfl")` (800 real rows) had **no entry for Pearsall at all**, yet DK's salary feed still listed him.

**Fix:** `sports/nfl/ingest.R::nfl_inactive_roster()` pulls nflverse's free, no-key, weekly **roster status** dataset (`roster_weekly_<year>.csv` — a *roster-eligibility* signal, independent of the injury-news cycle) and flags anyone with status `RES` (reserve/IR/PUP/NFI), `CUT`, `RET`, `EXE`, or `DEV` (practice squad). Applied in `project.R` across **all positions** (broader than §20's QB-only fix — this is about roster eligibility, not depth-chart rank).

**A real bug caught and fixed during development, not just the headline case:** matching by name alone is unsafe — "DeVonta Smith" (PHI WR, active, a clear starter) and "Devonta Smith" (CAR practice-squad DB, different capitalization) share a normalized name. An early version of this fix wrongly excluded the *active* Eagles starter too, caught by manually checking the drop list rather than trusting the Pearsall fix alone. Fixed by keying on **(normalized name, team)**, with an explicit tie-break: an `ACT` row for the same identity always wins over a stale/ambiguous inactive one — wrongly excluding a real player is worse than the bug this closes.

**Verified:** Pearsall correctly dropped; DeVonta Smith correctly kept; 251 players dropped total on a real live slate, salary range $2,500–$5,100 (consistent with genuine inactives — no plausible starter caught). Regression test `tests/test_nfl_active_roster.R` asserts the DeVonta-Smith-collision case specifically, not just the Pearsall case, so this exact failure mode can't silently regress.

**Not yet extended:** WNBA/golf/tennis don't need this (no long-term-IR concept in the same way, or no roster/depth structure at all). NCAAF has the exact same theoretical exposure (a CFB player on season-ending injury could similarly fall off whatever injury signal exists) but CFBD has no equivalent free roster-status dataset — flagged as a gap for the CFB parity work, not solved here.

## 22. Slate selection: multiple simultaneous Showdowns were silently collapsed to one (2026-09)

**Bug (user-reported, real):** the Rams/49ers Showdown wasn't appearing on the site. Root cause: `spine/R/dk_scrape.R::dk_slate_options()` — the function that enumerates which slate "layouts" (Full/Alt/Showdown) get built for `MULTI_LAYOUT_SPORTS` (`nfl`, `ncaaf`) — took `single <- s[GameCount == 1]; pick <- single[1]`, i.e. **only the single EARLIEST-kickoff showdown, always**, even though `dk_find_slates()` already returns one row per DK draft group and DK commonly runs *several* simultaneous Showdown/Captain-Mode contests (one per marquee matchup — TNF, SNF, a featured Sunday game, etc). Every showdown but the soonest was silently discarded.

**Fix:** loop over all live single-game draft groups (capped at `max_showdown = 4`), verify each is a genuine 2-roster-slot Showdown, and give each its own `slate_tag`/`layout_label`.

**A second real bug caught during the fix, not just the headline case:** the same live NFL lobby was *also* running a "Snake Showdown" (a 4-player snake draft — a completely different mechanic) and an in-game "2nd Half" contest (starts mid-game, not a pre-game builder) for the *same* matchup. A naive "enumerate everything with `GameCount==1`" fix would have tried to build full DFS lineup pools for formats the roster/scoring logic doesn't support at all. Excluded by name (`snake|in-game|2nd half|1st half`) — the same pattern already used for golf's round detection.

**Verified live:** now correctly enumerates 3 simultaneous real Showdowns (SF vs LAR, TB @ CIN, NO @ DET) with unique tags, while excluding the Snake and 2nd-Half contests for the same games. Regression test `tests/test_dk_slate_options.R` (4/4 passing, exercises the real multi-showdown case).

**Tennis "short slate" investigated — a different, NOT-yet-fixed issue.** Tennis isn't in `MULTI_LAYOUT_SPORTS` at all (only `nfl`/`ncaaf` are), so it always builds a single slate via `dk_main_slate()` — that part turned out to be a red herring, since only one tennis slate was live and it *was* being correctly picked. The REAL failure: DK's live "3-Player Winner Takes All" contest is a genuinely different roster **format** — not "pick any 3 of 8," but **3 distinct price-tiered slots**, each drawing from the same player pool at a *different* salary per slot (confirmed live: Zverev priced $15,500 / $23,300 / $19,400 across the 3 slots). `TENNIS_ROSTER` hardcodes a flat 6-player roster with a $47k salary floor — with only 8 real players and a 6-slot flat roster, `field_sim.R`'s optimizer can never assemble a valid lineup (`stop("empty field")`), so the values feed silently writes 0 names. This is structurally like a 3-tier Captain-Mode variant (each slot needs its own pool-expansion + pricing, similar to `expand_showdown_pool()` generalized to 3 tiers) — a real, separate feature to build properly, not a quick patch. **Deliberately not built yet** — flagged as a decision point for the user (how often is this niche format actually played) rather than rushed.

---

*Questions the previous maintainer would answer: “is the training loop actually running in Actions?” (see §12 — it wasn't for golf/master and tennis/wnba models; `train.yml` PR #1 fixes it). “Can I trust the sim's GPP EV?” (no — §10). “What's the real edge target?” (contest selection + differentiation + ownership, not more stud-projection accuracy).*
