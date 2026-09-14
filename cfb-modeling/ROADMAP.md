# CFB Model — Profitability & Monotonic-Edge Roadmap (working copy)

Condensed from the user's *Profitability and Monotonic-Edge Roadmap*. This tracks the target
architecture, the phased work order, and **what is already done vs open**. Companion to
`HANDOFF.md` (findings), `FEATURE_REGISTRY.md` (test log), `FORWARD_TEST_2026.md`,
`ATS_GAP.md` (why ATS is closed).

**Current live state (2026-09):** feed emits **UNDER totals only**; **ATS removed** from the feed
(`07_write_feed.R` `EMIT_ATS` gate = off) until a genuine ATS edge is established. Execution
window: within ~48h of kickoff, DraftKings; FanDuel = market control, not line-shopping.

---

## Target architecture (evolve toward this)

```
context/player-adjusted football DISTRIBUTION (margin, total, possessions)   [Layer A, market-blind]
        + fixed-decision-time MARKET-RESIDUAL model (what the line still misses) [Layer B]
        + exact line/price/PUSH/uncertainty handling                            [Layer C]
        → cross-fitted, monotonic expected-value score → bet
```
Replaces the current `ratings → mean score → point disagreement → threshold bet`.

Three highest-priority *new* builds (per the roadmap): (1) market-residual distributional/quantile
betting layer; (2) backdoor-cover / favorite-closeout behavioral layer; (3) availability-weighted
lineup value. **None credited until Phase 1 (below) is complete.**

---

## Phase 1 — settle what exists  ✅ DONE (this is the bulk of the recent reviews)

| Roadmap item | Status | Result |
|---|---|---|
| Reconcile 58.5% vs 56.6% | ✅ | doc error (leaky both-sides); canonical **UNDER≥4 = 55.9%** push-corrected |
| De-bias totals, matched thresholds | ✅ | asymmetry real (UNDER 54% > OVER 51% de-biased), but modest |
| Unconditional UNDER base rate + pre/post-2023 | ✅ | base ~50.7%; **pre-2023 52.1% (breakeven), post-2023 61.5%** — regime effect |
| Forecast-encompassing (ATS + totals) | ✅ | **proj_margin p=0.76, proj_total p=0.65 — BOTH fully encompassed.** ATS closed; totals edge is a tail effect, not projection skill |
| A-prime component encompassing | ✅ | components also encompassed (all p>0.35) — **no projection upgrade can create ATS edge** |
| RMSE by liquidity segment | ✅ | no soft corner (G5-G5 gap 0.90, still worse than market) |
| 4th-down line-movement + β-implied | ✅ | H1 (market-slow) dead (p=0.46); deploy β-implied ~55% not 57–63% |

**Stop-gate outcome:** totals retain *some* incremental selection post-2023 (tail/regime), so the
UNDER rule stays in paper — but the market fully encompasses the ATS projection **in every
segment**, so **ATS is not optimized on the current projection** and is removed from the feed.

---

## Phase 0 — ⚠️ URGENT, NOT YET DONE: preserve irreplaceable 2026 pregame data

Historical *results* can be reconstructed; **pregame information vintages cannot.** Start now:
1. Timestamped **DK + FanDuel ledger** (spread/total/juice, retrieval time) at a fixed decision
   snapshot (roadmap recommends **T−36h primary**; also store T−48/24/6h + close).
2. Archive **official availability-report vintages** (Big Ten/Big 12/ACC/SEC now publish these).
3. Archive **weather forecasts with issuance timestamps**.
4. Store **model inputs + outputs for ALL games incl. non-bets** (needed for calibration &
   selection-bias protection).
> This is the single most time-critical build — every week not captured is lost forever.

---

## Open work order (Phases 2–5), re-prioritized by the encompassing findings

Because the market encompasses our projection on **both** markets, **projection-quality features
cannot create edge** — only *new information* or *tail/regime* effects can. Reordered accordingly:

- **Next real-edge candidate — QB backup-quality mispricing** (roadmap §7.2/§5, Phase 4): per-QB
  opponent-adjusted EPA from cached pbp, **retrospective test on games where the starter's snaps=0**
  (no injury feed needed) — does the market's blunt QB-out adjustment misprice weak backups at thin
  programs? The one hypothesis where the market plausibly lacks info. **Start with the player-ID
  diagnostic** (roadmap §15): `names(pbp)[grepl("player|passer|rusher|receiver|athlete|id", …)]`.
- **Backdoor-cover / favorite-closeout layer** (§7.1): the one genuinely-new *ATS* family — models
  the late-game points quality ratings intentionally remove but ATS grading keeps. Coach/program
  state-behavior + expected-state-exposure interaction.
- **Betting-layer replacement** (Phase 2): fixed-time market-residual dataset, discrete/quantile
  outcome model with **push probability** (not `pnorm((proj-line)/const_σ)`), heteroskedastic σ,
  cross-fitted calibration + **monotonic edge-bin report**.
- **Totals-only, could beat encompassing:** `weather × style` (wind non-linear) — the market may
  under-price wind on totals; test with the encompassing bar on `total_points`.
- **Display/calibration only (no edge):** het-σ fixes the over-confident `model_prob` (slope 0.13).

**Deprioritized (RMSE-only, market already encompasses):** A-prime (rejected), per-metric K,
fitted prior, mixed model, ridge combination, venue HFA, fixed-drives-margin, neutral-script pace.
Do only if a *specific* one is shown to encompass-beat the market (none has). `turnover_luck` is the
least unlikely, since it changes *what* the rating measures.

---

## Admission gate (every new feature) & discipline

Point-in-time + asserted · beats the line (encompassing, not RMSE) · improves Brier/log-loss ·
correct sign in most folds · monotonic conviction · survives registered multiplicity (BH) ·
CLV at the actionable timestamp if the mechanism predicts line movement; **results, not CLV,** if
the mechanism is a persistent closing-line error. Cross-fit (discover/calibrate/threshold/report on
disjoint data). Log every test — incl. rejections — in `FEATURE_REGISTRY.md`.

## Do NOT prioritize
Generic team-efficiency columns; XGBoost on cover/over; more threshold searches on the current
projection; motivation/rivalry/letdown flags; betting every disagreement; selecting bets on
eventual closing movement; weather backtests with *realized* (not forecast) conditions; treating
final participation as known pregame; promoting on RMSE alone; publishing in-sample rates as
calibrated probabilities.

## Key references
CFBD players/usage/returning/portal + CORE context-adjusted ratings; NCAA 2023 timing rule;
conference availability policies; Dmochowski (optimal betting), Sides & Harvill (spread→prob),
Coleman (metamodel; travel effects), McMahon & Quintanar (off/def HFA), Salaga & Howley (weather
totals). Full URLs in the source roadmap doc.
