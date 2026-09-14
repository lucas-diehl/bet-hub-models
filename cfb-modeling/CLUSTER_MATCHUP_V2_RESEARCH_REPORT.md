# CFB ATS Cluster-Matchup Feature — Research Report (Tier 1 terminal)

**Dated 2026-09-05.** Executed against `CFB_CLUSTER_MATCHUP_FEATURE_RESEARCH_PLAN.md`. This
report covers **Tier 1 (Validate the discovery)** and **Phase 1 (quality vs. style)** in full,
per the plan's promotion gates and stop conditions. **Recommendation: terminate the research
program here — do not proceed to Tiers 2–6.** Rationale below.

No change to the live system: `EMIT_ATS` remains off, no `--reconcile`, no feed changes.

---

## 1. What "+0.44" precisely was

The prior session's headline number is the **Pearson correlation, across 25 offense×defense
cluster cells, between each cell's cover rate in a 2021–2023 training window and its cover rate
in a 2024–2025 test window** — a measure of whether the *rank pattern* of hot/cold matchup cells
repeats out of sample. It is not a regression coefficient, not OOF correlation with margin or
cover outcome, and not a profitability statistic. This distinction turns out to be the whole
story (§4).

- Method: k-means, k=5 offense / k=5 defense, on 8 style+efficiency inputs each (success rate,
  explosiveness, power success, stuff rate, rush/pass splits) from `adv_stats.rds`, team-season
  means (≥6 games), 2019–2025.
- As-of: each game uses the **prior-season** cluster assignment (`season+1` join) — genuinely
  pregame, no look-ahead.
- Games: 3,506–3,538 depending on join (push-excluded for ATS tests).
- Selection history: this is the **first and only** configuration tried before this report
  (k=5 was not grid-searched; disclosed as a caveat in §6, not hidden).

## 2. Cluster dictionary

**Offense** (z-score centroids): **A** = elite/methodical (success ↑1.4, low boom); **B** =
efficient grind (moderate success, low explosiveness); **C** = boom-or-bust (low success, high
explosiveness, worst power-run); **D** = explosive/spread (average success, high explosiveness);
**E** = weak across the board.
**Defense**: **A** = havoc/bend-a-lot (high explosiveness allowed, high stuff rate — high
variance); **B** = lockdown vs. the run and pass (best success rate allowed, worst explosiveness
allowed to offense — genuinely stout); **C/D** = middling, bend-don't-break variants; **E** =
weak across the board.

## 3. Decisive battery (Tier 1 required tests)

All chronological (train 2021–23 / test 2024–25), both sides of a game always in the same fold,
cluster fitting independent per representation.

| Representation | Omnibus interaction vs. line (game-level) | OOS bet-the-model win% | cor(pred, OOS cover margin) | **cor(pred, base-model OOF error)** | Cell persistence |
|---|---|---|---|---|---|
| **RAW** (original) | p=0.126 | 50.7% | +0.013 | **+0.001** | **+0.44** |
| **QUALITY-only** (clustered on PPA alone) | p=0.716 | 50.4% | −0.013 | −0.012 | +0.32 |
| **STYLE-only** (style residualized on PPA, then clustered) | p=0.661 | 51.6% | +0.023 | +0.042 | **−0.32** |

**Reading:** most of the RAW persistence (+0.44) is attributable to residual **quality**, not
style — the quality-only clustering alone reproduces +0.32 of it. Once quality is genuinely
removed, the pure-style effect does not just weaken, it **reverses sign** between train and test
— it does not replicate as a stable phenomenon in either direction. This is `Phase 1`'s exact
diagnostic purpose, and it returned a clean negative.

## 4. Permutation / placebo test (why the plan requires this before trusting any p-value)

500 within-season shuffles of cluster labels (preserves marginal cluster sizes), recomputing
train→test cell persistence each time:

- Observed persistence: **+0.442**
- Permutation null: mean **+0.021**, sd **0.223**
- **p(perm ≥ observed) = 0.032**

So the RAW persistence pattern is modestly outside the shuffled-label noise distribution — it is
not *purely* an artifact of 25-cell multiple comparisons. But per the plan's own interpretation
table, a pattern can clear a placebo test and still carry no usable information, which is exactly
what §3's incremental columns show.

## 5. Incremental value over the existing base model (the target the plan weights most)

- OOS R²-equivalent of the full interaction model vs. actual cover margin: **0.0002** (cor 0.013).
- **cor(cluster-matchup prediction, base model's own OOF error) = +0.001.** This is the
  decisive number: the existing PPD projection's error is **not** explained by style-matchup
  information at all. There is nothing here for the base model to incorporate.
- OOS flat betting the full model's ATS side: **50.7%**, 95% CI **[48.1%, 53.3%]** — indistinguishable
  from a coin flip, well under the −110 breakeven of 52.4%.

## 6. Heatmaps (required audit outputs)

**Cover% by (offense cluster) × (opponent defense cluster), all years:**
```
off    A     B     C     D     E
 A   45.0  48.0  47.1  47.3  54.8
 B   51.3  55.9  49.5  55.7  47.0
 C   41.1  59.7  53.1  51.9  55.2
 D   48.0  46.0  49.8  49.5  54.9
 E   44.9  44.4  50.9  51.3  48.4
```
**Sample size per cell (n games):**
```
off    A    B    C    D    E
 A   149  271  393  376  292
 B   158  272  467  393  351
 C    73   77  113  135   96
 D   254  337  508  584  381
 E   118  205  395  298  316
```
The smallest cells (C×A n=73, C×B n=77) are exactly where the most extreme rates appear
(41.1%, 59.7%) — the classic small-n-tail-of-the-distribution signature the plan warns about.

**Season stability of the single hottest well-populated cell (B×D, n=393, 55.7% overall):**
```
2021: 56.1% (n=41)   2022: 50.7% (n=73)   2023: 62.5% (n=88)
2024: 61.0% (n=82)   2025: 49.5% (n=109)
```
Not monotonic, reverts to breakeven in the most recent season, no visible trend — a diagnostic
picture of a noisy cell, not a stable archetype effect.

## 7. Leakage audit

| Stage | Status |
|---|---|
| Cluster inputs (style/efficiency stats) | **PASS** — prior-season team aggregates only |
| Cluster→game join | **PASS** — `season+1`, genuinely as-of |
| Train/test split | **PASS** — chronological, both game sides same fold |
| Cluster fitting | **PASS** — refit independently per representation |
| k selection, representation choice | **CAVEAT (disclosed)** — k=5 and the quality/style split were analyst choices, not selected inside a nested inner loop over multiple k. A smaller degree of freedom than the original discovery, but not zero. Any future revisit should nest this. |

## 8. Ablation table

| Model | OOS cor w/ cover margin | OOS win% betting the pick |
|---|---|---|
| Base model alone (existing PPD projection vs. spread) | — (already established: p=0.76 encompassed, §HANDOFF) | — (ATS closed at the line; not deployed) |
| + RAW cluster-matchup interaction | 0.013 | 50.7% |
| + QUALITY-only cluster interaction | −0.013 | 50.4% |
| + STYLE-only cluster interaction | 0.023 | 51.6% |

No configuration clears breakeven; none add measurable incremental value to the base model
(§5). Per the plan's ablation instructions: "do not promote a large bundle if only one
component is responsible" and "do not discard a modest feature that... improves ensemble
performance consistently" — here there is no component to promote and no ensemble improvement
to preserve.

## 9. Answer to the final research question

> *Does offense-versus-defense stylistic compatibility contain stable, incremental, pregame
> information about market error after controlling for unit quality, the existing model, sparse
> matchup samples, model-selection bias, and realistic betting prices?*

**No, on the evidence gathered.** The apparent signal (persistence +0.44) survives a placebo
test (p=0.032) but is predominantly a restatement of team **quality**, not style compatibility
— it disappears (and reverses direction) once quality is properly removed (Phase 1's exact
diagnostic), carries **zero correlation with the existing model's own forecast error** (the key
incremental-value target), and produces coin-flip OOS betting performance. The single hottest
well-populated cell is unstable season to season. This matches several of the plan's own stop
conditions simultaneously: *"the effect vanishes after separating style from quality"* and
*"the feature predicts raw margin but not ATS or base-model residuals."*

## 10. Recommendation

**Terminate this research program at Tier 1.** Do not proceed to Phases 2–11 / Tiers 2–6
(soft/GMM clustering, situational archetypes, response-based co-clustering, low-rank
factorization, market-timing analysis) — those phases exist to characterize *real* structure
found in Tier 1, and none was found. Running them would be searching a null for a mechanism,
which is the exact failure mode the plan's cited references (Bailey et al. on backtest
overfitting; Cawley & Talbot on selection-bias-inflated performance) warn against, and which the
plan's own stop-conditions section instructs against forcing.

**Preserved, not wasted:** `data_cache/style_clusters.rds` (interpretable offense/defense
"types") is kept as a **descriptive** feature for the Extras board (already shown there) —
explicitly labeled a football-description feature, not a betting feature, per the plan's
instruction to preserve non-edge value where it exists (it does not meaningfully improve
raw-margin RMSE either, so it is kept purely for display interpretability).

No forward-test is authorized. No promotion gate is met (0 of the required 11 gates in the
plan's "Promotion gates" section can be marked passed on this evidence).
