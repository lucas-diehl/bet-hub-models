# 2026 Forward Test — Pre-Registration

**Dated:** 2026-09-01 (frozen before the next graded pick).
**Why:** both edges are in-sample. A forward test with no pre-committed criterion
becomes a post-hoc rationalization the moment results arrive. This freezes the rules
and the decision criteria **now**, in writing, so 2026 is a genuine out-of-sample test.

**Do not edit the rules or thresholds below mid-season.** Any change to a rule, a
threshold, a filter, or the book **restarts the forward test** for that arm (new
start date, counter reset).

---

## 1. The arms under test (frozen as of 2026-09-01)

Config is inlined from `07_write_feed.R`. Both arms are **PAPER**.

**Arm T — UNDER totals**
- Bet UNDER when `book_total − proj_total ≥ 4` (`UNDER4`), OR
  `book_total − proj_total ≥ 3` AND the model likes the market underdog ATS (`UNDER3_DOG`).
- EV > 0 at −110. Book = DraftKings. FBS-vs-FBS only.

**Arm A — early-season 4th-down-aggressiveness ATS**
- Weeks 1–4 only. Both teams `n4 ≥ 20` career 4th-down decisions.
- `|go-rate gap| ≥ 0.10` → bet the more-aggressive team ATS.
- Cumulative **career** go-rate, coach-persistent (this is untested for a recency
  confound — see HANDOFF §5.2 / P1; the frozen rule uses career rate as deployed today).
- **REMOVED from feed 2026-09-03** (encompassing closed it at the close; see §5 log). Frozen
  here only as historical record.

**Book / grading:** DraftKings line, graded at the number posted in the picks file
(post/grade split), pushes graded as **push** (no decision), CLV = posted vs DK close.

**Arm O — `proj_vs_open` ATS (added 2026-09-08, live from wk2 2026)**
- Bet the model's margin vs the **FROZEN OPENING line** (captured early-week by
  `09_capture_open_lines.R`, before the number sharpens toward the model — the whole
  mechanism is time-to-kickoff decay, so the decision line is NEVER the current/live spread).
- `|proj_margin − open_margin| ≥ 3` → bet the side the model favors relative to the open.
- `model_prob`: 0.525 (mag 3–6) / 0.535 (mag ≥6) — deliberately shrunk near the 0.524
  breakeven, `prob_source: empirical_insample`. Do NOT raise these without a fresh backtest;
  the backtest win rates were 53.4%/53.6%, not the 57%+ that got Arm A removed for overclaiming.
  Stake 0.5u flat (no tiering — the edge is far thinner than Arm A ever claimed).
  Tag `OPEN_ATS`. Book = DraftKings preferred, FanDuel fallback (never Caesars — kept in the
  capture ledger for future shopping research only, not used as a bettable price).
- **Distinct mechanism from Arm A**: recency lag in the market's early-week number, not
  coaching-behavior mispricing. Independently validated on 3 line sources (cached Bovada/DK
  2021–25, clean Odds API 2023–25, an external repo's independent CFBD opens 2021–25) — see
  `FEATURE_REGISTRY.md` `proj_vs_open`. Encompassing coef 0.138, p=0.004 vs the open;
  p=0.57 (fully encompassed) vs the true close — **this arm has NO edge if graded at the
  close**; the ledger's frozen opening number IS the entire edge. If the capture pipeline
  ever fails to freeze a game's open (no ledger row), **no bet is emitted for that game** —
  it never silently falls back to the current/live line.
- **Known limitation, disclosed up front:** the dedicated early-capture tasks
  (`CFB-Open-Capture-Sun`/`-Mon`) currently run as `InteractiveToken` (need the user logged
  in), not the hands-off `S4U` the rest of the pipeline uses — a Windows permission wall this
  session couldn't clear non-interactively (see DATA_SCIENTIST_HANDOFF). `weekly_update.R`
  (confirmed S4U) re-attempts capture every Mon/Thu as a backstop, so a week is only fully
  missed if the user is both logged out at the two early windows AND the number has already
  moved materially by Monday/Thursday's backstop capture — expected to somewhat *shrink* the
  realized edge in weeks this happens, not invalidate it.

**Book / grading (Arm O):** DraftKings/FanDuel frozen opening line, graded at that exact
posted number (post/grade split as above), CLV = posted (open) vs DK **close** (the current
line at grading time) — a *positive* CLV is the core validating prediction (backtest: +0.31pt
@mag≥3, +0.43 @mag≥6). This is the opposite of Arm A's flat-CLV failure mode — if Arm O's
forward CLV comes back flat or negative, treat it exactly as fatally as Arm A's was.

---

## 2. Primary criterion — CLV (win rate is underpowered)

**Power reality (why CLV, not win rate):**
- Arm T: ~267 UNDER bets/season. One season *just* powers detection of a ~6pt edge;
  a 3pt edge needs ~1,000 bets (≈4 seasons). One season **cannot** settle it.
- Arm A: ~65 ATS bets/season. One season **cannot** distinguish 57% from 52%
  (needs ~380 bets ≈ 6 seasons).
- Therefore **win rate is a secondary, explicitly-underpowered readout.** CLV is primary.

**Primary metric:** rolling **mean CLV per arm** (last 50 graded bets), reported with SE.
- **Continue** while rolling mean CLV ≥ 0.
- **Suspend the arm** if rolling mean CLV ≤ **−1.0 point** (Arm A, spreads) / the
  equivalent **≤ −0.75 point** on the total (Arm T), i.e. clearly negative closing-line value.
- A CLV that is flat *and* a win rate at/below breakeven after ≥150 bets (T) / the full
  first season (A) → **suspend** (edge unconfirmed).

**Backtest priors (what "flat/negative" is measured against):**
- **Arm A CLV in backtest is FLAT: +0.025 pts, t=0.19, 48.9% positive (n=182, 2023–25).**
  The stated mechanism (market slow to price aggressiveness) predicts *positive* CLV.
  It is absent in backtest. **Forward CLV is the deciding evidence for Arm A** — if it
  stays flat/negative, the 57.4% backtest win rate was luck and Arm A is dropped.
- Arm T CLV is not measured in backtest (single cached consensus total). Forward CLV
  (posted-vs-DK-close) is new, clean signal — collect it from day one.
- **Arm O backtest CLV is POSITIVE and is the core reason it's live** (+0.31pt @mag≥3,
  +0.43 @mag≥6, both directions confirmed independently on 2 line sources). Suspend threshold:
  same as Arm A, ≤ −1.0pt rolling. ~65–100 bets/season expected (similar volume to old Arm A) —
  win rate alone won't settle it in one season; **forward CLV is again the deciding evidence.**

## 3. Secondary criterion — win rate (underpowered, reported for completeness)

Report per arm, per season, **push-excluded**, with bet count and a Wilson interval:
- Breakeven at −110 = 52.4%.
- Backtest baselines (leak-fixed, push-corrected): **Arm T UNDER≥4 = 55.9% / 454 bets**
  (season-inconsistent: 2021 below breakeven; carried by 2023 & 2025).
  **Arm A wk1–4 = 57.4% / 326 bets** (multiplicity-unadjusted p=0.041; ~0.8 adjusted, later
  removed). **Arm O = 53.4% @mag≥3 / 53.6% @mag≥6** (clean Odds API 2023–25; per-season
  50.6→53.0→56.1%, not decaying) — the frozen `model_prob` (0.525/0.535) is set BELOW this
  raw backtest number, deliberately.
- Do **not** act on a single season's win rate alone. It informs, CLV decides.

**Regime watch (Arm T decay risk, P1 §10):** the backtest UNDER edge is **heavily post-2023**
(61.6% vs 52.0% pre-clock-rule), likely a lagging-pace-adjustment inefficiency, and the
block-bootstrap 95% CI **includes breakeven [50.7%, 61.2%]**. 2026 has further pace/clock
tweaks — if 2026 UNDER win rate reverts toward ~52% AND CLV is flat, the edge has decayed;
suspend. The higher-volume **UNDER≥3** threshold is statistically more robust than the deployed
≥4 (which fails multiplicity); consider it if ≥4 underperforms live. Book check (§9): DK totals
sit only +0.13 pt above consensus, so the edge is model skill, not DK-off-consensus.

## 4. What must be built to run this test (tracking)

- [ ] Weekly output reports **per-arm mean CLV + SE** (aggregate the `clv_pct` in
      `results_<date>.json`), not just per-pick CLV. (P1 in the review.)
- [ ] Per-arm rolling-50 CLV panel on the dashboard, shown next to win rate with a
      **bet-count** and an interval (not a bare point estimate).
- [ ] `prob_source` on the feed so Arm A's empirical/in-sample 0.60/0.55 probabilities
      are labeled, not rendered as calibrated model output (P1).

## 5. Decision log (append-only; do not rewrite)

| Date | Arm | Bets to date | Rolling-50 mean CLV | Win% (push-excl) | Action |
|---|---|---|---|---|---|
| 2026-09-01 | T | 0 | — | — | pre-registered, PAPER (live feed) |
| 2026-09-01 | A | 0 | — | — | pre-registered, PAPER; on CLV probation (flat backtest CLV) |
| 2026-09-03 | A | 0 | — | — | **REMOVED from live feed** (encompassing: proj_margin p=0.76 + components all p>0.35 → ATS closed; H1 dead). Continues as **backtest-only research** under H2 (persistent closing-line error, needs ~6 seasons) — not on the dashboard. Re-emit via `PPP_EMIT_ATS=1` only if an edge is established. |
| 2026-09-08 | O | 0 | — | — | **ADDED to live feed** (PAPER), first live capture wk2 2026 (185 game/book opening lines frozen, 5 games cleared the mag≥3 threshold). Pre-registered here before any forward result is known. Capture-task S4U limitation disclosed above — track whether it costs missed/late captures. |
| 2026-09-09 | O | 0 | — | — | **RULE CHANGE → forward test RESTARTED for Arm O** (per §1's own rule). Arm O v2 adds a **DVOA consensus-agreement gate**: ≥3 of 5 independently-built DVOA-style ratings (`10_build_dvoa_ratings.R`) must agree with the PPD projection on the side. `model_prob` 0.525/0.535 → **0.58** (shrunk cross-variant estimate; the best single-variant number, 61.2%, is winner's curse and must NOT be used for sizing — it over-stakes ~57% under Kelly). Counter reset to 0; prior Arm O bets are void for evaluation purposes. Basis: FEATURE_REGISTRY `AGREEMENT_MECHANISM` — consensus shows a monotonic dose-response (≥1..≥5 votes → 56.3/56.8/58.6/58.9/61.9%), is NOT reproducible via cheap maturity/volatility proxies, and k≥3 was chosen a priori (simple majority), not for its score. **Known limits, disclosed:** (a) discovered on 2024-25, so forward data is the only clean test; (b) power analysis says the *improvement* over unfiltered (58% vs 55.1%) needs ~19 seasons to confirm — only the above-breakeven claim is confirmable (~3 seasons); (c) strategy runs ~85-89% underdogs by construction, and big dogs (≥14) are the weakest segment (55.6% vs 64.7% small dogs). |

**2026-09-09 (amendment to the Arm O v2 row above, made the same day, before any Arm O bet
was graded — 0 forward bets exist, so this is an amendment, NOT a second restart):**
Arm O stake moves from **flat 0.5u** to a **confidence ladder: 3 votes = 1u, 4 = 2u, 5 = 3u**
(`PPP_ATS_STAKE_MIN`/`_MAX`), at the user's direction. Recorded honestly:
- **The tiers are not statistically distinguishable.** Marginal win rates are ==3 55.6%,
  ==4 52.0%, ==5 61.9% — **non-monotonic** (==4 sits BELOW ==3); the k=3..5 trend test gives
  p=0.123; EB shrinkage returns **tau2 = 0**, collapsing all three tiers to the 58.6% grand
  mean. Two other candidate axes were tested and also flattened: dog line size (tau2=0.0003,
  1.5pp spread post-shrinkage, p=0.125) and edge magnitude (non-monotonic, p=0.787).
  The earlier "monotonic dose-response" was **cumulative** (>=k) and is an artifact of the
  n=294 5/5 bucket sitting inside every cumulative cell.
- Therefore `model_prob` stays **flat at 0.58 on every tier** — only the stake moves. The
  ladder is a **risk-appetite choice, not a measured edge**, and must not be reported as one.
- **Exposure roughly 5.5x**: 9.5u → 52u for wk2 (19 bets); backtest median week 41u, p90 67u,
  max 85u. These are ~85% underdogs on one Saturday and therefore correlated — the effective
  risk is higher than the unit count implies. What "1u" means in bankroll terms now dominates
  every modelling question in this arm.
- Backtest ROI +11.80% flat vs +13.64% laddered; the +1.84pp comes entirely from overweighting
  the 5/5 bucket whose 61.9% is the same winner's-curse figure sizing was explicitly kept off.
  **Do not treat it as expected gain.**
- **Primary criterion is unaffected**: win rate and CLV are stake-invariant. Only the unit-ROI
  readout changes, so the graduation test below still reads the same.
- **Arm T (UNDER) is deliberately unchanged** at its existing stake: it already has graded
  bets, so re-staking it mid-test would restart its forward test for no measured gain.

**Graduation to funded** requires, per arm: rolling mean CLV ≥ 0 sustained over the
first full season AND a win rate not below breakeven — reviewed at season end, not
mid-season.
