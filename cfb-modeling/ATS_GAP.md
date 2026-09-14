# Beating ATS "normally" — findings & what's missing

**Question:** we have a narrow early-season ATS signal. What would it take to beat the
**spread market in general** — any week, systematically — the way we (tentatively) beat
totals?

**Short answer:** we can't, and this is now **established, not inferred**. Beating ATS normally
requires information the closing line lacks — and everything our efficiency model knows is
already in the line.

> ## ⚠️ UPDATE (2026-09) — the decisive tests are in
> The earlier draft *inferred* ATS was closed from "RMSE 16.6 > 15.3." The reviewer was right that
> that's not proof — so I ran the **forecast-encompassing** test, and it settles it:
> - **`actual_margin ~ −spread + proj_margin` → proj_margin p=0.76.** The market fully encompasses
>   our margin projection. **ATS is closed by projection, established.**
> - **A-prime tested too:** projecting from **component** ratings (pass/rush/expl/epa/fin matchups)
>   is *also* fully encompassed — every component p>0.35 (net_pass, the lasso's #1 driver, p=0.84).
>   So **no projection improvement can create ATS edge** (not A-prime, better ratings, mixed model,
>   QB-blind upgrades). The market prices everything our ratings know.
> - **The "market is slow" mechanism (H1) is dead:** `line_move ~ agg_diff` over all wk1–4 games is
>   p=0.46; backtest CLV flat (t=0.19). The early-ATS niche, if real, is **H2 (closing line
>   persistently wrong)** — untestable on a one-season horizon, ~6 seasons of paper to resolve.
> - **Soft-corner check (§4):** model−market RMSE gap is smallest in G5-G5 (0.90) and late season
>   (wk9+ 0.50), worst in P5-G5 mismatches (5.73) — but the best segment is still worse than the
>   market and encompassing is dead-zero, so **there is no real soft corner** to exploit by
>   projection.
>
> **Re-ranked conclusion:** the only ATS avenue with a mechanism is **§5 below — pricing QB
> availability better than the market's blunt adjustment** (a *new-information* play buildable from
> cached pbp, not a projection upgrade). Opening-line capture is worth building **for the totals
> arm** (whose opener CLV is unmeasured), not for early-ATS (flat CLV). Everything else re-fishes an
> efficient market.

---

## 1. Where we actually stand on ATS (recent findings)

From the leakage-audited walk-forward review (2021–2025):

- **General ATS is a dead end for us.** In the A/B/C bake-off, every approach sat at ~50–52%
  ATS with negative ROI, and significance *below* breakeven. The closing spread is the
  sharpest number in football.
- **Our margin projection is worse than the market.** Approach-A `proj_margin` RMSE ≈ **16.6**
  vs the market's ≈ **15.3**. A model that predicts margin *less* accurately than the line it's
  betting into cannot beat that line by projection. (Totals is different: there our totals RMSE
  ~= the market's, and the edge comes from a *directional* over-projection asymmetry, not from
  out-predicting the number.)
- **The one ATS signal that showed anything is narrow and unconfirmed:** early-season 4th-down
  aggressiveness (weeks 1–4). It has real structural support — a clean weeks-5+ specificity null
  (p=0.001 wk1–4 vs 0.56 wk5+) and a controlled regression (β=+12.4, p=0.003, survives
  tenure/size/P5 controls, not a "bet newer coaches" proxy) — **but backtest CLV is flat**
  (+0.025 pts, t=0.19) and it **fails multiplicity adjustment** (raw p=0.041 → BH ≈ 0.08 over
  ~20 tests). It works, if at all, only because the market is briefly slow to price coaching
  tendencies in September, when player data is stale. It is on **CLV probation** for 2026.
- **Behavioral/coaching features we tested and the market already prices:** play-call entropy,
  2nd-and-short aggression, tempo, 4th-down aggression *as a general (all-season) feature* — all
  `lm(margin ~ −spread + feature)` p > 0.5. The market is not slow on these outside the opener
  window.

**Conclusion:** there is no "normal" ATS edge in hand. There is one September niche, unconfirmed.

---

## 2. Why beating the spread is structurally hard

The closing spread is a near-efficient consensus of (a) every public model, (b) injury/weather
news, and (c) sharp money that moves the line to that consensus by kickoff. To beat it you must
have one of:

1. **A materially better margin model than the market** — we have the opposite (RMSE 16.6 > 15.3).
2. **Information the closing line hasn't fully absorbed** — we ingest none of the big spread
   movers (below).
3. **A softer number than the close** — opening lines, or low-liquidity games books watch less.
   We bet/grade at the close (the hardest bar) and have no opening-line pipeline.

Totals slipped through on #1-adjacent grounds (equal RMSE + a persistent directional bias the
market shares). Spreads give us none of the three.

---

## 3. What's specifically lacking

### 3.1 Data the market has and we don't (the biggest gap)
The spread's largest movers are exactly the inputs we never load:

- **QB / injury availability.** A starting-QB change is worth ~7–10 points and is *the* dominant
  spread input. We have zero injury/depth-chart data. Any ATS system that ignores QB status is
  betting blind into the market's best information.
- **Weather** (wind especially) — moves totals and spreads; we don't ingest game weather.
- **Rest / travel / short weeks / time-zone** — real, quantifiable spread effects; not modeled.
- **Motivation / situation** — lookahead spots, letdowns, rivalry, bowl motivation, coaching
  changes, tanking late-season. Hard, but priced by sharps and slow to hit the opener.

Without these, our margin model is a pure efficiency projection competing against a market that
already has all of them. That is why RMSE 16.6 > 15.3.

### 3.2 Methodology gaps
- **Margin projection quality.** Our PPD→margin projection is over-extreme and needs calibration
  just to reach RMSE 16.6. A genuinely competitive margin model would need opponent-adjusted
  **per-play EPA with explicit QB/personnel adjustments**, not team-season PPD.
- **We optimize for the wrong thing on spreads.** The totals edge is a *directional bias* play.
  There is no analogous free structural bias on spreads (HFA is already in the line).
- **No opponent-specific matchup margin model.** Pass-vs-pass-D, rush-vs-rush-D mismatches that
  move margins are only weakly captured; the lasso showed passing matchup is the #1 margin driver
  but our aggregate PPD washes it out.

### 3.3 Execution / market-access gaps
- **We bet the close, the hardest number.** The early-ATS edge's *entire* thesis is "the market is
  slow" — yet we can't exploit slowness without betting the **opening** line before it moves. We
  have no opening-line feed and no fast execution loop.
- **No CLV-timed betting.** Beating ATS is really about beating the *closing* line from an earlier
  number; that requires opening-line capture + speed, which we lack (and the flat backtest CLV
  says even our early-ATS picks don't beat the close from cfbd's opening data).
- **One book (DraftKings), no line-shopping.** ATS margins are thin; betting the best of several
  books' spreads is itself a real edge we forgo.
- **Low-liquidity games** (G5, mid-week MAC, FCS-adjacent) — where books are softer and a decent
  model can beat the number — are exactly the games we currently *exclude* or under-weight.

---

## 4. Concrete paths that could plausibly beat ATS (ranked by expected payoff)

1. **Ingest QB/injury availability and re-price margins around it.** Highest-leverage single
   addition. Even a binary "starter out" flag, applied fast when news breaks vs a slow-moving
   opening line, is a real edge. Needs a depth-chart/injury feed (not in cfbfastR) + speed.
2. **Attack opening lines, not closing.** Build an opening-line capture (Odds API pull at line
   posting) and bet the model's disagreement immediately. Grade CLV vs close. This is the only way
   the "market is slow" theses (including early-ATS) actually pay — and it's an infrastructure
   build, not a model build.
3. **Target low-liquidity sub-markets.** Focus the margin model on G5 / mid-week games where the
   number is softer, rather than the P5 marquee games the market prices hardest.
4. **Add weather + rest/travel** as margin adjustments — cheap data, real (if small) spread effects
   the opener can be slow on.
5. **Upgrade the margin model** to opponent-adjusted per-play EPA with QB/personnel terms and
   explicit unit matchups (passing especially) to close the RMSE gap with the market. Necessary
   foundation for any of the above to matter, but insufficient alone.

**What is NOT a path:** more team-level efficiency features, more coaching-behavior features
(we've shown the market prices them), or tuning thresholds on the existing projection. Those
re-fish the same efficient market.

---

## 5. Honest recommendation

Keep the **early-season ATS niche** running in PAPER (it's cheap, it has structural support, and
2026 CLV will confirm or kill it), but do **not** expect to beat ATS "normally" without (a) QB/injury
data and (b) opening-line execution. Of everything above, **#1 (QB availability) and #2 (opening-line
capture) are the only two that change the structural picture.** Both are data/infrastructure
projects, not modeling tweaks — which is the honest reason we beat totals but not spreads: totals
handed us a free directional bias; spreads demand information and execution we don't yet have.
