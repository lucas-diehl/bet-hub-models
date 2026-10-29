"""4 bet segments: the 3 with real statistical support (P(ROI<=0) < 0.05) from the
2025-26 diagnostic pass, plus the next-best 1 by backtest_p_le0 among the original
candidates (owner's "core + next best" trim, 2026-10-20). See HANDOFF.md section 4 in the
bet-screen possessions repo for the full backtest history.

`backtest_win_pct` added 2026-10-21 so live picks can carry a real `model_prob` on the
Bet Hub feed (reports/_bet_menu.py re-run against the same sample; its roi/p_le0/n
output matched this file's existing numbers exactly before the win_pct column was
trusted and copied in here).

2026-10-29: replaced the role-narrowed rebounds/blowout-risk cut (bench-only,
backtest_roi=0.1614, n=262) with the broader, role-UNCONDITIONED version
(backtest_roi=0.0701, n=1086, role=None below). HANDOFF.md's own text on the bench-only
cut already called it "a hypothesis for FORWARD testing, not a confirmed sub-edge,"
built on a mechanism (starters sit early in blowouts) that the investigation itself
found backwards, and a third level of post-hoc conditioning on an already-thin sample --
textbook overfitting risk, not a tempting coincidence. It then went live anyway and lost
money three straight nights (2026-10-25/27/28, each 5-10 at flat stakes, -14.90u pooled,
all three concentrated in exactly this cut) -- consistent with, not just theoretically
at risk of, the overfitting the project's own research flagged. The broader cut is
better-supported on every axis that matters: 4x the sample, stable across spread
thresholds 3-8 (not fragile to the exact cutoff), and replicates in both halves of the
season (first half +10.73%, second half +3.61%, same direction). Freshly re-verified,
not just quoted from memory, via nba/reports/_phase2_rebounds_mechanism.py (deterministic
re-run, numbers unchanged) and the new nba/reports/_monotonic_power_test.py (continuous
edge significantly orders outcomes within rebounds-Under specifically, p=0.0022, ROI
+2.52% -- a different, independent test reaching the same conclusion). This absorbed the
old starter/blowout-risk "next-best" entry too, since it's now the complement of the
same role-blind population, not a separate thing to track.

Left alone, deliberately, pending its own explicit decision: the bench-only rebounds/
CLOSE-game cut below (backtest_roi=0.1138, p=0.016) has the identical risk shape -- its
own broader, role-unconditioned test in HANDOFF.md was NOT significant (close-game
rebounds edge +0.61pp, P<=0=0.348) -- but nobody asked to touch it yet.
"""

from __future__ import annotations

from dataclasses import dataclass

CLOSE_SPREAD = 5.0  # |spread| < this = "close"; >= this = "blowout-risk"
STARTER_MIN = 25.0  # own EWMA minutes >= this = "starter"; below = "bench"


@dataclass(frozen=True)
class Segment:
    market: str  # Odds API market key, e.g. "player_rebounds"
    side: str  # "Over" or "Under"
    role: str | None  # "starter", "bench", or None = matches either role (role-blind)
    script: str  # "close" or "blowout-risk"
    backtest_roi: float
    backtest_p_le0: float  # P(ROI<=0) from the 2025-26 date-block bootstrap backtest
    backtest_n: int
    backtest_win_pct: float  # realized win rate on the SAME 2025-26 sample (bet-screen/
    # nba/reports/_bet_menu.py, re-run 2026-10-21 to cross-check backtest_roi/p_le0/n above --
    # reproduced exactly). This is the real, measured number behind `model_prob` on a live
    # pick: this system doesn't fit a per-player probability, it bets a fixed historical
    # bucket, so its honest "model probability" IS that bucket's own realized win rate, not
    # a fabricated continuous figure.

    @property
    def confirmed(self) -> bool:
        """True only for the 3 segments with real statistical support (P(ROI<=0) < 0.05).
        The other 1 (next-best by backtest_p_le0) is being forward-tracked on the owner's
        explicit decision, not because it's validated -- every consumer of this flag
        (site, grading) must keep them visibly distinct, not silently equal."""
        return self.backtest_p_le0 < 0.05


SEGMENTS: list[Segment] = [
    # The 3 confirmed (P(ROI<=0) < 0.05).
    Segment("player_rebounds", "Under", None, "blowout-risk", 0.0701, 0.014, 1086, 0.557),
    Segment("player_rebounds", "Under", "bench", "close", 0.1138, 0.016, 207, 0.575),
    Segment("player_threes", "Under", "bench", "close", 0.1289, 0.029, 148, 0.608),
    # Next-best 1 by backtest_p_le0 among the original candidates.
    Segment("player_points", "Under", "starter", "close", 0.0313, 0.119, 478, 0.540),
]


def classify_role(min_ewma_prior: float | None) -> str | None:
    if min_ewma_prior is None:
        return None
    return "starter" if min_ewma_prior >= STARTER_MIN else "bench"


def classify_script(spread_abs: float | None) -> str | None:
    if spread_abs is None:
        return None
    return "blowout-risk" if spread_abs >= CLOSE_SPREAD else "close"


def matching_segment(market: str, side: str, role: str | None, script: str | None) -> Segment | None:
    # role is the PLAYER's classified role here and must be known (a player with no
    # minutes history can't be scored either way); seg.role is the SEGMENT's own
    # criterion and None there means "role-blind", matching a known player role of either
    # kind -- the two Nones mean different things, this isn't a redundant check.
    if role is None or script is None:
        return None
    for seg in SEGMENTS:
        if seg.market == market and seg.side == side and seg.script == script \
                and (seg.role is None or seg.role == role):
            return seg
    return None
