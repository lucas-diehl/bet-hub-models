"""5 bet segments: the 3 with real statistical support (P(ROI<=0) < 0.05) from the
2025-26 diagnostic pass, plus the next-best 2 by backtest_p_le0 among the original 11
candidates (owner's explicit "3 core + next best 2" trim, 2026-10-20). The other 6
original candidates (coin-flip to weak) were dropped to stop over-posting. See
HANDOFF.md section 4 in the bet-screen possessions repo for the full backtest history,
including a follow-up mechanism investigation that complicates the simple "p<0.05"
framing for the role-narrowed rebounds segments specifically.

`backtest_win_pct` added 2026-10-21 so live picks can carry a real `model_prob` on the
Bet Hub feed (reports/_bet_menu.py re-run against the same sample; its roi/p_le0/n
output matched this file's existing numbers exactly before the win_pct column was
trusted and copied in here).
"""

from __future__ import annotations

from dataclasses import dataclass

CLOSE_SPREAD = 5.0  # |spread| < this = "close"; >= this = "blowout-risk"
STARTER_MIN = 25.0  # own EWMA minutes >= this = "starter"; below = "bench"


@dataclass(frozen=True)
class Segment:
    market: str  # Odds API market key, e.g. "player_rebounds"
    side: str  # "Over" or "Under"
    role: str  # "starter" or "bench"
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
        The other 2 (next-best by backtest_p_le0) are being forward-tracked on the owner's
        explicit decision, not because they're validated -- every consumer of this flag
        (site, grading) must keep them visibly distinct, not silently equal."""
        return self.backtest_p_le0 < 0.05


SEGMENTS: list[Segment] = [
    # The 3 confirmed (P(ROI<=0) < 0.05).
    Segment("player_rebounds", "Under", "bench", "blowout-risk", 0.1614, 0.003, 262, 0.607),
    Segment("player_rebounds", "Under", "bench", "close", 0.1138, 0.016, 207, 0.575),
    Segment("player_threes", "Under", "bench", "close", 0.1289, 0.029, 148, 0.608),
    # Next-best 2 by backtest_p_le0 among the original 11 candidates.
    Segment("player_points", "Under", "starter", "close", 0.0313, 0.119, 478, 0.540),
    Segment("player_rebounds", "Under", "starter", "blowout-risk", 0.0410, 0.129, 824, 0.541),
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
    if role is None or script is None:
        return None
    for seg in SEGMENTS:
        if seg.market == market and seg.side == side and seg.role == role and seg.script == script:
            return seg
    return None
