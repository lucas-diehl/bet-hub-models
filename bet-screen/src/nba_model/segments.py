"""The 11 candidate bet segments from tonight's diagnostic work (owner's selection,
2026-10-20), with their backtested record attached so nothing downstream has to guess
which ones are statistically real vs. exploratory. 3 of 11 clear P(ROI<=0) < 0.05 on the
2025-26 backtest; the rest range from weak to a literal coin flip -- see HANDOFF.md section
4 for the full numbers. This module exists to make that status impossible to lose track of
as these get wired into a live pipeline and a public-facing site.
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

    @property
    def confirmed(self) -> bool:
        """True only for the 3 segments with real statistical support (P(ROI<=0) < 0.05).
        The other 8 are being forward-tracked on the owner's explicit decision, not because
        they're validated -- every consumer of this flag (site, grading) must keep them
        visibly distinct, not silently equal."""
        return self.backtest_p_le0 < 0.05


SEGMENTS: list[Segment] = [
    Segment("player_points", "Under", "bench", "close", 0.0480, 0.199, 248),
    Segment("player_points", "Under", "starter", "close", 0.0313, 0.119, 478),
    Segment("player_rebounds", "Under", "bench", "blowout-risk", 0.1614, 0.003, 262),
    Segment("player_rebounds", "Under", "starter", "blowout-risk", 0.0410, 0.129, 824),
    Segment("player_rebounds", "Under", "bench", "close", 0.1138, 0.016, 207),
    Segment("player_threes", "Under", "bench", "close", 0.1289, 0.029, 148),
    Segment("player_threes", "Under", "starter", "close", 0.0010, 0.492, 388),
    Segment("player_points_rebounds_assists", "Under", "starter", "blowout-risk", 0.0051, 0.445, 715),
    Segment("player_points_rebounds_assists", "Over", "bench", "blowout-risk", 0.0537, 0.250, 101),
    Segment("player_assists", "Over", "bench", "blowout-risk", 0.0974, 0.150, 109),
    Segment("player_points", "Over", "bench", "blowout-risk", 0.0065, 0.453, 331),
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
