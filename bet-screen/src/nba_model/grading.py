"""Shared odds-matching and bet-grading utilities -- extracted here because both
reports/_props_pilot_backtest.py (ROI-based v1) and the calibrated log-loss backtest
(the methodologically correct version per the user's plan section 0.1) need the exact same
matching logic, and duplicating it would risk the two drifting out of sync.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from .market import REQUIRED_BOOKS
from .names import normalize_name

MARKET_MAP = {
    "player_points": "pts",
    "player_rebounds": "reb",
    "player_assists": "ast",
    "player_threes": "fg3m",
    "player_points_rebounds_assists": "pra",
}
ACTUAL_COL = {"pts": "pts", "reb": "reb", "ast": "ast", "fg3m": "fg3m", "pra": "actual_pra"}


def american_profit(price: float, won: bool) -> float:
    if not won:
        return -1.0
    return price / 100.0 if price > 0 else 100.0 / abs(price)


def best_price_lines(props: pd.DataFrame) -> pd.DataFrame:
    """One row per (date, player_norm, market, side): the best (highest) price across the
    four required sharp books ONLY.

    Restricting to REQUIRED_BOOKS isn't just a convention carried over from the CBB spreads
    pipeline -- it's load-bearing here. Confirmed on the real pilot data: Bovada (and some
    others) post a full alternate-line ladder per player (7-8 different point values, each
    with its own escalating price), while draftkings/fanduel/betmgm/williamhill_us each post
    exactly one point per player/market. Taking "best price" across ALL books blindly pairs
    one book's extreme high-point Over alt-line against another's extreme low-point Under
    alt-line (since price moves monotonically with point along the ladder), producing a fake
    4-6 point gap between "point_over" and "point_under" that isn't a real line at all -- this
    silently wrecked the first version of the ROI backtest (38% win rate on points, nowhere
    near a real no-edge ~50%) before being caught by eyeballing concrete examples.
    """
    props = props[props["book"].isin(REQUIRED_BOOKS)].copy()
    props["player_norm"] = props["player"].map(normalize_name)
    idx = props.groupby(["date", "player_norm", "market", "side"])["price"].idxmax()
    return props.loc[idx]


def wilson_ci(wins: int, n: int, z: float = 1.96) -> tuple[float, float]:
    if n == 0:
        return (float("nan"), float("nan"))
    p = wins / n
    denom = 1 + z**2 / n
    center = p + z**2 / (2 * n)
    spread = z * np.sqrt(p * (1 - p) / n + z**2 / (4 * n**2))
    return ((center - spread) / denom, (center + spread) / denom)


def paired_lines(props: pd.DataFrame) -> pd.DataFrame:
    """Over/under best-price lines paired side by side, one row per (date, player, market)."""
    lines = best_price_lines(props)
    over = lines[lines["side"] == "Over"]
    under = lines[lines["side"] == "Under"]
    return over.merge(under, on=["date", "player_norm", "market"], suffixes=("_over", "_under"))
