"""Stage A -- minutes model.

v1 scope, deliberately narrow: project a player's minutes GIVEN he is expected to play (not
ruled out). Availability itself -- will he play at all tonight -- is a separate signal (live
injury report + the teammate-redistribution feature, see HANDOFF.md section 3) that combines
with this projection later; it is not estimated here. This file only answers "how many minutes,
given he suits up," which is the honestly-scoped baseline everything else builds on.

Every feature is leak-safe by construction: each player's game history is shifted by one
before any rolling/EWM computation, so game t's features only ever see games strictly before t
(mirrors cbb_model.features's no-lookahead discipline, cbb/tests/test_no_lookahead.py).
"""

from __future__ import annotations

import pandas as pd


def ewma_halflife_alpha(halflife: float) -> float:
    return 1.0 - 0.5 ** (1.0 / halflife)


def add_minutes_features(player_games: pd.DataFrame, halflife: float = 8.0) -> pd.DataFrame:
    df = player_games.sort_values(["player_id", "game_date"]).copy()
    alpha = ewma_halflife_alpha(halflife)
    grp = df.groupby("player_id")["min"]

    df["min_ewma_prior"] = grp.transform(lambda s: s.shift(1).ewm(alpha=alpha, min_periods=1).mean())
    df["min_prior_game"] = grp.transform(lambda s: s.shift(1))
    df["games_played_so_far"] = df.groupby("player_id").cumcount()
    # "in rotation" signal: how many of the player's last 10 prior games they featured in at
    # all -- distinguishes an established rotation player from someone just recalled/debuting.
    df["recent_games_count"] = grp.transform(lambda s: s.shift(1).rolling(10, min_periods=1).count())
    # Minutes volatility -- a role-stability signal in its own right (diagnosis plan 1.2:
    # "the player's minutes uncertainty"), not just a nuisance parameter for the projection.
    # Rolling, not EWM, std: EWM std is defined but noisier to interpret at small windows;
    # a plain trailing-10-game std is the more legible "how uncertain is this role" number.
    df["min_std_prior"] = grp.transform(lambda s: s.shift(1).rolling(10, min_periods=3).std())
    return df


def baseline_predict(df: pd.DataFrame) -> pd.Series:
    """The simplest honest projection: the leak-safe EWMA itself."""
    return df["min_ewma_prior"]
