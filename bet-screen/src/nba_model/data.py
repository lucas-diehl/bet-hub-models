"""Canonicalization: raw nba_api tables -> the per-player-per-game table the minutes/usage
models train on. No modeling here, only normalization -- same split cbb_model.data uses.
"""

from __future__ import annotations

import re

import numpy as np
import pandas as pd

MATCHUP_RE = re.compile(r"^(?P<team>[A-Z]{3})\s+(?P<sep>vs\.|@)\s+(?P<opp>[A-Z]{3})$")


def parse_matchup(matchup: str) -> tuple[str | None, bool | None]:
    """"LAL vs. MIN" -> ("MIN", True)  (LAL is home);  "MIN @ LAL" -> ("LAL", False)."""
    m = MATCHUP_RE.match(matchup.strip())
    if not m:
        return None, None
    is_home = m.group("sep") == "vs."
    return m.group("opp"), is_home


def add_matchup_columns(df: pd.DataFrame) -> pd.DataFrame:
    parsed = df["matchup"].map(parse_matchup)
    df = df.copy()
    df["opp_abbr"] = [p[0] for p in parsed]
    df["is_home"] = [p[1] for p in parsed]
    return df


def add_team_context(df: pd.DataFrame) -> pd.DataFrame:
    """Adds team-level denominators (derived from the same table, no extra API calls) and
    each player's usage% for that game. Possession estimate is the standard single-team
    approximation (FGA - OREB + TOV + 0.44*FTA); it ignores the opponent cross-term the full
    two-team formula uses, which is a deliberate simplification, not an oversight."""
    df = df.copy()
    group = df.groupby(["game_id", "team_id"])
    team_min = group["min"].transform("sum")
    team_fga = group["fga"].transform("sum")
    team_fta = group["fta"].transform("sum")
    team_tov = group["tov"].transform("sum")
    team_oreb = group["oreb"].transform("sum")
    team_pts = group["pts"].transform("sum")

    game_duration = team_min / 5.0
    usg_denom = team_fga + 0.44 * team_fta + team_tov
    usage_pct = np.where(
        (df["min"] > 0) & (usg_denom > 0),
        100.0 * (df["fga"] + 0.44 * df["fta"] + df["tov"]) * game_duration / (df["min"] * usg_denom),
        np.nan,
    )

    df["team_min"] = team_min
    df["team_poss_est"] = team_fga - team_oreb + team_tov + 0.44 * team_fta
    df["team_pts"] = team_pts
    df["usage_pct"] = usage_pct
    return df


def build_canonical_player_games(raw_game_log: pd.DataFrame) -> pd.DataFrame:
    df = raw_game_log.rename(columns={c: c.lower() for c in raw_game_log.columns})
    df = add_matchup_columns(df)
    df = add_team_context(df)
    keep = [
        "season", "game_id", "game_date", "team_id", "team_abbreviation", "opp_abbr", "is_home",
        "player_id", "player_name", "min", "pts", "reb", "oreb", "dreb", "ast", "stl", "blk",
        "tov", "pf", "fgm", "fga", "fg3m", "fg3a", "ftm", "fta", "plus_minus",
        "team_min", "team_poss_est", "team_pts", "usage_pct",
    ]
    return df[[c for c in keep if c in df.columns]]
