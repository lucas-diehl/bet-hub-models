"""ESPN's public (free, no-key) box-score API, used ONLY for grading -- fetching the
PREVIOUS day's final box scores to settle yesterday's posted picks.

Added 2026-10-21 to replace a stats.nba.com call in grade_props_slate.py that was
failing consistently (2/2 real runs, same `requests.exceptions.ReadTimeout` after 6
retries each) specifically from GitHub Actions runners. stats.nba.com is well known
to throttle/hang on traffic from cloud-provider IP ranges rather than return a clean
error, which no amount of retry/backoff tuning fixes (confirmed: a hand-retry minutes
later failed identically). ESPN's site.api.espn.com does NOT have this problem -- this
exact codebase already proves it works from these same GitHub Actions runners
(dfs-engine/spine/R/vegas.R, injuries.R, both scheduled via .github/workflows/dfs.yml)
-- it just needs a non-default User-Agent header, confirmed live (both from a local
machine and matching the already-working R pattern) before writing this.

Deliberately NOT used for download_boxscores.py's full-season backfill (that stays on
stats.nba.com/nba_api, which IS what minutes.py/segments.py were built and validated
against) -- this is a narrow, single-day, grading-only substitute, not a wholesale
swap of the project's data source.
"""

from __future__ import annotations

import pandas as pd
import requests

from .names import normalize_name

BASE = "https://site.api.espn.com/apis/site/v2/sports/basketball/nba"
HEADERS = {"User-Agent": "bet-screen/1.0"}
TIMEOUT = 20


def _stat_int(stats: list[str], idx: dict[str, int], label: str) -> int | None:
    """Plain counting stats (PTS/REB/AST) are a bare integer string. A DNP/inactive row's
    `stats` list is short or non-numeric ("DNP", "--") -- return None rather than raise,
    so one bad row doesn't take down the whole date's grading."""
    i = idx.get(label)
    if i is None or i >= len(stats):
        return None
    try:
        return int(stats[i])
    except (TypeError, ValueError):
        return None


def _stat_made(stats: list[str], idx: dict[str, int], label: str) -> int | None:
    """Shooting splits ("3PT") come back as "made-attempted", e.g. "7-12" -- only the
    made count is needed for grading a makes-based prop line."""
    i = idx.get(label)
    if i is None or i >= len(stats):
        return None
    try:
        return int(str(stats[i]).split("-")[0])
    except (ValueError, IndexError):
        return None


def fetch_boxscore_actuals(date_iso: str) -> pd.DataFrame:
    """Every player's final PTS/REB/AST/FG3M for every NBA game on date_iso (YYYY-MM-DD).
    Returns columns matching what grade_props_slate.py's STAT_COL/actual_pra already
    expect: player_norm, PTS, REB, AST, FG3M, actual_pra. Empty DataFrame (not an
    exception) if ESPN has nothing for that date -- same "nothing to grade" contract
    grade_props_slate.py already handles for an empty actuals table.
    """
    resp = requests.get(f"{BASE}/scoreboard", params={"dates": date_iso.replace("-", "")},
                        headers=HEADERS, timeout=TIMEOUT)
    resp.raise_for_status()
    events = resp.json().get("events", [])

    rows: list[dict] = []
    for ev in events:
        event_id = ev.get("id")
        if not event_id:
            continue
        r = requests.get(f"{BASE}/summary", params={"event": event_id}, headers=HEADERS, timeout=TIMEOUT)
        if r.status_code != 200:
            continue  # one game's summary failing shouldn't drop every other game's actuals
        boxscore = r.json().get("boxscore", {})
        for team in boxscore.get("players", []):
            for stat_group in team.get("statistics", []):
                labels = stat_group.get("labels") or stat_group.get("names") or []
                idx = {lab: i for i, lab in enumerate(labels)}
                for athlete in stat_group.get("athletes", []):
                    stats = athlete.get("stats") or []
                    name = (athlete.get("athlete") or {}).get("displayName")
                    if not name:
                        continue
                    pts, reb, ast = _stat_int(stats, idx, "PTS"), _stat_int(stats, idx, "REB"), _stat_int(stats, idx, "AST")
                    fg3m = _stat_made(stats, idx, "3PT")
                    if pts is None and reb is None and ast is None:
                        continue  # DNP/inactive row
                    rows.append({
                        "PLAYER_NAME": name,
                        "PTS": pts or 0, "REB": reb or 0, "AST": ast or 0, "FG3M": fg3m or 0,
                    })

    df = pd.DataFrame(rows, columns=["PLAYER_NAME", "PTS", "REB", "AST", "FG3M"])
    if df.empty:
        return df
    df["player_norm"] = df["PLAYER_NAME"].map(normalize_name)
    df["actual_pra"] = df["PTS"] + df["REB"] + df["AST"]
    return df
