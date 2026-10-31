"""The Odds API client for NBA player props. Same account/auth pattern as
cbb/scripts/download_multibook.py and live_fetch_slate.py (same vendor covers both sports on
one key) -- ported here rather than imported since this is a different package.
"""

from __future__ import annotations

import os
from pathlib import Path

import pandas as pd
import requests

SECRETS = Path(r"C:\Users\ljdie\OneDrive\Documents\bet-screen\secrets.env")
SPORT_KEY = "basketball_nba"
BASE = "https://api.the-odds-api.com/v4"
REQUIRED_BOOKS = ["draftkings", "fanduel", "betmgm", "williamhill_us"]  # williamhill_us = Caesars
PROP_MARKETS = [
    "player_points", "player_rebounds", "player_assists", "player_threes",
    "player_points_rebounds_assists",
]


def american_to_implied_prob(price: float) -> float:
    """Raw (vig-included) implied win probability from an American odds price."""
    return 100 / (price + 100) if price > 0 else -price / (-price + 100)


def devig_two_way(price_a: float, price_b: float) -> tuple[float, float]:
    """Removes the vig from a two-sided market by normalizing both sides' raw implied
    probabilities to sum to 1 (the standard, "multiplicative" devig -- same approach
    the research project's calibration.py used, re-implemented here since that module
    was never ported into the live pipeline). Returns (fair_prob_a, fair_prob_b)."""
    raw_a, raw_b = american_to_implied_prob(price_a), american_to_implied_prob(price_b)
    total = raw_a + raw_b
    return raw_a / total, raw_b / total


def odds_api_key() -> str:
    if os.getenv("ODDS_API_KEY"):
        return os.environ["ODDS_API_KEY"]
    for line in SECRETS.read_text(encoding="utf-8-sig").splitlines():
        if line.strip().startswith("ODDS_API_KEY"):
            return line.split("=", 1)[1].strip().strip("\"'")
    raise SystemExit("ODDS_API_KEY not found")


def list_sports() -> tuple[list[dict], dict[str, str]]:
    """Free call (0 credits) -- confirms the account covers basketball_nba and reports
    remaining quota via response headers."""
    resp = requests.get(f"{BASE}/sports", params={"apiKey": odds_api_key()}, timeout=30)
    resp.raise_for_status()
    quota = {k: v for k, v in resp.headers.items() if k.lower().startswith("x-requests")}
    return resp.json(), quota


def load_game_lines(path: str) -> pd.DataFrame:
    """Loads the bulk spreads+totals pull (download_game_lines.py) into one row per real
    game: the consensus (median across books) home spread and total, taken from whichever
    sampled query-date's snapshot was CLOSEST to the game's actual tip-off.

    The raw CSV is NOT one row per game -- the historical bulk odds snapshot at any query
    date returns every event posted on the board at that moment, including games weeks out
    (same contamination confirmed in download_props_pilot.py's events-list endpoint). A game
    commencing in December can appear under an October query date with an early, stale
    number. Picking the snapshot with the latest `date` that is still <= the game's own date
    (derived from `commence_time`, not the query `date` column) gets the closest-to-tipoff
    number available within whatever budget-limited date range was actually pulled.
    """
    df = pd.read_csv(path)
    df["commence_time"] = pd.to_datetime(df["commence_time"])
    # Same evening-UTC-rollover convention as the rest of this project: the real US game
    # date is commence_time minus a few hours, not the raw UTC calendar date.
    df["game_date"] = (df["commence_time"] - pd.Timedelta(hours=8)).dt.strftime("%Y-%m-%d")
    df = df[df["date"] <= df["game_date"]]

    idx = df.groupby(["event_id", "book", "market", "name"])["date"].idxmax()
    closest = df.loc[idx]

    spreads = closest[(closest["market"] == "spreads") & (closest["name"] == closest["home_team"])]
    totals = closest[(closest["market"] == "totals") & (closest["name"] == "Over")]

    home_spread = spreads.groupby("event_id")["point"].median().rename("home_spread")
    total_points = totals.groupby("event_id")["point"].median().rename("total_points")
    meta = closest.groupby("event_id")[["game_date", "home_team", "away_team"]].first()

    out = meta.join(home_spread).join(total_points).reset_index()
    out["margin_abs_implied"] = out["home_spread"].abs()
    return out


def list_events(date_iso: str | None = None) -> list[dict]:
    """Free call -- lists today's (or a given date's) NBA events and their event IDs, which
    the per-event historical props endpoint requires."""
    params = {"apiKey": odds_api_key()}
    path = f"{BASE}/sports/{SPORT_KEY}/events"
    if date_iso:
        path = f"{BASE}/historical/sports/{SPORT_KEY}/events"
        params["date"] = date_iso
    resp = requests.get(path, params=params, timeout=30)
    resp.raise_for_status()
    return resp.json() if not date_iso else resp.json()["data"]


# Live (non-historical) board fetchers -- shared by live_fetch_props_slate.py (the picks
# generator) and scripts/capture_closing_snapshot.py (the CLV forward-capture, added
# 2026-10-31). Moved here from live_fetch_props_slate.py so neither script duplicates the
# other's fetch logic; a "morning" and a "closing" snapshot are necessarily two separate
# script runs (can't capture near-tipoff data in the morning), but they should hit the
# exact same endpoints the exact same way.
LIVE_BOOKS = ["draftkings", "fanduel"]


def fetch_live_game_lines(as_of_date: str) -> pd.DataFrame:
    """Today's spread per team, live (non-historical) endpoint. Filtered to games actually
    commencing on as_of_date (US-evening-rollover window), same contamination fix as
    download_game_lines.py's historical version -- the live board also includes upcoming
    games days out."""
    resp = requests.get(
        f"{BASE}/sports/{SPORT_KEY}/odds",
        params={"apiKey": odds_api_key(), "regions": "us", "markets": "spreads", "oddsFormat": "american"},
        timeout=60,
    )
    resp.raise_for_status()
    window_start = pd.Timestamp(f"{as_of_date}T10:00:00Z")
    window_end = window_start + pd.Timedelta(hours=24)
    rows = []
    for event in resp.json():
        commence = pd.Timestamp(event["commence_time"])
        if not (window_start <= commence < window_end):
            continue
        home, away = event["home_team"], event["away_team"]
        spreads = [m for b in event.get("bookmakers", []) for m in b.get("markets", []) if m["key"] == "spreads"]
        if not spreads:
            continue
        home_pts = [o["point"] for m in spreads for o in m["outcomes"] if o["name"] == home]
        if not home_pts:
            continue
        home_spread = pd.Series(home_pts).median()
        rows.append({"event_id": event["id"], "home_team": home, "away_team": away,
                     "commence_time": event["commence_time"], "home_spread": home_spread})
    return pd.DataFrame(rows)


def fetch_live_props_for_event(event_id: str, commence_time: str | None = None) -> list[dict]:
    key = odds_api_key()
    resp = requests.get(
        f"{BASE}/sports/{SPORT_KEY}/events/{event_id}/odds",
        params={"apiKey": key, "regions": "us", "markets": ",".join(PROP_MARKETS), "oddsFormat": "american"},
        timeout=60,
    )
    resp.raise_for_status()
    payload = resp.json()
    rows = []
    for book in payload.get("bookmakers", []):
        if book["key"] not in LIVE_BOOKS:
            continue
        for market in book.get("markets", []):
            for outcome in market["outcomes"]:
                rows.append({
                    "event_id": event_id, "home_team": payload.get("home_team"),
                    "away_team": payload.get("away_team"), "commence_time": commence_time,
                    "book": book["key"],
                    "market": market["key"], "player": outcome.get("description"),
                    "side": outcome.get("name"), "point": outcome.get("point"),
                    "price": outcome.get("price"),
                })
    return rows


def best_price(rows: list[dict]) -> pd.DataFrame:
    df = pd.DataFrame(rows)
    if df.empty:
        return df
    idx = df.groupby(["player", "market", "side"])["price"].idxmax()
    return df.loc[idx]
