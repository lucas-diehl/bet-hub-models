"""Backfill player-game box scores + per-game inactive lists for the NBA minutes/usage model.

Two passes per season:
  1. One call to the season player game log (leaguegamelog) -- cheap, gives every played game.
  2. One call per unique game_id to boxscoresummaryv3 for the inactive-player list -- this is
     the expensive pass (~1,230 calls/season), manifest-resumable the same way
     cbb/scripts/download_pbp.py resumes per-date PBP pulls, because stats.nba.com has no
     documented request quota but does rate-limit / occasionally return empty bodies.

Usage:
    python scripts/download_boxscores.py --seasons 2024 2025 2026
    python scripts/download_boxscores.py --seasons 2026 --skip-inactive   # fast dry run
"""

from __future__ import annotations

import argparse
import sys
import time
from pathlib import Path

import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))

from nba_model.api import NBAApiError, NBAStatsClient  # noqa: E402
from nba_model.data import build_canonical_player_games  # noqa: E402

ROOT = Path(__file__).resolve().parents[1]
RAW_DIR = ROOT / "data" / "raw"
PROCESSED_DIR = ROOT / "data" / "processed"
RAW_DIR.mkdir(parents=True, exist_ok=True)
PROCESSED_DIR.mkdir(parents=True, exist_ok=True)


def manifest_path(season: int) -> Path:
    return RAW_DIR / f"_manifest_inactive_{season}.txt"


def load_manifest(season: int) -> set[str]:
    path = manifest_path(season)
    if not path.exists():
        return set()
    return set(path.read_text().splitlines())


def append_manifest(season: int, game_id: int | str) -> None:
    with manifest_path(season).open("a") as f:
        f.write(str(game_id) + "\n")


def fetch_game_log(client: NBAStatsClient, season: int, refresh: bool) -> pd.DataFrame:
    cache = RAW_DIR / f"player_game_log_{season}.csv"
    if cache.exists() and not refresh:
        return pd.read_csv(cache)
    df = client.get_player_game_log(season)
    df.to_csv(cache, index=False)
    return df


def fetch_inactive_log(client: NBAStatsClient, season: int, game_ids: list[str], sleep: float) -> pd.DataFrame:
    cache = RAW_DIR / f"inactive_log_{season}.csv"
    done = load_manifest(season)
    rows: list[pd.DataFrame] = []
    if cache.exists():
        rows.append(pd.read_csv(cache))
    todo = [g for g in game_ids if str(g) not in done]
    print(f"  inactive list: {len(done)} cached, {len(todo)} to fetch")
    for i, game_id in enumerate(todo):
        try:
            inactive = client.get_inactive_players(str(game_id).zfill(10))
        except NBAApiError as exc:
            print(f"    [{i+1}/{len(todo)}] FAILED {game_id}: {exc}")
            continue
        rows.append(inactive)
        append_manifest(season, game_id)
        if (i + 1) % 50 == 0:
            print(f"    [{i+1}/{len(todo)}] fetched through {game_id}")
        time.sleep(sleep)
    combined = pd.concat(rows, ignore_index=True) if rows else pd.DataFrame()
    combined.to_csv(cache, index=False)
    return combined


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--seasons", type=int, nargs="+", required=True)
    ap.add_argument("--skip-inactive", action="store_true", help="skip the per-game inactive-list pass")
    ap.add_argument("--refresh", action="store_true", help="refetch game logs even if cached")
    ap.add_argument("--sleep", type=float, default=0.6, help="seconds between inactive-list calls")
    args = ap.parse_args()

    client = NBAStatsClient()
    all_canonical = []

    for season in args.seasons:
        print(f"season {season} ({season - 1}-{str(season)[-2:]})")
        raw_log = fetch_game_log(client, season, args.refresh)
        print(f"  player game log: {len(raw_log)} rows, {raw_log['GAME_ID'].nunique()} games")

        canonical = build_canonical_player_games(raw_log)
        canonical.to_csv(PROCESSED_DIR / f"player_games_{season}.csv", index=False)
        all_canonical.append(canonical)

        if not args.skip_inactive:
            game_ids = sorted(raw_log["GAME_ID"].unique().tolist())
            fetch_inactive_log(client, season, game_ids, args.sleep)

    merged = pd.concat(all_canonical, ignore_index=True)
    merged.to_csv(PROCESSED_DIR / "player_games.csv", index=False)
    print(f"done: {len(merged)} total player-game rows across {len(args.seasons)} season(s)")


if __name__ == "__main__":
    main()
