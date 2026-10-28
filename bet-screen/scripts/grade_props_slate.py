"""Grades yesterday's posted picks against real results and writes a contract-compliant
results_<date>.json to $FEED_DIR for the shared ingest step to upsert into Supabase (joins on
bet_id, which must come from the archived picks file -- never recomputed independently, so it
can never drift from what was actually posted). Scheduled to run once daily at 8am ET, i.e.
after the prior day's games are long final but before that same day's new 10am picks post --
grades day N-1 the morning of day N.

Also appends graded_date's games to data/processed/player_games_<season>.csv, unconditionally
and before grading even starts (2026-10-27): download_boxscores.py's daily --refresh of that
file, meant to keep the live minutes EWMA current, failed 100% of runs since launch (same
stats.nba.com GitHub-Actions ReadTimeout that forced fetch_actuals onto ESPN below) -- so the
live pipeline was silently scoring every game off a never-updated snapshot. This reuses the
already-proven-reliable ESPN source to close that gap. Unconditional because real games
happen on graded_date regardless of whether any picks were posted for it.

    python scripts/grade_props_slate.py --date 2026-10-20
    python scripts/grade_props_slate.py                 # defaults to yesterday (UTC)
"""

from __future__ import annotations

import argparse
import json
import os
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))

from nba_model.espn import fetch_boxscore_actuals, fetch_canonical_player_games  # noqa: E402
from nba_model.grading import american_profit  # noqa: E402
from nba_model.names import normalize_name  # noqa: E402

ROOT = Path(__file__).resolve().parents[1]
ARCHIVE_DIR = ROOT / "data" / "picks_archive"
PROCESSED_DIR = ROOT / "data" / "processed"
FEED_DIR = Path(os.environ.get("FEED_DIR", ROOT / "_feed"))
CONTRACT_VERSION = "1.0"
SOURCE = "bet-screen"
SPORT = "nba"


def season_for(d) -> int:
    return d.year + 1 if d.month >= 10 else d.year


def update_player_games(graded_date: str) -> None:
    """Appends graded_date's games to player_games_<season>.csv (de-duped on player_norm +
    game_date, so a re-run of the same date never double-counts). Never raises -- a failure
    here should not take down grading/ingest, which is the part that actually has to run."""
    try:
        season = season_for(datetime.strptime(graded_date, "%Y-%m-%d").date())
        new_rows = fetch_canonical_player_games(graded_date, season)
        if new_rows.empty:
            print(f"no ESPN box scores found for {graded_date} -- player_games_{season}.csv unchanged")
            return
        new_rows["player_norm"] = new_rows["player_name"].map(normalize_name)

        path = PROCESSED_DIR / f"player_games_{season}.csv"
        if path.exists():
            existing = pd.read_csv(path)
            existing["player_norm"] = existing["player_name"].map(normalize_name)
            existing = existing[existing["game_date"].astype(str) != graded_date]
            combined = pd.concat([existing, new_rows], ignore_index=True)
        else:
            combined = new_rows
        combined.drop(columns=["player_norm"]).to_csv(path, index=False)
        print(f"appended {len(new_rows)} player-games for {graded_date} to {path}")
    except Exception as exc:  # noqa: BLE001
        print(f"player_games update failed for {graded_date} (non-fatal): {exc}")

STAT_COL = {
    "Points": "PTS", "Rebounds": "REB", "Assists": "AST", "3PM": "FG3M",
}


def fetch_actuals(graded_date: str) -> pd.DataFrame:
    """Pulls the actual box score for graded_date. Was a direct stats.nba.com call (free,
    no credits) -- switched 2026-10-21 to ESPN's site API (nba_model.espn) after
    stats.nba.com consistently timed out from GitHub Actions runners (2/2 real runs, same
    error); see espn.py's module docstring for the full story. normalize_name/column
    shape (PLAYER_NAME, PTS, REB, AST, FG3M, actual_pra, player_norm) is unchanged, so
    grade_bet() below needed no changes."""
    return fetch_boxscore_actuals(graded_date)


def grade_bet(bet: dict, actuals: pd.DataFrame) -> dict:
    player_norm = normalize_name(bet["selection"])
    row = actuals[actuals["player_norm"] == player_norm]
    if row.empty:
        return {"bet_id": bet["bet_id"], "result": "void"}

    stat_label = bet["market_label"].rsplit(" ", 1)[0]  # "Points Under" -> "Points"
    stat_col = "actual_pra" if stat_label == "PRA" else STAT_COL[stat_label]
    actual = float(row.iloc[0][stat_col])
    line = bet["line"]

    if actual == line:
        return {"bet_id": bet["bet_id"], "result": "push", "pnl_units": 0.0, "actual": {"value": actual}}

    won = (actual > line) if bet["side"] == "over" else (actual < line)
    pnl = american_profit(bet["odds_american"], won) * bet["stake_units"]
    return {
        "bet_id": bet["bet_id"],
        "result": "win" if won else "loss",
        "pnl_units": pnl,
        "actual": {"value": actual},
    }


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--date", type=str, default=None, help="date to grade (YYYY-MM-DD), default yesterday UTC")
    args = ap.parse_args()

    graded_date = args.date or (datetime.now(timezone.utc) - timedelta(days=1)).strftime("%Y-%m-%d")

    # Unconditional and first: real games happened on graded_date regardless of whether any
    # picks were posted for it, so the minutes EWMA's refresh can't be gated on picks existing.
    update_player_games(graded_date)

    picks_path = ARCHIVE_DIR / f"picks_{graded_date}.json"
    if not picks_path.exists():
        print(f"no picks archived for {graded_date} ({picks_path}) -- nothing to grade")
        return

    picks_file = json.loads(picks_path.read_text())
    bets = picks_file["bets"]
    print(f"grading {len(bets)} bets from {graded_date}")

    actuals = fetch_actuals(graded_date)
    print(f"found {len(actuals)} player box scores for {graded_date}")

    results = [grade_bet(b, actuals) for b in bets]
    wins = sum(1 for r in results if r["result"] == "win")
    losses = sum(1 for r in results if r["result"] == "loss")
    pushes = sum(1 for r in results if r["result"] == "push")
    voids = sum(1 for r in results if r["result"] == "void")
    units = sum(r.get("pnl_units") or 0.0 for r in results)
    print(f"W-L-P: {wins}-{losses}-{pushes}  (void: {voids})  units: {units:+.2f}u")

    payload = {
        "contract_version": CONTRACT_VERSION,
        "source": SOURCE,
        "sport": SPORT,
        "slate_date": graded_date,
        "graded_at": datetime.now(timezone.utc).isoformat(),
        "results": results,
    }
    FEED_DIR.mkdir(parents=True, exist_ok=True)
    (FEED_DIR / f"results_{graded_date}.json").write_text(json.dumps(payload, indent=2))
    print(f"wrote {FEED_DIR / f'results_{graded_date}.json'}")


if __name__ == "__main__":
    main()
