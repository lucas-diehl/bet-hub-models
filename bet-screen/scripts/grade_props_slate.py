"""Grades yesterday's posted picks against real results and writes a contract-compliant
results_<date>.json to $FEED_DIR for the shared ingest step to upsert into Supabase (joins on
bet_id, which must come from the archived picks file -- never recomputed independently, so it
can never drift from what was actually posted). Scheduled to run once daily at 8am ET, i.e.
after the prior day's games are long final but before that same day's new 10am picks post --
grades day N-1 the morning of day N.

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

from nba_model.espn import fetch_boxscore_actuals  # noqa: E402
from nba_model.grading import american_profit  # noqa: E402
from nba_model.names import normalize_name  # noqa: E402

ROOT = Path(__file__).resolve().parents[1]
ARCHIVE_DIR = ROOT / "data" / "picks_archive"
FEED_DIR = Path(os.environ.get("FEED_DIR", ROOT / "_feed"))
CONTRACT_VERSION = "1.0"
SOURCE = "bet-screen"
SPORT = "nba"

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
