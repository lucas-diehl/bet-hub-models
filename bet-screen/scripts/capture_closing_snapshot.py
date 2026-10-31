"""Captures the "closing" half of a forward CLV pair, for EVERY prop on tonight's board --
not just the ~15 picks live_fetch_props_slate.py posts. Run once, late in the day (near most
tip-offs), via its own cron entry. Pairs with that script's "morning" snapshot
(data/props_snapshots/morning_<date>.csv, written at ~9:23 ET) at grade time the next day,
once real outcomes are available too, to build an accumulating CLV log
(scripts/compute_clv.py, data/clv_log.csv) -- free, ongoing, the actual answer to whether
CLV predicts anything here (reports/_clv_test.py's retrospective 30-date look came back
inconclusive, not a confident null; this is what resolves it for real).

    python scripts/capture_closing_snapshot.py
    python scripts/capture_closing_snapshot.py --dry-run
"""

from __future__ import annotations

import argparse
import sys
import time
from datetime import datetime, timezone
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))

from nba_model.market import best_price, fetch_live_game_lines, fetch_live_props_for_event  # noqa: E402

ROOT = Path(__file__).resolve().parents[1]
SNAPSHOT_DIR = ROOT / "data" / "props_snapshots"


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--dry-run", action="store_true", help="fetch and print a summary, write nothing")
    args = ap.parse_args()

    as_of_date = datetime.now(timezone.utc).strftime("%Y-%m-%d")
    out_path = SNAPSHOT_DIR / f"closing_{as_of_date}.csv"
    if out_path.exists():
        print(f"{out_path} already exists -- already captured today, nothing to do")
        return

    lines = fetch_live_game_lines(as_of_date)
    print(f"{len(lines)} games with a real spread for {as_of_date}")
    if lines.empty:
        print("no games today -- nothing to capture")
        return

    all_rows = []
    for _, game in lines.iterrows():
        rows = fetch_live_props_for_event(game["event_id"], game["commence_time"])
        all_rows.extend(rows)
        print(f"  {game['home_team']} vs {game['away_team']}: {len(rows)} quotes")
        time.sleep(0.3)

    props = best_price(all_rows)
    print(f"\n{len(props)} (player, market, side) props captured")
    if args.dry_run or props.empty:
        if props.empty:
            print("no props found -- nothing written")
        return

    SNAPSHOT_DIR.mkdir(parents=True, exist_ok=True)
    props.assign(snapshot="closing", captured_at=datetime.now(timezone.utc).isoformat()) \
        .to_csv(out_path, index=False)
    print(f"wrote {out_path}")


if __name__ == "__main__":
    main()
