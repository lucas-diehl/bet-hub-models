"""Matches a day's morning + closing props snapshots (live_fetch_props_slate.py's archive +
capture_closing_snapshot.py's, both already run that day) against real outcomes (same ESPN
source grade_props_slate.py already uses) and appends one row per (player, market, side) to
data/clv_log.csv -- an accumulating, free, ever-growing CLV dataset. Run at grade time the
next day (both snapshots and the real result all exist by then). Non-fatal if either
snapshot is missing (e.g. a day the closing capture failed) -- same "an optional step's
absence never blocks the pipeline" pattern as update_player_games() in
grade_props_slate.py.

    python scripts/compute_clv.py --date 2026-10-30
    python scripts/compute_clv.py                       # defaults to yesterday (UTC)
"""

from __future__ import annotations

import argparse
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))

from nba_model.espn import fetch_boxscore_actuals  # noqa: E402
from nba_model.market import american_to_implied_prob  # noqa: E402
from nba_model.names import normalize_name  # noqa: E402

ROOT = Path(__file__).resolve().parents[1]
SNAPSHOT_DIR = ROOT / "data" / "props_snapshots"
OUT = ROOT / "data" / "clv_log.csv"
MARKET_TO_COL = {
    "player_points": "PTS", "player_rebounds": "REB", "player_assists": "AST",
    "player_threes": "FG3M", "player_points_rebounds_assists": "actual_pra",
}


def pair_snapshot(df: pd.DataFrame) -> pd.DataFrame:
    """One row per (player, market, point): over and under best-price side by side, same
    pairing convention nba_model.grading.paired_lines uses for the historical data."""
    over = df[df["side"] == "Over"]
    under = df[df["side"] == "Under"]
    return over.merge(under, on=["player", "market", "point"], suffixes=("_over", "_under"))


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--date", type=str, default=None, help="date to compute CLV for, default yesterday (UTC)")
    args = ap.parse_args()

    graded_date = args.date or (datetime.now(timezone.utc) - timedelta(days=1)).strftime("%Y-%m-%d")

    morning_path = SNAPSHOT_DIR / f"morning_{graded_date}.csv"
    closing_path = SNAPSHOT_DIR / f"closing_{graded_date}.csv"
    if not morning_path.exists() or not closing_path.exists():
        print(f"missing snapshot(s) for {graded_date} (morning={morning_path.exists()}, "
              f"closing={closing_path.exists()}) -- skipping, non-fatal")
        return

    morning = pair_snapshot(pd.read_csv(morning_path))
    closing = pair_snapshot(pd.read_csv(closing_path))
    merged = morning.merge(closing, on=["player", "market", "point"], suffixes=("_open", "_close"))
    print(f"{graded_date}: {len(merged)} (player, market, point) props with both open and close")
    if merged.empty:
        return

    # market.devig_two_way takes raw prices and is scalar-only (its one existing call site,
    # live_fetch_props_slate.py, calls it per-row); here we already have per-row
    # probabilities via .map(), so just do the same normalize-to-sum-1 devig directly --
    # pure elementwise arithmetic, safe on numpy arrays.
    p_over_open = merged["price_over_open"].map(american_to_implied_prob).to_numpy()
    p_under_open = merged["price_under_open"].map(american_to_implied_prob).to_numpy()
    p_over_close = merged["price_over_close"].map(american_to_implied_prob).to_numpy()
    p_under_close = merged["price_under_close"].map(american_to_implied_prob).to_numpy()
    open_total = p_over_open + p_under_open
    close_total = p_over_close + p_under_close
    over_open_dv, under_open_dv = p_over_open / open_total, p_under_open / open_total
    over_close_dv, under_close_dv = p_over_close / close_total, p_under_close / close_total

    actuals = fetch_boxscore_actuals(graded_date)
    print(f"found {len(actuals)} real player box scores for {graded_date}")
    if actuals.empty:
        print("no real outcomes yet -- skipping, non-fatal (try again once ESPN has results)")
        return
    actuals = actuals.set_index("player_norm")
    merged["player_norm"] = merged["player"].map(normalize_name)

    rows = []
    for side, col_prefix, dv_open, dv_close in [
        ("Over", "over", over_open_dv, over_close_dv),
        ("Under", "under", under_open_dv, under_close_dv),
    ]:
        for idx in range(len(merged)):
            r = merged.iloc[idx]
            stat_col = MARKET_TO_COL.get(r["market"])
            if stat_col is None or r["player_norm"] not in actuals.index:
                continue
            actual = actuals.loc[r["player_norm"], stat_col]
            if isinstance(actual, pd.Series):  # same name appeared twice that night -- skip, ambiguous
                continue
            if pd.isna(actual) or actual == r["point"]:
                continue
            won = (actual > r["point"]) if side == "Over" else (actual < r["point"])
            rows.append({
                "date": graded_date, "player": r["player"], "market": r["market"],
                "side": side, "point": r["point"],
                "price_open": r[f"price_{col_prefix}_open"], "price_close": r[f"price_{col_prefix}_close"],
                "clv": float(dv_close[idx] - dv_open[idx]),
                "actual": float(actual), "won": bool(won),
            })

    if not rows:
        print("no rows matched to a real outcome -- nothing to log")
        return
    out_df = pd.DataFrame(rows)
    OUT.parent.mkdir(parents=True, exist_ok=True)
    out_df.to_csv(OUT, mode="a", header=not OUT.exists(), index=False)
    print(f"appended {len(out_df)} rows to {OUT} "
          f"(win_rate={out_df['won'].mean():.1%}, mean_clv={out_df['clv'].mean():+.4f})")


if __name__ == "__main__":
    main()
