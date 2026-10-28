"""Daily live pick generator for the 5 segments in nba_model/segments.py -- the Bet Hub feed
for source "bet-screen" (sport "nba"). Pulls today's live spread (for game-script
classification) and player props (points/rebounds/assists/threes/PRA) across
DraftKings/FanDuel, classifies each player's role from their EWMA minutes (carried over
across the season boundary from last season, since a fresh season starts with ~0 games of
its own history), matches against the 5 segments, and writes a contract-compliant
picks_<date>.json to $FEED_DIR for the shared ingest step (checkout bet-dashboard -> pnpm
ingest -> Supabase). Also archives the same payload under data/picks_archive/ (persisted via
actions/cache across runs) so grade_props_slate.py can grade it the next day without needing
its own Supabase read.

Each matched bet also carries `model_prob` / `market_prob` / `edge`, same fields the other
sports' BetSchema rows use (2026-10-21, owner request: show the important context on a prop,
not just the side and the price). This system doesn't fit a per-player probability -- it bets
a fixed, backtested historical bucket (nba_model.segments.Segment) -- so `model_prob` is
honestly that bucket's own realized win rate (`backtest_win_pct`), not a fabricated live
figure. `market_prob` is the real two-sided de-vigged price (nba_model.market.devig_two_way),
computed from whichever book quotes BOTH sides; left null if only one side is quoted.

At most MAX_DAILY_PICKS bets are posted a day. The owner's own framing: "post a maximum of
15 plays a day (have the 15 best posted, but it is not by edge%)" -- edge% is a live-market
quantity (how far today's specific price has drifted from fair), not a measure of which
BUCKET is more trustworthy, so ranking by it would just reward a mispriced book, not pick the
best bets. Instead this ranks by the segment's own backtested confidence: the 3 CONFIRMED
segments (P(ROI<=0) < 0.05) before the 2 experimental ones, and within each tier, lowest
backtest_p_le0 (most statistically confident) first.

    python scripts/live_fetch_props_slate.py --dry-run              # live odds, no write
    python scripts/live_fetch_props_slate.py --as-of 2026-01-15 --dry-run   # cached replay, zero cost
"""

from __future__ import annotations

import argparse
import json
import os
import re
import sys
import time
from datetime import date, datetime, timezone
from pathlib import Path

import pandas as pd
import requests

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))

from nba_model.api import NBAStatsClient  # noqa: E402
from nba_model.market import BASE, PROP_MARKETS, SPORT_KEY, devig_two_way, odds_api_key  # noqa: E402
from nba_model.minutes import ewma_halflife_alpha  # noqa: E402
from nba_model.names import normalize_name  # noqa: E402
from nba_model.segments import classify_role, classify_script, matching_segment  # noqa: E402

ROOT = Path(__file__).resolve().parents[1]
MAX_DAILY_PICKS = 15
# bet365 was never integrated (would need a different Odds API region); betmgm is not one of
# the sanctioned books in the bet-dashboard contract (FanDuel | DraftKings | bet365 only).
LIVE_BOOKS = ["draftkings", "fanduel"]
BOOK_LABEL = {"draftkings": "DraftKings", "fanduel": "FanDuel"}
MARKET_LABEL = {
    "player_points": "Points", "player_rebounds": "Rebounds", "player_assists": "Assists",
    "player_threes": "3PM", "player_points_rebounds_assists": "PRA",
}
CONTRACT_VERSION = "1.0"
SOURCE = "bet-screen"
SPORT = "nba"
FEED_DIR = Path(os.environ.get("FEED_DIR", ROOT / "_feed"))
ARCHIVE_DIR = ROOT / "data" / "picks_archive"


def season_for(d: date) -> int:
    return d.year + 1 if d.month >= 10 else d.year  # NBA season label = the year it ends in


def current_minutes_estimate(as_of_date: str, halflife: float = 8.0) -> pd.DataFrame:
    """Each player's CURRENT minutes EWMA -- i.e. including their most recent played game,
    not shifted -- carried continuously across the season boundary. A fresh season starts
    with ~0 games of its own, so without this every player would show up as a leak-safe-NaN
    'bench' by default for the first few weeks, which is wrong, not just imprecise.

    Groups by player_norm, not player_id: the current season's rows now come from ESPN
    (grade_props_slate.py's daily append, since stats.nba.com reliably times out from
    GitHub Actions -- see nba_model/espn.py) while prior seasons' rows are stats.nba.com-
    sourced. The two use different, incompatible player-id numbering schemes; player_norm
    is the one identity that's stable across both and already the real join key everything
    downstream uses anyway."""
    season = season_for(datetime.strptime(as_of_date, "%Y-%m-%d").date())
    frames = []
    for s in [season - 1, season]:
        p = ROOT / "data" / "processed" / f"player_games_{s}.csv"
        if p.exists():
            frames.append(pd.read_csv(p))
    if not frames:
        raise SystemExit(f"no cached player_games for season {season - 1} or {season}")
    pg = pd.concat(frames, ignore_index=True)
    pg["game_date"] = pg["game_date"].astype(str)
    pg = pg[pg["game_date"] < as_of_date]  # strictly before -- leak safety even in live mode
    pg["player_norm"] = pg["player_name"].map(normalize_name)
    pg = pg.sort_values(["player_norm", "game_date"])

    alpha = ewma_halflife_alpha(halflife)
    current_ewma = pg.groupby("player_norm")["min"].apply(lambda s: s.ewm(alpha=alpha, min_periods=1).mean().iloc[-1])
    out = current_ewma.rename("min_ewma_now").reset_index()
    return out[["player_norm", "min_ewma_now"]]


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


def load_cached_replay(as_of_date: str) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Zero-cost testing path: replay an already-cached historical date from
    game_lines.csv + props_pilot.csv instead of hitting the live API."""
    from nba_model.market import load_game_lines
    lines = load_game_lines(ROOT / "data" / "oddsapi" / "game_lines.csv")
    lines = lines[lines["game_date"] == as_of_date]
    lines = lines.rename(columns={"margin_abs_implied": "spread_abs"})

    props = pd.read_csv(ROOT / "data" / "oddsapi" / "props_pilot.csv")
    props = props[(props["date"] == as_of_date) & (props["book"].isin(LIVE_BOOKS))]
    idx = props.groupby(["player", "market", "side"])["price"].idxmax()
    props = props.loc[idx]
    return lines, props


def slugify(text: str) -> str:
    return re.sub(r"[^a-z0-9]+", "-", str(text).lower()).strip("-")


def make_bet_id(as_of_date: str, player: str, market: str, side: str, point: float) -> str:
    point_slug = f"{point:g}".replace(".", "p").replace("-", "m")
    return f"{SOURCE}-{SPORT}-{as_of_date}-{slugify(player)}-{market}-{side.lower()}-{point_slug}"


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--as-of", type=str, default=None, help="replay a cached historical date, zero cost")
    ap.add_argument("--dry-run", action="store_true", help="print picks, don't write any file")
    args = ap.parse_args()

    as_of_date = args.as_of or datetime.now(timezone.utc).strftime("%Y-%m-%d")
    print(f"generating picks for {as_of_date} ({'cached replay' if args.as_of else 'LIVE'})")

    minutes = current_minutes_estimate(as_of_date)
    name_to_abbr = {t["full_name"]: t["abbreviation"] for t in NBAStatsClient.all_teams()}

    if args.as_of:
        lines, props = load_cached_replay(as_of_date)
        lines["home_abbr"] = lines["home_team"].map(name_to_abbr)
        lines["away_abbr"] = lines["away_team"].map(name_to_abbr)
        lines["commence_time"] = None
    else:
        lines = fetch_live_game_lines(as_of_date)
        lines["spread_abs"] = lines["home_spread"].abs()
        lines["home_abbr"] = lines["home_team"].map(name_to_abbr)
        lines["away_abbr"] = lines["away_team"].map(name_to_abbr)
        print(f"{len(lines)} games with real spread data for {as_of_date}")
        all_prop_rows = []
        for _, game in lines.iterrows():
            rows = fetch_live_props_for_event(game["event_id"], game["commence_time"])
            all_prop_rows.extend(rows)
            print(f"  {game['home_team']} vs {game['away_team']}: {len(rows)} quotes")
            time.sleep(0.3)
        props = best_price(all_prop_rows)

    # Only spread_abs comes from this join -- props already carries its own correct
    # event_id/home_team/away_team/commence_time natively (from the live API payload or
    # props_pilot.csv), so joining those in too would just collide with the existing columns
    # and get silently suffixed by pandas instead of overwriting them.
    team_spread = pd.concat([
        lines[["home_abbr"]].rename(columns={"home_abbr": "team_abbr"}).assign(spread_abs=lines["spread_abs"]),
        lines[["away_abbr"]].rename(columns={"away_abbr": "team_abbr"}).assign(spread_abs=lines["spread_abs"]),
    ], ignore_index=True)

    if props.empty:
        print("no props found")
        return

    props = props.copy()
    props["player_norm"] = props["player"].map(normalize_name)
    props = props.merge(minutes, on="player_norm", how="left")

    # Need each player's team to look up their game -- not directly in props rows, so match
    # via home/away team name + which roster the player is actually on this season.
    player_games = pd.read_csv(ROOT / "data" / "processed" / f"player_games_{season_for(datetime.strptime(as_of_date,'%Y-%m-%d').date())-1}.csv")
    player_games["player_norm"] = player_games["player_name"].map(normalize_name)
    last_team = player_games.sort_values("game_date").groupby("player_norm")["team_abbreviation"].last()
    props["team_abbr"] = props["player_norm"].map(last_team)
    props = props.merge(team_spread, on="team_abbr", how="left")

    props["role"] = props["min_ewma_now"].map(classify_role)
    props["script"] = props["spread_abs"].map(classify_script)

    # Opposing side's best price, for de-vigging -- e.g. a matched "Under" bet looks up
    # this same player/market's "Over" row. One row per (player, market, side) already,
    # courtesy of best_price()/load_cached_replay() above.
    opp_side = {"Over": "Under", "Under": "Over"}
    price_by_player_market_side = {
        (r["player"], r["market"], r["side"]): r["price"] for _, r in props.iterrows()
    }

    matched = []  # (bet dict, Segment) -- keeps the segment around for the rank/cap step below
    for _, row in props.iterrows():
        seg = matching_segment(row["market"], row["side"], row["role"], row["script"])
        if seg is None:
            continue
        book_key = row["book"]
        opp_price = price_by_player_market_side.get((row["player"], row["market"], opp_side[row["side"]]))
        market_prob = model_prob = edge = None
        if opp_price is not None:
            # devig_two_way(price_a, price_b) -> (fair_prob_a, fair_prob_b) -- keep whichever
            # slot corresponds to THIS row's own side, not just the first element.
            if row["side"] == "Over":
                fair_this_side, _ = devig_two_way(row["price"], opp_price)
            else:
                _, fair_this_side = devig_two_way(opp_price, row["price"])
            # float(): row["price"]/opp_price are numpy scalars off a DataFrame, which
            # json.dumps() can't serialize -- same reason "line"/"odds_american" below
            # are explicitly cast already.
            market_prob = float(fair_this_side)
            model_prob = seg.backtest_win_pct
            edge = float(model_prob - market_prob)
        bet = {
            "bet_id": make_bet_id(as_of_date, row["player"], row["market"], row["side"], row["point"]),
            "sport": SPORT,
            "event_start": row["commence_time"] if pd.notna(row.get("commence_time")) else None,
            "market": "prop",
            "market_label": f"{MARKET_LABEL.get(row['market'], row['market'])} {row['side']}",
            "selection": row["player"],
            "side": row["side"].lower(),
            "line": float(row["point"]),
            "odds_american": int(row["price"]),
            "book": BOOK_LABEL.get(book_key, book_key),
            "model_prob": model_prob,
            "market_prob": market_prob,
            "edge": edge,
            "stake_units": 1.0,
            "tags": [row["role"], row["script"]],
            "details": {
                "role": row["role"], "script": row["script"],
                "backtest_roi": seg.backtest_roi, "backtest_p_le0": seg.backtest_p_le0,
                "backtest_n": seg.backtest_n, "confirmed": seg.confirmed,
            },
        }
        if pd.notna(row.get("home_team")) and pd.notna(row.get("away_team")):
            bet["event"] = f"{row['away_team']} @ {row['home_team']}"
        matched.append((bet, seg))

    print(f"\n{len(matched)} bets matched")

    # Cap to MAX_DAILY_PICKS. Ranked by the segment's own backtested confidence -- NOT by
    # edge%, which is today's live mispricing on one specific player, not a measure of
    # which bucket is more trustworthy (see the module docstring). Confirmed segments
    # (P(ROI<=0) < 0.05) first, then lowest backtest_p_le0 first within each tier.
    matched.sort(key=lambda pair: (not pair[1].confirmed, pair[1].backtest_p_le0))
    dropped = len(matched) - MAX_DAILY_PICKS
    kept, cut = matched[:MAX_DAILY_PICKS], matched[MAX_DAILY_PICKS:]
    bets = [b for b, _ in kept]

    if dropped > 0:
        print(f"capped at {MAX_DAILY_PICKS}/day -- dropped {dropped} lower-confidence match(es):")
        for b, seg in cut:
            print(f"  (cut) {b['selection']} {b['side']} {b['line']} {b['market_label']} "
                  f"({'confirmed' if seg.confirmed else 'experimental'}, p_le0={seg.backtest_p_le0:.3f})")

    for b in bets:
        prob_bits = f" | model {b['model_prob']:.1%} vs mkt {b['market_prob']:.1%} (edge {b['edge']:+.1%})" \
            if b["model_prob"] is not None else " | model/market prob n/a (one side unquoted)"
        print(f"  {b['selection']} {b['side']} {b['line']} {b['market_label']} "
              f"({b['details']['role']}/{b['details']['script']}) @ {b['odds_american']} {b['book']}{prob_bits}")

    if args.dry_run:
        return

    payload = {
        "contract_version": CONTRACT_VERSION,
        "source": SOURCE,
        "sport": SPORT,
        "slate_date": as_of_date,
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "model_version": "segments-v1",
        "mode": "LIVE",
        "notes": "Structural role x game-script segments from an overnight diagnostic pass on the 2025-26 season.",
        "bets": bets,
    }

    FEED_DIR.mkdir(parents=True, exist_ok=True)
    (FEED_DIR / f"picks_{as_of_date}.json").write_text(json.dumps(payload, indent=2))
    ARCHIVE_DIR.mkdir(parents=True, exist_ok=True)
    (ARCHIVE_DIR / f"picks_{as_of_date}.json").write_text(json.dumps(payload, indent=2))
    print(f"\nwrote {FEED_DIR / f'picks_{as_of_date}.json'} and archived to {ARCHIVE_DIR / f'picks_{as_of_date}.json'}")


if __name__ == "__main__":
    main()
