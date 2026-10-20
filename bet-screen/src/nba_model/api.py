"""Thin wrapper over nba_api (stats.nba.com), mirroring cbb_model.api's conventions:
retry/backoff on transient failures, disk caching is the caller's responsibility, season
label translation. stats.nba.com has no documented credit cost, but it does rate-limit /
occasionally return empty bodies under load, so every call goes through the same retry path.
"""

from __future__ import annotations

import queue
import threading
import time
from dataclasses import dataclass

import pandas as pd
from nba_api.stats.endpoints import (
    boxscoreplayertrackv3, boxscoresummaryv3, commonteamroster, leaguegamelog,
)
from nba_api.stats.static import teams as static_teams


class NBAApiError(RuntimeError):
    pass


# Observed in practice (twice, both during the 2026-10-20 overnight backfill): a call can
# hang well past its own `timeout` kwarg -- nba_api/requests doesn't always honor it under
# certain network conditions -- silently stalling the whole script with no exception to
# retry on. A thread-based hard deadline is the backstop: if the call doesn't return in
# HARD_DEADLINE seconds, treat it as failed and move on, same as any other retryable error.
# Must be a raw daemon Thread, not concurrent.futures.ThreadPoolExecutor -- confirmed by
# testing: an abandoned ThreadPoolExecutor worker is NOT a daemon thread, so a genuinely
# hung call (one that never returns at all) keeps the whole PROCESS alive waiting for it at
# interpreter shutdown even though `_call` itself correctly raises and moves on. A daemon
# thread is abandoned for real -- the process can exit with it still stuck in the background.
HARD_DEADLINE = 90.0


def season_label(season: int) -> str:
    """season is the ending year (matches cbb's convention): 2025 -> "2024-25"."""
    start = season - 1
    return f"{start}-{str(season)[-2:]}"


@dataclass
class NBAStatsClient:
    max_retries: int = 6
    timeout: float = 60.0
    base_sleep: float = 1.0

    def _call(self, fn, *args, **kwargs):
        last_err: Exception | None = None
        for attempt in range(self.max_retries):
            result_q: queue.Queue = queue.Queue(maxsize=1)

            def _run() -> None:
                try:
                    result_q.put(("ok", fn(*args, timeout=self.timeout, **kwargs)))
                except Exception as exc:  # noqa: BLE001
                    result_q.put(("err", exc))

            thread = threading.Thread(target=_run, daemon=True)
            thread.start()
            try:
                status, value = result_q.get(timeout=HARD_DEADLINE)
            except queue.Empty:
                last_err = TimeoutError(f"{fn} exceeded hard deadline of {HARD_DEADLINE}s")
            else:
                if status == "ok":
                    return value
                last_err = value
            sleep_for = min(self.base_sleep * (2**attempt), 30.0)
            time.sleep(sleep_for)
        raise NBAApiError(f"{fn} failed after {self.max_retries} attempts: {last_err}") from last_err

    def get_player_game_log(self, season: int, season_type: str = "Regular Season") -> pd.DataFrame:
        """One call for the whole season -- every player's traditional box-score line in
        every game. Players who did not play that game are simply absent from this table,
        not present with 0 minutes -- see get_inactive_players for availability."""
        endpoint = self._call(
            leaguegamelog.LeagueGameLog,
            season=season_label(season),
            season_type_all_star=season_type,
            player_or_team_abbreviation="P",
        )
        df = endpoint.get_data_frames()[0]
        df["season"] = season
        # The API returns GAME_ID as a zero-padded 10-char string; a cached-CSV round trip
        # coerces it to int64 and drops the padding. Normalizing to int here, on every call
        # regardless of source, keeps game_id identity consistent between a cold-start run
        # and a resumed one -- without this, resuming after an interruption silently
        # re-fetches every already-completed game under the other format (hit in practice:
        # a restart re-did ~600 games before this was caught, see HANDOFF.md).
        df["GAME_ID"] = df["GAME_ID"].astype(int)
        return df

    def get_inactive_players(self, game_id: str) -> pd.DataFrame:
        """Authoritative per-game inactive list (rostered/dressed but did not play, any
        reason) -- the only reliable way to know who was missing, since the game log above
        omits absent players entirely rather than recording them with 0 minutes."""
        endpoint = self._call(boxscoresummaryv3.BoxScoreSummaryV3, game_id=game_id)
        frames = endpoint.get_data_frames()
        inactive = frames[5]
        out = inactive.rename(columns={"personId": "player_id", "teamId": "team_id"})[
            ["gameId", "team_id", "player_id", "firstName", "familyName"]
        ].rename(columns={"gameId": "game_id"})
        # nba_api's GAME_ID is a zero-padded 10-char string everywhere (the game log below
        # included); a CSV round-trip silently coerces that to int64 and drops the leading
        # zero, so every game_id in this project is stored/joined as int, not the padded
        # string -- only pad it back when it's an outgoing API parameter.
        out["game_id"] = out["game_id"].astype(int)
        return out

    def get_player_tracking(self, game_id: str) -> pd.DataFrame:
        """Per-game player tracking box score: rebound CHANCES (not just rebounds made),
        touches, contested/uncontested field goals -- the "stable drivers" the user's plan
        (section 8) flags as signal over noise, not available from the traditional box score
        at all. One call per game, same cost/rate-limit profile as get_inactive_players."""
        endpoint = self._call(boxscoreplayertrackv3.BoxScorePlayerTrackV3, game_id=game_id)
        df = endpoint.get_data_frames()[0]
        df = df.rename(columns={"gameId": "game_id", "teamId": "team_id", "personId": "player_id"})
        df["game_id"] = df["game_id"].astype(int)
        return df

    def get_team_roster(self, team_id: int, season: int) -> pd.DataFrame:
        endpoint = self._call(commonteamroster.CommonTeamRoster, team_id=team_id, season=season_label(season))
        return endpoint.get_data_frames()[0]

    @staticmethod
    def all_teams() -> list[dict]:
        return static_teams.get_teams()
