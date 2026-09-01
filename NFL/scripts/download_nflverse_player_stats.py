from pathlib import Path
import os
import urllib.request


OUTPUT_DIR = Path("data/raw/nflverse_player_stats")
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
for season in range(2014, 2022):
    url = (
        "https://github.com/nflverse/nflverse-data/releases/download/"
        f"stats_player/stats_player_week_{season}.rds"
    )
    destination = OUTPUT_DIR / f"stats_player_week_{season}.rds"
    if destination.exists() and destination.stat().st_size > 0:
        print(f"Cached {season}: {destination.stat().st_size:,} bytes")
        continue
    temporary = destination.with_suffix(".rds.part")
    request = urllib.request.Request(
        url,
        headers={"User-Agent": "nfl-dfs-value-backtest/1.0"},
    )
    with opener.open(request, timeout=60) as response, temporary.open("wb") as output:
        while chunk := response.read(1024 * 1024):
            output.write(chunk)
    os.replace(temporary, destination)
    print(f"Downloaded {season}: {destination.stat().st_size:,} bytes")

