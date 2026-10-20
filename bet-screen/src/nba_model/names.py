"""Player-name normalization for matching nba_api's PLAYER_NAME against the Odds API's
outcome "description" field. Confirmed real gap on the actual pilot data: 89.1% exact match
(310/348), with the remaining 10.9% falling into clear categories -- diacritics ("Bogdan
Bogdanovic" vs "Bogdan Bogdanović"), periods in initials ("A.J. Green" vs "AJ Green"), Jr/Sr/II
suffixes with or without a period, and a handful of true nicknames the Odds API uses that
nba_api doesn't ("Moe Wagner" vs "Moritz Wagner", "Bub Carrington" vs "Carlton Carrington").

This mirrors the CBB project's own team-name crosswalk pattern (see `all past/` KenPom
crosswalk) -- mechanical normalization gets most of the way, a small manual map catches the
true nicknames that no amount of string normalization would fix.
"""

from __future__ import annotations

import re
import unicodedata

SUFFIX_RE = re.compile(r"\s+(jr|sr|ii|iii|iv)\.?$", re.IGNORECASE)

# True nicknames / alternate names the Odds API uses that don't normalize mechanically from
# nba_api's PLAYER_NAME. Keyed by normalized odds-side name -> normalized nba_api-side name.
# Extend this as new mismatches turn up -- see reports/_name_match_audit.py.
NICKNAME_MAP = {
    "moe wagner": "moritz wagner",
    "bub carrington": "carlton carrington",
    "herb jones": "herbert jones",
    "nicolas claxton": "nic claxton",
    "ron holland": "ronald holland",
    "vincent williams": "vince williams",
}


def normalize_name(name: str) -> str:
    if not isinstance(name, str):
        return ""
    name = unicodedata.normalize("NFKD", name)
    name = "".join(c for c in name if not unicodedata.combining(c))  # strip diacritics
    name = name.replace(".", "")
    name = SUFFIX_RE.sub("", name)
    name = re.sub(r"\s+", " ", name).strip().lower()
    return NICKNAME_MAP.get(name, name)
