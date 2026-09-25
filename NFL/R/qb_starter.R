# ==============================================================================
# Automated projected-starting-QB resolver (no manual overrides).
#
# The pair-chemistry feature needs to know who will throw in an UPCOMING game.
# Reading it off the last box score detects a change a week late -- which is
# exactly how week 3 2026 was missed (ATL went Cooper Rush -> Michael Penix Jr.
# and the model had no pre-lock signal).
#
# Two public feeds settle it without anyone typing a name:
#   1. nflreadr::load_depth_charts() -- daily snapshots (pos_rank 1 = QB1).
#   2. nflreadr::load_injuries()     -- weekly report_status per player.
# Resolution: take the most recent depth-chart snapshot at or before the game
# date, walk down QB1 -> QB2 -> QB3 skipping anyone ruled Out/Doubtful that
# week, and fall back to the team's last observed starter if neither feed has
# the team covered.
# ==============================================================================

.qb_norm_team <- function(x) {
  if (exists("normalize_team")) return(normalize_team(x))
  toupper(trimws(x))
}

# Depth-chart QB ladder as of `asof` (Date), one row per team/rank.
qb_depth_ladder <- function(season, asof = Sys.Date()) {
  dc <- tryCatch(nflreadr::load_depth_charts(season), error = function(e) NULL)
  if (is.null(dc) || !nrow(dc)) return(NULL)
  dc <- data.table::as.data.table(dc)
  if (!all(c("pos_abb", "pos_rank", "team", "player_name", "dt") %in% names(dc))) return(NULL)
  qb <- dc[pos_abb == "QB" & !is.na(pos_rank)]
  if (!nrow(qb)) return(NULL)
  qb[, snap_date := as.Date(substr(as.character(dt), 1, 10))]
  qb <- qb[!is.na(snap_date) & snap_date <= as.Date(asof)]
  if (!nrow(qb)) return(NULL)
  # newest snapshot at//before asof, per team
  qb <- qb[qb[, .I[snap_date == max(snap_date)], by = team]$V1]
  qb[, .(team = .qb_norm_team(team), pos_rank = as.integer(pos_rank),
         qb = player_name, snap_date)][order(team, pos_rank)]
}

# Players a week's injury report rules out (Out / Doubtful).
#
# NOTE the argument is `target_week`, not `week`. Inside data.table's i-expression
# a bare `week` binds to the COLUMN, so `week == as.integer(week)` compares the
# column to itself and is TRUE for every row -- the week filter silently vanishes
# and every QB ruled out in ANY week comes back. That shadowing bug made the
# resolver think Michael Penix Jr. was out in week 3 2026 (he was Out in weeks 1-2,
# but week 3 carried no report_status at all and he started), so it fell through
# to QB3 Cooper Rush -- reproducing the exact miss this resolver exists to prevent.
qb_unavailable <- function(season, target_week) {
  inj <- tryCatch(nflreadr::load_injuries(season), error = function(e) NULL)
  if (is.null(inj) || !nrow(inj)) return(character(0))
  inj <- data.table::as.data.table(inj)
  poscol <- intersect(c("position", "pos"), names(inj))[1]
  if (is.na(poscol) || !"report_status" %in% names(inj)) return(character(0))
  tw <- as.integer(target_week)
  out <- inj[get(poscol) == "QB" & week == tw &
               report_status %in% c("Out", "Doubtful")]
  unique(as.character(out$full_name))
}

# Projected starter per team for one upcoming week.
#   fallback: data.table(team, qb) of each team's last observed starter.
project_qb_starters <- function(season, target_week, asof = Sys.Date(), fallback = NULL) {
  ladder <- qb_depth_ladder(season, asof)
  if (is.null(ladder)) {
    message("qb_starter: no depth chart available; using last observed starter")
    return(fallback)
  }
  blocked <- qb_unavailable(season, target_week)
  pick <- ladder[!(qb %in% blocked)][order(team, pos_rank)][, .SD[1], by = team][, .(team, qb)]
  if (!is.null(fallback) && nrow(fallback)) {
    fb <- data.table::as.data.table(fallback)[, .(team = .qb_norm_team(team), qb_fb = qb)]
    pick <- merge(fb, pick, by = "team", all.x = TRUE)
    pick[, qb := data.table::fifelse(is.na(qb), qb_fb, qb)]
    pick <- pick[, .(team, qb)]
  }
  pick[]
}
