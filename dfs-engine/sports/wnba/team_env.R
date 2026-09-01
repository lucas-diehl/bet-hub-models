# ==============================================================================
# WNBA plugin — game environment (Vegas-FREE, derived from box history)
# Builds team pace + offensive/defensive level from wehoop box scores, then for a
# slate estimates each game's expected TOTAL and PACE and each team's blowout risk.
# Used to shape the JOINT distribution (correlation strength, ceilings, blowout
# minute risk) — NOT the marginal mean, which stays the validated minutes model.
# This is the free stand-in for Vegas totals/spreads the notes call for.
#
# Team-abbreviation mapping (DK vs wehoop/ESPN) is tagged VERIFY — unmapped teams
# fall back to league-average environment (neutral, no harm).
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# canonical team key; map common DK / ESPN variants. VERIFY vs a live DK slate.
WNBA_TEAM_ALIAS <- c(
  LV="LVA", LVA="LVA", LAS="LAS", LA="LAS", NY="NYL", NYL="NYL", CONN="CON", CON="CON",
  CT="CON", PHO="PHX", PHX="PHX", WSH="WAS", WAS="WAS", GS="GSV", GSV="GSV", GV="GSV",
  LAV="LVA", LV="LVA", NYL="NYL", NY="NYL",
  ATL="ATL", CHI="CHI", DAL="DAL", IND="IND", MIN="MIN", SEA="SEA")
wnba_team_key <- function(x) {
  x <- toupper(trimws(as.character(x)))
  ifelse(x %in% names(WNBA_TEAM_ALIAS), WNBA_TEAM_ALIAS[x], x)
}

# canonical franchises — filters out All-Star / exhibition rosters that pollute pace.
WNBA_TEAMS <- c("ATL","CHI","CON","DAL","IND","LAS","LVA","MIN","NYL","PHX","SEA","WAS","GSV")

WNBA_TEAM_ENV_PATH <- function() dfs_path("data", "models", "wnba_team_games.rds")

# team-game aggregation: points for/against + possession estimate + pace.
wnba_build_team_games <- function(box) {
  B <- as.data.table(copy(box)); B[, game_date := as.Date(game_date)]
  gv <- function(c) if (c %in% names(B)) B[[c]] else 0
  B[, fga := gv("field_goals_attempted")][, fta := gv("free_throws_attempted")]
  B[, orb := gv("offensive_rebounds")]
  tg <- B[, .(pts = sum(pts, na.rm = TRUE), fga = sum(fga, na.rm = TRUE),
              fta = sum(fta, na.rm = TRUE), orb = sum(orb, na.rm = TRUE),
              tov = sum(tov, na.rm = TRUE)),
          by = .(game_id, team = team_abbreviation, opp = opponent_team_abbreviation, game_date)]
  tg[, poss := pmax(fga + 0.44 * fta - orb + tov, 1)]
  opp <- tg[, .(game_id, opp = team, opp_pts = pts, opp_poss = poss)]
  tg <- merge(tg, opp, by = c("game_id", "opp"), all.x = TRUE)
  tg[, pace := (poss + fcoalesce(opp_poss, poss)) / 2]
  tg[, tk := wnba_team_key(team)][, opp_tk := wnba_team_key(opp)]
  tg <- tg[tk %in% WNBA_TEAMS & opp_tk %in% WNBA_TEAMS]   # drop All-Star/exhibition
  setorder(tg, game_date)
  tg[]
}

# rolling per-team environment as of a date (last `window` games), + league means.
wnba_team_env_asof <- function(tg, as_of, window = 10L) {
  H <- tg[game_date < as.Date(as_of)]
  if (!nrow(H)) return(NULL)
  setorder(H, tk, game_date)
  env <- H[, { idx <- tail(seq_len(.N), window)
    .(pace = mean(pace[idx]), pts_for = mean(pts[idx]),
      pts_against = mean(opp_pts[idx], na.rm = TRUE), n = .N) }, by = tk]
  env[, net := pts_for - pts_against]
  attr(env, "lg_pace")  <- mean(env$pace, na.rm = TRUE)
  attr(env, "lg_total") <- mean(env$pts_for + env$pts_against, na.rm = TRUE)
  env
}

# attach per-player game-environment columns to the slate pool:
#   game_total_z  z-scored expected combined points (correlation + ceiling scaler)
#   pace_mult     game pace relative to league (mild)
#   blowout       |expected margin| signal (minute-truncation risk)
wnba_attach_game_env <- function(pool, env, vegas = NULL) {
  P <- as.data.table(copy(pool))
  if (is.null(env) || !nrow(env)) { P[, `:=`(game_total_z = 0, pace_mult = 1, blowout = 0)]; return(P[]) }
  lg_pace  <- attr(env, "lg_pace");  lg_total <- attr(env, "lg_total")
  sd_total <- stats::sd(env$pts_for + env$pts_against); if (!is.finite(sd_total) || sd_total == 0) sd_total <- 1
  P[, tk := wnba_team_key(team)]
  P[, opp := vapply(seq_len(.N), function(i) {
      g <- strsplit(as.character(game_id[i]), "@", fixed = TRUE)[[1]]
      o <- setdiff(g, team[i]); if (length(o)) wnba_team_key(o[1]) else NA_character_ }, character(1))]
  e <- env[, .(tk, pace, pts_for, pts_against, net)]
  P <- merge(P, e, by = "tk", all.x = TRUE)
  P <- merge(P, e[, .(opp = tk, o_pace = pace, o_for = pts_for, o_ag = pts_against, o_net = net)],
             by = "opp", all.x = TRUE)
  P[, g_total := (fcoalesce(pts_for, lg_total/2) + fcoalesce(o_for, lg_total/2) +
                  fcoalesce(pts_against, lg_total/2) + fcoalesce(o_ag, lg_total/2)) / 2]
  P[, g_pace  := (fcoalesce(pace, lg_pace) + fcoalesce(o_pace, lg_pace)) / 2]
  P[, .margin := abs(fcoalesce(net, 0) - fcoalesce(o_net, 0))]      # box-derived expected margin
  # VEGAS OVERRIDE: market total + spread beat rolling box estimates. Matched by the
  # normalized team pair, so DK/ESPN abbrev differences (LVA vs LV) don't block it.
  if (!is.null(vegas) && nrow(as.data.table(vegas))) {
    vg <- as.data.table(vegas)
    vg[, `:=`(hk = wnba_team_key(home), ak = wnba_team_key(away))]
    vg[, pairkey := paste(pmin(hk, ak), pmax(hk, ak))]
    vg <- unique(vg[, .(pairkey, v_total = vegas_total, v_spread = spread)], by = "pairkey")
    P[, pairkey := paste(pmin(tk, opp), pmax(tk, opp))]
    P <- merge(P, vg, by = "pairkey", all.x = TRUE)
    P[is.finite(v_total),  g_total := v_total]
    P[is.finite(v_spread), .margin := abs(v_spread)]
    nm <- P[is.finite(v_total), uniqueN(pairkey)]
    if (nm) msg(sprintf("  wnba: Vegas lines applied to %d game(s)", nm))
  }
  P[, game_total_z := pmin(pmax((g_total - lg_total) / sd_total, -2.5), 2.5)]
  P[, pace_mult := pmin(pmax(g_pace / lg_pace, 0.9), 1.1)]
  P[, blowout := pmin(.margin / 15, 1)]
  P[!is.finite(game_total_z), game_total_z := 0][!is.finite(pace_mult), pace_mult := 1][!is.finite(blowout), blowout := 0]
  P[]
}

# Injury minutes redistribution. The minutes-first model projects each player from
# their OWN history, so when a teammate is ruled OUT it UNDER-projects whoever absorbs
# those minutes/usage. Given the ESPN injury report + recent box minutes, we bump the
# available teammates of OUT players: vacated minutes are shared by "room to grow"
# (backups benefit most), capped at +30% and 36 min. Bumps UP only — never touches the
# mean of unaffected players. Returns the pool with proj/ceil/floor scaled.
wnba_injury_adjust <- function(pool, box, as_of, window = 8L, cap_fac = 1.30) {
  inj <- tryCatch(injury_report("wnba"), error = function(e) NULL)
  if (is.null(inj) || !nrow(inj) || is.null(box)) return(pool)
  P <- as.data.table(pool); B <- as.data.table(box)
  if (!"min" %in% names(B) || !"athlete_display_name" %in% names(B)) return(P)   # wehoop minutes col = "min"
  B[, gd := as.Date(game_date)]; B <- B[gd < as.Date(as_of)]
  if (!nrow(B)) return(P)
  B[, norm := norm_name(athlete_display_name)][, tk := wnba_team_key(team_abbreviation)]
  setorder(B, norm, gd)
  recent <- B[, .(cur_min = mean(tail(min, window), na.rm = TRUE), tk = tk[.N]), by = norm]
  out_norms <- inj[is_out == TRUE, norm]
  n_drop <- P[norm %in% out_norms, .N]
  P <- P[!(norm %in% out_norms)]                                   # confirmed-OUT: ESPN more timely than DK
  if (!nrow(P)) return(P)
  vac <- recent[norm %in% out_norms & cur_min > 5]                 # OUT players who actually played
  if (!nrow(vac)) { if (n_drop) msg(sprintf("  wnba: dropped %d ESPN-confirmed-OUT player(s)", n_drop)); return(P) }
  vac_team <- vac[, .(V = sum(cur_min)), by = tk]
  P[, tkk := wnba_team_key(team)]
  P <- merge(P, recent[, .(norm, cur_min)], by = "norm", all.x = TRUE)
  P[is.na(cur_min), cur_min := 12]
  P[, min_bump := 0]
  for (i in seq_len(nrow(vac_team))) {
    t <- vac_team$tk[i]; V <- vac_team$V[i]
    ix <- which(P$tkk == t & !(P$norm %in% out_norms))
    if (!length(ix)) next
    room <- pmax(34 - P$cur_min[ix], 2)                            # backups have the most room
    P$min_bump[ix] <- V * room / sum(room)
  }
  P[, fac := pmin(pmax(pmin(cur_min + min_bump, 36) / pmax(cur_min, 8), 1), cap_fac)]
  P[fac > 1.001, `:=`(proj = proj * fac, ceil = ceil * fac, floor = floor * fac)]
  nb <- P[fac > 1.02, .N]
  if (nb) msg(sprintf("  wnba: injury minutes-bump applied to %d teammate(s) of %d OUT player(s)",
                      nb, length(out_norms)))
  P[, c("tkk", "cur_min", "min_bump", "fac") := NULL]
  P[]
}
