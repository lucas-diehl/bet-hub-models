# ==============================================================================
# NFL plugin — injury pricing + VACATED-OPPORTUNITY redistribution (2026-11-13)
#
# Before this, a player ruled out simply vanished from the pool and nobody absorbed his
# work: Corey Kiner (NE RB3) stayed at 5.5 pts with TreVeyon Henderson OUT and Rhamondre
# Stevenson Questionable. And a Questionable player was penalised twice (projection
# x0.6 in apply_inactives() AND +0.30 DNP risk => valued at ~40% of his projection).
#
# CALIBRATION (2021-2025 REG, 1,152 absences of players with a prior-4-game PPR baseline
# >= 8; teammates' PPR vs their own prior-4 baselines, net of normal-week drift; script
# kept in the 2026-11-13 session scratchpad as vacated_calib.R):
#                                 first missed game    already missed last game
#   RB out -> other RBs absorb        52% (se 6.6)          27% (se 6.3)
#   WR/TE out -> other WR/TE          46% (se 6.6)          21% (se 6.7)
#   RB out -> WR/TE spill             19%                   ~10% (scaled)
#   WR/TE out -> RB spill             11%                   ~5%  (scaled)
#   allocation by remaining-teammate rank (by projection):
#     RB   first .79/.16/.05         continuing .71/.20/.09
#     WR/TE first .24/.24/.16/.36(4+) continuing .40/.27/.06/.28(4+)
# "Continuing" is smaller because the projection model's rolling usage windows have
# already partly absorbed an absence that started in an earlier week.
#
# MECHANICS. Every player carries proj_active (projection IF he plays) and p_zero (chance
# he doesn't); `proj` becomes the expected value proj_active * (1 - p_zero) for anyone
# with real injury risk (p_zero >= 0.10) -- ONE penalty, measured, not two. A teammate's
# bump b from an absentee who is certainly out is added to his projection outright
# (times P(out)). If the absentee is UNCERTAIN and still in the pool (Questionable), the
# bump is COUPLED in the slate sim: the teammate gets +b in exactly the simulated games
# where that absentee is zeroed (pool columns cuff_of / cuff_bump, read by slate_sim()),
# and `proj` carries the expectation p*b. That gives a handcuff its real GPP ceiling.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

NFL_VACATED <- list(
  min_p   = 0.10,                                          # below this = healthy noise
  q_rate  = 0.30,                                          # ESPN Questionable w/o a model rate
  out_p   = 0.97,                                          # ESPN Out/Doubtful/IR
  f_same  = c(RB_first = 0.52, RB_cont = 0.27, PC_first = 0.46, PC_cont = 0.21),
  f_cross = c(RB_first = 0.19, RB_cont = 0.10, PC_first = 0.11, PC_cont = 0.05),
  share   = list(RB_first = c(.79, .16, .05), RB_cont = c(.71, .20, .09),
                 PC_first = c(.24, .24, .16, .36), PC_cont = c(.40, .27, .06, .28)),
  max_tail = 3L,                                           # rank-4+ share split over at most 3 more players
  cap_mult = 1.0, cap_abs = 8)                             # bump <= max(1.0 * own proj, 8 pts)

.nfl_grp <- function(pos) { p <- toupper(pos); fifelse(p == "RB", "RB", fifelse(p %in% c("WR", "TE"), "PC", NA_character_)) }
.nfl_dk_team <- function(t) { t <- toupper(as.character(t)); t[t == "LA"] <- "LAR"; t[t == "JAC"] <- "JAX"; t[t == "WSH"] <- "WAS"; t }

# For each normalized name: did he play in his team's most recent game this season?
# (FALSE => an absence that started earlier => "continuing"). Cached ~12h; NULL on failure.
nfl_last_game_status <- function(max_age_hours = 12) {
  yr <- as.integer(format(Sys.Date(), "%Y")); if (as.integer(format(Sys.Date(), "%m")) < 3) yr <- yr - 1L
  p <- dfs_path("data", "raw", sprintf("nfl_last_game_%d.rds", yr))
  fresh <- file.exists(p) && difftime(Sys.time(), file.info(p)$mtime, units = "hours") < max_age_hours
  if (!fresh) {
    d <- tryCatch(.nfl_fetch_year(yr, prefer_cache = FALSE), error = function(e) NULL)
    if (!is.null(d) && nrow(d)) {
      d <- as.data.table(d); tc <- intersect(c("team", "recent_team"), names(d))[1]
      if (!is.na(tc) && "season_type" %in% names(d)) d <- d[season_type %in% c("REG", "POST")]
      if (!is.na(tc)) {
        d[, tm := .nfl_dk_team(get(tc))]
        tl <- d[, .(team_last = max(week)), by = tm]
        pl <- d[, .(player_last = max(week)), by = .(norm = norm_name(player_display_name), tm)]
        out <- merge(pl, tl, by = "tm")[, .(norm, team = tm, played_last = player_last >= team_last)]
        dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); saveRDS(out, p)
      }
    }
  }
  if (!file.exists(p)) return(NULL)
  tryCatch(as.data.table(readRDS(p)), error = function(e) NULL)
}

# pool: DK pool after projection (needs norm, team, position, proj, p_zero, sim_sd, ceil, floor).
# ext:  the external projection board (norm, team, position, e_proj, e_pzero) incl. players
#       DK already dropped as OUT. inj: injury_report("nfl") or NULL. Returns the pool with
#       proj_active / cuff_of / cuff_bump / vac_bump / inj_priced columns.
nfl_price_and_redistribute <- function(pool, ext = NULL, inj = NULL, last = NULL, prm = NFL_VACATED) {
  pool <- as.data.table(copy(pool))
  pool[, `:=`(proj_active = proj, cuff_of = NA_character_, cuff_bump = 0, vac_bump = 0, inj_priced = FALSE)]
  if (!"p_zero" %in% names(pool)) pool[, p_zero := 0.03]
  # ---- 1. who might not play, with what probability --------------------------------------
  esp <- if (!is.null(inj) && nrow(inj)) inj[, .(norm, cat)] else data.table(norm = character(), cat = character())
  pool[esp, on = "norm", esp_cat := i.cat]
  pool[esp_cat == "questionable", p_zero := pmax(p_zero, prm$q_rate)]
  pool[, inj_priced := p_zero >= prm$min_p | esp_cat %in% c("questionable", "out")]
  ab <- pool[p_zero >= prm$min_p & !is.na(.nfl_grp(position)),
             .(a_norm = norm, a_id = player_id, team, grp = .nfl_grp(position), p = p_zero, base = proj_active, in_pool = TRUE)]
  if (!is.null(ext) && nrow(ext)) {                       # absentees DK already removed (OUT) live only in ext
    ex <- ext[!norm %in% pool$norm & !is.na(.nfl_grp(position)),
              .(a_norm = norm, a_id = NA_character_, team = .nfl_dk_team(team), grp = .nfl_grp(position), p = e_pzero, base = e_proj, in_pool = FALSE)]
    ex[esp[cat == "out"], on = c(a_norm = "norm"), p := pmax(p, prm$out_p)]
    ex[esp[cat == "questionable"], on = c(a_norm = "norm"), p := pmax(p, prm$q_rate)]
    ex <- ex[p >= prm$min_p & team %in% unique(pool$team) & is.finite(base) & base > 0]
    ab <- rbind(ab, ex)
  }
  # ---- 2. expected-value projection for anyone with real risk (ONE penalty) -----------------
  r <- which(pool$inj_priced)
  for (cc in intersect(c("proj", "ceil", "floor"), names(pool))) set(pool, r, cc, pool[[cc]][r] * (1 - pool$p_zero[r]))
  if (!nrow(ab)) { pool[, esp_cat := NULL]; return(pool[]) }
  # ---- 3. vacated work -> teammates ----------------------------------------------------------
  if (is.null(last)) last <- tryCatch(nfl_last_game_status(), error = function(e) NULL)
  ab[, cont := TRUE]                                       # unknown -> conservative (smaller f)
  if (!is.null(last) && nrow(last)) ab[last, on = c(a_norm = "norm", team = "team"), cont := !i.played_last]
  ab[, key := paste0(grp, fifelse(cont, "_cont", "_first"))]
  give <- list()
  elig <- pool[!is.na(.nfl_grp(position)) & p_zero < 0.5]
  for (i in seq_len(nrow(ab))) {
    a <- ab[i]
    mates <- elig[team == a$team & .nfl_grp(position) == a$grp & norm != a$a_norm][order(-proj_active)]
    if (nrow(mates)) {
      sh <- prm$share[[a$key]]; k <- nrow(mates)
      w <- c(sh[seq_len(min(3L, k))], if (k > 3) rep(sh[4] / min(k - 3L, prm$max_tail), min(k - 3L, prm$max_tail)) else numeric(0))
      w <- w / sum(w)
      give[[length(give) + 1L]] <- data.table(norm = mates$norm[seq_along(w)], a_id = a$a_id, p = a$p,
                                              b = prm$f_same[[a$key]] * a$base * w, src = a$a_norm)
    }
    other <- elig[team == a$team & !is.na(.nfl_grp(position)) & .nfl_grp(position) != a$grp]
    if (nrow(other)) {
      w <- other$proj_active / sum(other$proj_active)
      give[[length(give) + 1L]] <- data.table(norm = other$norm, a_id = a$a_id, p = a$p,
                                              b = prm$f_cross[[a$key]] * a$base * w, src = a$a_norm)
    }
  }
  G <- rbindlist(give)
  if (!nrow(G)) { pool[, esp_cat := NULL]; return(pool[]) }
  # coupled: the single biggest bump per recipient from an absentee still IN the pool (uncertain)
  G[, coupled := FALSE]
  cp <- G[!is.na(a_id) & p < 0.9][order(-b)][, .SD[1], by = norm]
  G[cp, on = .(norm, a_id), coupled := TRUE]
  det <- G[coupled == FALSE, .(det = sum(p * b)), by = norm]          # expected, added when he plays
  pool[, `:=`(det = 0, c_p = 0)]
  pool[det, on = "norm", det := i.det]
  pool[cp,  on = "norm", `:=`(cuff_of = i.a_id, cuff_bump = i.b, c_p = i.p)]
  # cap total bump at max(1.0 x own active projection, 8 pts)
  tot <- pool$det + pool$cuff_bump
  cap <- pmax(prm$cap_mult * pool$proj_active, prm$cap_abs)
  sc  <- fifelse(tot > cap & tot > 0, cap / tot, 1)
  pool[, `:=`(det = det * sc, cuff_bump = cuff_bump * sc)]
  # value IF he plays (sim mean, coupled part added only in sims where that absentee sits)
  pool[, base_exp := proj]                                             # expected value before bumps
  pool[, proj_active := proj_active + det]
  pool[, vac_bump := det + c_p * cuff_bump]
  up <- pool$vac_bump > 0
  pool[up, proj := (proj_active + c_p * cuff_bump) * fifelse(inj_priced, 1 - p_zero, 1)]
  pool[up, `:=`(ceil = ceil + (proj - base_exp), floor = floor + 0.5 * (proj - base_exp),
                sim_sd = sim_sd * sqrt(pmax(proj, 1) / pmax(base_exp, 1)))]
  pool[is.na(cuff_of) | cuff_bump <= 0, `:=`(cuff_of = NA_character_, cuff_bump = 0)]
  pool[, c("det", "c_p", "base_exp") := NULL]
  big <- pool[vac_bump >= 1][order(-vac_bump)]
  if (nrow(big)) msg(sprintf("  nfl vacated: %d teammate bump(s) >= 1 pt: %s", nrow(big),
                             paste(head(sprintf("%s +%.1f%s", big$player_name, big$vac_bump,
                                                ifelse(is.na(big$cuff_of), "", " (sim-coupled)")), 8), collapse = ", ")))
  pool[, esp_cat := NULL]
  pool[]
}
