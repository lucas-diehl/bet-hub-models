#!/usr/bin/env Rscript
# ==============================================================================
# GOLF v2 — engine/matchplay_sim.R   Presidents Cup / Ryder Cup match-play Monte Carlo
#
# Hole-by-hole match-play simulator feeding DK's Presidents Cup scoring. The DK
# point table itself lives in the SIBLING DFS ENGINE project
# (sports/golf/matchplay.R) since it's also used there to grade a SETTLED real
# match -- this file sources that one so a settled match and a simulated one are
# graded IDENTICALLY (see .mp_locate_scoring_file() below).
#
# WHAT THIS BUILDS, PER PLAYER, PER HOLE:
#   A categorical outcome distribution over {eagle, birdie, par, bogey, double+}
#   built from each player's own trailing rate features (engine/rich_features.R,
#   persisted in golf_picks/v2_master.rds): birdie_rate_24, bogey_rate_24,
#   ceiling_rate_24 (birdie-or-better), disaster_rate_24 (double-or-worse) --
#   all stored as an AVERAGE COUNT PER 18-HOLE ROUND, so dividing by 18 gives a
#   per-hole probability. Verified live this session: all 24 2026 Presidents Cup
#   rostered players (12 USA + 12 International) have finite, populated values
#   for these features in v2_master.rds's latest snapshot -- INCLUDING the
#   International players with thinner PGA TOUR schedules (Bezuidenhout,
#   Hisatsune, Echavarria, etc. all matched cleanly). The ELO/career_sg fallback
#   below exists for robustness (a future roster change, a rain-shortened
#   season, etc.) but is NOT what's driving today's 24 projections.
#
# SIMPLIFICATIONS (v1, documented -- revisit if this becomes a recurring product):
#   - A player's per-hole outcome probability is CONSTANT across all 18 holes
#     (no hole-specific difficulty/length modeling -- Medinah's hole-by-hole
#     scoring averages aren't in this codebase). This understates variance in
#     WHICH holes swing a match, not the overall point total.
#   - FOURSOMES (alternate shot): no alternate-shot-specific data exists
#     anywhere in this codebase to calibrate against. A team's per-hole
#     distribution is approximated as the PROBABILITY-WISE AVERAGE of the two
#     partners' individual (stroke-play) distributions (mp_blend_dist()). This
#     is a real approximation -- true alternate-shot skill is correlated in
#     ways a simple average can't capture (a bad tee shot by one partner
#     constrains the other's approach) -- so foursomes estimates are
#     lower-confidence than fourball/singles. No better data exists to fix this.
#   - Eagles are not a separate tracked rate feature -- EAGLE_SHARE below carves
#     a small, FIXED fraction out of each player's birdie-or-better bucket
#     (same fraction for every player; no per-player eagle-rate history
#     exists to calibrate this individually). Only matters for hole-win
#     determination when BOTH sides are in the birdie-or-better bucket the same
#     hole (rare) -- it does not affect a player's OWN total any other way.
#   - Session/day fatigue, home-crowd, momentum: SKIPPED entirely for v1 (not
#     worth the complexity on this timeline) -- a documented future refinement.
#
# Source: Sys.setenv(MPSIM_SOURCE_ONLY="1"); source("engine/matchplay_sim.R")
# Run standalone:
#   Rscript engine/matchplay_sim.R              # quick self-test / sanity prints
#   Rscript engine/matchplay_sim.R --export      # writes golf_picks/dfs_presidents_cup_projections.rds
#                                                 # (the DFS-ENGINE bridge file; mirrors round_sim.R's
#                                                 #  export_round_projection() pattern)
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
OUT  <- "golf_picks"
emsg <- get0("emsg", ifnotfound = function(...) cat(..., "\n"))
`%||%` <- get0("%||%", ifnotfound = function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a)

# ── locate + source the DK match-play POINT TABLE (single source of truth) ────
# Lives in the sibling DFS ENGINE project. Falls back to an embedded copy of the
# same logic if that project isn't colocated, so this file still works standalone
# (e.g. if golf-modeling is ever run on a machine without DFS ENGINE checked out).
.mp_locate_scoring_file <- function() {
  cands <- c(Sys.getenv("DFS_ENGINE_ROOT", ""),
             file.path(dirname(normalizePath(getwd(), mustWork = FALSE)), "DFS ENGINE"),
             "c:/Users/ljdie/OneDrive/Documents/DFS ENGINE")
  for (d in cands) {
    if (!nzchar(d)) next
    f <- file.path(d, "sports", "golf", "matchplay.R")
    if (file.exists(f)) return(f)
  }
  NA_character_
}
if (!exists("dk_matchplay_points")) {
  .mp_f <- .mp_locate_scoring_file()
  if (!is.na(.mp_f)) { source(.mp_f); emsg("matchplay_sim: sourced DK scoring table from ", .mp_f) } else {
    emsg("matchplay_sim: DFS ENGINE/sports/golf/matchplay.R not found -- using an EMBEDDED fallback copy",
         "of the point table (keep in sync by hand if you ever see this warning).")
    DK_MATCHPLAY_POINTS <- list(hole_won = 3, hole_halved = 0.75, hole_lost = -0.75, hole_unplayed_win = 1.6,
      match_won = 5, match_halved = 2, match_lost = 0, bonus_streak3 = 5, bonus_zero_lost = 7.5,
      bonus_zero_lost_min_holes = 10L)
  }
}

# ── name matching (v2_master.rds stores "Last, First") ────────────────────────
.mp_norm <- function(x) {
  x <- iconv(as.character(x), to = "ASCII//TRANSLIT"); x <- tolower(trimws(x))
  swap <- grepl(",", x); x[swap] <- sub("^\\s*([^,]+),\\s*(.+)$", "\\2 \\1", x[swap])
  x <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", x); x <- gsub("[^a-z ]", "", x)
  trimws(gsub("\\s+", " ", x))
}

# ── per-player latest trailing-rate snapshot (from golf-modeling's own master) ─
# Returns data.table(name, player_id, elo_pre, career_sg, sg_last_24,
# birdie_rate_24, bogey_rate_24, ceiling_rate_24, disaster_rate_24,
# bogey_avoid_24, data_ok) keyed by the INPUT name string (not the internal id),
# with attr(., "field") = list(birdie=, bogey=, dbl=, elo=) field-average
# baselines (from ALL players' latest snapshot, not just these) for the fallback
# mapping in mp_hole_dist().
mp_player_snapshot <- function(names) {
  M <- as.data.table(readRDS(file.path(OUT, "v2_master.rds"))$master)
  M <- M[!is.na(player_name)]
  M[, nkey := .mp_norm(player_name)]

  # name -> player_id lookup built from EVERY historical row, not just each
  # player's most recent one -- the same player_id can carry more than one name
  # SPELLING across rows (confirmed live: player_id 22833 appears as both
  # "Echavarria, Nico" [most of this project's own history, incl. its latest
  # row] and "Echavarria, Nicolas" [what DK's live salary feed calls him]).
  # Resolving on the full history means whichever spelling a caller passes in
  # (e.g. DK's live displayName) still finds the right player_id even when that
  # spelling isn't the one attached to the chronologically latest row.
  idmap <- unique(M[, .(nkey, player_id)], by = "nkey")

  setorder(M, player_id, event_date)
  latest <- M[, .SD[.N], by = player_id]

  req <- data.table(name = names, nkey = .mp_norm(names))
  req <- merge(req, idmap, by = "nkey", all.x = TRUE, sort = FALSE)
  m <- merge(req[, .(name, player_id)], latest, by = "player_id", all.x = TRUE, sort = FALSE)
  cols <- c("name", "player_id", "elo_pre", "career_sg", "sg_last_24", "birdie_rate_24",
            "bogey_rate_24", "ceiling_rate_24", "disaster_rate_24", "bogey_avoid_24")
  m <- m[, ..cols]
  m[, data_ok := is.finite(birdie_rate_24) & is.finite(bogey_rate_24) &
                 is.finite(ceiling_rate_24) & is.finite(disaster_rate_24)]
  if (any(is.na(m$player_id)))
    emsg("matchplay_sim: NO snapshot match for: ", paste(m$name[is.na(m$player_id)], collapse = ", "),
         " -- will use the field-average ELO fallback for them.")

  fld <- latest[is.finite(birdie_rate_24) & is.finite(bogey_rate_24) &
                is.finite(disaster_rate_24) & is.finite(elo_pre)]
  attr(m, "field") <- list(birdie = mean(fld$ceiling_rate_24, na.rm = TRUE),
                           bogey  = mean(fld$bogey_rate_24,  na.rm = TRUE),
                           dbl    = mean(fld$disaster_rate_24, na.rm = TRUE),
                           elo    = mean(fld$elo_pre, na.rm = TRUE))
  m[]
}

# ── per-hole outcome distribution for one player ───────────────────────────────
EAGLE_SHARE <- 0.04   # documented approx (see header) -- fixed fraction of the
                      # birdie-or-better bucket assigned to eagle-or-better.
HOLE_VALUES <- c(eagle = -2, birdie = -1, par = 0, bogey = 1, double = 2)

mp_hole_dist <- function(row, field = NULL) {
  if (is.null(field)) field <- list(birdie = 3.8, bogey = 2.6, dbl = 0.20, elo = 1600)
  if (isTRUE(row$data_ok)) {
    boB <- as.numeric(row$ceiling_rate_24) / 18     # birdie-or-better probability per hole
    bg  <- as.numeric(row$bogey_rate_24)   / 18
    db  <- as.numeric(row$disaster_rate_24) / 18
  } else {
    # FALLBACK: no rate history (thin/no recent PGA TOUR sample) -- derive an
    # approximate per-hole distribution from elo_pre, scaled off the CURRENT
    # field's own rate/skill relationship (not a hardcoded constant), so it
    # self-calibrates if the field composition shifts.
    elo <- suppressWarnings(as.numeric(row$elo_pre)); if (!is.finite(elo)) elo <- field$elo
    d_elo <- (elo - field$elo) / 100                # ~100 Elo per notable skill tier
    boB <- max((field$birdie + 0.55 * d_elo) / 18, 0.02)
    bg  <- max((field$bogey  - 0.45 * d_elo) / 18, 0.02)
    db  <- max((field$dbl    - 0.05 * d_elo) / 18, 0.002)
  }
  eagle  <- boB * EAGLE_SHARE
  birdie <- boB - eagle
  par    <- 1 - eagle - birdie - bg - db
  if (!is.finite(par) || par < 0) {   # defensive renormalize if inputs were inconsistent
    tot <- eagle + birdie + bg + db
    eagle <- eagle / tot; birdie <- birdie / tot; bg <- bg / tot; db <- db / tot; par <- 0
  }
  c(eagle = eagle, birdie = birdie, par = par, bogey = bg, double = db)
}

# FOURSOMES team-distribution approximation -- see header SIMPLIFICATIONS.
mp_blend_dist <- function(d1, d2) (d1 + d2) / 2

# ── low-level Monte Carlo primitives ───────────────────────────────────────────
.mp_draw <- function(dist, n_holes, n_sims) {
  dist <- pmax(dist, 0); dist <- dist / sum(dist)
  cats <- names(dist)
  idx <- sample.int(length(cats), n_holes * n_sims, replace = TRUE, prob = dist)
  matrix(HOLE_VALUES[cats][idx], nrow = n_holes, ncol = n_sims)
}

# Simulate the match-play CLOSEOUT mechanics given two [n_holes x n_sims]
# relative-to-par score matrices (already team-level for fourball/foursomes).
# Standard match-play mercy rule: once the trailing side's deficit exceeds the
# holes remaining, the match ends (e.g. 4up with 3 to play = "4&3").
.mp_run_match <- function(scoreA, scoreB, max_holes = 18L) {
  n_sims <- ncol(scoreA)
  hole_res <- sign(scoreB - scoreA)     # +1 = A won the hole, -1 = A lost it, 0 = halved
  status_vec  <- integer(n_sims)
  closed      <- rep(FALSE, n_sims)
  n_played    <- rep(max_holes, n_sims)
  won_mask    <- matrix(FALSE, max_holes, n_sims)   # from A's perspective
  lost_mask   <- matrix(FALSE, max_holes, n_sims)
  halved_mask <- matrix(FALSE, max_holes, n_sims)
  for (h in seq_len(max_holes)) {
    active <- !closed
    if (!any(active)) break
    r <- hole_res[h, ]
    won_mask[h, active]    <- r[active] == 1L
    lost_mask[h, active]   <- r[active] == -1L
    halved_mask[h, active] <- r[active] == 0L
    status_vec[active] <- status_vec[active] + r[active]
    holes_remaining <- max_holes - h
    newly_closed <- active & (abs(status_vec) > holes_remaining)
    n_played[newly_closed] <- h
    closed[newly_closed] <- TRUE
  }
  list(status = status_vec, n_played = n_played, max_holes = max_holes,
       won_mask = won_mask, lost_mask = lost_mask, halved_mask = halved_mask)
}

# DK points for one side of a simulated match, fully vectorized across sims.
# Mirrors sports/golf/matchplay.R's dk_matchplay_points() exactly (same
# DK_MATCHPLAY_POINTS constants) -- see the cross-check in the validation run.
.mp_score_side <- function(sim, side = c("A", "B")) {
  side <- match.arg(side)
  P <- DK_MATCHPLAY_POINTS
  n_sims <- ncol(sim$won_mask)
  if (side == "A") { won <- sim$won_mask; lost <- sim$lost_mask; halved <- sim$halved_mask; status <- sim$status
  } else           { won <- sim$lost_mask; lost <- sim$won_mask; halved <- sim$halved_mask; status <- -sim$status }
  pts_hole <- colSums(won) * P$hole_won + colSums(halved) * P$hole_halved + colSums(lost) * P$hole_lost
  match_result <- ifelse(status > 0L, "win", ifelse(status < 0L, "loss", "halve"))
  holes_unplayed <- sim$max_holes - sim$n_played
  pts_unplayed <- ifelse(match_result == "win", holes_unplayed * P$hole_unplayed_win, 0)
  pts_match <- c(win = P$match_won, halve = P$match_halved, loss = P$match_lost)[match_result]
  nH <- nrow(won)
  streak <- rep(FALSE, n_sims)
  if (nH >= 3L) for (i in seq_len(nH - 2L)) streak <- streak | (won[i, ] & won[i + 1, ] & won[i + 2, ])
  bonus_streak <- as.numeric(streak) * P$bonus_streak3
  bonus_zero_lost <- as.numeric(sim$n_played >= P$bonus_zero_lost_min_holes & colSums(lost) == 0) * P$bonus_zero_lost
  total <- pts_hole + pts_unplayed + as.numeric(pts_match) + bonus_streak + bonus_zero_lost
  list(total = total, match_result = match_result, n_played = sim$n_played)
}

mp_summarize <- function(side_result) {
  t <- side_result$total
  data.table(mean_pts = mean(t), sd_pts = stats::sd(t),
             p10 = as.numeric(stats::quantile(t, 0.10)), p90 = as.numeric(stats::quantile(t, 0.90)),
             win_prob = mean(side_result$match_result == "win"),
             halve_prob = mean(side_result$match_result == "halve"),
             loss_prob = mean(side_result$match_result == "loss"))
}

# ── per-session-type match simulators ──────────────────────────────────────────
mp_simulate_singles <- function(distA, distB, n_sims = 8000L, max_holes = 18L, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  sA <- .mp_draw(distA, max_holes, n_sims); sB <- .mp_draw(distB, max_holes, n_sims)
  sim <- .mp_run_match(sA, sB, max_holes)
  list(A = .mp_score_side(sim, "A"), B = .mp_score_side(sim, "B"), sim = sim)
}

# FOURBALL (better-ball): each partner draws their OWN outcome; the team's
# effective hole score is the BETTER (lower) of the two.
mp_simulate_fourball <- function(distA1, distA2, distB1, distB2, n_sims = 8000L, max_holes = 18L, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  a1 <- .mp_draw(distA1, max_holes, n_sims); a2 <- .mp_draw(distA2, max_holes, n_sims)
  b1 <- .mp_draw(distB1, max_holes, n_sims); b2 <- .mp_draw(distB2, max_holes, n_sims)
  teamA <- pmin(a1, a2); teamB <- pmin(b1, b2)
  sim <- .mp_run_match(teamA, teamB, max_holes)
  list(A = .mp_score_side(sim, "A"), B = .mp_score_side(sim, "B"), sim = sim)   # BOTH partners on a side get the same total
}

# FOURSOMES (alternate shot): blended team distribution (see header), ONE draw
# per team per hole (not best-of-two -- see mp_blend_dist()).
mp_simulate_foursomes <- function(distA1, distA2, distB1, distB2, n_sims = 8000L, max_holes = 18L, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  teamA_dist <- mp_blend_dist(distA1, distA2); teamB_dist <- mp_blend_dist(distB1, distB2)
  sA <- .mp_draw(teamA_dist, max_holes, n_sims); sB <- .mp_draw(teamB_dist, max_holes, n_sims)
  sim <- .mp_run_match(sA, sB, max_holes)
  list(A = .mp_score_side(sim, "A"), B = .mp_score_side(sim, "B"), sim = sim)
}

# ── one full SESSION (all matches of one type) -> per-player point stats ──────
mp_simulate_session <- function(type, matches, snap, n_sims = 8000L, seed = 2026L) {
  empty <- data.table(player_name = character(), team = character(), match = integer(),
                       mean_pts = numeric(), sd_pts = numeric(), p10 = numeric(), p90 = numeric(),
                       win_prob = numeric(), halve_prob = numeric(), loss_prob = numeric())
  if (!length(matches)) return(empty)
  fld <- attr(snap, "field")
  get_row <- function(nm) {
    r <- snap[name == nm]
    if (!nrow(r)) stop("matchplay_sim: player not found in roster/snapshot: '", nm, "'")
    r[1]
  }
  rows <- vector("list", length(matches))
  for (mi in seq_along(matches)) {
    mtch <- matches[[mi]]; usa <- mtch$team_usa; intl <- mtch$team_intl
    if (type == "singles") {
      r <- mp_simulate_singles(mp_hole_dist(get_row(usa[1]), fld), mp_hole_dist(get_row(intl[1]), fld),
                               n_sims = n_sims, seed = seed + mi)
      rows[[mi]] <- rbind(cbind(data.table(player_name = usa[1],  team = "USA",  match = mi), mp_summarize(r$A)),
                          cbind(data.table(player_name = intl[1], team = "INTL", match = mi), mp_summarize(r$B)))
    } else {
      dA1 <- mp_hole_dist(get_row(usa[1]), fld);  dA2 <- mp_hole_dist(get_row(usa[2]), fld)
      dB1 <- mp_hole_dist(get_row(intl[1]), fld); dB2 <- mp_hole_dist(get_row(intl[2]), fld)
      r <- if (type == "fourball") mp_simulate_fourball(dA1, dA2, dB1, dB2, n_sims = n_sims, seed = seed + mi)
           else                    mp_simulate_foursomes(dA1, dA2, dB1, dB2, n_sims = n_sims, seed = seed + mi)
      sA <- mp_summarize(r$A); sB <- mp_summarize(r$B)
      rows[[mi]] <- rbind(cbind(data.table(player_name = usa[1],  team = "USA",  match = mi), sA),
                          cbind(data.table(player_name = usa[2],  team = "USA",  match = mi), sA),
                          cbind(data.table(player_name = intl[1], team = "INTL", match = mi), sB),
                          cbind(data.table(player_name = intl[2], team = "INTL", match = mi), sB))
    }
  }
  rbindlist(rows)
}

# Neutral "average field" expected points per session type -- the FALLBACK for
# any session that has no announced pairings yet (a coin-flip matchup between
# two field-average distributions, since we don't know an unannounced session's
# real opponents). Clearly a placeholder, not a real projection.
mp_neutral_session_estimate <- function(snap, n_sims = 4000L) {
  fld <- attr(snap, "field")
  d <- mp_hole_dist(list(ceiling_rate_24 = fld$birdie, bogey_rate_24 = fld$bogey,
                         disaster_rate_24 = fld$dbl, data_ok = TRUE), fld)
  s  <- mp_simulate_singles(d, d, n_sims = n_sims, seed = 999L)$A
  fb <- mp_simulate_fourball(d, d, d, d, n_sims = n_sims, seed = 998L)$A
  fs <- mp_simulate_foursomes(d, d, d, d, n_sims = n_sims, seed = 997L)$A
  list(mean = list(singles = mean(s$total), fourball = mean(fb$total), foursomes = mean(fs$total)),
       var  = list(singles = stats::var(s$total), fourball = stats::var(fb$total), foursomes = stats::var(fs$total)))
}

# ── WEEKLY aggregate: sum each rostered player's projected points across every
# CONFIRMED match they're in + a neutral fallback for each not-yet-announced
# session -- see config/presidents_cup_pairings.R's header for the update
# workflow and the documented "everyone plays a pending session" assumption.
presidents_cup_project <- function(pairings, roster_names, n_sims = 8000L) {
  snap <- mp_player_snapshot(roster_names)
  neutral <- mp_neutral_session_estimate(snap, n_sims = min(n_sims, 4000L))
  agg <- data.table(player_name = roster_names, proj = 0, var_sum = 0,
                    sessions_confirmed = 0L, sessions_fallback = 0L)
  for (s in pairings$sessions) {
    type <- s$type
    announced <- identical(s$status, "announced") && length(s$matches) > 0L
    if (announced) {
      rows <- mp_simulate_session(type, s$matches, snap, n_sims = n_sims)
      for (p in roster_names) {
        r <- rows[player_name == p]
        if (nrow(r))
          agg[player_name == p, `:=`(proj = proj + r$mean_pts, var_sum = var_sum + r$sd_pts^2,
                                     sessions_confirmed = sessions_confirmed + 1L)]
        # else: this player sits the (known, real) session out -> +0, no fallback added
      }
    } else {
      m <- neutral$mean[[type]]; v <- neutral$var[[type]]
      agg[, `:=`(proj = proj + m, var_sum = var_sum + v, sessions_fallback = sessions_fallback + 1L)]
    }
  }
  agg[, sim_sd := sqrt(pmax(var_sum, 0))]
  agg[, `:=`(ceil = proj + 1.28 * sim_sd, floor = pmax(proj - 1.28 * sim_sd, 0))]
  agg[]
}

# ── DFS-ENGINE bridge: export golf_picks/dfs_presidents_cup_projections.rds ────
# Mirrors round_sim.R's export_round_projection() -> dfs_round_projections.rds
# pattern so sports/golf/adapter.R can read it the same way.
export_presidents_cup_projection <- function(cfg_path = file.path("config", "presidents_cup_pairings.R"),
                                             n_sims = 8000L) {
  if (!file.exists(cfg_path)) stop("matchplay_sim: pairings config not found at ", cfg_path)
  pe <- new.env(); sys.source(cfg_path, envir = pe)
  pairings <- pe$PRESIDENTS_CUP_PAIRINGS; roster <- pe$PRESIDENTS_CUP_ROSTER
  if (is.null(pairings) || is.null(roster))
    stop("matchplay_sim: ", cfg_path, " did not define PRESIDENTS_CUP_PAIRINGS / PRESIDENTS_CUP_ROSTER")
  agg <- presidents_cup_project(pairings, roster, n_sims = n_sims)
  out <- agg[, .(player_name, proj = round(proj, 2), sim_sd = round(pmax(sim_sd, 0.5), 3),
                ceil = round(ceil, 2), floor = round(pmax(floor, 0), 2), sessions_confirmed, sessions_fallback)]
  meta <- list(generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), date = as.character(Sys.Date()),
              event = pairings$meta$event %||% "Presidents Cup",
              sessions_confirmed_avg = round(mean(agg$sessions_confirmed), 2),
              sessions_fallback_avg  = round(mean(agg$sessions_fallback), 2),
              engine = "matchplay_sim v1")
  saveRDS(list(projections = out, meta = meta), file.path(OUT, "dfs_presidents_cup_projections.rds"))
  emsg(sprintf("wrote dfs_presidents_cup_projections.rds -- %d players, avg %.1f/5 sessions confirmed (%.1f fallback)",
              nrow(out), meta$sessions_confirmed_avg, meta$sessions_fallback_avg))
  invisible(out)
}

# ── CLI ────────────────────────────────────────────────────────────────────────
if (identical(environment(), globalenv()) && !nzchar(Sys.getenv("MPSIM_SOURCE_ONLY"))) {
  a <- commandArgs(trailingOnly = TRUE)
  if ("--export" %in% a) {
    export_presidents_cup_projection()
    quit(save = "no", status = 0)
  }
  emsg("=== matchplay_sim.R quick self-test (hypothetical matchups; no real pairings exist yet) ===")
  snap <- mp_player_snapshot(c("Scottie Scheffler", "Justin Thomas", "Hideki Matsuyama", "Nico Echavarria"))
  print(snap[, .(name, elo_pre = round(elo_pre), birdie_rate_24 = round(birdie_rate_24, 2),
                bogey_rate_24 = round(bogey_rate_24, 2), ceiling_rate_24 = round(ceiling_rate_24, 2),
                disaster_rate_24 = round(disaster_rate_24, 3), data_ok)])
  fld <- attr(snap, "field")
  cat("\n-- SINGLES: Scheffler (huge favorite) vs Echavarria --\n")
  r1 <- mp_simulate_singles(mp_hole_dist(snap[name == "Scottie Scheffler"], fld),
                            mp_hole_dist(snap[name == "Nico Echavarria"], fld), n_sims = 10000L, seed = 1L)
  print(rbind(Scheffler = as.list(mp_summarize(r1$A)), Echavarria = as.list(mp_summarize(r1$B))))
  cat("\n-- SINGLES: Scheffler vs Thomas (closer matchup) --\n")
  r2 <- mp_simulate_singles(mp_hole_dist(snap[name == "Scottie Scheffler"], fld),
                            mp_hole_dist(snap[name == "Justin Thomas"], fld), n_sims = 10000L, seed = 2L)
  print(rbind(Scheffler = as.list(mp_summarize(r2$A)), Thomas = as.list(mp_summarize(r2$B))))
  emsg("done -- run with --export to write the DFS-ENGINE bridge file (needs config/presidents_cup_pairings.R)")
}
