#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS v2 — engine/data.R  (Phase 0: data foundation)
#
# One clean per-player-event training table for the whole v2 engine, plus a
# round-grain table for hole-outcome calibration and a live-slate assembler.
#
# Design:
#   * Feature base   = golf_picks/rich_snapshots.rds ($snap): 48 leakage-free,
#     pre-event features (per-category trailing SG, scoring-shape rates, course).
#     Reused as-is (already lagged/round-1 state -> no leakage).
#   * Targets (NEW)  = realised per-CATEGORY event SG (t_ott/app/arg/putt/t2g)
#     + total, n_rounds, made_cut -- computed from rich_rounds_long.rds.
#     v1 only had the TOTAL target; v2 needs per-category to weight SG types.
#   * DFS labels     = golf_picks/dfs_raw_data.rds: salary, ownership, the six
#     decomposed DK components + total_pts (257 slates). Exact DK scoring:
#       hole  = 8*eob + 3*birdie + 0.5*par - 0.5*bogey - 1*double   (cor 1.000)
#       bonus = +3/bogey-free round, +5 all-4-under-70, +3/birdie-streak(3+),
#               +5 hole-in-one;  finish table 1st=30..41-50=1, 51+=0
#     (verified against the data — used to calibrate/validate the simulator.)
#
# Source: Sys.setenv(ENGINE_SOURCE_ONLY="1"); source("engine/data.R")
#   -> build_master(), build_round_shapes(), assemble_live_slate(), DK_FINISH_PTS,
#      dk_finish_pts(), MASTER_FEATS, SG_CATS
# Run:    Rscript engine/data.R      (builds + caches golf_picks/v2_master.rds)
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table) }))

if (Sys.getenv("ENGINE_WD_SET") == "") {
  if (dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
    setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
  Sys.setenv(ENGINE_WD_SET = "1")
}
emsg <- function(...) cat(format(Sys.time(), "[%H:%M:%S]"), ..., "\n", sep = " ")
OUT  <- "golf_picks"

SG_CATS   <- c("ott", "app", "arg", "putt")          # the four skill pillars
SG_CATS_ALL <- c("ott", "app", "arg", "putt", "t2g") # + tee-to-green rollup
MASTER_OUT <- file.path(OUT, "v2_master.rds")

# ── DK scoring constants (verified against dfs_raw_data.rds) ───────────────────
# finish/place bonus by finishing position (ties should split; live sim averages)
DK_FINISH_PTS <- local({
  v <- integer(0)
  v[1] <- 30L; v[2] <- 20L; v[3] <- 18L; v[4] <- 16L; v[5] <- 14L
  v[6] <- 12L; v[7] <- 10L; v[8] <- 9L;  v[9] <- 8L;  v[10] <- 7L
  v[11:15] <- 6L; v[16:20] <- 5L; v[21:25] <- 4L; v[26:30] <- 3L
  v[31:40] <- 2L; v[41:50] <- 1L
  v
})
# vectorised finish -> place points (0 for 51+ / NA)
dk_finish_pts <- function(pos) {
  pos <- as.integer(pos)
  out <- rep(0L, length(pos))
  ok  <- !is.na(pos) & pos >= 1L & pos <= 50L
  out[ok] <- DK_FINISH_PTS[pos[ok]]
  out
}
# per-round hole-scoring points from outcome counts (exact DK formula)
dk_hole_pts <- function(eob, birdie, par, bogey, dbl)
  8 * eob + 3 * birdie + 0.5 * par - 0.5 * bogey - 1 * dbl

# ── FanDuel scoring constants (verified vs FanDuel/RotoGrinders golf rules 2025) ─
# FD differs from DK: steeper bogey/double penalties, birdie 3.1 (vs 3), a top-heavy
# but SHALLOW finish table (nothing past 25th), a per-round "5+ birdies" (+4) bonus
# (DK has all-4-under-70 instead), bogey-free +5 (vs +3), and small streak(+0.6 per
# 2-in-a-row under par) / bounce-back(+0.3) micro-bonuses. No hole-in-one bonus.
#   per hole:  Eagle+ +7 | Birdie +3.1 | Par +0.5 | Bogey -1 | Double+ -3
# finish tiers: 1st=20, 2-5=12, 6-10=8, 11-25=5, else 0
FD_FINISH_PTS <- local({
  v <- integer(25)
  v[1] <- 20L; v[2:5] <- 12L; v[6:10] <- 8L; v[11:25] <- 5L
  v
})
fd_finish_pts <- function(pos) {
  pos <- as.integer(pos)
  out <- rep(0L, length(pos))
  ok  <- !is.na(pos) & pos >= 1L & pos <= 25L
  out[ok] <- FD_FINISH_PTS[pos[ok]]
  out
}
# per-round hole-scoring points from outcome counts (exact FanDuel formula)
fd_hole_pts <- function(eob, birdie, par, bogey, dbl)
  7 * eob + 3.1 * birdie + 0.5 * par - 1 * bogey - 3 * dbl

# ── loaders ───────────────────────────────────────────────────────────────────
.load_snap <- function() {
  rs <- readRDS(file.path(OUT, "rich_snapshots.rds"))
  snap <- as.data.table(rs$snap)
  snap[, `:=`(event_id = as.character(event_id), year = as.integer(year),
              player_id = as.integer(player_id))]
  attr(snap, "feats") <- rs$feats
  snap
}
.load_long <- function() {
  l <- as.data.table(readRDS(file.path(OUT, "rich_rounds_long.rds")))
  l[, `:=`(event_id = as.character(event_id), year = as.integer(year),
           player_id = as.integer(player_id))]
  l
}
.load_dfs <- function() {
  d <- as.data.table(readRDS(file.path(OUT, "dfs_raw_data.rds")))
  d[, `:=`(event_id = as.character(event_id), year = as.integer(year),
           player_id = as.integer(dg_id))]
  comp <- c("hole_score_pts","streak_pts","bogey_free_pts","sub_70_pts",
            "finish_pts","hole_in_one_pts","total_pts")
  keep <- d[!is.na(total_pts) & !is.na(salary) & salary > 0,
            c("player_id","event_id","year","player_name","salary","ownership",
              comp), with = FALSE]
  keep[, salary := as.numeric(salary)]
  keep[, ownership := as.numeric(ownership)]
  keep[, .SD[1], by = .(player_id, event_id, year)]   # one row per player-event
}

# ── MASTER: one row per player-event (features + per-category targets + DFS) ───
build_master <- function() {
  snap <- .load_snap(); feats <- attr(snap, "feats")
  long <- .load_long()

  # realised per-category event SG (the NEW targets) + cut/rounds
  tg <- long[!is.na(sg_total), .(
    t_sg   = mean(sg_total,  na.rm = TRUE),
    t_ott  = mean(sg_ott,    na.rm = TRUE),
    t_app  = mean(sg_app,    na.rm = TRUE),
    t_arg  = mean(sg_arg,    na.rm = TRUE),
    t_putt = mean(sg_putt,   na.rm = TRUE),
    t_t2g  = mean(sg_t2g,    na.rm = TRUE),
    n_rounds = .N,
    made_cut = as.integer(.N >= 3L),
    start_hole = as.integer(names(sort(table(start_hole), decreasing = TRUE))[1])
  ), by = .(player_id, event_id, year)]

  D <- merge(snap, tg, by = c("player_id","event_id","year"))

  # per-category CAREER baseline + sample count + 50-round trailing, as the state
  # ENTERING the event (round-1 snapshot, leakage-free via shift). These feed the
  # empirical-Bayes shrinkage in engine/skill.R (recent cat SG shrunk toward the
  # player's own long-run baseline by sample size, then toward the field).
  setorder(long, player_id, event_date, round_num)
  cb <- long[, c("player_id","event_id","year","event_date","round_num",
                 paste0("sg_", SG_CATS_ALL)), with = FALSE]
  for (c in SG_CATS_ALL) {
    sc <- paste0("sg_", c)
    cb[, cum := shift(cumsum(fcoalesce(get(sc), 0)), 1L), by = player_id]
    cb[, cnt := shift(cumsum(!is.na(get(sc))),      1L), by = player_id]
    cb[, paste0("car_", c) := fifelse(cnt > 0, cum / cnt, NA_real_)]
    cb[, paste0("cnt_", c) := cnt]
    cb[, paste0("r50_", c) := frollmean(shift(get(sc), 1L), 50, align = "right", na.rm = TRUE),
       by = player_id]
    cb[, c("cum","cnt") := NULL]
  }
  cbcols <- c(paste0("car_", SG_CATS_ALL), paste0("cnt_", SG_CATS_ALL),
              paste0("r50_", SG_CATS_ALL))
  D <- merge(D, cb[round_num == 1L, c("player_id","event_id","year", cbcols), with = FALSE],
             by = c("player_id","event_id","year"), all.x = TRUE)

  # player-course history, done RIGHT: expanding lagged mean of the player's realised
  # event SG at THIS course (leakage-free). NB rich_features' player_course_sg uses a
  # frollmean(.,20) that needs 20 visits -> it is all-NA; this replaces it.
  setorder(D, player_id, course_num, event_date)
  D[, pc_prior_n  := shift(cumsum(!is.na(t_sg)), 1L, fill = 0L), by = .(player_id, course_num)]
  D[, pc_prior_sg := shift(cumsum(fcoalesce(t_sg, 0)), 1L) /
                     shift(cumsum(!is.na(t_sg)),      1L), by = .(player_id, course_num)]
  D[!is.finite(pc_prior_sg), pc_prior_sg := NA_real_]

  # personalized Elo (leakage-free rating ENTERING the event) — the strongest single
  # skill predictor (OOS cor .329 vs next-event SG). Cached; computed if missing.
  elo <- if (file.exists(file.path(OUT, "v2_elo.rds")))
            as.data.table(readRDS(file.path(OUT, "v2_elo.rds")))
          else { Sys.setenv(ELO_SOURCE_ONLY = "1")
                 if (!exists("get_elo")) source("engine/elo.R"); get_elo() }
  D <- merge(D, elo[, .(player_id, event_id, year, elo_pre)],
             by = c("player_id","event_id","year"), all.x = TRUE)
  D[!is.finite(elo_pre), elo_pre := 1500]

  # field-relative per-category target (SG vs event field mean) — used later
  for (c in c("ott","app","arg","putt","t2g"))
    D[, paste0("tf_", c) := get(paste0("t_", c)) - mean(get(paste0("t_", c)), na.rm = TRUE),
      by = .(event_id, year)]

  # DFS labels (only DK-salaried events; NA elsewhere)
  dfs <- .load_dfs()
  D <- merge(D, dfs, by = c("player_id","event_id","year"), all.x = TRUE)

  setorder(D, year, event_date, event_id, player_id)
  attr(D, "feats") <- feats
  emsg("master:", nrow(D), "player-events |",
       uniqueN(paste(D$event_id, D$year)), "events |",
       sum(!is.na(D$salary)), "with DFS labels")
  D[]
}

# ── ROUND SHAPES: per-round outcome counts for hole-model calibration ─────────
# One row per player-round with the five outcome counts, the round score vs par,
# the round SG, and the player's TRAILING shape rates (entering the event) so the
# simulator can be calibrated: outcome mix as a function of round SG + skill.
build_round_shapes <- function() {
  long <- .load_long()
  rs <- long[!is.na(sg_total) & !is.na(score), .(
    player_id, event_id, year, event_date, round_num, start_hole,
    sg_total, sg_ott, sg_app, sg_arg, sg_putt,
    eob = eagles_or_better, birdie = birdies, par = pars,
    bogey = bogies, dbl = doubles_or_worse,
    score, course_par,
    to_par = score - course_par,
    hole_pts    = dk_hole_pts(eagles_or_better, birdies, pars, bogies, doubles_or_worse),
    hole_pts_fd = fd_hole_pts(eagles_or_better, birdies, pars, bogies, doubles_or_worse),
    bogey_free = as.integer((bogies + doubles_or_worse) == 0),
    under_70 = as.integer(score < 70),
    bird5    = as.integer(birdies >= 5)     # FanDuel: 5+ birdies in a round (+4)
  )]
  emsg("round shapes:", nrow(rs), "player-rounds")
  rs[]
}

# ── LIVE SLATE: field + salary + ownership + latest pre-event features ─────────
# Mirrors dfs_pipeline_v2::get_slate + attach_features but returns the v2 feature
# frame. DataGolf enrichments (skill decomp, tee times) are attached in data2.R
# (Phase 0b) via attach_dg_enrichments() if present.
assemble_live_slate <- function(tour = "pga", slate = "main", master = NULL) {
  if (is.null(master)) master <- build_master()
  feats <- attr(master, "feats")
  key <- Sys.getenv("DATAGOLF_API_KEY")
  stopifnot("Set DATAGOLF_API_KEY" = nzchar(key))
  dg <- function(endpoint, params = list()) {
    params$key <- key; params$file_format <- "json"
    url <- paste0("https://feeds.datagolf.com/", endpoint)
    resp <- tryCatch(httr2::request(url) |> httr2::req_url_query(!!!params) |>
                       httr2::req_perform(), error = function(e) NULL)
    if (is.null(resp) || httr2::resp_status(resp) != 200) return(NULL)
    httr2::resp_body_json(resp, simplifyVector = TRUE)
  }
  raw <- dg("preds/fantasy-projection-defaults",
            list(tour = tour, site = "draftkings", slate = slate))
  stopifnot("Could not fetch slate" = !is.null(raw) && "projections" %in% names(raw))
  p <- as.data.table(raw$projections)
  p[, `:=`(player_id = as.integer(dg_id), salary = as.numeric(salary),
           dg_proj = as.numeric(proj_points_total),
           own = pmax(fcoalesce(as.numeric(proj_ownership), 0.1), 0.1) / 100,
           dg_sd = suppressWarnings(as.numeric(std_dev)))]
  pl <- p[!is.na(salary) & salary > 0,
          .(player_id, player_name, salary, dg_proj, own, dg_sd)]

  # latest pre-event feature snapshot per player (already lagged in the snapshot).
  # carry the FULL set the skill/coursefit models read: rich feats + Elo + per-category
  # career anchors + course history (else they'd be median-filled live, losing signal).
  live_cols <- intersect(c(feats, "elo_pre", "car_ott","car_app","car_arg","car_putt",
                           "pc_prior_sg","pc_prior_n"), names(master))
  latest <- master[order(year, event_date),
                   .SD[.N], by = player_id][, c("player_id", live_cols), with = FALSE]
  sl <- merge(pl, latest, by = "player_id", all.x = TRUE)
  n_missing <- sum(is.na(sl$sg_last_24))
  if (n_missing) emsg(" ", n_missing, "field players lack history -> field-relative fill")
  if ("elo_pre" %in% names(sl)) sl[!is.finite(elo_pre), elo_pre := 1500]
  for (f in feats) {
    md <- median(sl[[f]], na.rm = TRUE); if (!is.finite(md)) md <- 0
    fallback <- if (grepl("^sg_", f)) md - 0.25 else md
    sl[is.na(get(f)), (f) := fallback]
  }
  list(event = raw$event_name, tour = tour, slate = slate,
       players = sl, feats = feats)
}

# ── build + cache ─────────────────────────────────────────────────────────────
if (!nzchar(Sys.getenv("ENGINE_SOURCE_ONLY"))) {
  emsg("=== engine/data.R — building v2 master table ===")
  D  <- build_master()
  saveRDS(list(master = D, feats = attr(D, "feats")), MASTER_OUT)
  emsg("saved", MASTER_OUT)
  rs <- build_round_shapes()
  saveRDS(rs, file.path(OUT, "v2_round_shapes.rds"))
  emsg("saved v2_round_shapes.rds")

  # ── self-test / coverage report ─────────────────────────────────────────────
  cat("\n== coverage ==\n")
  cat(sprintf("  player-events: %d | events: %d | years: %s\n",
      nrow(D), uniqueN(paste(D$event_id, D$year)), paste(range(D$year), collapse="-")))
  cat(sprintf("  with DFS labels: %d (%.0f%%) | made-cut rate: %.1f%%\n",
      sum(!is.na(D$salary)), 100*mean(!is.na(D$salary)), 100*mean(D$made_cut)))
  cat("  per-category target means (should track OTT/APP > ARG > PUTT spread):\n")
  cat(sprintf("    t_ott %.3f  t_app %.3f  t_arg %.3f  t_putt %.3f\n",
      mean(D$t_ott,na.rm=TRUE), mean(D$t_app,na.rm=TRUE),
      mean(D$t_arg,na.rm=TRUE), mean(D$t_putt,na.rm=TRUE)))
  # leakage tripwire: a trailing feature must NOT equal its realised target
  cat(sprintf("  leakage check cor(sg_app_24 trailing, t_app realised)=%.3f (expect <0.9)\n",
      cor(D$sg_app_24, D$t_app, use="complete.obs")))
  cat("DONE.\n")
}
