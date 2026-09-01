#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS v2 — engine/simulate.R  (Phase 4: tournament sim -> DK points, w/ CUT)
#
# Turns each player's projected per-round SG distribution (mu, round_sd from
# skill+coursefit+market) into a full DraftKings fantasy-point distribution by
# Monte-Carloing the whole tournament, WITH THE CUT. This is what makes v2's
# ceilings/floors/make-cut% real (no xgboost-2.0 quantile dependency) and correctly
# models that DK points hinge on surviving the cut.
#
# Per sim, per round:  SG_r ~ Normal(mu, round_sd) + a per-sim course-conditions
# shift (slate-wide boom/bust). Round SG -> DK points via the exact, data-calibrated
# maps (see engine/data.R for the scoring identity; coefficients fit on 103k rounds):
#     hole_pts  = a + b*SG + N(0, shape_sd)          [b~1.62; R2 .63]
#     bogey_free= 3 * Bernoulli(logit(af + bf*SG))
#     under_70  = Bernoulli(logit(au + su*SG + pu*par))     (per round)
#     streak    = 3 * Poisson(lambda(birdies(SG)))          [3+ birdie runs]
# CUT: rank 36-hole SG per sim; top cut_n (+ ties band) make it; missed-cut players
#   score 0 on R3-R4 and take NO finish bonus. finish_pts from the simulated 72-hole
#   leaderboard (exact DK place table). all-4-under-70 (+5) requires made cut.
#
# Source: Sys.setenv(ENGINE_SOURCE_ONLY="1"); source("engine/simulate.R")
#   -> calibrate_dk(), simulate_dk()
# Run:    Rscript engine/simulate.R   (calibration + validation vs historical DFS)
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
Sys.setenv(ENGINE_SOURCE_ONLY = "1")
if (!exists("dk_finish_pts")) source("engine/data.R")
emsg <- get0("emsg", ifnotfound = function(...) cat(..., "\n"))
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
OUT  <- "golf_picks"

# FanDuel micro-bonus lookup: E[0.6*(#runs>=2 under par) + 0.3*(#bounce-backs)] as a
# function of a round's under-par (u) and over-par (o) hole counts, under random hole
# placement (we only carry per-round outcome COUNTS, not the hole-by-hole sequence, so
# we MC the arrangement — same approach the DK streak LUT uses for 3+ birdie runs).
.fd_micro_lut <- function(seed = 11L, reps = 500L, maxc = 12L) {
  set.seed(seed); U <- 0:maxc; O <- 0:maxc
  M <- matrix(0, length(U), length(O))
  for (iu in seq_along(U)) for (io in seq_along(O)) {
    u <- U[iu]; o <- O[io]
    if (u + o > 18) { M[iu, io] <- NA_real_; next }
    M[iu, io] <- mean(replicate(reps, {
      s <- rep(0L, 18); idx <- sample.int(18)
      if (u > 0) s[idx[seq_len(u)]] <- 1L          #  1 = under par
      if (o > 0) s[idx[u + seq_len(o)]] <- -1L      # -1 = over par
      rr <- rle(s == 1L); nrun2 <- sum(rr$lengths[rr$values] >= 2)   # 2-in-a-row under
      bounce <- sum(s[-18] == -1L & s[-1] == 1L)                     # over then under
      0.6 * nrun2 + 0.3 * bounce
    }))
  }
  M
}

# ── calibrate the SG -> fantasy-points maps from the round-shape table ─────────
# site = "dk" (default; behaviour byte-identical to before) or "fd" (FanDuel re-score:
# fits the FD hole formula + FD-specific bonuses). Returns a `cal` tagged with $site.
calibrate_dk <- function(round_shapes = NULL, site = "dk") {
  R <- if (is.null(round_shapes)) as.data.table(readRDS(file.path(OUT, "v2_round_shapes.rds")))
       else as.data.table(round_shapes)
  holecol <- if (site == "fd") "hole_pts_fd" else "hole_pts"
  m_hole <- lm(reformulate("sg_total", holecol), data = R)
  g_bf   <- glm(bogey_free ~ sg_total, data = R, family = binomial())
  m_bird <- lm(birdie ~ sg_total, data = R)
  cal <- list(site = site,
       hole_int = coef(m_hole)[[1]], hole_b = coef(m_hole)[[2]],
       hole_sd = sd(resid(m_hole)),
       bf = coef(g_bf), bf_pts = if (site == "fd") 5 else 3,
       bird_int = coef(m_bird)[[1]], bird_b = coef(m_bird)[[2]],
       field_hole_mean = mean(R[[holecol]]), field_hole_sd = sd(R[[holecol]]))
  if (site == "fd") {
    # FD per-round bonus = 5+ birdies (+4); micro = streak/bounce; over-par count model
    cal$b5     <- coef(glm(bird5 ~ sg_total, data = R, family = binomial()))
    m_over     <- lm(I(bogey + dbl) ~ sg_total, data = R)
    cal$over_int <- coef(m_over)[[1]]; cal$over_b <- coef(m_over)[[2]]
    cal$micro_mat <- .fd_micro_lut()
  } else {
    cal$u70 <- coef(glm(under_70 ~ sg_total + course_par, data = R, family = binomial()))
    # streak: E[# of 3+ birdie runs | k birdies in 18 holes] via a quick MC lookup
    set.seed(11); cal$runs_lut <- vapply(0:16, function(k) {
      if (k < 3) return(0)
      mean(replicate(1500, {
        v <- rep(0L, 18); v[sample.int(18, min(k, 18))] <- 1L
        r <- rle(v); sum(r$lengths[r$values == 1] >= 3)   # count runs of >=3
      }))
    }, numeric(1))
  }
  cal
}

.logit <- function(x) 1 / (1 + exp(-x))

# per-round fantasy points from a round-SG matrix (P x S). Returns a list of P x S
# mats: $dk = round points, $u70 = under-70 flag (DK all-4-under-70 bonus; 0 for FD).
.round_points <- function(SG, cal, par) {
  P <- nrow(SG); S <- ncol(SG); n <- P * S
  v <- as.vector(SG); site <- cal$site %||% "dk"
  hole <- cal$hole_int + cal$hole_b * v + rnorm(n, 0, cal$hole_sd)
  hole <- pmax(hole, -6)                                   # realistic floor
  bf   <- (cal$bf_pts %||% 3) * rbinom(n, 1, .logit(cal$bf[[1]] + cal$bf[[2]] * v))
  bird <- pmax(0, round(cal$bird_int + cal$bird_b * v))
  if (site == "fd") {
    # FanDuel: 5+ birdies (+4) per round + micro streak/bounce; no under-70 bonus.
    b5   <- 4 * rbinom(n, 1, .logit(cal$b5[[1]] + cal$b5[[2]] * v))
    over <- pmin(pmax(round(cal$over_int + cal$over_b * v), 0), 12)
    ui   <- pmin(bird, 12) + 1L; oi <- over + 1L
    micro <- cal$micro_mat[cbind(ui, oi)]; micro[is.na(micro)] <- 0
    list(dk = matrix(hole + bf + b5 + micro, P, S), u70 = matrix(0L, P, S))
  } else {
    u70  <- rbinom(n, 1, .logit(cal$u70[[1]] + cal$u70[[2]] * v + cal$u70[[3]] * par))
    lam  <- cal$runs_lut[pmin(bird, 16) + 1L]
    strk <- 3 * rpois(n, lam)
    list(dk = matrix(hole + bf + strk, P, S), u70 = matrix(u70, P, S))
  }
}

# ── the tournament simulator -> per-player DK-point distribution ───────────────
# pool: data.table with player_id, mu (per-round SG), round_sd, and course_par.
# Returns pool + proj/floor/ceil/sim_sd/make_cut (+ optional sim matrix, cut rate).
simulate_dk <- function(pool, cal = NULL, n_sims = 4000L, cut_n = 65L,
                        cond_sd = 0.35, spread_scale = 1.0, seed = 1L, return_sims = FALSE) {
  if (is.null(cal)) cal <- calibrate_dk()
  set.seed(seed)
  pl <- as.data.table(copy(pool))
  pl <- pl[is.finite(mu) & is.finite(round_sd)]
  P  <- nrow(pl); S <- n_sims
  if (P < 3) stop("need >= 3 players")
  par <- if ("course_par" %in% names(pl)) stats::median(pl$course_par, na.rm=TRUE) else 71
  if (!is.finite(par)) par <- 71
  # spread_scale calibrates the win-TAIL: >1 widens (more longshot upside), <1 sharpens
  mu <- pl$mu; sd <- pmax(pl$round_sd, 0.4) * spread_scale; cond_sd <- cond_sd * spread_scale

  draw <- function() {
    m <- matrix(rnorm(P * S, mu, sd), P, S)                # per-player, per-sim
    m + rep(rnorm(S, 0, cond_sd), each = P)                # slate-wide conditions shift
  }
  R1 <- draw(); R2 <- draw(); R3 <- draw(); R4 <- draw()

  # cut on 36-hole SG (higher = better). top cut_n incl. a small ties band.
  h36 <- R1 + R2
  made <- apply(h36, 2L, function(col) {
    thr <- sort(col, decreasing = TRUE)[min(cut_n, length(col))]
    col >= thr - 1e-9
  })

  p1 <- .round_points(R1, cal, par); p2 <- .round_points(R2, cal, par)
  p3 <- .round_points(R3, cal, par); p4 <- .round_points(R4, cal, par)

  site <- cal$site %||% "dk"
  dk <- p1$dk + p2$dk + made * (p3$dk + p4$dk)             # weekend only if made cut
  if (site != "fd") {                                      # DK-only tournament bonuses
    # all-4-under-70 (+5): needs made cut AND all four rounds < 70
    all4 <- (p1$u70 == 1) & (p2$u70 == 1) & (p3$u70 == 1) & (p4$u70 == 1)
    dk <- dk + 5 * (made & all4)
    # hole-in-one (+5), rare (FanDuel has no hole-in-one bonus)
    dk <- dk + 5 * matrix(rbinom(P * S, 1, 0.006), P, S)
  }

  # finish rank from the simulated 72-hole leaderboard (missed cut sunk to bottom)
  score <- h36 + R3 + R4
  score[!made] <- h36[!made] - 1000
  rnk <- apply(score, 2L, function(col) frank(-col, ties.method = "first"))   # P x S
  finish_fn <- if (site == "fd") fd_finish_pts else dk_finish_pts              # site place table
  dk  <- dk + matrix(finish_fn(as.vector(rnk)), nrow(rnk), ncol(rnk))          # place bonus

  q <- function(m, p) apply(m, 1L, stats::quantile, probs = p, names = FALSE)
  pl[, `:=`(proj     = rowMeans(dk),
            floor    = q(dk, 0.10),
            ceil     = q(dk, 0.90),
            sim_sd   = apply(dk, 1L, sd),
            make_cut = rowMeans(made),
            p_win    = rowMeans(rnk == 1L),      # outright win / top-N probabilities
            p_top5   = rowMeans(rnk <= 5L),
            p_top10  = rowMeans(rnk <= 10L),
            p_top20  = rowMeans(rnk <= 20L))]
  out <- list(players = pl[], cut_n = cut_n, n_sims = S)
  if (return_sims) { rownames(dk) <- as.character(pl$player_id); out$sims <- dk }
  out
}

# ── calibration + validation vs historical DFS ────────────────────────────────
if (identical(environment(), globalenv()) && !nzchar(Sys.getenv("SIMULATE_SOURCE_ONLY"))) {
  emsg("=== engine/simulate.R — calibrate + validate the DK-point simulator ===")
  cal <- calibrate_dk()
  cat(sprintf("hole_pts = %.2f + %.3f*SG (+/- %.2f) | bogey_free logit %.2f+%.2f*SG\n",
      cal$hole_int, cal$hole_b, cal$hole_sd, cal$bf[[1]], cal$bf[[2]]))
  cat(sprintf("streak E[3+runs] at 4/6/8 birdies: %.2f / %.2f / %.2f  (=> pts x3)\n",
      cal$runs_lut[5], cal$runs_lut[7], cal$runs_lut[9]))
  saveRDS(cal, file.path(OUT, "v2_sim_calib.rds"))

  # VALIDATION: feed players' REALISED per-round SG as mu (perfect skill) and check
  # the simulated DK-point distribution reproduces the ACTUAL DK points + components.
  M <- as.data.table(readRDS(file.path(OUT, "v2_master.rds"))$master)
  R <- as.data.table(readRDS(file.path(OUT, "v2_round_shapes.rds")))
  # course par per event (median)
  parE <- R[, .(course_par = stats::median(course_par, na.rm=TRUE)), by=.(event_id, year)]
  set.seed(3); evs <- M[!is.na(total_pts) & made_cut %in% c(0,1),
                        unique(.SD), .SDcols=c("event_id","year")][sample(.N, 12)]
  agg <- data.table()
  for (i in seq_len(nrow(evs))) {
    e <- M[event_id == evs$event_id[i] & year == evs$year[i] & is.finite(t_sg) & !is.na(salary)]
    if (nrow(e) < 40) next
    e[, `:=`(mu = t_sg, round_sd = pmax(sg_sd_24, 1.5))]     # realised skill as mu
    e[, course_par := parE[.(evs$event_id[i], evs$year[i]), course_par, on=c("event_id","year")]]
    sim <- simulate_dk(e, cal, n_sims = 1500L, seed = i)$players
    m <- merge(sim[, .(player_id, sim_proj = proj, sim_mc = make_cut)],
               e[, .(player_id, act_pts = total_pts, act_mc = made_cut)], by="player_id")
    agg <- rbind(agg, m)
  }
  cat(sprintf("\nVALIDATION over %d player-events in 12 events:\n", nrow(agg)))
  cat(sprintf("  actual DK pts   mean %.1f | simulated mean %.1f  (bias %+.1f)\n",
      mean(agg$act_pts), mean(agg$sim_proj), mean(agg$sim_proj) - mean(agg$act_pts)))
  cat(sprintf("  cor(sim_proj, actual_pts) = %.3f\n", cor(agg$sim_proj, agg$act_pts)))
  cat(sprintf("  make-cut calibration: sim mean %.1f%% vs actual %.1f%%\n",
      100*mean(agg$sim_mc), 100*mean(agg$act_mc)))
  emsg("saved v2_sim_calib.rds ; DONE")
}
