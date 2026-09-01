#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS v2 — engine/coursefit.R  (Phase 2: course fit + conditions)
#
# The base skill projection (engine/skill.R -> mu) is course-BLIND. This layer
# adds a course-fit delta so a player's category strengths are rewarded where the
# course rewards them ("horses for courses"), and mixes in the player's own history
# at the venue. It answers a different question than skill.R: not "how good is this
# player" but "how much better/worse than baseline at THIS course".
#
# adj_mu = mu + cf_delta,  where cf_delta is a fitted residual model:
#     resid = t_sg - mu   (mu is course-blind, so this residual carries the course
#                          signal in full)  ~
#       pc_hist                         player's shrunk SG history at this course
#     + course_fit (heuristic)          the v1 static fit score (as a prior feature)
#     + demand x skill interactions     long course x OTT, tough greens x APP,
#                                        rough x accuracy, easy/birdie x putting/ARG
#
# Also: k-means COURSE ARCHETYPES (length/rough/proximity/difficulty/GIR) so thin-
# history venues (majors, new courses) inherit a sensible demand profile, and an
# IDEAL-PROFILE similarity score (reference method: match a field player's skill
# shape to the course's historical top-finisher shape).
#
# Conditions (weather / AM-PM wave) are LIVE-only (historical teetime is NA in the
# round data), so they are applied at projection time via apply_conditions(), not
# fit here. See engine/project.R.
#
# Source: Sys.setenv(ENGINE_SOURCE_ONLY="1"); source("engine/coursefit.R")
#   -> train_coursefit(), project_coursefit(), course_archetypes(), apply_conditions()
# Run:    Rscript engine/coursefit.R    (walk-forward: does course fit beat no-fit?)
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
Sys.setenv(ENGINE_SOURCE_ONLY = "1", SKILL_SOURCE_ONLY = "1")
if (!exists("build_master")) source("engine/data.R")
if (!exists("train_skill"))  source("engine/skill.R")
emsg <- get0("emsg", ifnotfound = function(...) cat(..., "\n"))
OUT  <- "golf_picks"

.zc <- function(x) { x <- as.numeric(x); m <- mean(x, na.rm=TRUE); s <- sd(x, na.rm=TRUE)
  if (!is.finite(s) || s == 0) rep(0, length(x)) else (x - m) / s }
PC_SHRINK <- 8   # player-course history shrinkage (rounds-equivalent prior)

# course attribute columns already in the master (static per course)
COURSE_ATTR <- c("c_len","c_rough","c_prox","c_score","c_gir")

# build the cf feature columns on a table that already has mu + s_ott/app/arg/putt
.cf_features <- function(D) {
  D <- as.data.table(copy(D))
  # live pools carry only the snapshot feats -> default any missing course cols
  for (cc in c("pc_prior_n","pc_prior_sg","course_fit", COURSE_ATTR))
    if (!cc %in% names(D)) D[, (cc) := NA_real_]
  # player's shrunk SG history at the course ("plays well here"), from the
  # corrected expanding-mean columns in engine/data.R (pc_prior_sg/pc_prior_n)
  D[, pc_n := fifelse(is.finite(pc_prior_n), pc_prior_n, 0)]
  D[, pc_hist := fifelse(is.finite(pc_prior_sg) & pc_n > 0,
                         pc_prior_sg * pc_n / (pc_n + PC_SHRINK), 0)]
  # course demand z (static) and player skill z (relative to the field this event)
  # (strip the "c_" prefix so the columns are z_len/z_rough/z_prox/z_score/z_gir)
  for (a in COURSE_ATTR) D[, paste0("z_", sub("^c_", "", a)) := .zc(get(a))]
  for (c in c("ott","app","arg","putt"))
    D[, paste0("zs_", c) := .zc(get(paste0("s_", c)))]
  D[, zs_acc  := .zc(driving_acc_24)]
  D[, zs_wild := .zc(wildness_cost_24)]
  # demand x skill interactions (what each course archetype rewards)
  D[, i_len_ott  := z_len   * zs_ott]                 # bombers' paradise
  D[, i_diff_app := z_score * zs_app]                 # hard scoring -> approach
  D[, i_prox_app := z_prox  * zs_app]                 # long approaches -> approach
  D[, i_rough_acc:= z_rough * zs_acc]                 # thick rough -> accuracy
  D[, i_rough_wild := z_rough * zs_wild]              # thick rough punishes wildness
  D[, i_easy_putt := (-z_score) * zs_putt]            # birdie-fests -> putting
  D[, i_rough_arg := z_rough * zs_arg]                # gnarly greens -> scrambling
  D[!is.finite(course_fit), course_fit := 0]
  D
}
CF_TERMS <- c("pc_hist","course_fit","i_len_ott","i_diff_app","i_prox_app",
              "i_rough_acc","i_rough_wild","i_easy_putt","i_rough_arg")

# ── course archetypes (k-means on course-profile attributes) ──────────────────
course_archetypes <- function(D, k = 6L) {
  cp <- unique(as.data.table(D)[is.finite(c_len),
              c("course_num", COURSE_ATTR), with = FALSE], by = "course_num")
  X  <- scale(as.matrix(cp[, COURSE_ATTR, with = FALSE]))
  X[!is.finite(X)] <- 0
  set.seed(7); km <- kmeans(X, centers = min(k, nrow(cp) - 1L), nstart = 10)
  cp[, archetype := km$cluster]
  list(map = cp[, .(course_num, archetype)], centers = km$centers)
}

# ── ideal-profile similarity (reference method) ───────────────────────────────
# per course: the category-SG shape of historical TOP-20 finishers -> score each
# player's projected category shape by cosine similarity to it.
ideal_profile_sim <- function(train, newdata) {
  tp <- as.data.table(train)[finish <= 20 & is.finite(t_ott)]
  prof <- tp[, .(p_ott = mean(t_ott), p_app = mean(t_app),
                 p_arg = mean(t_arg), p_putt = mean(t_putt)), by = course_num]
  nd <- merge(as.data.table(copy(newdata)), prof, by = "course_num", all.x = TRUE)
  cos <- function(a, b) { d <- sqrt(rowSums(a^2)) * sqrt(rowSums(b^2))
    ifelse(d > 0, rowSums(a * b) / d, 0) }
  A <- as.matrix(nd[, .(s_ott, s_app, s_arg, s_putt)])
  B <- as.matrix(nd[, .(p_ott, p_app, p_arg, p_putt)]); B[!is.finite(B)] <- 0
  nd[, ideal_sim := cos(A, B)]
  nd[!is.finite(ideal_sim), ideal_sim := 0]
  nd$ideal_sim
}

# ── train / project the course-fit residual model ─────────────────────────────
train_coursefit <- function(D_mu) {
  D <- .cf_features(D_mu)
  D[, resid := t_sg - mu]
  D <- D[is.finite(resid)]
  m <- lm(reformulate(CF_TERMS, "resid"), data = D)
  structure(list(lm = m, terms = CF_TERMS), class = "coursefit_model")
}
project_coursefit <- function(model, newdata_mu) {
  D <- .cf_features(newdata_mu)
  D[, cf_delta := as.numeric(predict(model$lm, D))]
  D[!is.finite(cf_delta), cf_delta := 0]
  # cap the adjustment so course fit can't dominate the skill projection
  D[, cf_delta := pmax(pmin(cf_delta, 0.6), -0.6)]
  D[, adj_mu := mu + cf_delta]
  D[]
}

# ── conditions (LIVE): weather / AM-PM wave shift on mu + variance ────────────
# wave: named list or data.table(player_id, wave_shift, vol_mult). No-op if absent.
apply_conditions <- function(D, wave = NULL) {
  D <- as.data.table(copy(D))
  if (is.null(wave)) { D[, `:=`(wave_shift = 0, vol_mult = 1)]; return(D[]) }
  w <- as.data.table(wave)
  D <- merge(D, w, by = "player_id", all.x = TRUE)
  D[!is.finite(wave_shift), wave_shift := 0]
  D[!is.finite(vol_mult),   vol_mult := 1]
  D[, adj_mu := adj_mu + wave_shift]
  D[, round_sd := round_sd * vol_mult]
  D[]
}

# ── LIVE course attachment (fixes the stale "last event's course" bug) ────────
# The live snapshot carries each player's LAST event's course profile + course
# history -- wrong for the CURRENT event. Overwrite BOTH with the current course:
# its demand profile (c_*), and each player's history AT THIS course (pc_prior).
resolve_course_num <- function(event_id, master) {
  m <- as.data.table(master)[get("event_id") == event_id & is.finite(course_num)]
  if (!nrow(m)) return(NA_integer_)
  as.integer(names(sort(table(m$course_num), decreasing = TRUE))[1])
}
attach_current_course <- function(pool, course_num, master) {
  pool <- as.data.table(copy(pool))
  if (is.na(course_num)) return(pool)                 # new venue -> keep field-avg
  M <- as.data.table(master); cn <- course_num
  cp <- M[course_num == cn, lapply(.SD, mean, na.rm = TRUE),
          .SDcols = intersect(COURSE_ATTR, names(M))]
  for (col in names(cp)) if (is.finite(cp[[col]][1])) pool[, (col) := cp[[col]][1]]
  hist <- M[course_num == cn & is.finite(t_sg),
            .(pc_prior_sg = mean(t_sg), pc_prior_n = .N), by = player_id]
  for (col in c("pc_prior_sg","pc_prior_n")) if (col %in% names(pool)) pool[, (col) := NULL]
  pool <- merge(pool, hist, by = "player_id", all.x = TRUE)
  pool[!is.finite(pc_prior_n), pc_prior_n := 0L]
  pool[]
}

# ── walk-forward: does course fit beat no-fit? ────────────────────────────────
if (identical(environment(), globalenv()) && !nzchar(Sys.getenv("COURSEFIT_SOURCE_ONLY"))) {
  emsg("=== engine/coursefit.R — walk-forward course-fit eval ===")
  M <- as.data.table(readRDS(file.path(OUT, "v2_master.rds"))$master)
  rmse <- function(a,b) sqrt(mean((a-b)^2, na.rm=TRUE))
  co   <- function(a,b) suppressWarnings(cor(a,b, use="complete.obs"))

  cat("\nyear |  n   | base mu cor/rmse | +coursefit cor/rmse | +ideal_sim gain\n")
  for (Y in 2024:2026) {
    tr <- M[year < Y]; te <- M[year == Y & is.finite(t_sg)]
    if (nrow(tr) < 2000 || nrow(te) < 50) next
    sm  <- train_skill(tr)
    trp <- project_skill(sm, tr); tep <- project_skill(sm, te)
    cfm <- train_coursefit(trp)
    tef <- project_coursefit(cfm, tep)
    # ideal-profile similarity as an extra ensemble term
    tef[, isim := ideal_profile_sim(trp, tef)]
    lm_i <- lm(I(t_sg - adj_mu) ~ isim, data = tef)
    adj2 <- tef$adj_mu + predict(lm_i, tef)
    cat(sprintf(" %d | %4d | %.3f / %.3f    | %.3f / %.3f       | %+.4f cor\n", Y, nrow(te),
        co(tep$mu, te$t_sg), rmse(tep$mu, te$t_sg),
        co(tef$adj_mu, te$t_sg), rmse(tef$adj_mu, te$t_sg),
        co(adj2, te$t_sg) - co(tef$adj_mu, te$t_sg)))
  }
  # final course-fit model (on all data, over the final skill model's projections)
  sm  <- readRDS(file.path(OUT, "v2_skill_model.rds"))
  Mp  <- project_skill(sm, M)
  cfm <- train_coursefit(Mp)
  cat("\n== course-fit residual weights (which demands matter) ==\n")
  print(round(coef(cfm$lm), 3))
  arch <- course_archetypes(M)
  cat(sprintf("\ncourse archetypes: %d clusters over %d courses\n",
      length(unique(arch$map$archetype)), nrow(arch$map)))
  saveRDS(list(model = cfm, archetypes = arch), file.path(OUT, "v2_coursefit_model.rds"))
  emsg("saved v2_coursefit_model.rds")
}
