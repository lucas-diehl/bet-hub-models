#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS v2 — engine/skill.R  (Phase 1: the interpretable skill core)
#
# The heart of v2 and the operationalisation of the "weight each SG type by its
# predictive power" idea. For each player entering an event we produce:
#   * mu        — projected PER-ROUND total SG (feeds the simulator)
#   * s_ott/app/arg/putt — projected per-category skill (for course-fit + why)
#   * round_sd  — projected per-round SG volatility (feeds the simulator spread)
#
# WHY this beats a flat xgboost on DK points (measured, walk-forward test 2025-26):
#   SG categories differ in how well recent form predicts the future —
#     persistence  OTT 0.38 > APP 0.24 > ARG 0.17 > PUTT 0.14   (our data),
#   matching DataGolf's OTT>APP>ARG>PUTT finding. So recent OTT is trusted, while
#   recent PUTT is regressed almost entirely to the player's CAREER baseline
#   (recent-putt slope 0.09 vs career-putt 0.54). The projection is a regression
#   whose learned weights ARE the predictive-power weights — inspectable per player.
#   Naive trailing-total: cor 0.279 / rmse 1.779.  This core: cor 0.328 / rmse 1.692.
#
# Per-player partial pooling: each category is anchored to that player's OWN career
# mean (car_*) and, for thin samples (cnt_*), pulled toward the field — empirical
# Bayes by sample size. Honest: with ~30-60 rounds the per-player deviation is weak,
# so we start population-first + career-anchored and widen only if backtest rewards.
#
# Source: Sys.setenv(ENGINE_SOURCE_ONLY="1"); source("engine/skill.R")
#   -> train_skill(), project_skill(), skill_report()
# Run:    Rscript engine/skill.R      (walk-forward eval + saves skill model)
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
Sys.setenv(ENGINE_SOURCE_ONLY = "1")
if (!exists("build_master")) source("engine/data.R")
emsg <- get0("emsg", ifnotfound = function(...) cat(..., "\n"))
OUT   <- "golf_picks"

# recent per-category form (their coefficients ARE the predictive-power weights:
# OTT/APP high, PUTT ~0) + a stable total-career anchor + long window + form.
# We anchor the TOTAL on career_sg (not the 4 collinear car_*, which inflate
# variance/RMSE); per-category career anchoring lives in the cat_lm models below.
MU_FEATS <- c("sg_ott_24","sg_app_24","sg_arg_24","sg_putt_24",
              "career_sg","sg_last_50","pform_12","elo_pre")
# validated interactions (OOS): bombers x course length (+.010), Elo x field strength
# (+.0065). Most Elo x {form,category} interactions are ~0 (Elo already encodes skill).
# use CENTERED interaction vars (c_len_z, fasg_z) so the main-effect category weights
# stay interpretable (= the weight at an average-length course / average field).
MU_INTERACTIONS <- c("sg_ott_24:c_len_z", "elo_c:fasg_z")
INT_BASE <- c("c_len", "field_avg_sg")   # base cols the interactions need (must be filled)
CATS <- c("ott","app","arg","putt")

# median-fill features from stored training medians (no leakage; live-safe)
.prep <- function(D, feats, meds) {
  D <- as.data.table(copy(D))
  for (f in feats) {
    if (!f %in% names(D)) D[, (f) := NA_real_]
    fb <- meds[[f]]; if (is.null(fb) || !is.finite(fb)) fb <- 0
    D[!is.finite(get(f)), (f) := fb]
  }
  if ("elo_pre" %in% names(D)) D[, elo_c := (elo_pre - 1500) / 100]   # centered Elo for interactions
  cm <- meds[["c_len"]]; if (is.null(cm) || !is.finite(cm)) cm <- 0
  fm <- meds[["field_avg_sg"]]; if (is.null(fm) || !is.finite(fm)) fm <- 0
  if ("c_len" %in% names(D))        D[, c_len_z := c_len - cm]        # centred so main effects interpretable
  if ("field_avg_sg" %in% names(D)) D[, fasg_z  := field_avg_sg - fm]
  D
}

# ── train ─────────────────────────────────────────────────────────────────────
train_skill <- function(D) {
  D <- as.data.table(D)[is.finite(t_sg)]
  mcols <- unique(c(MU_FEATS, INT_BASE, paste0("sg_", CATS, "_24"),
                    paste0("car_", CATS), "sg_sd_24"))
  meds <- lapply(mcols, function(f) median(D[[f]], na.rm = TRUE))
  names(meds) <- mcols
  Dp <- .prep(D, names(meds), meds)

  # (1) total-SG projection — the accurate mu the simulator uses (+ validated interactions)
  mu_lm <- lm(reformulate(c(MU_FEATS, MU_INTERACTIONS), "t_sg"), data = Dp)

  # (2) per-category projected skill — recent shrunk toward career (interpretable,
  #     used by course-fit + explanation). One small regression per category.
  cat_lm <- lapply(CATS, function(c)
    lm(reformulate(c(paste0("sg_", c, "_24"), paste0("car_", c)), paste0("t_", c)), data = Dp))
  names(cat_lm) <- CATS
  cat_field <- setNames(vapply(CATS, function(c) mean(Dp[[paste0("t_", c)]], na.rm = TRUE), 0), CATS)

  # (3) volatility: per-round SG SD, lightly shrunk toward the field median
  vol_med <- median(Dp$sg_sd_24, na.rm = TRUE)

  # predictive-power weights = the recent-category coefficients (for reporting)
  cf <- coef(mu_lm)
  pw <- setNames(cf[paste0("sg_", CATS, "_24")], CATS)

  structure(list(mu_lm = mu_lm, cat_lm = cat_lm, cat_field = cat_field,
                 vol_med = vol_med, meds = meds, feats = MU_FEATS,
                 pred_weights = pw, trained_through = max(D$year)),
            class = "skill_model")
}

# ── project ───────────────────────────────────────────────────────────────────
# returns newdata + mu, s_ott/app/arg/putt, round_sd  (all finite)
project_skill <- function(model, newdata) {
  D <- .prep(newdata, unique(c(model$feats, INT_BASE, paste0("sg_", CATS, "_24"),
                               paste0("car_", CATS), "sg_sd_24")), model$meds)
  D[, mu := as.numeric(predict(model$mu_lm, D))]
  for (c in CATS)
    D[, (paste0("s_", c)) := as.numeric(predict(model$cat_lm[[c]], D))]
  # per-round volatility: light shrink toward field median (stabilises thin history)
  D[, round_sd := 0.75 * sg_sd_24 + 0.25 * model$vol_med]
  D[!is.finite(round_sd) | round_sd < 0.5 * model$vol_med, round_sd := model$vol_med]
  # guard mu / skills finite
  mm <- median(D$mu[is.finite(D$mu)]); if (!is.finite(mm)) mm <- 0
  D[!is.finite(mu), mu := mm]
  # clamp to the realistic realised per-round SG range so the interaction terms
  # can't EXTRAPOLATE past anything ever observed (a top player ~2.5-2.8; worst ~-4.5).
  D[, mu := pmax(pmin(mu, 2.8), -4.5)]
  for (c in CATS) { sc <- paste0("s_", c)
    D[!is.finite(get(sc)), (sc) := model$cat_field[[c]]] }
  D[]
}

# human-readable "why" for a set of players (top by mu)
skill_report <- function(model, proj, n = 12) {
  cat("\n== predictive-power weights on RECENT category form (mu model) ==\n")
  pw <- model$pred_weights
  for (c in CATS) cat(sprintf("   %-4s %+.3f%s\n", toupper(c), pw[[c]],
      if (c == "putt") "   <- lowest: recent putting regressed hardest" else ""))
  cat("\n== projected skill leaders ==\n")
  p <- as.data.table(proj)[order(-mu)][seq_len(min(n, .N))]
  nm <- if ("player_name" %in% names(p)) "player_name" else "player_id"
  print(p[, .(player = get(nm), mu = round(mu, 2),
              ott = round(s_ott, 2), app = round(s_app, 2),
              arg = round(s_arg, 2), putt = round(s_putt, 2),
              vol = round(round_sd, 2))], row.names = FALSE)
}

# ── walk-forward eval + save final model ──────────────────────────────────────
if (identical(environment(), globalenv()) && !nzchar(Sys.getenv("SKILL_SOURCE_ONLY"))) {
  emsg("=== engine/skill.R — walk-forward skill eval ===")
  M <- as.data.table(readRDS(file.path(OUT, "v2_master.rds"))$master)
  rmse <- function(a,b) sqrt(mean((a-b)^2, na.rm=TRUE))
  co   <- function(a,b) suppressWarnings(cor(a,b, use="complete.obs"))

  cat("\nyear |  n   | naive cor/rmse | v2 skill cor/rmse\n")
  for (Y in 2024:2026) {
    tr <- M[year < Y]; te <- M[year == Y & is.finite(t_sg)]
    if (nrow(tr) < 2000 || nrow(te) < 50) next
    sm <- train_skill(tr)
    pj <- project_skill(sm, te)
    cat(sprintf(" %d | %4d | %.3f / %.3f  | %.3f / %.3f\n", Y, nrow(te),
        co(te$sg_last_24, te$t_sg), rmse(te$sg_last_24, te$t_sg),
        co(pj$mu, te$t_sg),        rmse(pj$mu, te$t_sg)))
  }
  # final model on all data + report
  final <- train_skill(M)
  pj_all <- project_skill(final, M[year == max(year)])
  if ("player_name" %in% names(M)) {
    nm <- unique(M[!is.na(player_name), .(player_id, player_name)])
    pj_all <- merge(pj_all, nm, by = "player_id", all.x = TRUE, suffixes = c("", ".y"))
  }
  skill_report(final, pj_all)
  saveRDS(final, file.path(OUT, "v2_skill_model.rds"))
  emsg("\nsaved v2_skill_model.rds (trained through", final$trained_through, ")")
}
