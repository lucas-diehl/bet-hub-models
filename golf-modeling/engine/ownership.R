#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS v2 — engine/ownership.R
# Our OWN ownership model (validated cor 0.81 per-slate vs actual, beating salary-
# only 0.71). Used where DataGolf gives no ownership (single-round R2, opposite-field)
# instead of a hand-tuned synthetic, and blendable with DG's proj_ownership on main
# slates. Plus DG-vs-actual logging so we can A/B our model against DataGolf.
#
#   train_ownership_model(master) -> golf_picks/v2_own_model.rds
#   predict_ownership(pool)       -> ownership fractions (sum ~ roster spots)
#   log_dg_ownership(tour)        -> append DG proj_ownership + event to a log (A/B)
#
# Source: Sys.setenv(ENGINE_SOURCE_ONLY="1"); source("engine/ownership.R")
# Run:    Rscript engine/ownership.R   (train + walk-forward report + save)
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
Sys.setenv(ENGINE_SOURCE_ONLY="1", SKILL_SOURCE_ONLY="1")
if (!exists("project_skill")) source("engine/skill.R")
if (!exists(".EKEY"))         source("engine/enrich.R")
emsg <- get0("emsg", ifnotfound=function(...) cat(..., "\n")); OUT <- "golf_picks"
OWN_MODEL <- file.path(OUT, "v2_own_model.rds")
N_ROSTER  <- 6L

# ── player-style archetype (k-means on career SG shape) ───────────────────────
# Adds a SMALL but consistent, behaviour-motivated ownership signal: bombers /
# ball-strikers draw name-brand chalk (over-owned vs salary+proj), and archetype
# chalk shifts with course length. No lift in the POINTS model (continuous SG
# already encodes it) — ownership only. Style feats fall back gracefully.
SFEAT <- c("rel_ott","rel_app","rel_arg","rel_putt","driving_dist_24","driving_acc_24")
.style_rel <- function(D) { D[, m4 := (car_ott+car_app+car_arg+car_putt)/4]
  D[, `:=`(rel_ott=car_ott-m4, rel_app=car_app-m4, rel_arg=car_arg-m4, rel_putt=car_putt-m4)]; D }
.build_style <- function(M, k = 6L) {
  S <- .style_rel(as.data.table(copy(M)))
  pr <- S[order(event_date)][, .SD[.N], by=player_id][
    is.finite(rel_ott)&is.finite(driving_dist_24)&is.finite(driving_acc_24)&cnt_putt>=20]
  X <- as.matrix(pr[, ..SFEAT]); ctr <- colMeans(X); sca <- apply(X,2,sd)
  set.seed(42); km <- kmeans(scale(X,ctr,sca), centers=k, nstart=50, iter.max=100)
  list(centers=km$centers, ctr=ctr, sca=sca, k=k,
       modal=as.integer(names(sort(table(km$cluster),decreasing=TRUE))[1]),
       c_len_med=median(M$c_len, na.rm=TRUE))
}
# assign each row to its nearest style centroid from as-of career SG. Thin-history
# players (no stable profile) get level "0" = the reference, so they keep the BASE
# prediction and only classifiable players receive the archetype adjustment.
.style_assign <- function(D, st) {
  D <- .style_rel(as.data.table(copy(D)))
  cl <- rep(0L, nrow(D))
  ok <- is.finite(D$rel_ott)&is.finite(D$driving_dist_24)&is.finite(D$driving_acc_24)
  if (any(ok)) { Z <- scale(as.matrix(D[ok, ..SFEAT]), st$ctr, st$sca)
    dm <- sapply(seq_len(nrow(st$centers)), function(j)
      rowSums((Z - matrix(st$centers[j,], nrow(Z), ncol(Z), byrow=TRUE))^2))
    cl[ok] <- max.col(-dm) }
  factor(cl, levels=c(0L, seq_len(st$k)))     # 0 = unknown/thin (reference)
}

# per-slate ownership features from projection + salary (all pre-lock)
.own_feats <- function(D, style = NULL) {
  D <- as.data.table(copy(D))
  if (!"proj" %in% names(D) && "mu" %in% names(D)) D[, proj := mu]
  D[, val := proj / pmax(salary/1000, 1)]
  if (!is.null(style)) {                                   # archetype + course-len interaction inputs
    D[, clu := .style_assign(D, style)]
    cm <- style$c_len_med; if (is.null(cm) || !is.finite(cm)) cm <- 0
    D[, c_len_z := if ("c_len" %in% names(D)) fifelse(is.finite(c_len), c_len - cm, 0) else 0]
  }
  # per-slate ranks (.N must be referenced directly in j, not via a helper)
  if (all(c("event_id","year") %in% names(D)))
    D[, `:=`(sal_rank = frank(-salary, ties.method="average")/.N,
             proj_rank= frank(-proj,   ties.method="average")/.N,
             val_rank = frank(-val,    ties.method="average")/.N), by=.(event_id,year)]
  else
    D[, `:=`(sal_rank = frank(-salary, ties.method="average")/.N,
             proj_rank= frank(-proj,   ties.method="average")/.N,
             val_rank = frank(-val,    ties.method="average")/.N)]
  D
}

train_ownership_model <- function(master = NULL) {
  if (is.null(master)) master <- as.data.table(readRDS(file.path(OUT,"v2_master.rds"))$master)
  M <- as.data.table(master)
  sm <- if (file.exists(file.path(OUT,"v2_skill_model.rds"))) readRDS(file.path(OUT,"v2_skill_model.rds"))
        else train_skill(M)
  M[, proj := project_skill(sm, M)$mu]
  style <- tryCatch(.build_style(M), error = function(e) NULL)
  D <- .own_feats(M[is.finite(ownership) & ownership>0 & !is.na(salary) & salary>0 & is.finite(proj)], style)
  fml <- if (!is.null(style)) ownership ~ sal_rank + proj_rank + val_rank + val + salary + clu + clu:c_len_z + clu:proj_rank
         else                 ownership ~ sal_rank + proj_rank + val_rank + val + salary
  m <- lm(fml, data = D)
  saveRDS(list(lm = m, style = style), OWN_MODEL)
  m
}

# returns ownership FRACTIONS (clamped, normalised to ~ roster spots)
predict_ownership <- function(pool, model = NULL) {
  if (is.null(model)) model <- if (file.exists(OWN_MODEL)) readRDS(OWN_MODEL) else list(lm=train_ownership_model(), style=NULL)
  D <- .own_feats(pool, model$style)
  own <- as.numeric(predict(model$lm, D))
  own[!is.finite(own)] <- 0
  own <- pmax(own, 0.001)
  own <- pmin(own, 0.60)
  s <- sum(own); if (s > 0) own <- own * (N_ROSTER / s)     # normalise to roster spots
  pmin(own, 0.60)
}

# blend our model with DataGolf's proj_ownership where both exist
blend_ownership <- function(pool, dg_own = NULL, w_ours = 0.5) {
  ours <- predict_ownership(pool)
  if (is.null(dg_own) || all(!is.finite(dg_own)) || max(dg_own, na.rm=TRUE) <= 0.0015) return(ours)
  out <- w_ours * ours + (1 - w_ours) * dg_own
  s <- sum(out, na.rm=TRUE); if (s > 0) out <- out * (N_ROSTER / s)
  pmin(pmax(out, 0.001), 0.60)
}

# DG-vs-actual A/B: append today's DataGolf proj_ownership so we can grade it later
log_dg_ownership <- function(tour = "pga", slate = "main") {
  raw <- .dg("preds/fantasy-projection-defaults", list(tour=tour, site="draftkings", slate=slate))
  if (is.null(raw) || is.null(raw$projections)) { emsg("dg-own log: no slate"); return(invisible()) }
  p <- as.data.table(raw$projections)
  if (!"proj_ownership" %in% names(p)) { emsg("dg-own log: no proj_ownership"); return(invisible()) }
  rec <- p[, .(date=as.character(Sys.Date()), event=raw$event_name, tour,
               player_id=as.integer(dg_id), player_name,
               dg_own=as.numeric(proj_ownership))][is.finite(dg_own)]
  f <- file.path(OUT, "dg_ownership_log.csv")
  fwrite(rec, f, append = file.exists(f))
  emsg("dg-own log: appended ", nrow(rec), " players for ", raw$event_name)
  invisible(rec)
}

if (identical(environment(), globalenv()) && !nzchar(Sys.getenv("OWNERSHIP_SOURCE_ONLY"))) {
  emsg("=== engine/ownership.R — train + validate ownership model ===")
  M <- as.data.table(readRDS(file.path(OUT,"v2_master.rds"))$master)
  sm <- readRDS(file.path(OUT,"v2_skill_model.rds")); M[, proj := project_skill(sm, M)$mu]
  style_tr <- .build_style(M[year<=2024])            # leakage-safe: centroids from train only
  D <- .own_feats(M[is.finite(ownership) & ownership>0 & !is.na(salary) & salary>0 & is.finite(proj)], style_tr)
  tr <- D[year<=2024]; te <- D[year>=2025]; te[, clu := factor(clu, levels=levels(tr$clu))]
  psc <- function(dt) mean(dt[, .(c=cor(pred, ownership, use="complete.obs")), by=.(event_id,year)]$c, na.rm=TRUE)
  m0 <- lm(ownership ~ sal_rank + proj_rank + val_rank + val + salary, data=tr)
  m1 <- lm(ownership ~ sal_rank + proj_rank + val_rank + val + salary + clu + clu:c_len_z + clu:proj_rank, data=tr)
  te[, pred := predict(m0, te)]; ps0 <- psc(te)
  te[, pred := predict(m1, te)]; ps1 <- psc(te)
  cat(sprintf("walk-forward per-slate cor: base %.4f -> +archetype %.4f (salary-only ~0.71)\n", ps0, ps1))
  train_ownership_model(M)
  emsg("saved ", OWN_MODEL)
}
