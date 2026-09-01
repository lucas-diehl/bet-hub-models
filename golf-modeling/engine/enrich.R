#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS v2 — engine/enrich.R   (P0b: live DataGolf enrichments)
#
# Live-only signals that sharpen the projection but can't be historically
# backtested (no cached history for these feeds). All are OPTIONAL: the pipeline
# works without them; when present they blend in.
#   * skill-ratings  -> DataGolf's REGRESSED per-category true skill (sg_ott/app/
#                       arg/putt) -> blended into our per-category core + mu. A
#                       strong course-agnostic prior; complements our recent-form model.
#   * pre-tournament (baseline_history_fit) -> course-HISTORY-and-FIT adjusted win
#                       probs -> the market signal (folds DG's course fit into the blend).
#   * field-updates  -> tee times + AM/PM wave -> conditions (variance + wave grouping).
#
# Source: Sys.setenv(ENGINE_SOURCE_ONLY="1"); source("engine/enrich.R")
#   -> get_dg_skill_ratings(), get_dg_pretourn(), get_dg_waves(), blend_dg_skill()
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table); library(httr2) }))
.EKEY <- function() {
  k <- Sys.getenv("DATAGOLF_API_KEY")
  if (!nzchar(k)) for (p in c(".Renviron","../DFS ENGINE/.Renviron"))
    if (file.exists(p)) { readRenviron(p); k <- Sys.getenv("DATAGOLF_API_KEY"); if (nzchar(k)) break }
  k
}
.dg <- function(ep, ps = list()) {
  ps$key <- .EKEY(); ps$file_format <- "json"
  url <- paste0("https://feeds.datagolf.com/", ep, "?",
                paste(names(ps), unlist(ps), sep = "=", collapse = "&"))
  tryCatch(httr2::request(url) |> httr2::req_timeout(30) |> httr2::req_perform() |>
             httr2::resp_body_json(simplifyVector = TRUE), error = function(e) NULL)
}

# DataGolf regressed per-category skill (course-agnostic true-skill estimates)
get_dg_skill_ratings <- function() {
  raw <- .dg("preds/skill-ratings", list(display = "value"))
  if (is.null(raw) || is.null(raw$players)) return(data.table())
  b <- as.data.table(raw$players)
  b[, .(player_id = as.integer(dg_id),
        dg_sg_ott = as.numeric(sg_ott), dg_sg_app = as.numeric(sg_app),
        dg_sg_arg = as.numeric(sg_arg), dg_sg_putt = as.numeric(sg_putt),
        dg_sg_total = as.numeric(sg_total))]
}

# course-history-&-fit-adjusted win probabilities (the strong course-aware market signal)
get_dg_pretourn <- function(tour = "pga", model = "baseline_history_fit") {
  raw <- .dg("preds/pre-tournament", list(tour = tour, odds_format = "percent"))
  if (is.null(raw)) return(data.table())
  b <- as.data.table(raw[[if (model %in% names(raw)) model else "baseline"]])
  if (!nrow(b) || !"dg_id" %in% names(b)) return(data.table())
  out <- b[, .(player_id = as.integer(dg_id),
               p_win = as.numeric(win) / 100, am = suppressWarnings(as.integer(am)))]
  out[is.finite(p_win) & p_win > 0][, p_win := p_win / sum(p_win)][]  # de-vig to sum 1
}

# tee times + AM/PM wave
get_dg_waves <- function(tour = "pga") {
  raw <- .dg("field-updates", list(tour = tour))
  if (is.null(raw) || is.null(raw$field)) return(data.table())
  b <- as.data.table(raw$field)
  b[, .(player_id = as.integer(dg_id), wave = suppressWarnings(as.integer(am)))]
}

# blend DataGolf per-category skill into our mu + per-category projections.
# DG skill is vs a FIXED baseline -> centre to the field mean so it's vs-field like
# our mu (preserves our level), then blend at weight w.
blend_dg_skill <- function(Pm, sr = NULL, w = 0.30) {
  if (is.null(sr)) sr <- get_dg_skill_ratings()
  if (!nrow(sr)) return(as.data.table(Pm))
  D <- merge(as.data.table(copy(Pm)), sr, by = "player_id", all.x = TRUE)
  ctr <- function(x) x - mean(x, na.rm = TRUE)
  if (any(is.finite(D$dg_sg_total))) {
    D[, dgt_c := ctr(dg_sg_total)]
    D[is.finite(dgt_c), mu := (1 - w) * mu + w * dgt_c]
  }
  for (c in c("ott","app","arg","putt")) {
    dc <- paste0("dg_sg_", c); sc <- paste0("s_", c)
    if (dc %in% names(D) && any(is.finite(D[[dc]]))) {
      D[, tmp_c := ctr(get(dc))]
      D[is.finite(tmp_c), (sc) := (1 - w) * get(sc) + w * tmp_c]
      D[, tmp_c := NULL]
    }
  }
  D[, intersect(c("dg_sg_ott","dg_sg_app","dg_sg_arg","dg_sg_putt","dg_sg_total","dgt_c"),
                names(D)) := NULL]
  D[]
}
