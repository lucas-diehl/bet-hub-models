# R/reshape_rounds.R
library(data.table)

reshape_wide_scores_to_long <- function(rounds_wide_dt, events_dt) {
  wide <- as.data.table(rounds_wide_dt)
  
  # ---- HARD DEDUPE EVENTS (guarantee 1 row per event_id) ----
  ev <- as.data.table(events_dt)[, .(event_id, start_date)]
  ev[, start_date := as.Date(start_date)]
  ev <- ev[!is.na(event_id) & !is.na(start_date)]
  ev <- ev[, .(start_date = min(start_date)), by = event_id]
  setkey(ev, event_id)
  
  # ---- event_id ----
  if (!("event_id" %in% names(wide))) stop("rounds_wide_dt missing event_id")
  wide[, event_id := as.character(event_id)]
  
  # ---- player id (schema drift) ----
  id_cands <- c("scores.dg_id", "dg_id", "player_id")
  id_col <- intersect(id_cands, names(wide))[1]
  if (is.na(id_col) || is.null(id_col)) {
    stop(
      "reshape_wide_scores_to_long(): no player id column found. Tried: ",
      paste(id_cands, collapse = ", "),
      "\nAvailable: ", paste(names(wide), collapse = ", ")
    )
  }
  if (id_col != "player_id") setnames(wide, id_col, "player_id")
  wide[, player_id := as.integer(player_id)]
  
  # ---- If feed is already long-ish (round + metrics) ----
  # If we already have round_num and a score column, just compute round_date and return.
  roundnum_cands <- c("round_num", "round", "rnd", "r")
  round_col <- intersect(roundnum_cands, names(wide))[1]
  score_cands <- c("score", "round_score", "strokes", "round_strokes")
  score_col <- intersect(score_cands, names(wide))[1]
  
  if (!is.na(round_col) && !is.na(score_col)) {
    if (round_col != "round_num") setnames(wide, round_col, "round_num")
    if (score_col != "round_score") setnames(wide, score_col, "round_score")
    wide[, round_num := as.integer(round_num)]
    wide[, round_score := suppressWarnings(as.numeric(round_score))]
    
    # try to standardize sg columns if present
    sg_map <- list(
      sg_ott   = c("sg_ott", "sgOTT", "ott_sg"),
      sg_app   = c("sg_app", "sgAPP", "app_sg"),
      sg_arg   = c("sg_arg", "sgARG", "arg_sg"),
      sg_putt  = c("sg_putt", "sgPUTT", "putt_sg"),
      sg_total = c("sg_total", "sgTOTAL", "total_sg", "sg")
    )
    for (nm in names(sg_map)) {
      cand <- intersect(sg_map[[nm]], names(wide))[1]
      if (!is.na(cand) && cand != nm) setnames(wide, cand, nm)
      if (nm %in% names(wide)) wide[, (nm) := suppressWarnings(as.numeric(get(nm)))]
    }
    
    out <- wide[, .(event_id, player_id, round_num,
                    round_score,
                    sg_ott = get0("sg_ott", ifnotfound = NA_real_),
                    sg_app = get0("sg_app", ifnotfound = NA_real_),
                    sg_arg = get0("sg_arg", ifnotfound = NA_real_),
                    sg_putt = get0("sg_putt", ifnotfound = NA_real_),
                    sg_total = get0("sg_total", ifnotfound = NA_real_)
    )]
    
    out <- out[ev, on = "event_id"]
    out[, round_date := start_date + (round_num - 1L)]
    out[, start_date := NULL]
    return(out[])
  }
  
  # ---- Wide-ish feed: detect metric columns by multiple patterns ----
  metrics_keep <- c("score", "sg_ott", "sg_app", "sg_arg", "sg_putt", "sg_total", "sg_t2g")
  
  # Pattern set A: scores.round_1.score / scores.round_1.sg_app ...
  pattA <- function(m) paste0("^scores\\.round_\\d+\\.", m, "$")
  
  # Pattern set B: round_1_score, round_1_sg_app, r1_score, r1_sg_app
  pattB1 <- function(m) paste0("^round_\\d+_", m, "$")
  pattB2 <- function(m) paste0("^r\\d+_", m, "$")
  
  # Pattern set C: score_r1, sg_app_r1, sg_app_round_1, etc.
  pattC1 <- function(m) paste0("^", m, "_r\\d+$")
  pattC2 <- function(m) paste0("^", m, "_round_\\d+$")
  
  keep_cols <- unlist(lapply(metrics_keep, function(m) {
    unique(c(
      grep(pattA(m), names(wide), value = TRUE),
      grep(pattB1(m), names(wide), value = TRUE),
      grep(pattB2(m), names(wide), value = TRUE),
      grep(pattC1(m), names(wide), value = TRUE),
      grep(pattC2(m), names(wide), value = TRUE)
    ))
  }), use.names = FALSE)
  
  keep_cols <- unique(keep_cols)
  
  if (length(keep_cols) == 0) {
    stop(
      "reshape_wide_scores_to_long(): no round metric columns found for score/SG.\n",
      "Available columns: ", paste(names(wide), collapse = ", ")
    )
  }
  
  base <- wide[, c("event_id", "player_id", keep_cols), with = FALSE]
  
  long <- melt(
    base,
    id.vars = c("event_id", "player_id"),
    variable.name = "var",
    value.name = "value",
    variable.factor = FALSE
  )
  
  # Extract round_num for multiple patterns
  #  - scores.round_2.sg_app
  #  - round_2_sg_app
  #  - r2_sg_app
  #  - sg_app_r2
  #  - sg_app_round_2
  long[, round_num := fifelse(
    grepl("^scores\\.round_\\d+\\.", var),
    as.integer(sub("^scores\\.round_(\\d+)\\..*$", "\\1", var)),
    fifelse(
      grepl("^round_\\d+_", var),
      as.integer(sub("^round_(\\d+)_.*$", "\\1", var)),
      fifelse(
        grepl("^r\\d+_", var),
        as.integer(sub("^r(\\d+)_.*$", "\\1", var)),
        fifelse(
          grepl("_r\\d+$", var),
          as.integer(sub("^.*_r(\\d+)$", "\\1", var)),
          fifelse(
            grepl("_round_\\d+$", var),
            as.integer(sub("^.*_round_(\\d+)$", "\\1", var)),
            NA_integer_
          )
        )
      )
    )
  )]
  
  # Extract metric for multiple patterns
  long[, metric := fifelse(
    grepl("^scores\\.round_\\d+\\.", var),
    sub("^scores\\.round_\\d+\\.", "", var),
    fifelse(
      grepl("^round_\\d+_", var),
      sub("^round_\\d+_", "", var),
      fifelse(
        grepl("^r\\d+_", var),
        sub("^r\\d+_", "", var),
        fifelse(
          grepl("_r\\d+$", var),
          sub("_r\\d+$", "", var),
          fifelse(
            grepl("_round_\\d+$", var),
            sub("_round_\\d+$", "", var),
            NA_character_
          )
        )
      )
    )
  )]
  
  long <- long[!is.na(round_num) & !is.na(metric)]
  
  out <- dcast(long, event_id + player_id + round_num ~ metric, value.var = "value")
  
  # Coerce numeric columns
  num_cols <- setdiff(names(out), c("event_id", "player_id", "round_num"))
  for (cc in num_cols) set(out, j = cc, value = suppressWarnings(as.numeric(out[[cc]])))
  
  # Join start_date (deduped, no cartesian)
  out <- out[ev, on = "event_id"]
  out[, round_date := start_date + (round_num - 1L)]
  out[, start_date := NULL]
  
  # standardize "score" -> "round_score"
  if ("score" %in% names(out)) setnames(out, "score", "round_score")
  
  out[]
}