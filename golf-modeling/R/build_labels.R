# R/build_labels.R
library(data.table)

build_results_from_wide_scores <- function(rounds_wide_dt) {
  dt <- as.data.table(rounds_wide_dt)
  
  # --- event_id ---
  if (!("event_id" %in% names(dt))) stop("Missing event_id column in rounds_wide_dt.")
  dt[, event_id := as.character(event_id)]
  
  # --- player id (schema drift) ---
  id_cands <- c("scores.dg_id", "dg_id", "player_id")
  id_col <- intersect(id_cands, names(dt))[1]
  if (is.na(id_col) || is.null(id_col)) {
    stop(
      "No player id column found. Expected one of: ",
      paste(id_cands, collapse = ", "),
      "\nAvailable columns: ", paste(names(dt), collapse = ", ")
    )
  }
  if (id_col != "player_id") setnames(dt, id_col, "player_id")
  dt[, player_id := as.integer(player_id)]
  
  # -----------------------------
  # CASE A: The feed is already "long-ish":
  # columns like round_num + score or round + score
  # -----------------------------
  roundnum_cands <- c("round_num", "round", "rnd", "r")
  score_cands <- c("score", "round_score", "strokes", "round_strokes")
  
  round_col <- intersect(roundnum_cands, names(dt))[1]
  score_col <- intersect(score_cands, names(dt))[1]
  
  if (!is.na(round_col) && !is.na(score_col)) {
    # normalize column names
    if (round_col != "round_num") setnames(dt, round_col, "round_num")
    if (score_col != "score") setnames(dt, score_col, "score")
    dt[, score := suppressWarnings(as.numeric(score))]
    
    out <- dt[, .(
      total_score = sum(score, na.rm = TRUE),
      n_rounds_scored = sum(is.finite(score))
    ), by = .(event_id, player_id)]
    
    setorder(out, event_id, total_score)
    out[, finish_pos := frank(total_score, ties.method = "min"), by = event_id]
    out[, top20 := as.integer(finish_pos <= 20)]
    return(out[])
  }
  
  # -----------------------------
  # CASE B: Wide-ish feed: find round score columns
  # -----------------------------
  score_cols <- character()
  
  # Your original JSON-flattened style
  score_cols <- c(score_cols, grep("^scores\\.round_\\d+\\.score$", names(dt), value = TRUE))
  
  # Common flat CSV variants
  score_cols <- c(score_cols, grep("^round_\\d+_score$", names(dt), value = TRUE))
  score_cols <- c(score_cols, grep("^r\\d+_score$", names(dt), value = TRUE))
  score_cols <- c(score_cols, grep("^score_round_\\d+$", names(dt), value = TRUE))
  
  # Additional variants seen in sports feeds
  score_cols <- c(score_cols, grep("^r[1-4]$", names(dt), value = TRUE))          # r1 r2 r3 r4
  score_cols <- c(score_cols, grep("^round[1-4]$", names(dt), value = TRUE))      # round1 round2...
  score_cols <- c(score_cols, grep("^rnd[1-4]$", names(dt), value = TRUE))        # rnd1 rnd2...
  score_cols <- c(score_cols, grep("^round_?[1-4]$", names(dt), value = TRUE))    # round_1 or round1
  score_cols <- c(score_cols, grep("^rnd_?[1-4]$", names(dt), value = TRUE))      # rnd_1 or rnd1
  score_cols <- c(score_cols, grep("^score_?r[1-4]$", names(dt), value = TRUE))   # score_r1 / scorer1
  
  score_cols <- unique(score_cols)
  
  # -----------------------------
  # CASE C: Already has total score
  # -----------------------------
  if (length(score_cols) == 0) {
    total_cands <- c("total_score", "tot_score", "total", "score_total", "tournament_score")
    total_col <- intersect(total_cands, names(dt))[1]
    
    if (!is.na(total_col) && !is.null(total_col)) {
      if (total_col != "total_score") setnames(dt, total_col, "total_score")
      dt[, total_score := suppressWarnings(as.numeric(total_score))]
      
      out <- dt[, .(
        total_score = total_score[1],
        n_rounds_scored = NA_integer_
      ), by = .(event_id, player_id)]
      
      setorder(out, event_id, total_score)
      out[, finish_pos := frank(total_score, ties.method = "min"), by = event_id]
      out[, top20 := as.integer(finish_pos <= 20)]
      return(out[])
    }
    
    stop(
      "No round score columns found.\n",
      "Tried patterns: scores.round_#.score, round_#_score, r#_score, score_round_#,\n",
      "plus r1/r2/r3/r4, rnd1.., round1.. variants, and total_score variants.\n",
      "Available columns: ", paste(names(dt), collapse = ", ")
    )
  }
  
  # compute totals from detected round columns
  out <- dt[, {
    scores <- suppressWarnings(as.numeric(unlist(.SD, use.names = FALSE)))
    list(
      total_score = sum(scores, na.rm = TRUE),
      n_rounds_scored = sum(is.finite(scores))
    )
  }, by = .(event_id, player_id), .SDcols = score_cols]
  
  setorder(out, event_id, total_score)
  out[, finish_pos := frank(total_score, ties.method = "min"), by = event_id]
  out[, top20 := as.integer(finish_pos <= 20)]
  
  out[]
}