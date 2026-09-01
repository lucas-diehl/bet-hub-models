# ==============================================================================
# WNBA plugin — projections (distribution per player)
# Uses the trained minutes-first MODEL (model.R / data/models/wnba_models.rds) when
# available, matched to slate players by name. Players with no wehoop history (early
# rookies, name mismatches) fall back to the DK season-avg BASELINE so the slate is
# always fully projected. Salary / team / game / ownership come from the DK scrape.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

.wnba_norm_pos <- function(p) {
  p <- toupper(sub("/.*$", "", as.character(p)))
  fifelse(p %in% c("PG", "SG", "G"), "G",
  fifelse(p %in% c("SF", "PF", "C", "F"), "F", "F"))
}

# DK season-avg baseline (the original runnable projection; now the fallback).
.wnba_baseline <- function(pool) {
  med_val <- median(pool$dk_avg / pmax(pool$salary / 1000, 0.1), na.rm = TRUE)
  if (!is.finite(med_val)) med_val <- 4
  proj <- pool$dk_avg
  proj[is.na(proj) | proj <= 0] <- (pool$salary[is.na(proj) | proj <= 0] / 1000) * med_val
  sd <- pmax(0.38 * proj, 4)
  data.table(proj = proj, sim_sd = sd, ceil = proj + 0.84 * sd,
             floor = pmax(proj - 1.2 * sd, 0), p_zero = 0.05)
}

wnba_project_players <- function(slate) {
  path <- dk_salary_path(slate$sport, slate$date, slate$name)
  pool <- read_dk_salary_csv(path, sport = "wnba", slate_id = slate$slate_id)
  pool[, position := .wnba_norm_pos(position)]

  # --- keep box current (auto-refresh if stale), then load -------------------
  if (exists("wnba_ensure_fresh_box")) tryCatch(wnba_ensure_fresh_box(slate$date), error = function(e) NULL)
  models <- if (exists("wnba_load_models")) wnba_load_models() else NULL
  box    <- if (file.exists(wnba_box_path())) as.data.table(readRDS(wnba_box_path())) else NULL
  pool[, proj := NA_real_]

  if (!is.null(models) && !is.null(box)) {
    nm <- unique(box[, .(athlete_id, athlete_display_name)])
    nm[, norm := norm_name(athlete_display_name)]
    nm <- unique(nm, by = "norm")
    pool[nm, on = "norm", athlete_id := i.athlete_id]
    ids <- pool[!is.na(athlete_id), athlete_id]
    if (length(ids)) {
      feat <- wnba_entering_features(box, slate$date, athlete_ids = ids)
      pred <- wnba_predict(models, feat)[, .(athlete_id, proj, sim_sd, ceil, floor, p_zero)]
      pool[pred, on = "athlete_id", `:=`(proj = i.proj, sim_sd = i.sim_sd,
            ceil = i.ceil, floor = i.floor, p_zero = i.p_zero)]
      msg("  WNBA model projected", sum(!is.na(pool$proj)), "of", nrow(pool),
          "players;", sum(is.na(pool$proj)), "on baseline")
    }
  } else {
    msg("  No WNBA model cached -> baseline (run wnba_train()).")
  }

  # --- baseline fallback for unmatched players -------------------------------
  miss <- is.na(pool$proj)
  if (any(miss)) {
    bl <- .wnba_baseline(pool)
    for (c in c("proj","sim_sd","ceil","floor","p_zero"))
      pool[[c]][miss] <- bl[[c]][miss]
  }

  # --- injuries: drop confirmed-OUT + redistribute vacated minutes to teammates
  if (!is.null(box)) pool <- tryCatch(wnba_injury_adjust(pool, box, slate$date), error = function(e) pool)

  # --- game environment (Vegas-free): shapes ceiling/blowout + correlation ----
  # NOTE: leaves the validated MEAN (proj) untouched; only the joint distribution.
  pool[, `:=`(game_total_z = 0, blowout = 0)]
  if (!is.null(box)) {
    env <- tryCatch(wnba_team_env_asof(wnba_build_team_games(box), slate$date),
                    error = function(e) NULL)
    if (!is.null(env)) {
      vg <- tryCatch(vegas_games("wnba", slate$date), error = function(e) NULL)   # market lines (free)
      ge <- wnba_attach_game_env(pool, env, vegas = vg)
      m <- match(pool$player_id, ge$player_id)
      pool[, `:=`(game_total_z = ge$game_total_z[m], blowout = ge$blowout[m])]
      pool[, ceil   := pmax(ceil + 0.06 * game_total_z * proj, proj)]   # upside ~ game total
      pool[, p_zero := pmin(p_zero + 0.04 * blowout, 0.40)]             # blowout exit risk
    }
  }

  # --- projected ownership: TRAINED model (beats the heuristic on held-out slates:
  # corr 0.40->0.70) when one exists, else the value-rank heuristic (cold-start fallback).
  own_pred <- tryCatch(predict_ownership(pool, "wnba"), error = function(e) NULL)
  if (!is.null(own_pred) && length(own_pred) == nrow(pool)) {
    pool[, own := pmax(own_pred, 0.005)]
    msg("  WNBA ownership: trained model")
  } else {
    pool[, .val := proj / pmax(salary / 1000, 0.1)]
    r <- frank(pool$.val) / nrow(pool)
    pool[, own := pmax(0.005, pmin(0.55, 0.02 + 0.45 * r^2))]
    pool[, .val := NULL]
  }

  persist_salaries(pool, slate$slate_id, "wnba")
  pool[, .(player_id, player_name, dk_id, team, game_id, position,
           salary, proj, sim_sd, ceil, floor, p_zero, own, game_total_z)]
}
