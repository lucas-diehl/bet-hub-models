# ==============================================================================
# NCAAF (CFB) plugin — LIVE slate projections (connects the CFBD model to a DK slate)
# Reads the DK CFB slate pool (real salaries), maps players to their CFBD game history
# by normalized name, projects each from their rolling opportunity (model.R), falls back
# to a salary-implied baseline for players with no history (true freshmen / transfers /
# name mismatches). DK's own position field is authoritative here (training used a
# stat-profile heuristic; live serving does not need to). Injuries are handled generically
# by the pipeline's apply_inactives() (ESPN college-football feed) — no per-sport code.
# Same contract as every other sport's project_players().
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

.ncaaf_norm_pos <- function(p) toupper(sub("/.*$", "", as.character(p)))

# salary-implied baseline (points-per-$1k) — fallback when the model can't project a
# player (no CFBD history: true freshmen, transfers, name mismatches).
.ncaaf_baseline <- function(pool) {
  val <- suppressWarnings(median(pool$dk_avg / pmax(pool$salary / 1000, 0.1), na.rm = TRUE))
  if (!is.finite(val) || val <= 0) val <- 2.0
  proj <- as.numeric(pool$dk_avg); bad <- is.na(proj) | proj <= 0
  proj[bad] <- (pool$salary[bad] / 1000) * val
  sd <- pmax(0.45 * proj, 4)
  data.table(proj = proj, sim_sd = sd, ceil = proj + 0.84 * sd, floor = pmax(proj - 1.2 * sd, 0), p_zero = 0.04)
}

ncaaf_project_players <- function(slate) {
  path <- dk_salary_path(slate$sport, slate$date, slate$name)
  pool <- read_dk_salary_csv(path, sport = "ncaaf", slate_id = slate$slate_id)
  pool[, position := .ncaaf_norm_pos(position)]
  pool[, proj := NA_real_]

  W <- if (exists("cfb_weekly_path") && file.exists(cfb_weekly_path())) as.data.table(readRDS(cfb_weekly_path())) else NULL
  m <- if (exists("NCAAF_MODEL_PATH") && file.exists(NCAAF_MODEL_PATH())) readRDS(NCAAF_MODEL_PATH()) else NULL

  if (!is.null(W) && !is.null(m)) {
    ref <- unique(W[, .(athlete_id, norm = norm_name(player))])
    g <- W[, .(games = .N), by = athlete_id]
    ref <- merge(ref, g, by = "athlete_id")[order(-games)][, .SD[1], by = norm][, .(norm, athlete_id)]
    pool[ref, on = "norm", cfb_id := i.athlete_id]
    ids <- pool[!is.na(cfb_id), cfb_id]
    if (length(ids)) {
      feat <- ncaaf_entering_features(W, ids)
      pool[feat, on = c(cfb_id = "athlete_id"), `:=`(
        r_dk_pts = i.r_dk_pts, r_pass_yds = i.r_pass_yds, r_pass_td = i.r_pass_td,
        r_rush_yds = i.r_rush_yds, r_rush_td = i.r_rush_td, r_rec_yds = i.r_rec_yds,
        r_rec_td = i.r_rec_td, r_receptions = i.r_receptions, r_carries = i.r_carries,
        g_played = i.g_played)]
      matched <- !is.na(pool$r_dk_pts)
      pred <- rep(NA_real_, nrow(pool))
      for (p in names(m$models)) {
        idx <- which(matched & pool$position == p)
        if (length(idx)) pred[idx] <- pmax(as.numeric(predict(m$models[[p]], pool[idx])), 0)
      }
      pool[, proj := pred]
      msg(sprintf("  NCAAF model projected %d of %d players (site %s); %d -> salary baseline",
                  sum(!is.na(pool$proj)), nrow(pool), toupper(slate$site %||% "dk"), sum(is.na(pool$proj))))
      pool[!is.na(proj), `:=`(sim_sd = pmax(0.45 * proj, 4))]
      pool[!is.na(proj), `:=`(ceil = proj + 0.84 * sim_sd, floor = pmax(proj - 1.2 * sim_sd, 0), p_zero = 0.04)]
    }
  } else msg("  No NCAAF model cached -> salary baseline (run ncaaf_train()).")

  miss <- is.na(pool$proj)
  if (any(miss)) { bl <- .ncaaf_baseline(pool)
    for (c1 in c("proj", "sim_sd", "ceil", "floor", "p_zero")) pool[[c1]][miss] <- bl[[c1]][miss] }

  # cold-start projected ownership (chalk tracks value); refined once trained (train_ownership_model)
  own_pred <- tryCatch(predict_ownership(pool, "ncaaf"), error = function(e) NULL)
  if (!is.null(own_pred)) { pool[, own := own_pred]; msg("  NCAAF ownership: trained model") }
  else {
    pool[, .val := proj / pmax(salary / 1000, 0.1)]
    r <- frank(pool$.val) / nrow(pool)
    pool[, own := pmax(0.005, pmin(0.5, 0.02 + 0.42 * r^2))][, .val := NULL]
  }

  persist_salaries(pool, slate$slate_id, "ncaaf")
  pool[, .(player_id, player_name, dk_id, team, game_id, position, salary, proj, sim_sd, ceil, floor, p_zero, own)]
}
