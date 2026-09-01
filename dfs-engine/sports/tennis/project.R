# ==============================================================================
# Tennis plugin — projections (distribution per player)
# Uses the surface-Elo model (model.R / data/models/tennis_models.rds) when
# available: pairs the two players in each DK match (by game_id), matches names to
# Sackmann ids, and projects the DK-points mixture from win prob + DK|win / DK|loss.
# Players we can't match or pair fall back to the DK season-avg BASELINE.
#
# Live-feed limits (documented): DK's tennis export doesn't carry surface or
# best_of, so pass slate$surface (default "Hard") and slate$best_of (default 3),
# or set them per tournament. Salary / ownership come from the DK scrape.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

.tennis_baseline <- function(pool) {
  med_val <- median(pool$dk_avg / pmax(pool$salary / 1000, 0.1), na.rm = TRUE)
  if (!is.finite(med_val)) med_val <- 5
  proj <- pool$dk_avg
  proj[is.na(proj) | proj <= 0] <- (pool$salary[is.na(proj) | proj <= 0] / 1000) * med_val
  sd <- pmax(0.55 * proj, 12)
  data.table(proj = proj, sim_sd = sd, ceil = proj + 1.1 * sd,
             floor = pmax(proj - 0.8 * sd, 0), p_zero = 0.05)
}

tennis_project_players <- function(slate) {
  path <- dk_salary_path(slate$sport, slate$date, slate$name)
  pool <- read_dk_salary_csv(path, sport = "tennis", slate_id = slate$slate_id)
  pool[, position := "P"]
  surface <- slate$surface %||% "Hard"; best_of <- as.integer(slate$best_of %||% 3)
  tour    <- slate$tour %||% "atp"

  # Per-match surface: DK's feed omits surface, but the event name (tournament) lets
  # us look it up from the cached tournaments file. Falls back to the slate default.
  pool[, .surface := surface]
  slk <- tryCatch(tennis_surface_lookup(tour), error = function(e) NULL)
  if (!is.null(slk) && "event" %in% names(pool)) {
    ek <- tennis_event_key(pool$event); hit <- slk[ek]
    pool[!is.na(hit), .surface := hit[!is.na(hit)]]
    msg("  Tennis surfaces:", paste(names(table(pool$.surface)), table(pool$.surface), sep="=", collapse=", "))
  }

  models <- if (exists("tennis_load_models")) tennis_load_models() else NULL
  pool[, `:=`(proj = NA_real_, sim_sd = NA_real_, ceil = NA_real_, floor = NA_real_, p_zero = NA_real_)]

  if (!is.null(models) && !is.null(models$name_map)) {
    nm <- models$name_map
    pool[nm, on = "norm", sack_id := i.player_id]
    # opponent = the other player in the same DK match (game_id)
    pool[, .n_in_match := .N, by = game_id]
    setkey(pool, game_id)
    for (gid in unique(pool$game_id[pool$.n_in_match == 2])) {
      idx <- which(pool$game_id == gid)
      if (length(idx) != 2) next
      for (k in 1:2) {
        me <- idx[k]; op <- idx[3 - k]
        if (is.na(pool$sack_id[me]) || is.na(pool$sack_id[op])) next
        pr <- tennis_project_one(models, pool$sack_id[me], pool$sack_id[op],
                                 surface = pool$.surface[me] %||% surface, best_of = best_of, tour = tour)
        set(pool, me, c("proj","sim_sd","ceil","floor","p_zero"),
            list(pr$proj, pr$sim_sd, pr$ceil, pr$floor, pr$p_zero))
      }
    }
    msg("  Tennis model projected", sum(!is.na(pool$proj)), "of", nrow(pool),
        "players;", sum(is.na(pool$proj)), "on baseline")
  } else {
    msg("  No tennis model cached -> baseline (run tennis_train()).")
  }

  miss <- is.na(pool$proj)
  if (any(miss)) { bl <- .tennis_baseline(pool)
    for (c in c("proj","sim_sd","ceil","floor","p_zero")) pool[[c]][miss] <- bl[[c]][miss] }

  # projected ownership: TRAINED model (beats heuristic on held-out slates: corr 0.16->0.58)
  # when one exists, else the value-rank heuristic (cold-start fallback).
  own_pred <- tryCatch(predict_ownership(pool, "tennis"), error = function(e) NULL)
  if (!is.null(own_pred) && length(own_pred) == nrow(pool)) {
    pool[, own := pmax(own_pred, 0.005)]
    msg("  tennis ownership: trained model")
  } else {
    pool[, .val := proj / pmax(salary / 1000, 0.1)]
    r <- frank(pool$.val) / nrow(pool)
    pool[, own := pmax(0.005, pmin(0.6, 0.02 + 0.5 * r^2))]
    pool[, .val := NULL]
  }

  persist_salaries(pool, slate$slate_id, "tennis")
  pool[, .(player_id, player_name, dk_id, team, game_id, position,
           salary, proj, sim_sd, ceil, floor, p_zero, own)]
}
