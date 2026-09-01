# ==============================================================================
# NFL plugin — LIVE slate projections (connects the model to a DK NFL slate)
# The model + correlation + roster + scoring were already built and validated; this is
# the missing piece that makes NFL a full dashboard sport. Reads the DK slate pool,
# maps players to their nflverse history by name, projects each from their current
# rolling opportunity (model.R), falls back to a salary-implied baseline for players
# with no history (rookies / DST / name mismatches), then layers Vegas game environment
# + cold-start ownership. Same contract as every other sport's project_players().
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

.nfl_norm_pos <- function(p) {
  p <- toupper(sub("/.*$", "", as.character(p)))
  fifelse(p %in% c("D", "DEF", "DST"), "DST", p)
}

# The user's external NFL model (sibling project C:/.../NFL) writes weekly component
# projections to outputs/fantasy_prop_<yr>_week<N>_projections.csv (nflverse GSIS
# player_id + projected_* components + ppr_low/high residual bands + status). We PREFER
# it over the built-in model. DK slates carry no GSIS id, so we join by normalized name,
# and we RE-SCORE from the projected COMPONENTS through the site scoring function so the
# projection is site-correct — the file's projected_ppr is a generic full-PPR line
# (rec 1, INT/fum -2, no yardage bonuses) that matches neither DK nor FanDuel (half-PPR).
.nfl_ext_dir <- function() {
  d <- Sys.getenv("NFL_MODEL_DIR", "")
  if (nzchar(d) && dir.exists(d)) return(normalizePath(d))
  cand <- file.path(DFS_ROOT, "..", "NFL")
  if (dir.exists(cand)) normalizePath(cand) else NA_character_
}
# Flatten the JSON ingestion contract (scoring="components") into the SAME wide,
# column-per-component shape the CSV yields, so the site re-score downstream is identical.
.nfl_json_to_dt <- function(path) {
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  pl <- j$players; if (is.null(pl) || !length(pl)) return(NULL)
  g <- function(x) if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(x))
  s <- function(x) if (is.null(x)) NA_character_ else as.character(x)
  d <- rbindlist(lapply(pl, function(p) { cc <- p$components %||% list()
    data.table(player = s(p$name), position = s(p$pos), team = s(p$team),
               opponent_team = s(p$opponent), game_id = s(p$game_id),
               projected_ppr = g(p$proj_ppr), ppr_low = g(p$ppr_low), ppr_high = g(p$ppr_high),
               projected_passing_yards = g(cc$pass_yds), projected_passing_tds = g(cc$pass_td),
               projected_interceptions = g(cc$interceptions), projected_rushing_yards = g(cc$rush_yds),
               projected_rushing_tds = g(cc$rush_td), projected_receptions = g(cc$receptions),
               projected_receiving_yards = g(cc$rec_yds), projected_receiving_tds = g(cc$rec_td),
               projected_fumbles_lost = g(cc$fumbles_lost), projected_two_point_conversions = g(cc$two_pt),
               projection_status = s(p$status), role_status = s(p$role_status)) }), fill = TRUE)
  if (!nrow(d)) return(NULL)
  d[, norm := norm_name(player)]
  setorder(d, -projected_ppr); unique(d, by = "norm")[]
}

.nfl_read_ext <- function(dir, date) {
  od <- file.path(dir, "outputs")
  # PREFER the JSON contract (site-agnostic components); fall back to the wide CSV. Both
  # return the identical shape. NB: no game_date filter — the DK slate pool defines slate
  # membership and a main slate spans Thu/Sun/Mon; extra names here simply won't match.
  js <- list.files(od, pattern = "fantasy_prop_.*week\\d+_projections\\.json$", full.names = TRUE)
  latest <- file.path(od, "dfs_projections_latest.json")
  if (file.exists(latest)) js <- c(latest, js)
  if (length(js)) {
    f <- js[which.max(file.mtime(js))]
    d <- tryCatch(.nfl_json_to_dt(f), error = function(e) NULL)
    if (!is.null(d) && nrow(d)) { attr(d, "file") <- basename(f); return(d[]) }
  }
  fs <- list.files(od, pattern = "fantasy_prop_.*week\\d+_projections\\.csv$", full.names = TRUE)
  if (!length(fs)) return(NULL)
  f <- fs[which.max(file.mtime(fs))]                       # newest week file = current week
  d <- tryCatch(as.data.table(fread(f)), error = function(e) NULL)
  if (is.null(d) || !all(c("projected_ppr", "player") %in% names(d))) return(NULL)
  d[, norm := norm_name(player)]
  setorder(d, -projected_ppr); d <- unique(d, by = "norm")  # one row per name (keep the starter)
  attr(d, "file") <- basename(f); d[]
}

# salary-implied baseline (points-per-$1k), the fallback when the model can't project a
# player (no nflverse history: rookies, DST, unmatched names).
.nfl_baseline <- function(pool) {
  val <- suppressWarnings(median(pool$dk_avg / pmax(pool$salary / 1000, 0.1), na.rm = TRUE))
  if (!is.finite(val) || val <= 0) val <- 2.2
  proj <- as.numeric(pool$dk_avg); bad <- is.na(proj) | proj <= 0
  proj[bad] <- (pool$salary[bad] / 1000) * val
  sd <- pmax(0.42 * proj, 4)
  data.table(proj = proj, sim_sd = sd, ceil = proj + 0.84 * sd,
             floor = pmax(proj - 1.2 * sd, 0), p_zero = 0.03)
}

nfl_project_players <- function(slate) {
  path <- dk_salary_path(slate$sport, slate$date, slate$name)
  pool <- read_dk_salary_csv(path, sport = "nfl", slate_id = slate$slate_id)
  pool[, position := .nfl_norm_pos(position)]
  pool[, proj := NA_real_]

  # --- EXTERNAL NFL model (preferred): re-score projected components at THIS site -------
  ext_dir <- .nfl_ext_dir()
  ext <- if (!is.na(ext_dir)) tryCatch(.nfl_read_ext(ext_dir, slate$date), error = function(e) NULL) else NULL
  if (!is.null(ext) && nrow(ext) && "projected_receptions" %in% names(ext)) {
    score_fn <- tryCatch(sport_scoring("nfl", slate$site %||% "dk"), error = function(e) nfl_dk_scoring)
    box <- ext[, .(pass_yds = projected_passing_yards, pass_td = projected_passing_tds,
                   interceptions = projected_interceptions, rush_yds = projected_rushing_yards,
                   rush_td = projected_rushing_tds, receptions = projected_receptions,
                   rec_yds = projected_receiving_yards, rec_td = projected_receiving_tds,
                   two_pt = projected_two_point_conversions, fumbles_lost = projected_fumbles_lost)]
    ext[, e_proj := as.numeric(score_fn(box))]
    ext[, k := e_proj / pmax(projected_ppr, 1)]             # scale the PPR-scale bands to the site scale
    ext[, `:=`(e_floor = pmax(k * ppr_low, 0),
               e_ceil  = pmax(k * ppr_high, e_proj),
               e_sd    = pmax(k * (ppr_high - ppr_low) / 2.563, 4))]  # p10..p90 span ~2.563 sd
    pool[ext, on = "norm", `:=`(proj = i.e_proj, sim_sd = i.e_sd, ceil = i.e_ceil,
                                floor = i.e_floor, p_zero = 0.03)]
    msg(sprintf("  NFL: external model [%s] projected %d of %d (site %s); %d -> built-in/baseline",
                attr(ext, "file") %||% "ext", sum(!is.na(pool$proj)), nrow(pool),
                toupper(slate$site %||% "dk"), sum(is.na(pool$proj))))
  }

  # --- BUILT-IN model: fill only players the external model did NOT cover ---------------
  models <- if (exists("nfl_load_models")) nfl_load_models() else NULL
  Wp <- if (exists("nfl_weekly_path")) nfl_weekly_path() else NULL
  W <- if (!is.null(Wp) && file.exists(Wp)) as.data.table(readRDS(Wp)) else NULL

  if (!is.null(models) && !is.null(W) && any(is.na(pool$proj))) {
    # map DK players -> nflverse id by normalized name (pick the most-active id per name)
    g <- W[, .(games = .N), by = player_id]
    ref <- unique(W[, .(player_id, norm = norm_name(player_display_name))])
    ref <- merge(ref, g, by = "player_id")[order(-games)][, .SD[1], by = norm][, .(norm, nfl_id = player_id)]
    pool[ref, on = "norm", nfl_id := i.nfl_id]
    ids <- pool[is.na(proj) & !is.na(nfl_id), nfl_id]       # only what the external model missed
    if (length(ids)) {
      n_before <- sum(is.na(pool$proj))
      pred <- nfl_predict(models, nfl_entering_features(W, ids))
      pool[pred, on = c(nfl_id = "player_id"),
           `:=`(proj = i.proj, sim_sd = i.sim_sd, ceil = i.ceil, floor = i.floor, p_zero = i.p_zero)]
      n_fill <- n_before - sum(is.na(pool$proj))
      # NB: the built-in model is DK-scale (not site-aware) — on FanDuel these gap-fillers
      # are ~full-PPR. In production the external model covers all relevant players, so this
      # only touches name-mismatch stragglers; still worth flagging.
      msg(sprintf("  NFL built-in model filled %d more%s; %d on salary baseline", n_fill,
                  if (n_fill > 0 && tolower(slate$site %||% "dk") %in% c("fd","fanduel"))
                    " (DK-scale)" else "", sum(is.na(pool$proj))))
    }
  } else if (is.null(ext) || !nrow(ext))
    msg("  No external/built-in NFL projections -> salary baseline (see sports/nfl).")

  miss <- is.na(pool$proj)
  if (any(miss)) { bl <- .nfl_baseline(pool)
    for (c in c("proj", "sim_sd", "ceil", "floor", "p_zero")) pool[[c]][miss] <- bl[[c]][miss] }

  # --- Vegas game environment (free, ESPN): high totals -> higher ceilings + correlation
  pool[, game_total_z := 0]
  vg <- tryCatch(vegas_games("nfl", slate$date), error = function(e) NULL)
  if (!is.null(vg) && nrow(vg) && "vegas_total" %in% names(vg)) {
    pool[as.data.table(vg)[, .(game_id, vt = vegas_total)], on = "game_id", vt := i.vt]
    if (sum(!is.na(pool$vt)) >= 0.5 * nrow(pool)) {
      mu <- mean(pool$vt, na.rm = TRUE); sv <- sd(pool$vt, na.rm = TRUE)
      pool[, game_total_z := fifelse(is.na(vt) | !is.finite(sv) | sv == 0, 0, (vt - mu) / sv)]
      pool[, ceil := pmax(ceil + 0.05 * game_total_z * proj, proj)]
      msg("  vegas: NFL game totals applied to", sum(!is.na(pool$vt)), "of", nrow(pool), "players")
    }
    pool[, vt := NULL]
  }

  # --- cold-start projected ownership (chalk tracks value); refined once trained ------
  pool[, .val := proj / pmax(salary / 1000, 0.1)]
  r <- frank(pool$.val) / nrow(pool)
  pool[, own := pmax(0.005, pmin(0.5, 0.02 + 0.42 * r^2))][, .val := NULL]

  persist_salaries(pool, slate$slate_id, "nfl")
  pool[, .(player_id, player_name, dk_id, team, game_id, position,
           salary, proj, sim_sd, ceil, floor, p_zero, own, game_total_z)]
}
