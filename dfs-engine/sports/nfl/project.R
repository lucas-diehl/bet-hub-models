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
               p_zero = g(p$p_zero), injury_status = s(p$injury_status),
               sim_sd = g(p$sim_sd),
               projection_status = s(p$status), role_status = s(p$role_status)) }), fill = TRUE)
  if (!nrow(d)) return(NULL)
  d[, norm := norm_name(player)]
  setorder(d, -projected_ppr); unique(d, by = "norm")[]
}

# Warn (not fail) when the chosen projection file is stale relative to the slate
# being built -- a silently multi-week-stale read is exactly how the 2026-10-12
# bug below went unnoticed for this long. Visible in the build log, never blocks it.
.nfl_warn_if_stale <- function(path, date, max_days = 7) {
  age <- suppressWarnings(as.numeric(difftime(as.Date(date), as.Date(file.mtime(path)), units = "days")))
  if (length(age) && is.finite(age) && age > max_days) {
    msg(sprintf(
      "NFL projections: using %s, %.0f day(s) old relative to slate date %s - check for a newer week's file.",
      basename(path), age, as.character(date)))
  }
}

.nfl_read_ext <- function(dir, date) {
  od <- file.path(dir, "outputs")
  # Prefer whichever of the JSON/CSV contracts is actually NEWEST -- NOT "any JSON
  # over any CSV" unconditionally, which was a real bug (found 2026-10-13): on
  # 2026-10-12 (Week 6, no week-6 file existed yet) a 2026-10-03 Week-4 JSON was
  # still being preferred over a 2026-10-06 Week-5 CSV, silently scoring that
  # Monday-night slate off 9-day-stale data. Both formats return the identical
  # shape, so "newest file wins" is the correct rule regardless of format.
  # NB: still no game_date filter — the DK slate pool defines slate membership
  # and a main slate spans Thu/Sun/Mon; extra names here simply won't match.
  js <- list.files(od, pattern = "fantasy_prop_.*week\\d+_projections\\.json$", full.names = TRUE)
  latest <- file.path(od, "dfs_projections_latest.json")
  if (file.exists(latest)) js <- c(latest, js)
  fs <- list.files(od, pattern = "fantasy_prop_.*week\\d+_projections\\.csv$", full.names = TRUE)

  newest_json <- if (length(js)) js[which.max(file.mtime(js))] else NA_character_
  newest_csv  <- if (length(fs)) fs[which.max(file.mtime(fs))] else NA_character_
  prefer_json <- !is.na(newest_json) &&
    (is.na(newest_csv) || file.mtime(newest_json) >= file.mtime(newest_csv))

  if (prefer_json) {
    d <- tryCatch(.nfl_json_to_dt(newest_json), error = function(e) NULL)
    if (!is.null(d) && nrow(d)) {
      .nfl_warn_if_stale(newest_json, date)
      attr(d, "file") <- basename(newest_json); return(d[])
    }
  }
  if (is.na(newest_csv)) return(NULL)
  d <- tryCatch(as.data.table(fread(newest_csv)), error = function(e) NULL)
  if (is.null(d) || !all(c("projected_ppr", "player") %in% names(d))) return(NULL)
  d[, norm := norm_name(player)]
  setorder(d, -projected_ppr); d <- unique(d, by = "norm")  # one row per name (keep the starter)
  .nfl_warn_if_stale(newest_csv, date)
  attr(d, "file") <- basename(newest_csv); d[]
}

# salary-implied baseline (points-per-$1k), the fallback when the model can't project a
# player (no nflverse history: rookies, DST, unmatched names).
#
# Value-per-$1k is NOT flat across the salary range -- measured directly (NFL
# fantasy_salary_training_frame, 16,406 player-games, 2021-2025): it's close to HALF
# the pool-wide median at the price floor, because min salary is a hard DK FLOOR
# (can't price below ~$3,000), not a value signal -- a buried 4th-string RB and a real
# committee piece can carry the identical $4,000 tag despite wildly different real
# expectation. Using one pool-wide median for every fallback player (the old formula)
# meant distinct deep-bench players at the same price point came out with the exact
# SAME inflated projection -- confirmed live 2026-10-12: four different RBs across
# three different teams, all $4,000, all proj=8.8 -- which is also why they kept
# showing up as "free" GPP value across many optimizer lineups despite being pure
# roster filler. Measured median value/$1k by tier:
#   <=3500: 0.76 | 3501-4500: 0.78 | 4501-5500: 1.69 | 5501-6500: 2.14 | 6501-7500: 2.17 | 7501+: 2.16
.nfl_baseline_tier_val <- function(sal) {
  fifelse(sal <= 3500, 0.76,
  fifelse(sal <= 4500, 0.78,
  fifelse(sal <= 5500, 1.69,
  fifelse(sal <= 6500, 2.14,
  fifelse(sal <= 7500, 2.17, 2.16)))))
}
.nfl_baseline <- function(pool) {
  proj <- as.numeric(pool$dk_avg); bad <- is.na(proj) | proj <= 0
  proj[bad] <- (pool$salary[bad] / 1000) * .nfl_baseline_tier_val(pool$salary[bad])
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
    # Prefer the external model's own sim_sd when supplied. Its ppr_low/ppr_high
    # are now player-specific (previously one fixed width per position, which
    # covered only ~67% of stud outcomes), and sim_sd is derived from that same
    # span - so deriving it here would duplicate the arithmetic and drift if the
    # producer changes its interval definition. Scaled by k to this site's
    # scoring, same as the bands. Falls back to the derived value when absent.
    if (!"sim_sd" %in% names(ext)) ext[, sim_sd := NA_real_]
    ext[, `:=`(e_floor = pmax(k * ppr_low, 0),
               e_ceil  = pmax(k * ppr_high, e_proj),
               e_sd    = pmax(fifelse(is.finite(sim_sd) & sim_sd > 0,
                                      k * sim_sd,
                                      k * (ppr_high - ppr_low) / 2.563), 4))]
    # p_zero: prefer the external model's per-player inactive probability when it
    # supplies one. It is graded off the current injury report (Out ~0.97,
    # Doubtful ~0.68, Questionable ~0.24, healthy 0.03) rather than the flat 0.03
    # this line used to hardcode, which priced a Questionable back identically to
    # an ironman starter. Falls back to 0.03 per row when the field is absent or
    # unusable, so an older projection file still behaves exactly as before.
    if (!"p_zero" %in% names(ext)) ext[, p_zero := NA_real_]
    ext[, e_pzero := fifelse(is.finite(p_zero) & p_zero >= 0 & p_zero <= 0.99,
                             p_zero, 0.03)]
    pool[ext, on = "norm", `:=`(proj = i.e_proj, sim_sd = i.e_sd, ceil = i.e_ceil,
                                floor = i.e_floor, p_zero = i.e_pzero)]
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
  pool[, `:=`(game_total_z = 0, exp_margin = 11.1)]   # exp_margin default = empirical mean |margin| (2021-25)
  vg <- tryCatch(vegas_games("nfl", slate$date), error = function(e) NULL)
  if (!is.null(vg) && nrow(vg) && "vegas_total" %in% names(vg)) {
    vg <- as.data.table(vg)
    pool[vg[, .(game_id, vt = vegas_total)], on = "game_id", vt := i.vt]
    if (sum(!is.na(pool$vt)) >= 0.5 * nrow(pool)) {
      mu <- mean(pool$vt, na.rm = TRUE); sv <- sd(pool$vt, na.rm = TRUE)
      pool[, game_total_z := fifelse(is.na(vt) | !is.finite(sv) | sv == 0, 0, (vt - mu) / sv)]
      pool[, ceil := pmax(ceil + 0.05 * game_total_z * proj, proj)]
      msg("  vegas: NFL game totals applied to", sum(!is.na(pool$vt)), "of", nrow(pool), "players")
    }
    pool[, vt := NULL]
    # real closing spread -> pregame expected |margin| for scenario-conditioned correlation
    # (nfl_correlation, sports/nfl/correlate.R). Reuses the SAME game_id-matched vg table.
    if ("spread" %in% names(vg)) {
      pool[vg[, .(game_id, sp = spread)], on = "game_id", sp := i.sp]
      pool[is.finite(sp), exp_margin := abs(sp)]
      pool[, sp := NULL]
    }
  }

  # --- DROP players not on an active roster (IR/cut/retired/practice-squad) -----------
  # Closes a real blind spot in apply_inactives(): the ESPN injury feed only reports
  # THIS WEEK's game-status, so a long-term IR player (season-ending, placed weeks ago)
  # can fall off it entirely — see nfl_inactive_roster() for the confirmed case (Ricky
  # Pearsall, on IR, absent from the injury feed but still in DK's salary file). ALL
  # positions, not just QB — this is a roster-eligibility signal, not a depth-chart one.
  inactive <- tryCatch(nfl_inactive_roster(), error = function(e) NULL)
  if (!is.null(inactive) && "team" %in% names(pool)) {
    key <- paste(pool$norm, toupper(pool$team))
    ikey <- paste(inactive$norm, inactive$team)
    drop_ir <- key %in% ikey
    if (any(drop_ir)) { msg(sprintf("  nfl: dropped %d player(s) not on an active roster (%s)", sum(drop_ir),
                                    paste(head(pool$player_name[drop_ir], 5), collapse = ", ")))
      pool <- pool[!drop_ir] }
  }

  # --- DROP backup QBs (won't play, so their salary-baseline "value" is fictional) -----
  # QB is a clean 1-starter-per-team position (unlike RB/WR/TE committees) — see
  # nfl_qb_starters() for the depth-chart source + rationale. Skips (never blocks the
  # build) if the depth-chart pull is unavailable and there's no cache yet.
  starters <- tryCatch(nfl_qb_starters(), error = function(e) NULL)
  if (!is.null(starters)) {
    is_qb <- toupper(pool$position) == "QB"
    drop <- is_qb & !(pool$norm %in% starters)
    if (any(drop)) { msg(sprintf("  nfl: dropped %d backup QB(s) (%s)", sum(drop),
                                 paste(head(pool$player_name[drop], 5), collapse = ", ")))
      pool <- pool[!drop] }
  }

  # --- projected ownership: TRAINED model (leave-one-slate-out validated 2026-10-08,
  # 12 slates / 1972 rows, 10 of 10 valid folds: mean per-slate corr 0.42->0.69, pooled
  # corr 0.28->0.45, RMSE -52%) when one exists, else the value-rank heuristic (cold-start
  # fallback; also the fallback for any sport too data-thin to train, see train_ownership_model).
  own_pred <- tryCatch(predict_ownership(pool, "nfl"), error = function(e) NULL)
  if (!is.null(own_pred) && length(own_pred) == nrow(pool)) {
    pool[, own := pmax(own_pred, 0.005)]
    msg("  NFL ownership: trained model")
  } else {
    pool[, .val := proj / pmax(salary / 1000, 0.1)]
    r <- frank(pool$.val) / nrow(pool)
    pool[, own := pmax(0.005, pmin(0.5, 0.02 + 0.42 * r^2))]
    pool[, .val := NULL]
  }

  persist_salaries(pool, slate$slate_id, "nfl")
  pool[, .(player_id, player_name, dk_id, team, game_id, position,
           salary, proj, sim_sd, ceil, floor, p_zero, own, game_total_z, exp_margin)]
}
