# DVOA-style opponent-adjusted ratings for NFL, ported from the cfb-modeling
# project's 10_build_dvoa_ratings.R (5 variants v3-v7, same iterative
# empirical-Bayes solver). See that file's header for the full rationale;
# summarized here for what's NFL-specific.
#
# What this brings that the existing game model doesn't have: the current
# home_off_epa_r4 etc. features (R/feature_registry.R) are simple rolling
# means of a team's own play-level stats - NOT opponent-adjusted. DVOA's whole
# point is the opponent adjustment (an iterative solve, KenPom-style), which
# doesn't exist anywhere in this project's game-market features today. That's
# the part worth testing, independent of whether EPA-winsorization itself adds
# anything - CFB's own finding was "capping doesn't matter much, opponent
# adjustment plus multi-variant AGREEMENT does, but only as a confidence
# filter on an existing edge, not as a standalone replacement rating."
#
# Leak-free: as-of (week < w within a season), prior-season carryover via
# regression toward the league mean, same K/iters/REG as CFB for consistency
# (these are generic empirical-Bayes hyperparameters, not CFB-specific).

nfl_dvoa_variant_names <- function() c("v3", "v4", "v5", "v6", "v7")

nfl_dvoa_cap <- function(x, q) pmin(pmax(x, q[1]), q[2])

# Garbage-time filter mirrors this project's own convention where one exists
# and CFB's score+period rule elsewhere: Q3+ margin >28, Q4+ margin >21.
# Applied from the POSSESSION team's perspective (score_differential is
# already posteam-relative in nflverse pbp, so no sign-flip needed).
nfl_dvoa_garbage <- function(qtr, score_differential) {
  (!is.na(qtr) & !is.na(score_differential)) &
    ((qtr >= 3 & abs(score_differential) > 28) |
       (qtr >= 4 & abs(score_differential) > 21))
}

# Builds the 5 variant team-game tables from raw pbp. Returns a named list of
# tibbles (season, week, game_id, team, opponent, ov, dv) - offense/defense
# value per team-game, ready for nfl_dvoa_build_asof().
nfl_dvoa_team_games <- function(pbp, min_plays_side = 15) {
  p <- pbp |>
    dplyr::filter(
      .data$season_type == "REG",
      !is.na(.data$epa), !is.na(.data$posteam), !is.na(.data$defteam),
      .data$posteam != "", .data$defteam != "",
      .data$play_type %in% c("pass", "run")
    ) |>
    dplyr::transmute(
      season = as.integer(.data$season), week = as.integer(.data$week),
      game_id = .data$game_id,
      posteam = normalize_team(.data$posteam), defteam = normalize_team(.data$defteam),
      epa = as.numeric(.data$epa),
      qtr = suppressWarnings(as.numeric(.data$qtr)),
      sd  = suppressWarnings(as.numeric(.data$score_differential)),
      down = suppressWarnings(as.numeric(.data$down)),
      ydstogo = suppressWarnings(as.numeric(.data$ydstogo)),
      is_pass = as.integer(.data$pass == 1), is_rush = as.integer(.data$rush == 1)
    )
  p <- p |> dplyr::filter(!nfl_dvoa_garbage(.data$qtr, .data$sd))
  cat(sprintf("  NFL DVOA: %s plays after garbage filter\n", format(nrow(p), big.mark = ",")))

  q05 <- stats::quantile(p$epa, c(0.05, 0.95), na.rm = TRUE)
  q02 <- stats::quantile(p$epa, c(0.02, 0.98), na.rm = TRUE)
  q01 <- stats::quantile(p$epa, c(0.01, 0.99), na.rm = TRUE)

  p <- p |> dplyr::mutate(
    pdown = (!is.na(.data$down) & !is.na(.data$ydstogo)) &
      ((.data$down == 2 & .data$ydstogo >= 8) | (.data$down >= 3 & .data$ydstogo >= 5)),
    m_v3 = nfl_dvoa_cap(.data$epa, q05),
    m_v4 = nfl_dvoa_cap(.data$epa, q01),
    m_v5 = as.integer(.data$epa > 0),
    m_v6 = ifelse(.data$pdown, 1.5, 1.0) * nfl_dvoa_cap(.data$epa, q02)
  )

  agg_simple <- function(col) {
    o <- p |> dplyr::group_by(.data$season, .data$week, .data$game_id,
                              team = .data$posteam, opponent = .data$defteam) |>
      dplyr::summarise(ov = mean(.data[[col]], na.rm = TRUE), n_o = dplyr::n(), .groups = "drop")
    d <- p |> dplyr::group_by(.data$season, .data$week, .data$game_id,
                              team = .data$defteam, opponent = .data$posteam) |>
      dplyr::summarise(dv = mean(.data[[col]], na.rm = TRUE), n_d = dplyr::n(), .groups = "drop")
    o |> dplyr::inner_join(d, by = c("season", "week", "game_id", "team", "opponent")) |>
      dplyr::filter(.data$n_o >= min_plays_side, .data$n_d >= min_plays_side) |>
      dplyr::select(-"n_o", -"n_d")
  }
  agg_rushpass <- function() {
    o <- p |> dplyr::group_by(.data$season, .data$week, .data$game_id,
                              team = .data$posteam, opponent = .data$defteam) |>
      dplyr::summarise(
        op = mean(nfl_dvoa_cap(.data$epa, q02)[.data$is_pass == 1], na.rm = TRUE),
        orr = mean(nfl_dvoa_cap(.data$epa, q02)[.data$is_rush == 1], na.rm = TRUE),
        n_o = dplyr::n(), .groups = "drop"
      )
    d <- p |> dplyr::group_by(.data$season, .data$week, .data$game_id,
                              team = .data$defteam, opponent = .data$posteam) |>
      dplyr::summarise(
        dp = mean(nfl_dvoa_cap(.data$epa, q02)[.data$is_pass == 1], na.rm = TRUE),
        dr = mean(nfl_dvoa_cap(.data$epa, q02)[.data$is_rush == 1], na.rm = TRUE),
        n_d = dplyr::n(), .groups = "drop"
      )
    o |> dplyr::inner_join(d, by = c("season", "week", "game_id", "team", "opponent")) |>
      dplyr::filter(.data$n_o >= min_plays_side, .data$n_d >= min_plays_side,
                    !is.na(.data$op), !is.na(.data$orr), !is.na(.data$dp), !is.na(.data$dr)) |>
      dplyr::transmute(.data$season, .data$week, .data$game_id, .data$team, .data$opponent,
                       ov = 0.6 * .data$op + 0.4 * .data$orr, dv = 0.6 * .data$dp + 0.4 * .data$dr)
  }
  list(v3 = agg_simple("m_v3"), v4 = agg_simple("m_v4"), v5 = agg_simple("m_v5"),
       v6 = agg_simple("m_v6"), v7 = agg_rushpass())
}

# Same generic iterative opponent-adjustment as CFB's `adjust()` - identical
# algorithm, team-name-agnostic.
nfl_dvoa_adjust <- function(rows, prior_off, prior_def, lg, k_shrink = 5, iters = 8) {
  rows <- rows[!is.na(rows$oval) & !is.na(rows$dval) & !is.na(rows$opponent), , drop = FALSE]
  teams <- union(rows$team, rows$opponent)
  if (!length(teams)) {
    return(tibble::tibble(team = character(), adj_off = numeric(), adj_def = numeric()))
  }
  fill <- function(pri) {
    v <- stats::setNames(rep(lg, length(teams)), teams)
    k <- intersect(names(pri), teams); v[k] <- pri[k]; v
  }
  ao <- fill(prior_off); ad <- fill(prior_def); po <- ao; pd <- ad
  n_i <- tapply(rep(1, nrow(rows)), rows$team, sum)
  for (it in seq_len(iters)) {
    off_c <- rows$oval - (ad[rows$opponent] - lg)
    def_c <- rows$dval - (ao[rows$opponent] - lg)
    ar <- tapply(off_c, rows$team, mean); dr <- tapply(def_c, rows$team, mean)
    tu <- names(ar); ni <- as.numeric(n_i[tu])
    ao[tu] <- (ni * as.numeric(ar[tu]) + k_shrink * po[tu]) / (ni + k_shrink)
    ad[tu] <- (ni * as.numeric(dr[tu]) + k_shrink * pd[tu]) / (ni + k_shrink)
  }
  tibble::tibble(team = names(ao), adj_off = as.numeric(ao), adj_def = as.numeric(ad))
}

# As-of ratings across seasons for one variant's team-game table.
nfl_dvoa_build_asof <- function(tg, label, reg = 0.5) {
  lg <- mean(c(tg$ov, tg$dv), na.rm = TRUE)
  out <- list(); fin <- list()
  for (s in sort(unique(tg$season))) {
    prev <- fin[[as.character(s - 1)]]
    if (is.null(prev)) {
      po <- stats::setNames(numeric(0), character(0)); pd <- po
    } else {
      po <- stats::setNames(lg + reg * (prev$adj_off - lg), prev$team)
      pd <- stats::setNames(lg + reg * (prev$adj_def - lg), prev$team)
    }
    ss <- tg |> dplyr::filter(.data$season == s)
    for (w in sort(unique(ss$week))) {
      h <- ss |> dplyr::filter(.data$week < w)
      if (nrow(h) < 20) next
      out[[length(out) + 1]] <- nfl_dvoa_adjust(
        data.frame(team = h$team, opponent = h$opponent, oval = h$ov, dval = h$dv), po, pd, lg
      ) |> dplyr::mutate(season = s, as_of_week = w)
    }
    fin[[as.character(s)]] <- nfl_dvoa_adjust(
      data.frame(team = ss$team, opponent = ss$opponent, oval = ss$ov, dval = ss$dv), po, pd, lg
    )
  }
  dplyr::bind_rows(out) |>
    dplyr::rename(!!paste0("aoff_", label) := "adj_off", !!paste0("adef_", label) := "adj_def")
}

nfl_dvoa_build_all <- function(pbp, min_plays_side = 15, reg = 0.5) {
  tgs <- nfl_dvoa_team_games(pbp, min_plays_side)
  res <- NULL
  for (nm in names(tgs)) {
    a <- nfl_dvoa_build_asof(tgs[[nm]], nm, reg)
    res <- if (is.null(res)) a else dplyr::full_join(res, a, by = c("team", "season", "as_of_week"))
    cat(sprintf("  %s: %d as-of rows\n", nm, nrow(a)))
  }
  res
}
