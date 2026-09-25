fantasy_lagged_roll <- function(x, window, statistic = c("mean", "sd")) {
  statistic <- match.arg(statistic)
  slider::slide_dbl(
    dplyr::lag(as.numeric(x)),
    function(values) {
      if (!length(values) || all(is.na(values))) return(NA_real_)
      if (statistic == "mean") return(mean(values, na.rm = TRUE))
      if (sum(is.finite(values)) < 2) return(0)
      stats::sd(values, na.rm = TRUE)
    },
    .before = window - 1L,
    .complete = FALSE
  )
}

fantasy_target_specifications <- function() {
  list(
    receptions = list(
      outcome = "receptions",
      family = "receiving",
      objective = "count:poisson",
      label = "Receptions"
    ),
    receiving_yards = list(
      outcome = "receiving_yards",
      family = "receiving",
      objective = "reg:squarederror",
      label = "Receiving yards"
    ),
    rushing_yards = list(
      outcome = "rushing_yards",
      family = "rushing",
      objective = "reg:squarederror",
      label = "Rushing yards"
    ),
    passing_yards = list(
      outcome = "passing_yards",
      family = "passing",
      objective = "reg:squarederror",
      label = "Passing yards"
    ),
    passing_tds = list(
      outcome = "passing_tds",
      family = "passing",
      objective = "count:poisson",
      label = "Passing touchdowns"
    ),
    interceptions = list(
      outcome = "passing_interceptions",
      family = "passing",
      objective = "count:poisson",
      label = "Interceptions thrown"
    ),
    rushing_tds = list(
      outcome = "rushing_tds",
      family = "rushing",
      objective = "count:poisson",
      label = "Rushing touchdowns"
    ),
    receiving_tds = list(
      outcome = "receiving_tds",
      family = "receiving",
      objective = "count:poisson",
      label = "Receiving touchdowns"
    ),
    fumbles_lost = list(
      outcome = "offensive_fumbles_lost",
      family = "all",
      objective = "count:poisson",
      label = "Fumbles lost"
    )
  )
}

## QB x RECEIVER PAIR CHEMISTRY.
##
## Who is throwing materially changes a receiver's output, independent of team
## pass volume. Drake London: 20.5 PPR/gm with Penix vs 13.9 with Cousins on
## 31.3 vs 31.9 team attempts/gm -- target share, not volume. League-wide across
## 601 receiver-seasons where a receiver played for >=2 QBs in the same season,
## a receiver's prior over/under-performance with a specific QB predicts his
## FUTURE performance with that QB out of sample at dR2 +0.028 (PPR) / +0.035
## (target share), coefficient ~1.0 -- it carries forward at nearly full strength.
##
## GATED ON A QB CHANGE, deliberately. Tuning (scripts/98) compared plain
## shrinkage (K=4/12), a hard cap, and a min-sample gate: all of them helped on
## QB-change games but DEGRADED the tail, costing R2 -0.006 to -0.009 at the top
## 5% of |delta| where small-sample noise dominates. Applying the delta only when
## the QB actually changed from the player's last game turns that tail from
## -0.0072 to +0.0099 and improves every other cut too (receiving yards: ALL
## dR2 +0.0025, QB-change dR2 +0.0144, MAE -1.26%). When the QB is stable the
## receiver's own rolling features already encode the relationship, so the delta
## was adding noise and nothing else.
##
## Deployment note: for an upcoming game the starter is imputed as the team's most
## recent primary QB. That means a change is detected the week AFTER it happens
## unless config/qb_starter_overrides.csv names the expected starter (columns:
## season, week, team, qb). Week 3 2026 is exactly why that override exists --
## ATL switched Rush -> Penix and the model had no way to know pre-lock.
QB_PAIR_SHRINKAGE_K <- 8

add_qb_pair_features <- function(players) {
  need <- c("player_id", "season", "week", "team", "position", "fantasy_points_ppr")
  if (!all(need %in% names(players))) {
    players$qb_pair_delta_r5 <- 0
    players$qb_pair_games_r5 <- 0
    return(players)
  }

  # primary QB per team-game = most pass attempts (from these same rows)
  qb_by_game <- players |>
    dplyr::filter(.data$position == "QB", !is.na(.data$attempts), .data$attempts > 0) |>
    dplyr::group_by(.data$season, .data$week, .data$team) |>
    dplyr::summarise(qb = .data$player_display_name[which.max(.data$attempts)], .groups = "drop")

  # upcoming games carry no box score, so the QB is unknown -> carry the team's most
  # recent known starter forward, then let the override file correct it when a change
  # is announced before we would otherwise see it.
  starters <- qb_by_game |>
    dplyr::arrange(.data$team, .data$season, .data$week)
  players <- players |>
    dplyr::left_join(qb_by_game, by = c("season", "week", "team")) |>
    dplyr::arrange(.data$team, .data$season, .data$week) |>
    dplyr::group_by(.data$team) |>
    dplyr::mutate(qb = .data$qb |> vctrs::vec_fill_missing(direction = "down")) |>
    dplyr::ungroup()

  ov_path <- "config/qb_starter_overrides.csv"
  if (file.exists(ov_path)) {
    ov <- tryCatch(readr::read_csv(ov_path, show_col_types = FALSE), error = function(e) NULL)
    if (!is.null(ov) && all(c("season", "week", "team", "qb") %in% names(ov))) {
      ov <- ov |> dplyr::transmute(season = as.integer(.data$season), week = as.integer(.data$week),
                                    team = normalize_team(.data$team), qb_override = as.character(.data$qb))
      players <- players |>
        dplyr::left_join(ov, by = c("season", "week", "team")) |>
        dplyr::mutate(qb = dplyr::coalesce(.data$qb_override, .data$qb)) |>
        dplyr::select(-"qb_override")
      message("QB overrides applied: ", nrow(ov))
    }
  }

  players <- players |>
    dplyr::arrange(.data$player_id, .data$season, .data$week) |>
    dplyr::group_by(.data$player_id) |>
    dplyr::mutate(
      .g = dplyr::row_number(),
      .prior_ppr = (cumsum(dplyr::coalesce(.data$fantasy_points_ppr, 0)) -
                      dplyr::coalesce(.data$fantasy_points_ppr, 0)) / pmax(.data$.g - 1, 1),
      .prior_ppr = dplyr::if_else(.data$.g == 1, NA_real_, .data$.prior_ppr),
      .prev_qb = dplyr::lag(.data$qb),
      .qb_changed = !is.na(.data$.prev_qb) & !is.na(.data$qb) & .data$qb != .data$.prev_qb
    ) |>
    dplyr::group_by(.data$player_id, .data$qb) |>
    dplyr::mutate(
      .pair_i = dplyr::row_number(),
      .prior_pair_ppr = (cumsum(dplyr::coalesce(.data$fantasy_points_ppr, 0)) -
                           dplyr::coalesce(.data$fantasy_points_ppr, 0)) / pmax(.data$.pair_i - 1, 1),
      .prior_pair_ppr = dplyr::if_else(.data$.pair_i == 1, NA_real_, .data$.prior_pair_ppr),
      .n_pair = .data$.pair_i - 1L
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      .raw = dplyr::coalesce(.data$.prior_pair_ppr - .data$.prior_ppr, 0),
      qb_pair_games_r5 = as.numeric(dplyr::coalesce(.data$.n_pair, 0L)),
      qb_pair_delta_r5 = dplyr::if_else(
        .data$.qb_changed,
        .data$.raw * .data$qb_pair_games_r5 / (.data$qb_pair_games_r5 + QB_PAIR_SHRINKAGE_K),
        0
      ),
      qb_pair_delta_r5 = dplyr::coalesce(.data$qb_pair_delta_r5, 0),
      # Pass-catchers only. A QB is trivially "paired" with himself, which made the
      # raw feature light up for QBs (Cooper Rush with QB Cooper Rush) -- meaningless,
      # and for a QB "the QB changed" really means he was benched, which the position's
      # own usage features already say far more directly.
      qb_pair_delta_r5 = dplyr::if_else(.data$position == "QB", 0, .data$qb_pair_delta_r5),
      qb_pair_games_r5 = dplyr::if_else(.data$position == "QB", 0, .data$qb_pair_games_r5)
    ) |>
    dplyr::select(-dplyr::starts_with("."))

  players
}

prepare_fantasy_player_features <- function(player_stats, game_context) {
  measures <- c(
    "completions", "attempts", "passing_yards", "passing_tds",
    "passing_interceptions", "passing_first_downs", "passing_air_yards",
    "carries", "rushing_yards", "rushing_tds", "rushing_first_downs",
    "receptions", "targets", "receiving_yards", "receiving_tds",
    "receiving_first_downs", "receiving_air_yards",
    "target_share", "air_yards_share", "wopr",
    "passing_2pt_conversions", "rushing_2pt_conversions",
    "receiving_2pt_conversions", "fumbles_lost_total",
    "fantasy_points_ppr"
  )
  measures <- intersect(measures, names(player_stats))

  players <- player_stats |>
    dplyr::filter(
      .data$season_type == "REG",
      .data$position %in% c("QB", "RB", "FB", "WR", "TE")
    ) |>
    dplyr::mutate(
      position = dplyr::if_else(.data$position == "FB", "RB", .data$position),
      team = normalize_team(.data$team),
      opponent_team = normalize_team(.data$opponent_team),
      dplyr::across(
        dplyr::all_of(measures),
        ~ dplyr::coalesce(as.numeric(.x), 0)
      ),
      offensive_fumbles_lost = dplyr::coalesce(
        as.numeric(.data$fumbles_lost_total),
        0
      ),
      two_point_conversions =
        dplyr::coalesce(as.numeric(.data$passing_2pt_conversions), 0) +
        dplyr::coalesce(as.numeric(.data$rushing_2pt_conversions), 0) +
        dplyr::coalesce(as.numeric(.data$receiving_2pt_conversions), 0),
      opportunity = .data$attempts + .data$carries + .data$targets,
      ppr_points_actual =
        0.04 * .data$passing_yards +
        4 * .data$passing_tds -
        2 * .data$passing_interceptions +
        0.1 * .data$rushing_yards +
        6 * .data$rushing_tds +
        .data$receptions +
        0.1 * .data$receiving_yards +
        6 * .data$receiving_tds -
        2 * .data$offensive_fumbles_lost +
        2 * .data$two_point_conversions
    ) |>
    dplyr::left_join(
      game_context,
      by = c("game_id", "season", "week")
    ) |>
    dplyr::mutate(
      game_date = dplyr::coalesce(
        as.Date(.data$game_date),
        as.Date(NA)
      ),
      is_home = as.integer(.data$team == .data$home_team),
      team_spread = dplyr::if_else(
        .data$is_home == 1L,
        .data$home_line,
        -.data$home_line
      ),
      implied_team_total = dplyr::if_else(
        .data$is_home == 1L,
        .data$total_line / 2 - .data$home_line / 2,
        .data$total_line / 2 + .data$home_line / 2
      ),
      position_qb = as.integer(.data$position == "QB"),
      position_rb = as.integer(.data$position == "RB"),
      position_wr = as.integer(.data$position == "WR"),
      position_te = as.integer(.data$position == "TE")
    )

  ## Snap share (scripts/86). Every other usage measure here is TARGET-derived,
  ## so it only registers a player once the ball comes his way. Snap share says
  ## whether he is on the field at all - the part of role that moves first when
  ## a receiver is promoted, and the gap that target-based rolling means take
  ## weeks to close. Added to rolling_measures below so the existing lagged
  ## roll makes it leak-free exactly like everything else; the raw same-game
  ## value is never a feature.
  snap_path <- "data/processed/nfl_snap_features.rds"
  if (file.exists(snap_path)) {
    snaps <- readRDS(snap_path)
    n_before_snap <- nrow(players)
    players <- players |>
      dplyr::left_join(snaps, by = c("season", "week", "player_id"))
    if (nrow(players) != n_before_snap) {
      stop("Snap join changed row count: ", n_before_snap, " -> ", nrow(players),
           ". Duplicate (season, week, player_id) in nfl_snap_features.rds.",
           call. = FALSE)
    }
  } else {
    message("No ", snap_path, " (run scripts/86); snap features unavailable.")
  }

  rolling_measures <- c(
    "completions", "attempts", "passing_yards", "passing_tds",
    "passing_interceptions", "passing_first_downs", "passing_air_yards",
    "carries", "rushing_yards", "rushing_tds", "rushing_first_downs",
    "receptions", "targets", "receiving_yards", "receiving_tds",
    "receiving_first_downs", "receiving_air_yards",
    "target_share", "air_yards_share", "wopr",
    "offensive_fumbles_lost", "two_point_conversions",
    "opportunity", "fantasy_points_ppr", "ppr_points_actual",
    "snap_share", "offense_snaps"
  )
  rolling_measures <- intersect(rolling_measures, names(players))

  players <- players |>
    dplyr::arrange(.data$player_id, .data$game_date, .data$game_id) |>
    dplyr::group_by(.data$player_id) |>
    dplyr::mutate(prior_games = dplyr::row_number() - 1L)

  ## ROLLING WINDOWS: BLEND ACROSS SEASON BOUNDARIES, DON'T HARD-CUT.
  ##
  ## Confirmed bug (2026-09-23): the ORIGINAL code grouped these rolling means
  ## by player_id only (no season), so an r8 window in week 2-3 of a new
  ## season was still majority-composed of the tail of the PRIOR season. For a
  ## player who changed teams/roles, this silently anchored his "recent usage"
  ## features to the OLD context and made the model nearly unresponsive to
  ## real in-season role change or decline for the first several weeks of
  ## every season, every year. Verified live: 64% of fantasy-relevant players
  ## had <0.5pt week-over-week projection movement regardless of actual
  ## outcome, and the worst weekly misses in the league (e.g. a player scoring
  ## 3 actual on a 15-point projection) saw next week's projection INCREASE.
  ##
  ## A first attempt just hard-reset the window at each season boundary
  ## (group_by(player_id, season) only). That fixed the reactivity but
  ## overcorrected the other way: verified live it also cut ~89% of
  ## established, UNCHANGED starters' projections (mean -2.6pts) purely from
  ## discarding a legitimate stabilizing prior -- 2 games below your career
  ## norm is often just normal early-season variance, not decline, and a
  ## proven starter's true talent didn't reset just because the calendar did.
  ##
  ## Fix: blend the within-season rolling mean with the player's PRIOR
  ## season's per-game average for that measure, with the blend weight
  ## shifting to the current season fast (fully in-season by 3 real current-
  ## season games, independent of the window length) rather than only once the
  ## window itself fills (which for r8 would take 8 games -- too slow for the
  ## exact case this is meant to fix, e.g. a trade). This keeps real
  ## responsiveness to genuine change while not treating early-season noise as
  ## a level shift for players whose situation hasn't actually changed.
  season_priors <- players |>
    dplyr::group_by(.data$player_id, .data$season) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(rolling_measures),
        ~ mean(.x, na.rm = TRUE),
        .names = "{.col}_prior"
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$player_id, .data$season) |>
    dplyr::group_by(.data$player_id) |>
    dplyr::mutate(
      dplyr::across(dplyr::ends_with("_prior"), ~ dplyr::lag(.x))
    ) |>
    dplyr::ungroup()

  players <- players |>
    dplyr::left_join(season_priors, by = c("player_id", "season")) |>
    dplyr::group_by(.data$player_id, .data$season) |>
    dplyr::mutate(prior_games_season = dplyr::row_number() - 1L)

  ## Tested tightening this to 2 (2026-09-23) to react faster to two confirmed
  ## live cases (Wan'Dale Robinson traded; Theo Johnson displaced by a
  ## teammate's target-share takeover) -- reverted: verified it barely moved
  ## either player's projection (Wan'Dale 11.13->11.05, Theo Johnson actually
  ## went UP 8.39->8.46), so the convergence window isn't the real bottleneck
  ## for these cases. Something else (likely the static preseason_role_rank/
  ## draft_priority features, which never update in-season, or the salary-
  ## stack blend pulling back toward a DK-salary-implied expectation) is
  ## holding their floor up regardless of rolling-window weighting. Needs a
  ## real follow-up investigation, not a hyperparameter tweak.
  blend_convergence_games <- 3   # fully weighted to current season by this many current-season games
  for (window in c(3L, 5L, 8L)) {
    players <- players |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(rolling_measures),
          ~ fantasy_lagged_roll(.x, window, "mean"),
          .names = "{.col}_r{window}_within"
        )
      )
    w <- pmin(players$prior_games_season / blend_convergence_games, 1)
    for (measure in rolling_measures) {
      within_col <- paste0(measure, "_r", window, "_within")
      prior_col  <- paste0(measure, "_prior")
      out_col    <- paste0(measure, "_r", window)
      within_val <- dplyr::coalesce(players[[within_col]], 0)
      prior_val  <- dplyr::coalesce(players[[prior_col]], players[[within_col]], 0)
      players[[out_col]] <- w * within_val + (1 - w) * prior_val
      players[[out_col]][players$prior_games == 0L] <- NA_real_  # true rookie debut: no prior at all
      players[[within_col]] <- NULL
    }
  }
  players <- players |>
    dplyr::select(-dplyr::ends_with("_prior"))

  players <- add_qb_pair_features(players)

  volatility_measures <- intersect(
    c(
      "attempts", "passing_yards", "carries", "rushing_yards",
      "targets", "receptions", "receiving_yards", "fantasy_points_ppr"
    ),
    names(players)
  )
  players <- players |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(volatility_measures),
        ~ fantasy_lagged_roll(.x, 5L, "sd"),
        .names = "{.col}_sd5"
      )
    ) |>
    dplyr::ungroup()

  ## ROLE-TREND features: short window minus long window, so a role that is
  ## CHANGING is visible as a level rather than as a difference the booster has
  ## to discover between two of 130+ columns.
  ##
  ## Motivated by a specific measured failure, not added speculatively. On WR
  ## rows - 40% of the pool and the one position where the model loses to DK
  ## salary - the error decomposition showed:
  ##   - when salary disagrees with the model, salary is directionally right,
  ##     monotonically (model bias -1.59 in the bucket where salary says lower,
  ##     +1.01 where salary says higher)
  ##   - the gap concentrates in players whose target share is RISING FAST, and
  ##     in veterans, i.e. exactly where a trailing mean lags a role change
  ##
  ## Salary reprices weekly off the current depth chart; rolling means take
  ## several games to catch up. These deltas are the cheapest way to hand the
  ## model the same information, and unlike salary they exist for every player
  ## whether or not he is on a DK main slate.
  trend_measures <- intersect(
    c("targets", "target_share", "receptions", "receiving_air_yards",
      "air_yards_share", "wopr", "carries", "opportunity", "attempts",
      "snap_share"),
    names(players)
  )
  for (m in trend_measures) {
    short <- paste0(m, "_r3")
    long <- paste0(m, "_r8")
    if (all(c(short, long) %in% names(players))) {
      players[[paste0(m, "_trend")]] <-
        dplyr::coalesce(players[[short]], 0) - dplyr::coalesce(players[[long]], 0)
    }
  }

  ## NOT WIRED IN: add_fantasy_redzone_defense() and
  ## add_fantasy_matchup_features() are defined below, validated, and
  ## deliberately not called. Measured 2026-09-15 on 2023-2025 walk-forward:
  ##
  ##   aggregate PPR MAE  +0.16% / 0.00% / +0.13%   (2023/24/25)
  ##   aggregate PPR R2   -0.0020 / -0.0016 / -0.0023
  ##   rushing_yards      +0.06% MAE, R2 -0.00133
  ##   passing_yards      +0.45% MAE, R2 -0.00817
  ##
  ## This was not a plumbing failure. The models ranked the features highly
  ## (matchup_rush_volume_x_def #8 by gain for rushing_yards) and the effect is
  ## real in the raw data - RB-weeks at matched volume (~15 carries) against
  ## the weakest vs strongest run defenses averaged 66.0 vs 56.8 actual rushing
  ## yards. The information is simply redundant with what a player's own
  ## trailing usage already encodes, and ~20 extra columns cost more variance
  ## than they return on a fixed depth-3/180-round booster.
  ##
  ## Left in place rather than deleted because the artifacts and joins are
  ## validated and cheap to re-enable (add the two calls back here), and a
  ## different model family or a retuned booster may yet extract what this one
  ## could not. Same lesson as the DVOA spread/total test in this repo.
  add_fantasy_opponent_features(players)
}

build_fantasy_2026_features <- function(
  player_stats,
  rosters,
  historical_game_context,
  upcoming_game_context,
  schedules = NULL
) {
  current_roster <- rosters |>
    dplyr::filter(
      .data$season == 2026,
      .data$status == "ACT",
      .data$position %in% c("QB", "RB", "FB", "WR", "TE"),
      !is.na(.data$gsis_id)
    ) |>
    dplyr::arrange(.data$gsis_id, dplyr::desc(.data$week)) |>
    dplyr::group_by(.data$gsis_id) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      player_id = .data$gsis_id,
      player_display_name = .data$full_name,
      position = .data$position,
      team = normalize_team(.data$team),
      entry_year = as.numeric(.data$entry_year),
      draft_number = as.numeric(.data$draft_number),
      years_exp = as.numeric(.data$years_exp)
    )

  game_teams <- dplyr::bind_rows(
    upcoming_game_context |>
      dplyr::transmute(
        .data$game_id, .data$season, .data$week,
        team = .data$home_team,
        opponent_team = .data$away_team
      ),
    upcoming_game_context |>
      dplyr::transmute(
        .data$game_id, .data$season, .data$week,
        team = .data$away_team,
        opponent_team = .data$home_team
      )
  )
  synthetic <- current_roster |>
    dplyr::inner_join(game_teams, by = "team") |>
    dplyr::mutate(season = 2026L, season_type = "REG")

  game_context <- dplyr::bind_rows(historical_game_context, upcoming_game_context)

  ## Confirmed bug (2026-09-24): historical_game_context (td_game_context.rds) is
  ## a slow-to-refresh cache that has ZERO 2026 rows until scripts/12 is re-run
  ## for the season -- so every ALREADY-PLAYED 2026 game (weeks before the one
  ## being projected) gets game_date = NA from the join in
  ## prepare_fantasy_player_features(), while the synthetic row for the week
  ## actually being projected DOES get a real date (from upcoming_game_context,
  ## built fresh every run). dplyr::arrange() sorts NA last, so that one real
  ## date sorts BEFORE the NA-dated real games it should follow -- inverting
  ## the chronological order and making prior_games_season compute as 0 for
  ## EVERY player EVERY week, which silently disabled the current/prior-season
  ## blend added in prepare_fantasy_player_features() (always fell back to
  ## ~100% prior-season weight for the live weekly projection, the one place
  ## that blend most needed to work). Patch in a game_date-only row, sourced
  ## directly from the schedule (always complete/accurate), for any 2026 game
  ## missing from both context sources -- anti_join by game_id first so this
  ## never creates a duplicate-key row for a game already present.
  if (!is.null(schedules)) {
    known_ids <- unique(game_context$game_id)
    schedule_patch <- schedules |>
      dplyr::filter(.data$season == 2026, !.data$game_id %in% known_ids) |>
      dplyr::transmute(
        .data$game_id, .data$season, .data$week,
        game_date = as.Date(.data$gameday)
      )
    if (nrow(schedule_patch)) game_context <- dplyr::bind_rows(game_context, schedule_patch)
  }

  prepared <- prepare_fantasy_player_features(
    dplyr::bind_rows(player_stats, synthetic),
    game_context
  ) |>
    dplyr::filter(
      .data$season == 2026,
      .data$game_id %in% upcoming_game_context$game_id
    )

  prepared |>
    dplyr::mutate(
      recent_opportunity = dplyr::coalesce(.data$opportunity_r5, 0),
      draft_priority = dplyr::if_else(
        .data$prior_games == 0 & is.finite(.data$draft_number),
        pmax(0, 300 - .data$draft_number) / 100,
        0
      ),
      role_score = .data$recent_opportunity + .data$draft_priority
    ) |>
    dplyr::arrange(
      .data$team, .data$position,
      dplyr::desc(.data$role_score),
      .data$draft_number
    ) |>
    dplyr::group_by(.data$game_id, .data$team, .data$position) |>
    dplyr::mutate(preseason_role_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::filter(
      (.data$position == "QB" & .data$preseason_role_rank <= 1) |
        (.data$position == "RB" & .data$preseason_role_rank <= 4) |
        (.data$position == "WR" & .data$preseason_role_rank <= 5) |
        (.data$position == "TE" & .data$preseason_role_rank <= 3)
    )
}

add_fantasy_opponent_features <- function(players) {
  allowed <- players |>
    dplyr::group_by(
      .data$game_id, .data$game_date,
      defense = .data$opponent_team,
      .data$position
    ) |>
    dplyr::summarise(
      allowed_attempts = sum(.data$attempts, na.rm = TRUE),
      allowed_pass_yards = sum(.data$passing_yards, na.rm = TRUE),
      allowed_pass_tds = sum(.data$passing_tds, na.rm = TRUE),
      allowed_interceptions = sum(.data$passing_interceptions, na.rm = TRUE),
      allowed_carries = sum(.data$carries, na.rm = TRUE),
      allowed_rush_yards = sum(.data$rushing_yards, na.rm = TRUE),
      allowed_rush_tds = sum(.data$rushing_tds, na.rm = TRUE),
      allowed_targets = sum(.data$targets, na.rm = TRUE),
      allowed_receptions = sum(.data$receptions, na.rm = TRUE),
      allowed_rec_yards = sum(.data$receiving_yards, na.rm = TRUE),
      allowed_rec_tds = sum(.data$receiving_tds, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      .data$defense, .data$position, .data$game_date, .data$game_id
    ) |>
    dplyr::group_by(.data$defense, .data$position) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("allowed_"),
        ~ fantasy_lagged_roll(.x, 5L, "mean"),
        .names = "{.col}_r5"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      "game_id", "defense", "position", dplyr::ends_with("_r5")
    )

  players |>
    dplyr::left_join(
      allowed,
      by = c(
        "game_id",
        "opponent_team" = "defense",
        "position"
      )
    )
}

## Red-zone / expected-TD defense, joined from data/processed/td_redzone_features.rds
## (built by scripts/73). These already existed and were fully validated, but
## were wired ONLY into the separate anytime-TD classifier - never into the
## fantasy TD regressions that most obviously want them. That mattered: as of
## the pre-change baseline, rushing_tds (-0.019 MAE vs the naive rolling
## average) and receiving_tds (-0.015) were the two targets actually LOSING to
## a trailing-5-game mean. "Faces a defense that defends the end zone well" is
## exactly the signal they were missing.
##
## Why this doesn't just call roll_td_redzone_defense(): that function joins the
## defense's rolled value on (season, week, opponent_team), and rows only exist
## in def_game for games that have been PLAYED. Every upcoming game - the only
## kind we actually project - has no row, so it would take NA and get median-
## imputed. Here the defense-game universe is taken from the player frame
## itself (which includes the synthetic future rows), the raw per-game values
## are attached to it, and the lagged roll then carries real trailing history
## onto future rows. Same aggregate-then-roll shape as
## add_fantasy_opponent_features() above, for the same reason: one row per
## defense-game, so a defense with more pass-catchers doesn't get weighted more.
add_fantasy_redzone_defense <- function(
  players,
  path = "data/processed/td_redzone_features.rds",
  min_match_rate = 0.90
) {
  if (!file.exists(path)) {
    stop("Missing ", path, " - run scripts/73_td_redzone_features.R first.",
         call. = FALSE)
  }
  rz <- readRDS(path)
  measures <- c(
    "def_rz10_carries_allowed", "def_rz10_targets_allowed",
    "def_rz_td_conv_allowed", "def_xtd_allowed"
  )

  ## normalize_team() on the artifact side: raw pbp says "LA", this frame says
  ## "LAR". scripts/73 now normalizes at build time, so this is belt-and-braces
  ## for a stale cache rather than the primary fix.
  def_raw <- rz$def_game |>
    dplyr::mutate(defteam = normalize_team(.data$defteam)) |>
    dplyr::select(
      "season", "week", defense = "defteam", dplyr::all_of(measures)
    ) |>
    dplyr::distinct(.data$season, .data$week, .data$defense, .keep_all = TRUE)

  ## One row per defense-game, drawn from the player frame so that upcoming
  ## games are present in the sequence and can receive rolled history.
  def_games <- players |>
    dplyr::distinct(
      .data$game_id, .data$season, .data$week,
      defense = .data$opponent_team
    ) |>
    dplyr::filter(!is.na(.data$defense))

  n_def_games <- nrow(def_games)
  rolled <- def_games |>
    dplyr::left_join(def_raw, by = c("season", "week", "defense")) |>
    dplyr::arrange(.data$defense, .data$season, .data$week, .data$game_id) |>
    dplyr::group_by(.data$defense) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(measures),
        ~ fantasy_lagged_roll(.x, 5L, "mean"),
        .names = "{.col}_r5"
      )
    ) |>
    dplyr::ungroup()

  if (nrow(rolled) != n_def_games) {
    stop("Redzone defense roll changed defense-game count: ", n_def_games,
         " -> ", nrow(rolled), ". Duplicate (season, week, defteam) in def_game.",
         call. = FALSE)
  }

  ## Match-rate guard on the RAW attach, before rolling hides it. A team-code
  ## drift or a missing season shows up as columns that exist and are entirely
  ## NA, which median imputation then makes invisible - the precise failure
  ## this feature set was already suffering from silently.
  covered <- rolled |>
    dplyr::filter(
      .data$season >= min(def_raw$season, na.rm = TRUE),
      .data$season <= max(def_raw$season, na.rm = TRUE)
    )
  if (nrow(covered) > 0) {
    match_rate <- mean(!is.na(covered$def_xtd_allowed))
    message(sprintf(
      "Redzone defense: %.1f%% of %s defense-games in covered seasons matched.",
      100 * match_rate, format(nrow(covered), big.mark = ",")
    ))
    if (match_rate < min_match_rate) {
      stop("Redzone defense match rate ", round(100 * match_rate, 1),
           "% below threshold ", round(100 * min_match_rate),
           "%. Likely a team-code mismatch (pbp LA/OAK/SD vs normalize_team ",
           "LAR/LV/LAC) or def_game missing seasons.", call. = FALSE)
    }
  }

  n_before <- nrow(players)
  out <- players |>
    dplyr::left_join(
      rolled |> dplyr::select("game_id", "defense", dplyr::ends_with("_r5")),
      by = c("game_id", "opponent_team" = "defense")
    )
  if (nrow(out) != n_before) {
    stop("Redzone defense join changed row count: ", n_before, " -> ",
         nrow(out), ".", call. = FALSE)
  }
  out
}

## Position x play-type defensive efficiency, from data/processed/
## nfl_matchup_features.rds (scripts/79). Three things happen here that the
## existing raw-count opponent block does not do:
##
##   1. EFFICIENCY, not volume. EPA and success rate allowed, so "good defense"
##      is separable from "faced few plays."
##   2. NORMALIZED to the league. A rolled EPA-allowed of -0.05 means nothing
##      on its own; as a z-score against the other 31 defenses that same week
##      it means "top-5 run defense," which is the form the user's own example
##      ("a standard run team against the best run defense") requires.
##   3. EXPLICIT INTERACTIONS. Trees can in principle discover volume x
##      matchup themselves, but with ~6k training rows per target and 100+
##      features, handing them the product directly is the difference between
##      a learnable signal and one lost in the split search.
##
## Sign convention, kept consistent so the interactions are interpretable:
## def_*_epa_allowed is HIGHER when the defense is WORSE, so the z-scores are
## "defensive weakness" and a positive interaction always means a good spot.
matchup_measure_names <- function() {
  c("def_rush_epa_allowed", "def_pass_epa_allowed",
    "def_rush_success_allowed", "def_pass_success_allowed",
    "def_rush_plays_allowed", "def_pass_plays_allowed",
    "def_rush_rz_epa_allowed", "def_pass_rz_epa_allowed")
}

matchup_interaction_names <- function() {
  c("matchup_rush_volume_x_def", "matchup_target_volume_x_def",
    "matchup_rz_target_x_def", "matchup_rush_share_x_def")
}

add_fantasy_matchup_features <- function(
  players,
  path = "data/processed/nfl_matchup_features.rds",
  min_match_rate = 0.90
) {
  if (!file.exists(path)) {
    stop("Missing ", path, " - run scripts/79_build_matchup_features.R first.",
         call. = FALSE)
  }
  mf <- readRDS(path)
  measures <- intersect(matchup_measure_names(), names(mf$def_pos_game))

  raw <- mf$def_pos_game |>
    dplyr::mutate(defense = normalize_team(.data$defteam)) |>
    dplyr::select(
      "season", "week", "defense", "position", dplyr::all_of(measures)
    ) |>
    dplyr::distinct(
      .data$season, .data$week, .data$defense, .data$position, .keep_all = TRUE
    )

  ## Universe from the player frame, so upcoming games are in the sequence and
  ## can receive rolled history (a future game has no pbp row of its own).
  def_games <- players |>
    dplyr::distinct(
      .data$game_id, .data$season, .data$week,
      defense = .data$opponent_team, .data$position
    ) |>
    dplyr::filter(!is.na(.data$defense), !is.na(.data$position))

  n_def_games <- nrow(def_games)
  rolled <- def_games |>
    dplyr::left_join(raw, by = c("season", "week", "defense", "position")) |>
    dplyr::arrange(
      .data$defense, .data$position, .data$season, .data$week, .data$game_id
    ) |>
    dplyr::group_by(.data$defense, .data$position) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(measures),
        ~ fantasy_lagged_roll(.x, 5L, "mean"),
        .names = "{.col}_r5"
      )
    ) |>
    dplyr::ungroup()

  if (nrow(rolled) != n_def_games) {
    stop("Matchup roll changed defense-game count: ", n_def_games, " -> ",
         nrow(rolled), ". Duplicate keys in def_pos_game.", call. = FALSE)
  }

  covered <- rolled |>
    dplyr::filter(
      .data$season >= min(raw$season, na.rm = TRUE),
      .data$season <= max(raw$season, na.rm = TRUE)
    )
  if (nrow(covered) > 0) {
    ## Test whether the ROW matched, not whether a particular measure is
    ## populated. plays_allowed is zero-filled at build time, so it is NA if
    ## and only if the left_join found nothing - whereas the epa columns are
    ## legitimately NA for structural reasons (a QB is essentially never a
    ## target, so def_pass_epa_allowed is empty for every QB row, and QBs are
    ## a quarter of this universe: checking that column reported a bogus 75%).
    match_rate <- mean(!is.na(covered$def_rush_plays_allowed))
    message(sprintf(
      "Matchup features: %.1f%% of %s (defense, game, position) rows matched.",
      100 * match_rate, format(nrow(covered), big.mark = ",")
    ))
    if (match_rate < min_match_rate) {
      stop("Matchup match rate ", round(100 * match_rate, 1),
           "% below threshold ", round(100 * min_match_rate),
           "%. Likely a team-code or position-convention mismatch.",
           call. = FALSE)
    }
  }

  ## League normalization, within (season, week, position): compare a defense
  ## only against the other defenses facing that same position at that same
  ## point in time. Using the rolled (already lagged) value keeps this
  ## leak-free - the z-score is computed from information available before
  ## kickoff, not from the week's outcomes.
  rolled_cols <- paste0(measures, "_r5")
  rolled <- rolled |>
    dplyr::group_by(.data$season, .data$week, .data$position) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(rolled_cols),
        ~ {
          s <- stats::sd(.x, na.rm = TRUE)
          if (!is.finite(s) || s == 0) {
            rep(0, length(.x))
          } else {
            (.x - mean(.x, na.rm = TRUE)) / s
          }
        },
        .names = "{.col}_z"
      )
    ) |>
    dplyr::ungroup()

  n_before <- nrow(players)
  out <- players |>
    dplyr::left_join(
      rolled |>
        dplyr::select(
          "game_id", "defense", "position",
          dplyr::all_of(rolled_cols), dplyr::ends_with("_z")
        ),
      by = c("game_id", "opponent_team" = "defense", "position")
    )
  if (nrow(out) != n_before) {
    stop("Matchup join changed row count: ", n_before, " -> ", nrow(out), ".",
         call. = FALSE)
  }

  ## The four interactions. Each pairs a player's own recent role with the
  ## specific dimension of the defense that role runs into.
  zz <- function(x) dplyr::coalesce(x, 0)
  out |>
    dplyr::mutate(
      ## "a standard run team against the best run defense" - the literal case
      matchup_rush_volume_x_def =
        zz(.data$carries_r5) * zz(.data$def_rush_epa_allowed_r5_z),
      matchup_rush_share_x_def =
        zz(.data$carries_r5) * zz(.data$def_rush_success_allowed_r5_z),
      ## pass-catcher volume into pass-defense efficiency
      matchup_target_volume_x_def =
        zz(.data$targets_r5) * zz(.data$def_pass_epa_allowed_r5_z),
      ## "a defense that defends the end zone well" - red-zone efficiency
      ## against the share of the offense this player actually commands
      matchup_rz_target_x_def =
        zz(.data$target_share_r5) * zz(.data$def_pass_rz_epa_allowed_r5_z)
    )
}

fantasy_model_feature_names <- function(data) {
  rolling <- names(data)[stringr::str_detect(
    names(data),
    "(_r3|_r5|_r8|_sd5)$"
  )]
  ## The _z normalized matchup columns, the interaction terms, and the role
  ## trend deltas do not end in a rolling suffix, so they are listed explicitly
  ## rather than swept up by the regex above.
  matchup <- c(
    paste0(matchup_measure_names(), "_r5_z"),
    matchup_interaction_names()
  )
  trends <- names(data)[stringr::str_detect(names(data), "_trend$")]
  context <- c(
    "week", "prior_games", "total_line", "team_spread",
    "implied_team_total", "is_home",
    "temperature", "wind_speed", "precip_probability",
    "is_dome", "grass_flag", "turf_flag", "rain_flag", "snow_flag",
    "cold_index", "high_wind_index",
    "position_qb", "position_rb", "position_wr", "position_te"
  )
  intersect(unique(c(rolling, matchup, trends, context)), names(data))
}

fantasy_target_candidates <- function(data, family, deployment = FALSE) {
  recent_pass <- dplyr::coalesce(data$attempts_r3, 0)
  recent_rush <- dplyr::coalesce(data$carries_r3, 0)
  recent_receive <- dplyr::coalesce(data$targets_r3, 0)
  is_future <- deployment | data$season >= 2026

  keep <- switch(
    family,
    passing = data$position == "QB" & (recent_pass >= 3 | is_future),
    rushing = data$position %in% c("QB", "RB", "WR", "TE") &
      (recent_rush >= 0.5 | is_future),
    receiving = data$position %in% c("RB", "WR", "TE") &
      (recent_receive >= 0.5 | is_future),
    all = recent_pass + recent_rush + recent_receive >= 0.5 | is_future
  )
  data[which(keep & is.finite(data$prior_games)), , drop = FALSE]
}

fantasy_matrix_pair <- function(train, test, features, medians = NULL) {
  train_x <- train[, features, drop = FALSE]
  test_x <- test[, features, drop = FALSE]
  if (is.null(medians)) {
    medians <- vapply(
      train_x,
      function(x) stats::median(as.numeric(x), na.rm = TRUE),
      numeric(1)
    )
    medians[!is.finite(medians)] <- 0
  }
  for (feature in features) {
    train_x[[feature]] <- as.numeric(train_x[[feature]])
    test_x[[feature]] <- as.numeric(test_x[[feature]])
    train_x[[feature]][!is.finite(train_x[[feature]])] <- medians[[feature]]
    test_x[[feature]][!is.finite(test_x[[feature]])] <- medians[[feature]]
  }
  list(
    train = as.matrix(train_x),
    test = as.matrix(test_x),
    medians = medians
  )
}

fit_fantasy_stat_model <- function(
  train,
  test,
  outcome,
  objective,
  seed = 20260728L
) {
  features <- fantasy_model_feature_names(train)
  matrices <- fantasy_matrix_pair(train, test, features)
  params <- list(
    objective = objective,
    eval_metric = if (objective == "count:poisson") "poisson-nloglik" else "rmse",
    learning_rate = 0.035,
    max_depth = 3,
    min_child_weight = 15,
    subsample = 0.8,
    colsample_bytree = 0.8,
    reg_lambda = 3,
    reg_alpha = 0.15,
    nthread = 1,
    verbosity = 0
  )
  if (objective == "count:poisson") params$max_delta_step <- 0.7
  # The R package ignores params$seed and takes its RNG state from R, so
  # subsample/colsample draws are only reproducible via set.seed().
  set.seed(seed)
  fit <- xgboost::xgb.train(
    params = params,
    data = xgboost::xgb.DMatrix(
      matrices$train,
      label = as.numeric(train[[outcome]])
    ),
    nrounds = 180
  )
  prediction <- if (nrow(test)) {
    pmax(0, as.numeric(stats::predict(fit, matrices$test)))
  } else {
    numeric()
  }
  list(
    fit = portable_booster(fit),
    prediction = prediction,
    features = features,
    medians = matrices$medians
  )
}

fantasy_regression_metrics <- function(data) {
  if (!nrow(data)) return(tibble::tibble())
  truth <- data$actual
  prediction <- data$prediction
  baseline <- data$baseline
  denominator <- sum((truth - mean(truth))^2)
  tibble::tibble(
    observations = nrow(data),
    actual_mean = mean(truth),
    predicted_mean = mean(prediction),
    mae = mean(abs(prediction - truth)),
    rmse = sqrt(mean((prediction - truth)^2)),
    r_squared = dplyr::if_else(
      denominator > 0,
      1 - sum((prediction - truth)^2) / denominator,
      NA_real_
    ),
    bias = mean(prediction - truth),
    baseline_mae = mean(abs(baseline - truth)),
    mae_improvement = .data$baseline_mae - .data$mae
  )
}

walk_forward_fantasy_models <- function(
  features,
  test_seasons = 2023:2025,
  seed = 20260728L
) {
  specifications <- fantasy_target_specifications()
  predictions <- list()
  artifacts <- list()
  metrics <- list()

  for (target_name in names(specifications)) {
    specification <- specifications[[target_name]]
    target_data <- fantasy_target_candidates(
      features,
      specification$family
    )
    target_predictions <- list()
    target_artifacts <- list()
    for (test_season in test_seasons) {
      train <- target_data |>
        dplyr::filter(.data$season < test_season, .data$prior_games >= 1)
      test <- target_data |>
        dplyr::filter(.data$season == test_season, .data$prior_games >= 1)
      if (!nrow(train) || !nrow(test)) next
      fitted <- fit_fantasy_stat_model(
        train,
        test,
        specification$outcome,
        specification$objective,
        seed + test_season + match(target_name, names(specifications))
      )
      baseline_column <- paste0(specification$outcome, "_r5")
      baseline <- dplyr::coalesce(
        as.numeric(test[[baseline_column]]),
        mean(train[[specification$outcome]], na.rm = TRUE)
      )
      board <- test |>
        dplyr::transmute(
          .data$game_id,
          .data$season,
          .data$week,
          .data$game_date,
          .data$player_id,
          .data$player_display_name,
          .data$position,
          .data$team,
          .data$opponent_team,
          target = target_name,
          actual = as.numeric(.data[[specification$outcome]]),
          prediction = fitted$prediction,
          baseline = baseline
        )
      target_predictions[[as.character(test_season)]] <- board
      target_artifacts[[as.character(test_season)]] <- fitted
      metrics[[paste(target_name, test_season)]] <-
        fantasy_regression_metrics(board) |>
        dplyr::mutate(
          target = target_name,
          season = test_season,
          .before = 1
        )
    }
    predictions[[target_name]] <- dplyr::bind_rows(target_predictions)
    artifacts[[target_name]] <- target_artifacts
  }

  list(
    predictions = dplyr::bind_rows(predictions),
    metrics = dplyr::bind_rows(metrics),
    artifacts = artifacts
  )
}

fit_fantasy_deployment_models <- function(features, seed = 20260728L) {
  specifications <- fantasy_target_specifications()
  purrr::imap(specifications, function(specification, target_name) {
    train <- fantasy_target_candidates(features, specification$family) |>
      dplyr::filter(.data$season <= 2025, .data$prior_games >= 1)
    fitted <- fit_fantasy_stat_model(
      train,
      train[0, , drop = FALSE],
      specification$outcome,
      specification$objective,
      seed + match(target_name, names(specifications))
    )
    fitted$prediction <- NULL
    fitted$outcome <- specification$outcome
    fitted$outcome_mean <- mean(train[[specification$outcome]], na.rm = TRUE)
    fitted$family <- specification$family
    fitted$label <- specification$label
    fitted
  })
}

fantasy_deployment_blend_weights <- function(walk_forward_predictions) {
  walk_forward_predictions |>
    dplyr::group_by(.data$target) |>
    dplyr::summarise(
      numerator = sum(
        (.data$prediction - .data$baseline) *
          (.data$actual - .data$baseline)
      ),
      denominator = sum((.data$prediction - .data$baseline)^2),
      model_weight = dplyr::if_else(
        .data$denominator > 0,
        pmin(pmax(.data$numerator / .data$denominator, 0), 1),
        1
      ),
      .groups = "drop"
    ) |>
    dplyr::select("target", "model_weight")
}

predict_fantasy_deployment <- function(
  models,
  future_features,
  blend_weights = NULL
) {
  predictions <- purrr::imap_dfr(models, function(model, target_name) {
    candidates <- fantasy_target_candidates(
      future_features,
      model$family,
      deployment = TRUE
    )
    if (!nrow(candidates)) return(tibble::tibble())
    matrices <- fantasy_matrix_pair(
      candidates[0, , drop = FALSE],
      candidates,
      model$features,
      model$medians
    )
    model_prediction <- pmax(
      0,
      as.numeric(stats::predict(
        revive_booster(model$fit, target_name),
        matrices$test
      ))
    )
    baseline_column <- paste0(model$outcome, "_r5")
    baseline_prediction <- dplyr::coalesce(
      as.numeric(candidates[[baseline_column]]),
      model$outcome_mean
    )
    candidates |>
      dplyr::transmute(
        .data$game_id,
        .data$season,
        .data$week,
        .data$game_date,
        .data$player_id,
        player = .data$player_display_name,
        .data$position,
        .data$prior_games,
        .data$preseason_role_rank,
        .data$team,
        .data$opponent_team,
        .data$total_line,
        .data$team_spread,
        .data$implied_team_total,
        .data$temperature,
        .data$wind_speed,
        .data$precip_probability,
        .data$is_dome,
        weather_status = dplyr::coalesce(
          as.character(.data$weather_status),
          "PENDING"
        ),
        target = target_name,
        model_prediction = model_prediction,
        baseline_prediction = baseline_prediction
      )
  })
  if (is.null(blend_weights)) {
    predictions$model_weight <- 1
  } else {
    predictions <- predictions |>
      dplyr::left_join(blend_weights, by = "target") |>
      dplyr::mutate(model_weight = dplyr::coalesce(.data$model_weight, 1))
  }
  predictions |>
    dplyr::mutate(
      prediction = pmax(
        0,
        .data$model_weight * .data$model_prediction +
          (1 - .data$model_weight) * .data$baseline_prediction
      )
    )
}

fantasy_residual_intervals <- function(walk_forward_predictions) {
  walk_forward_predictions |>
    dplyr::mutate(residual = .data$actual - .data$prediction) |>
    dplyr::group_by(.data$target) |>
    dplyr::summarise(
      residual_p10 = stats::quantile(.data$residual, 0.10, na.rm = TRUE),
      residual_p90 = stats::quantile(.data$residual, 0.90, na.rm = TRUE),
      .groups = "drop"
    )
}

build_fantasy_projection_board <- function(
  long_predictions,
  residual_intervals,
  historical_features
) {
  projected <- long_predictions |>
    dplyr::left_join(residual_intervals, by = "target") |>
    dplyr::mutate(
      projection_low = pmax(0, .data$prediction + .data$residual_p10),
      projection_high = pmax(0, .data$prediction + .data$residual_p90)
    )

  id_columns <- c(
    "game_id", "season", "week", "game_date", "player_id", "player",
    "position", "prior_games", "preseason_role_rank",
    "team", "opponent_team", "total_line", "team_spread",
    "implied_team_total", "temperature", "wind_speed",
    "precip_probability", "is_dome", "weather_status"
  )
  point <- projected |>
    dplyr::select(dplyr::all_of(id_columns), "target", "prediction") |>
    tidyr::pivot_wider(
      names_from = "target",
      values_from = "prediction",
      names_prefix = "projected_",
      values_fill = 0
    )
  low <- projected |>
    dplyr::select("game_id", "player_id", "target", "projection_low") |>
    tidyr::pivot_wider(
      names_from = "target",
      values_from = "projection_low",
      names_prefix = "low_",
      values_fill = 0
    )
  high <- projected |>
    dplyr::select("game_id", "player_id", "target", "projection_high") |>
    tidyr::pivot_wider(
      names_from = "target",
      values_from = "projection_high",
      names_prefix = "high_",
      values_fill = 0
    )

  two_point_rates <- historical_features |>
    dplyr::filter(.data$season >= 2023, .data$season <= 2025) |>
    dplyr::group_by(.data$position) |>
    dplyr::summarise(
      projected_two_point_conversions =
        (sum(.data$two_point_conversions) + 0.5) /
        (dplyr::n() + 50),
      .groups = "drop"
    )

  point |>
    dplyr::left_join(low, by = c("game_id", "player_id")) |>
    dplyr::left_join(high, by = c("game_id", "player_id")) |>
    dplyr::left_join(two_point_rates, by = "position") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with(c("projected_", "low_", "high_")),
        ~ dplyr::coalesce(.x, 0)
      ),
      projected_ppr =
        0.04 * .data$projected_passing_yards +
        4 * .data$projected_passing_tds -
        2 * .data$projected_interceptions +
        0.1 * .data$projected_rushing_yards +
        6 * .data$projected_rushing_tds +
        .data$projected_receptions +
        0.1 * .data$projected_receiving_yards +
        6 * .data$projected_receiving_tds -
        2 * .data$projected_fumbles_lost +
        2 * .data$projected_two_point_conversions,
      ppr_low = pmax(
        0,
        0.04 * .data$low_passing_yards +
          4 * .data$low_passing_tds -
          2 * .data$high_interceptions +
          0.1 * .data$low_rushing_yards +
          6 * .data$low_rushing_tds +
          .data$low_receptions +
          0.1 * .data$low_receiving_yards +
          6 * .data$low_receiving_tds -
          2 * .data$high_fumbles_lost
      ),
      ppr_high =
        0.04 * .data$high_passing_yards +
        4 * .data$high_passing_tds -
        2 * .data$low_interceptions +
        0.1 * .data$high_rushing_yards +
        6 * .data$high_rushing_tds +
        .data$high_receptions +
        0.1 * .data$high_receiving_yards +
        6 * .data$high_receiving_tds -
        2 * .data$low_fumbles_lost +
        2 * .data$projected_two_point_conversions,
      projection_status = dplyr::case_when(
        .data$weather_status == "PENDING" ~ "PRELIMINARY_WEATHER_PENDING",
        TRUE ~ "PRELIMINARY_DEPTH_CHART_PENDING"
      ),
      role_status = dplyr::case_when(
        .data$prior_games == 0 ~ "ROOKIE_PRIOR",
        TRUE ~ "PRESEASON_ROLE_ESTIMATE"
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$projected_ppr))
}
