# Season-long PPR draft model.
#
# The weekly model projects one game from rolling in-season form and has no
# season-long mode, which is why it tested as no better than last year's points
# per game for draft purposes. This is a separate model fitted to the thing a
# draft actually cares about.
#
# Two targets, modelled separately and multiplied:
#
#   points per game  - how good the player is when he plays
#   games played     - whether he is available
#
# Season TOTAL is what wins a league, and the earlier analysis showed
# availability is where a per-game projection loses most of its value
# (correlation with season PPG 0.78, with season total 0.71, with games 0.26).
# Modelling games explicitly is the point of the exercise.
#
# Every feature is knowable before a ball is snapped: prior-season production,
# career history, draft capital and experience.

# Team offensive environment, one row per team-season.
#
# A player's output is bounded by the offense he plays in: a receiver on a team
# that threw 380 times has a smaller pie than one on a team that threw 620. The
# model previously had no team features at all, which is why a player changing
# teams was completely invisible to it.
#
# These are joined LAGGED - season S uses the team's S-1 output - and joined on
# the team the player will play for in S. Both halves are knowable before the
# season starts: you know last year's team stats and you know the current
# roster.
draft_team_offense <- function(player_stats) {
  player_stats |>
    dplyr::filter(.data$season_type == "REG") |>
    # Team codes have to be normalized on every table that takes part in a
    # team join. player_stats calls the Rams "LA"; the roster file calls them
    # "LAR". Left alone, the join silently misses and every Rams player gets
    # median-imputed team features plus a false changed-team flag.
    dplyr::mutate(team = normalize_team(.data$team)) |>
    dplyr::group_by(.data$season, .data$team) |>
    dplyr::summarise(
      tm_attempts = sum(.data$attempts, na.rm = TRUE),
      tm_pass_yards = sum(.data$passing_yards, na.rm = TRUE),
      tm_pass_tds = sum(.data$passing_tds, na.rm = TRUE),
      tm_carries = sum(.data$carries, na.rm = TRUE),
      tm_rush_yards = sum(.data$rushing_yards, na.rm = TRUE),
      tm_rush_tds = sum(.data$rushing_tds, na.rm = TRUE),
      tm_targets = sum(.data$targets, na.rm = TRUE),
      tm_ppr = sum(.data$fantasy_points_ppr, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(.data$team), .data$tm_attempts > 0)
}

# Vacated and returning opportunity, one row per team-season.
#
# Team-level offence says how big the pie is. This says how much of it is
# unclaimed. When a team loses the receiver who absorbed 140 targets, those
# targets do not evaporate - they redistribute to whoever is left, which is
# exactly the Bears/DJ Moore case: Odunze, Burden and Loveland all inherit a
# share of what walked out the door.
#
# Roster membership is inferred from who actually recorded stats for the team
# that season. That is slightly generous for the training years, because a
# mid-season signing counts as "on the roster" when a drafter in August could
# not have known. The dominant signal is offseason movement, which is knowable,
# but the historical fit is mildly optimistic and the A/B test below is the
# check on whether it still earns its place.
draft_vacated_opportunity <- function(seasons_table) {
  roster <- seasons_table |>
    dplyr::select("season", "team", "player_id", "targets", "carries",
                  "ppr_total")

  previous <- roster |>
    dplyr::transmute(
      season = .data$season + 1L, .data$team, .data$player_id,
      prev_targets = .data$targets, prev_carries = .data$carries,
      prev_ppr = .data$ppr_total
    )

  current <- roster |> dplyr::select("season", "team", "player_id")

  departed <- previous |>
    dplyr::anti_join(current, by = c("season", "team", "player_id")) |>
    dplyr::group_by(.data$season, .data$team) |>
    dplyr::summarise(
      tm_vacated_targets = sum(.data$prev_targets, na.rm = TRUE),
      tm_vacated_carries = sum(.data$prev_carries, na.rm = TRUE),
      tm_vacated_ppr = sum(.data$prev_ppr, na.rm = TRUE),
      .groups = "drop"
    )

  returning <- previous |>
    dplyr::semi_join(current, by = c("season", "team", "player_id")) |>
    dplyr::group_by(.data$season, .data$team) |>
    dplyr::summarise(
      tm_returning_targets = sum(.data$prev_targets, na.rm = TRUE),
      tm_returning_carries = sum(.data$prev_carries, na.rm = TRUE),
      .groups = "drop"
    )

  departed |>
    dplyr::full_join(returning, by = c("season", "team")) |>
    # Only the count columns get zero-filled. Sweeping everything() through
    # as.numeric() would turn the team code into NA and break the join.
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric), ~ dplyr::if_else(is.na(.x), 0, as.numeric(.x))
    )) |>
    dplyr::mutate(
      season = as.integer(.data$season),
      team = as.character(.data$team),
      vacated_target_share = .data$tm_vacated_targets /
        pmax(1, .data$tm_vacated_targets + .data$tm_returning_targets),
      vacated_carry_share = .data$tm_vacated_carries /
        pmax(1, .data$tm_vacated_carries + .data$tm_returning_carries)
    )
}

draft_vacated_feature_names <- function() {
  c(
    "tm_vacated_targets", "tm_vacated_carries", "tm_vacated_ppr",
    "tm_returning_targets", "tm_returning_carries",
    "vacated_target_share", "vacated_carry_share",
    "vacated_targets_net", "vacated_carries_net"
  )
}

draft_season_aggregates <- function(player_stats) {
  player_stats |>
    dplyr::filter(
      .data$season_type == "REG",
      .data$position %in% c("QB", "RB", "WR", "TE")
    ) |>
    dplyr::group_by(.data$season, .data$player_id) |>
    dplyr::summarise(
      player_name = dplyr::first(.data$player_display_name),
      position = dplyr::first(.data$position),
      team = normalize_team(dplyr::last(.data$team)),
      games = dplyr::n(),
      ppr_total = sum(.data$fantasy_points_ppr, na.rm = TRUE),
      ppr_ppg = mean(.data$fantasy_points_ppr, na.rm = TRUE),
      targets = sum(.data$targets, na.rm = TRUE),
      carries = sum(.data$carries, na.rm = TRUE),
      attempts = sum(.data$attempts, na.rm = TRUE),
      .groups = "drop"
    )
}

# Lagged views of the previous three seasons plus career totals to date.
draft_build_features <- function(seasons_table, draft_picks,
                                 team_offense = NULL, vacated = NULL) {
  lagged <- function(k) {
    seasons_table |>
      dplyr::transmute(
        season = .data$season + k,
        .data$player_id,
        "ppg_{k}" := .data$ppr_ppg,
        "games_{k}" := .data$games,
        "total_{k}" := .data$ppr_total,
        "targets_{k}" := .data$targets,
        "carries_{k}" := .data$carries,
        "attempts_{k}" := .data$attempts
      )
  }

  career <- seasons_table |>
    dplyr::arrange(.data$player_id, .data$season) |>
    dplyr::group_by(.data$player_id) |>
    dplyr::mutate(
      career_seasons = dplyr::row_number() - 1L,
      career_games = dplyr::lag(cumsum(.data$games), default = 0L),
      career_ppg = dplyr::lag(
        cumsum(.data$ppr_total) / pmax(1, cumsum(.data$games))
      ),
      best_ppg = dplyr::lag(cummax(.data$ppr_ppg))
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      "season", "player_id", "career_seasons", "career_games", "career_ppg",
      "best_ppg"
    )

  capital <- draft_picks |>
    dplyr::filter(!is.na(.data$gsis_id)) |>
    dplyr::transmute(
      player_id = .data$gsis_id,
      draft_season = as.integer(.data$season),
      draft_round = as.integer(.data$round),
      draft_pick = as.integer(.data$pick)
    ) |>
    dplyr::distinct(.data$player_id, .keep_all = TRUE)

  # The team the player lined up for in the previous season, so a move can be
  # detected and the two offensive environments compared.
  prior_team <- seasons_table |>
    dplyr::transmute(
      season = .data$season + 1L, .data$player_id, prev_team = .data$team
    )

  team_lagged <- if (is.null(team_offense)) NULL else {
    team_offense |> dplyr::mutate(season = .data$season + 1L)
  }
  team_cols <- if (is.null(team_lagged)) character(0) else {
    setdiff(names(team_lagged), c("season", "team"))
  }

  out <- seasons_table |>
    dplyr::select("season", "player_id", "player_name", "position", "team",
                  "games", "ppr_total", "ppr_ppg") |>
    dplyr::left_join(prior_team, by = c("season", "player_id")) |>
    dplyr::left_join(lagged(1), by = c("season", "player_id")) |>
    dplyr::left_join(lagged(2), by = c("season", "player_id")) |>
    dplyr::left_join(lagged(3), by = c("season", "player_id")) |>
    dplyr::left_join(career, by = c("season", "player_id")) |>
    dplyr::left_join(capital, by = "player_id") |>
    dplyr::mutate(
      # Undrafted players get a pick number past the end of the draft rather
      # than a missing value, so the model can use "no draft capital" as
      # information instead of imputing a median round.
      draft_round = dplyr::coalesce(.data$draft_round, 8L),
      draft_pick = dplyr::coalesce(.data$draft_pick, 300L),
      experience = dplyr::if_else(
        is.na(.data$draft_season), .data$career_seasons,
        as.numeric(.data$season - .data$draft_season)
      ),
      is_rookie = as.numeric(dplyr::coalesce(.data$career_games, 0) == 0),
      position_qb = as.numeric(.data$position == "QB"),
      position_rb = as.numeric(.data$position == "RB"),
      position_wr = as.numeric(.data$position == "WR"),
      position_te = as.numeric(.data$position == "TE"),
      trend_ppg = .data$ppg_1 - .data$ppg_2,
      changed_team = as.numeric(
        !is.na(.data$prev_team) & .data$team != .data$prev_team
      )
    )

  if (is.null(team_lagged)) return(out)

  # New team's prior-season offence, then the same for the team he left, so the
  # model can learn what moving from a 620-attempt passing offence to a
  # 380-attempt one does to a receiver.
  out <- out |>
    dplyr::left_join(team_lagged, by = c("season", "team")) |>
    dplyr::left_join(
      team_lagged |>
        dplyr::rename_with(~ paste0("prev_", .x), dplyr::all_of(team_cols)) |>
        dplyr::rename(prev_team = "team"),
      by = c("season", "prev_team")
    ) |>
    dplyr::mutate(
      tm_ppr_delta = .data$tm_ppr - .data$prev_tm_ppr,
      tm_attempts_delta = .data$tm_attempts - .data$prev_tm_attempts,
      tm_carries_delta = .data$tm_carries - .data$prev_tm_carries,
      tm_targets_delta = .data$tm_targets - .data$prev_tm_targets
    ) |>
    dplyr::select(-dplyr::starts_with("prev_tm_"))

  if (is.null(vacated)) return(out)

  # Net of the player's own prior volume: a returning starter is not competing
  # for the targets he already had, so what matters to him is the volume freed
  # up by everyone else leaving.
  out |>
    dplyr::left_join(vacated, by = c("season", "team")) |>
    dplyr::mutate(
      vacated_targets_net = .data$tm_vacated_targets -
        dplyr::if_else(
          dplyr::coalesce(.data$changed_team, 0) == 1, 0,
          dplyr::coalesce(.data$targets_1, 0)
        ),
      vacated_carries_net = .data$tm_vacated_carries -
        dplyr::if_else(
          dplyr::coalesce(.data$changed_team, 0) == 1, 0,
          dplyr::coalesce(.data$carries_1, 0)
        )
    )
}

draft_team_feature_names <- function() {
  c(
    "tm_attempts", "tm_pass_yards", "tm_pass_tds", "tm_carries",
    "tm_rush_yards", "tm_rush_tds", "tm_targets", "tm_ppr",
    "tm_ppr_delta", "tm_attempts_delta", "tm_carries_delta",
    "tm_targets_delta", "changed_team"
  )
}

draft_feature_names <- function() {
  c(
    "ppg_1", "ppg_2", "ppg_3", "games_1", "games_2", "games_3",
    "total_1", "total_2", "total_3",
    "targets_1", "targets_2", "carries_1", "carries_2",
    "attempts_1", "attempts_2",
    "career_seasons", "career_games", "career_ppg", "best_ppg",
    "draft_round", "draft_pick", "experience", "is_rookie", "trend_ppg",
    "position_qb", "position_rb", "position_wr", "position_te"
  )
}

draft_matrix_pair <- function(train, test, features, medians = NULL) {
  build <- function(d) {
    m <- d[, features, drop = FALSE]
    for (f in features) m[[f]] <- as.numeric(m[[f]])
    m
  }
  train_x <- build(train)
  test_x <- build(test)
  if (is.null(medians)) {
    medians <- vapply(
      train_x, function(x) stats::median(x, na.rm = TRUE), numeric(1)
    )
    medians[!is.finite(medians)] <- 0
  }
  for (f in features) {
    train_x[[f]][!is.finite(train_x[[f]])] <- medians[[f]]
    test_x[[f]][!is.finite(test_x[[f]])] <- medians[[f]]
  }
  list(train = as.matrix(train_x), test = as.matrix(test_x), medians = medians)
}

fit_draft_target <- function(train, test, outcome, features, seed = 20260814L) {
  matrices <- draft_matrix_pair(train, test, features)
  set.seed(seed)
  fit <- xgboost::xgb.train(
    params = list(
      objective = "reg:squarederror", eval_metric = "rmse",
      learning_rate = 0.04, max_depth = 4, min_child_weight = 10,
      subsample = 0.8, colsample_bytree = 0.8,
      reg_lambda = 3, reg_alpha = 0.1, nthread = 1, verbosity = 0
    ),
    data = xgboost::xgb.DMatrix(
      matrices$train, label = as.numeric(train[[outcome]])
    ),
    nrounds = 300
  )
  list(
    fit = portable_booster(fit),
    features = features,
    medians = matrices$medians,
    prediction = if (nrow(test)) {
      pmax(0, as.numeric(stats::predict(fit, matrices$test)))
    } else numeric()
  )
}

walk_forward_draft_model <- function(features_table, test_seasons,
                                     features = draft_feature_names()) {
  purrr::map_dfr(test_seasons, function(test_season) {
    train <- dplyr::filter(features_table, .data$season < test_season)
    test <- dplyr::filter(features_table, .data$season == test_season)
    if (!nrow(train) || !nrow(test)) return(tibble::tibble())

    # Named to avoid colliding with the `games` column: inside mutate() a bare
    # `games` resolves to the data, not to the fitted object.
    ppg_fit <- fit_draft_target(train, test, "ppr_ppg", features)
    games_fit <- fit_draft_target(train, test, "games", features, seed = 991L)
    ppg_hat <- ppg_fit$prediction
    games_hat <- pmin(17, games_fit$prediction)

    test |>
      dplyr::select("season", "player_id", "player_name", "position", "team",
                    "games", "ppr_total", "ppr_ppg", "ppg_1", "games_1",
                    "total_1", "is_rookie") |>
      dplyr::mutate(
        projected_ppg = ppg_hat,
        projected_games = games_hat,
        projected_total = .data$projected_ppg * .data$projected_games
      )
  })
}
