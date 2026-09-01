# Player-level additions: pregame injury designations and lagged
# opportunity/role measures.
#
# Two different kinds of input are built here and they should not be confused:
#
#   Injury columns are current-week and pregame. They are the only genuinely
#   new information relative to what the closing line already knows.
#
#   Opportunity and snap columns are lagged summaries of past games. They do
#   not add information the market lacks; they make the projection less noisy.
#
# Everything rolling is built with fantasy_lagged_roll(), which lags before it
# rolls, so no row sees its own game.

player_injury_context <- function(injuries, player_teams) {
  base <- injuries |>
    dplyr::filter(.data$game_type == "REG") |>
    dplyr::mutate(
      team = normalize_team(.data$team),
      season = as.integer(.data$season),
      week = as.integer(.data$week),
      group = injury_position_group(.data$position),
      weight = injury_availability_weight(
        .data$report_status, .data$practice_status
      )
    ) |>
    dplyr::filter(!is.na(.data$gsis_id))

  self <- base |>
    dplyr::group_by(.data$season, .data$week, player_id = .data$gsis_id) |>
    dplyr::summarise(inj_self_weight = max(.data$weight), .groups = "drop")

  team_totals <- base |>
    dplyr::group_by(.data$season, .data$week, .data$team) |>
    dplyr::summarise(
      team_qb_out = as.numeric(any(
        .data$group == "qb" &
          tolower(trimws(as.character(.data$report_status))) %in% "out"
      )),
      team_skill_burden = sum(.data$weight[.data$group == "skill"]),
      team_db_burden = sum(.data$weight[.data$group == "db"]),
      team_dl_burden = sum(.data$weight[.data$group == "dl"]),
      .groups = "drop"
    )

  # Own contribution is removed so a player's own injury does not inflate the
  # "competition missing" signal that is meant to describe teammates.
  own_skill <- base |>
    dplyr::filter(.data$group == "skill") |>
    dplyr::group_by(.data$season, .data$week, player_id = .data$gsis_id) |>
    dplyr::summarise(own_weight = max(.data$weight), .groups = "drop")

  player_teams |>
    dplyr::left_join(self, by = c("season", "week", "player_id")) |>
    dplyr::left_join(
      team_totals |>
        dplyr::rename(
          inj_team_qb_out = "team_qb_out",
          inj_team_skill_burden = "team_skill_burden"
        ) |>
        dplyr::select(
          "season", "week", "team", "inj_team_qb_out", "inj_team_skill_burden"
        ),
      by = c("season", "week", "team")
    ) |>
    dplyr::left_join(
      team_totals |>
        dplyr::select(
          "season", "week", opponent_team = "team",
          inj_opp_db_burden = "team_db_burden",
          inj_opp_dl_burden = "team_dl_burden"
        ),
      by = c("season", "week", "opponent_team")
    ) |>
    dplyr::left_join(own_skill, by = c("season", "week", "player_id")) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(
          "inj_self_weight", "inj_team_qb_out", "inj_team_skill_burden",
          "inj_opp_db_burden", "inj_opp_dl_burden", "own_weight"
        )),
        ~ dplyr::coalesce(.x, 0)
      ),
      inj_team_skill_other = pmax(
        0, .data$inj_team_skill_burden - .data$own_weight
      )
    ) |>
    dplyr::select(
      "season", "week", "player_id",
      "inj_self_weight", "inj_team_qb_out", "inj_team_skill_other",
      "inj_opp_db_burden", "inj_opp_dl_burden"
    ) |>
    dplyr::distinct(.data$season, .data$week, .data$player_id,
                    .keep_all = TRUE)
}

opportunity_measures <- function() {
  c(
    "receptions_exp", "rec_yards_gained_exp", "rush_yards_gained_exp",
    "rec_touchdown_exp", "rush_touchdown_exp", "pass_yards_gained_exp",
    "pass_touchdown_exp", "total_fantasy_points_exp",
    "receptions_diff", "rec_yards_gained_diff", "rush_yards_gained_diff"
  )
}

build_opportunity_features <- function(ff_opportunity, windows = c(3L, 5L)) {
  measures <- intersect(opportunity_measures(), names(ff_opportunity))
  base <- ff_opportunity |>
    dplyr::transmute(
      season = as.integer(.data$season),
      week = as.integer(.data$week),
      player_id = as.character(.data$player_id),
      dplyr::across(dplyr::all_of(measures), as.numeric)
    ) |>
    dplyr::filter(!is.na(.data$player_id)) |>
    dplyr::group_by(.data$season, .data$week, .data$player_id) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(measures), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$player_id, .data$season, .data$week)

  rolled <- base |> dplyr::group_by(.data$player_id)
  for (w in windows) {
    rolled <- rolled |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(measures),
          ~ fantasy_lagged_roll(.x, w, "mean"),
          .names = paste0("opp_{.col}_r", w)
        )
      )
  }
  rolled |>
    dplyr::ungroup() |>
    dplyr::select("season", "week", "player_id", dplyr::starts_with("opp_"))
}

build_snap_features <- function(snap_counts, crosswalk, windows = c(3L, 5L)) {
  base <- snap_counts |>
    dplyr::filter(.data$game_type == "REG") |>
    dplyr::inner_join(crosswalk, by = c("pfr_player_id" = "pfr_id")) |>
    dplyr::transmute(
      season = as.integer(.data$season),
      week = as.integer(.data$week),
      player_id = as.character(.data$gsis_id),
      snap_pct = as.numeric(.data$offense_pct)
    ) |>
    dplyr::filter(!is.na(.data$player_id)) |>
    dplyr::group_by(.data$season, .data$week, .data$player_id) |>
    dplyr::summarise(snap_pct = max(.data$snap_pct), .groups = "drop") |>
    dplyr::arrange(.data$player_id, .data$season, .data$week)

  rolled <- base |> dplyr::group_by(.data$player_id)
  for (w in windows) {
    rolled <- rolled |>
      dplyr::mutate(
        "snap_pct_r{w}" := fantasy_lagged_roll(.data$snap_pct, w, "mean")
      )
  }
  rolled |>
    dplyr::ungroup() |>
    dplyr::select("season", "week", "player_id", dplyr::starts_with("snap_pct_r"))
}

# Current-week injury columns are not rolling, so they must be named
# explicitly for fantasy_model_feature_names() to pick them up.
player_injury_feature_columns <- function() {
  c(
    "inj_self_weight", "inj_team_qb_out", "inj_team_skill_other",
    "inj_opp_db_burden", "inj_opp_dl_burden"
  )
}

augment_player_features <- function(features, injuries, ff_opportunity,
                                    snap_counts = NULL, crosswalk = NULL) {
  player_teams <- features |>
    dplyr::transmute(
      season = as.integer(.data$season),
      week = as.integer(.data$week),
      player_id = as.character(.data$player_id),
      team = normalize_team(.data$team),
      opponent_team = normalize_team(.data$opponent_team)
    ) |>
    dplyr::distinct()

  out <- features |>
    dplyr::left_join(
      player_injury_context(injuries, player_teams),
      by = c("season", "week", "player_id")
    ) |>
    dplyr::left_join(
      build_opportunity_features(ff_opportunity),
      by = c("season", "week", "player_id")
    )

  if (!is.null(snap_counts) && !is.null(crosswalk)) {
    out <- out |>
      dplyr::left_join(
        build_snap_features(snap_counts, crosswalk),
        by = c("season", "week", "player_id")
      )
  }

  out |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(player_injury_feature_columns()),
        ~ dplyr::coalesce(.x, 0)
      )
    )
}
