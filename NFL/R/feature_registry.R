# Registered feature schemas.
#
# Every model here used to pick its own inputs by pattern: the game models took
# "all numeric columns except this blocklist", the touchdown and fantasy models
# took "everything ending _r3/_r5". That is convenient and wrong. Adding a
# column to the feature table silently changed every model that had ever been
# validated, and renaming one silently removed it, in both cases without
# changing a line of model code and without any run failing. A backtest and a
# deployment could disagree about what the model even is.
#
# So the schema is written down. The builders below still describe the features
# structurally rather than as an 83-line literal - the game table is genuinely
# symmetric across home and away, and spelling that out is more legible than a
# flat list - but the set they produce is fixed, and any drift from what the
# data actually carries is an error rather than a silent substitution.
#
# To add a feature: add it here, then re-run the validation that justifies it.
# Both steps, in that order. The assertion exists to make skipping the second
# one impossible to do by accident.

# The rolling-window stems shared by both sides of a game.
game_team_feature_stems <- function() {
  c(
    "prior_games", "rest_days",
    outer(
      c("off_plays", "off_epa", "off_success", "pass_epa", "rush_epa",
        "pass_yards_play", "rush_yards_play", "explosive_rate",
        "giveaway_rate", "sack_rate", "early_down_pass_rate",
        "def_epa", "def_success_allowed", "takeaways_rate", "pressure_rate",
        "explosive_allowed", "points_for", "points_against"),
      c("r4", "r8"), paste, sep = "_"
    ) |> as.vector() |> (\(x) x[order(sub(".*_", "", x))])(),
    "pythagorean_win_pct"
  )
}

registered_game_features <- function() {
  stems <- game_team_feature_stems()
  c(
    paste0("home_", stems),
    paste0("away_", stems),
    "temperature", "precip_probability", "wind_speed",
    "neutral_temperature", "neutral_wind"
  )
}

# Columns that are legitimately present in the feature table and are not
# features: identifiers, targets, and the market numbers the model is graded
# against. Anything numeric that is neither registered nor listed here is drift.
game_non_feature_columns <- function() {
  c(
    "game_id", "season", "week", "game_date", "home_team", "away_team",
    "home_score", "away_score", "home_score_rw", "away_score_rw",
    "home_margin", "game_total", "home_line", "total_line",
    "market_margin", "market_total", "surface", "weather", "precip_type"
  )
}

# The single place a schema mismatch is reported, so the three model families
# fail the same way.
assert_registered_features <- function(data, registered, ignore = character(),
                                       label = "model") {
  present <- names(data)
  missing <- setdiff(registered, present)
  if (length(missing)) {
    stop(
      sprintf(
        "%s: %d registered features are absent from the data: %s",
        label, length(missing), paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  numeric_present <- present[vapply(data[present], is.numeric, logical(1))]
  unregistered <- setdiff(numeric_present, c(registered, ignore))
  if (length(unregistered)) {
    stop(
      sprintf(
        paste(
          "%s: %d numeric columns are in the data but not in the registry: %s.",
          "Add them to R/feature_registry.R and re-run the validation that",
          "justifies them, or list them as non-features."
        ),
        label, length(unregistered), paste(unregistered, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  registered
}
