# Pregame injury-report features.
#
# This is the one input in the project that the market has and the models did
# not. Official reports are published Wednesday through Friday, so the final
# game designation and the week's practice participation are both known before
# kickoff and using them is not leakage.
#
# Coverage starts in 2009, which spans the whole 2018-2025 backtest window.

injury_position_group <- function(position) {
  position <- toupper(trimws(as.character(position)))
  dplyr::case_when(
    position == "QB" ~ "qb",
    position %in% c("T", "G", "C", "OL", "OT", "OG") ~ "ol",
    position %in% c("RB", "WR", "TE", "FB", "HB") ~ "skill",
    position %in% c("DE", "DT", "NT", "DL") ~ "dl",
    position %in% c("LB", "ILB", "OLB", "MLB", "EDGE") ~ "lb",
    position %in% c("CB", "S", "FS", "SS", "DB") ~ "db",
    TRUE ~ "other"
  )
}

# A single designation is not a single amount of missing player. Out is
# certain, Questionable historically plays more often than not, and a
# Wednesday DNP that never upgrades is a stronger signal than the label alone.
injury_availability_weight <- function(report_status, practice_status) {
  report <- tolower(trimws(as.character(report_status)))
  practice <- tolower(trimws(as.character(practice_status)))

  base <- dplyr::case_when(
    report == "out" ~ 1.00,
    report == "doubtful" ~ 0.75,
    report == "questionable" ~ 0.35,
    TRUE ~ 0.00
  )
  practice_bump <- dplyr::case_when(
    grepl("did not participate", practice) ~ 0.20,
    grepl("limited", practice) ~ 0.10,
    TRUE ~ 0.00
  )
  pmin(1, base + dplyr::if_else(base > 0, practice_bump, practice_bump * 0.5))
}

build_team_injury_features <- function(injuries) {
  injuries |>
    dplyr::filter(.data$game_type == "REG") |>
    dplyr::mutate(
      team = normalize_team(.data$team),
      group = injury_position_group(.data$position),
      weight = injury_availability_weight(
        .data$report_status, .data$practice_status
      ),
      # Most rows carry no game designation at all, meaning the player is
      # expected to play. `%in%` keeps those as FALSE; `==` would make them NA
      # and poison every count downstream.
      is_out = tolower(trimws(as.character(.data$report_status))) %in% "out",
      is_dnp = grepl(
        "did not participate", tolower(as.character(.data$practice_status))
      )
    ) |>
    dplyr::group_by(
      season = as.integer(.data$season),
      week = as.integer(.data$week),
      .data$team
    ) |>
    dplyr::summarise(
      inj_qb_out = as.numeric(any(.data$group == "qb" & .data$is_out)),
      inj_qb_weight = max(c(0, .data$weight[.data$group == "qb"])),
      inj_out_total = sum(.data$is_out),
      inj_dnp_total = sum(.data$is_dnp),
      inj_burden = sum(.data$weight),
      inj_burden_ol = sum(.data$weight[.data$group == "ol"]),
      inj_burden_skill = sum(.data$weight[.data$group == "skill"]),
      inj_burden_dl = sum(.data$weight[.data$group == "dl"]),
      inj_burden_lb = sum(.data$weight[.data$group == "lb"]),
      inj_burden_db = sum(.data$weight[.data$group == "db"]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      inj_burden_off = .data$inj_burden_ol + .data$inj_burden_skill +
        .data$inj_qb_weight,
      inj_burden_def = .data$inj_burden_dl + .data$inj_burden_lb +
        .data$inj_burden_db
    )
}

injury_feature_columns <- function() {
  c(
    "inj_qb_out", "inj_qb_weight", "inj_out_total", "inj_dnp_total",
    "inj_burden", "inj_burden_ol", "inj_burden_skill", "inj_burden_dl",
    "inj_burden_lb", "inj_burden_db", "inj_burden_off", "inj_burden_def"
  )
}

# Attaches home and away injury columns plus their differentials. Teams with no
# report that week are treated as fully healthy, which is what an empty report
# means rather than missing data.
add_game_injury_features <- function(games, team_injuries) {
  columns <- injury_feature_columns()

  side <- function(team_column, prefix) {
    games |>
      dplyr::select(
        "game_id", "season", "week", team = dplyr::all_of(team_column)
      ) |>
      dplyr::left_join(team_injuries, by = c("season", "week", "team")) |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(columns), ~ dplyr::coalesce(.x, 0))
      ) |>
      dplyr::select("game_id", dplyr::all_of(columns)) |>
      dplyr::rename_with(~ paste0(prefix, .x), dplyr::all_of(columns))
  }

  out <- games |>
    dplyr::left_join(side("home_team", "home_"), by = "game_id") |>
    dplyr::left_join(side("away_team", "away_"), by = "game_id")

  for (column in columns) {
    out[[paste0("diff_", column)]] <-
      out[[paste0("home_", column)]] - out[[paste0("away_", column)]]
  }
  out
}

game_injury_feature_columns <- function() {
  columns <- injury_feature_columns()
  c(
    paste0("home_", columns), paste0("away_", columns),
    paste0("diff_", columns)
  )
}
