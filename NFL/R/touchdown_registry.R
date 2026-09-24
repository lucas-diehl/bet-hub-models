## ---------------------------------------------------------------------------
## R/touchdown_registry.R
##
## Three things, in the order they must happen:
##   1. Schema registry + assertion  — locks the feature set so the join below
##      cannot silently change model scope (handoff §3: this model has no
##      registered-schema assertion the way the game models do).
##   2. Redzone join                 — wires td_redzone_features.rds into the
##      player-week frame BEFORE rolling.
##   3. Carryover staleness features — makes the model aware of how much of a
##      row's rolling history came from a prior season or a prior team.
##
## Sourced from R/touchdown_features.R.
##
## Two fixes from the handed-off draft, both found by tracing through what
## actually happens on a live 2026 call rather than trusting the comments:
##
##   - Opponent (def_*) redzone measures are now rolled by DEFENSE
##     (roll_td_redzone_defense(), called from build_td_player_features(),
##     mirroring the existing add_td_defense_features() pattern) instead of
##     being routed through the per-PLAYER rolling_measures loop. Rolling them
##     per-player would average across whatever different opponent a player
##     happened to face each week, not "this upcoming opponent's own recent
##     defensive trend." join_td_redzone_features() below only attaches the
##     player_game half now; the def_game half is handled separately.
##
##   - The match-rate guard in join_td_redzone_features() checked
##     !is.na(game_id) to mean "a completed game." In this codebase game_id is
##     a schedule-assigned string present on every scheduled game, played or
##     not, so that check never actually excluded future games - it would have
##     counted every unplayed 2026 row as "played but unmatched" and hard-
##     crashed build_td_2026_player_features() on every live call. Changed to
##     game_date < Sys.Date(), matching what the comment already claimed it did.
## ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(rlang)
})

## ===========================================================================
## 1. SCHEMA REGISTRY
## ===========================================================================

TD_SCHEMA_LOCK <- "config/td_feature_schema.lock.rds"

snapshot_td_schema <- function(feature_names, path = TD_SCHEMA_LOCK,
                               note = "") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(
    list(features = sort(unique(feature_names)),
         locked_at = Sys.time(),
         note = note),
    path
  )
  message("Schema locked: ", length(feature_names), " features -> ", path)
  invisible(feature_names)
}

assert_td_schema <- function(feature_names, path = TD_SCHEMA_LOCK,
                             allow_additions = character(0)) {
  if (!file.exists(path)) {
    stop("No schema lock at ", path,
         ". Run snapshot_td_schema() on a known-good build first.",
         call. = FALSE)
  }
  lock    <- readRDS(path)
  actual  <- sort(unique(feature_names))
  added   <- setdiff(actual, c(lock$features, allow_additions))
  removed <- setdiff(lock$features, actual)

  if (length(added) || length(removed)) {
    stop(
      "TD feature schema drift.\n",
      if (length(added))
        paste0("  UNEXPECTED (", length(added), "): ",
               paste(added, collapse = ", "), "\n") else "",
      if (length(removed))
        paste0("  MISSING (", length(removed), "): ",
               paste(removed, collapse = ", "), "\n") else "",
      "  Locked ", format(lock$locked_at, "%Y-%m-%d %H:%M"),
      " with ", length(lock$features), " features.\n",
      "  If intended, pass allow_additions= or re-run snapshot_td_schema().",
      call. = FALSE
    )
  }
  message("Schema OK: ", length(actual), " features match lock.")
  invisible(TRUE)
}

## The columns the redzone + carryover work is *supposed* to add, once rolled.
td_redzone_expected_features <- function() {
  base <- td_redzone_rolling_measures()
  c(
    paste0(base, "_r3"),
    paste0(base, "_r5"),
    paste0(td_redzone_opponent_measures(), "_r5"),
    "carryover_r3", "team_changed_r3", "days_since_prior_game"
  )
}

## Snap-share features, added 2026-09-17. Listed as an explicit allowed addition
## rather than silently re-snapshotting the lock, so the schema assertion still
## records that the model's scope changed deliberately and when.
td_snap_expected_features <- function() {
  c("snap_share_r3", "snap_share_r5",
    "offense_snaps_r3", "offense_snaps_r5")
}

## Base measure names appended to `rolling_measures` in prepare_td_player_weeks().
## Keep these OUT of the model feature list un-rolled - same-game values, would
## leak the target game.
td_redzone_rolling_measures <- function() {
  c("rz10_touches", "gtg_touches", "xtd_total",
    "rz10_carry_share", "gtg_carry_share",
    "rz10_target_share", "gtg_target_share", "xtd_share")
}

## Rolled by DEFENSE, not by player - see header note. Handled by
## roll_td_redzone_defense(), not the per-player rolling_measures loop.
td_redzone_opponent_measures <- function() {
  c("def_rz10_carries_allowed", "def_rz10_targets_allowed",
    "def_rz_td_conv_allowed", "def_xtd_allowed")
}

## ===========================================================================
## 2. REDZONE JOIN — player side only. Opponent side is roll_td_redzone_defense().
##
## Call inside prepare_td_player_weeks(), after the player-week frame is
## assembled and BEFORE the rolling step.
## ===========================================================================

join_td_redzone_features <- function(player_weeks,
                                     path = "data/processed/td_redzone_features.rds",
                                     min_match_rate = 0.90) {
  if (!file.exists(path)) {
    stop("Missing ", path, " — run scripts/73_td_redzone_features.R first.",
         call. = FALSE)
  }
  rz <- readRDS(path)
  n_before <- nrow(player_weeks)

  pg <- rz$player_game %>%
    select(-any_of(c("game_id", "posteam", "defteam")))

  out <- player_weeks %>%
    left_join(pg, by = c("season", "week", "player_id"))

  ## Fan-out guard. A left_join that adds rows means duplicate keys upstream,
  ## which would silently multiply a player's history in the rolling windows.
  if (nrow(out) != n_before) {
    stop("Redzone join changed row count: ", n_before, " -> ", nrow(out),
         ". Duplicate keys in td_redzone_features.rds$player_game.",
         call. = FALSE)
  }

  ## Match-rate guard, evaluated only on games that have actually been played
  ## AND where the player recorded a touch. game_id is a schedule-assigned
  ## string present on every scheduled game, played or not, so it cannot be
  ## used to detect "no PBP yet" - game_date against today's date is the only
  ## reliable signal available here. Restricting to touches > 0 matters as
  ## much as the date filter does: the base player-week universe is every QB/
  ## RB/WR/TE/FB in player_stats that week, which includes plenty of
  ## legitimate zero-carry, zero-target rows (backups, healthy scratches still
  ## carrying a zero-stat line) that will never appear in the touch-level
  ## redzone table because there was no touch to record - that is correct
  ## behaviour, not a join failure, and checking match rate before excluding
  ## them makes a real ID-convention mismatch indistinguishable from normal
  ## non-participation.
  played <- out %>%
    filter(!is.na(.data$game_date), .data$game_date < Sys.Date(),
           .data$touches > 0)
  if (nrow(played) > 0) {
    match_rate <- mean(!is.na(played$xtd_total))
    message(sprintf("Redzone join: %.1f%% of %s completed player-games matched.",
                    100 * match_rate, format(nrow(played), big.mark = ",")))
    if (match_rate < min_match_rate) {
      stop("Redzone match rate ", round(100 * match_rate, 1),
           "% below threshold ", round(100 * min_match_rate),
           "%. Likely a player_id convention mismatch (GSIS vs other).",
           call. = FALSE)
    }
  }

  ## Zero-fill counts (a player genuinely had no redzone touches, or this is a
  ## future game with no same-game value to have at all - same treatment the
  ## existing base measures already get). Leave shares as NA (undefined when
  ## the team never reached the zone) so the existing median imputation in
  ## td_matrix_pair() handles them.
  out %>%
    mutate(across(any_of(c("rz10_touches", "gtg_touches", "xtd_total",
                           "rz20_carries", "rz10_carries", "rz5_carries",
                           "gtg_carries", "rz20_targets", "rz10_targets",
                           "rz5_targets", "gtg_targets",
                           "xtd_rush", "xtd_rec")),
                  ~ ifelse(is.na(.x), 0, .x)))
}

## Opponent side: roll by defense (arranged by season/week within each
## defteam), producing already-_r5-suffixed columns, then join by
## (game_id, opponent_team). Call from build_td_player_features(), right
## after add_td_defense_features() - same place, same pattern as the
## existing def_*_r5 features.
roll_td_redzone_defense <- function(players,
                                    path = "data/processed/td_redzone_features.rds",
                                    min_match_rate = 0.90) {
  if (!file.exists(path)) {
    stop("Missing ", path, " — run scripts/73_td_redzone_features.R first.",
         call. = FALSE)
  }
  rz <- readRDS(path)
  ## normalize_team() on both sides. Raw pbp writes "LA" for the Rams; every
  ## player-week frame here says "LAR", so this join silently missed all 133
  ## Rams defense-games and fed median-imputed red-zone defense to the 3.0% of
  ## player-weeks facing them. scripts/73 now normalizes at build time too;
  ## this is the belt-and-braces half, and it keeps a stale pre-fix cache
  ## joining correctly instead of reintroducing the bug.
  rolled <- rz$def_game %>%
    mutate(defteam = normalize_team(.data$defteam)) %>%
    arrange(.data$defteam, .data$season, .data$week) %>%
    group_by(.data$defteam) %>%
    mutate(
      across(
        all_of(td_redzone_opponent_measures()),
        ~ td_lagged_roll(.x, 5L),
        .names = "{.col}_r5"
      )
    ) %>%
    ungroup() %>%
    select("game_id", "season", "week", "defteam", ends_with("_r5"))

  n_before <- nrow(players)
  out <- players %>%
    mutate(opponent_team = normalize_team(.data$opponent_team)) %>%
    left_join(
      rolled %>% select(-"game_id") %>% rename(opponent_team = "defteam"),
      by = c("season", "week", "opponent_team")
    )
  if (nrow(out) != n_before) {
    stop("Opponent redzone roll changed row count: ", n_before, " -> ",
         nrow(out), ". Duplicate (season, week, defteam) in def_game.",
         call. = FALSE)
  }

  ## Match-rate guard. Without this, a team-code convention drift (the exact
  ## bug above) shows up as nothing at all: the join "succeeds", the columns
  ## exist, they are simply NA, and median imputation downstream makes the
  ## model look healthy while flying blind on a slice of the league. Evaluate
  ## only on rows the artifact could legitimately cover - completed games
  ## within the seasons it was actually built over. A defense's first game of
  ## the earliest season has no prior week to roll, so it is NA by design.
  covered <- out %>%
    filter(
      !is.na(.data$season),
      .data$season > min(rz$def_game$season, na.rm = TRUE),
      .data$season <= max(rz$def_game$season, na.rm = TRUE),
      !is.na(.data$game_date), .data$game_date < Sys.Date()
    )
  if (nrow(covered) > 0) {
    match_rate <- mean(!is.na(covered$def_xtd_allowed_r5))
    message(sprintf(
      "Redzone defense roll: %.1f%% of %s covered player-games matched.",
      100 * match_rate, format(nrow(covered), big.mark = ",")
    ))
    if (match_rate < min_match_rate) {
      stop("Redzone defense match rate ", round(100 * match_rate, 1),
           "% below threshold ", round(100 * min_match_rate),
           "%. Likely a team-code convention mismatch between pbp (LA/OAK/SD) ",
           "and normalize_team() (LAR/LV/LAC), or def_game is missing seasons.",
           call. = FALSE)
    }
  }
  out
}

## ===========================================================================
## 3. CARRYOVER STALENESS FEATURES
## ===========================================================================

add_td_carryover_flags <- function(player_weeks) {
  stopifnot(all(c("player_id", "season", "team") %in% names(player_weeks)))

  date_col <- if ("game_date" %in% names(player_weeks)) "game_date" else "week"

  player_weeks %>%
    arrange(.data$player_id, .data[[date_col]]) %>%
    group_by(.data$player_id) %>%
    mutate(
      carryover_r3 =
        (is.na(lag(.data$season, 1)) | lag(.data$season, 1) < .data$season) +
        (is.na(lag(.data$season, 2)) | lag(.data$season, 2) < .data$season) +
        (is.na(lag(.data$season, 3)) | lag(.data$season, 3) < .data$season),

      team_changed_r3 = as.integer(
        !is.na(lag(.data$team, 1)) & lag(.data$team, 1) != .data$team
      ),

      days_since_prior_game = if (date_col == "game_date") {
        as.numeric(.data[[date_col]] - lag(.data[[date_col]], 1))
      } else {
        NA_real_
      }
    ) %>%
    ungroup() %>%
    mutate(
      team_changed_r3 = ifelse(is.na(.data$team_changed_r3), 0L,
                               .data$team_changed_r3),
      days_since_prior_game = pmin(.data$days_since_prior_game, 400)
    )
}

## ===========================================================================
## 4. INTEGRATION — see edits applied to R/touchdown_features.R and
##    R/touchdown_model.R in this same change.
## ===========================================================================
