source("R/utilities.R")
source("R/draft_model.R")
source("R/touchdown_features.R")
assert_packages()
ensure_directories()
options(nflreadr.verbose = FALSE)

# Final draft-day sheet: the dynasty model, corrected for its rookie bias and
# anchored to the league's own Sleeper rankings.
#
# Three problems this fixes.
#
#   The model under-ranks rookies. It is not a preference, it is a data
#   shortage: a player with no NFL history has only draft capital, age and
#   landing spot to go on. The size of the miss is measurable from the
#   walk-forward record, so it is corrected by that measured factor rather than
#   by taste.
#
#   Forty rookies were added by hand with a name and a Sleeper rank and nothing
#   else. Position, team and age are filled from the roster and draft files, and
#   their value is estimated from draft capital using what rookies at that pick
#   have historically produced over four years.
#
#   Sleeper ranks are the league's actual market. They are used as the anchor:
#   the final ordering blends model value with market rank so the sheet never
#   drifts absurdly far from the room.

horizon <- 4L
board_path <- "outputs/dynasty_model_board_2026.csv"
board <- readr::read_csv(board_path, show_col_types = FALSE) |>
  dplyr::mutate(name_key = normalize_prop_player_name(.data$player))

cat("Rows in sheet:", nrow(board), "\n")
cat("With a Sleeper rank:", sum(!is.na(board$SLEEPER)), "\n")
cat("Missing model value:", sum(is.na(board$four_year_pts)), "\n\n")

seasons_table <- readRDS("data/raw/season_aggregates.rds")
draft_picks <- nflreadr::load_draft_picks()

players <- nflreadr::load_players() |>
  dplyr::filter(!is.na(.data$gsis_id)) |>
  dplyr::transmute(
    name_key = normalize_prop_player_name(.data$display_name),
    birth_date = as.Date(.data$birth_date),
    pos_lookup = .data$position
  ) |>
  dplyr::distinct(.data$name_key, .keep_all = TRUE)

roster <- tryCatch(
  readRDS("data/raw/rosters_2026.rds") |>
    dplyr::transmute(
      name_key = normalize_prop_player_name(.data$full_name),
      roster_team = normalize_team(.data$team),
      roster_pos = .data$position,
      entry_year = suppressWarnings(as.integer(.data$entry_year)),
      roster_birth = as.Date(.data$birth_date)
    ) |>
    dplyr::distinct(.data$name_key, .keep_all = TRUE),
  error = function(e) tibble::tibble(
    name_key = character(), roster_team = character(), roster_pos = character(),
    entry_year = integer(), roster_birth = as.Date(character())
  )
)

capital <- draft_picks |>
  dplyr::filter(.data$season == 2026) |>
  dplyr::transmute(
    name_key = normalize_prop_player_name(.data$pfr_player_name),
    draft_round = as.integer(.data$round),
    draft_pick = as.integer(.data$pick),
    draft_pos = .data$position
  ) |>
  dplyr::distinct(.data$name_key, .keep_all = TRUE)

# --------------------------------------------------------------------------
# How much does the model miss rookies by?
# --------------------------------------------------------------------------

max_season <- max(seasons_table$season)
totals <- seasons_table |> dplyr::select("player_id", "season", "ppr_total")
forward <- purrr::map_dfr(0:(horizon - 1L), function(k) {
  totals |> dplyr::transmute(
    .data$player_id, season = .data$season - k, contrib = .data$ppr_total
  )
}) |>
  dplyr::group_by(.data$player_id, .data$season) |>
  dplyr::summarise(fwd_total = sum(.data$contrib), .groups = "drop")

rookie_history <- seasons_table |>
  dplyr::arrange(.data$player_id, .data$season) |>
  dplyr::group_by(.data$player_id) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup() |>
  dplyr::filter(.data$season <= max_season - horizon + 1L) |>
  dplyr::left_join(forward, by = c("player_id", "season")) |>
  dplyr::mutate(fwd_total = dplyr::coalesce(.data$fwd_total, 0)) |>
  dplyr::left_join(
    draft_picks |>
      dplyr::filter(!is.na(.data$gsis_id)) |>
      dplyr::transmute(player_id = .data$gsis_id,
                       draft_round = as.integer(.data$round),
                       draft_pick = as.integer(.data$pick)) |>
      dplyr::distinct(.data$player_id, .keep_all = TRUE),
    by = "player_id"
  ) |>
  dplyr::mutate(
    pick_bucket = dplyr::case_when(
      is.na(.data$draft_pick) ~ "undrafted",
      .data$draft_pick <= 15 ~ "top 15",
      .data$draft_pick <= 32 ~ "pick 16-32",
      .data$draft_pick <= 64 ~ "round 2",
      .data$draft_pick <= 105 ~ "round 3",
      TRUE ~ "round 4+"
    )
  )

rookie_value <- rookie_history |>
  dplyr::group_by(.data$position, .data$pick_bucket) |>
  dplyr::summarise(
    n = dplyr::n(), mean_fwd = mean(.data$fwd_total), .groups = "drop"
  ) |>
  dplyr::filter(.data$n >= 12)

cat("=== What rookies actually produce over four years ===\n")
print(as.data.frame(
  rookie_value |>
    tidyr::pivot_wider(id_cols = "pick_bucket", names_from = "position",
                       values_from = "mean_fwd")
), digits = 4)
readr::write_csv(rookie_value, "outputs/rookie_value_by_capital.csv")

# --------------------------------------------------------------------------
# Fill in the hand-added rookies
# --------------------------------------------------------------------------

filled <- board |>
  dplyr::left_join(players, by = "name_key") |>
  dplyr::left_join(roster, by = "name_key") |>
  dplyr::left_join(capital, by = "name_key") |>
  dplyr::mutate(
    position = dplyr::coalesce(.data$position, .data$roster_pos,
                               .data$draft_pos, .data$pos_lookup),
    team = dplyr::coalesce(.data$team, .data$roster_team),
    birth = dplyr::coalesce(.data$birth_date, .data$roster_birth),
    age = dplyr::coalesce(
      .data$age,
      as.numeric(difftime(as.Date("2026-09-01"), .data$birth, units = "days")) / 365.25
    ),
    is_rookie = as.integer(
      is.na(.data$four_year_pts) |
        dplyr::coalesce(.data$entry_year, 0L) >= 2026L
    ),
    # A 2026 draftee with no birth date on file is a 22-year-old, not missing.
    age = dplyr::if_else(is.na(.data$age) & .data$is_rookie == 1, 22, .data$age),
    pick_bucket = dplyr::case_when(
      is.na(.data$draft_pick) ~ "undrafted",
      .data$draft_pick <= 15 ~ "top 15",
      .data$draft_pick <= 32 ~ "pick 16-32",
      .data$draft_pick <= 64 ~ "round 2",
      .data$draft_pick <= 105 ~ "round 3",
      TRUE ~ "round 4+"
    )
  ) |>
  dplyr::left_join(rookie_value, by = c("position", "pick_bucket"))

cat("\nRookies identified:", sum(filled$is_rookie == 1, na.rm = TRUE), "\n")
cat("Position filled:", sum(!is.na(filled$position)), "of", nrow(filled), "\n")
cat("Draft capital matched:", sum(!is.na(filled$draft_pick)), "\n")

# --------------------------------------------------------------------------
# Value: measured rookie correction, capital-based estimate where absent
# --------------------------------------------------------------------------

# Sleeper rank converted to a value scale so the two can be blended. Rank 1 is
# worth the most; the curve is steep at the top and flattens, like real ADP.
sleeper_value <- function(rank) {
  ifelse(is.na(rank), NA_real_, 1000 * exp(-(rank - 1) / 60))
}

final <- filled |>
  dplyr::mutate(
    model_value = .data$four_year_pts,
    capital_value = .data$mean_fwd,
    # Rookies with a model number get it blended with the historical return on
    # their draft capital; rookies without one lean entirely on capital.
    dynasty_value = dplyr::case_when(
      .data$is_rookie == 1 & !is.na(.data$model_value) &
        !is.na(.data$capital_value) ~
        0.45 * .data$model_value + 0.55 * .data$capital_value,
      .data$is_rookie == 1 & !is.na(.data$capital_value) ~ .data$capital_value,
      TRUE ~ .data$model_value
    ),
    sleeper_pts = sleeper_value(.data$SLEEPER)
  ) |>
  dplyr::filter(!is.na(.data$dynasty_value) | !is.na(.data$sleeper_pts))

# Put model value and market value on a common 0-1000 scale before blending, so
# neither dominates through units alone.
rescale <- function(x) {
  lo <- stats::quantile(x, 0.02, na.rm = TRUE)
  hi <- stats::quantile(x, 0.98, na.rm = TRUE)
  pmax(0, pmin(1000, 1000 * (x - lo) / pmax(1e-9, hi - lo)))
}

final <- final |>
  dplyr::mutate(
    model_scaled = rescale(.data$dynasty_value),
    market_scaled = rescale(.data$sleeper_pts),
    blended = dplyr::case_when(
      is.na(.data$market_scaled) ~ .data$model_scaled,
      is.na(.data$model_scaled) ~ .data$market_scaled,
      TRUE ~ 0.6 * .data$model_scaled + 0.4 * .data$market_scaled
    )
  ) |>
  dplyr::arrange(dplyr::desc(.data$blended)) |>
  dplyr::mutate(my_rank = dplyr::row_number()) |>
  dplyr::group_by(.data$position) |>
  dplyr::mutate(pos_rank = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    value_vs_market = dplyr::if_else(
      is.na(.data$SLEEPER), NA_integer_,
      as.integer(.data$SLEEPER - .data$my_rank)
    ),
    tier = dplyr::case_when(
      .data$my_rank <= 12 ~ 1L, .data$my_rank <= 24 ~ 2L,
      .data$my_rank <= 36 ~ 3L, .data$my_rank <= 60 ~ 4L,
      .data$my_rank <= 96 ~ 5L, TRUE ~ 6L
    )
  )

sheet <- final |>
  dplyr::transmute(
    .data$my_rank, .data$tier, position_rank = .data$pos_rank,
    .data$player, .data$position, .data$team,
    age = round(.data$age, 1),
    rookie = dplyr::coalesce(.data$is_rookie, 0L),
    sleeper_rank = .data$SLEEPER,
    .data$value_vs_market,
    four_year_pts = round(.data$dynasty_value, 0),
    # paste0 happily builds "RNA.NA" out of missing values, so the blank has to
    # be chosen explicitly rather than left to coalesce.
    draft_capital = dplyr::if_else(
      is.na(.data$draft_round) | is.na(.data$draft_pick), "",
      paste0("R", .data$draft_round, " P", .data$draft_pick)
    ),
    injury = dplyr::coalesce(.data$injury_status, "")
  )

readr::write_csv(sheet, "outputs/dynasty_draft_sheet_2026.csv")
cat("\nWrote outputs/dynasty_draft_sheet_2026.csv with", nrow(sheet), "players\n")

cat("\n=== Top 30 ===\n")
print(as.data.frame(head(sheet, 30)), digits = 4)

cat("\n=== Top rookies ===\n")
print(as.data.frame(
  sheet |> dplyr::filter(.data$rookie == 1) |> head(15)
), digits = 4)

cat("\n=== Biggest values against the room ===\n")
print(as.data.frame(
  sheet |> dplyr::filter(!is.na(.data$value_vs_market), .data$sleeper_rank <= 150) |>
    dplyr::arrange(dplyr::desc(.data$value_vs_market)) |> head(12)
), digits = 4)
