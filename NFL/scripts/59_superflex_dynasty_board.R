source("R/utilities.R")
source("R/draft_model.R")
assert_packages()
ensure_directories()
options(nflreadr.verbose = FALSE)

# Superflex dynasty startup board, 12 teams.
#
# The redraft board is wrong for this league in two structural ways and both
# had to be fixed rather than adjusted for by eye:
#
#   Superflex. Replacement level for quarterbacks is not QB13, it is roughly
#   QB25 - twelve teams starting two apiece. In a one-quarterback league the gap
#   between QB1 and QB13 is small, which is why the redraft board buries
#   quarterbacks. Here that gap is the single biggest source of value on the
#   board and the ordering inverts.
#
#   Dynasty. The model projects one season. A startup is a multi-year asset
#   draft, so a 23-year-old and a 29-year-old with identical 2026 projections
#   are worth very different picks. Value here is the discounted sum of
#   projected production across a four-year window, with each future year scaled
#   by a position-specific age curve estimated from 2006-2025.
#
# The age curve is measured, not assumed: mean points per game by position and
# age across twenty seasons, smoothed and normalised to each position's peak.

horizon <- 4L
discount <- 0.92

seasons_table <- readRDS("data/raw/season_aggregates.rds")
team_offense <- readRDS("data/raw/team_offense.rds")
draft_picks <- nflreadr::load_draft_picks()

# --------------------------------------------------------------------------
# Ages
# --------------------------------------------------------------------------

players <- nflreadr::load_players() |>
  dplyr::filter(!is.na(.data$gsis_id), !is.na(.data$birth_date)) |>
  dplyr::transmute(
    player_id = .data$gsis_id,
    birth_date = as.Date(.data$birth_date)
  ) |>
  dplyr::distinct(.data$player_id, .keep_all = TRUE)

age_at <- function(birth, season) {
  as.numeric(difftime(as.Date(paste0(season, "-09-01")), birth, units = "days")) / 365.25
}

history <- seasons_table |>
  dplyr::inner_join(players, by = "player_id") |>
  dplyr::mutate(age = age_at(.data$birth_date, .data$season)) |>
  dplyr::filter(.data$age >= 20, .data$age <= 38, .data$games >= 4)

# --------------------------------------------------------------------------
# Age curves
# --------------------------------------------------------------------------

# Cross-sectional means by age are survivorship-poisoned: only the good players
# are still starting at 34, so average output *rises* with age and a dynasty
# board built on it would prefer thirty-somethings. The fix is to measure each
# player against himself - the year-over-year change in points per game from age
# a to a+1 - and chain those deltas into a curve. Every player is his own
# control, so quality drops out.
transitions <- history |>
  dplyr::select("player_id", "season", "position", "age", "ppr_ppg") |>
  dplyr::inner_join(
    history |>
      dplyr::transmute(
        .data$player_id, season = .data$season - 1L,
        next_ppg = .data$ppr_ppg, next_games = .data$games
      ),
    by = c("player_id", "season")
  ) |>
  dplyr::left_join(
    history |> dplyr::select("player_id", "season", this_games = "games"),
    by = c("player_id", "season")
  ) |>
  # Select on playing time, never on production. Requiring a good prior season
  # (ppg >= 3) guarantees the sample regresses downward next year and makes the
  # curve collapse - it had wide receivers at 10% of peak by 34. Games played is
  # the honest availability filter and leaves the production change unbiased.
  dplyr::filter(
    dplyr::coalesce(.data$this_games, 0) >= 8,
    dplyr::coalesce(.data$next_games, 0) >= 8
  ) |>
  dplyr::mutate(
    age_bin = round(.data$age),
    delta = log(pmax(.data$next_ppg, 0.5) / pmax(.data$ppr_ppg, 0.5))
  )

steps <- transitions |>
  dplyr::group_by(.data$position, .data$age_bin) |>
  dplyr::summarise(
    n = dplyr::n(), mean_delta = mean(.data$delta), .groups = "drop"
  ) |>
  dplyr::filter(.data$n >= 25) |>
  dplyr::arrange(.data$position, .data$age_bin)

curve <- steps |>
  dplyr::group_by(.data$position) |>
  dplyr::mutate(
    level = exp(cumsum(.data$mean_delta)),
    factor = .data$level / max(.data$level)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(age_bin = .data$age_bin + 1L) |>
  dplyr::select("position", "age_bin", "n", "factor")

# Anchor the youngest observed age at the curve's own start so a 22-year-old is
# not treated as missing.
curve <- dplyr::bind_rows(
  curve |>
    dplyr::group_by(.data$position) |>
    dplyr::slice_min(.data$age_bin, n = 1) |>
    dplyr::mutate(age_bin = .data$age_bin - 1L) |>
    dplyr::ungroup(),
  curve
) |>
  dplyr::arrange(.data$position, .data$age_bin)

readr::write_csv(curve, "outputs/dynasty_age_curves.csv")
cat("=== Age curve, share of positional peak ===\n")
print(as.data.frame(
  curve |>
    dplyr::filter(.data$age_bin %in% c(22, 24, 26, 28, 30, 32, 34)) |>
    dplyr::select("position", "age_bin", "factor") |>
    tidyr::pivot_wider(names_from = "position", values_from = "factor")
), digits = 3)

curve_factor <- function(position, age) {
  vapply(seq_along(position), function(i) {
    rows <- curve[curve$position == position[[i]], ]
    if (!nrow(rows)) return(1)
    a <- max(min(round(age[[i]]), max(rows$age_bin)), min(rows$age_bin))
    rows$factor[which.min(abs(rows$age_bin - a))]
  }, numeric(1))
}

# --------------------------------------------------------------------------
# 2026 projections, reusing the validated model
# --------------------------------------------------------------------------

board <- readr::read_csv("outputs/draft_board_2026.csv", show_col_types = FALSE)

roster <- tryCatch(
  readRDS("data/raw/rosters_2026.rds") |>
    dplyr::filter(!is.na(.data$gsis_id)) |>
    dplyr::transmute(
      player_id = .data$gsis_id,
      full_name = .data$full_name
    ) |>
    dplyr::distinct(.data$player_id, .keep_all = TRUE),
  error = function(e) tibble::tibble(player_id = character(),
                                     full_name = character())
)

# The board carries names, not ids, so ages are joined on a normalised name.
source("R/touchdown_features.R")
# Birth dates are resolved to a name key up front, so the board joins once on
# name rather than hopping through an id column it does not carry.
birth_by_name <- dplyr::bind_rows(
  roster |> dplyr::transmute(
    name_key = normalize_prop_player_name(.data$full_name), .data$player_id
  ),
  seasons_table |> dplyr::transmute(
    name_key = normalize_prop_player_name(.data$player_name), .data$player_id
  )
) |>
  dplyr::distinct(.data$name_key, .keep_all = TRUE) |>
  dplyr::inner_join(players, by = "player_id") |>
  dplyr::select("name_key", "birth_date")

dynasty <- board |>
  dplyr::mutate(name_key = normalize_prop_player_name(.data$player_name)) |>
  dplyr::left_join(birth_by_name, by = "name_key") |>
  dplyr::mutate(
    age = age_at(.data$birth_date, 2026),
    # A rookie with no birth date on file is assumed to be draft age rather
    # than dropped, since the class is the point of a startup draft.
    age = dplyr::if_else(
      is.na(.data$age) & .data$is_rookie == 1, 22, .data$age
    ),
    age = dplyr::coalesce(.data$age, 26)
  )

cat("\nPlayers with a real birth date:", sum(!is.na(dynasty$birth_date)),
    "of", nrow(dynasty), "\n")

# Multi-year value: this season at full weight, then each subsequent year
# discounted and scaled by how the age curve moves.
dynasty <- dynasty |>
  dplyr::mutate(
    base_factor = curve_factor(.data$position, .data$age),
    dynasty_points = purrr::pmap_dbl(
      list(.data$position, .data$age, .data$projected_total,
           .data$base_factor),
      function(pos, age, total, base) {
        if (!is.finite(total) || !is.finite(base) || base <= 0) return(NA_real_)
        sum(vapply(0:(horizon - 1L), function(k) {
          discount^k * total * (curve_factor(pos, age + k) / base)
        }, numeric(1)))
      }
    )
  )

# --------------------------------------------------------------------------
# Superflex replacement levels
# --------------------------------------------------------------------------

teams <- 12L
replacement_rank <- c(
  QB = teams * 2L + 1L,   # two quarterbacks start per team
  RB = round(teams * 2.5),
  WR = round(teams * 3),
  TE = teams + 1L
)
cat("\nReplacement ranks:",
    paste(names(replacement_rank), replacement_rank, sep = "=", collapse = "  "),
    "\n")

replacement <- dynasty |>
  dplyr::filter(!is.na(.data$dynasty_points)) |>
  dplyr::group_by(.data$position) |>
  dplyr::summarise(
    replacement = {
      k <- replacement_rank[[dplyr::first(.data$position)]]
      v <- sort(.data$dynasty_points, decreasing = TRUE)
      if (length(v) >= k) v[[k]] else min(v)
    },
    .groups = "drop"
  )

final <- dynasty |>
  dplyr::inner_join(replacement, by = "position") |>
  dplyr::mutate(dynasty_vor = .data$dynasty_points - .data$replacement) |>
  dplyr::arrange(dplyr::desc(.data$dynasty_vor)) |>
  dplyr::mutate(rank = dplyr::row_number()) |>
  dplyr::group_by(.data$position) |>
  dplyr::mutate(pos_rank = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    .data$rank, .data$pos_rank, player = .data$player_name, .data$position,
    .data$team, age = round(.data$age, 1),
    dynasty_vor = round(.data$dynasty_vor, 1),
    dynasty_points = round(.data$dynasty_points, 1),
    season_2026 = round(.data$projected_total, 1),
    proj_games = round(.data$projected_games, 1),
    rookie = .data$is_rookie
  )

readr::write_csv(final, "outputs/superflex_dynasty_board_2026.csv")

cat("\n=== Top 40, superflex dynasty ===\n")
print(as.data.frame(head(final, 40)), digits = 4)

cat("\n=== Positional mix by tier ===\n")
print(as.data.frame(
  final |>
    dplyr::mutate(tier = dplyr::case_when(
      .data$rank <= 12 ~ "1-12", .data$rank <= 24 ~ "13-24",
      .data$rank <= 36 ~ "25-36", .data$rank <= 60 ~ "37-60",
      TRUE ~ "61+"
    )) |>
    dplyr::filter(.data$rank <= 60) |>
    dplyr::count(.data$tier, .data$position) |>
    tidyr::pivot_wider(names_from = "position", values_from = "n",
                       values_fill = 0)
))
