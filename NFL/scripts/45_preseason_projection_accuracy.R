source("R/utilities.R")
source("R/fantasy_prop_model.R")
assert_packages()
ensure_directories()

# How well do the model's projections hold up under preseason conditions, and
# are they useful for a draft?
#
# An important framing caveat is built into this script. The model projects one
# game at a time from rolling in-season form. It has no season-long mode. The
# only week where every feature comes from the prior season - the preseason
# information state - is Week 1, so Week 1 across 2023-2025 is the honest test
# bed. A draft, however, cares about full-season value, so the more relevant
# question is not "how close was the Week 1 number" but "does a preseason-state
# projection rank players the way the season ends up ranking them".
#
# Both are measured, against a naive prior-season points-per-game baseline,
# which is roughly the information a drafter has anyway.

features <- readRDS("data/processed/fantasy_prop_features_augmented.rds")
walk_forward <- readRDS("data/processed/fantasy_prop_walk_forward.rds")$predictions

ppr_weights <- c(
  passing_yards = 0.04, passing_tds = 4, interceptions = -2,
  rushing_yards = 0.10, rushing_tds = 6, receptions = 1,
  receiving_yards = 0.10, receiving_tds = 6, fumbles_lost = -2
)

# Compose projected PPR from the nine component models.
projected <- walk_forward |>
  dplyr::filter(.data$target %in% names(ppr_weights)) |>
  dplyr::mutate(weight = unname(ppr_weights[.data$target])) |>
  dplyr::group_by(
    .data$season, .data$week, .data$game_id, .data$player_id,
    .data$player_display_name, .data$position
  ) |>
  dplyr::summarise(
    projected_ppr = sum(.data$prediction * .data$weight),
    .groups = "drop"
  )

actuals <- features |>
  dplyr::select(
    "season", "week", "game_id", "player_id", "position",
    "ppr_points_actual", "prior_games"
  )

# Season-long outcome per player: points per game over games actually played.
season_totals <- features |>
  dplyr::group_by(.data$season, .data$player_id) |>
  dplyr::summarise(
    games = dplyr::n(),
    season_ppr = sum(.data$ppr_points_actual, na.rm = TRUE),
    season_ppg = mean(.data$ppr_points_actual, na.rm = TRUE),
    .groups = "drop"
  )

prior_season <- season_totals |>
  dplyr::mutate(season = .data$season + 1L) |>
  dplyr::select(
    "season", "player_id",
    prior_ppg = "season_ppg", prior_games_played = "games"
  )

week1 <- projected |>
  dplyr::filter(.data$week == 1) |>
  dplyr::inner_join(
    actuals |> dplyr::filter(.data$week == 1) |>
      dplyr::select("season", "player_id", "ppr_points_actual", "prior_games"),
    by = c("season", "player_id")
  ) |>
  dplyr::left_join(prior_season, by = c("season", "player_id")) |>
  dplyr::left_join(
    season_totals |> dplyr::select("season", "player_id", "games",
                                   "season_ppr", "season_ppg"),
    by = c("season", "player_id")
  ) |>
  dplyr::filter(!is.na(.data$season_ppg))

readr::write_csv(week1, "outputs/preseason_week1_accuracy.csv")

cat("=== Sample ===\n")
cat("Week 1 player projections graded, 2023-2025:", nrow(week1), "\n")
print(as.data.frame(week1 |> dplyr::count(.data$season, name = "players")))
cat("With a prior season on record:", sum(!is.na(week1$prior_ppg)), "\n\n")

metric_block <- function(d, label) {
  d <- dplyr::filter(d, !is.na(.data$prior_ppg))
  if (nrow(d) < 20) return(tibble::tibble())
  tibble::tibble(
    cut = label,
    players = nrow(d),
    # Accuracy on the week actually projected
    model_wk1_mae = mean(abs(d$projected_ppr - d$ppr_points_actual)),
    prior_wk1_mae = mean(abs(d$prior_ppg - d$ppr_points_actual)),
    # The draft-relevant question: does it rank the season correctly
    model_season_r = stats::cor(d$projected_ppr, d$season_ppg),
    prior_season_r = stats::cor(d$prior_ppg, d$season_ppg),
    model_season_rho = stats::cor(d$projected_ppr, d$season_ppg,
                                  method = "spearman"),
    prior_season_rho = stats::cor(d$prior_ppg, d$season_ppg,
                                  method = "spearman")
  )
}

cat("=== Week 1 projection accuracy and season-rank power ===\n")
print(as.data.frame(dplyr::bind_rows(
  metric_block(week1, "all positions"),
  purrr::map_dfr(c("QB", "RB", "WR", "TE"), function(p) {
    metric_block(dplyr::filter(week1, .data$position == p), p)
  })
)), digits = 4)

cat("\n=== By season ===\n")
print(as.data.frame(
  purrr::map_dfr(sort(unique(week1$season)), function(s) {
    metric_block(dplyr::filter(week1, .data$season == s), as.character(s))
  })
), digits = 4)

# --------------------------------------------------------------------------
# Draft-shaped test: of the model's top N at a position, how many finished
# top N by season points per game?
# --------------------------------------------------------------------------

hit_rate <- function(d, n) {
  d <- d |> dplyr::filter(.data$games >= 8)
  if (nrow(d) < n) return(NA_real_)
  model_top <- d |> dplyr::slice_max(.data$projected_ppr, n = n) |>
    dplyr::pull(.data$player_id)
  prior_top <- d |> dplyr::filter(!is.na(.data$prior_ppg)) |>
    dplyr::slice_max(.data$prior_ppg, n = n) |> dplyr::pull(.data$player_id)
  actual_top <- d |> dplyr::slice_max(.data$season_ppg, n = n) |>
    dplyr::pull(.data$player_id)
  c(model = length(intersect(model_top, actual_top)) / n,
    prior = length(intersect(prior_top, actual_top)) / n)
}

cat("\n=== Top-N hit rate: preseason pick vs season finish (8+ games) ===\n")
grid <- tidyr::expand_grid(
  season = sort(unique(week1$season)),
  position = c("QB", "RB", "WR", "TE"),
  n = c(12L, 24L)
)
hits <- purrr::pmap_dfr(grid, function(season, position, n) {
  d <- week1 |>
    dplyr::filter(.data$season == !!season, .data$position == !!position)
  h <- hit_rate(d, n)
  if (all(is.na(h))) return(tibble::tibble())
  tibble::tibble(
    season = season, position = position, top_n = n,
    model_hit = h[["model"]], prior_hit = h[["prior"]]
  )
})
print(as.data.frame(
  hits |>
    dplyr::group_by(.data$position, .data$top_n) |>
    dplyr::summarise(
      seasons = dplyr::n(),
      model_hit = mean(.data$model_hit),
      prior_season_hit = mean(.data$prior_hit),
      .groups = "drop"
    )
), digits = 3)
readr::write_csv(hits, "outputs/preseason_top_n_hit_rate.csv")

# --------------------------------------------------------------------------
# Availability: the single biggest driver of season value, and one the model
# does not model at all.
# --------------------------------------------------------------------------

cat("\n=== Games played vs season total, by preseason projection tier ===\n")
print(as.data.frame(
  week1 |>
    dplyr::filter(.data$position %in% c("QB", "RB", "WR", "TE")) |>
    dplyr::mutate(
      tier = dplyr::ntile(-.data$projected_ppr, 4),
      tier = paste0("Q", .data$tier)
    ) |>
    dplyr::group_by(.data$tier) |>
    dplyr::summarise(
      players = dplyr::n(),
      mean_projected_wk1 = mean(.data$projected_ppr),
      mean_games = mean(.data$games),
      pct_played_15plus = mean(.data$games >= 15),
      mean_season_ppg = mean(.data$season_ppg),
      mean_season_total = mean(.data$season_ppr),
      .groups = "drop"
    )
), digits = 4)

cat("\nCorrelation of Week 1 projection with:\n")
w <- week1 |> dplyr::filter(!is.na(.data$prior_ppg))
cat("  season points per game :", round(stats::cor(w$projected_ppr, w$season_ppg), 4), "\n")
cat("  season TOTAL points    :", round(stats::cor(w$projected_ppr, w$season_ppr), 4), "\n")
cat("  games played           :", round(stats::cor(w$projected_ppr, w$games), 4), "\n")
