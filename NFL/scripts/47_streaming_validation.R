source("R/utilities.R")
source("R/fantasy_prop_model.R")
assert_packages()
ensure_directories()

# Is the weekly model useful for streaming a position?
#
# The decision being tested is the real one: it is Thursday, the starter is on
# bye or hurt, and a replacement must be picked from what is plausibly available.
# That is not the same as ranking all players — the top of the position is
# rostered. So the candidate pool each week is restricted to players outside the
# top N at the position by season-to-date points per game entering that week,
# which approximates a waiver wire.
#
# The model's top pick from that pool is compared against what a manager would
# otherwise do: take the best season-to-date scorer available, take the hottest
# recent form, or pick at random.

ppr_weights <- c(
  passing_yards = 0.04, passing_tds = 4, interceptions = -2,
  rushing_yards = 0.10, rushing_tds = 6, receptions = 1,
  receiving_yards = 0.10, receiving_tds = 6, fumbles_lost = -2
)

features <- readRDS("data/processed/fantasy_prop_features_augmented.rds")
walk_forward <- readRDS("data/processed/fantasy_prop_walk_forward.rds")$predictions

projected <- walk_forward |>
  dplyr::filter(.data$target %in% names(ppr_weights)) |>
  dplyr::mutate(weight = unname(ppr_weights[.data$target])) |>
  dplyr::group_by(
    .data$season, .data$week, .data$player_id, .data$position
  ) |>
  dplyr::summarise(
    projected_ppr = sum(.data$prediction * .data$weight), .groups = "drop"
  )

actual <- features |>
  dplyr::select(
    "season", "week", "player_id", "position", "team", "ppr_points_actual",
    "player_display_name"
  )

pool <- projected |>
  dplyr::inner_join(
    actual, by = c("season", "week", "player_id", "position")
  ) |>
  dplyr::filter(!is.na(.data$ppr_points_actual))

# Season-to-date and recent form entering each week, both strictly lagged.
form <- actual |>
  dplyr::arrange(.data$player_id, .data$season, .data$week) |>
  dplyr::group_by(.data$player_id, .data$season) |>
  dplyr::mutate(
    std_ppg = dplyr::lag(cumsum(.data$ppr_points_actual) / dplyr::row_number()),
    recent3 = slider::slide_dbl(
      dplyr::lag(.data$ppr_points_actual), mean,
      .before = 2, .complete = FALSE, na.rm = TRUE
    ),
    games_so_far = dplyr::row_number() - 1L
  ) |>
  dplyr::ungroup() |>
  dplyr::select("season", "week", "player_id", "std_ppg", "recent3",
                "games_so_far")

board <- pool |>
  dplyr::inner_join(form, by = c("season", "week", "player_id")) |>
  dplyr::filter(.data$games_so_far >= 2, !is.na(.data$std_ppg))

# Roster depth by position: how many at each spot are realistically owned in a
# 12-team league before you reach the wire.
rostered_depth <- c(QB = 14L, TE = 14L, RB = 40L, WR = 48L)

set.seed(20260812L)
streaming <- purrr::map_dfr(names(rostered_depth), function(pos) {
  depth <- rostered_depth[[pos]]
  board |>
    dplyr::filter(.data$position == pos) |>
    dplyr::group_by(.data$season, .data$week) |>
    dplyr::filter(dplyr::n() > depth + 5) |>
    dplyr::mutate(owned_rank = dplyr::min_rank(dplyr::desc(.data$std_ppg))) |>
    # The waiver pool: everyone below the rostered tier.
    dplyr::filter(.data$owned_rank > depth) |>
    dplyr::filter(dplyr::n() >= 5) |>
    dplyr::summarise(
      candidates = dplyr::n(),
      model = .data$ppr_points_actual[which.max(.data$projected_ppr)],
      best_season = .data$ppr_points_actual[which.max(.data$std_ppg)],
      hot_hand = .data$ppr_points_actual[which.max(.data$recent3)],
      random = .data$ppr_points_actual[sample.int(dplyr::n(), 1)],
      pool_mean = mean(.data$ppr_points_actual),
      pool_best = max(.data$ppr_points_actual),
      model_pick = .data$player_display_name[which.max(.data$projected_ppr)],
      .groups = "drop"
    ) |>
    dplyr::mutate(position = pos)
})

readr::write_csv(streaming, "outputs/streaming_validation.csv")

cat("=== Streaming decisions evaluated ===\n")
print(as.data.frame(streaming |> dplyr::count(.data$position, name = "weeks")))

summarise_stream <- function(d) {
  tibble::tibble(
    weeks = nrow(d),
    model = mean(d$model),
    best_season_ppg = mean(d$best_season),
    hot_hand = mean(d$hot_hand),
    random = mean(d$random),
    pool_mean = mean(d$pool_mean),
    pool_best = mean(d$pool_best),
    beats_season = mean(d$model > d$best_season),
    beats_hot = mean(d$model > d$hot_hand),
    beats_random = mean(d$model > d$random)
  )
}

cat("\n=== Mean actual PPR of the streamed pick, by strategy ===\n")
print(as.data.frame(
  streaming |> dplyr::group_by(.data$position) |>
    dplyr::group_modify(~ summarise_stream(.x)) |> dplyr::ungroup()
), digits = 4)

cat("\n=== Same, by season ===\n")
print(as.data.frame(
  streaming |> dplyr::group_by(.data$position, .data$season) |>
    dplyr::group_modify(~ summarise_stream(.x)) |> dplyr::ungroup() |>
    dplyr::select("position", "season", "weeks", "model", "best_season_ppg",
                  "hot_hand", "beats_season")
), digits = 4)

# Paired bootstrap on the per-week difference against the strongest rival.
set.seed(20260812L)
cat("\n=== Model minus best-available-by-season-PPG, paired bootstrap ===\n")
print(as.data.frame(
  streaming |>
    dplyr::group_by(.data$position) |>
    dplyr::group_modify(function(d, key) {
      delta <- d$model - d$best_season
      draws <- replicate(2000, mean(sample(delta, length(delta), replace = TRUE)))
      tibble::tibble(
        weeks = nrow(d),
        mean_gain_ppr = mean(delta),
        ci_low = stats::quantile(draws, 0.025),
        ci_high = stats::quantile(draws, 0.975),
        p_model_better = mean(draws > 0)
      )
    }) |>
    dplyr::ungroup()
), digits = 4)
