source("R/utilities.R")
source("R/backtest.R")
assert_packages()
ensure_directories()
cfg <- read_config()

predictions <- readr::read_csv("outputs/predictions.csv", show_col_types = FALSE)

add_percentage_edge <- function(data) {
  data |>
    dplyr::mutate(
      market_prediction = dplyr::if_else(
        .data$target == "home_margin", -.data$home_line, .data$total_line
      ),
      edge = .data$prediction - .data$market_prediction,
      edge_denominator = dplyr::if_else(
        .data$target == "game_total",
        .data$total_line,
        pmax(abs(.data$market_prediction), 3)
      ),
      edge_pct = 100 * .data$edge / .data$edge_denominator
    )
}

grade_percentage_edges <- function(data, threshold_pct, american_odds = -110) {
  add_percentage_edge(data) |>
    dplyr::filter(.data$model != "sportsbook") |>
    dplyr::mutate(
      bet_side = dplyr::case_when(
        abs(.data$edge_pct) < threshold_pct ~ "pass",
        .data$target == "home_margin" & .data$edge > 0 ~ "home",
        .data$target == "home_margin" ~ "away",
        .data$edge > 0 ~ "over",
        TRUE ~ "under"
      ),
      raw_result = dplyr::case_when(
        .data$target == "home_margin" ~ .data$truth + .data$home_line,
        TRUE ~ .data$truth - .data$total_line
      ),
      bet_result = dplyr::case_when(
        .data$bet_side == "pass" ~ NA_real_,
        .data$bet_side %in% c("home", "over") ~ signed_result(.data$raw_result),
        TRUE ~ -signed_result(.data$raw_result)
      ),
      profit = american_profit(.data$bet_result, american_odds)
    )
}

percentage_thresholds <- c(
  0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 15, 20,
  25, 30, 40, 50, 75, 100, 150, 200
)

percentage_threshold_table <- function(data) {
  purrr::map_dfr(percentage_thresholds, function(threshold_pct) {
    grade_percentage_edges(data, threshold_pct, cfg$backtest$american_odds) |>
      dplyr::filter(.data$bet_side != "pass") |>
      dplyr::group_by(.data$target, .data$model) |>
      dplyr::summarise(
        threshold_pct = threshold_pct,
        bets = dplyr::n(),
        wins = sum(.data$bet_result > 0),
        losses = sum(.data$bet_result < 0),
        pushes = sum(.data$bet_result == 0),
        win_rate = .data$wins / (.data$wins + .data$losses),
        profit_units = sum(.data$profit),
        roi = .data$profit_units / .data$bets,
        .groups = "drop"
      )
  })
}

percentage_walk_forward_bets <- function(data) {
  seasons <- sort(unique(data$season))
  models <- setdiff(unique(data$model), "sportsbook")
  targets <- unique(data$target)

  purrr::map_dfr(seasons, function(test_season) {
    validation_years <- tail(
      seasons[seasons < test_season],
      cfg$backtest$validation_seasons
    )
    if (length(validation_years) < cfg$backtest$validation_seasons) {
      return(dplyr::tibble())
    }

    purrr::map_dfr(targets, function(target_name) {
      purrr::map_dfr(models, function(model_name) {
        validation <- data |>
          dplyr::filter(
            .data$season %in% validation_years,
            .data$target == target_name,
            .data$model == model_name
          )
        candidates <- percentage_threshold_table(validation) |>
          dplyr::filter(.data$bets >= cfg$backtest$minimum_bets) |>
          dplyr::arrange(dplyr::desc(.data$roi), dplyr::desc(.data$bets))
        if (!nrow(candidates)) return(dplyr::tibble())
        chosen <- candidates[1, ]

        data |>
          dplyr::filter(
            .data$season == test_season,
            .data$target == target_name,
            .data$model == model_name
          ) |>
          grade_percentage_edges(
            chosen$threshold_pct,
            cfg$backtest$american_odds
          ) |>
          dplyr::filter(.data$bet_side != "pass") |>
          dplyr::mutate(
            selected_threshold_pct = chosen$threshold_pct,
            validation_start = min(validation_years),
            validation_end = max(validation_years),
            validation_bets = chosen$bets,
            validation_roi = chosen$roi
          )
      })
    })
  })
}

summarize_bets <- function(data, ...) {
  data |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      bets = dplyr::n(),
      wins = sum(.data$bet_result > 0),
      losses = sum(.data$bet_result < 0),
      pushes = sum(.data$bet_result == 0),
      win_rate = .data$wins / (.data$wins + .data$losses),
      profit_units = sum(.data$profit),
      roi = .data$profit_units / .data$bets,
      .groups = "drop"
    )
}

pct_bets <- percentage_walk_forward_bets(predictions)
pct_summary <- summarize_bets(pct_bets, .data$target, .data$model)
pct_by_season <- summarize_bets(
  pct_bets,
  .data$season, .data$target, .data$model, .data$selected_threshold_pct
)
pct_bankroll <- bankroll_analysis(pct_bets, cfg)

interaction_data <- pct_bets |>
  dplyr::mutate(
    favorite_strength = cut(
      abs(.data$home_line),
      breaks = c(-Inf, 2.5, 6.5, Inf),
      labels = c("small_0_to_2.5", "medium_3_to_6.5", "heavy_7_plus")
    ),
    favorite_location = dplyr::case_when(
      .data$home_line < 0 ~ "home_favorite",
      .data$home_line > 0 ~ "away_favorite",
      TRUE ~ "pickem"
    ),
    total_band = cut(
      .data$total_line,
      breaks = c(-Inf, 41.5, 48, Inf),
      labels = c("low_41.5_or_less", "medium_42_to_48", "high_over_48")
    ),
    period = cut(
      .data$week,
      breaks = c(0, 4, 9, 14, 18),
      labels = c("weeks_1_4", "weeks_5_9", "weeks_10_14", "weeks_15_18")
    ),
    evaluation = dplyr::if_else(
      .data$season <= 2022, "discovery_2020_2022", "validation_2023_2025"
    )
  )

interaction_cells <- summarize_bets(
  interaction_data,
  .data$evaluation, .data$target, .data$model, .data$bet_side,
  .data$favorite_strength, .data$favorite_location, .data$total_band
)

example_interaction <- interaction_data |>
  dplyr::filter(
    .data$target == "game_total",
    .data$model == "forward_linear",
    .data$bet_side == "over",
    .data$favorite_strength == "heavy_7_plus"
  ) |>
  summarize_bets(.data$evaluation)

simple_interactions <- summarize_bets(
  interaction_data,
  .data$evaluation, .data$target, .data$model,
  .data$bet_side, .data$favorite_strength
)

# Rank discovery cells with a meaningful sample, then carry the exact same rule
# into validation. No validation statistic participates in rule selection.
discovery_rank <- interaction_cells |>
  dplyr::filter(
    .data$evaluation == "discovery_2020_2022",
    .data$bets >= 20
  ) |>
  dplyr::arrange(dplyr::desc(.data$roi))

validation_of_discovery <- discovery_rank |>
  dplyr::select(
    "target", "model", "bet_side", "favorite_strength",
    "favorite_location", "total_band",
    discovery_bets = "bets", discovery_win_rate = "win_rate",
    discovery_roi = "roi"
  ) |>
  dplyr::left_join(
    interaction_cells |>
      dplyr::filter(.data$evaluation == "validation_2023_2025") |>
      dplyr::select(
        "target", "model", "bet_side", "favorite_strength",
        "favorite_location", "total_band",
        validation_bets = "bets", validation_win_rate = "win_rate",
        validation_roi = "roi"
      ),
    by = c(
      "target", "model", "bet_side", "favorite_strength",
      "favorite_location", "total_band"
    )
  )

readr::write_csv(pct_bets, "outputs/percentage_edge_bets.csv")
readr::write_csv(pct_summary, "outputs/percentage_edge_summary.csv")
readr::write_csv(pct_by_season, "outputs/percentage_edge_by_season.csv")
readr::write_csv(pct_bankroll$summary, "outputs/percentage_edge_bankroll.csv")
readr::write_csv(interaction_cells, "outputs/interaction_cells.csv")
readr::write_csv(simple_interactions, "outputs/simple_interactions.csv")
readr::write_csv(example_interaction, "outputs/example_over_heavy_favorite.csv")
readr::write_csv(
  validation_of_discovery,
  "outputs/interaction_discovery_validation.csv"
)
message("Percentage-edge and interaction analysis written to outputs/.")
