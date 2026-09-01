source("R/utilities.R")
source("R/odds_api.R")
source("R/touchdown_features.R")
source("R/fantasy_prop_model.R")
assert_packages()
ensure_directories()

# Grades the fantasy component projections against real over/under player prop
# lines pulled by scripts/29.
#
# The projections used here are the walk-forward ones: each season was predicted
# by a model trained only on earlier seasons, so joining them to market lines is
# a genuine out-of-sample test. The edge threshold is then selected on 2024 and
# applied unchanged to 2025, so the reported 2025 figure is not tuned on itself.

odds_path <- "data/processed/player_prop_odds.csv"
if (!file.exists(odds_path)) {
  stop("Missing ", odds_path, ". Run scripts/29 first.", call. = FALSE)
}

market_to_target <- c(
  player_reception_yds = "receiving_yards",
  player_receptions = "receptions",
  player_rush_yds = "rushing_yards",
  player_pass_yds = "passing_yards"
)

odds <- readr::read_csv(odds_path, show_col_types = FALSE) |>
  dplyr::filter(.data$market_key %in% names(market_to_target)) |>
  dplyr::mutate(
    target = unname(market_to_target[.data$market_key]),
    side = stringr::str_to_lower(.data$side),
    prop_name_key = normalize_prop_player_name(.data$player)
  ) |>
  dplyr::filter(.data$side %in% c("over", "under"), is.finite(.data$line))

# Consensus line first: books disagree, and quoting a model edge against one
# book's outlier number would manufacture edge that is really line disagreement.
consensus <- odds |>
  dplyr::group_by(
    .data$game_id, .data$season, .data$week, .data$target, .data$prop_name_key
  ) |>
  dplyr::summarise(
    consensus_line = stats::median(.data$line, na.rm = TRUE),
    books = dplyr::n_distinct(.data$bookmaker_key),
    player = dplyr::first(.data$player),
    .groups = "drop"
  ) |>
  dplyr::filter(.data$books >= 2)

# Best price available at the consensus line, per side.
best_price <- odds |>
  dplyr::inner_join(
    consensus,
    by = c("game_id", "season", "week", "target", "prop_name_key")
  ) |>
  dplyr::filter(.data$line == .data$consensus_line) |>
  dplyr::group_by(
    .data$game_id, .data$season, .data$week, .data$target,
    .data$prop_name_key, .data$side
  ) |>
  dplyr::arrange(dplyr::desc(.data$american_odds), .by_group = TRUE) |>
  dplyr::summarise(
    american_odds = dplyr::first(.data$american_odds),
    bookmaker = dplyr::first(.data$bookmaker),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = "side",
    values_from = c("american_odds", "bookmaker")
  )

board <- consensus |>
  dplyr::inner_join(
    best_price,
    by = c("game_id", "season", "week", "target", "prop_name_key")
  ) |>
  dplyr::filter(
    is.finite(.data$american_odds_over),
    is.finite(.data$american_odds_under)
  )

# Attach nflverse player IDs using the same alias set the touchdown board uses.
walk_forward <- readRDS("data/processed/fantasy_prop_walk_forward.rds")
predictions <- walk_forward$predictions |>
  dplyr::filter(.data$season %in% 2024:2025)

features <- readRDS("data/processed/fantasy_prop_features.rds") |>
  dplyr::select(
    "season", "player_id", "player_display_name", "position"
  ) |>
  dplyr::distinct()

directory <- readRDS("data/raw/players.rds")
aliases <- directory |>
  dplyr::transmute(
    player_id = .data$gsis_id,
    alias = purrr::pmap(
      list(
        .data$display_name, .data$first_name, .data$common_first_name,
        .data$football_name, .data$last_name, .data$short_name
      ),
      function(display, first, common, football, last, short) {
        unique(stats::na.omit(c(
          display, paste(first, last), paste(common, last),
          paste(football, last), short
        )))
      }
    )
  ) |>
  tidyr::unnest_longer(.data$alias) |>
  dplyr::transmute(
    .data$player_id,
    prop_name_key = normalize_prop_player_name(.data$alias)
  ) |>
  dplyr::filter(nzchar(.data$prop_name_key), !is.na(.data$player_id)) |>
  dplyr::distinct()

base_keys <- features |>
  dplyr::transmute(
    .data$season, .data$player_id,
    prop_name_key = normalize_prop_player_name(.data$player_display_name)
  ) |>
  dplyr::distinct()

name_map <- dplyr::bind_rows(
  base_keys,
  base_keys |>
    dplyr::select("season", "player_id") |>
    dplyr::inner_join(aliases, by = "player_id")
) |>
  dplyr::distinct() |>
  dplyr::add_count(.data$season, .data$prop_name_key, name = "collisions") |>
  dplyr::filter(.data$collisions == 1L) |>
  dplyr::select("season", "prop_name_key", "player_id")

graded <- board |>
  dplyr::inner_join(name_map, by = c("season", "prop_name_key")) |>
  dplyr::inner_join(
    predictions |>
      dplyr::select("game_id", "player_id", "target", "actual", "prediction"),
    by = c("game_id", "player_id", "target")
  ) |>
  dplyr::mutate(
    edge = .data$prediction - .data$consensus_line,
    bet_side = dplyr::if_else(.data$edge > 0, "over", "under"),
    price = dplyr::if_else(
      .data$bet_side == "over",
      .data$american_odds_over,
      .data$american_odds_under
    ),
    outcome = dplyr::case_when(
      .data$actual > .data$consensus_line ~ "over",
      .data$actual < .data$consensus_line ~ "under",
      TRUE ~ "push"
    ),
    result = dplyr::case_when(
      .data$outcome == "push" ~ 0,
      .data$outcome == .data$bet_side ~ 1,
      TRUE ~ -1
    ),
    profit = dplyr::case_when(
      .data$result > 0 & .data$price > 0 ~ .data$price / 100,
      .data$result > 0 ~ 100 / abs(.data$price),
      .data$result < 0 ~ -1,
      TRUE ~ 0
    )
  ) |>
  # A handful of players are quoted under two spellings in the same game, which
  # would otherwise stake the same player-game twice.
  dplyr::arrange(.data$game_id, .data$player_id, .data$target) |>
  dplyr::distinct(
    .data$game_id, .data$player_id, .data$target,
    .keep_all = TRUE
  )

readr::write_csv(graded, "outputs/player_prop_graded_bets.csv")

summarise_bets <- function(d) {
  decisions <- sum(d$result != 0)
  tibble::tibble(
    bets = nrow(d),
    wins = sum(d$result > 0),
    losses = sum(d$result < 0),
    pushes = sum(d$result == 0),
    win_rate = if (decisions) sum(d$result > 0) / decisions else NA_real_,
    profit_units = sum(d$profit),
    roi = if (nrow(d)) sum(d$profit) / nrow(d) else NA_real_
  )
}

# Edge grids differ by market because the units differ: receptions are counts,
# yards are tens of yards.
edge_grid <- list(
  receptions = seq(0, 2, by = 0.25),
  receiving_yards = seq(0, 30, by = 2.5),
  rushing_yards = seq(0, 30, by = 2.5),
  passing_yards = seq(0, 60, by = 5)
)

curves <- purrr::imap_dfr(edge_grid, function(grid, target_name) {
  purrr::map_dfr(grid, function(threshold) {
    d <- graded |>
      dplyr::filter(
        .data$target == target_name, abs(.data$edge) >= threshold
      )
    if (!nrow(d)) return(tibble::tibble())
    purrr::map_dfr(c(2024, 2025), function(s) {
      dd <- dplyr::filter(d, .data$season == s)
      if (!nrow(dd)) return(tibble::tibble())
      dplyr::bind_cols(
        tibble::tibble(target = target_name, threshold = threshold, season = s),
        summarise_bets(dd)
      )
    })
  })
})
readr::write_csv(curves, "outputs/player_prop_edge_curves.csv")

# Select on 2024 only, then apply unchanged to 2025.
minimum_bets <- 40L
selected <- curves |>
  dplyr::filter(.data$season == 2024, .data$bets >= minimum_bets) |>
  dplyr::group_by(.data$target) |>
  dplyr::slice_max(.data$roi, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(
    "target", "threshold",
    selection_bets = "bets", selection_roi = "roi"
  )

forward <- selected |>
  dplyr::inner_join(
    curves |> dplyr::filter(.data$season == 2025),
    by = c("target", "threshold")
  )
readr::write_csv(forward, "outputs/player_prop_forward_test.csv")

cat("\n=== Coverage ===\n")
cat("Prop rows loaded:", nrow(odds), "\n")
cat("Player-game-markets with 2+ books:", nrow(consensus), "\n")
cat("With prices both sides:", nrow(board), "\n")
cat("Joined to a walk-forward projection:", nrow(graded), "\n")
cat("Name-match rate:", round(nrow(graded) / nrow(board), 4), "\n")

cat("\n=== All qualifying bets, zero edge filter ===\n")
print(as.data.frame(
  graded |>
    dplyr::group_by(.data$target, .data$season) |>
    dplyr::group_modify(~ summarise_bets(.x)) |>
    dplyr::ungroup()
), digits = 4)

cat("\n=== Threshold selected on 2024, applied to 2025 ===\n")
if (nrow(forward)) {
  print(as.data.frame(forward), digits = 4)
} else {
  cat("No market reached the minimum bet count in 2024.\n")
}

# Why the model loses while winning more than half its decisions: it is quoting
# a conditional mean against a line that books set nearer the median of a
# right-skewed distribution, so "over" triggers too often and pays too little.
cat("\n=== Diagnosis: price paid vs win rate needed ===\n")
print(as.data.frame(
  graded |>
    dplyr::mutate(
      breakeven = dplyr::if_else(
        .data$price < 0,
        abs(.data$price) / (abs(.data$price) + 100),
        100 / (.data$price + 100)
      )
    ) |>
    dplyr::group_by(.data$target) |>
    dplyr::summarise(
      bets = dplyr::n(),
      median_price = stats::median(.data$price),
      breakeven_needed = mean(.data$breakeven),
      actual_win_rate = mean(.data$result > 0),
      .groups = "drop"
    )
), digits = 4)

cat("\n=== Discovery split: model overs vs unders (not validated) ===\n")
print(as.data.frame(
  graded |>
    dplyr::group_by(.data$target, .data$bet_side) |>
    dplyr::group_modify(~ summarise_bets(.x)) |>
    dplyr::ungroup()
), digits = 4)

# Same walk-forward discipline applied to unders only: selected on 2024, tested
# on 2025. This is a second look at the same data, so treat it as a candidate.
under_curves <- purrr::imap_dfr(edge_grid, function(grid, target_name) {
  purrr::map_dfr(grid, function(threshold) {
    purrr::map_dfr(c(2024, 2025), function(s) {
      d <- graded |>
        dplyr::filter(
          .data$target == target_name, .data$bet_side == "under",
          abs(.data$edge) >= threshold, .data$season == s
        )
      if (!nrow(d)) return(tibble::tibble())
      dplyr::bind_cols(
        tibble::tibble(target = target_name, threshold = threshold, season = s),
        summarise_bets(d)
      )
    })
  })
})
under_forward <- under_curves |>
  dplyr::filter(.data$season == 2024, .data$bets >= minimum_bets) |>
  dplyr::group_by(.data$target) |>
  dplyr::slice_max(.data$roi, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(
    "target", "threshold",
    selection_bets = "bets", selection_roi = "roi"
  ) |>
  dplyr::inner_join(
    under_curves |> dplyr::filter(.data$season == 2025),
    by = c("target", "threshold")
  )
readr::write_csv(under_forward, "outputs/player_prop_under_forward_test.csv")

cat("\n=== Unders only: selected on 2024, applied to 2025 ===\n")
if (nrow(under_forward)) {
  print(as.data.frame(under_forward), digits = 4)
} else {
  cat("No market reached the minimum bet count in 2024.\n")
}
