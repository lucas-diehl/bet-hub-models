source("R/utilities.R")
source("R/touchdown_features.R")
source("R/dashboard_feed.R")
assert_packages()
ensure_directories()

# How much of the multi-book edge comes back as realistically-available books
# are added to the feed's two.
#
# The earlier finding was that most of the "best of eight" advantage on spreads
# came from LowVig.ag and other offshore shops. Caesars and BetMGM are regulated
# US books available in most states, so adding them is a decision the user can
# actually act on, unlike benchmarking against an offshore reduced-juice book.

universes <- list(
  "DK/FD"                 = c("draftkings", "fanduel"),
  "+ Caesars"             = c("draftkings", "fanduel", "williamhill_us"),
  "+ BetMGM"              = c("draftkings", "fanduel", "betmgm"),
  "+ Caesars + BetMGM"    = c("draftkings", "fanduel", "williamhill_us", "betmgm"),
  "4 above + BetRivers"   = c("draftkings", "fanduel", "williamhill_us", "betmgm",
                              "betrivers"),
  "+ Fanatics (6 US)"     = c("draftkings", "fanduel", "williamhill_us", "betmgm",
                              "betrivers", "fanatics"),
  "all books"             = NULL
)

summarise_bets <- function(d) {
  tibble::tibble(
    bets = nrow(d),
    win_rate = if (sum(d$result != 0)) sum(d$result > 0) / sum(d$result != 0) else NA_real_,
    mean_price = mean(d$price),
    roi = sum(d$profit) / nrow(d)
  )
}

# --------------------------------------------------------------------------
# Yardage and reception props
# --------------------------------------------------------------------------

raw_props <- readr::read_csv(
  "data/processed/player_prop_odds.csv", show_col_types = FALSE
) |>
  dplyr::mutate(
    target = dplyr::case_when(
      .data$market_key == "player_reception_yds" ~ "receiving_yards",
      .data$market_key == "player_receptions" ~ "receptions",
      .data$market_key == "player_rush_yds" ~ "rushing_yards"
    ),
    side = tolower(.data$side),
    bookmaker_key = tolower(.data$bookmaker_key)
  ) |>
  dplyr::filter(!is.na(.data$target), !is.na(.data$american_odds))

prop_model <- readr::read_csv(
  "outputs/player_prop_upgraded_bets.csv", show_col_types = FALSE
) |>
  dplyr::filter(.data$arm == "full") |>
  # season is dropped here: the raw odds table already carries it, and keeping
  # both sides would collide into season.x/season.y during the join.
  dplyr::select(
    "game_id", "target", "player", "player_id", "side",
    "consensus_line", "p_over", "outcome"
  )

positions <- readRDS("data/processed/fantasy_prop_features_augmented.rds") |>
  dplyr::select("game_id", "player_id", "position") |>
  dplyr::distinct()

prop_universe <- function(keys, label) {
  d <- raw_props
  if (!is.null(keys)) d <- dplyr::filter(d, .data$bookmaker_key %in% keys)
  d |>
    dplyr::inner_join(
      prop_model, by = c("game_id", "target", "player", "side")
    ) |>
    dplyr::filter(.data$line == .data$consensus_line) |>
    dplyr::group_by(
      .data$game_id, .data$season, .data$target, .data$player,
      .data$player_id, .data$side, .data$consensus_line, .data$p_over,
      .data$outcome
    ) |>
    dplyr::summarise(price = max(.data$american_odds), .groups = "drop") |>
    dplyr::mutate(
      universe = label,
      p_side = dplyr::if_else(.data$side == "over", .data$p_over, 1 - .data$p_over),
      ev = .data$p_side * american_payout(.data$price) - (1 - .data$p_side),
      result = dplyr::case_when(
        .data$outcome == "push" ~ 0, .data$outcome == .data$side ~ 1, TRUE ~ -1
      ),
      profit = dplyr::case_when(
        .data$result > 0 ~ american_payout(.data$price),
        .data$result < 0 ~ -1, TRUE ~ 0
      )
    )
}

props <- purrr::imap_dfr(universes, function(keys, label) prop_universe(keys, label)) |>
  dplyr::left_join(positions, by = c("game_id", "player_id")) |>
  dplyr::mutate(universe = factor(.data$universe, levels = names(universes)))

readr::write_csv(props, "outputs/book_universe_props.csv")

cat("=== Props: all positive-EV bets, pooled 2024-25 ===\n")
print(as.data.frame(
  props |> dplyr::filter(.data$ev > 0) |>
    dplyr::group_by(.data$universe) |>
    dplyr::group_modify(~ summarise_bets(.x)) |> dplyr::ungroup()
), digits = 4)

cat("\n=== Props: EV >= 0.18 rule, by season ===\n")
print(as.data.frame(
  props |> dplyr::filter(.data$ev >= 0.18) |>
    dplyr::group_by(.data$universe, .data$season) |>
    dplyr::group_modify(~ summarise_bets(.x)) |> dplyr::ungroup() |>
    tidyr::pivot_wider(
      id_cols = "universe", names_from = "season",
      values_from = c("bets", "roi")
    )
), digits = 4)

cat("\n=== Props: tight ends only, positive EV, pooled ===\n")
print(as.data.frame(
  props |> dplyr::filter(.data$ev > 0, .data$position == "TE") |>
    dplyr::group_by(.data$universe) |>
    dplyr::group_modify(~ summarise_bets(.x)) |> dplyr::ungroup()
), digits = 4)

cat("\n=== Props: tight-end unders, positive EV, pooled ===\n")
print(as.data.frame(
  props |>
    dplyr::filter(.data$ev > 0, .data$position == "TE", .data$side == "under") |>
    dplyr::group_by(.data$universe) |>
    dplyr::group_modify(~ summarise_bets(.x)) |> dplyr::ungroup()
), digits = 4)

# --------------------------------------------------------------------------
# Anytime touchdown core tier
# --------------------------------------------------------------------------

raw_td <- readr::read_csv(
  "data/processed/anytime_td_odds_early_2023_2025.csv", show_col_types = FALSE
) |>
  dplyr::filter(tolower(.data$outcome) == "yes") |>
  dplyr::mutate(
    bookmaker_key = tolower(.data$bookmaker_key),
    name_key = normalize_prop_player_name(.data$player)
  )

td_model <- readr::read_csv(
  "outputs/td_walk_forward_predictions.csv", show_col_types = FALSE
) |>
  dplyr::mutate(name_key = normalize_prop_player_name(.data$player)) |>
  dplyr::select(
    "season", "game_id", "name_key", "position", "model_probability",
    "total_line", "anytime_td"
  )

td_universe <- function(keys, label) {
  d <- raw_td
  if (!is.null(keys)) d <- dplyr::filter(d, .data$bookmaker_key %in% keys)
  d |>
    dplyr::group_by(.data$game_id, .data$name_key) |>
    dplyr::summarise(price = max(.data$american_odds), .groups = "drop") |>
    dplyr::inner_join(td_model, by = c("game_id", "name_key")) |>
    dplyr::mutate(
      universe = label,
      edge = .data$model_probability / american_to_prob(.data$price) - 1,
      result = dplyr::if_else(.data$anytime_td > 0, 1, -1),
      profit = dplyr::if_else(
        .data$anytime_td > 0, american_payout(.data$price), -1
      )
    ) |>
    dplyr::filter(.data$total_line <= 42 | .data$position == "TE",
                  .data$edge >= 0.05)
}

td <- purrr::imap_dfr(universes, function(keys, label) td_universe(keys, label)) |>
  dplyr::mutate(universe = factor(.data$universe, levels = names(universes)))
readr::write_csv(td, "outputs/book_universe_td.csv")

cat("\n=== Anytime TD core tier, pooled 2023-25 ===\n")
print(as.data.frame(
  td |> dplyr::group_by(.data$universe) |>
    dplyr::group_modify(~ summarise_bets(.x)) |> dplyr::ungroup()
), digits = 4)

cat("\n=== Anytime TD core tier, by season ===\n")
print(as.data.frame(
  td |> dplyr::group_by(.data$universe, .data$season) |>
    dplyr::group_modify(~ summarise_bets(.x)) |> dplyr::ungroup() |>
    tidyr::pivot_wider(
      id_cols = "universe", names_from = "season",
      values_from = c("bets", "roi")
    )
), digits = 4)
