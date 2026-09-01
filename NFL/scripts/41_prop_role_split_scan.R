source("R/utilities.R")
assert_packages()
ensure_directories()

# Role-based split scan over the upgraded yardage and reception prop bets.
#
# The earlier 320-segment scan had no depth-chart or passer dimensions and ran
# on the pre-upgrade model, so these splits are genuinely untested. The
# discipline is unchanged and deliberately unforgiving: every cell is measured
# on 2024 and then on 2025, and the summary compares how many discovery-positive
# cells survived against the roughly half that survive by coin flip. Individual
# winners mean nothing if the survival rate matches chance.
#
# Every split variable is lagged or rolling, so nothing here uses information
# unavailable before kickoff.

bets <- readr::read_csv(
  "outputs/player_prop_upgraded_bets.csv", show_col_types = FALSE
) |>
  dplyr::filter(.data$arm == "full", .data$ev > 0)

features <- readRDS("data/processed/fantasy_prop_features_augmented.rds")

# Depth-chart role, approximated by where a player's recent target share ranks
# among his own team's pass catchers that week. This is what "WR2 versus WR3"
# means operationally, and it is knowable pregame.
depth <- features |>
  dplyr::filter(.data$position %in% c("WR", "TE", "RB")) |>
  dplyr::group_by(.data$season, .data$week, .data$team) |>
  dplyr::mutate(
    target_rank = dplyr::min_rank(dplyr::desc(
      dplyr::coalesce(.data$target_share_r3, 0)
    ))
  ) |>
  dplyr::ungroup() |>
  dplyr::select("game_id", "player_id", "target_rank")

# Lagged passing volume of the best quarterback on the roster that week, as a
# proxy for who is throwing and how much.
passer <- features |>
  dplyr::filter(.data$position == "QB") |>
  dplyr::group_by(.data$season, .data$week, .data$team) |>
  dplyr::summarise(
    qb_attempts_r5 = max(dplyr::coalesce(.data$attempts_r5, 0)),
    qb_yards_r5 = max(dplyr::coalesce(.data$passing_yards_r5, 0)),
    .groups = "drop"
  )

context <- features |>
  dplyr::select(
    "game_id", "player_id", "season", "week", "team", "position",
    "is_home", "implied_team_total", "target_share_r3",
    "receiving_air_yards_r3", "targets_r3", "snap_pct_r3",
    "receiving_yards_sd5", "receptions_sd5",
    "opp_rec_yards_gained_diff_r3"
  ) |>
  dplyr::left_join(depth, by = c("game_id", "player_id")) |>
  dplyr::left_join(passer, by = c("season", "week", "team"))

scan_data <- bets |>
  dplyr::inner_join(
    context, by = c("game_id", "player_id", "season", "week")
  ) |>
  dplyr::mutate(
    position = dplyr::coalesce(.data$position, "UNK"),
    depth_role = dplyr::case_when(
      is.na(.data$target_rank) ~ "role unknown",
      .data$target_rank == 1 ~ "team target rank 1",
      .data$target_rank == 2 ~ "team target rank 2",
      .data$target_rank == 3 ~ "team target rank 3",
      TRUE ~ "team target rank 4+"
    ),
    adot = dplyr::if_else(
      dplyr::coalesce(.data$targets_r3, 0) > 0,
      .data$receiving_air_yards_r3 / .data$targets_r3, NA_real_
    ),
    adot_tier = dplyr::case_when(
      is.na(.data$adot) ~ "adot unknown",
      .data$adot < 7 ~ "short aDOT",
      .data$adot < 12 ~ "medium aDOT",
      TRUE ~ "deep aDOT"
    ),
    snap_tier = dplyr::case_when(
      is.na(.data$snap_pct_r3) ~ "snap unknown",
      .data$snap_pct_r3 < 0.5 ~ "snaps < 50%",
      .data$snap_pct_r3 < 0.75 ~ "snaps 50-75%",
      TRUE ~ "snaps 75%+"
    ),
    volatility_tier = dplyr::case_when(
      .data$target == "receptions" & is.na(.data$receptions_sd5) ~ "vol unknown",
      .data$target == "receptions" & .data$receptions_sd5 >= 2 ~ "high volatility",
      .data$target == "receptions" ~ "low volatility",
      is.na(.data$receiving_yards_sd5) ~ "vol unknown",
      .data$receiving_yards_sd5 >= 30 ~ "high volatility",
      TRUE ~ "low volatility"
    ),
    qb_tier = dplyr::case_when(
      is.na(.data$qb_attempts_r5) ~ "qb unknown",
      .data$qb_attempts_r5 < 28 ~ "low-volume passer",
      .data$qb_attempts_r5 < 36 ~ "mid-volume passer",
      TRUE ~ "high-volume passer"
    ),
    regression_tier = dplyr::case_when(
      is.na(.data$opp_rec_yards_gained_diff_r3) ~ "gap unknown",
      .data$opp_rec_yards_gained_diff_r3 > 0 ~ "outperforming expectation",
      TRUE ~ "underperforming expectation"
    ),
    team_total_tier = dplyr::case_when(
      is.na(.data$implied_team_total) ~ "implied unknown",
      .data$implied_team_total <= 20 ~ "implied <= 20",
      .data$implied_team_total <= 25 ~ "implied 20-25",
      TRUE ~ "implied > 25"
    ),
    venue = dplyr::if_else(.data$is_home == 1, "home", "away")
  )

cat("Bets available for scan:", nrow(scan_data), "\n")
cat("Seasons:", paste(sort(unique(scan_data$season)), collapse = ", "), "\n\n")

roi_of <- function(d) {
  if (!nrow(d)) return(c(bets = 0, roi = NA_real_, win_rate = NA_real_))
  decisions <- sum(d$result != 0)
  c(
    bets = nrow(d),
    roi = sum(d$profit) / nrow(d),
    win_rate = if (decisions) sum(d$result > 0) / decisions else NA_real_
  )
}

dimensions <- c(
  "target", "side", "position", "depth_role", "adot_tier", "snap_tier",
  "volatility_tier", "qb_tier", "regression_tier", "team_total_tier", "venue"
)
combos <- c(
  purrr::map(dimensions, ~ .x),
  purrr::map(utils::combn(dimensions, 2, simplify = FALSE), identity)
)

minimum_bets <- 40L
scan <- purrr::map_dfr(combos, function(dims) {
  scan_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(dims))) |>
    dplyr::group_modify(function(d, key) {
      disc <- roi_of(dplyr::filter(d, .data$season == 2024))
      val <- roi_of(dplyr::filter(d, .data$season == 2025))
      tibble::tibble(
        segment = paste(dims, collapse = " x "),
        discovery_bets = disc[["bets"]], discovery_roi = disc[["roi"]],
        validation_bets = val[["bets"]], validation_roi = val[["roi"]],
        validation_win_rate = val[["win_rate"]]
      )
    }) |>
    dplyr::ungroup() |>
    tidyr::unite("cell", dplyr::all_of(dims), sep = " | ")
}) |>
  dplyr::filter(
    .data$discovery_bets >= minimum_bets,
    .data$validation_bets >= minimum_bets
  )

readr::write_csv(scan, "outputs/prop_role_split_scan.csv")

positive <- scan |> dplyr::filter(.data$discovery_roi > 0)
survivors <- positive |> dplyr::filter(.data$validation_roi > 0)

cat("=== Search size ===\n")
cat("Segments tested:", nrow(scan), "\n")
cat("Profitable in 2024:", nrow(positive), "\n")
cat("Still profitable in 2025:", nrow(survivors), "\n")
cat("Survival rate:",
    round(nrow(survivors) / max(1L, nrow(positive)), 4),
    " (chance is ~0.5)\n")
if (nrow(positive)) {
  cat("One-sided binomial p vs chance:", signif(stats::binom.test(
    nrow(survivors), nrow(positive), p = 0.5, alternative = "greater"
  )$p.value, 4), "\n")
}

cat("\n--- Does selecting on 2024 help at all? ---\n")
cat("Mean 2025 ROI, cells profitable in 2024:",
    round(mean(positive$validation_roi), 4), "\n")
cat("Mean 2025 ROI, all tested cells        :",
    round(mean(scan$validation_roi, na.rm = TRUE), 4), "\n")

cat("\n=== One-way splits, both seasons shown ===\n")
print(as.data.frame(
  scan |>
    dplyr::filter(!grepl(" x ", .data$segment)) |>
    dplyr::arrange(.data$segment, dplyr::desc(.data$validation_roi)) |>
    dplyr::select("segment", "cell", "discovery_bets", "discovery_roi",
                  "validation_bets", "validation_roi")
), digits = 3)

cat("\n=== Top surviving two-way cells ===\n")
if (nrow(survivors)) {
  print(as.data.frame(
    survivors |>
      dplyr::filter(grepl(" x ", .data$segment)) |>
      dplyr::arrange(dplyr::desc(.data$validation_roi)) |>
      dplyr::select("cell", "discovery_bets", "discovery_roi",
                    "validation_bets", "validation_roi") |>
      head(12)
  ), digits = 3)
}
