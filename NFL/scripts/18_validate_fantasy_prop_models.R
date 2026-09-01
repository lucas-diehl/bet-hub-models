source("R/utilities.R")
source("R/fantasy_prop_model.R")
assert_packages()

deployment <- readRDS(
  "data/processed/fantasy_prop_deployment_models.rds"
)
walk_forward <- readRDS(
  "data/processed/fantasy_prop_walk_forward.rds"
)
board <- readr::read_csv(
  "outputs/fantasy_prop_2026_week1_projections.csv",
  show_col_types = FALSE
)
ppr_metrics <- readr::read_csv(
  "outputs/fantasy_ppr_metrics_by_season.csv",
  show_col_types = FALSE
)

expected_targets <- names(fantasy_target_specifications())

# Structural checks alone once passed while every stored booster was an empty
# list, so assert the reloaded models can actually score before anything else.
dead <- names(deployment$models)[
  !vapply(
    deployment$models,
    function(m) booster_is_live(m$fit, m$features),
    logical(1)
  )
]
if (length(dead)) {
  stop(
    "These deployment boosters cannot score after reload: ",
    paste(dead, collapse = ", "),
    ". Retrain with scripts/17_build_fantasy_prop_models.R.",
    call. = FALSE
  )
}

stopifnot(
  setequal(names(deployment$models), expected_targets),
  setequal(unique(walk_forward$predictions$target), expected_targets),
  setequal(sort(unique(walk_forward$predictions$season)), 2023:2025),
  !anyDuplicated(paste(
    walk_forward$predictions$game_id,
    walk_forward$predictions$player_id,
    walk_forward$predictions$target
  )),
  nrow(board) > 0,
  !anyDuplicated(paste(board$game_id, board$player_id)),
  all(board$season == 2026),
  all(board$week == 1),
  all(board$projected_ppr >= 0),
  all(board$ppr_low >= 0),
  all(board$ppr_high >= board$ppr_low),
  all(ppr_metrics$mae_improvement > 0)
)

forbidden <- c(
  "american_odds", "best_book", "relative_edge", "expected_roi",
  "stake", "units", "bankroll"
)
stopifnot(!any(forbidden %in% names(board)))

major_targets <- c(
  "receptions", "receiving_yards", "rushing_yards",
  "passing_yards", "passing_tds", "interceptions"
)
overall <- readr::read_csv(
  "outputs/fantasy_prop_metrics_overall.csv",
  show_col_types = FALSE
)
stopifnot(all(
  overall$mae_improvement[overall$target %in% major_targets] > 0
))

cat("Fantasy prop model validation: OK\n")
cat("Separate models:", length(expected_targets), "\n")
cat("Walk-forward component predictions:", nrow(walk_forward$predictions), "\n")
cat("2026 Week 1 projected players:", nrow(board), "\n")
cat("2026 Week 1 games:", dplyr::n_distinct(board$game_id), "\n")
