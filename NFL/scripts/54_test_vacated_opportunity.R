source("R/utilities.R")
source("R/draft_model.R")
assert_packages()
ensure_directories()
options(nflreadr.verbose = FALSE)

# Does knowing how much opportunity a team lost improve the draft model?
#
#   team     - player history plus the team's offensive environment
#   vacated  - plus how many targets and carries walked out the door, how much
#              is returning, and the net vacated volume for this player
#
# Identical rows and walk-forward seasons in both arms.

seasons_table <- readRDS("data/raw/season_aggregates.rds")
team_offense <- readRDS("data/raw/team_offense.rds")
draft_picks <- nflreadr::load_draft_picks()
vacated <- draft_vacated_opportunity(seasons_table)

cat("Team-seasons with vacancy data:", nrow(vacated), "\n")
cat("Median vacated targets:",
    round(stats::median(vacated$tm_vacated_targets), 1), "\n")
cat("Median vacated target share:",
    round(stats::median(vacated$vacated_target_share), 3), "\n\n")

keep <- function(d) {
  dplyr::filter(
    d, .data$games >= 1,
    dplyr::coalesce(.data$career_games, 0) >= 1 | .data$is_rookie == 1
  )
}

team_table <- keep(draft_build_features(seasons_table, draft_picks, team_offense))
vac_table <- keep(
  draft_build_features(seasons_table, draft_picks, team_offense, vacated)
)

team_features <- c(draft_feature_names(), draft_team_feature_names())
vac_features <- c(team_features, draft_vacated_feature_names())

test_seasons <- 2016:2025
team_pred <- walk_forward_draft_model(team_table, test_seasons, team_features)
vac_pred <- walk_forward_draft_model(vac_table, test_seasons, vac_features)

grade <- function(pred, label) {
  d <- dplyr::filter(pred, !is.na(.data$ppr_total))
  tibble::tibble(
    arm = label, players = nrow(d),
    rho = stats::cor(d$projected_total, d$ppr_total, method = "spearman"),
    r = stats::cor(d$projected_total, d$ppr_total),
    mae = mean(abs(d$projected_total - d$ppr_total))
  )
}

cat("=== Overall ===\n")
print(as.data.frame(dplyr::bind_rows(
  grade(team_pred, "team"), grade(vac_pred, "vacated")
)), digits = 5)

by_pos <- function(pred, label) {
  purrr::map_dfr(c("QB", "RB", "WR", "TE"), function(p) {
    d <- dplyr::filter(pred, .data$position == p, !is.na(.data$ppr_total))
    tibble::tibble(
      cut = p, arm = label,
      rho = stats::cor(d$projected_total, d$ppr_total, method = "spearman"),
      mae = mean(abs(d$projected_total - d$ppr_total))
    )
  })
}
cat("\n=== By position ===\n")
print(as.data.frame(
  dplyr::bind_rows(by_pos(team_pred, "team"), by_pos(vac_pred, "vacated")) |>
    tidyr::pivot_wider(id_cols = "cut", names_from = "arm",
                       values_from = c("rho", "mae"))
), digits = 4)

# The cut this feature is meant to serve: players on teams that lost a lot of
# target volume, where a redistribution should be predictable.
high_vacancy <- vac_table |>
  dplyr::filter(.data$vacated_target_share >= 0.25) |>
  dplyr::distinct(.data$season, .data$player_id)

cat("\n=== Players on teams that vacated 25%+ of their targets ===\n")
cut_grade <- function(pred, label) {
  d <- pred |>
    dplyr::semi_join(high_vacancy, by = c("season", "player_id")) |>
    dplyr::filter(!is.na(.data$ppr_total))
  tibble::tibble(
    arm = label, players = nrow(d),
    rho = stats::cor(d$projected_total, d$ppr_total, method = "spearman"),
    mae = mean(abs(d$projected_total - d$ppr_total))
  )
}
print(as.data.frame(dplyr::bind_rows(
  cut_grade(team_pred, "team"), cut_grade(vac_pred, "vacated")
)), digits = 5)

paired <- team_pred |>
  dplyr::select("season", "player_id", "ppr_total", a = "projected_total") |>
  dplyr::inner_join(
    vac_pred |> dplyr::select("season", "player_id", b = "projected_total"),
    by = c("season", "player_id")
  ) |>
  dplyr::filter(!is.na(.data$ppr_total)) |>
  dplyr::mutate(
    delta = abs(.data$b - .data$ppr_total) - abs(.data$a - .data$ppr_total)
  )

set.seed(20260814L)
boot <- function(d, label) {
  draws <- replicate(2000, mean(sample(d$delta, nrow(d), replace = TRUE)))
  cat(sprintf(
    "%-22s n=%5d  mean %+.3f  95%% CI %+.3f to %+.3f  P(vacated better) %.3f\n",
    label, nrow(d), mean(d$delta),
    stats::quantile(draws, 0.025), stats::quantile(draws, 0.975),
    mean(draws < 0)
  ))
}
cat("\n=== Paired bootstrap, vacated arm minus team arm (MAE) ===\n")
boot(paired, "all players")
boot(paired |> dplyr::semi_join(high_vacancy, by = c("season", "player_id")),
     "high-vacancy teams")
