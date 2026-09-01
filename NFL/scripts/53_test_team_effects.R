source("R/utilities.R")
source("R/draft_model.R")
assert_packages()
ensure_directories()
options(nflreadr.verbose = FALSE)

# Does knowing the offence a player lines up in improve the draft model?
#
# Two arms over identical rows and identical walk-forward seasons, so the only
# difference is the feature set:
#
#   base  - the original player-history features
#   team  - plus the new team's prior-season offensive output, the change in
#           that output versus the team he left, and a moved flag
#
# The question is asked of everyone, not just movers: a player staying put on a
# team that threw 620 times is in a different situation from one on a team that
# threw 380, whether or not he changed jerseys.

history_path <- "data/raw/season_aggregates.rds"
seasons_table <- readRDS(history_path)

team_path <- "data/raw/team_offense.rds"
if (file.exists(team_path)) {
  team_offense <- readRDS(team_path)
} else {
  cat("Rebuilding team offence from player stats...\n")
  stats <- nflreadr::load_player_stats(2006:2025)
  team_offense <- draft_team_offense(stats)
  saveRDS(team_offense, team_path)
}
cat("Team-seasons:", nrow(team_offense), "\n")

draft_picks <- nflreadr::load_draft_picks()

base_table <- draft_build_features(seasons_table, draft_picks)
team_table <- draft_build_features(seasons_table, draft_picks, team_offense)

keep <- function(d) {
  dplyr::filter(
    d, .data$games >= 1,
    dplyr::coalesce(.data$career_games, 0) >= 1 | .data$is_rookie == 1
  )
}
base_table <- keep(base_table)
team_table <- keep(team_table)

cat("Rows, base:", nrow(base_table), " team:", nrow(team_table), "\n")
cat("Team-feature coverage:",
    round(mean(!is.na(team_table$tm_ppr)), 4), "\n")
cat("Players who changed team:",
    sum(team_table$changed_team == 1, na.rm = TRUE), "\n\n")

test_seasons <- 2016:2025
base_pred <- walk_forward_draft_model(base_table, test_seasons)
team_pred <- walk_forward_draft_model(
  team_table, test_seasons,
  features = c(draft_feature_names(), draft_team_feature_names())
)

grade <- function(pred, label) {
  d <- pred |>
    dplyr::filter(!is.na(.data$ppr_total)) |>
    dplyr::mutate(baseline_total = dplyr::coalesce(.data$total_1, 0))
  tibble::tibble(
    arm = label,
    players = nrow(d),
    rho = stats::cor(d$projected_total, d$ppr_total, method = "spearman"),
    r = stats::cor(d$projected_total, d$ppr_total),
    mae = mean(abs(d$projected_total - d$ppr_total))
  )
}

cat("=== Overall ===\n")
print(as.data.frame(dplyr::bind_rows(
  grade(base_pred, "base"), grade(team_pred, "team")
)), digits = 5)

by_cut <- function(pred, label, column, values) {
  purrr::map_dfr(values, function(v) {
    d <- pred |>
      dplyr::filter(.data[[column]] == v, !is.na(.data$ppr_total))
    if (nrow(d) < 40) return(tibble::tibble())
    tibble::tibble(
      cut = as.character(v), arm = label, players = nrow(d),
      rho = stats::cor(d$projected_total, d$ppr_total, method = "spearman"),
      mae = mean(abs(d$projected_total - d$ppr_total))
    )
  })
}

cat("\n=== By position ===\n")
print(as.data.frame(
  dplyr::bind_rows(
    by_cut(base_pred, "base", "position", c("QB", "RB", "WR", "TE")),
    by_cut(team_pred, "team", "position", c("QB", "RB", "WR", "TE"))
  ) |>
    tidyr::pivot_wider(id_cols = "cut", names_from = "arm",
                       values_from = c("rho", "mae"))
), digits = 4)

# The cut that matters most for the complaint that prompted this: does knowing
# the new offence help specifically for players who switched teams?
movers <- team_table |>
  dplyr::filter(.data$changed_team == 1) |>
  dplyr::distinct(.data$season, .data$player_id)

cat("\n=== Players who changed teams that season ===\n")
mover_grade <- function(pred, label) {
  d <- pred |>
    dplyr::semi_join(movers, by = c("season", "player_id")) |>
    dplyr::filter(!is.na(.data$ppr_total))
  tibble::tibble(
    arm = label, players = nrow(d),
    rho = stats::cor(d$projected_total, d$ppr_total, method = "spearman"),
    mae = mean(abs(d$projected_total - d$ppr_total))
  )
}
print(as.data.frame(dplyr::bind_rows(
  mover_grade(base_pred, "base"), mover_grade(team_pred, "team")
)), digits = 5)

# Paired bootstrap on the per-player absolute error between the two arms.
paired <- base_pred |>
  dplyr::select("season", "player_id", "ppr_total",
                base_proj = "projected_total") |>
  dplyr::inner_join(
    team_pred |> dplyr::select("season", "player_id",
                               team_proj = "projected_total"),
    by = c("season", "player_id")
  ) |>
  dplyr::filter(!is.na(.data$ppr_total)) |>
  dplyr::mutate(
    delta = abs(.data$team_proj - .data$ppr_total) -
      abs(.data$base_proj - .data$ppr_total)
  )

set.seed(20260814L)
draws <- replicate(2000, mean(sample(paired$delta, nrow(paired), replace = TRUE)))
cat("\n=== Paired bootstrap, team arm minus base arm (MAE) ===\n")
cat(sprintf(
  "n = %d, mean %.3f (negative favours team features), 95%% CI %.3f to %.3f, P(team better) %.3f\n",
  nrow(paired), mean(paired$delta),
  stats::quantile(draws, 0.025), stats::quantile(draws, 0.975),
  mean(draws < 0)
))

movers_only <- paired |> dplyr::semi_join(movers, by = c("season", "player_id"))
draws_m <- replicate(
  2000, mean(sample(movers_only$delta, nrow(movers_only), replace = TRUE))
)
cat(sprintf(
  "movers only: n = %d, mean %.3f, 95%% CI %.3f to %.3f, P(team better) %.3f\n",
  nrow(movers_only), mean(movers_only$delta),
  stats::quantile(draws_m, 0.025), stats::quantile(draws_m, 0.975),
  mean(draws_m < 0)
))

saveRDS(team_pred, "data/processed/draft_model_team_predictions.rds")
