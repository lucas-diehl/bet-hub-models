source("R/utilities.R")
source("R/touchdown_features.R")
assert_packages()
ensure_directories()

# The 2026 preseason board against current expert consensus rankings.
#
# nflreadr only serves the latest ECR scrape, so there is no historical ADP to
# backtest against - the accuracy work in scripts/45 therefore benchmarks
# against prior-season points per game instead, which is close to the
# information ADP encodes. This script is a disagreement view for the current
# draft, not evidence that the disagreements are right.

options(nflreadr.verbose = FALSE)

board <- readr::read_csv(
  "outputs/fantasy_prop_2026_week1_projections.csv", show_col_types = FALSE
)

ecr <- nflreadr::load_ff_rankings("draft") |>
  dplyr::filter(.data$ecr_type == "dp", .data$pos %in% c("QB", "RB", "WR", "TE")) |>
  dplyr::mutate(name_key = normalize_prop_player_name(.data$player)) |>
  dplyr::group_by(.data$name_key) |>
  dplyr::slice_min(.data$ecr, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select("name_key", "player", "pos", "team", "ecr", "sd", "best", "worst")

cat("ECR rows (PPR draft):", nrow(ecr), "\n")
cat("Board rows:", nrow(board), "\n")

joined <- board |>
  dplyr::mutate(name_key = normalize_prop_player_name(.data$player)) |>
  dplyr::inner_join(ecr, by = "name_key", suffix = c("", "_ecr")) |>
  dplyr::filter(.data$position %in% c("QB", "RB", "WR", "TE"))

cat("Matched to consensus:", nrow(joined),
    sprintf("(%.1f%%)\n", 100 * nrow(joined) / nrow(board)))

# Rank within position on each side, then compare.
ranked <- joined |>
  dplyr::group_by(.data$position) |>
  dplyr::mutate(
    model_pos_rank = dplyr::min_rank(dplyr::desc(.data$projected_ppr)),
    ecr_pos_rank = dplyr::min_rank(.data$ecr),
    rank_gap = .data$ecr_pos_rank - .data$model_pos_rank
  ) |>
  dplyr::ungroup()

readr::write_csv(ranked, "outputs/board_2026_vs_consensus.csv")

cat("\n=== Agreement between model and consensus, by position ===\n")
print(as.data.frame(
  ranked |>
    dplyr::group_by(.data$position) |>
    dplyr::summarise(
      players = dplyr::n(),
      rank_correlation = stats::cor(
        .data$model_pos_rank, .data$ecr_pos_rank, method = "spearman"
      ),
      median_abs_rank_gap = stats::median(abs(.data$rank_gap)),
      .groups = "drop"
    )
), digits = 4)

show_side <- function(d, label, n = 12) {
  cat("\n===", label, "===\n")
  print(as.data.frame(
    d |>
      dplyr::slice_head(n = n) |>
      dplyr::transmute(
        player = .data$player, pos = .data$position, team = .data$team,
        proj_ppr = round(.data$projected_ppr, 1),
        model_rank = .data$model_pos_rank,
        consensus_rank = .data$ecr_pos_rank,
        gap = .data$rank_gap
      )
  ))
}

# Restrict to players consensus actually drafts, so the list is not dominated
# by deep bench names the model happens to rank oddly.
relevant <- ranked |> dplyr::filter(.data$ecr_pos_rank <= 40)

show_side(
  relevant |> dplyr::arrange(dplyr::desc(.data$rank_gap)),
  "Model higher than consensus (potential values)"
)
show_side(
  relevant |> dplyr::arrange(.data$rank_gap),
  "Consensus higher than model (potential fades)"
)

cat("\n=== Top 15 by model projection, with consensus rank ===\n")
print(as.data.frame(
  ranked |>
    dplyr::arrange(dplyr::desc(.data$projected_ppr)) |>
    dplyr::slice_head(n = 15) |>
    dplyr::transmute(
      player = .data$player, pos = .data$position, team = .data$team,
      proj_ppr = round(.data$projected_ppr, 1),
      ppr_low = round(.data$ppr_low, 1), ppr_high = round(.data$ppr_high, 1),
      consensus_pos_rank = .data$ecr_pos_rank
    )
))
