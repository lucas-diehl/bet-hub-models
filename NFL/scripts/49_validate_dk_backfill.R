source("R/utilities.R")
source("R/dfs_salaries.R")
source("R/touchdown_features.R")
assert_packages()
ensure_directories()

# Sanity checks on the DraftKings salary backfill, plus a true cross-validation.
#
# Matching a draft group to a week does not prove the salaries inside it are
# right. The strongest available test is 2021: RotoGuru still serves that season
# and it is already in the archive, so the same week can be recovered
# independently through the DraftKings API and compared player by player. If the
# two agree on 2021 the method is sound for 2022-2025, where no second source
# exists.

recovered <- readRDS("data/processed/dfs_salaries_dk_2022_plus.rds")

cat("=== Coverage ===\n")
print(as.data.frame(
  recovered |>
    dplyr::group_by(.data$season) |>
    dplyr::summarise(
      weeks = dplyr::n_distinct(.data$week),
      rows = dplyr::n(),
      players_per_week = round(dplyr::n() / dplyr::n_distinct(.data$week)),
      min_salary = min(.data$salary),
      median_salary = stats::median(.data$salary),
      max_salary = max(.data$salary),
      .groups = "drop"
    )
))

cat("\n=== Missing weeks ===\n")
expected <- tidyr::expand_grid(season = 2022:2025, week = 1:18)
missing <- expected |>
  dplyr::anti_join(
    recovered |> dplyr::distinct(.data$season, .data$week),
    by = c("season", "week")
  )
print(as.data.frame(missing))

cat("\n=== Position mix (should look like an NFL slate) ===\n")
print(as.data.frame(
  recovered |>
    dplyr::count(.data$season, .data$position) |>
    tidyr::pivot_wider(names_from = "position", values_from = "n",
                       values_fill = 0)
))

cat("\n=== Highest-paid players per season (should be recognisable stars) ===\n")
print(as.data.frame(
  recovered |>
    dplyr::group_by(.data$season) |>
    dplyr::slice_max(.data$salary, n = 4, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select("season", "week", "player_name", "position", "team", "salary")
))

# --------------------------------------------------------------------------
# Cross-validation against RotoGuru on 2021
# --------------------------------------------------------------------------

dk21_rows <- dplyr::filter(recovered, .data$season == 2021)
if (!nrow(dk21_rows)) {
  cat("\nNo 2021 rows recovered, so there is nothing to cross-check.\n")
  quit(save = "no", status = 0)
}

dk21 <- dk21_rows |>
  dplyr::mutate(name_key = normalize_prop_player_name(.data$player_name)) |>
  dplyr::select("season", "week", "name_key", "position",
                dk_salary = "salary")

rg21 <- readRDS("data/processed/dfs_salaries.rds") |>
  dplyr::filter(.data$season == 2021, .data$site == "DK") |>
  dplyr::mutate(name_key = normalize_prop_player_name(.data$player_name)) |>
  dplyr::select("season", "week", "name_key",
                rg_salary = "salary")

joined <- dk21 |>
  dplyr::inner_join(rg21, by = c("season", "week", "name_key")) |>
  dplyr::mutate(
    diff = .data$dk_salary - .data$rg_salary,
    exact = .data$diff == 0
  )

cat("\n=== 2021 cross-validation: DraftKings API vs RotoGuru ===\n")
cat("DK rows recovered:", nrow(dk21), "\n")
cat("RotoGuru DK rows :", nrow(rg21), "\n")
cat("Matched on name+week:", nrow(joined), "\n")
if (nrow(joined)) {
  cat("Exact salary agreement:",
      sprintf("%.2f%%", 100 * mean(joined$exact)), "\n")
  cat("Mean absolute difference: $",
      round(mean(abs(joined$diff)), 2), "\n", sep = "")
  cat("Correlation:", round(stats::cor(joined$dk_salary, joined$rg_salary), 6), "\n")
  cat("\nDisagreements by size:\n")
  print(as.data.frame(
    joined |>
      dplyr::mutate(bucket = dplyr::case_when(
        .data$diff == 0 ~ "exact",
        abs(.data$diff) <= 200 ~ "within $200",
        abs(.data$diff) <= 1000 ~ "within $1000",
        TRUE ~ "over $1000"
      )) |>
      dplyr::count(.data$bucket) |>
      dplyr::mutate(share = round(.data$n / sum(.data$n), 4))
  ))
  cat("\nWorst disagreements:\n")
  print(as.data.frame(
    joined |> dplyr::slice_max(abs(.data$diff), n = 8, with_ties = FALSE) |>
      dplyr::select("week", "name_key", "position", "dk_salary", "rg_salary",
                    "diff")
  ))
  readr::write_csv(joined, "outputs/dk_backfill_validation_2021.csv")
}
