## ---------------------------------------------------------------------------
## 86_build_snap_features.R
##
## Snap-share features for pass catchers and backs.
##
## Why this is worth a new ingestion when re-arranging existing columns was not:
## every usage feature the model has is TARGET-derived (targets, target_share,
## air yards, wopr, receptions). Those only register a player once the ball
## comes his way. Snap share measures whether he is on the field at all, which
## is the part of "role" that moves FIRST - a receiver promoted to starter plays
## the snaps in week 1 of the new role and the targets follow later.
##
## That is the specific failure measured on WR rows (40% of the pool, the one
## position losing to DK salary): when salary disagrees with the model salary is
## directionally right, monotonically, and the gap concentrates in players whose
## role is rising. Salary reprices off the depth chart immediately; trailing
## target means take weeks. Role-trend deltas built from the SAME target data
## were tried first and produced nothing (+0.002 R2 on WR, net zero overall) -
## which is the evidence that the missing ingredient is information, not
## encoding.
##
## Join note: snap counts key on pfr_player_id, everything else in this repo on
## GSIS. The crosswalk comes from nflreadr::load_players(). Match rate is
## asserted rather than assumed - a silent id mismatch would look exactly like
## "these features do not help".
##
## Run:  & $rscript scripts/86_build_snap_features.R
## ---------------------------------------------------------------------------

source("R/utilities.R")
suppressPackageStartupMessages({
  library(dplyr)
})
assert_packages()
ensure_directories()

SEASONS <- 2021:2026
OUT_PATH <- "data/processed/nfl_snap_features.rds"

message("Loading snap counts ", min(SEASONS), "-", max(SEASONS), " ...")
sc <- nflreadr::load_snap_counts(SEASONS)
message("  ", format(nrow(sc), big.mark = ","), " player-games.")

## ---------------------------------------------------------------------------
## 1. pfr_player_id -> GSIS crosswalk
## ---------------------------------------------------------------------------

players <- nflreadr::load_players()
id_cols <- intersect(c("gsis_id", "pfr_id", "position"), names(players))
if (!all(c("gsis_id", "pfr_id") %in% id_cols)) {
  stop("load_players() lacks gsis_id/pfr_id; cannot build the crosswalk.",
       call. = FALSE)
}
xwalk <- players |>
  select(all_of(c("gsis_id", "pfr_id"))) |>
  filter(!is.na(.data$gsis_id), !is.na(.data$pfr_id)) |>
  distinct(.data$pfr_id, .keep_all = TRUE)
message("Crosswalk rows: ", format(nrow(xwalk), big.mark = ","))

snaps <- sc |>
  filter(.data$game_type == "REG" | is.na(.data$game_type)) |>
  transmute(
    season = as.integer(.data$season),
    week = as.integer(.data$week),
    pfr_id = .data$pfr_player_id,
    position = .data$position,
    offense_snaps = suppressWarnings(as.numeric(.data$offense_snaps)),
    offense_pct = suppressWarnings(as.numeric(.data$offense_pct))
  ) |>
  filter(.data$position %in% c("QB", "RB", "FB", "WR", "TE")) |>
  left_join(xwalk, by = "pfr_id")

match_rate <- mean(!is.na(snaps$gsis_id))
message(sprintf("GSIS match rate: %.1f%% of %s snap rows",
                100 * match_rate, format(nrow(snaps), big.mark = ",")))
if (match_rate < 0.90) {
  stop("Snap->GSIS match rate ", round(100 * match_rate, 1),
       "% below 90%. Crosswalk is broken; these features would be mostly NA ",
       "and would look like 'no signal' rather than a join failure.",
       call. = FALSE)
}

snaps <- snaps |>
  filter(!is.na(.data$gsis_id)) |>
  transmute(
    season = .data$season, week = .data$week, player_id = .data$gsis_id,
    offense_snaps = coalesce(.data$offense_snaps, 0),
    snap_share = pmin(pmax(coalesce(.data$offense_pct, 0), 0), 1)
  ) |>
  ## One row per player-week; a player traded mid-week could otherwise appear
  ## twice and silently fan out the join downstream.
  group_by(.data$season, .data$week, .data$player_id) |>
  summarise(offense_snaps = sum(.data$offense_snaps),
            snap_share = max(.data$snap_share), .groups = "drop")

saveRDS(snaps, OUT_PATH)

cat("\n================ SNAP FEATURES ================\n")
cat("\nrows:", nrow(snaps), " ->  ", OUT_PATH, "\n")
cat("\n-- coverage by season --\n")
print(as.data.frame(snaps |> count(season)), row.names = FALSE)
cat("\n-- 2026 weeks --\n")
print(as.data.frame(snaps |> filter(season == 2026) |> count(week)), row.names = FALSE)
cat("\n-- snap share distribution (2025) --\n")
print(summary(snaps$snap_share[snaps$season == 2025]))
cat("\n  Sanity: a starting WR sits near 0.85-1.00, a rotational WR3 near\n",
    " 0.40-0.60. A distribution piled at zero means the join failed.\n")
cat("\n==============================================\n")
