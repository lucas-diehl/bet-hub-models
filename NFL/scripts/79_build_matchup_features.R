## ---------------------------------------------------------------------------
## 79_build_matchup_features.R
##
## Position x play-type defensive efficiency from play-by-play.
##
## What this adds that nothing in the repo had: the fantasy model's existing
## opponent block (add_fantasy_opponent_features) rolls RAW BOX-SCORE COUNTS
## allowed by (defense, position) - yards, targets, TDs. Raw counts cannot
## separate "this defense is good" from "this defense faced few plays", and
## they are not comparable across weeks or seasons. What's missing is
## efficiency: EPA and success rate allowed, split by the position being
## defended AND by whether it happened on a run or a pass.
##
## That split is the point. A defense that is stout against RB runs but soft
## against RB receiving is a completely different matchup for the same back
## depending on game script, and one blended "rushing yards allowed" number
## cannot express it. Likewise "faces the best run defense in the league"
## only becomes a usable model input once it is efficiency-based and
## normalized - which is what the roll/normalize step in
## R/fantasy_prop_model.R does with this artifact.
##
## Leak-safety: everything written here is RAW PER-DEFENSE-GAME, not rolled
## and not lagged - same contract as scripts/73. The lagging/rolling happens
## in add_fantasy_matchup_features(), so a same-game value can never reach a
## model. Do not add these columns to a feature list un-rolled.
##
## Conventions deliberately shared with the rest of the repo:
##   - garbage-time filter and EPA winsorization reuse nfl_dvoa_garbage() /
##     nfl_dvoa_cap() from R/nfl_dvoa_ratings.R, so "EPA" means the same thing
##     here as it does in the team ratings.
##   - normalize_team() on posteam/defteam, because raw pbp says LA/OAK/SD
##     where every player-week frame in this repo says LAR/LV/LAC. Getting
##     this wrong is not a loud failure, it is 3% of the league silently
##     becoming NA (see the bug fixed in scripts/73 in this same change).
##
## Run:  & $rscript scripts/79_build_matchup_features.R
## ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(nflreadr)
})

source("R/utilities.R")
source("R/nfl_dvoa_ratings.R")

PBP_SEASONS <- 2018:2026
OUT_PATH <- "data/processed/nfl_matchup_features.rds"

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

## ---------------------------------------------------------------------------
## 1. Load play-by-play
##
## Same split-pull pattern scripts/73 uses: nflreadr::load_pbp() hard-errors on
## a season past its most_recent_season() heuristic rather than returning
## nothing for it, so the current season is attempted separately and degrades
## to "nothing posted yet" instead of failing the whole build.
## ---------------------------------------------------------------------------

available_seasons <- PBP_SEASONS[PBP_SEASONS <= nflreadr::most_recent_season()]
future_seasons <- setdiff(PBP_SEASONS, available_seasons)

message("Loading play-by-play for ", min(available_seasons), "-",
        max(available_seasons), " ...")
pbp <- nflreadr::load_pbp(available_seasons)
message("  ", format(nrow(pbp), big.mark = ","), " plays loaded.")

if (length(future_seasons)) {
  for (s in future_seasons) {
    extra <- tryCatch(nflreadr::load_pbp(s), error = function(e) NULL)
    if (!is.null(extra) && nrow(extra)) {
      message("  ", s, ": ", format(nrow(extra), big.mark = ","), " plays.")
      pbp <- dplyr::bind_rows(pbp, extra)
    } else {
      message("  ", s, ": nothing posted yet.")
    }
  }
}

## ---------------------------------------------------------------------------
## 2. Play universe + EPA conventions
## ---------------------------------------------------------------------------

plays <- pbp |>
  filter(
    season_type == "REG",
    !is.na(epa), !is.na(posteam), !is.na(defteam),
    posteam != "", defteam != "",
    play_type %in% c("pass", "run")
  ) |>
  transmute(
    season = as.integer(season),
    week = as.integer(week),
    game_id,
    posteam = normalize_team(posteam),
    defteam = normalize_team(defteam),
    epa = as.numeric(epa),
    qtr = suppressWarnings(as.numeric(qtr)),
    score_differential = suppressWarnings(as.numeric(score_differential)),
    yardline_100 = suppressWarnings(as.numeric(yardline_100)),
    play_type,
    rusher_id = rusher_player_id,
    receiver_id = receiver_player_id
  )

n_all <- nrow(plays)
plays <- plays |> filter(!nfl_dvoa_garbage(qtr, score_differential))
message("  ", format(nrow(plays), big.mark = ","), " plays after garbage filter (",
        format(n_all - nrow(plays), big.mark = ","), " dropped).")

## Winsorize at the 2/98 percentile - the same cap the DVOA v6/v7 variants use.
## A 90-yard TD is real but it is not 12x more informative than a 20-yard gain,
## and un-capped EPA lets one play dominate a defense's weekly average.
q02 <- stats::quantile(plays$epa, c(0.02, 0.98), na.rm = TRUE)
plays <- plays |> mutate(epa_c = nfl_dvoa_cap(epa, q02))

## ---------------------------------------------------------------------------
## 3. Attribute each play to the position being defended
##
## This join is the genuinely new piece - nothing in this repo previously
## connected play-level EPA to the position of the player it happened to.
## Runs are credited to the rusher, passes to the TARGETED receiver (nflfastR
## populates receiver_player_id on incompletions too, which is what we want:
## an opportunity-level measure, not a completions-only one).
##
## Position comes from weekly player stats rather than load_players(), because
## that is season-specific - load_players() carries one career position, which
## silently mislabels position changes. FB collapses to RB to match
## prepare_fantasy_player_features().
## ---------------------------------------------------------------------------

message("Building season-accurate position map ...")
pos_map <- nflreadr::load_player_stats(sort(unique(plays$season)), summary_level = "week") |>
  filter(!is.na(player_id), !is.na(position)) |>
  transmute(
    season = as.integer(season),
    player_id,
    position = if_else(position == "FB", "RB", position)
  ) |>
  filter(position %in% c("QB", "RB", "WR", "TE")) |>
  count(season, player_id, position) |>
  group_by(season, player_id) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(season, player_id, position)

message("  ", format(nrow(pos_map), big.mark = ","), " (season, player) position rows.")

touch_plays <- bind_rows(
  plays |>
    filter(play_type == "run", !is.na(rusher_id)) |>
    transmute(season, week, game_id, defteam, epa_c, yardline_100,
              player_id = rusher_id, touch_type = "rush"),
  plays |>
    filter(play_type == "pass", !is.na(receiver_id)) |>
    transmute(season, week, game_id, defteam, epa_c, yardline_100,
              player_id = receiver_id, touch_type = "pass")
) |>
  left_join(pos_map, by = c("season", "player_id"))

match_rate <- mean(!is.na(touch_plays$position))
message(sprintf("  position matched on %.1f%% of %s credited plays.",
                100 * match_rate, format(nrow(touch_plays), big.mark = ",")))
if (match_rate < 0.90) {
  stop("Position match rate ", round(100 * match_rate, 1),
       "% below 90%. Likely a player_id convention mismatch (GSIS vs other).",
       call. = FALSE)
}
touch_plays <- touch_plays |> filter(!is.na(position))

## ---------------------------------------------------------------------------
## 4. Per-defense-game aggregates, split by position x play type
## ---------------------------------------------------------------------------

def_pos_game <- touch_plays |>
  group_by(season, week, game_id, defteam, position, touch_type) |>
  summarise(
    epa_allowed = mean(epa_c, na.rm = TRUE),
    success_allowed = mean(epa_c > 0, na.rm = TRUE),
    plays_allowed = n(),
    rz_epa_allowed = {
      rz <- epa_c[!is.na(yardline_100) & yardline_100 <= 20]
      if (length(rz)) mean(rz, na.rm = TRUE) else NA_real_
    },
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = touch_type,
    values_from = c(epa_allowed, success_allowed, plays_allowed, rz_epa_allowed),
    names_glue = "def_{touch_type}_{.value}"
  ) |>
  ## plays_allowed is a count: absent means zero faced, not unknown.
  mutate(across(ends_with("_plays_allowed"), ~ coalesce(.x, 0)))

message("  ", format(nrow(def_pos_game), big.mark = ","),
        " (defense, game, position) rows.")

out <- list(
  def_pos_game = def_pos_game,
  epa_cap = q02,
  built_at = Sys.time(),
  seasons = PBP_SEASONS
)
saveRDS(out, OUT_PATH)
message("Wrote ", OUT_PATH)

## ---------------------------------------------------------------------------
## 5. Sanity checks - read these, do not skip them
## ---------------------------------------------------------------------------

cat("\n================ SANITY CHECKS ================\n")

cat("\n-- rows per season --\n")
print(def_pos_game |> count(season) |> as.data.frame())

cat("\n-- team codes (must be normalize_team() convention: LAR/LV/LAC, not LA/OAK/SD) --\n")
print(sort(unique(def_pos_game$defteam)))

cat("\n-- 2026 weeks present --\n")
print(def_pos_game |> filter(season == 2026) |> count(week) |> as.data.frame())

cat("\n-- mean EPA allowed by position x play type, 2025 --\n")
print(
  def_pos_game |>
    filter(season == 2025) |>
    group_by(position) |>
    summarise(
      rush_epa = round(mean(def_rush_epa_allowed, na.rm = TRUE), 4),
      pass_epa = round(mean(def_pass_epa_allowed, na.rm = TRUE), 4),
      rush_plays = round(mean(def_rush_plays_allowed, na.rm = TRUE), 1),
      pass_plays = round(mean(def_pass_plays_allowed, na.rm = TRUE), 1),
      .groups = "drop"
    ) |> as.data.frame()
)
cat("\n  Expect: RB carries the rush volume, WR/TE carry the pass volume, and\n",
    " QB rush appears only for scrambles/designed runs. Pass EPA should sit\n",
    " above rush EPA - passing is the more efficient play in the modern NFL.\n")

cat("\n-- best / worst run defenses vs RB, 2025 (eyeball test) --\n")
rb25 <- def_pos_game |>
  filter(season == 2025, position == "RB") |>
  group_by(defteam) |>
  summarise(rush_epa_allowed = round(mean(def_rush_epa_allowed, na.rm = TRUE), 4),
            .groups = "drop") |>
  arrange(rush_epa_allowed)
cat("  strongest (lowest EPA allowed):\n"); print(head(rb25, 5) |> as.data.frame())
cat("  weakest  (highest EPA allowed):\n"); print(tail(rb25, 5) |> as.data.frame())

cat("\n-- NA rates --\n")
print(
  def_pos_game |>
    summarise(across(starts_with("def_"), ~ round(mean(is.na(.x)), 3))) |>
    as.data.frame()
)
cat("\n  High NA on def_rush_* for WR/TE is expected and correct - a WR carry is\n",
    " rare, so most defense-games have no WR rush to measure. Same for rz_epa\n",
    " columns: many defense-games allow no red-zone snap to that position.\n")

cat("\n===============================================\n")
