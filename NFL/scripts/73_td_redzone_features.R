## ---------------------------------------------------------------------------
## 73_td_redzone_features.R  (numbered 70 in the original draft; renumbered to
## avoid a collision with this repo's existing scripts/70_dfs_backtest_2022_2025.R)
##
## Purpose: close the §7.1 gap in the touchdown model handoff — nothing in the
## current feature set measures proximity to the end zone. This builds
## scoring-opportunity features from play-by-play and writes them to
## data/processed/td_redzone_features.rds, keyed to join into
## prepare_td_player_weeks() BEFORE the rolling/lagging step.
##
## IMPORTANT: everything written here is RAW PER-PLAYER-GAME, not rolled and
## not lagged. Your existing td_lagged_roll() does the rolling. Do not add
## these columns to the model feature list directly — add the base names to
## `rolling_measures` in prepare_td_player_weeks() and let the _r3/_r5 columns
## be generated, which td_model_feature_names() then picks up automatically.
##
## Run:  & $rscript scripts/73_td_redzone_features.R
## ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(nflreadr)
})

## normalize_team(). Raw play-by-play uses nflfastR's own team codes, which are
## NOT this repo's canonical set: pbp says "LA" for the Rams where every
## player-week frame in this codebase says "LAR" (R/utilities.R's alias map).
## Both posteam and defteam are normalized at build time below so every
## consumer joins on one convention. This was a live silent-failure: the
## def_game half of this artifact was being joined to normalized opponent_team
## in roll_td_redzone_defense(), so all 133 Rams defense-games (3.1% of
## def_game) never matched and every player facing the Rams - 903 player-weeks,
## 3.0% of the universe - got NA red-zone defense features that median
## imputation then quietly filled in.
source("R/utilities.R")

PBP_SEASONS   <- 2018:2026
OUT_PATH      <- "data/processed/td_redzone_features.rds"
XTD_TABLE_OUT <- "outputs/td_xtd_rate_table.csv"

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

## nflreadr::load_pbp() hard-errors on any season beyond its own
## most_recent_season() heuristic rather than returning an empty frame for it -
## on 2026-09-08, with week 1 not yet kicked off, that heuristic still says
## 2025, so a request that includes 2026 fails the whole call rather than just
## coming back short for that one year. Split the pull: everything nflreadr
## will actually serve, then a best-effort attempt at the current season that
## degrades to "nothing yet" instead of crashing the script.
available_seasons <- PBP_SEASONS[PBP_SEASONS <= nflreadr::most_recent_season()]
future_seasons <- setdiff(PBP_SEASONS, available_seasons)

message("Loading play-by-play for ", min(available_seasons), "-",
        max(available_seasons), " ...")
pbp_raw <- nflreadr::load_pbp(available_seasons)
message("  ", format(nrow(pbp_raw), big.mark = ","), " plays loaded.")

if (length(future_seasons)) {
  message("Seasons ", paste(future_seasons, collapse = ", "),
          " are not yet available from nflreadr (most_recent_season() = ",
          nflreadr::most_recent_season(), "); attempting anyway per-season.")
  for (s in future_seasons) {
    extra <- tryCatch(nflreadr::load_pbp(s), error = function(e) NULL)
    if (!is.null(extra) && nrow(extra)) {
      message("  ", s, ": ", format(nrow(extra), big.mark = ","), " plays.")
      pbp_raw <- dplyr::bind_rows(pbp_raw, extra)
    } else {
      message("  ", s, ": nothing posted yet.")
    }
  }
}

## ---------------------------------------------------------------------------
## 1. Normalize the play universe
##
## Scope matches the model's label: rushing + receiving TDs only. Two-point
## conversions are excluded because they do not count as touchdowns.
## ---------------------------------------------------------------------------

plays <- pbp_raw %>%
  filter(
    season_type == "REG",
    !is.na(yardline_100),
    !is.na(posteam),
    is.na(two_point_attempt) | two_point_attempt == 0,
    play_type %in% c("run", "pass")
  ) %>%
  transmute(
    season, week, game_id,
    posteam = normalize_team(posteam),
    defteam = normalize_team(defteam),
    yardline_100,
    goal_to_go   = ifelse(is.na(goal_to_go), 0, goal_to_go),
    play_type,
    rusher_id    = rusher_player_id,
    receiver_id  = receiver_player_id,
    rush_td      = ifelse(is.na(rush_touchdown), 0, rush_touchdown),
    pass_td      = ifelse(is.na(pass_touchdown), 0, pass_touchdown)
  )

## Long format: one row per (player, touch-opportunity). A carry is a rush
## attempt credited to rusher_id; a target is a pass credited to receiver_id
## (nflfastR populates receiver_player_id on incompletions, so this is targets,
## not receptions — which is what we want, an opportunity measure).
touches <- bind_rows(
  plays %>%
    filter(play_type == "run", !is.na(rusher_id)) %>%
    transmute(season, week, game_id, posteam, defteam, yardline_100, goal_to_go,
              player_id = rusher_id, touch_type = "rush", scored = rush_td),
  plays %>%
    filter(play_type == "pass", !is.na(receiver_id)) %>%
    transmute(season, week, game_id, posteam, defteam, yardline_100, goal_to_go,
              player_id = receiver_id, touch_type = "rec", scored = pass_td)
)

message("  ", format(nrow(touches), big.mark = ","), " credited touch opportunities.")

## ---------------------------------------------------------------------------
## 2. Expected-TD-per-touch table (leak-safe)
##
## The headline new signal. Each touch is worth a TD probability determined by
## where on the field it happened and whether it was a run or a pass. Summing
## those per player-game gives xtd — an opportunity-weighted scoring-chance
## measure that a 15-target game between the 20s cannot inflate.
##
## Leakage control: the rate table applied to season S is fit ONLY on seasons
## strictly before S. PBP_SEASONS starts at 2018 specifically so that 2021 -
## the earliest season any model input actually reaches back to (the
## fundamental model's thinnest walk-forward fold trains on 2021 alone) - gets
## a clean 2018-2020 prior instead of being fit on itself. 2018 itself is still
## fit on itself and flagged fitted_on_self = TRUE; that's fine, since nothing
## downstream ever trains on 2018 rows.
## ---------------------------------------------------------------------------

yardline_bucket <- function(y) {
  cut(y,
      breaks = c(0, 2, 5, 10, 20, 40, 100),
      labels = c("1-2", "3-5", "6-10", "11-20", "21-40", "41+"),
      include.lowest = TRUE, right = TRUE)
}

touches <- touches %>% mutate(yl_bucket = yardline_bucket(yardline_100))

## Two-level shrinkage. The original single-level version shrunk every cell
## toward the touch_type grand mean, which is dominated by midfield volume -
## an order-of-magnitude-wrong prior for a goal-to-go 1-2 cell, and confirmed
## as the actual defect (not sampling noise) because the self-fit 2021 table
## landed in the same 0.91-0.97 calibration band as every other season despite
## having zero out-of-sample gap to explain it. Cells now shrink toward their
## own yardline bucket's rate instead, which is itself only lightly shrunk
## toward the grand mean and has enough volume (tens of thousands of plays)
## that the light shrink barely moves it.
build_rate_table <- function(df) {
  grand <- df %>%
    group_by(touch_type) %>%
    summarise(grand_rate = mean(scored), .groups = "drop")

  bucket_lvl <- df %>%
    group_by(touch_type, yl_bucket) %>%
    summarise(b_rate = mean(scored), b_n = n(), .groups = "drop") %>%
    left_join(grand, by = "touch_type") %>%
    mutate(b_prior = (b_rate * b_n + grand_rate * 25) / (b_n + 25))

  df %>%
    group_by(touch_type, yl_bucket, goal_to_go) %>%
    summarise(cell_rate = mean(scored), n = n(), .groups = "drop") %>%
    left_join(bucket_lvl %>% select(touch_type, yl_bucket, b_prior),
              by = c("touch_type", "yl_bucket")) %>%
    mutate(td_rate = (cell_rate * n + b_prior * 25) / (n + 25)) %>%
    select(touch_type, yl_bucket, goal_to_go, td_rate, n)
}

rate_tables <- map_dfr(sort(unique(touches$season)), function(s) {
  prior <- touches %>% filter(season < s)
  if (nrow(prior) < 5000) {
    prior <- touches %>% filter(season == s)
    fitted_on_self <- TRUE
  } else {
    fitted_on_self <- FALSE
  }
  build_rate_table(prior) %>%
    mutate(season = s, fitted_on_self = fitted_on_self)
})

readr::write_csv(rate_tables, XTD_TABLE_OUT)
message("  xTD rate table written to ", XTD_TABLE_OUT)

touches <- touches %>%
  left_join(rate_tables %>% select(season, touch_type, yl_bucket, goal_to_go, td_rate),
            by = c("season", "touch_type", "yl_bucket", "goal_to_go")) %>%
  mutate(td_rate = ifelse(is.na(td_rate), 0, td_rate))

## ---------------------------------------------------------------------------
## 3. Player-game scoring-opportunity aggregates
## ---------------------------------------------------------------------------

player_game <- touches %>%
  group_by(season, week, game_id, posteam, defteam, player_id) %>%
  summarise(
    rz20_carries = sum(touch_type == "rush" & yardline_100 <= 20),
    rz10_carries = sum(touch_type == "rush" & yardline_100 <= 10),
    rz5_carries  = sum(touch_type == "rush" & yardline_100 <= 5),
    gtg_carries  = sum(touch_type == "rush" & goal_to_go == 1),
    rz20_targets = sum(touch_type == "rec"  & yardline_100 <= 20),
    rz10_targets = sum(touch_type == "rec"  & yardline_100 <= 10),
    rz5_targets  = sum(touch_type == "rec"  & yardline_100 <= 5),
    gtg_targets  = sum(touch_type == "rec"  & goal_to_go == 1),
    xtd_rush     = sum(td_rate[touch_type == "rush"]),
    xtd_rec      = sum(td_rate[touch_type == "rec"]),
    .groups = "drop"
  ) %>%
  mutate(
    rz10_touches = rz10_carries + rz10_targets,
    rz20_touches = rz20_carries + rz20_targets,
    gtg_touches  = gtg_carries + gtg_targets,
    xtd_total    = xtd_rush + xtd_rec
  )

## Team totals in the same game, for shares. Shares are the point: a back with
## 4 of his team's 5 goal-to-go carries is a different animal from one with 4
## of 14, and raw counts cannot tell them apart.
team_game <- player_game %>%
  group_by(season, week, game_id, posteam) %>%
  summarise(across(c(rz20_carries, rz10_carries, rz5_carries, gtg_carries,
                     rz20_targets, rz10_targets, rz5_targets, gtg_targets,
                     rz10_touches, rz20_touches, gtg_touches, xtd_total),
                   sum, .names = "team_{.col}"),
            .groups = "drop")

safe_share <- function(num, den) ifelse(den > 0, num / den, NA_real_)

player_game <- player_game %>%
  left_join(team_game, by = c("season", "week", "game_id", "posteam")) %>%
  mutate(
    rz20_carry_share  = safe_share(rz20_carries, team_rz20_carries),
    rz10_carry_share  = safe_share(rz10_carries, team_rz10_carries),
    rz5_carry_share   = safe_share(rz5_carries,  team_rz5_carries),
    gtg_carry_share   = safe_share(gtg_carries,  team_gtg_carries),
    rz20_target_share = safe_share(rz20_targets, team_rz20_targets),
    rz10_target_share = safe_share(rz10_targets, team_rz10_targets),
    gtg_target_share  = safe_share(gtg_targets,  team_gtg_targets),
    rz10_touch_share  = safe_share(rz10_touches, team_rz10_touches),
    gtg_touch_share   = safe_share(gtg_touches,  team_gtg_touches),
    xtd_share         = safe_share(xtd_total,    team_xtd_total)
  )

## ---------------------------------------------------------------------------
## 4. Opponent red-zone defense, per defense-game
##
## Split rush/pass rather than by offensive position. The existing def_*_r5
## features are position-split; this is a deliberate simplification to avoid a
## weekly-roster join, and rush/pass captures most of the red-zone positional
## signal. If it earns its keep, upgrade it to position-split later.
## ---------------------------------------------------------------------------

def_game <- touches %>%
  group_by(season, week, game_id, defteam) %>%
  summarise(
    def_rz20_carries_allowed = sum(touch_type == "rush" & yardline_100 <= 20),
    def_rz10_carries_allowed = sum(touch_type == "rush" & yardline_100 <= 10),
    def_rz20_targets_allowed = sum(touch_type == "rec"  & yardline_100 <= 20),
    def_rz10_targets_allowed = sum(touch_type == "rec"  & yardline_100 <= 10),
    def_rz_rush_td_allowed   = sum(scored[touch_type == "rush" & yardline_100 <= 20]),
    def_rz_rec_td_allowed    = sum(scored[touch_type == "rec"  & yardline_100 <= 20]),
    def_xtd_allowed          = sum(td_rate),
    .groups = "drop"
  ) %>%
  ## conversion rate allowed: did they hold up once the offense got there
  mutate(
    def_rz_td_conv_allowed = safe_share(
      def_rz_rush_td_allowed + def_rz_rec_td_allowed,
      def_rz20_carries_allowed + def_rz20_targets_allowed
    )
  )

## ---------------------------------------------------------------------------
## 5. Write
## ---------------------------------------------------------------------------

out <- list(
  player_game = player_game %>% select(-starts_with("team_")),
  def_game    = def_game,
  rate_tables = rate_tables,
  built_at    = Sys.time(),
  seasons     = PBP_SEASONS
)

saveRDS(out, OUT_PATH)
message("Wrote ", OUT_PATH)

## ---------------------------------------------------------------------------
## 6. Sanity checks — read this output, do not skip it
## ---------------------------------------------------------------------------

cat("\n================ SANITY CHECKS ================\n")

cat("\n-- rows per season (player_game) --\n")
print(player_game %>% count(season))

cat("\n-- 2026 weeks present (should show week 1 only, or nothing if PBP not yet posted) --\n")
print(player_game %>% filter(season == 2026) %>% count(week))

cat("\n-- xTD calibration: sum(xtd_total) vs actual TDs, by season --\n")
actual_td <- touches %>%
  group_by(season) %>%
  summarise(actual = sum(scored), .groups = "drop")
print(
  player_game %>%
    group_by(season) %>%
    summarise(expected = sum(xtd_total), .groups = "drop") %>%
    left_join(actual_td, by = "season") %>%
    mutate(ratio = round(expected / actual, 3))
)
cat("\n  ratio should sit near 1.00. Materially off means the rate table is\n",
    " mis-specified or the touch universe is double-counting.\n")

cat("\n-- top 15 xTD player-games, 2025 (eyeball test) --\n")
print(
  player_game %>%
    filter(season == 2025) %>%
    arrange(desc(xtd_total)) %>%
    select(week, posteam, player_id, rz10_touches, gtg_touches, xtd_total) %>%
    head(15) %>%
    as.data.frame()
)

cat("\n-- NA rates on the new share columns --\n")
print(
  player_game %>%
    summarise(across(ends_with("_share"), ~ round(mean(is.na(.x)), 3)))
)
cat("\n  High NA on gtg_* shares is expected and correct — most teams have zero\n",
    " goal-to-go snaps in a given game, so the share is undefined, not zero.\n",
    " Your median-imputation in td_matrix_pair() will handle it, but check that\n",
    " the imputed value is not doing something silly at the tails.\n")

cat("\n===============================================\n")

## ---------------------------------------------------------------------------
## 7. INTEGRATION — what to change in R/touchdown_features.R
##
## a) In prepare_td_player_weeks(), after the player-week frame is assembled
##    and BEFORE the rolling step, join:
##
##      rz <- readRDS("data/processed/td_redzone_features.rds")
##      player_weeks <- player_weeks %>%
##        left_join(rz$player_game,
##                  by = c("season", "week", "player_id")) %>%
##        left_join(rz$def_game %>% rename(opponent_team = defteam),
##                  by = c("season", "week", "opponent_team"))
##
##    (adjust the key names to whatever your frame actually uses — if your
##    player id column is gsis_id or player_gsis_id, map it here.)
##
## b) Add to `rolling_measures` so the existing td_lagged_roll() generates
##    _r3/_r5 versions, which td_model_feature_names() then picks up:
##
##      "rz10_touches", "gtg_touches", "xtd_total",
##      "rz10_carry_share", "gtg_carry_share",
##      "rz10_target_share", "gtg_target_share",
##      "xtd_share"
##
## c) Add the opponent columns to the 5-game opponent block alongside the
##    existing def_*_r5 features:
##
##      "def_rz10_carries_allowed", "def_rz10_targets_allowed",
##      "def_rz_td_conv_allowed", "def_xtd_allowed"
##
## d) Do NOT add the raw un-rolled columns to the feature list. They are
##    same-game values and would leak the target game directly.
##
## e) The handoff (§3) notes there is no registered-schema assertion on this
##    model. You are about to add ~12 features through an automatic
##    intersection. Add the assertion now or this join silently changing
##    scope is exactly the failure mode the game models already had.
## ---------------------------------------------------------------------------
