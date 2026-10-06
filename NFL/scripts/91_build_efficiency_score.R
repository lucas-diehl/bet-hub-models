source("R/utilities.R")
assert_packages()
ensure_directories()

## ---------------------------------------------------------------------------
## STEP 1: play-by-play -> player-game "expected points per opportunity"
##
## ep = nflfastR's expected-points value of the game state BEFORE the play
## (down/distance/field position/score/time), i.e. how valuable was the
## situation this player's touch happened in -- independent of what he did
## with it. Averaged over every play where a player carried, was targeted, or
## (for QBs) threw, this is a real, defensible "opportunity quality" signal:
## a player getting mostly garbage-time checkdowns has a low ep_per_touch;
## one getting red-zone carries and 3rd-and-short targets has a high one.
## ---------------------------------------------------------------------------
ep_cache_path <- "data/processed/player_ep_per_touch.rds"

if (file.exists(ep_cache_path)) {
  cat("Using cached", ep_cache_path, "\n")
  ep_per_touch <- readRDS(ep_cache_path)
} else {
  cat("Pulling play-by-play 2021-2026 (nflreadr) -- this takes a few minutes...\n")
  pbp <- nflreadr::load_pbp(2021:2026)
  cat("pbp rows:", nrow(pbp), "\n")

  rush_side <- pbp |>
    dplyr::filter(!is.na(.data$rusher_player_id), .data$play_type == "run") |>
    dplyr::transmute(
      player_id = .data$rusher_player_id, .data$game_id, .data$season, .data$week,
      ep = .data$ep
    )
  rec_side <- pbp |>
    dplyr::filter(!is.na(.data$receiver_player_id), .data$play_type == "pass") |>
    dplyr::transmute(
      player_id = .data$receiver_player_id, .data$game_id, .data$season, .data$week,
      ep = .data$ep
    )
  pass_side <- pbp |>
    dplyr::filter(!is.na(.data$passer_player_id), .data$play_type == "pass") |>
    dplyr::transmute(
      player_id = .data$passer_player_id, .data$game_id, .data$season, .data$week,
      ep = .data$ep
    )

  ep_per_touch <- dplyr::bind_rows(rush_side, rec_side, pass_side) |>
    dplyr::filter(is.finite(.data$ep)) |>
    dplyr::group_by(.data$player_id, .data$game_id, .data$season, .data$week) |>
    dplyr::summarise(
      ep_touches = dplyr::n(),
      ep_sum = sum(.data$ep, na.rm = TRUE),
      ep_per_touch = mean(.data$ep, na.rm = TRUE),
      .groups = "drop"
    )
  saveRDS(ep_per_touch, ep_cache_path)
  cat("Saved", ep_cache_path, "--", nrow(ep_per_touch), "player-games\n")
}

## ---------------------------------------------------------------------------
## STEP 2: per-game touches / yards / tds / first downs, generalized across
## position by summing whichever of rushing/receiving/passing actually
## applies to that player-role (a WR has 0 carries/attempts, a pure passer
## has 0 carries/targets, a dual-threat RB/QB naturally combines both) --
## "opportunity" = carries + targets + attempts, so it's genuinely
## position-specific without needing separate formulas per position.
## ---------------------------------------------------------------------------
player_stats <- readRDS("data/raw/player_stats_2021_2026.rds")

game_level <- player_stats |>
  dplyr::filter(.data$season_type == "REG", .data$position %in% c("QB", "RB", "FB", "WR", "TE")) |>
  dplyr::mutate(position = dplyr::if_else(.data$position == "FB", "RB", .data$position)) |>
  dplyr::transmute(
    .data$player_id, player_display_name = .data$player_display_name,
    .data$position, team = normalize_team(.data$team),
    opponent_team = normalize_team(.data$opponent_team),
    .data$game_id, .data$season, .data$week,
    touches = dplyr::coalesce(.data$carries, 0) + dplyr::coalesce(.data$targets, 0) +
      dplyr::coalesce(.data$attempts, 0),
    yards = dplyr::coalesce(.data$rushing_yards, 0) + dplyr::coalesce(.data$receiving_yards, 0) +
      dplyr::coalesce(.data$passing_yards, 0),
    tds = dplyr::coalesce(.data$rushing_tds, 0) + dplyr::coalesce(.data$receiving_tds, 0) +
      dplyr::coalesce(.data$passing_tds, 0),
    first_downs = dplyr::coalesce(.data$rushing_first_downs, 0) +
      dplyr::coalesce(.data$receiving_first_downs, 0) + dplyr::coalesce(.data$passing_first_downs, 0),
    fantasy_points_ppr = dplyr::coalesce(.data$fantasy_points_ppr, 0)
  ) |>
  dplyr::filter(.data$touches > 0) |>
  dplyr::left_join(ep_per_touch, by = c("player_id", "game_id", "season", "week")) |>
  dplyr::mutate(
    ep_per_touch = dplyr::coalesce(.data$ep_per_touch, stats::median(.data$ep_per_touch, na.rm = TRUE)),
    yards_per_touch = .data$yards / .data$touches,
    td_rate = .data$tds / .data$touches,
    fd_rate = .data$first_downs / .data$touches
  )

cat("game_level rows:", nrow(game_level), "  missing ep_per_touch (median-filled):",
    sum(is.na(dplyr::left_join(game_level, ep_per_touch, by = c("player_id","game_id","season","week"))$ep_per_touch.y)), "\n")

## ---------------------------------------------------------------------------
## STEP 3: next-game actual PPR points (the thing we're testing predictive
## power against), and a train/test split matching the convention already
## used elsewhere in this repo (train < 2023, held-out test 2023-2025; 2026
## is this-season-in-progress, reported separately, not used to fit anything).
## ---------------------------------------------------------------------------
ordered <- game_level |>
  dplyr::arrange(.data$player_id, .data$season, .data$week) |>
  dplyr::group_by(.data$player_id) |>
  dplyr::mutate(next_ppr = dplyr::lead(.data$fantasy_points_ppr)) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(.data$next_ppr))

train <- ordered |> dplyr::filter(.data$season < 2023)
test  <- ordered |> dplyr::filter(.data$season >= 2023, .data$season <= 2025)
cat("train rows:", nrow(train), " test rows:", nrow(test), "\n")

## ---------------------------------------------------------------------------
## STEP 4a: STANDARD PPR WEIGHTS variant (hand-set, matches how the sport is
## actually scored: 0.1 pt/yard, 6 pt/TD; first downs aren't standard-scored
## anywhere, so 0.5 pt/first-down mirrors common "first-down PPR" formats).
## ---------------------------------------------------------------------------
W_ppr <- list(yards = 0.1, td = 6, fd = 0.5)

score_with_weights <- function(data, w) {
  raw <- (data$yards_per_touch * w$yards + data$td_rate * w$td + data$fd_rate * w$fd) / data$ep_per_touch
  raw
}

add_scaled_score <- function(data, raw_col_name, score_col_name) {
  data |>
    dplyr::group_by(.data$position, .data$season) |>
    dplyr::mutate(!!score_col_name := 100 * .data[[raw_col_name]] / mean(.data[[raw_col_name]], na.rm = TRUE)) |>
    dplyr::ungroup()
}

ordered$raw_ppr_score <- score_with_weights(ordered, W_ppr)
ordered <- add_scaled_score(ordered, "raw_ppr_score", "score_ppr")

## ---------------------------------------------------------------------------
## STEP 4b: EMPIRICALLY FIT WEIGHTS variant -- OLS, by position, TRAIN seasons
## only, predicting NEXT game's actual PPR points from THIS game's three rate
## components (already divided by ep_per_touch is NOT done here; ep_per_touch
## enters as a separate covariate so its own coefficient is estimated rather
## than assumed to enter as a simple denominator -- this is the "let the data
## pick the linear weights" analogue of how wOBA's coefficients are derived
## from empirical run values rather than assumed).
## ---------------------------------------------------------------------------
fit_position_weights <- function(train_data, pos) {
  d <- train_data |> dplyr::filter(.data$position == pos)
  fit <- stats::lm(next_ppr ~ yards_per_touch + td_rate + fd_rate + ep_per_touch, data = d)
  fit
}

positions <- c("QB", "RB", "WR", "TE")
fits <- stats::setNames(lapply(positions, function(p) fit_position_weights(train, p)), positions)

cat("\n=== Empirically fit weights (train seasons < 2023), by position ===\n")
for (p in positions) {
  cat("\n---", p, "---\n")
  print(summary(fits[[p]])$coefficients)
  cat("R-squared (in-sample, train):", round(summary(fits[[p]])$r.squared, 4), "\n")
}

# apply each position's fitted coefficients to build the empirical-weight score
# for the FULL dataset (train+test+2026), then scale to league-average=100.
ordered$raw_emp_score <- NA_real_
for (p in positions) {
  idx <- ordered$position == p
  co <- stats::coef(fits[[p]])
  ordered$raw_emp_score[idx] <-
    co["(Intercept)"] +
    co["yards_per_touch"] * ordered$yards_per_touch[idx] +
    co["td_rate"] * ordered$td_rate[idx] +
    co["fd_rate"] * ordered$fd_rate[idx] +
    co["ep_per_touch"] * ordered$ep_per_touch[idx]
}
ordered <- add_scaled_score(ordered, "raw_emp_score", "score_emp")

saveRDS(ordered, "data/processed/efficiency_score_research_gamelevel.rds")
cat("\nSaved game-level research dataset:", nrow(ordered), "rows\n")

## ---------------------------------------------------------------------------
## STEP 5: SAME-GAME predictive test (cleanest, no rolling-window plumbing to
## get wrong) -- does THIS game's score predict NEXT game's actual PPR points,
## by position, out of sample (fit weights on train, evaluate correlation and
## incremental R^2 on the 2023-2025 held-out test set)? Baseline = this game's
## raw fantasy_points_ppr (the naive "hot hand" predictor) and opportunity
## (touches) alone, so we can see whether the new score beats naive carries.
## ---------------------------------------------------------------------------
cat("\n\n================ SAME-GAME -> NEXT-GAME PREDICTIVE TEST (held-out 2023-2025) ================\n")
sig_row <- function(label, pos, fit_full, fit_base) {
  a1 <- stats::anova(fit_base, fit_full)
  list(label = label, position = pos,
       r2_base = round(summary(fit_base)$r.squared, 4),
       r2_full = round(summary(fit_full)$r.squared, 4),
       delta_r2 = round(summary(fit_full)$r.squared - summary(fit_base)$r.squared, 4),
       f_p_value = signif(a1$`Pr(>F)`[2], 4))
}

results <- list()
for (p in positions) {
  d <- test |> dplyr::filter(.data$position == p)
  d$score_ppr <- ordered$score_ppr[match(paste(d$player_id, d$game_id), paste(ordered$player_id, ordered$game_id))]
  d$score_emp <- ordered$score_emp[match(paste(d$player_id, d$game_id), paste(ordered$player_id, ordered$game_id))]

  base_naive <- stats::lm(next_ppr ~ fantasy_points_ppr + touches, data = d)
  full_ppr   <- stats::lm(next_ppr ~ fantasy_points_ppr + touches + score_ppr, data = d)
  full_emp   <- stats::lm(next_ppr ~ fantasy_points_ppr + touches + score_emp, data = d)

  cat("\n---", p, "(n =", nrow(d), ") ---\n")
  cat("  raw correlation, score_ppr vs next_ppr:", round(stats::cor(d$score_ppr, d$next_ppr, use = "complete.obs"), 4), "\n")
  cat("  raw correlation, score_emp vs next_ppr:", round(stats::cor(d$score_emp, d$next_ppr, use = "complete.obs"), 4), "\n")
  cat("  raw correlation, this-game PPR vs next_ppr (naive baseline):", round(stats::cor(d$fantasy_points_ppr, d$next_ppr, use = "complete.obs"), 4), "\n")

  r_ppr <- sig_row("PPR-weighted score", p, full_ppr, base_naive)
  r_emp <- sig_row("Empirically-fit score", p, full_emp, base_naive)
  cat(sprintf("  %-24s base_R2=%.4f  full_R2=%.4f  delta_R2=%.4f  incremental p=%s\n",
              r_ppr$label, r_ppr$r2_base, r_ppr$r2_full, r_ppr$delta_r2, r_ppr$f_p_value))
  cat(sprintf("  %-24s base_R2=%.4f  full_R2=%.4f  delta_R2=%.4f  incremental p=%s\n",
              r_emp$label, r_emp$r2_base, r_emp$r2_full, r_emp$delta_r2, r_emp$f_p_value))
  results[[p]] <- list(ppr = r_ppr, emp = r_emp)
}

## ---------------------------------------------------------------------------
## STEP 6: CONTEXT ADJUSTMENTS -- test dome / weather / opponent defense as
## additional regressors alongside the score, rather than assuming
## hand-picked multipliers. If a term is significant here, its fitted
## coefficient is the empirically justified "adjustment"; if not, the score
## already captures what that context would have added (plausible, since
## ep_per_touch itself reflects situational value) and adding a hand-waved
## multiplier would just be noise.
## ---------------------------------------------------------------------------
cat("\n\n================ CONTEXT ADJUSTMENTS (dome / weather / opponent defense) ================\n")
game_ctx <- readRDS("data/processed/td_game_context.rds") |>
  dplyr::select(.data$game_id, .data$is_dome, .data$wind_speed, .data$temperature, .data$high_wind_index, .data$cold_index)
matchup <- readRDS("data/processed/nfl_matchup_features.rds")$def_pos_game |>
  dplyr::transmute(opponent_team = .data$defteam, .data$position, .data$season, .data$week,
                    opp_def_epa = dplyr::coalesce(.data$def_rush_epa_allowed, 0) + dplyr::coalesce(.data$def_pass_epa_allowed, 0))

ctx_test <- test |>
  dplyr::left_join(game_ctx, by = "game_id") |>
  dplyr::left_join(matchup, by = c("opponent_team", "position", "season", "week"))
ctx_test$score_emp <- ordered$score_emp[match(paste(ctx_test$player_id, ctx_test$game_id), paste(ordered$player_id, ordered$game_id))]

for (p in positions) {
  d <- ctx_test |>
    dplyr::filter(.data$position == p) |>
    dplyr::select(.data$next_ppr, .data$score_emp, .data$is_dome, .data$high_wind_index,
                  .data$cold_index, .data$opp_def_epa) |>
    stats::na.omit()
  if (nrow(d) < 30) { cat("\n---", p, ": insufficient matchup coverage, skipped ---\n"); next }
  base <- stats::lm(next_ppr ~ score_emp, data = d)
  full <- stats::lm(next_ppr ~ score_emp + is_dome + high_wind_index + cold_index + opp_def_epa, data = d)
  cat("\n---", p, "(n =", nrow(d), ") ---\n")
  print(round(summary(full)$coefficients, 4))
  a1 <- stats::anova(base, full)
  cat("  delta R2 from adding context:", round(summary(full)$r.squared - summary(base)$r.squared, 4),
      " incremental F-test p =", signif(a1$`Pr(>F)`[2], 4), "\n")
}

## ---------------------------------------------------------------------------
## STEP 7: THE FAIR TEST -- rolled (5-game trailing average) version, since
## that's how WOPR/opportunity are actually used in the live model (never as
## a single-game value). Within-season-only rolling here (simpler than the
## full cross-season blend used in production; fine for a significance check,
## would need that treatment if this graduates to an actual model feature).
## Tests whether the ROLLED score adds anything beyond the ROLLED versions of
## the features already driving the model (opportunity_r5-equivalent =
## rolled touches, and rolled fantasy_points_ppr itself).
## ---------------------------------------------------------------------------
cat("\n\n================ ROLLED (5-game) VERSION -- the actual apples-to-apples test ================\n")
roll5 <- function(x) fantasy_lagged_roll(x, 5L, "mean")
# fantasy_lagged_roll lives in fantasy_prop_model.R; source it standalone (avoids
# pulling in the rest of that file's heavier dependencies for this research script)
if (!exists("fantasy_lagged_roll")) {
  fantasy_lagged_roll <- function(x, window, statistic = c("mean", "sd")) {
    statistic <- match.arg(statistic)
    slider::slide_dbl(dplyr::lag(as.numeric(x)), function(values) {
      if (!length(values) || all(is.na(values))) return(NA_real_)
      if (statistic == "mean") return(mean(values, na.rm = TRUE))
      if (sum(is.finite(values)) < 2) return(0)
      stats::sd(values, na.rm = TRUE)
    }, .before = window - 1L, .complete = FALSE)
  }
}

rolled <- ordered |>
  dplyr::arrange(.data$player_id, .data$season, .data$week) |>
  dplyr::group_by(.data$player_id, .data$season) |>
  dplyr::mutate(
    yards_per_touch_r5 = roll5(.data$yards_per_touch),
    td_rate_r5 = roll5(.data$td_rate),
    fd_rate_r5 = roll5(.data$fd_rate),
    ep_per_touch_r5 = roll5(.data$ep_per_touch),
    touches_r5 = roll5(.data$touches),
    ppr_r5 = roll5(.data$fantasy_points_ppr)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(.data$yards_per_touch_r5))

rolled$raw_ppr_score_r5 <- (rolled$yards_per_touch_r5 * W_ppr$yards + rolled$td_rate_r5 * W_ppr$td +
                              rolled$fd_rate_r5 * W_ppr$fd) / rolled$ep_per_touch_r5
rolled$raw_emp_score_r5 <- NA_real_
for (p in positions) {
  idx <- rolled$position == p
  co <- stats::coef(fits[[p]])
  rolled$raw_emp_score_r5[idx] <-
    co["(Intercept)"] + co["yards_per_touch"] * rolled$yards_per_touch_r5[idx] +
    co["td_rate"] * rolled$td_rate_r5[idx] + co["fd_rate"] * rolled$fd_rate_r5[idx] +
    co["ep_per_touch"] * rolled$ep_per_touch_r5[idx]
}
rolled <- rolled |>
  dplyr::group_by(.data$position, .data$season) |>
  dplyr::mutate(
    score_ppr_r5 = 100 * .data$raw_ppr_score_r5 / mean(.data$raw_ppr_score_r5, na.rm = TRUE),
    score_emp_r5 = 100 * .data$raw_emp_score_r5 / mean(.data$raw_emp_score_r5, na.rm = TRUE)
  ) |>
  dplyr::ungroup()

rolled_test <- rolled |> dplyr::filter(.data$season >= 2023, .data$season <= 2025)
for (p in positions) {
  d <- rolled_test |> dplyr::filter(.data$position == p) |>
    dplyr::select(.data$next_ppr, .data$ppr_r5, .data$touches_r5, .data$score_ppr_r5, .data$score_emp_r5) |>
    stats::na.omit()
  base <- stats::lm(next_ppr ~ ppr_r5 + touches_r5, data = d)
  full_ppr <- stats::lm(next_ppr ~ ppr_r5 + touches_r5 + score_ppr_r5, data = d)
  full_emp <- stats::lm(next_ppr ~ ppr_r5 + touches_r5 + score_emp_r5, data = d)
  cat("\n---", p, "(n =", nrow(d), ") ---\n")
  cat("  corr(score_ppr_r5, next_ppr):", round(stats::cor(d$score_ppr_r5, d$next_ppr), 4),
      "   corr(score_emp_r5, next_ppr):", round(stats::cor(d$score_emp_r5, d$next_ppr), 4),
      "   corr(ppr_r5, next_ppr) [existing baseline]:", round(stats::cor(d$ppr_r5, d$next_ppr), 4), "\n")
  a_ppr <- stats::anova(base, full_ppr); a_emp <- stats::anova(base, full_emp)
  cat(sprintf("  PPR-weighted r5 score:    base_R2=%.4f full_R2=%.4f delta_R2=%.4f  p=%s\n",
              summary(base)$r.squared, summary(full_ppr)$r.squared,
              summary(full_ppr)$r.squared - summary(base)$r.squared, signif(a_ppr$`Pr(>F)`[2], 4)))
  cat(sprintf("  Empirically-fit r5 score: base_R2=%.4f full_R2=%.4f delta_R2=%.4f  p=%s\n",
              summary(base)$r.squared, summary(full_emp)$r.squared,
              summary(full_emp)$r.squared - summary(base)$r.squared, signif(a_emp$`Pr(>F)`[2], 4)))
}

cat("\n\nDone.\n")
