source("R/utilities.R")
assert_packages()
ensure_directories()

## Builds on 91's cached ep_per_touch. Tests multiple formula variants and
## parameters to see if ANY beats "rolled PPR + rolled touches" for predicting
## next week's actual points, out of sample (2023-2025 held out, weights/
## windows chosen using train < 2023 only where fitting is involved).

ep_per_touch <- readRDS("data/processed/player_ep_per_touch.rds")
player_stats <- readRDS("data/raw/player_stats_2021_2026.rds")

fantasy_lagged_roll <- function(x, window, statistic = c("mean", "sd")) {
  statistic <- match.arg(statistic)
  slider::slide_dbl(dplyr::lag(as.numeric(x)), function(values) {
    if (!length(values) || all(is.na(values))) return(NA_real_)
    if (statistic == "mean") return(mean(values, na.rm = TRUE))
    if (sum(is.finite(values)) < 2) return(0)
    stats::sd(values, na.rm = TRUE)
  }, .before = window - 1L, .complete = FALSE)
}
# exponentially-weighted trailing mean (alpha = 2/(span+1)), same lag/no-lookahead contract
fantasy_lagged_ewma <- function(x, span) {
  a <- 2 / (span + 1)
  lagged <- dplyr::lag(as.numeric(x))
  out <- rep(NA_real_, length(lagged))
  running <- NA_real_
  for (i in seq_along(lagged)) {
    v <- lagged[i]
    if (is.na(v)) { out[i] <- running; next }
    running <- if (is.na(running)) v else a * v + (1 - a) * running
    out[i] <- running
  }
  out
}

## ---------------------------------------------------------------------------
## Game-level build: POOLED touches (as in script 91) AND SPLIT by touch-type
## (rush / rec / pass), so we can test a share-weighted "true talent by role"
## formula instead of blending a RB's rushing and receiving skill into one
## number.
## ---------------------------------------------------------------------------
game_level <- player_stats |>
  dplyr::filter(.data$season_type == "REG", .data$position %in% c("QB", "RB", "FB", "WR", "TE")) |>
  dplyr::mutate(position = dplyr::if_else(.data$position == "FB", "RB", .data$position)) |>
  dplyr::transmute(
    .data$player_id, .data$player_display_name, .data$position,
    team = normalize_team(.data$team), opponent_team = normalize_team(.data$opponent_team),
    .data$game_id, .data$season, .data$week,
    carries = dplyr::coalesce(.data$carries, 0), targets = dplyr::coalesce(.data$targets, 0),
    attempts = dplyr::coalesce(.data$attempts, 0),
    rushing_yards = dplyr::coalesce(.data$rushing_yards, 0), receiving_yards = dplyr::coalesce(.data$receiving_yards, 0),
    passing_yards = dplyr::coalesce(.data$passing_yards, 0),
    rushing_tds = dplyr::coalesce(.data$rushing_tds, 0), receiving_tds = dplyr::coalesce(.data$receiving_tds, 0),
    passing_tds = dplyr::coalesce(.data$passing_tds, 0),
    rushing_first_downs = dplyr::coalesce(.data$rushing_first_downs, 0),
    receiving_first_downs = dplyr::coalesce(.data$receiving_first_downs, 0),
    passing_first_downs = dplyr::coalesce(.data$passing_first_downs, 0),
    fantasy_points_ppr = dplyr::coalesce(.data$fantasy_points_ppr, 0)
  ) |>
  dplyr::mutate(touches = .data$carries + .data$targets + .data$attempts) |>
  dplyr::filter(.data$touches > 0) |>
  dplyr::left_join(ep_per_touch, by = c("player_id", "game_id", "season", "week")) |>
  dplyr::mutate(ep_per_touch = dplyr::coalesce(.data$ep_per_touch, stats::median(.data$ep_per_touch, na.rm = TRUE)))

W_ppr <- list(yards = 0.1, td = 6, fd = 0.5)

ordered <- game_level |>
  dplyr::arrange(.data$player_id, .data$season, .data$week) |>
  dplyr::group_by(.data$player_id) |>
  dplyr::mutate(next_ppr = dplyr::lead(.data$fantasy_points_ppr)) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(.data$next_ppr))

train <- ordered |> dplyr::filter(.data$season < 2023)
test_seasons <- c(2023, 2024, 2025)
positions <- c("QB", "RB", "WR", "TE")

## ---------------------------------------------------------------------------
## PART A: STABILITY CHECK -- does the rate itself predict itself next game?
## (classic "is this a repeatable skill or just noise" diagnostic, independent
## of whether it predicts fantasy points). Pooled yards/touch, by position.
## ---------------------------------------------------------------------------
cat("================ PART A: game-to-game STABILITY of pooled yards/touch (autocorrelation) ================\n")
ordered <- ordered |>
  dplyr::arrange(.data$player_id, .data$season, .data$week) |>
  dplyr::group_by(.data$player_id) |>
  dplyr::mutate(
    yards_per_touch = .data$touches |> {\(t) ifelse(t > 0, (.data$rushing_yards + .data$receiving_yards + .data$passing_yards) / t, NA_real_)}(),
    next_yards_per_touch = dplyr::lead(.data$yards_per_touch),
    next_touches = dplyr::lead(.data$touches)
  ) |>
  dplyr::ungroup()

for (p in positions) {
  d <- ordered |> dplyr::filter(.data$position == p, is.finite(.data$next_yards_per_touch))
  cat(sprintf("  %-3s all-touches: r=%.4f (n=%d)   touches>=8 both games: r=%.4f (n=%d)\n",
              p, stats::cor(d$yards_per_touch, d$next_yards_per_touch, use = "complete.obs"), nrow(d),
              { d2 <- d |> dplyr::filter(.data$touches >= 8, .data$next_touches >= 8)
                stats::cor(d2$yards_per_touch, d2$next_yards_per_touch, use = "complete.obs") },
              { d2 <- d |> dplyr::filter(.data$touches >= 8, .data$next_touches >= 8); nrow(d2) }))
}

## ---------------------------------------------------------------------------
## PART B: ROLLED VARIANTS GRID.
## Dimensions: window (3/5/8), mean-type (simple/ewma), weight scheme
## (ppr/empirical), formula (pooled vs split-by-role), min-touches filter
## (none / position-specific "real workload" threshold), ep treatment
## (divisor / additive / dropped).
## Baseline every time: next_ppr ~ ppr_r{w} + touches_r{w} (already-existing
## style features). We add the candidate score and report delta R^2 + p.
## ---------------------------------------------------------------------------
cat("\n================ PART B: ROLLED VARIANT GRID (held out 2023-2025) ================\n")

MIN_TOUCHES <- list(QB = 15, RB = 8, WR = 4, TE = 3)  # "real workload" cutoffs, roughly median+ for each position

build_rolled <- function(data, window, mean_fn) {
  data |>
    dplyr::arrange(.data$player_id, .data$season, .data$week) |>
    dplyr::group_by(.data$player_id, .data$season) |>
    dplyr::mutate(
      touches_r = mean_fn(.data$touches, window),
      ppr_r = mean_fn(.data$fantasy_points_ppr, window),
      ep_per_touch_r = mean_fn(.data$ep_per_touch, window),
      # pooled
      yards_r = mean_fn(.data$rushing_yards + .data$receiving_yards + .data$passing_yards, window),
      tds_r = mean_fn(.data$rushing_tds + .data$receiving_tds + .data$passing_tds, window),
      fds_r = mean_fn(.data$rushing_first_downs + .data$receiving_first_downs + .data$passing_first_downs, window),
      # split by role (each on its own touch-type denominator, rolled independently)
      carries_r = mean_fn(.data$carries, window), targets_r = mean_fn(.data$targets, window),
      attempts_r = mean_fn(.data$attempts, window),
      rush_yards_r = mean_fn(.data$rushing_yards, window), rec_yards_r = mean_fn(.data$receiving_yards, window),
      pass_yards_r = mean_fn(.data$passing_yards, window),
      rush_tds_r = mean_fn(.data$rushing_tds, window), rec_tds_r = mean_fn(.data$receiving_tds, window),
      pass_tds_r = mean_fn(.data$passing_tds, window),
      rush_fds_r = mean_fn(.data$rushing_first_downs, window), rec_fds_r = mean_fn(.data$receiving_first_downs, window),
      pass_fds_r = mean_fn(.data$passing_first_downs, window)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$touches_r), .data$touches_r > 0)
}

score_pooled <- function(d, w, ep_mode) {
  rate <- (d$yards_r / d$touches_r) * w$yards + (d$tds_r / d$touches_r) * w$td + (d$fds_r / d$touches_r) * w$fd
  if (ep_mode == "divisor") rate / d$ep_per_touch_r else rate
}
score_split <- function(d, w, ep_mode) {
  rush_share <- dplyr::coalesce(d$carries_r / d$touches_r, 0)
  rec_share  <- dplyr::coalesce(d$targets_r / d$touches_r, 0)
  pass_share <- dplyr::coalesce(d$attempts_r / d$touches_r, 0)
  comp <- function(yr, td, fd, touch) {
    r <- (yr / touch) * w$yards + (td / touch) * w$td + (fd / touch) * w$fd
    dplyr::coalesce(r, 0)
  }
  rush_c <- comp(d$rush_yards_r, d$rush_tds_r, d$rush_fds_r, d$carries_r)
  rec_c  <- comp(d$rec_yards_r,  d$rec_tds_r,  d$rec_fds_r,  d$targets_r)
  pass_c <- comp(d$pass_yards_r, d$pass_tds_r, d$pass_fds_r, d$attempts_r)
  rate <- rush_share * rush_c + rec_share * rec_c + pass_share * pass_c
  if (ep_mode == "divisor") rate / d$ep_per_touch_r else rate
}
scale100 <- function(x, position, season) {
  df <- data.frame(x = x, position = position, season = season)
  df |> dplyr::group_by(.data$position, .data$season) |>
    dplyr::mutate(s = 100 * .data$x / mean(.data$x, na.rm = TRUE)) |> dplyr::pull(.data$s)
}

fit_empirical_weights <- function(train_rolled, formula_fn, ep_mode) {
  co <- list()
  for (p in positions) {
    d <- train_rolled |> dplyr::filter(.data$position == p)
    raw <- formula_fn(d, W_ppr, ep_mode)  # placeholder scale to get yards/td/fd RATE components for regression
    # regress next_ppr on the underlying rate components directly (not pre-weighted), by role formula type
    if (identical(formula_fn, score_pooled)) {
      dd <- data.frame(next_ppr = d$next_ppr, ypt = d$yards_r / d$touches_r, tdr = d$tds_r / d$touches_r,
                        fdr = d$fds_r / d$touches_r, ep = d$ep_per_touch_r)
      fit <- stats::lm(next_ppr ~ ypt + tdr + fdr + ep, data = dd)
    } else {
      rush_share <- dplyr::coalesce(d$carries_r / d$touches_r, 0)
      rec_share  <- dplyr::coalesce(d$targets_r / d$touches_r, 0)
      pass_share <- dplyr::coalesce(d$attempts_r / d$touches_r, 0)
      dd <- data.frame(
        next_ppr = d$next_ppr,
        rush_ypt = rush_share * dplyr::coalesce(d$rush_yards_r / d$carries_r, 0),
        rec_ypt  = rec_share  * dplyr::coalesce(d$rec_yards_r  / d$targets_r, 0),
        pass_ypt = pass_share * dplyr::coalesce(d$pass_yards_r / d$attempts_r, 0),
        tdr = rush_share * dplyr::coalesce(d$rush_tds_r/d$carries_r,0) + rec_share*dplyr::coalesce(d$rec_tds_r/d$targets_r,0) + pass_share*dplyr::coalesce(d$pass_tds_r/d$attempts_r,0),
        fdr = rush_share * dplyr::coalesce(d$rush_fds_r/d$carries_r,0) + rec_share*dplyr::coalesce(d$rec_fds_r/d$targets_r,0) + pass_share*dplyr::coalesce(d$pass_fds_r/d$attempts_r,0),
        ep = d$ep_per_touch_r
      )
      fit <- stats::lm(next_ppr ~ rush_ypt + rec_ypt + pass_ypt + tdr + fdr + ep, data = dd)
    }
    co[[p]] <- fit
  }
  co
}

test_variant <- function(label, window, mean_fn, formula_fn, weight_mode, ep_mode, min_touch_filter) {
  rolled <- build_rolled(ordered, window, mean_fn)
  if (weight_mode == "ppr") {
    rolled$raw_score <- formula_fn(rolled, W_ppr, ep_mode)
  } else {
    train_r <- rolled |> dplyr::filter(.data$season < 2023)
    fits <- fit_empirical_weights(train_r, formula_fn, ep_mode)
    rolled$raw_score <- NA_real_
    for (p in positions) {
      idx <- rolled$position == p
      d <- rolled[idx, ]
      co <- stats::coef(fits[[p]])
      if (identical(formula_fn, score_pooled)) {
        ypt <- d$yards_r/d$touches_r; tdr <- d$tds_r/d$touches_r; fdr <- d$fds_r/d$touches_r
        rolled$raw_score[idx] <- co["(Intercept)"] + co["ypt"]*ypt + co["tdr"]*tdr + co["fdr"]*fdr + co["ep"]*d$ep_per_touch_r
      } else {
        rush_share <- dplyr::coalesce(d$carries_r/d$touches_r,0); rec_share <- dplyr::coalesce(d$targets_r/d$touches_r,0); pass_share <- dplyr::coalesce(d$attempts_r/d$touches_r,0)
        rush_ypt <- rush_share*dplyr::coalesce(d$rush_yards_r/d$carries_r,0); rec_ypt <- rec_share*dplyr::coalesce(d$rec_yards_r/d$targets_r,0); pass_ypt <- pass_share*dplyr::coalesce(d$pass_yards_r/d$attempts_r,0)
        tdr <- rush_share*dplyr::coalesce(d$rush_tds_r/d$carries_r,0)+rec_share*dplyr::coalesce(d$rec_tds_r/d$targets_r,0)+pass_share*dplyr::coalesce(d$pass_tds_r/d$attempts_r,0)
        fdr <- rush_share*dplyr::coalesce(d$rush_fds_r/d$carries_r,0)+rec_share*dplyr::coalesce(d$rec_fds_r/d$targets_r,0)+pass_share*dplyr::coalesce(d$pass_fds_r/d$attempts_r,0)
        rolled$raw_score[idx] <- co["(Intercept)"] + co["rush_ypt"]*rush_ypt + co["rec_ypt"]*rec_ypt + co["pass_ypt"]*pass_ypt + co["tdr"]*tdr + co["fdr"]*fdr + co["ep"]*d$ep_per_touch_r
      }
    }
  }
  rolled$score <- scale100(rolled$raw_score, rolled$position, rolled$season)

  test_r <- rolled |> dplyr::filter(.data$season %in% test_seasons)
  for (p in positions) {
    d <- test_r |> dplyr::filter(.data$position == p)
    if (min_touch_filter) d <- d |> dplyr::filter(.data$touches_r >= MIN_TOUCHES[[p]])
    d <- d |> dplyr::select(.data$next_ppr, .data$ppr_r, .data$touches_r, .data$score) |> stats::na.omit()
    if (nrow(d) < 40) { cat(sprintf("  [%s] %-3s: n too small (%d), skipped\n", label, p, nrow(d))); next }
    base <- stats::lm(next_ppr ~ ppr_r + touches_r, data = d)
    full <- stats::lm(next_ppr ~ ppr_r + touches_r + score, data = d)
    a <- stats::anova(base, full)
    r <- stats::cor(d$score, d$next_ppr)
    cat(sprintf("  [%-38s] %-3s (n=%5d): corr=%+.4f  dR2=%+.5f  p=%s\n",
                label, p, nrow(d), r, summary(full)$r.squared - summary(base)$r.squared, signif(a$`Pr(>F)`[2], 3)))
  }
}

variants <- list(
  list(label = "pooled/simple/r3/emp/div/nofilter",  window=3, mean_fn=fantasy_lagged_roll, formula_fn=score_pooled, weight_mode="empirical", ep_mode="divisor", min_touch_filter=FALSE),
  list(label = "pooled/simple/r5/emp/div/nofilter",  window=5, mean_fn=fantasy_lagged_roll, formula_fn=score_pooled, weight_mode="empirical", ep_mode="divisor", min_touch_filter=FALSE),
  list(label = "pooled/simple/r8/emp/div/nofilter",  window=8, mean_fn=fantasy_lagged_roll, formula_fn=score_pooled, weight_mode="empirical", ep_mode="divisor", min_touch_filter=FALSE),
  list(label = "pooled/simple/r5/emp/div/MINTOUCH",  window=5, mean_fn=fantasy_lagged_roll, formula_fn=score_pooled, weight_mode="empirical", ep_mode="divisor", min_touch_filter=TRUE),
  list(label = "pooled/simple/r8/emp/div/MINTOUCH",  window=8, mean_fn=fantasy_lagged_roll, formula_fn=score_pooled, weight_mode="empirical", ep_mode="divisor", min_touch_filter=TRUE),
  list(label = "pooled/EWMA5/emp/div/MINTOUCH",      window=5, mean_fn=fantasy_lagged_ewma, formula_fn=score_pooled, weight_mode="empirical", ep_mode="divisor", min_touch_filter=TRUE),
  list(label = "pooled/simple/r5/emp/NOEP/MINTOUCH", window=5, mean_fn=fantasy_lagged_roll, formula_fn=score_pooled, weight_mode="empirical", ep_mode="none",    min_touch_filter=TRUE),
  list(label = "pooled/simple/r5/PPRwt/div/MINTOUCH",window=5, mean_fn=fantasy_lagged_roll, formula_fn=score_pooled, weight_mode="ppr",       ep_mode="divisor", min_touch_filter=TRUE),
  list(label = "SPLIT/simple/r5/emp/div/nofilter",   window=5, mean_fn=fantasy_lagged_roll, formula_fn=score_split,  weight_mode="empirical", ep_mode="divisor", min_touch_filter=FALSE),
  list(label = "SPLIT/simple/r5/emp/div/MINTOUCH",   window=5, mean_fn=fantasy_lagged_roll, formula_fn=score_split,  weight_mode="empirical", ep_mode="divisor", min_touch_filter=TRUE),
  list(label = "SPLIT/simple/r8/emp/div/MINTOUCH",   window=8, mean_fn=fantasy_lagged_roll, formula_fn=score_split,  weight_mode="empirical", ep_mode="divisor", min_touch_filter=TRUE),
  list(label = "SPLIT/EWMA5/emp/div/MINTOUCH",       window=5, mean_fn=fantasy_lagged_ewma, formula_fn=score_split,  weight_mode="empirical", ep_mode="divisor", min_touch_filter=TRUE)
)

for (v in variants) {
  cat(sprintf("\n--- %s ---\n", v$label))
  test_variant(v$label, v$window, v$mean_fn, v$formula_fn, v$weight_mode, v$ep_mode, v$min_touch_filter)
}

cat("\n\nDone.\n")
