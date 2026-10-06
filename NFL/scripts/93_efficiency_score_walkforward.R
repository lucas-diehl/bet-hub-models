source("R/utilities.R")
source("R/fantasy_prop_model.R")
assert_packages()
ensure_directories()

## Definitive test: does the WR efficiency score survive in the REAL XGBoost
## walk-forward, or is its +0.4% linear delta-R2 just noise a depth-3 booster
## can't use? Focused on the receiving targets (where script 92 found the
## effect); non-invasive -- builds an augmented copy of the cached feature
## frame, never modifies prepare_fantasy_player_features() itself.
##
## Built from the EXISTING production _r5 columns, so it inherits the
## season-boundary blend for free. EP denominator dropped: script 92 showed
## div vs none produced byte-identical results (dR2 +0.00383, p 0.000126 both).

features <- readRDS("data/processed/fantasy_prop_features.rds")
cat("cached feature frame:", nrow(features), "rows,", ncol(features), "cols\n")

need <- c("carries_r5", "targets_r5", "attempts_r5", "rushing_yards_r5", "receiving_yards_r5",
          "passing_yards_r5", "rushing_tds_r5", "receiving_tds_r5", "passing_tds_r5",
          "rushing_first_downs_r5", "receiving_first_downs_r5", "passing_first_downs_r5",
          "ppr_points_actual")
missing <- setdiff(need, names(features))
if (length(missing)) stop("missing required columns: ", paste(missing, collapse = ", "))

safe_div <- function(a, b) dplyr::coalesce(dplyr::if_else(b > 0, a / b, 0), 0)

build_score <- function(d, w) {
  touches <- dplyr::coalesce(d$carries_r5, 0) + dplyr::coalesce(d$targets_r5, 0) +
    dplyr::coalesce(d$attempts_r5, 0)
  rush_share <- safe_div(dplyr::coalesce(d$carries_r5, 0), touches)
  rec_share  <- safe_div(dplyr::coalesce(d$targets_r5, 0), touches)
  pass_share <- safe_div(dplyr::coalesce(d$attempts_r5, 0), touches)
  comp <- function(y, td, fd, t) safe_div(y, t) * w$yards + safe_div(td, t) * w$td + safe_div(fd, t) * w$fd
  rush_c <- comp(d$rushing_yards_r5,  d$rushing_tds_r5,  d$rushing_first_downs_r5,  d$carries_r5)
  rec_c  <- comp(d$receiving_yards_r5, d$receiving_tds_r5, d$receiving_first_downs_r5, d$targets_r5)
  pass_c <- comp(d$passing_yards_r5,  d$passing_tds_r5,  d$passing_first_downs_r5,  d$attempts_r5)
  list(score = rush_share * rush_c + rec_share * rec_c + pass_share * pass_c, touches = touches)
}

## ---- empirical weights, fit on TRAIN seasons only (< 2023) -----------------
ordered <- features |>
  dplyr::arrange(.data$player_id, .data$season, .data$week) |>
  dplyr::group_by(.data$player_id) |>
  dplyr::mutate(next_ppr = dplyr::lead(.data$ppr_points_actual)) |>
  dplyr::ungroup()

fit_pos_weights <- function(pos) {
  d <- ordered |> dplyr::filter(.data$position == pos, .data$season < 2023, !is.na(.data$next_ppr))
  touches <- dplyr::coalesce(d$carries_r5, 0) + dplyr::coalesce(d$targets_r5, 0) + dplyr::coalesce(d$attempts_r5, 0)
  rush_share <- safe_div(dplyr::coalesce(d$carries_r5, 0), touches)
  rec_share  <- safe_div(dplyr::coalesce(d$targets_r5, 0), touches)
  pass_share <- safe_div(dplyr::coalesce(d$attempts_r5, 0), touches)
  dd <- data.frame(
    next_ppr = d$next_ppr,
    y = rush_share * safe_div(d$rushing_yards_r5, d$carries_r5) +
        rec_share  * safe_div(d$receiving_yards_r5, d$targets_r5) +
        pass_share * safe_div(d$passing_yards_r5, d$attempts_r5),
    td = rush_share * safe_div(d$rushing_tds_r5, d$carries_r5) +
         rec_share  * safe_div(d$receiving_tds_r5, d$targets_r5) +
         pass_share * safe_div(d$passing_tds_r5, d$attempts_r5),
    fd = rush_share * safe_div(d$rushing_first_downs_r5, d$carries_r5) +
         rec_share  * safe_div(d$receiving_first_downs_r5, d$targets_r5) +
         pass_share * safe_div(d$passing_first_downs_r5, d$attempts_r5)
  )
  dd <- dd[is.finite(dd$next_ppr) & is.finite(dd$y) & is.finite(dd$td) & is.finite(dd$fd), ]
  stats::lm(next_ppr ~ y + td + fd, data = dd)
}

pos_fits <- lapply(c("QB", "RB", "WR", "TE"), fit_pos_weights)
names(pos_fits) <- c("QB", "RB", "WR", "TE")
cat("\n=== empirical weights (train < 2023), from production _r5 columns ===\n")
for (p in names(pos_fits)) {
  co <- stats::coef(pos_fits[[p]])
  cat(sprintf("  %-3s  yards=%+8.4f  td=%+8.4f  fd=%+8.4f\n", p, co["y"], co["td"], co["fd"]))
}

## build score per row using that row's position weights
sc <- rep(NA_real_, nrow(features))
tch <- rep(NA_real_, nrow(features))
for (p in names(pos_fits)) {
  idx <- which(features$position == p)
  if (!length(idx)) next
  co <- stats::coef(pos_fits[[p]])
  w <- list(yards = unname(co["y"]), td = unname(co["td"]), fd = unname(co["fd"]))
  built <- build_score(features[idx, , drop = FALSE], w)
  sc[idx] <- built$score
  tch[idx] <- built$touches
}
# scale to league-average 100 within position-season (wRC+ convention)
scale_df <- data.frame(sc = sc, position = features$position, season = features$season)
sc_scaled <- scale_df |>
  dplyr::group_by(.data$position, .data$season) |>
  dplyr::mutate(s = 100 * .data$sc / mean(.data$sc, na.rm = TRUE)) |>
  dplyr::pull(.data$s)

MIN_TOUCHES <- c(QB = 15, RB = 8, WR = 4, TE = 3)
thresh <- unname(MIN_TOUCHES[features$position])

feat_all <- features; feat_all$efficiency_score_r5 <- sc_scaled
feat_flt <- features; feat_flt$efficiency_score_r5 <- dplyr::if_else(tch >= thresh, sc_scaled, NA_real_)
cat("\nscore coverage: all-rows =", sum(is.finite(sc_scaled)),
    " | min-touch-filtered =", sum(is.finite(feat_flt$efficiency_score_r5)), "of", nrow(features), "\n")

## ---- focused walk-forward on the receiving targets -------------------------
specs <- fantasy_target_specifications()[c("receptions", "receiving_yards", "receiving_tds")]

run_wf <- function(feature_frame, label) {
  out <- list()
  for (tname in names(specs)) {
    spec <- specs[[tname]]
    td <- fantasy_target_candidates(feature_frame, spec$family)
    for (ts in 2023:2025) {
      tr <- td |> dplyr::filter(.data$season < ts, .data$prior_games >= 1)
      te <- td |> dplyr::filter(.data$season == ts, .data$prior_games >= 1)
      if (!nrow(tr) || !nrow(te)) next
      fitted <- fit_fantasy_stat_model(tr, te, spec$outcome, spec$objective,
                                        20260728L + ts + match(tname, names(specs)))
      # WR-only evaluation (that's where script 92 found the effect) + all-position
      res <- data.frame(target = tname, season = ts, position = te$position,
                        actual = as.numeric(te[[spec$outcome]]), prediction = fitted$prediction)
      out[[length(out) + 1L]] <- res
    }
    cat("   ", label, "-", tname, "done\n")
  }
  dplyr::bind_rows(out)
}

cat("\n=== running focused walk-forward (3 targets x 3 seasons x 3 variants) ===\n")
res_base <- run_wf(features, "BASELINE")
res_all  <- run_wf(feat_all,  "+score(all rows)")
res_flt  <- run_wf(feat_flt,  "+score(min-touch)")

summarise_res <- function(r, label, pos_filter = NULL) {
  d <- if (is.null(pos_filter)) r else r |> dplyr::filter(.data$position %in% pos_filter)
  d |> dplyr::group_by(.data$target) |>
    dplyr::summarise(
      variant = label,
      n = dplyr::n(),
      mae = mean(abs(.data$prediction - .data$actual)),
      rmse = sqrt(mean((.data$prediction - .data$actual)^2)),
      r2 = 1 - sum((.data$prediction - .data$actual)^2) / sum((.data$actual - mean(.data$actual))^2),
      .groups = "drop"
    )
}

for (posset in list(list(name = "WR only", p = "WR"), list(name = "ALL receiving positions", p = NULL))) {
  cat("\n\n================", posset$name, "================\n")
  cmp <- dplyr::bind_rows(
    summarise_res(res_base, "baseline", posset$p),
    summarise_res(res_all,  "+score(all)", posset$p),
    summarise_res(res_flt,  "+score(filtered)", posset$p)
  ) |> dplyr::arrange(.data$target, .data$variant)
  print(as.data.frame(cmp), digits = 6)

  cat("\n  --- deltas vs baseline (negative MAE/RMSE = better, positive R2 = better) ---\n")
  for (tname in unique(cmp$target)) {
    b <- cmp |> dplyr::filter(.data$target == tname, .data$variant == "baseline")
    for (v in c("+score(all)", "+score(filtered)")) {
      x <- cmp |> dplyr::filter(.data$target == tname, .data$variant == v)
      cat(sprintf("    %-16s %-18s  dMAE=%+.5f  dRMSE=%+.5f  dR2=%+.6f\n",
                  tname, v, x$mae - b$mae, x$rmse - b$rmse, x$r2 - b$r2))
    }
  }
}

cat("\n\nDone.\n")
