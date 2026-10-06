source("R/utilities.R")
source("R/fantasy_prop_model.R")
assert_packages()
ensure_directories()

## Follow-up to script 93. The composite score failed the walk-forward, but
## the fitted weights pointed somewhere specific: for WR, first-down rate per
## target carried essentially ALL the signal (+7.08) while yards/target
## (-0.018) and TD rate (+0.38) carried none. Script 91 found the same thing
## independently (WR fd_rate was the only significant component, p=0.00046).
##
## Raw first-down COUNTS are already features (receiving_first_downs_r5/r8).
## The RATE (first downs per target) is not -- and tree models are notoriously
## bad at representing division, so an explicit ratio feature is a real
## candidate even when both components are already present. That's a much
## narrower, cheaper test than the composite: one column, no weights to fit.

features <- readRDS("data/processed/fantasy_prop_features.rds")
safe_div <- function(a, b) dplyr::coalesce(dplyr::if_else(b > 0, a / b, NA_real_), NA_real_)

variants <- list(
  baseline = features,
  fd_rate = { f <- features; f$fd_per_target_r5 <- safe_div(f$receiving_first_downs_r5, f$targets_r5); f },
  ypt     = { f <- features; f$yards_per_target_r5 <- safe_div(f$receiving_yards_r5, f$targets_r5); f },
  both    = { f <- features
              f$fd_per_target_r5 <- safe_div(f$receiving_first_downs_r5, f$targets_r5)
              f$yards_per_target_r5 <- safe_div(f$receiving_yards_r5, f$targets_r5); f }
)

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
      out[[length(out) + 1L]] <- data.frame(
        target = tname, season = ts, position = te$position,
        actual = as.numeric(te[[spec$outcome]]), prediction = fitted$prediction)
    }
  }
  cat("  ", label, "done\n")
  dplyr::bind_rows(out)
}

res <- lapply(names(variants), function(n) run_wf(variants[[n]], n))
names(res) <- names(variants)

summarise_res <- function(r, label, pos_filter = NULL) {
  d <- if (is.null(pos_filter)) r else r |> dplyr::filter(.data$position %in% pos_filter)
  d |> dplyr::group_by(.data$target) |>
    dplyr::summarise(variant = label, n = dplyr::n(),
      mae = mean(abs(.data$prediction - .data$actual)),
      rmse = sqrt(mean((.data$prediction - .data$actual)^2)),
      r2 = 1 - sum((.data$prediction - .data$actual)^2) / sum((.data$actual - mean(.data$actual))^2),
      .groups = "drop")
}

for (posset in list(list(name = "WR only", p = "WR"), list(name = "ALL receiving positions", p = NULL))) {
  cat("\n\n================", posset$name, "================\n")
  cmp <- dplyr::bind_rows(lapply(names(res), function(n) summarise_res(res[[n]], n, posset$p)))
  for (tname in unique(cmp$target)) {
    b <- cmp |> dplyr::filter(.data$target == tname, .data$variant == "baseline")
    cat("\n ", tname, sprintf("(baseline MAE=%.5f RMSE=%.5f R2=%.6f)\n", b$mae, b$rmse, b$r2))
    for (v in setdiff(names(res), "baseline")) {
      x <- cmp |> dplyr::filter(.data$target == tname, .data$variant == v)
      cat(sprintf("    %-10s dMAE=%+.5f  dRMSE=%+.5f  dR2=%+.6f\n",
                  v, x$mae - b$mae, x$rmse - b$rmse, x$r2 - b$r2))
    }
  }
}
cat("\n\nDone.\n")
