## ---------------------------------------------------------------------------
## 84_build_variance_model.R
##
## Fits and validates the player-specific projection-spread model
## (R/fantasy_variance.R), then caches it for scripts/17.
##
## The comparison that decides whether this ships: today's position-constant
## bands vs the new player-specific bands, both scored out-of-sample on the same
## rows, on COVERAGE and SHARPNESS.
##
##   coverage = share of actuals landing inside p10..p90   (target 0.80)
##   width    = mean interval width                        (narrower is better)
##
## Neither number means anything alone - an interval of [0, 100] has perfect
## coverage and no information. The bar is: hold coverage at ~0.80 AND come in
## narrower, or improve coverage at equal width. Per projection bucket too, not
## just on average, because the whole defect being fixed is that the old bands
## were too wide at the bottom of the board and too narrow at the top, which
## averages out to looking fine.
##
## Walk-forward: fit on seasons strictly before the test season.
##
## Run:  & $rscript scripts/84_build_variance_model.R
## ---------------------------------------------------------------------------

source("R/utilities.R")
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})
source("R/dfs_value_backtest.R")
source("R/fantasy_salary_stack.R")
source("R/fantasy_variance.R")
assert_packages()
ensure_directories()

OUT_RDS <- "data/processed/fantasy_variance_model.rds"
OUT_CSV <- "outputs/fantasy_variance_validation.csv"

## ---------------------------------------------------------------------------
## 1. Walk-forward residuals + the features the scale model needs
## ---------------------------------------------------------------------------

fr <- fantasy_salary_training_frame(verbose = FALSE) |>
  filter(is.finite(.data$ppr_actual), is.finite(.data$ppr_model))

feat_path <- "data/processed/fantasy_prop_features.rds"
if (!file.exists(feat_path)) {
  stop("Missing ", feat_path,
       " - run scripts/17_build_fantasy_prop_models.R --retrain first.",
       call. = FALSE)
}
feats <- readRDS(feat_path) |>
  select(any_of(c("game_id", "player_id", "fantasy_points_ppr_sd5",
                  "opportunity_r5"))) |>
  distinct(.data$game_id, .data$player_id, .keep_all = TRUE)

d <- fr |> left_join(feats, by = c("game_id", "player_id"))
message("Rows: ", format(nrow(d), big.mark = ","),
        " | seasons: ", paste(range(d$season), collapse = "-"))

## ---------------------------------------------------------------------------
## 2. Walk-forward comparison
## ---------------------------------------------------------------------------

seasons <- sort(unique(d$season))
test_seasons <- seasons[-1]          # first season has nothing to train on
if (!length(test_seasons)) stop("Need at least two seasons.", call. = FALSE)

results <- list()
for (s in test_seasons) {
  train <- d |> filter(.data$season < s)
  test <- d |> filter(.data$season == s)
  if (nrow(train) < 500 || !nrow(test)) next

  ## --- baseline: position-constant residual quantiles (what ships today) ---
  pos_q <- train |>
    mutate(resid = .data$ppr_actual - .data$ppr_model) |>
    group_by(.data$position) |>
    summarise(p10 = quantile(.data$resid, 0.10, na.rm = TRUE),
              p90 = quantile(.data$resid, 0.90, na.rm = TRUE),
              .groups = "drop")
  base_test <- test |>
    left_join(pos_q, by = "position") |>
    mutate(base_lo = pmax(0, .data$ppr_model + .data$p10),
           base_hi = .data$ppr_model + .data$p90)

  ## --- new: player-specific scale ---
  model <- fit_fantasy_variance(train)
  if (is.null(model)) next
  new_test <- apply_fantasy_variance(
    base_test |> mutate(projected_ppr = .data$ppr_model),
    model, proj_col = "projected_ppr"
  )

  results[[as.character(s)]] <- new_test |>
    transmute(
      season = s, .data$position, .data$ppr_model, .data$ppr_actual,
      .data$base_lo, .data$base_hi,
      new_lo = .data$ppr_low, new_hi = .data$ppr_high, .data$sim_sd
    )
}
res <- bind_rows(results)
if (!nrow(res)) stop("No walk-forward folds produced results.", call. = FALSE)

score <- function(lo, hi, actual) {
  inside <- actual >= lo & actual <= hi
  c(coverage = mean(inside, na.rm = TRUE), width = mean(hi - lo, na.rm = TRUE))
}

cat("\n================ INTERVAL VALIDATION ================\n")
cat("\n-- overall (target coverage 0.80) --\n")
ov <- rbind(
  data.frame(method = "position-constant (current)",
             t(score(res$base_lo, res$base_hi, res$ppr_actual))),
  data.frame(method = "player-specific (new)",
             t(score(res$new_lo, res$new_hi, res$ppr_actual)))
)
ov$coverage <- round(ov$coverage, 4); ov$width <- round(ov$width, 2)
print(ov, row.names = FALSE)

cat("\n-- by projection bucket: where the old bands actually fail --\n")
res$bucket <- ntile(res$ppr_model, 6)
by_bucket <- res |>
  group_by(.data$bucket) |>
  summarise(
    n = n(),
    mean_proj = round(mean(.data$ppr_model), 2),
    cov_old = round(mean(.data$ppr_actual >= .data$base_lo &
                           .data$ppr_actual <= .data$base_hi), 3),
    cov_new = round(mean(.data$ppr_actual >= .data$new_lo &
                           .data$ppr_actual <= .data$new_hi), 3),
    width_old = round(mean(.data$base_hi - .data$base_lo), 2),
    width_new = round(mean(.data$new_hi - .data$new_lo), 2),
    .groups = "drop"
  )
print(as.data.frame(by_bucket), row.names = FALSE)
cat("\n  Old coverage should drift ABOVE 0.80 in the low buckets (bands too wide)\n",
    " and BELOW it in the high buckets (too narrow). New should sit near 0.80\n",
    " across all of them - that flatness is the entire point.\n")

cat("\n-- by season --\n")
by_season <- res |>
  group_by(.data$season) |>
  summarise(
    n = n(),
    cov_old = round(mean(.data$ppr_actual >= .data$base_lo &
                           .data$ppr_actual <= .data$base_hi), 3),
    cov_new = round(mean(.data$ppr_actual >= .data$new_lo &
                           .data$ppr_actual <= .data$new_hi), 3),
    width_old = round(mean(.data$base_hi - .data$base_lo), 2),
    width_new = round(mean(.data$new_hi - .data$new_lo), 2),
    .groups = "drop"
  )
print(as.data.frame(by_season), row.names = FALSE)

cat("\n-- does sim_sd actually separate players at the same projection? --\n")
mid <- res |> filter(.data$ppr_model >= 9, .data$ppr_model <= 12)
cat(sprintf("  projections 9-12: %s rows, sim_sd range %.2f - %.2f, sd of sim_sd %.2f\n",
            format(nrow(mid), big.mark = ","), min(mid$sim_sd), max(mid$sim_sd),
            stats::sd(mid$sim_sd)))
cat("  (the old method gives ONE width per position here, by construction)\n")

readr::write_csv(by_bucket, OUT_CSV)

## ---------------------------------------------------------------------------
## 3. Fit on everything and cache
## ---------------------------------------------------------------------------

final <- fit_fantasy_variance(d)
if (is.null(final)) stop("Final variance fit failed.", call. = FALSE)
saveRDS(final, OUT_RDS)
cat("\nWrote ", OUT_RDS, " and ", OUT_CSV, "\n", sep = "")
cat("\n-- scale model coefficients (log link; positive = wider spread) --\n")
print(round(stats::coef(final$scale_fit), 4))
cat("\n-- standardized z quantiles by position (shape, incl. skew) --\n")
print(final$z_quantiles, row.names = FALSE)
