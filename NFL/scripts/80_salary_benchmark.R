## ---------------------------------------------------------------------------
## 80_salary_benchmark.R
##
## The honest scorecard: how much of weekly fantasy scoring does DraftKings
## salary already explain, and does our model explain anything beyond it?
##
## Why this exists. The premise behind the projection work is that salary is an
## extremely strong composite prior - DK's pricing already embeds Vegas totals,
## recent usage, injury news and depth chart - so a projection model that only
## matches salary is an expensive way to reprint a number DraftKings publishes
## for free. The reported figures that motivated this (salary alone R2 ~0.401,
## perfect knowledge R2 ~0.457, leaving ~0.056 of headroom) come from a
## different dataset and site-scoring, so nothing here inherits them. This
## script computes OUR OWN versions of those numbers on OUR OWN data and
## scoring, which is the only version that can legitimately gate our work.
##
## Measurement only - no training, no refitting of the component models. It
## reads the walk-forward predictions scripts/17 already produced.
##
## Four predictors are compared on exactly the same rows:
##   1. salary_only  - points ~ salary, fit per position, walk-forward
##   2. naive_r5     - the model's existing baseline (trailing 5-game mean)
##   3. model        - our blended component model, summed to PPR
##   4. model+salary - OLS stack of (3) and (1), the incremental-value test
##
## (4) is the one that answers the real question. If stacking salary onto the
## model beats the model alone by a wide margin, the model is missing what
## salary knows. If the model alone is close to the stack, it has already
## captured it.
##
## Run:  & $rscript scripts/80_salary_benchmark.R
## ---------------------------------------------------------------------------

source("R/utilities.R")
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})
source("R/dfs_value_backtest.R")   # dfs_player_name_key(), fuzzy_match_salary_index()
source("R/fantasy_prop_model.R")   # fantasy_target_specifications()

WF_PATH <- "data/processed/fantasy_prop_walk_forward.rds"
SALARY_DK <- "outputs/dfs_salaries_dk_2022_plus.csv"
OUT_PATH <- "outputs/fantasy_salary_benchmark.csv"

if (!file.exists(WF_PATH)) {
  stop("Missing ", WF_PATH, " - run scripts/17_build_fantasy_prop_models.R --retrain first.",
       call. = FALSE)
}

## ---------------------------------------------------------------------------
## 1. Reconstruct PPR per player-week from the component predictions
##
## There is no single "total points" model - PPR is summed from the 9 component
## targets. Same scoring weights scripts/17 uses so the numbers are comparable.
## ---------------------------------------------------------------------------

wf <- readRDS(WF_PATH)
preds <- wf$predictions

ppr_from <- function(d, prefix) {
  g <- function(target) {
    col <- paste0(prefix, "_", target)
    if (col %in% names(d)) coalesce(d[[col]], 0) else 0
  }
  0.04 * g("passing_yards") + 4 * g("passing_tds") - 2 * g("interceptions") +
    0.1 * g("rushing_yards") + 6 * g("rushing_tds") +
    g("receptions") + 0.1 * g("receiving_yards") + 6 * g("receiving_tds") -
    2 * g("fumbles_lost")
}

wide <- preds |>
  select("game_id", "season", "week", "player_id", "position",
         "target", "actual", "prediction", "baseline") |>
  pivot_wider(
    names_from = "target",
    values_from = c("actual", "prediction", "baseline"),
    values_fill = 0
  )

ppr <- wide |>
  mutate(
    ppr_actual = ppr_from(wide, "actual"),
    ppr_model = ppr_from(wide, "prediction"),
    ppr_naive = ppr_from(wide, "baseline")
  ) |>
  select("game_id", "season", "week", "player_id", "position",
         "ppr_actual", "ppr_model", "ppr_naive")

message("Walk-forward player-weeks: ", format(nrow(ppr), big.mark = ","))

## ---------------------------------------------------------------------------
## 2. Attach player name + team, then DK salary
##
## The salary files key on player NAME; everything model-side keys on GSIS
## player_id, so the join has to go through a normalized name within
## (season, week, team, position). Reusing dfs_player_name_key() and
## fuzzy_match_salary_index() from R/dfs_value_backtest.R rather than writing a
## second name-matching implementation that would drift from the first.
## ---------------------------------------------------------------------------

stats_path <- if (file.exists("data/raw/player_stats_2021_2026.rds")) {
  "data/raw/player_stats_2021_2026.rds"
} else {
  "data/raw/player_stats_2021_2025.rds"
}
ps <- readRDS(stats_path) |>
  transmute(
    game_id = .data$game_id,
    player_id = .data$player_id,
    player_display_name = .data$player_display_name,
    team = normalize_team(.data$team)
  ) |>
  distinct(.data$game_id, .data$player_id, .keep_all = TRUE)

ppr <- ppr |> left_join(ps, by = c("game_id", "player_id"))
message("  name/team attached on ",
        sprintf("%.1f%%", 100 * mean(!is.na(ppr$player_display_name))), " of rows.")

salaries <- readr::read_csv(SALARY_DK, show_col_types = FALSE, guess_max = 50000) |>
  filter(toupper(.data$site) == "DK",
         .data$position %in% c("QB", "RB", "WR", "TE")) |>
  transmute(
    season = as.integer(.data$season),
    week = as.integer(.data$week),
    team = normalize_team(.data$team),
    position = .data$position,
    player_name = .data$player_name,
    player_name_key = dfs_player_name_key(.data$player_name),
    salary = as.numeric(.data$salary)
  ) |>
  filter(is.finite(.data$salary), .data$salary > 0) |>
  arrange(.data$season, .data$week, .data$team, .data$position, desc(.data$salary)) |>
  distinct(.data$season, .data$week, .data$team, .data$position,
           .data$player_name_key, .keep_all = TRUE)

joined <- ppr |>
  mutate(player_name_key = dfs_player_name_key(.data$player_display_name)) |>
  left_join(
    salaries |> select("season", "week", "team", "position",
                       "player_name_key", "salary"),
    by = c("season", "week", "team", "position", "player_name_key")
  ) |>
  mutate(match_method = if_else(!is.na(.data$salary), "EXACT", NA_character_))

## Fuzzy pass for the rows exact matching missed (nicknames, punctuation,
## suffix drift). Same distance rule the DFS backtest uses.
unmatched <- which(is.na(joined$salary))
if (length(unmatched)) {
  sal <- as.data.frame(salaries)
  for (i in unmatched) {
    cand <- which(sal$season == joined$season[[i]] &
                    sal$week == joined$week[[i]] &
                    sal$team == joined$team[[i]] &
                    sal$position == joined$position[[i]])
    j <- fuzzy_match_salary_index(joined$player_name_key[[i]],
                                  sal$player_name_key[cand])
    if (is.na(j)) next
    joined$salary[[i]] <- sal$salary[cand[[j]]]
    joined$match_method[[i]] <- "FUZZY"
  }
}
joined$match_method <- coalesce(joined$match_method, "UNMATCHED")

cat("\n-- salary match rate by season --\n")
print(as.data.frame(
  joined |> group_by(season) |>
    summarise(player_weeks = n(),
              matched = sum(match_method != "UNMATCHED"),
              match_rate = round(mean(match_method != "UNMATCHED"), 3),
              .groups = "drop")
), row.names = FALSE)

matched <- joined |> filter(.data$match_method != "UNMATCHED",
                            is.finite(.data$ppr_actual))
message("\nMatched rows used for the benchmark: ",
        format(nrow(matched), big.mark = ","))
if (!nrow(matched)) stop("No salary-matched rows; cannot benchmark.", call. = FALSE)

## ---------------------------------------------------------------------------
## 3. Walk-forward salary-only baseline
##
## Fit points ~ salary separately per position, training only on seasons
## STRICTLY BEFORE the season being scored - the same leak discipline the
## component models use. A single pooled fit would leak cross-season salary
## inflation into its own evaluation.
## ---------------------------------------------------------------------------

test_seasons <- sort(unique(matched$season))
salary_pred <- vector("list", length(test_seasons))

for (k in seq_along(test_seasons)) {
  s <- test_seasons[[k]]
  train <- matched |> filter(.data$season < s)
  test <- matched |> filter(.data$season == s)
  if (!nrow(train)) {
    ## Earliest season has no prior year to fit on; fall back to fitting on
    ## itself and flag it, rather than dropping the season silently.
    train <- test
    test$salary_fit_on_self <- TRUE
  } else {
    test$salary_fit_on_self <- FALSE
  }
  test$ppr_salary <- NA_real_
  for (pos in unique(test$position)) {
    tr <- train |> filter(.data$position == pos)
    idx <- which(test$position == pos)
    if (nrow(tr) < 30 || !length(idx)) next
    fit <- stats::lm(ppr_actual ~ salary, data = tr)
    test$ppr_salary[idx] <- stats::predict(fit, newdata = test[idx, ])
  }
  salary_pred[[k]] <- test
}
bench <- bind_rows(salary_pred) |> filter(is.finite(.data$ppr_salary))

## ---------------------------------------------------------------------------
## 4. Score every predictor on identical rows
## ---------------------------------------------------------------------------

metrics <- function(actual, pred, label) {
  ok <- is.finite(actual) & is.finite(pred)
  a <- actual[ok]; p <- pred[ok]
  ss_res <- sum((a - p)^2)
  ss_tot <- sum((a - mean(a))^2)
  tibble::tibble(
    predictor = label,
    n = length(a),
    mae = mean(abs(a - p)),
    rmse = sqrt(mean((a - p)^2)),
    r_squared = 1 - ss_res / ss_tot,
    bias = mean(p - a)
  )
}

## The stack: does adding salary to the model improve on the model alone?
## Fit walk-forward too, so this is an honest out-of-sample comparison.
bench$ppr_stack <- NA_real_
for (s in test_seasons) {
  tr <- bench |> filter(.data$season < s)
  idx <- which(bench$season == s)
  if (nrow(tr) < 50 || !length(idx)) next
  fit <- stats::lm(ppr_actual ~ ppr_model + ppr_salary, data = tr)
  bench$ppr_stack[idx] <- stats::predict(fit, newdata = bench[idx, ])
}

## Apples-to-apples: the stack needs a prior season to fit, so it scores no
## rows in the earliest season. Comparing an R2 computed on 14.9k rows against
## one computed on 9.9k different rows is not a comparison. Restrict the
## headline table to rows where ALL four predictors exist.
common <- bench |>
  filter(is.finite(.data$ppr_salary), is.finite(.data$ppr_naive),
         is.finite(.data$ppr_model), is.finite(.data$ppr_stack))
message("Common-support rows (all four predictors): ",
        format(nrow(common), big.mark = ","))

overall <- bind_rows(
  metrics(common$ppr_actual, common$ppr_salary, "salary_only"),
  metrics(common$ppr_actual, common$ppr_naive, "naive_r5"),
  metrics(common$ppr_actual, common$ppr_model, "model"),
  metrics(common$ppr_actual, common$ppr_stack, "model_plus_salary")
)

by_season <- bind_rows(lapply(test_seasons, function(s) {
  b <- bench |> filter(.data$season == s)
  bind_rows(
    metrics(b$ppr_actual, b$ppr_salary, "salary_only"),
    metrics(b$ppr_actual, b$ppr_naive, "naive_r5"),
    metrics(b$ppr_actual, b$ppr_model, "model"),
    metrics(b$ppr_actual, b$ppr_stack, "model_plus_salary")
  ) |> mutate(season = s, .before = 1)
}))

## Also on common support, so the per-position verdicts are directly comparable.
by_position <- bind_rows(lapply(sort(unique(common$position)), function(pos) {
  b <- common |> filter(.data$position == pos)
  bind_rows(
    metrics(b$ppr_actual, b$ppr_salary, "salary_only"),
    metrics(b$ppr_actual, b$ppr_naive, "naive_r5"),
    metrics(b$ppr_actual, b$ppr_model, "model"),
    metrics(b$ppr_actual, b$ppr_stack, "model_plus_salary")
  ) |> mutate(position = pos, .before = 1)
}))

readr::write_csv(
  bind_rows(
    overall |> mutate(scope = "overall", .before = 1),
    by_season |> mutate(scope = paste0("season_", .data$season)) |>
      select(-"season") |> select("scope", everything()),
    by_position |> mutate(scope = paste0("position_", .data$position)) |>
      select(-"position") |> select("scope", everything())
  ),
  OUT_PATH
)

cat("\n================ SALARY BENCHMARK ================\n")
cat("\n-- overall, ", format(nrow(bench), big.mark = ","),
    " salary-matched player-weeks --\n", sep = "")
print(as.data.frame(overall |> mutate(across(where(is.numeric), ~ round(.x, 4)))),
      row.names = FALSE)

cat("\n-- by season --\n")
print(as.data.frame(by_season |> mutate(across(where(is.numeric), ~ round(.x, 4)))),
      row.names = FALSE)

cat("\n-- by position --\n")
print(as.data.frame(by_position |> mutate(across(where(is.numeric), ~ round(.x, 4)))),
      row.names = FALSE)

r2 <- setNames(overall$r_squared, overall$predictor)
cat("\n-- the headline numbers --\n")
cat(sprintf("  salary alone explains        R2 = %.4f\n", r2[["salary_only"]]))
cat(sprintf("  our model explains           R2 = %.4f\n", r2[["model"]]))
cat(sprintf("  model + salary stacked       R2 = %.4f\n", r2[["model_plus_salary"]]))
cat(sprintf("  model's edge over salary     %+.4f R2\n",
            r2[["model"]] - r2[["salary_only"]]))
cat(sprintf("  salary's residual value      %+.4f R2 (stack over model alone)\n",
            r2[["model_plus_salary"]] - r2[["model"]]))
cat("\n  Read it this way: if the model already beats salary AND stacking salary\n",
    " on top adds little, the model has genuinely absorbed what DK pricing\n",
    " knows and is adding its own signal. If stacking adds a lot, salary still\n",
    " holds information the model is blind to - and that gap is the roadmap.\n")
cat("\nWrote ", OUT_PATH, "\n", sep = "")
