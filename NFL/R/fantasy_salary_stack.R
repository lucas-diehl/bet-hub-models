## ---------------------------------------------------------------------------
## R/fantasy_salary_stack.R
##
## Blends DraftKings salary into the fantasy point projection.
##
## Why at the PPR level and not as a per-target feature: the 9 component models
## predict receptions, rushing yards, passing TDs and so on, but salary is a
## price on the WHOLE player-week. It has no natural per-component counterpart,
## and the measured gain was measured at the total-points level -
## scripts/80_salary_benchmark.R, 2023-2025 walk-forward, 9,873 common-support
## player-weeks:
##
##     naive rolling-5     R2 0.3340
##     salary alone        R2 0.3864
##     model alone         R2 0.3988
##     model + salary      R2 0.4131
##
## So the model does beat salary on its own (+0.0124), but salary still carries
## +0.0144 R2 that the model is blind to. This file captures that residual.
##
## Per position, deliberately. The same benchmark by position:
##
##     QB   salary 0.2603  model 0.3005  stack 0.3239
##     RB   salary 0.4303  model 0.4417  stack 0.4592
##     TE   salary 0.2730  model 0.3067  stack 0.3100
##     WR   salary 0.3321  model 0.3283  stack 0.3428   <- model LOSES to salary
##
## A single pooled weight would split the difference between a position where
## the model is clearly ahead (QB) and the one where it is behind (WR), which
## is exactly the case that needs different treatment.
##
## Leak discipline: both stages are fit on the walk-forward predictions, which
## were themselves produced out-of-sample, and the salary->points stage is fit
## per position on that same frame. Nothing here sees a game it is scoring.
## ---------------------------------------------------------------------------

## Reconstruct PPR from the 9 component walk-forward predictions and attach
## historical DK salary. Used by BOTH scripts/80 (benchmarking) and the stack
## fit in scripts/17, deliberately: two copies of a name-matching join is how
## the measured number and the shipped number quietly stop meaning the same
## thing.
fantasy_salary_training_frame <- function(
  wf_path = "data/processed/fantasy_prop_walk_forward.rds",
  salary_path = "outputs/dfs_salaries_dk_2022_plus.csv",
  stats_path = NULL,
  verbose = TRUE
) {
  if (!file.exists(wf_path)) {
    stop("Missing ", wf_path,
         " - run scripts/17_build_fantasy_prop_models.R --retrain first.",
         call. = FALSE)
  }
  preds <- readRDS(wf_path)$predictions

  ppr_from <- function(d, prefix) {
    g <- function(target) {
      col <- paste0(prefix, "_", target)
      if (col %in% names(d)) dplyr::coalesce(d[[col]], 0) else 0
    }
    0.04 * g("passing_yards") + 4 * g("passing_tds") - 2 * g("interceptions") +
      0.1 * g("rushing_yards") + 6 * g("rushing_tds") +
      g("receptions") + 0.1 * g("receiving_yards") + 6 * g("receiving_tds") -
      2 * g("fumbles_lost")
  }

  wide <- preds |>
    dplyr::select("game_id", "season", "week", "player_id", "position",
                  "target", "actual", "prediction", "baseline") |>
    tidyr::pivot_wider(
      names_from = "target",
      values_from = c("actual", "prediction", "baseline"),
      values_fill = 0
    )

  ppr <- wide |>
    dplyr::mutate(
      ppr_actual = ppr_from(wide, "actual"),
      ppr_model = ppr_from(wide, "prediction"),
      ppr_naive = ppr_from(wide, "baseline")
    ) |>
    dplyr::select("game_id", "season", "week", "player_id", "position",
                  "ppr_actual", "ppr_model", "ppr_naive")

  if (is.null(stats_path)) {
    stats_path <- if (file.exists("data/raw/player_stats_2021_2026.rds")) {
      "data/raw/player_stats_2021_2026.rds"
    } else {
      "data/raw/player_stats_2021_2025.rds"
    }
  }
  ps <- readRDS(stats_path) |>
    dplyr::transmute(
      game_id = .data$game_id, player_id = .data$player_id,
      player_display_name = .data$player_display_name,
      team = normalize_team(.data$team)
    ) |>
    dplyr::distinct(.data$game_id, .data$player_id, .keep_all = TRUE)
  ppr <- ppr |> dplyr::left_join(ps, by = c("game_id", "player_id"))

  salaries <- readr::read_csv(salary_path, show_col_types = FALSE,
                              guess_max = 50000) |>
    dplyr::filter(toupper(.data$site) == "DK",
                  .data$position %in% c("QB", "RB", "WR", "TE")) |>
    dplyr::transmute(
      season = as.integer(.data$season), week = as.integer(.data$week),
      team = normalize_team(.data$team), position = .data$position,
      player_name_key = dfs_player_name_key(.data$player_name),
      salary = as.numeric(.data$salary)
    ) |>
    dplyr::filter(is.finite(.data$salary), .data$salary > 0) |>
    dplyr::arrange(.data$season, .data$week, .data$team, .data$position,
                   dplyr::desc(.data$salary)) |>
    dplyr::distinct(.data$season, .data$week, .data$team, .data$position,
                    .data$player_name_key, .keep_all = TRUE)

  joined <- ppr |>
    dplyr::mutate(player_name_key = dfs_player_name_key(.data$player_display_name)) |>
    dplyr::left_join(salaries, by = c("season", "week", "team", "position",
                                      "player_name_key")) |>
    dplyr::mutate(match_method = dplyr::if_else(!is.na(.data$salary),
                                                "EXACT", NA_character_))

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
  joined$match_method <- dplyr::coalesce(joined$match_method, "UNMATCHED")

  if (verbose) {
    message(sprintf(
      "Salary training frame: %s player-weeks, %.1f%% salary-matched.",
      format(nrow(joined), big.mark = ","),
      100 * mean(joined$match_method != "UNMATCHED")
    ))
  }
  joined
}

## Fit stage 1 (salary -> points, per position) and stage 2 (model + salary
## stack, per position). Returns an object cached alongside the models.
fit_fantasy_salary_stack <- function(scored, min_rows = 200L) {
  needed <- c("position", "ppr_actual", "ppr_model", "salary")
  missing <- setdiff(needed, names(scored))
  if (length(missing)) {
    stop("fit_fantasy_salary_stack() needs columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  usable <- scored[
    is.finite(scored$ppr_actual) & is.finite(scored$ppr_model) &
      is.finite(scored$salary) & scored$salary > 0,
    ,
    drop = FALSE
  ]
  positions <- sort(unique(usable$position))

  fits <- list()
  for (pos in positions) {
    d <- usable[usable$position == pos, , drop = FALSE]
    if (nrow(d) < min_rows) next

    salary_fit <- stats::lm(ppr_actual ~ salary, data = d)
    d$ppr_salary <- stats::predict(salary_fit, newdata = d)

    stack_fit <- stats::lm(ppr_actual ~ ppr_model + ppr_salary, data = d)
    cf <- stats::coef(stack_fit)

    ## Guard against a degenerate stack. If either leg comes back with a
    ## negative weight the blend is extrapolating rather than averaging, which
    ## is how a stack turns a good model into a worse one out of sample. Fall
    ## back to model-only for that position instead of shipping it.
    w_model <- unname(cf[["ppr_model"]])
    w_salary <- unname(cf[["ppr_salary"]])
    degenerate <- !is.finite(w_model) || !is.finite(w_salary) ||
      w_model < 0 || w_salary < 0

    fits[[pos]] <- list(
      position = pos,
      n = nrow(d),
      salary_coef = stats::coef(salary_fit),
      stack_coef = cf,
      degenerate = degenerate,
      in_sample_r2_model = summary(stats::lm(ppr_actual ~ ppr_model, data = d))$r.squared,
      in_sample_r2_stack = summary(stack_fit)$r.squared
    )
  }

  list(
    fits = fits,
    positions = names(fits),
    fitted_at = Sys.time(),
    n_rows = nrow(usable)
  )
}

## Apply the stack to a projection board. Returns the board with
## `projected_ppr_model` (the untouched model number, kept for comparison),
## `projected_ppr` (the stacked number where salary was available), and
## `salary_stack_applied` so downstream consumers can tell which rows moved.
##
## Degrades to model-only, row by row, whenever salary is missing - which is
## the normal case for a player who is not on the DK main slate, and the case
## for EVERY row if the weekly capture did not run. A projection that silently
## became model-only would be indistinguishable from a working stack, so the
## flag and the message below exist to make that visible.
apply_fantasy_salary_stack <- function(board, stack, salary_lookup = NULL) {
  if (is.null(stack) || !length(stack$fits)) {
    board$projected_ppr_model <- board$projected_ppr
    board$salary_stack_applied <- FALSE
    message("Salary stack: no fitted stack available; projections are model-only.")
    return(board)
  }

  if (!is.null(salary_lookup)) {
    board <- dplyr::left_join(board, salary_lookup, by = "salary_join_key")
  }
  if (!"salary" %in% names(board)) board$salary <- NA_real_

  board$projected_ppr_model <- board$projected_ppr
  board$salary_stack_applied <- FALSE

  for (pos in names(stack$fits)) {
    f <- stack$fits[[pos]]
    if (isTRUE(f$degenerate)) next
    idx <- which(
      board$position == pos & is.finite(board$salary) & board$salary > 0 &
        is.finite(board$projected_ppr_model)
    )
    if (!length(idx)) next

    sc <- f$salary_coef
    ppr_salary <- sc[["(Intercept)"]] + sc[["salary"]] * board$salary[idx]
    st <- f$stack_coef
    stacked <- st[["(Intercept)"]] +
      st[["ppr_model"]] * board$projected_ppr_model[idx] +
      st[["ppr_salary"]] * ppr_salary

    board$projected_ppr[idx] <- pmax(0, stacked)
    board$salary_stack_applied[idx] <- TRUE
  }

  n_applied <- sum(board$salary_stack_applied)
  message(sprintf(
    "Salary stack: applied to %d of %d projected players (%.0f%%).",
    n_applied, nrow(board), 100 * n_applied / max(1, nrow(board))
  ))
  if (n_applied == 0) {
    message("  No salaries matched. Run scripts/21 then scripts/81 to refresh; ",
            "projections are model-only until then.")
  }
  board
}

## Build a (key -> salary) lookup for a target week from the current salary
## table, keyed the same way the board is. Name-keyed within team+position,
## reusing dfs_player_name_key() so this matches the historical join used to
## fit the stack rather than inventing a second convention.
fantasy_salary_lookup <- function(season, week,
                                  path = "outputs/dfs_salaries_dk_current.csv") {
  if (!file.exists(path)) {
    message("Salary stack: no current salary table at ", path, ".")
    return(NULL)
  }
  sal <- readr::read_csv(path, show_col_types = FALSE, guess_max = 50000)
  sal <- sal |>
    dplyr::filter(
      toupper(.data$site) == "DK",
      as.integer(.data$season) == as.integer(!!season),
      as.integer(.data$week) == as.integer(!!week),
      .data$position %in% c("QB", "RB", "WR", "TE"),
      is.finite(.data$salary), .data$salary > 0
    )
  if (!nrow(sal)) {
    message("Salary stack: salary table has no rows for ", season,
            " week ", week, ".")
    return(NULL)
  }
  sal |>
    dplyr::mutate(
      salary_join_key = paste(
        normalize_team(.data$team), .data$position,
        dfs_player_name_key(.data$player_name), sep = "|"
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$salary)) |>
    dplyr::distinct(.data$salary_join_key, .keep_all = TRUE) |>
    dplyr::select("salary_join_key", "salary")
}
