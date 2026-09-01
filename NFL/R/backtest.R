walk_forward_predictions <- function(data, cfg) {
  seasons <- seq(cfg$backtest$start_season, cfg$backtest$end_season)
  models <- unlist(cfg$models$enabled)
  targets <- c(home_margin = "margin", game_total = "total")
  out <- list()
  k <- 1L

  set.seed(cfg$backtest$seed)
  for (test_season in seasons) {
    train <- dplyr::filter(data, .data$season < test_season)
    test <- dplyr::filter(data, .data$season == test_season)
    if (!nrow(test) || nrow(train) < 500) next

    for (target in names(targets)) {
      market_col <- if (target == "home_margin") "market_margin" else "market_total"
      out[[k]] <- test |>
        dplyr::transmute(
          .data$game_id, .data$season, .data$week, .data$game_date,
          .data$home_team, .data$away_team,
          target = target,
          model = "sportsbook",
          truth = .data[[target]],
          prediction = .data[[market_col]],
          home_line = .data$home_line,
          total_line = .data$total_line
        )
      k <- k + 1L

      for (model_name in models) {
        message("Season ", test_season, "; ", target, "; ", model_name)
        pred <- fit_predict_model(model_name, train, test, target, cfg)
        out[[k]] <- test |>
          dplyr::transmute(
            .data$game_id, .data$season, .data$week, .data$game_date,
            .data$home_team, .data$away_team,
            target = target,
            model = model_name,
            truth = .data[[target]],
            prediction = pred,
            home_line = .data$home_line,
            total_line = .data$total_line
          )
        k <- k + 1L
      }
    }
  }
  dplyr::bind_rows(out)
}

score_predictions <- function(predictions) {
  predictions |>
    dplyr::group_by(.data$target, .data$model, .data$season) |>
    dplyr::summarise(
      games = dplyr::n(),
      mae = mae_vec(.data$truth, .data$prediction),
      rmse = rmse_vec(.data$truth, .data$prediction),
      r_squared = r_squared_vec(.data$truth, .data$prediction),
      bias = mean(.data$prediction - .data$truth, na.rm = TRUE),
      .groups = "drop"
    )
}

grade_edges <- function(predictions, threshold, american_odds = -110) {
  predictions |>
    dplyr::filter(.data$model != "sportsbook") |>
    dplyr::mutate(
      market_prediction = dplyr::if_else(
        .data$target == "home_margin", -.data$home_line, .data$total_line
      ),
      edge = .data$prediction - .data$market_prediction,
      bet_side = dplyr::case_when(
        abs(.data$edge) < threshold ~ "pass",
        .data$target == "home_margin" & .data$edge > 0 ~ "home",
        .data$target == "home_margin" ~ "away",
        .data$edge > 0 ~ "over",
        TRUE ~ "under"
      ),
      raw_result = dplyr::case_when(
        .data$target == "home_margin" ~ .data$truth + .data$home_line,
        TRUE ~ .data$truth - .data$total_line
      ),
      bet_result = dplyr::case_when(
        .data$bet_side == "pass" ~ NA_real_,
        .data$bet_side %in% c("home", "over") ~ signed_result(.data$raw_result),
        TRUE ~ -signed_result(.data$raw_result)
      ),
      profit = american_profit(.data$bet_result, american_odds)
    )
}

threshold_table <- function(predictions, cfg) {
  purrr::map_dfr(unlist(cfg$backtest$edge_thresholds), function(threshold) {
    grade_edges(predictions, threshold, cfg$backtest$american_odds) |>
      dplyr::filter(.data$bet_side != "pass") |>
      dplyr::group_by(.data$target, .data$model) |>
      dplyr::summarise(
        threshold = threshold,
        bets = dplyr::n(),
        wins = sum(.data$bet_result > 0, na.rm = TRUE),
        losses = sum(.data$bet_result < 0, na.rm = TRUE),
        pushes = sum(.data$bet_result == 0, na.rm = TRUE),
        win_rate = .data$wins / (.data$wins + .data$losses),
        profit_units = sum(.data$profit, na.rm = TRUE),
        roi = .data$profit_units / .data$bets,
        .groups = "drop"
      )
  })
}

select_thresholds_walk_forward <- function(predictions, cfg) {
  bets <- selected_threshold_bets_walk_forward(predictions, cfg)
  if (!nrow(bets)) return(dplyr::tibble())

  bets |>
    dplyr::group_by(
      .data$season, .data$target, .data$model, .data$selected_threshold,
      .data$validation_start, .data$validation_end,
      .data$validation_bets, .data$validation_roi
    ) |>
    dplyr::summarise(
      bets = dplyr::n(),
      wins = sum(.data$bet_result > 0, na.rm = TRUE),
      losses = sum(.data$bet_result < 0, na.rm = TRUE),
      pushes = sum(.data$bet_result == 0, na.rm = TRUE),
      profit_units = sum(.data$profit, na.rm = TRUE),
      roi = .data$profit_units / .data$bets,
      .groups = "drop"
    )
}

selected_threshold_bets_walk_forward <- function(predictions, cfg) {
  seasons <- sort(unique(predictions$season))
  models <- setdiff(unique(predictions$model), "sportsbook")
  targets <- unique(predictions$target)

  purrr::map_dfr(seasons, function(test_season) {
    validation_years <- tail(
      seasons[seasons < test_season],
      cfg$backtest$validation_seasons
    )
    if (length(validation_years) < cfg$backtest$validation_seasons) {
      return(dplyr::tibble())
    }

    purrr::map_dfr(targets, function(target_name) {
      purrr::map_dfr(models, function(model_name) {
        validation <- predictions |>
          dplyr::filter(
            .data$season %in% validation_years,
            .data$target == target_name,
            .data$model == model_name
          )
        candidates <- threshold_table(validation, cfg) |>
          dplyr::filter(.data$bets >= cfg$backtest$minimum_bets) |>
          dplyr::arrange(dplyr::desc(.data$roi), dplyr::desc(.data$bets))
        if (!nrow(candidates)) return(dplyr::tibble())

        chosen <- candidates[1, ]
        current <- predictions |>
          dplyr::filter(
            .data$season == test_season,
            .data$target == target_name,
            .data$model == model_name
          ) |>
          grade_edges(chosen$threshold, cfg$backtest$american_odds) |>
          dplyr::filter(.data$bet_side != "pass")

        current |>
          dplyr::mutate(
          selected_threshold = chosen$threshold,
          validation_start = min(validation_years),
          validation_end = max(validation_years),
          validation_bets = chosen$bets,
          validation_roi = chosen$roi
        )
      })
    })
  })
}

max_losing_streak <- function(result) {
  losses <- result < 0
  runs <- rle(losses)
  if (!any(runs$values)) return(0L)
  max(runs$lengths[runs$values])
}

bankroll_analysis <- function(bets, cfg) {
  if (!nrow(bets)) return(list(summary = dplyr::tibble(), paths = dplyr::tibble()))
  start <- cfg$backtest$starting_bankroll
  flat_stake <- cfg$backtest$flat_stake_units
  pct <- cfg$backtest$proportional_stake
  win_multiplier <- if (cfg$backtest$american_odds < 0) {
    100 / abs(cfg$backtest$american_odds)
  } else {
    cfg$backtest$american_odds / 100
  }

  paths <- bets |>
    dplyr::arrange(.data$target, .data$model, .data$game_date, .data$game_id) |>
    dplyr::group_by(.data$target, .data$model) |>
    dplyr::group_modify(function(.x, .y) {
      flat_profit <- .x$profit * flat_stake
      flat_bankroll <- start + cumsum(flat_profit)
      proportional <- numeric(nrow(.x))
      proportional_stake <- numeric(nrow(.x))
      bank <- start
      for (i in seq_len(nrow(.x))) {
        stake <- bank * pct
        proportional_stake[i] <- stake
        pnl <- if (.x$bet_result[i] > 0) {
          stake * win_multiplier
        } else if (.x$bet_result[i] < 0) {
          -stake
        } else {
          0
        }
        bank <- bank + pnl
        proportional[i] <- bank
      }
      .x |>
        dplyr::mutate(
          flat_stake = flat_stake,
          flat_profit = flat_profit,
          flat_bankroll = flat_bankroll,
          flat_peak = cummax(c(start, head(flat_bankroll, -1))),
          flat_drawdown = flat_bankroll - .data$flat_peak,
          proportional_stake = proportional_stake,
          proportional_bankroll = proportional,
          proportional_peak = cummax(c(start, head(proportional, -1))),
          proportional_drawdown_pct =
            (.data$proportional_bankroll - .data$proportional_peak) /
            .data$proportional_peak
        )
    }) |>
    dplyr::ungroup()

  summary <- paths |>
    dplyr::group_by(.data$target, .data$model) |>
    dplyr::summarise(
      bets = dplyr::n(),
      wins = sum(.data$bet_result > 0),
      losses = sum(.data$bet_result < 0),
      pushes = sum(.data$bet_result == 0),
      win_rate = .data$wins / (.data$wins + .data$losses),
      profit_units = sum(.data$flat_profit),
      flat_roi = .data$profit_units / sum(.data$flat_stake),
      ending_flat_bankroll = dplyr::last(.data$flat_bankroll),
      max_flat_drawdown_units = min(.data$flat_drawdown),
      ending_proportional_bankroll = dplyr::last(.data$proportional_bankroll),
      proportional_return =
        .data$ending_proportional_bankroll / start - 1,
      max_proportional_drawdown = min(.data$proportional_drawdown_pct),
      longest_losing_streak = max_losing_streak(.data$bet_result),
      .groups = "drop"
    )
  list(summary = summary, paths = paths)
}

edge_bin_analysis <- function(bets) {
  bets |>
    dplyr::mutate(
      absolute_edge = abs(.data$edge),
      edge_bin = cut(
        .data$absolute_edge,
        breaks = c(0, 2, 3, 4, 5, 6, 8, Inf),
        right = FALSE,
        labels = c("[0,2)", "[2,3)", "[3,4)", "[4,5)", "[5,6)", "[6,8)", "8+")
      )
    ) |>
    dplyr::group_by(.data$target, .data$model, .data$edge_bin, .drop = FALSE) |>
    dplyr::summarise(
      bets = sum(!is.na(.data$bet_result)),
      wins = sum(.data$bet_result > 0, na.rm = TRUE),
      losses = sum(.data$bet_result < 0, na.rm = TRUE),
      pushes = sum(.data$bet_result == 0, na.rm = TRUE),
      win_rate = dplyr::if_else(
        .data$wins + .data$losses > 0,
        .data$wins / (.data$wins + .data$losses),
        NA_real_
      ),
      profit_units = sum(.data$profit, na.rm = TRUE),
      roi = dplyr::if_else(.data$bets > 0, .data$profit_units / .data$bets, NA_real_),
      .groups = "drop"
    )
}

betting_uncertainty <- function(
  bets,
  iterations = 5000L,
  seed = 1L,
  block_by_season = FALSE
) {
  set.seed(seed)
  bets |>
    dplyr::group_by(.data$target, .data$model) |>
    dplyr::group_modify(function(.x, .y) {
      seasons <- unique(.x$season)
      if (block_by_season) {
        blocks <- .x |>
          dplyr::group_by(.data$season) |>
          dplyr::summarise(
            bets = dplyr::n(),
            profit = sum(.data$profit),
            wins = sum(.data$bet_result > 0),
            decisions = sum(.data$bet_result != 0),
            .groups = "drop"
          )
        simulations <- replicate(iterations, {
          idx <- sample.int(nrow(blocks), nrow(blocks), replace = TRUE)
          c(
            roi = sum(blocks$profit[idx]) / sum(blocks$bets[idx]),
            win_rate = sum(blocks$wins[idx]) / sum(blocks$decisions[idx])
          )
        })
      } else {
        simulations <- replicate(iterations, {
          idx <- sample.int(nrow(.x), nrow(.x), replace = TRUE)
          c(
            roi = mean(.x$profit[idx]),
            win_rate = sum(.x$bet_result[idx] > 0) /
              sum(.x$bet_result[idx] != 0)
          )
        })
      }
      dplyr::tibble(
        method = if (block_by_season) "season_block" else "individual_bet",
        roi_low = stats::quantile(simulations["roi", ], 0.025),
        roi_high = stats::quantile(simulations["roi", ], 0.975),
        probability_roi_positive = mean(simulations["roi", ] > 0),
        win_rate_low = stats::quantile(simulations["win_rate", ], 0.025),
        win_rate_high = stats::quantile(simulations["win_rate", ], 0.975)
      )
    }) |>
    dplyr::ungroup()
}

paired_bootstrap <- function(predictions, iterations = 1000L, seed = 1L) {
  set.seed(seed)
  models <- setdiff(unique(predictions$model), "sportsbook")
  purrr::map_dfr(unique(predictions$target), function(target_name) {
    wide <- predictions |>
      dplyr::filter(.data$target == target_name) |>
      dplyr::select("game_id", "model", "truth", "prediction") |>
      tidyr::pivot_wider(names_from = .data$model, values_from = .data$prediction)
    if (!"sportsbook" %in% names(wide)) return(dplyr::tibble())

    purrr::map_dfr(models, function(model_name) {
      if (!model_name %in% names(wide)) return(dplyr::tibble())
      diffs <- replicate(iterations, {
        idx <- sample.int(nrow(wide), nrow(wide), replace = TRUE)
        mae_vec(wide$truth[idx], wide[[model_name]][idx]) -
          mae_vec(wide$truth[idx], wide$sportsbook[idx])
      })
      dplyr::tibble(
        target = target_name,
        model = model_name,
        comparison = "MAE minus sportsbook MAE",
        mean_difference = mean(diffs),
        conf_low = stats::quantile(diffs, 0.025),
        conf_high = stats::quantile(diffs, 0.975)
      )
    })
  })
}
