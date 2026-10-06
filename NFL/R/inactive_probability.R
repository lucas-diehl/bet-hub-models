## ---------------------------------------------------------------------------
## R/inactive_probability.R
##
## Turns the current week's injury report into a per-player p_zero - the
## probability the player does not play - for the DFS optimizer.
##
## Rates come from scripts/83_build_inactive_probability.R, measured against
## weekly roster status over 2016-2025. Out is rule-based (a player listed Out
## is ineligible); Doubtful / Questionable / no-designation are measured, and
## land close to independent DFS consensus (Questionable ~25-30%, Doubtful
## ~75%). Players absent from the injury report take a healthy base rate.
##
## Why a probability rather than just reading roster status on the day: gameday
## inactives are not declared until 90 minutes before kickoff. This pipeline
## runs on Tuesday, so the injury report is the only forward-looking signal
## that exists at projection time.
## ---------------------------------------------------------------------------

fantasy_inactive_rates <- function(path = "data/processed/inactive_rates.rds") {
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

## Attach p_zero to a projection board. `board` needs player_id and position.
## Degrades to the healthy base rate for anyone not on the report, and to a
## flat base for everyone if the rate table or the injury feed is unavailable -
## never silently drops the column, since a missing p_zero downstream is
## indistinguishable from a confident zero-risk call.
add_inactive_probability <- function(board, season, week,
                                     rates_obj = fantasy_inactive_rates()) {
  base_rate <- if (is.null(rates_obj)) 0.03 else rates_obj$no_report_rate

  if (is.null(rates_obj)) {
    message("p_zero: no rate table (run scripts/83); using flat ", base_rate, ".")
    board$p_zero <- base_rate
    board$injury_status <- NA_character_
    return(board)
  }

  injuries <- tryCatch(
    nflreadr::load_injuries(as.integer(season)),
    error = function(e) NULL
  )
  if (is.null(injuries) || !nrow(injuries)) {
    message("p_zero: no injury report for ", season, "; using flat ", base_rate, ".")
    board$p_zero <- base_rate
    board$injury_status <- NA_character_
    return(board)
  }

  wk <- injuries |>
    dplyr::filter(as.integer(.data$week) == as.integer(!!week),
                  !is.na(.data$gsis_id)) |>
    dplyr::transmute(
      player_id = .data$gsis_id,
      injury_status = dplyr::case_when(
        is.na(.data$report_status) | .data$report_status == "" ~ "NONE",
        .data$report_status %in% c("Out", "Doubtful", "Questionable") ~
          .data$report_status,
        TRUE ~ "NONE"
      )
    ) |>
    ## One row per player: keep the most severe designation if the feed carries
    ## several (a practice-day row plus a game-status row).
    dplyr::mutate(sev = match(.data$injury_status,
                              c("Out", "Doubtful", "Questionable", "NONE"))) |>
    dplyr::arrange(.data$player_id, .data$sev) |>
    dplyr::distinct(.data$player_id, .keep_all = TRUE) |>
    dplyr::select("player_id", "injury_status")

  rate_lookup <- rates_obj$rates |>
    dplyr::transmute(
      pos_group = .data$pos_group,
      injury_status = .data$status,
      p_zero_rate = .data$p_inactive
    )

  n_before <- nrow(board)
  out <- board |>
    dplyr::left_join(wk, by = "player_id") |>
    dplyr::mutate(
      pos_group = dplyr::case_when(
        .data$position %in% c("QB") ~ "QB",
        .data$position %in% c("RB", "FB") ~ "RB",
        .data$position %in% c("WR") ~ "WR",
        .data$position %in% c("TE") ~ "TE",
        TRUE ~ "WR"
      )
    ) |>
    dplyr::left_join(rate_lookup, by = c("pos_group", "injury_status")) |>
    dplyr::mutate(
      p_zero = dplyr::coalesce(.data$p_zero_rate, base_rate),
      p_zero = pmin(pmax(.data$p_zero, 0), 0.99)
    ) |>
    dplyr::select(-dplyr::any_of(c("pos_group", "p_zero_rate")))

  if (nrow(out) != n_before) {
    stop("Injury join changed row count: ", n_before, " -> ", nrow(out),
         ". Duplicate player_id in the injury feed.", call. = FALSE)
  }

  n_flagged <- sum(!is.na(out$injury_status))
  message(sprintf(
    "p_zero: %d of %d players on the week-%s injury report (%d Out, %d Doubtful, %d Questionable).",
    n_flagged, nrow(out), week,
    sum(out$injury_status %in% "Out", na.rm = TRUE),
    sum(out$injury_status %in% "Doubtful", na.rm = TRUE),
    sum(out$injury_status %in% "Questionable", na.rm = TRUE)
  ))
  out
}
