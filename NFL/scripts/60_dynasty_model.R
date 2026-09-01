source("R/utilities.R")
source("R/draft_model.R")
source("R/touchdown_features.R")
assert_packages()
ensure_directories()
options(nflreadr.verbose = FALSE)

# A model fitted to dynasty, not a redraft model with an aging adjustment
# bolted on.
#
# The target is what a startup pick actually buys: total PPR produced over the
# next four seasons, counting zero for the years a player is out of the league.
# That folds longevity, availability and decline into the thing being predicted
# instead of correcting for them afterwards, and it is directly validatable -
# every window ending in 2025 or earlier has a known answer.
#
# Age is a feature here rather than a post-hoc multiplier, so the model learns
# the position-specific shape itself.

horizon <- 4L
seasons_table <- readRDS("data/raw/season_aggregates.rds")
team_offense <- readRDS("data/raw/team_offense.rds")
draft_picks <- nflreadr::load_draft_picks()

players <- nflreadr::load_players() |>
  dplyr::filter(!is.na(.data$gsis_id), !is.na(.data$birth_date)) |>
  dplyr::transmute(
    player_id = .data$gsis_id, birth_date = as.Date(.data$birth_date)
  ) |>
  dplyr::distinct(.data$player_id, .keep_all = TRUE)

age_at <- function(birth, season) {
  as.numeric(
    difftime(as.Date(paste0(season, "-09-01")), birth, units = "days")
  ) / 365.25
}

# --------------------------------------------------------------------------
# Forward four-year target
# --------------------------------------------------------------------------

max_season <- max(seasons_table$season)
totals <- seasons_table |> dplyr::select("player_id", "season", "ppr_total")

forward <- purrr::map_dfr(0:(horizon - 1L), function(k) {
  totals |>
    dplyr::transmute(
      .data$player_id, season = .data$season - k, contrib = .data$ppr_total
    )
}) |>
  dplyr::group_by(.data$player_id, .data$season) |>
  dplyr::summarise(fwd_total = sum(.data$contrib), .groups = "drop")

# 2026 placeholder rows carry each player's current team forward so the lag
# structure and team features resolve for the upcoming season.
roster_2026 <- tryCatch(
  readRDS("data/raw/rosters_2026.rds") |>
    dplyr::filter(!is.na(.data$gsis_id)) |>
    dplyr::transmute(player_id = .data$gsis_id,
                     team_2026 = normalize_team(.data$team)) |>
    dplyr::distinct(.data$player_id, .keep_all = TRUE),
  error = function(e) tibble::tibble(player_id = character(),
                                     team_2026 = character())
)
placeholder <- seasons_table |>
  dplyr::filter(.data$season == max_season) |>
  dplyr::left_join(roster_2026, by = "player_id") |>
  dplyr::mutate(
    season = 2026L, games = NA_integer_, ppr_total = NA_real_,
    ppr_ppg = NA_real_, team = dplyr::coalesce(.data$team_2026, .data$team)
  ) |>
  dplyr::select(-"team_2026")

features <- draft_build_features(
  dplyr::bind_rows(seasons_table, placeholder), draft_picks, team_offense
) |>
  dplyr::left_join(players, by = "player_id") |>
  dplyr::mutate(age = age_at(.data$birth_date, .data$season)) |>
  dplyr::left_join(forward, by = c("player_id", "season")) |>
  # A player with no rows in the following seasons produced nothing, which is
  # exactly the downside a dynasty pick carries.
  dplyr::mutate(fwd_total = dplyr::coalesce(.data$fwd_total, 0)) |>
  dplyr::filter(
    # games is the upcoming season's outcome, so it is NA for 2026 by
    # definition. Applying the played-a-game filter to future rows would drop
    # the entire board.
    .data$season > max_season | .data$games >= 1,
    dplyr::coalesce(.data$career_games, 0) >= 1 | .data$is_rookie == 1,
    !is.na(.data$age)
  )

model_features <- c(draft_feature_names(), draft_team_feature_names(), "age")

# Only windows that have fully elapsed can be trained or scored on.
complete <- features |> dplyr::filter(.data$season <= max_season - horizon + 1L)
cat("Player-seasons with a complete", horizon, "year window:", nrow(complete), "\n")
cat("Seasons:", paste(range(complete$season), collapse = "-"), "\n\n")

# --------------------------------------------------------------------------
# Walk-forward validation
# --------------------------------------------------------------------------

test_seasons <- 2014:max(complete$season)
predictions <- purrr::map_dfr(test_seasons, function(s) {
  train <- dplyr::filter(complete, .data$season < s)
  test <- dplyr::filter(complete, .data$season == s)
  if (nrow(train) < 500 || !nrow(test)) return(tibble::tibble())
  fit <- fit_draft_target(train, test, "fwd_total", model_features)
  test |>
    dplyr::select("season", "player_id", "player_name", "position", "age",
                  "fwd_total", "ppr_total", "ppg_1", "total_1") |>
    dplyr::mutate(dynasty_pred = fit$prediction)
})

scored <- predictions |>
  dplyr::mutate(
    prior_total = dplyr::coalesce(.data$total_1, 0),
    this_year = dplyr::coalesce(.data$ppr_total, 0)
  )

# Only baselines a drafter could actually use. Season S's own production is
# excluded deliberately: it is one of the four years being predicted, so it
# correlates with the target by construction, and nobody knows it on draft day.
# Comparing against it made the model look far worse than it is.
cat("=== Predicting four-year output, preseason information only ===\n")
compare <- function(d, label) {
  if (nrow(d) < 40) return(tibble::tibble())
  tibble::tibble(
    cut = label, players = nrow(d),
    dynasty_rho = stats::cor(d$dynasty_pred, d$fwd_total, method = "spearman"),
    prior_total_rho = stats::cor(d$prior_total, d$fwd_total, method = "spearman"),
    prior_ppg_rho = stats::cor(dplyr::coalesce(d$ppg_1, 0), d$fwd_total,
                               method = "spearman"),
    dynasty_mae = mean(abs(d$dynasty_pred - d$fwd_total)),
    prior_mae = mean(abs(d$prior_total * horizon - d$fwd_total))
  )
}
print(as.data.frame(dplyr::bind_rows(
  compare(scored, "all"),
  purrr::map_dfr(c("QB", "RB", "WR", "TE"), function(p) {
    compare(dplyr::filter(scored, .data$position == p), p)
  })
)), digits = 4)

cat("\n=== By season ===\n")
print(as.data.frame(
  purrr::map_dfr(sort(unique(scored$season)), function(s) {
    compare(dplyr::filter(scored, .data$season == s), as.character(s))
  }) |> dplyr::select("cut", "players", "dynasty_rho", "prior_total_rho")
), digits = 4)

set.seed(20260819L)
delta <- abs(scored$dynasty_pred - scored$fwd_total) -
  abs(scored$prior_total * horizon - scored$fwd_total)
draws <- replicate(2000, mean(sample(delta, length(delta), replace = TRUE)))
cat(sprintf(
  "\nPaired bootstrap vs prior season x %d: mean %+.2f (negative favours model), 95%% CI %+.2f to %+.2f, P(model better) %.3f\n",
  horizon, mean(delta), stats::quantile(draws, 0.025),
  stats::quantile(draws, 0.975), mean(draws < 0)
))

# Does it know young from old? The whole point of a dynasty fit.
cat("\n=== Mean four-year output by age, actual vs predicted ===\n")
print(as.data.frame(
  scored |>
    dplyr::mutate(age_bin = cut(.data$age, c(0, 23, 25, 27, 29, 31, 99),
                                labels = c("<=23", "24-25", "26-27", "28-29",
                                           "30-31", "32+"))) |>
    dplyr::group_by(.data$age_bin) |>
    dplyr::summarise(
      n = dplyr::n(), actual = mean(.data$fwd_total),
      predicted = mean(.data$dynasty_pred), .groups = "drop"
    )
), digits = 4)

# --------------------------------------------------------------------------
# 2026 board
# --------------------------------------------------------------------------

future <- features |> dplyr::filter(.data$season == 2026)
if (!nrow(future)) {
  cat("\nNo 2026 rows; run scripts/51 first to build the 2026 feature table.\n")
  quit(save = "no", status = 0)
}
fit_all <- fit_draft_target(complete, future, "fwd_total", model_features)

teams <- 12L
replacement_rank <- c(QB = teams * 2L + 1L, RB = round(teams * 2.5),
                      WR = round(teams * 3), TE = teams + 1L)

board <- future |>
  dplyr::mutate(dynasty_pred = fit_all$prediction) |>
  dplyr::group_by(.data$position) |>
  dplyr::mutate(
    replacement = {
      k <- replacement_rank[[dplyr::first(.data$position)]]
      v <- sort(.data$dynasty_pred, decreasing = TRUE)
      if (length(v) >= k) v[[k]] else min(v)
    }
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(dynasty_vor = .data$dynasty_pred - .data$replacement) |>
  dplyr::arrange(dplyr::desc(.data$dynasty_vor)) |>
  dplyr::mutate(rank = dplyr::row_number()) |>
  dplyr::group_by(.data$position) |>
  dplyr::mutate(pos_rank = dplyr::row_number()) |>
  dplyr::ungroup()

# Sleeper's ordering, joined for cross-reference. It is a general search
# ranking, not a dynasty superflex ADP - Sleeper exposes no public ADP endpoint
# - so it is a sanity check on obscurity, not a market price.
sleeper <- tryCatch({
  raw <- readRDS("data/raw/sleeper_players.rds")
  pick <- function(p, f) { v <- p[[f]]; if (is.null(v) || length(v) != 1) NA else v }
  purrr::map_dfr(raw, function(p) tibble::tibble(
    name_key = normalize_prop_player_name(as.character(pick(p, "full_name"))),
    pos = as.character(pick(p, "position")),
    sleeper_rank = suppressWarnings(as.integer(pick(p, "search_rank"))),
    injury_status = as.character(pick(p, "injury_status"))
  )) |>
    dplyr::filter(.data$pos %in% c("QB", "RB", "WR", "TE")) |>
    dplyr::mutate(sleeper_rank = dplyr::if_else(
      .data$sleeper_rank >= 9999, NA_integer_, .data$sleeper_rank)) |>
    dplyr::arrange(dplyr::coalesce(.data$sleeper_rank, 9999L)) |>
    dplyr::distinct(.data$name_key, .keep_all = TRUE) |>
    dplyr::select("name_key", "sleeper_rank", "injury_status")
}, error = function(e) tibble::tibble(
  name_key = character(), sleeper_rank = integer(), injury_status = character()
))

final <- board |>
  dplyr::mutate(name_key = normalize_prop_player_name(.data$player_name)) |>
  dplyr::left_join(sleeper, by = "name_key") |>
  dplyr::transmute(
    .data$rank, .data$pos_rank, player = .data$player_name, .data$position,
    .data$team, age = round(.data$age, 1),
    dynasty_vor = round(.data$dynasty_vor, 1),
    four_year_pts = round(.data$dynasty_pred, 1),
    .data$sleeper_rank, .data$injury_status, rookie = .data$is_rookie
  )

readr::write_csv(final, "outputs/dynasty_model_board_2026.csv")
cat("\n=== Top 36, dynasty-fitted, 12-team superflex ===\n")
print(as.data.frame(head(final, 36)), digits = 4)
