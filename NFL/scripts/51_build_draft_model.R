source("R/utilities.R")
source("R/draft_model.R")
assert_packages()
ensure_directories()
options(nflreadr.verbose = FALSE)

# Trains and validates the season-long draft model, then projects 2026.
#
# Validation answers the only question that matters for a draft: is this better
# than going in blind? "Blind" is defined generously - not random, but the two
# things a manager can do for free, namely rank by last season's points per game
# or by last season's total. Beating those is the bar.

history_path <- "data/raw/season_aggregates.rds"
if (file.exists(history_path)) {
  seasons_table <- readRDS(history_path)
} else {
  cat("Downloading player stats 2006-2025...\n")
  stats <- nflreadr::load_player_stats(2006:2025)
  seasons_table <- draft_season_aggregates(stats)
  saveRDS(seasons_table, history_path)
}

team_path <- "data/raw/team_offense.rds"
team_offense <- if (file.exists(team_path)) {
  readRDS(team_path)
} else {
  draft_team_offense(nflreadr::load_player_stats(2006:2025))
}

# Both tables may have been cached before team-code normalization existed, so
# normalize on load as well as at build time. normalize_team() is idempotent,
# so this is a no-op on a freshly built cache. Without it the Rams are "LA" in
# the stats and "LAR" on the roster, the team join misses, and every Rams
# player draws median-imputed team features and a spurious changed_team = 1.
seasons_table <- dplyr::mutate(seasons_table, team = normalize_team(.data$team))
team_offense <- team_offense |>
  dplyr::mutate(team = normalize_team(.data$team)) |>
  # Collapsing after the rename guards the team join against a fan-out: two
  # rows for one (season, team) would duplicate every player on that team.
  dplyr::group_by(.data$season, .data$team) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE)),
                   .groups = "drop")

draft_picks <- nflreadr::load_draft_picks()

# The 2026 draft class carries placeholder identifiers upstream - its gsis_id
# reads "MEN516487" rather than "00-0040785" - so it shares no key at all with
# the roster file and the draft-capital join silently misses every rookie.
# Every incoming rookie then falls through to the undrafted default of round 8 /
# pick 300, which collapses the whole class onto one projection.
#
# The current roster carries the real pick number against the real gsis_id, so
# capital is rebuilt from there and appended as ordinary pick rows. Rookies with
# no draft_number are genuinely undrafted and keep the default.
rookie_capital <- tryCatch({
  rounds <- draft_picks |>
    dplyr::filter(.data$season == 2026L) |>
    dplyr::distinct(pick = .data$pick, round = .data$round)

  readRDS("data/raw/rosters_2026.rds") |>
    dplyr::filter(!is.na(.data$gsis_id), !is.na(.data$draft_number),
                  .data$years_exp == 0) |>
    dplyr::distinct(.data$gsis_id, .keep_all = TRUE) |>
    dplyr::transmute(
      gsis_id = .data$gsis_id,
      season = 2026L,
      pick = as.integer(.data$draft_number)
    ) |>
    dplyr::left_join(rounds, by = "pick") |>
    # A pick number missing from the round map still implies a round: 32 picks
    # to a round is right to within the compensatory picks.
    dplyr::mutate(round = dplyr::coalesce(
      as.integer(.data$round), pmin(7L, as.integer(ceiling(.data$pick / 32)))
    ))
}, error = function(e) tibble::tibble(
  gsis_id = character(), season = integer(), pick = integer(),
  round = integer()
))

cat("2026 rookies with draft capital recovered:", nrow(rookie_capital), "\n")

# Prepended so distinct() in the capital join keeps these over the placeholder
# rows they replace.
draft_picks <- dplyr::bind_rows(rookie_capital, draft_picks)

features_table <- draft_build_features(seasons_table, draft_picks, team_offense)
model_features <- c(draft_feature_names(), draft_team_feature_names())

cat("Player-seasons:", nrow(features_table), "\n")
cat("Seasons:", paste(range(features_table$season), collapse = "-"), "\n")

# Only players with a real shot at being drafted are graded. Including every
# practice-squad body inflates the correlations by making the easy calls
# (nobodies score nothing) dominate the sample.
gradeable <- features_table |>
  dplyr::filter(
    .data$games >= 1,
    dplyr::coalesce(.data$career_games, 0) >= 1 | .data$is_rookie == 1
  )

test_seasons <- 2016:2025
predictions <- walk_forward_draft_model(gradeable, test_seasons, model_features)
saveRDS(predictions, "data/processed/draft_model_predictions.rds")
readr::write_csv(predictions, "outputs/draft_model_predictions.csv")

cat("Graded player-seasons:", nrow(predictions), "\n\n")

# --------------------------------------------------------------------------
# Validation
# --------------------------------------------------------------------------

scored <- predictions |>
  dplyr::filter(!is.na(.data$ppr_total)) |>
  dplyr::mutate(
    baseline_ppg = dplyr::coalesce(.data$ppg_1, 0),
    baseline_total = dplyr::coalesce(.data$total_1, 0)
  )

compare <- function(d, label) {
  if (nrow(d) < 30) return(tibble::tibble())
  tibble::tibble(
    cut = label,
    players = nrow(d),
    model_r = stats::cor(d$projected_total, d$ppr_total),
    prior_ppg_r = stats::cor(d$baseline_ppg, d$ppr_total),
    prior_total_r = stats::cor(d$baseline_total, d$ppr_total),
    model_rho = stats::cor(d$projected_total, d$ppr_total, method = "spearman"),
    prior_ppg_rho = stats::cor(d$baseline_ppg, d$ppr_total, method = "spearman"),
    prior_total_rho = stats::cor(d$baseline_total, d$ppr_total,
                                 method = "spearman"),
    model_mae = mean(abs(d$projected_total - d$ppr_total))
  )
}

cat("=== Season-total prediction: model vs free baselines ===\n")
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
  }) |>
    dplyr::select("cut", "players", "model_rho", "prior_ppg_rho",
                  "prior_total_rho")
), digits = 4)

# The availability half of the model, tested on its own.
cat("\n=== Games-played projection ===\n")
cat("correlation with actual games:",
    round(stats::cor(scored$projected_games, scored$games), 4), "\n")
cat("prior-season games correlation:",
    round(stats::cor(dplyr::coalesce(scored$games_1, 0), scored$games), 4), "\n")
cat("mean absolute error, games:",
    round(mean(abs(scored$projected_games - scored$games)), 3), "\n")

# --------------------------------------------------------------------------
# The draft-shaped test
# --------------------------------------------------------------------------

hit_rates <- function(d, n) {
  d <- dplyr::filter(d, !is.na(.data$ppr_total))
  if (nrow(d) < n) return(NULL)
  top <- function(column) {
    d |> dplyr::slice_max(.data[[column]], n = n, with_ties = FALSE) |>
      dplyr::pull(.data$player_id)
  }
  actual <- top("ppr_total")
  c(
    model = length(intersect(top("projected_total"), actual)) / n,
    prior_ppg = length(intersect(top("baseline_ppg"), actual)) / n,
    prior_total = length(intersect(top("baseline_total"), actual)) / n
  )
}

cat("\n=== Top-N hit rate against actual season finish ===\n")
grid <- tidyr::expand_grid(
  season = sort(unique(scored$season)),
  position = c("QB", "RB", "WR", "TE"),
  n = c(12L, 24L)
)
hits <- purrr::pmap_dfr(grid, function(season, position, n) {
  h <- hit_rates(
    dplyr::filter(scored, .data$season == !!season,
                  .data$position == !!position), n
  )
  if (is.null(h)) return(tibble::tibble())
  tibble::tibble(season = season, position = position, top_n = n,
                 model = h[["model"]], prior_ppg = h[["prior_ppg"]],
                 prior_total = h[["prior_total"]])
})
print(as.data.frame(
  hits |> dplyr::group_by(.data$position, .data$top_n) |>
    dplyr::summarise(
      seasons = dplyr::n(),
      model = mean(.data$model),
      prior_ppg = mean(.data$prior_ppg),
      prior_total = mean(.data$prior_total),
      .groups = "drop"
    )
), digits = 3)
readr::write_csv(hits, "outputs/draft_model_hit_rates.csv")

# Paired bootstrap on the per-player absolute error against the better baseline.
set.seed(20260814L)
cat("\n=== Paired bootstrap: model vs prior-season total (MAE) ===\n")
delta <- abs(scored$projected_total - scored$ppr_total) -
  abs(scored$baseline_total - scored$ppr_total)
draws <- replicate(2000, mean(sample(delta, length(delta), replace = TRUE)))
cat(sprintf(
  "mean MAE difference %.3f (negative favours model), 95%% CI %.3f to %.3f, P(model better) %.3f\n",
  mean(delta), stats::quantile(draws, 0.025), stats::quantile(draws, 0.975),
  mean(draws < 0)
))

# --------------------------------------------------------------------------
# 2026 projections
# --------------------------------------------------------------------------

# The 2026 placeholder rows must carry each player's 2026 team, not the team he
# finished 2025 on. Otherwise the team features describe the offence he left and
# the move is invisible - which was the whole point of adding them.
roster_2026 <- tryCatch(
  readRDS("data/raw/rosters_2026.rds") |>
    dplyr::filter(!is.na(.data$gsis_id)) |>
    dplyr::transmute(
      player_id = .data$gsis_id, team_2026 = normalize_team(.data$team)
    ) |>
    dplyr::distinct(.data$player_id, .keep_all = TRUE),
  error = function(e) tibble::tibble(
    player_id = character(), team_2026 = character()
  )
)

placeholder <- seasons_table |>
  dplyr::filter(.data$season == 2025) |>
  dplyr::left_join(roster_2026, by = "player_id") |>
  dplyr::mutate(
    season = 2026L, games = NA_integer_,
    ppr_total = NA_real_, ppr_ppg = NA_real_,
    team = dplyr::coalesce(.data$team_2026, .data$team)
  ) |>
  dplyr::select(-"team_2026")

# Incoming rookies have no 2025 stat line, so building the 2026 slate from the
# 2025 season alone left the entire draft class off the board - not ranked low,
# absent. They are added from the current roster instead: everything the model
# can know about a rookie is draft capital, position and the offence he landed
# in, and all three are on the roster file or the pick list.
#
# Their lagged production columns stay NA and are median-imputed by
# draft_matrix_pair, which is exactly how rookies appear in the training years,
# so train and predict see the same representation.
rookies_2026 <- tryCatch(
  readRDS("data/raw/rosters_2026.rds") |>
    dplyr::filter(
      !is.na(.data$gsis_id), .data$years_exp == 0,
      .data$position %in% c("QB", "RB", "WR", "TE")
    ) |>
    dplyr::transmute(
      season = 2026L,
      player_id = .data$gsis_id,
      player_name = .data$full_name,
      .data$position,
      team = normalize_team(.data$team),
      games = NA_integer_, ppr_total = NA_real_, ppr_ppg = NA_real_,
      targets = NA_integer_, carries = NA_integer_, attempts = NA_integer_
    ) |>
    dplyr::distinct(.data$player_id, .keep_all = TRUE),
  error = function(e) placeholder[0, ]
)

# A player who debuted in 2025 is on both lists. The 2025 record wins, because
# it carries real production the roster file does not have.
rookies_2026 <- dplyr::anti_join(rookies_2026, placeholder, by = "player_id")
placeholder <- dplyr::bind_rows(placeholder, rookies_2026)

cat("2026 players with a current-roster team:",
    sum(!is.na(placeholder$team)), "of", nrow(placeholder), "\n")
cat("Incoming rookies added:", nrow(rookies_2026), "\n")

future <- draft_build_features(
  dplyr::bind_rows(seasons_table, placeholder), draft_picks, team_offense
) |>
  dplyr::filter(.data$season == 2026)

cat("2026 players flagged as having changed teams:",
    sum(future$changed_team == 1, na.rm = TRUE), "\n")

train_all <- dplyr::filter(gradeable, .data$season <= 2025)
ppg_model <- fit_draft_target(train_all, future, "ppr_ppg", model_features)
games_model <- fit_draft_target(train_all, future, "games", model_features,
                                seed = 991L)

ppg_hat_2026 <- ppg_model$prediction
games_hat_2026 <- pmin(17, games_model$prediction)

board_2026 <- future |>
  dplyr::mutate(
    projected_ppg = ppg_hat_2026,
    projected_games = games_hat_2026,
    projected_total = .data$projected_ppg * .data$projected_games
  ) |>
  dplyr::arrange(dplyr::desc(.data$projected_total)) |>
  dplyr::group_by(.data$position) |>
  dplyr::mutate(position_rank = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::ungroup()

# Raw projected points is the wrong sort order for a draft. A quarterback
# outscores a running back in absolute PPR terms, but in a one-quarterback
# league only the points above what is freely available at the position are
# worth a pick. Replacement level is the last starter-ish player at each spot
# in a 12-team league.
#
# The four numbers are derived from the league settings rather than written
# down directly, because a hardcoded WR37 quietly encoded a three-receiver
# league. This one starts two, so six fewer receivers are startable, the
# replacement receiver is a better player, and every WR's value over him is
# smaller than the old constant implied.
league_teams <- 12L
league_starters <- c(QB = 1, RB = 2, WR = 2, TE = 1)
league_flex_spots <- 1
# How a PPR flex is actually filled league-wide. Receivers take it slightly
# more often than backs in full PPR; tight ends almost never.
league_flex_share <- c(QB = 0, RB = 0.45, WR = 0.50, TE = 0.05)

replacement_rank <- as.integer(floor(
  league_teams * league_starters +
    league_teams * league_flex_spots *
      league_flex_share[names(league_starters)]
)) + 1L
names(replacement_rank) <- names(league_starters)

cat("Replacement level from league settings (", league_teams, " teams): ",
    paste(names(replacement_rank), replacement_rank, sep = "", collapse = " / "),
    "\n", sep = "")

replacement_level <- board_2026 |>
  dplyr::group_by(.data$position) |>
  dplyr::summarise(
    replacement = {
      k <- replacement_rank[[dplyr::first(.data$position)]]
      values <- sort(.data$projected_total, decreasing = TRUE)
      if (length(values) >= k) values[[k]] else min(values)
    },
    .groups = "drop"
  )

board_2026 <- board_2026 |>
  dplyr::left_join(replacement_level, by = "position") |>
  dplyr::mutate(vor = .data$projected_total - .data$replacement) |>
  dplyr::arrange(dplyr::desc(.data$vor)) |>
  dplyr::mutate(draft_rank = dplyr::row_number()) |>
  dplyr::select(
    "draft_rank", "position_rank", "player_id", "player_name", "position",
    "team", "vor", "projected_total", "projected_ppg", "projected_games",
    prior_ppg = "ppg_1", prior_games = "games_1", "is_rookie",
    "draft_round", "draft_pick"
  )

readr::write_csv(board_2026, "outputs/draft_board_2026.csv")
cat("\n=== 2026 draft board, top 25 by value over replacement ===\n")
print(as.data.frame(head(board_2026, 25)), digits = 4)

cat("\n=== Positional breakdown of the top 60 ===\n")
print(as.data.frame(
  board_2026 |> head(60) |> dplyr::count(.data$position)
))

cat("\n=== Rookies on the board ===\n")
cat("total:", sum(board_2026$is_rookie == 1, na.rm = TRUE), "\n")
print(as.data.frame(
  board_2026 |>
    dplyr::filter(.data$is_rookie == 1) |>
    dplyr::select("draft_rank", "player_name", "position", "team", "vor",
                  "projected_total") |>
    head(20)
), digits = 4)
