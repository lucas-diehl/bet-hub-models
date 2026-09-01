source("R/utilities.R")
source("R/odds.R")
source("R/features.R")
source("R/models.R")
source("R/backtest.R")
assert_packages()
cfg <- read_config()

seasons <- 2022:2024
pbp <- nflfastR::load_pbp(seasons)
schedules <- nflfastR::load_schedules(seasons)

odds_path <- tempfile(fileext = ".json")
download_rotowire(cfg$data$rotowire_url, odds_path, refresh = TRUE)
odds <- read_rotowire(odds_path)

games <- schedule_games(schedules, regular_season_only = TRUE)
team_games <- summarize_team_games(pbp, regular_season_only = TRUE) |>
  add_team_context(games)
rolled <- add_rolling_features(team_games, unlist(cfg$features$rolling_windows))
features <- build_game_features(
  games, rolled, odds, cfg$features$minimum_prior_games
)

stopifnot(nrow(features) > 500)
stopifnot(mean(!is.na(features$home_line)) == 1)
stopifnot(mean(!is.na(features$total_line)) == 1)
stopifnot(all(features$home_prior_games >= cfg$features$minimum_prior_games))
stopifnot(all(features$away_prior_games >= cfg$features$minimum_prior_games))

train <- dplyr::filter(features, .data$season < 2024)
test <- dplyr::filter(features, .data$season == 2024)
pred <- fit_predict_model("linear", train, test, "home_margin", cfg)
stopifnot(length(pred) == nrow(test), all(is.finite(pred)))

smoke_predictions <- test |>
  dplyr::transmute(
    .data$game_id, .data$season, .data$week, .data$game_date,
    .data$home_team, .data$away_team,
    target = "home_margin",
    model = "linear",
    truth = .data$home_margin,
    prediction = pred,
    .data$home_line,
    .data$total_line
  )
graded <- grade_edges(smoke_predictions, threshold = 2.5)
stopifnot(nrow(graded) == nrow(test))

message(
  "Smoke test passed: ", nrow(features), " feature rows; ",
  nrow(test), " held-out games; margin MAE ",
  round(mae_vec(test$home_margin, pred), 3), "."
)
