if (dir.exists(".Rlib")) {
  .libPaths(c(normalizePath(".Rlib"), .libPaths()))
}

required_packages <- c(
  "nflfastR", "nflreadr", "dplyr", "tidyr", "purrr", "slider", "readr",
  "stringr", "lubridate", "yaml", "jsonlite", "httr2", "ranger", "xgboost", "nnet",
  "ggplot2"
)

assert_packages <- function(packages = required_packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Missing packages: ", paste(missing, collapse = ", "),
      ". Run scripts/00_install.R first.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

read_config <- function(path = "config.yml") {
  yaml::read_yaml(path)
}

ensure_directories <- function() {
  dirs <- c("data/raw", "data/processed", "outputs")
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

normalize_team <- function(x) {
  x <- toupper(trimws(x))
  aliases <- c(
    ARZ = "ARI", CRD = "ARI", SLC = "ARI",
    BLT = "BAL", CLV = "CLE", HST = "HOU",
    JAC = "JAX",
    SD = "LAC", SDG = "LAC",
    LA = "LAR", RAM = "LAR", STL = "LAR",
    OAK = "LV", LVR = "LV", LAD = "LV",
    WAS = "WAS", WSH = "WAS"
  )
  hit <- x %in% names(aliases)
  x[hit] <- unname(aliases[x[hit]])
  x
}

safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

## ---------------------------------------------------------------------------
## SPREAD SIGN CONVENTION - single source of truth (fixed 2026-11-23).
##
##   home_line    the HOME team's betting line: NEGATIVE when home is favored
##                (home -3.5 => home_line = -3.5). Matches the Odds API home
##                outcome `point` and the RotoWire `line` field.
##   team_spread  the same thing from the player's team's side: NEGATIVE =
##                favorite. home: team_spread = home_line; away: -home_line.
##   implied_team_total = total_line / 2 - team_spread / 2
##                (so home + away == total, and the favorite gets the larger).
##
## nflverse schedules use the OPPOSITE sign: spread_line is the expected home
## margin, POSITIVE when home is favored. It was being coalesced straight into
## home_line as the fallback for games with no Odds API / RotoWire line, which
## flipped the favorite on exactly those games. Every nflverse spread_line MUST
## go through nflverse_spread_to_home_line() before it meets a market line.
## ---------------------------------------------------------------------------
nflverse_spread_to_home_line <- function(spread_line) {
  -as.numeric(spread_line)
}

## Regression guard for the convention above: where a market line AND the
## (converted) nflverse line both exist for a game, they should name the same
## favorite. Small lines legitimately cross pick'em as the market moves, so
## only games where both are >= `min_abs` points are counted. A majority of
## disagreements means a whole source is on the wrong sign -> stop; any
## disagreement at all is printed so it cannot pass silently.
check_home_line_sources <- function(market_home_line, schedule_home_line,
                                    game_ids = NULL, label = "game lines",
                                    min_abs = 3) {
  m <- as.numeric(market_home_line)
  s <- as.numeric(schedule_home_line)
  both <- is.finite(m) & is.finite(s) & abs(m) >= min_abs & abs(s) >= min_abs
  n <- sum(both)
  flipped <- both & sign(m) != sign(s)
  n_flip <- sum(flipped)
  if (n_flip > 0) {
    ids <- if (is.null(game_ids)) which(flipped) else game_ids[flipped]
    msg <- sprintf(
      "%s: %d of %d games (|line| >= %g) disagree on the favorite between the market home_line and the nflverse spread (after sign conversion): %s",
      label, n_flip, n, min_abs, paste(utils::head(ids, 10), collapse = ", ")
    )
    if (n >= 4 && n_flip / n > 0.5) {
      stop(msg, ". A majority disagreeing means one source is on the wrong ",
           "sign convention - see nflverse_spread_to_home_line() in R/utilities.R.",
           call. = FALSE)
    }
    message("::warning::", msg)
  }
  invisible(list(n_compared = n, n_flipped = n_flip))
}

## Independent guard on the nflverse conversion itself (it is the fallback path,
## so on exactly the games where it is used there is no market line to compare
## against): nflverse's own moneylines name the favorite too. A converted
## home_line < 0 (home favored) should come with home_moneyline < away_moneyline.
check_nflverse_spread_sign <- function(schedule_home_line, home_moneyline,
                                       away_moneyline, game_ids = NULL,
                                       label = "nflverse spread", min_abs = 3) {
  l <- as.numeric(schedule_home_line)
  h <- suppressWarnings(as.numeric(home_moneyline))
  a <- suppressWarnings(as.numeric(away_moneyline))
  ok <- is.finite(l) & abs(l) >= min_abs & is.finite(h) & is.finite(a) & h != a
  n <- sum(ok)
  flipped <- ok & ((l < 0) != (h < a))
  n_flip <- sum(flipped)
  if (n_flip > 0) {
    ids <- if (is.null(game_ids)) which(flipped) else game_ids[flipped]
    msg <- sprintf(
      "%s: %d of %d games have a converted home_line whose favorite contradicts nflverse's own moneylines: %s",
      label, n_flip, n, paste(utils::head(ids, 10), collapse = ", ")
    )
    if (n >= 4 && n_flip / n > 0.5) {
      stop(msg, ". nflverse_spread_to_home_line() sign is wrong.", call. = FALSE)
    }
    message("::warning::", msg)
  }
  invisible(list(n_compared = n, n_flipped = n_flip))
}

## Output-side guard: on a frame carrying game_id/team/total_line/team_spread/
## implied_team_total (a player board is fine - it is reduced to one row per
## team per game), the two teams' spreads must be opposite, their implied
## totals must sum to the game total, and the favorite (team_spread < 0) must
## carry the larger implied total. Stops on any violation.
assert_team_line_convention <- function(frame, label = "board", tolerance = 1e-6) {
  need <- c("game_id", "team", "total_line", "team_spread", "implied_team_total")
  if (!nrow(frame) || !all(need %in% names(frame))) return(invisible(character()))
  teams <- unique(as.data.frame(frame)[, need])
  teams <- teams[is.finite(teams$total_line) & is.finite(teams$team_spread), ]
  bad <- character()
  for (g in unique(teams$game_id)) {
    t <- teams[teams$game_id == g, ]
    if (nrow(t) != 2) next
    sum_ok <- abs(sum(t$implied_team_total) - t$total_line[[1]]) <= tolerance
    spread_ok <- abs(t$team_spread[[1]] + t$team_spread[[2]]) <= tolerance
    fav <- which(t$team_spread < 0)
    fav_ok <- !length(fav) ||
      t$implied_team_total[[fav]] > t$implied_team_total[[3 - fav]]
    if (!(sum_ok && spread_ok && fav_ok)) bad <- c(bad, g)
  }
  if (length(bad)) {
    stop(label, ": team_spread / implied_team_total inconsistent for ",
         length(bad), " game(s): ", paste(bad, collapse = ", "),
         " (see the spread sign convention in R/utilities.R).", call. = FALSE)
  }
  invisible(bad)
}

# xgboost keeps the trained booster behind an external pointer. That pointer
# does not survive saveRDS/readRDS into a later session, and a booster written
# by xgboost 2.x unserializes under 1.7 as an empty list with its class
# attribute dropped, silently. Persist the portable raw payload next to every
# fit and rebuild the handle from it on load.
portable_booster <- function(fit) {
  attr(fit, "raw_model") <- xgboost::xgb.save.raw(fit, raw_format = "json")
  fit
}

revive_booster <- function(fit, label = "model") {
  raw_model <- attr(fit, "raw_model")
  if (!is.null(raw_model)) {
    return(xgboost::xgb.load.raw(raw_model, as_booster = TRUE))
  }
  if (inherits(fit, "xgb.Booster")) return(fit)
  stop(
    "The stored '", label, "' booster did not survive serialization and has ",
    "no portable payload. Retrain it with the installed xgboost version.",
    call. = FALSE
  )
}

# Cheap predictability probe for validation scripts: a structurally intact
# artifact whose booster cannot score is worse than a missing one, because the
# failure only surfaces at deployment time. Feature names are not carried in
# the raw payload, so the caller supplies the model's own feature vector.
booster_is_live <- function(fit, features) {
  tryCatch({
    revived <- revive_booster(fit)
    probe <- matrix(0, nrow = 1L, ncol = length(features),
                    dimnames = list(NULL, features))
    is.finite(as.numeric(stats::predict(revived, probe))[[1]])
  }, error = function(e) FALSE, warning = function(w) FALSE)
}

rmse_vec <- function(truth, estimate) {
  sqrt(mean((truth - estimate)^2, na.rm = TRUE))
}

mae_vec <- function(truth, estimate) {
  mean(abs(truth - estimate), na.rm = TRUE)
}

r_squared_vec <- function(truth, estimate) {
  ok <- stats::complete.cases(truth, estimate)
  if (sum(ok) < 3 || stats::var(truth[ok]) == 0) return(NA_real_)
  stats::cor(truth[ok], estimate[ok])^2
}

american_profit <- function(result, odds = -110, stake = 1) {
  win_profit <- if (odds < 0) 100 / abs(odds) else odds / 100
  dplyr::case_when(
    result > 0 ~ stake * win_profit,
    result < 0 ~ -stake,
    TRUE ~ 0
  )
}

signed_result <- function(x, tolerance = 1e-9) {
  ifelse(abs(x) <= tolerance, 0, sign(x))
}
