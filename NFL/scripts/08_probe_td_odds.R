source("R/utilities.R")
source("R/odds_api.R")
assert_packages()
ensure_directories()

snapshot <- "2025-09-04T23:20:00Z"
events_result <- odds_api_historical_events(snapshot)
events <- events_result$data$data

target <- events |>
  dplyr::filter(
    .data$home_team == "Philadelphia Eagles",
    .data$away_team == "Dallas Cowboys"
  )
if (nrow(target) != 1L) {
  stop("Could not resolve the DAL at PHI historical event uniquely.", call. = FALSE)
}

props_result <- odds_api_historical_event_props(
  target$id[[1]],
  snapshot,
  market = "player_anytime_td",
  region = "us"
)
props <- props_result$data$data

destination <- "data/raw/odds_api_probe_2025_DAL_PHI.json"
jsonlite::write_json(
  props_result$data,
  destination,
  auto_unbox = TRUE,
  pretty = TRUE,
  na = "null"
)

bookmakers <- props$bookmakers
bookmaker_count <- if (is.null(bookmakers)) 0L else nrow(bookmakers)
outcome_count <- 0L
if (bookmaker_count) {
  outcome_count <- sum(vapply(
    bookmakers$markets,
    function(markets) {
      if (is.null(markets) || !nrow(markets)) return(0L)
      sum(vapply(markets$outcomes, nrow, integer(1)))
    },
    integer(1)
  ))
}

cat("Probe saved:", destination, "\n")
cat("Bookmakers returned:", bookmaker_count, "\n")
cat("Player outcomes returned:", outcome_count, "\n")
cat("Probe request cost:", props_result$quota$last, "\n")
cat("Quota remaining:", props_result$quota$remaining, "\n")

