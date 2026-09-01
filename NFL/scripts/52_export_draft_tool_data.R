source("R/utilities.R")
source("R/touchdown_features.R")
assert_packages()
ensure_directories()
options(nflreadr.verbose = FALSE)

# Exports one JSON payload for the draft-day tool: our model's board joined to
# expert consensus, ownership and bye weeks. Everything is embedded in the
# artifact so the tool works offline at the table.

board <- readr::read_csv("outputs/draft_board_2026.csv", show_col_types = FALSE) |>
  dplyr::mutate(name_key = normalize_prop_player_name(.data$player_name))

ecr <- nflreadr::load_ff_rankings("draft") |>
  dplyr::filter(
    .data$ecr_type == "dp",
    .data$pos %in% c("QB", "RB", "WR", "TE")
  ) |>
  dplyr::mutate(name_key = normalize_prop_player_name(.data$player)) |>
  dplyr::group_by(.data$name_key) |>
  dplyr::slice_min(.data$ecr, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(
    "name_key", "ecr", "sd", "best", "worst", "bye",
    "player_owned_espn", "player_owned_avg", ecr_team = "team"
  )

# The model carries each player's 2025 team, because that is the last season it
# has data for. Showing that on a 2026 draft board is worse than useless: a
# player who changed teams has the one thing the projection cannot account for,
# and the stale label hides it. Current team comes from the 2026 roster, then
# consensus, and only falls back to 2025.
roster_2026 <- tryCatch(
  readRDS("data/raw/rosters_2026.rds") |>
    dplyr::filter(!is.na(.data$gsis_id)) |>
    dplyr::transmute(
      name_key = normalize_prop_player_name(.data$full_name),
      roster_team = normalize_team(.data$team)
    ) |>
    dplyr::distinct(.data$name_key, .keep_all = TRUE),
  error = function(e) tibble::tibble(
    name_key = character(), roster_team = character()
  )
)

# Sleeper's player feed, joined on gsis_id rather than name. It carries three
# things the model structurally cannot have: a live popularity/draft ordering,
# current depth-chart position, and injury status as of today. Sleeper exposes
# no public ADP endpoint, so search_rank is the ordering available - it is a
# consensus draft ordering, not a true average draft position, and is labelled
# that way in the payload so it is not mistaken for ADP at the table.
sleeper <- tryCatch({
  players <- if (file.exists("data/raw/sleeper_players.rds")) {
    readRDS("data/raw/sleeper_players.rds")
  } else {
    out <- httr2::request("https://api.sleeper.app/v1/players/nfl") |>
      httr2::req_timeout(90) |>
      httr2::req_user_agent("draft-tool/1.0") |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    saveRDS(out, "data/raw/sleeper_players.rds")
    out
  }

  pick <- function(p, field) {
    v <- p[[field]]
    if (is.null(v) || length(v) != 1) {
      NA
    } else {
      v
    }
  }
  purrr::map_dfr(players, function(p) {
    tibble::tibble(
      player_id = as.character(pick(p, "gsis_id")),
      sleeper_name = as.character(pick(p, "full_name")),
      sleeper_pos = as.character(pick(p, "position")),
      sleeper_rank = suppressWarnings(as.integer(pick(p, "search_rank"))),
      depth_order = suppressWarnings(as.integer(pick(p, "depth_chart_order"))),
      injury_status = as.character(pick(p, "injury_status"))
    )
  }) |>
    dplyr::filter(.data$sleeper_pos %in% c("QB", "RB", "WR", "TE")) |>
    # 9999 is Sleeper's "unranked" sentinel, not a rank.
    dplyr::mutate(
      sleeper_rank = dplyr::if_else(
        .data$sleeper_rank >= 9999, NA_integer_, .data$sleeper_rank
      ),
      player_id = dplyr::if_else(
        .data$player_id == "NA", NA_character_, .data$player_id
      ),
      name_key = normalize_prop_player_name(.data$sleeper_name)
    ) |>
    # Sleeper leaves gsis_id empty for most of the incoming rookie class, which
    # is exactly the group whose depth-chart slot matters most. Ranked players
    # sort first so the name fallback keeps the draft-relevant duplicate.
    dplyr::arrange(dplyr::coalesce(.data$sleeper_rank, 9999L))
}, error = function(e) {
  cat("Sleeper feed unavailable:", conditionMessage(e), "\n")
  tibble::tibble(player_id = character(), name_key = character(),
                 sleeper_pos = character(), sleeper_rank = integer(),
                 depth_order = integer(), injury_status = character())
})

sleeper_cols <- c("sleeper_rank", "depth_order", "injury_status")

sleeper_by_id <- sleeper |>
  dplyr::filter(!is.na(.data$player_id)) |>
  dplyr::distinct(.data$player_id, .keep_all = TRUE) |>
  dplyr::select("player_id", dplyr::all_of(sleeper_cols))

# Name plus position, not name alone: "Michael Carter" is two different players
# and the position keeps them apart.
sleeper_by_name <- sleeper |>
  dplyr::distinct(.data$name_key, .data$sleeper_pos, .keep_all = TRUE) |>
  dplyr::select("name_key", position = "sleeper_pos",
                dplyr::all_of(sleeper_cols)) |>
  dplyr::rename_with(~ paste0("nm_", .x), dplyr::all_of(sleeper_cols))

cat("Sleeper skill players:", nrow(sleeper),
    "| with gsis_id:", nrow(sleeper_by_id), "\n")

merged <- board |>
  dplyr::left_join(ecr, by = "name_key") |>
  dplyr::left_join(roster_2026, by = "name_key") |>
  dplyr::left_join(sleeper_by_id, by = "player_id") |>
  dplyr::left_join(sleeper_by_name, by = c("name_key", "position")) |>
  dplyr::mutate(
    sleeper_rank = dplyr::coalesce(.data$sleeper_rank, .data$nm_sleeper_rank),
    depth_order = dplyr::coalesce(.data$depth_order, .data$nm_depth_order),
    injury_status = dplyr::coalesce(.data$injury_status,
                                    .data$nm_injury_status)
  ) |>
  # Keep anyone any of the three sources considers draftable. The model
  # systematically under-ranks rookies, so a model-rank cutoff on its own would
  # drop a consensus top-100 rookie off the board entirely.
  dplyr::filter(
    .data$draft_rank <= 300 | !is.na(.data$ecr) |
      dplyr::coalesce(.data$sleeper_rank, 9999L) <= 300L
  ) |>
  # The board's team column already carries the 2026 roster, so comparing it
  # against itself finds almost no movers. The prior team has to come from the
  # 2025 season record.
  dplyr::left_join(
    readRDS("data/raw/season_aggregates.rds") |>
      dplyr::filter(.data$season == 2025) |>
      dplyr::group_by(.data$player_id) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup() |>
      dplyr::transmute(
        name_key = normalize_prop_player_name(.data$player_name),
        team_2025 = normalize_team(.data$team)
      ) |>
      dplyr::distinct(.data$name_key, .keep_all = TRUE),
    by = "name_key"
  ) |>
  dplyr::mutate(
    team = dplyr::coalesce(.data$roster_team, .data$ecr_team, .data$team),
    changed_team = as.integer(
      !is.na(.data$team_2025) & !is.na(.data$team) &
        .data$team != .data$team_2025
    )
  ) |>
  dplyr::arrange(.data$draft_rank)

# Consensus overall rank, densely ranked so it reads like a draft board.
merged <- merged |>
  dplyr::mutate(
    consensus_rank = dplyr::if_else(
      is.na(.data$ecr), NA_integer_, as.integer(dplyr::min_rank(.data$ecr))
    ),
    team = dplyr::coalesce(.data$team, .data$ecr_team),
    value_gap = .data$consensus_rank - .data$draft_rank
  )

payload <- merged |>
  dplyr::transmute(
    id = dplyr::row_number(),
    name = .data$player_name,
    pos = .data$position,
    team = dplyr::coalesce(.data$team, "FA"),
    bye = as.integer(dplyr::coalesce(.data$bye, NA_integer_)),
    ourRank = as.integer(.data$draft_rank),
    posRank = as.integer(.data$position_rank),
    vor = round(as.numeric(.data$vor), 1),
    projTotal = round(as.numeric(.data$projected_total), 1),
    projPpg = round(as.numeric(.data$projected_ppg), 1),
    projGames = round(as.numeric(.data$projected_games), 1),
    priorPpg = round(as.numeric(.data$prior_ppg), 1),
    ecr = round(as.numeric(.data$ecr), 1),
    ecrRank = as.integer(.data$consensus_rank),
    ecrBest = as.integer(.data$best),
    ecrWorst = as.integer(.data$worst),
    ownEspn = round(as.numeric(.data$player_owned_espn), 1),
    valueGap = as.integer(.data$value_gap),
    rookie = as.integer(.data$is_rookie),
    draftRound = as.integer(.data$draft_round),
    draftPick = as.integer(.data$draft_pick),
    # Sleeper's consensus draft ordering, NOT an average draft position.
    sleeperRank = as.integer(.data$sleeper_rank),
    depthOrder = as.integer(.data$depth_order),
    injury = .data$injury_status,
    prevTeam = .data$team_2025,
    movedTeam = as.integer(dplyr::coalesce(.data$changed_team, 0L))
  ) |>
  dplyr::slice_head(n = 400)

jsonlite::write_json(
  payload, "outputs/draft_tool_players.json",
  auto_unbox = TRUE, na = "null", digits = 4
)

cat("Players exported:", nrow(payload), "\n")
cat("With consensus rank:", sum(!is.na(payload$ecrRank)), "\n")
cat("With bye week:", sum(!is.na(payload$bye)), "\n")
print(as.data.frame(payload |> dplyr::count(.data$pos)))
cat("Changed teams since 2025:", sum(payload$movedTeam == 1), "\n")
cat("With Sleeper rank:", sum(!is.na(payload$sleeperRank)), "\n")
cat("Flagged with an injury designation:", sum(!is.na(payload$injury)), "\n")
cat("Rookies exported:", sum(payload$rookie == 1, na.rm = TRUE), "\n")

cat("\n=== rookies: model rank vs consensus ===\n")
print(as.data.frame(
  payload |>
    dplyr::filter(.data$rookie == 1) |>
    dplyr::arrange(dplyr::coalesce(.data$ecrRank, 9999L)) |>
    dplyr::select("name", "pos", "team", "draftPick", "ourRank", "ecrRank",
                  "sleeperRank", "projTotal") |>
    head(20)
))

cat("\n=== biggest disagreements with consensus (we are higher) ===\n")
print(as.data.frame(
  payload |>
    dplyr::filter(!is.na(.data$valueGap), .data$ecrRank <= 200) |>
    dplyr::slice_max(.data$valueGap, n = 12, with_ties = FALSE) |>
    dplyr::select("name", "pos", "team", "ourRank", "ecrRank", "valueGap",
                  "injury")
))

cat("\n=== biggest disagreements with consensus (we are lower) ===\n")
print(as.data.frame(
  payload |>
    dplyr::filter(!is.na(.data$valueGap), .data$ecrRank <= 200) |>
    dplyr::slice_min(.data$valueGap, n = 12, with_ties = FALSE) |>
    dplyr::select("name", "pos", "team", "ourRank", "ecrRank", "valueGap",
                  "injury")
))

cat("\nTop 10:\n")
print(as.data.frame(head(payload |> dplyr::select(
  "ourRank", "name", "pos", "team", "bye", "vor", "projTotal", "ecrRank",
  "valueGap"
), 10)))
