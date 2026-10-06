source("R/utilities.R")
assert_packages()
ensure_directories()

# JSON payload for the dynasty draft tool, embedded in the artifact so it works
# offline at the table.

sheet <- readr::read_csv(
  "outputs/dynasty_draft_sheet_2026.csv", show_col_types = FALSE
)

payload <- sheet |>
  dplyr::mutate(id = dplyr::row_number()) |>
  dplyr::transmute(
    .data$id,
    rank = as.integer(.data$my_rank),
    tier = as.integer(.data$tier),
    posRank = as.integer(.data$position_rank),
    name = .data$player,
    pos = dplyr::coalesce(.data$position, "?"),
    team = dplyr::coalesce(.data$team, "FA"),
    age = round(as.numeric(.data$age), 1),
    rookie = as.integer(dplyr::coalesce(.data$rookie, 0)),
    sleeper = as.integer(.data$sleeper_rank),
    gap = as.integer(.data$value_vs_market),
    pts = as.integer(round(.data$four_year_pts)),
    capital = dplyr::coalesce(.data$draft_capital, ""),
    injury = dplyr::coalesce(.data$injury, "")
  ) |>
  dplyr::slice_head(n = 400)

jsonlite::write_json(
  payload, "outputs/dynasty_tool_players.json",
  auto_unbox = TRUE, na = "null", digits = 4
)
cat("Players exported:", nrow(payload), "\n")
print(as.data.frame(payload |> dplyr::count(.data$pos)))
cat("Rookies:", sum(payload$rookie == 1), "\n")
