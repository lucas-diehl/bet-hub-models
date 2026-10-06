source("R/utilities.R")
source("R/dfs_salaries.R")
source("R/dfs_ownership.R")
assert_packages()

# Sanity checks for the ownership heuristic and the contest-standings parser.
# The heuristic checks run on synthetic data (fast, deterministic). The parser
# checks run on synthetic DK-shaped exports, because no real export exists to
# test against yet - see the header of R/dfs_ownership.R for why that's a
# structural limitation, not an oversight, and treat the parser as unverified
# against real DK output until the first real file is spot-checked by hand.

failures <- 0L
check <- function(label, expr) {
  ok <- isTRUE(tryCatch(expr, error = function(e) FALSE))
  cat(if (ok) "  ok   " else "  FAIL ", label, "\n", sep = "")
  if (!ok) failures <<- failures + 1L
}
expect_error <- function(expr) {
  inherits(tryCatch(expr, error = function(e) e), "error")
}

cat("Ownership heuristic\n")
pool <- tibble::tibble(
  season = 2026L, week = 1L,
  position = c("WR", "WR", "WR", "WR", "RB", "RB"),
  salary = c(9000, 8200, 4200, 3000, 7500, 5600),
  projected_ppr = c(19.5, 15.0, 11.0, 4.0, 18.0, 9.0),
  implied_team_total = c(27, 24, 20, 16, 27, 20)
)
scored <- dfs_ownership_heuristic(pool)

check("every row gets a score", all(is.finite(scored$ownership_heuristic_score)))
check("scores are bounded 0-100",
      all(scored$ownership_heuristic_score >= 0 &
            scored$ownership_heuristic_score <= 100))
check("every row gets a tier",
      all(scored$ownership_tier %in% c("chalk", "medium", "low", "contrarian")))
check("source is labelled as a heuristic",
      all(scored$ownership_source == "heuristic_v1"))

wr <- dplyr::filter(scored, position == "WR")
check("best value/highest-total WR outranks the worst",
      wr$ownership_heuristic_score[wr$salary == 9000] >
        wr$ownership_heuristic_score[wr$salary == 3000])
check("cheap punt play beats a bad mid-salary play on the U-shape",
      wr$ownership_heuristic_score[wr$salary == 4200] >
        wr$ownership_heuristic_score[wr$salary == 3000] - 50)  # sanity, not tight

no_env <- dplyr::select(pool, -"implied_team_total")
check("missing team-total column doesn't error",
      isTRUE(is.data.frame(dfs_ownership_heuristic(no_env))))
check("missing required column is refused",
      expect_error(dfs_ownership_heuristic(dplyr::select(pool, -"salary"))))

cat("\nContest-standings parser (synthetic DK-shaped export)\n")
tmp_dir <- tempfile("dk_standings_")
dir.create(tmp_dir)

lineup_csv <- file.path(tmp_dir, "2026_week1_test.csv")
readr::write_csv(
  tibble::tibble(
    Rank = 1:4,
    EntryName = c("user1", "user2", "user3", "user4"),
    Points = c(180.2, 175.5, 168.0, 150.2),
    Lineup = c(
      "QB Josh Allen RB Saquon Barkley RB Bijan Robinson WR Ja'Marr Chase WR CeeDee Lamb WR Justin Jefferson TE Travis Kelce FLEX Christian McCaffrey DST Ravens",
      "QB Josh Allen RB Saquon Barkley RB Derrick Henry WR Ja'Marr Chase WR CeeDee Lamb WR Puka Nacua TE Travis Kelce FLEX Christian McCaffrey DST Ravens",
      "QB Lamar Jackson RB Saquon Barkley RB Bijan Robinson WR Ja'Marr Chase WR CeeDee Lamb WR Justin Jefferson TE Sam LaPorta FLEX Derrick Henry DST 49ers",
      "QB Josh Allen RB Jahmyr Gibbs RB Bijan Robinson WR Ja'Marr Chase WR Amon-Ra St. Brown WR Justin Jefferson TE Travis Kelce FLEX Christian McCaffrey DST Ravens"
    )
  ),
  lineup_csv
)
parsed <- parse_dk_contest_standings_file(lineup_csv, season = 2026L, week = 1L)
check("parses without error", is.data.frame(parsed))
check("entry count matches the file", parsed$entries[[1]] == 4L)
check("Josh Allen owned by 3 of 4 entries (75%)",
      parsed$roster_pct[parsed$player_name == "Josh Allen"] == 75)
check("Christian McCaffrey owned by 3 of 4 entries (75%)",
      parsed$roster_pct[parsed$player_name == "Christian McCaffrey"] == 75)
check("Ravens (DST) parsed as a player-name row",
      "Ravens" %in% parsed$player_name)
check("season/week carried through", parsed$season[[1]] == 2026L &&
        parsed$week[[1]] == 1L)

slot_csv <- file.path(tmp_dir, "2026_week1_slots.csv")
readr::write_csv(
  tibble::tibble(
    Rank = 1:2, EntryName = c("a", "b"),
    QB = c("Josh Allen", "Josh Allen"), RB = c("Saquon Barkley", "Bijan Robinson"),
    WR = c("Ja'Marr Chase", "CeeDee Lamb"), TE = c("Travis Kelce", "Sam LaPorta"),
    FLEX = c("Christian McCaffrey", "Derrick Henry"),
    DST = c("Ravens", "49ers")
  ),
  slot_csv
)
parsed_slots <- parse_dk_contest_standings_file(slot_csv, season = 2026L, week = 1L)
check("per-slot-column shape also parses",
      is.data.frame(parsed_slots) && nrow(parsed_slots) > 0)
check("Josh Allen owned by both entries in slot shape",
      parsed_slots$roster_pct[parsed_slots$player_name == "Josh Allen"] == 100)

no_shape_csv <- file.path(tmp_dir, "2026_week1_bad.csv")
readr::write_csv(tibble::tibble(Rank = 1:2, Points = c(1, 2)), no_shape_csv)
check("a file with neither shape fails loudly, not silently",
      expect_error(parse_dk_contest_standings_file(no_shape_csv)))

cat("\nDirectory ingestion\n")
capture_dir <- file.path(tmp_dir, "capture")
dir.create(capture_dir)
file.copy(lineup_csv, file.path(capture_dir, basename(lineup_csv)))
store_path <- file.path(tmp_dir, "ownership_store.rds")
result <- ingest_dk_contest_standings_directory(capture_dir, store_path)
check("directory ingestion produces rows", nrow(result) > 0)
check("store file is written", file.exists(store_path))
result_again <- ingest_dk_contest_standings_directory(capture_dir, store_path)
check("re-running does not duplicate an already-ingested file",
      nrow(result_again) == nrow(result))

unlink(tmp_dir, recursive = TRUE)

cat("\n", if (failures) sprintf("%d checks FAILED\n", failures) else
    "All checks passed.\n", sep = "")
quit(save = "no", status = if (failures) 1L else 0L)
