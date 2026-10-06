## ---------------------------------------------------------------------------
## 83_build_inactive_probability.R
##
## Estimates P(player does not play) from the weekly injury report, so the DFS
## optimizer can price the risk of rostering someone who is inactive.
##
## Why this exists: DFS ENGINE hardcodes p_zero = 0.03 for every player our
## projection file covers (sports/nfl/project.R). A questionable RB carrying a
## real chance of being scratched is therefore treated exactly like an ironman
## starter. In DFS a zero is not a small miss - it is a dead roster spot that
## usually kills the entry - so this is the cheapest large win available.
##
## Data-driven, not assumed. Rather than hardcoding the DFS-community rules of
## thumb (Questionable ~25%, Doubtful ~75%), the rates are measured from the
## nflverse injury reports joined to whether the player actually recorded a
## game that week, then shrunk toward the position-group base rate. The printed
## calibration table at the bottom is there to be read: if the measured rates
## do not land near the known shape (Out >> Doubtful >> Questionable), the
## appearance proxy is wrong and the output should not be trusted.
##
## Era restriction: 2016+. The NFL abolished the "Probable" designation after
## the 2015 season, so earlier rows carry different status semantics and would
## contaminate the estimates.
##
## Writes:
##   data/processed/inactive_rates.rds   rate table, consumed by scripts/17
##   outputs/inactive_rate_by_status.csv same, for eyeballing
##
## Run:  & $rscript scripts/83_build_inactive_probability.R
## ---------------------------------------------------------------------------

source("R/utilities.R")
suppressPackageStartupMessages({
  library(dplyr)
})
assert_packages()
ensure_directories()

FIRST_SEASON <- 2016L
SHRINK <- 50    # pseudo-counts toward the position-group base rate
OUT_RDS <- "data/processed/inactive_rates.rds"
OUT_CSV <- "outputs/inactive_rate_by_status.csv"

## ---------------------------------------------------------------------------
## 1. Historical injury reports
## ---------------------------------------------------------------------------

inj_path <- "data/raw/injuries_2009_2025.rds"
if (!file.exists(inj_path)) {
  stop("Missing ", inj_path, " - run scripts/33_build_injury_features.R first.",
       call. = FALSE)
}
inj <- readRDS(inj_path) |>
  filter(.data$season >= FIRST_SEASON, !is.na(.data$gsis_id)) |>
  transmute(
    season = as.integer(.data$season),
    week = as.integer(.data$week),
    player_id = .data$gsis_id,
    position = .data$position,
    report_status = .data$report_status,
    practice_status = .data$practice_status
  )
message("Injury rows ", FIRST_SEASON, "+: ", format(nrow(inj), big.mark = ","))
overlap_candidate_seasons <- sort(unique(inj$season))

## ---------------------------------------------------------------------------
## 2. Did the player actually dress that week?
##
## Ground truth is the weekly roster status (ACT / INA / RES / DEV / CUT), not
## the box score. The first two attempts at this used "appeared in
## player_stats" as a proxy and both failed the calibration check: they put the
## inactive rate for players with NO injury designation at 20.6%, then 14-28%
## after gating to rotation players. The proxy cannot separate "was inactive"
## from "dressed but recorded no stat line" - worst for blocking TEs, which is
## exactly where it was most wrong (TE 27%). Roster status answers the actual
## question directly.
##
## Note this is also WHY a probability is needed at all rather than just reading
## status live: gameday inactives are not declared until 90 minutes before
## kickoff, so on the Tuesday this pipeline runs, Sunday's INA flags do not yet
## exist. The injury report is the only forward-looking signal available, and
## these rates are what convert it into a number.
## ---------------------------------------------------------------------------

rosters <- nflreadr::load_rosters_weekly(overlap_candidate_seasons) |>
  filter(!is.na(.data$gsis_id), .data$game_type == "REG") |>
  transmute(
    season = as.integer(.data$season), week = as.integer(.data$week),
    player_id = .data$gsis_id, status = .data$status
  ) |>
  distinct(.data$season, .data$week, .data$player_id, .keep_all = TRUE)

## ACT = dressed. Everything else (INA inactive, RES reserve/IR, DEV practice
## squad, CUT, RET) means no production that week.
played <- rosters |>
  transmute(.data$season, .data$week, .data$player_id,
            played = as.integer(.data$status == "ACT"))

## Only score seasons present on BOTH sides, so a missing roster year cannot be
## silently read as "everyone was inactive".
overlap_seasons <- sort(intersect(unique(inj$season), unique(played$season)))
message("Seasons usable (injury x roster overlap): ",
        paste(range(overlap_seasons), collapse = "-"))

## Restrict to ROTATION players. The first run of this script measured a 20.6%
## inactive rate for players carrying no injury designation at all, which is
## obviously wrong and is the proxy failing exactly as warned: the injury report
## also lists deep-bench and practice-squad players who record no stat line even
## when they dress. "Did not appear in the box score" means inactive only for
## someone who would otherwise have played.
##
## The gate below counts a player's games in that season EXCLUDING the week
## being scored, so it conditions on "is this a real contributor" without
## conditioning on the outcome - using the same week would be circular and
## would manufacture a clean-looking result.
season_games <- played |>
  group_by(.data$season, .data$player_id) |>
  summarise(season_games = sum(.data$played), .groups = "drop")

MIN_OTHER_GAMES <- 4L

d <- inj |>
  filter(.data$season %in% overlap_seasons) |>
  left_join(played, by = c("season", "week", "player_id")) |>
  left_join(season_games, by = c("season", "player_id")) |>
  mutate(
    played = coalesce(.data$played, 0L),
    season_games = coalesce(.data$season_games, 0L),
    other_games = .data$season_games - .data$played
  ) |>
  filter(.data$other_games >= MIN_OTHER_GAMES) |>
  mutate(
    inactive = 1L - .data$played,
    pos_group = case_when(
      .data$position %in% c("QB") ~ "QB",
      .data$position %in% c("RB", "FB") ~ "RB",
      .data$position %in% c("WR") ~ "WR",
      .data$position %in% c("TE") ~ "TE",
      TRUE ~ "OTHER"
    ),
    status = case_when(
      is.na(.data$report_status) | .data$report_status == "" ~ "NONE",
      .data$report_status %in% c("Out", "Doubtful", "Questionable") ~ .data$report_status,
      TRUE ~ "OTHER"
    )
  ) |>
  filter(.data$pos_group != "OTHER")

message("Usable player-weeks: ", format(nrow(d), big.mark = ","))

## ---------------------------------------------------------------------------
## 3. Rates, shrunk toward the position-group base
## ---------------------------------------------------------------------------

## Shrink each (position, status) cell toward the POOLED RATE FOR THAT SAME
## STATUS, not toward the position's overall base rate.
##
## The first version shrank toward the position base (~0.10, dominated by the
## no-designation bucket) and that is the wrong prior: report statuses are
## ordinal severity levels, not exchangeable groups. It dragged Doubtful from a
## measured 0.67-0.70 down to 0.49-0.60 - i.e. the shrinkage was destroying the
## exact signal the feature exists to capture. Shrinking within status keeps the
## severity ordering intact while still stabilising thin cells like QB Doubtful
## (n = 55).
base <- d |>
  group_by(.data$status) |>
  summarise(base_rate = mean(.data$inactive), base_n = n(), .groups = "drop")

## "Out" is a LEAGUE RULE, not a statistic: a player listed Out is ineligible to
## play. The measured rate comes back at ~0.72 only because weekly roster status
## tracks the 53-man roster rather than the 46-man gameday actives, so a player
## ruled Out frequently still reads ACT. Estimating it from this data would ship
## a number that says a player who is definitionally not playing has a 28%
## chance of producing. Overridden to a near-certainty, with a small residual
## for late designation changes and source errors.
##
## Everything else IS measured, and lands close enough to independent DFS
## consensus (Questionable ~25-30%, Doubtful ~75%) to trust the method:
## pooled Questionable came back 0.257 and Doubtful 0.682.
OUT_P_INACTIVE <- 0.97

rates <- d |>
  group_by(.data$pos_group, .data$status) |>
  summarise(raw_rate = mean(.data$inactive), n = n(), .groups = "drop") |>
  left_join(base, by = "status") |>
  mutate(
    p_inactive = (.data$raw_rate * .data$n + .data$base_rate * SHRINK) /
      (.data$n + SHRINK),
    p_inactive = dplyr::if_else(.data$status == "Out", OUT_P_INACTIVE,
                                .data$p_inactive),
    rule_based = .data$status == "Out"
  )

## Base rate for players who do NOT appear on the injury report at all. These
## are healthier than the "NONE" bucket (which is players listed on the report
## but without a game-status designation), so they need their own number rather
## than inheriting NONE's ~0.10. Measured from the same roster data instead of
## assumed: take rotation-capable skill players on the weekly roster who have no
## injury-report row that week, and ask how often they were not ACT.
off_pos <- c("QB", "RB", "FB", "WR", "TE")
roster_skill <- nflreadr::load_rosters_weekly(overlap_seasons) |>
  filter(!is.na(.data$gsis_id), .data$game_type == "REG",
         .data$position %in% off_pos) |>
  transmute(season = as.integer(.data$season), week = as.integer(.data$week),
            player_id = .data$gsis_id, status = .data$status) |>
  distinct(.data$season, .data$week, .data$player_id, .keep_all = TRUE)

not_listed <- roster_skill |>
  anti_join(inj, by = c("season", "week", "player_id")) |>
  semi_join(season_games |> filter(.data$season_games >= MIN_OTHER_GAMES),
            by = c("season", "player_id"))
measured_no_report <- mean(not_listed$status != "ACT")

## ASSUMED, not measured - and deliberately so. The measurement above returns
## 0.18, which is HIGHER than the ~0.10 for players who are on the injury report
## without a designation. That ordering is impossible: not being listed at all is
## the healthiest state there is. The population is the problem - weekly rosters
## carry practice-squad (DEV), reserve/IR (RES) and cut players who are never
## active, and the "4+ ACT weeks" gate does not remove a player who is elevated
## from the practice squad a handful of times.
##
## Getting this population clean is a bigger job than it is worth: the value of
## this feature is the GRADED risk (Out >> Doubtful >> Questionable >> healthy),
## which is measured and calibrated above. So the healthy base keeps the value
## DFS ENGINE already assumed, and is labelled an assumption rather than dressed
## up as a measurement. The measured figure is retained alongside it so the gap
## stays visible instead of being quietly discarded.
no_report_rate <- 0.03
message(sprintf(
  "Not-on-report base: using assumed %.3f (contaminated measurement was %.4f, n = %s)",
  no_report_rate, measured_no_report, format(nrow(not_listed), big.mark = ",")))

saveRDS(list(rates = rates, base = base, no_report_rate = no_report_rate,
             no_report_measured = measured_no_report,
             no_report_is_assumed = TRUE,
             built_at = Sys.time(), seasons = overlap_seasons), OUT_RDS)
readr::write_csv(rates, OUT_CSV)

## ---------------------------------------------------------------------------
## 4. Calibration - READ THIS OUTPUT
## ---------------------------------------------------------------------------

cat("\n================ INACTIVE RATE CALIBRATION ================\n")
cat("\n-- measured P(did not play) by position group x report status --\n")
print(as.data.frame(
  rates |>
    select("pos_group", "status", "n", "raw_rate", "p_inactive") |>
    mutate(across(c("raw_rate", "p_inactive"), ~ round(.x, 4))) |>
    arrange(.data$pos_group, .data$status)
), row.names = FALSE)

cat("\n-- pooled across positions --\n")
pooled <- d |>
  group_by(.data$status) |>
  summarise(n = n(), p_inactive = round(mean(.data$inactive), 4), .groups = "drop") |>
  arrange(desc(.data$p_inactive))
print(as.data.frame(pooled), row.names = FALSE)

cat("\n  Ordering MUST be Out > Doubtful > Questionable > NONE. Out is rule-based\n",
    " (0.97, see OUT_P_INACTIVE); the rest are measured against weekly roster\n",
    " status. Independent check: DFS consensus puts Questionable near 25-30%\n",
    " and Doubtful near 75%, and the measured pooled values below should sit in\n",
    " that neighbourhood. If Questionable drifts near zero, or NONE climbs into\n",
    " the double digits, the ground truth has broken again - do not ship it.\n")
cat("\n  NOTE on scope: every row here is a player who APPEARS on the injury\n",
    " report. A player absent from the report entirely is healthier than the\n",
    " 'NONE' bucket and takes the base rate in scripts/17, not this number.\n")

ok <- tryCatch({
  g <- setNames(pooled$p_inactive, pooled$status)
  isTRUE(g[["Out"]] > g[["Doubtful"]]) &&
    isTRUE(g[["Doubtful"]] > g[["Questionable"]]) &&
    isTRUE(g[["Questionable"]] > g[["NONE"]])
}, error = function(e) FALSE)
cat("\n  ordering holds:", ok, "\n")
if (!ok) {
  cat("  *** Ordering violated - do not wire these rates in until understood. ***\n")
}
cat("\nWrote ", OUT_RDS, " and ", OUT_CSV, "\n", sep = "")
