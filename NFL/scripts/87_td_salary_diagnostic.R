## ---------------------------------------------------------------------------
## 87_td_salary_diagnostic.R
##
## Annotates the touchdown bet card with a DK-salary sanity check.
##
## Deliberately a DIAGNOSTIC, not a feature. DK salary is a player-level market
## price set by the same operator that prices the anytime-TD props, and this
## model's edge comes from being derived independently of that market - it takes
## only GAME-level market inputs (total, spread, implied team total) on purpose.
## Feeding salary in as a feature would import the market's own player-level
## opinion through the back door and make a parity model more market-like,
## shrinking edge rather than growing it.
##
## What it IS good for: catching role misreads. If the model loves a player DK
## prices near the positional minimum, that is either a genuine edge or the
## model has his role wrong - and the second case is the one that quietly loses
## money. This flags it without touching the model.
##
## Adds:
##   salary            DK salary for the player that week (NA if not on slate)
##   salary_pct        his salary's percentile within his own position that week
##   salary_min_flag   TRUE if priced at/near the positional minimum
##   role_disagree     TRUE if the model ranks him top-40% while DK prices him
##                     bottom-25% - the actionable conflict
##
## Run:  & $rscript scripts/87_td_salary_diagnostic.R
## ---------------------------------------------------------------------------

source("R/utilities.R")
suppressPackageStartupMessages({
  library(dplyr)
})
source("R/dfs_value_backtest.R")   # dfs_player_name_key()
assert_packages()
ensure_directories()

CARD <- "outputs/td_2026_bet_card.csv"
SALARY <- "outputs/dfs_salaries_dk_current.csv"
OUT <- "outputs/td_2026_bet_card_annotated.csv"

if (!file.exists(CARD)) {
  stop("No bet card at ", CARD, " - run scripts/15_build_2026_td_board.R first.",
       call. = FALSE)
}
card <- readr::read_csv(CARD, show_col_types = FALSE)
message("Bet card rows: ", format(nrow(card), big.mark = ","))

prob_col <- intersect(c("model_probability", "model_prob", "td_probability",
                        "probability"), names(card))[1]
if (is.na(prob_col)) {
  message("No model probability column found; ranking on row order instead.")
}

if (!file.exists(SALARY)) {
  message("No ", SALARY, " - run scripts/21 then scripts/81. Writing card unannotated.")
  readr::write_csv(card, OUT)
  quit(save = "no", status = 0)
}

sal <- readr::read_csv(SALARY, show_col_types = FALSE, guess_max = 50000) |>
  filter(toupper(.data$site) == "DK",
         .data$position %in% c("QB", "RB", "WR", "TE"),
         is.finite(.data$salary), .data$salary > 0) |>
  mutate(
    key = paste(normalize_team(.data$team), .data$position,
                dfs_player_name_key(.data$player_name), sep = "|")
  ) |>
  arrange(desc(.data$salary)) |>
  distinct(.data$key, .keep_all = TRUE) |>
  select("key", "salary", sal_week = "week")

n_before <- nrow(card)
ann <- card |>
  mutate(key = paste(normalize_team(.data$team), .data$position,
                     dfs_player_name_key(.data$player), sep = "|")) |>
  left_join(sal, by = "key")
if (nrow(ann) != n_before) {
  stop("Salary annotation changed row count: ", n_before, " -> ", nrow(ann),
       call. = FALSE)
}

ann <- ann |>
  group_by(.data$position) |>
  mutate(
    salary_pct = dplyr::if_else(
      is.finite(.data$salary),
      dplyr::percent_rank(.data$salary),
      NA_real_
    ),
    pos_min = suppressWarnings(min(.data$salary, na.rm = TRUE))
  ) |>
  ungroup() |>
  mutate(
    salary_min_flag = is.finite(.data$salary) & is.finite(.data$pos_min) &
      .data$salary <= .data$pos_min * 1.05
  )

if (!is.na(prob_col)) {
  ann <- ann |>
    mutate(model_pct = dplyr::percent_rank(.data[[prob_col]])) |>
    mutate(
      role_disagree = is.finite(.data$salary) &
        .data$model_pct >= 0.60 & .data$salary_pct <= 0.25
    )
} else {
  ann$model_pct <- NA_real_
  ann$role_disagree <- FALSE
}

ann <- ann |> select(-dplyr::any_of(c("key", "pos_min")))
readr::write_csv(ann, OUT)

cat("\n================ SALARY DIAGNOSTIC ================\n")
cat(sprintf("\nsalary matched: %.1f%% of %s card rows\n",
            100 * mean(is.finite(ann$salary)), format(nrow(ann), big.mark = ",")))
cat(sprintf("priced at/near positional minimum: %d\n", sum(ann$salary_min_flag, na.rm = TRUE)))
cat(sprintf("ROLE DISAGREEMENTS (model top-40%%, DK bottom-25%%): %d\n",
            sum(ann$role_disagree, na.rm = TRUE)))

if (any(ann$role_disagree, na.rm = TRUE)) {
  cat("\n-- review these: model likes them, DK's pricing does not --\n")
  cols <- intersect(c("player", "position", "team", "opponent_team",
                      prob_col, "model_pct", "salary", "salary_pct"),
                    names(ann))
  print(as.data.frame(
    ann |> filter(.data$role_disagree) |>
      arrange(desc(.data$model_pct)) |>
      select(all_of(cols)) |>
      mutate(across(where(is.numeric), ~ round(.x, 4))) |>
      head(15)
  ), row.names = FALSE)
  cat("\n  Each is a genuine edge OR a role misread. DK reprices weekly off the\n",
      " depth chart; if it says minimum and we say top-40%, check the depth\n",
      " chart before betting.\n")
} else {
  cat("\n  No role disagreements on the current card.\n")
}
cat("\nWrote ", OUT, "\n", sep = "")
