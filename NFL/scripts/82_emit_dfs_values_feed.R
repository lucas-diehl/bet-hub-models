## ---------------------------------------------------------------------------
## 82_emit_dfs_values_feed.R
##
## Publishes the weekly NFL fantasy projections to the Bet Hub as a
## values_<date>.json feed file - the site's DFS value-plays contract, rendered
## live on the /dfs page.
##
## This file type could not be produced before today. Its required headline
## metric is `value` = projected points per $1,000 of salary, and until
## scripts/21 + scripts/81 started capturing current-season DraftKings prices
## there was no 2026 salary in the repo at all, so there was no denominator.
##
## Contract: packages/contract/src/schema.ts, ValuesFileSchema / ValueItemSchema.
## Required per item: name, salary, proj, value. Optional: rank, team, position,
## ceiling, ownership, exposure, note. Zod STRIPS unknown keys rather than
## rejecting them, so an invented field name does not fail loudly - it silently
## vanishes and the column renders empty. That bug already bit this project once
## on the board feed, so every field below is copied from the schema, not
## guessed.
##
## Scoring note: `proj` is documented as "Projected DK fantasy points". We emit
## full PPR, which is DK's base scoring, and deliberately do NOT add DK's
## yardage bonuses (+3 at 100 rush/rec yards, +3 at 300 pass yards). Those are
## threshold bonuses, and applying a threshold to an EXPECTED value is not the
## expected value of the bonus - E[bonus(yards)] != bonus(E[yards]). Adding them
## naively would inflate every high-volume player. The gap is small and
## understated rather than overstated, which is the right direction to be wrong.
##
##   --week=3    which week to publish (default: newest projections file)
##   --execute   write the feed file (otherwise dry run, prints only)
## ---------------------------------------------------------------------------

source("R/utilities.R")
source("R/dashboard_feed.R")
source("R/dfs_value_backtest.R")   # dfs_player_name_key()
assert_packages()
ensure_directories()

CONTRACT_VERSION <- "1.0"

## Position-balanced, and not by preference - a raw points-per-$1k sort is
## actively misleading in NFL DFS. QBs score the most points in full PPR and DK
## prices them modestly, while TEs are the cheapest position on the board, so an
## unbalanced value list comes back as ~13 QBs and TEs out of 15 (measured, not
## assumed - that is exactly what the first run of this script produced). You
## roster ONE quarterback and ONE tight end, so that board cannot be acted on.
##
## Caps below track DK classic roster shape (QB1 / RB2 / WR3 / TE1 / FLEX / DST)
## with enough depth at each slot to actually choose between options. Items are
## still sorted by value within the final list, so the headline metric is
## unchanged - it is the candidate pool that is made representative.
POSITION_CAPS <- c(QB = 4L, RB = 8L, WR = 10L, TE = 4L)

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", flag, "="), "", hit[[1]])
}
execute <- "--execute" %in% args

## ---------------------------------------------------------------------------
## 1. Projections
## ---------------------------------------------------------------------------

week_arg <- arg_value("--week")
proj_path <- if (!is.null(week_arg)) {
  p <- sprintf("outputs/fantasy_prop_2026_week%s_projections.csv", week_arg)
  if (!file.exists(p)) stop("No projections at ", p, call. = FALSE)
  p
} else {
  fs <- list.files("outputs",
                   pattern = "^fantasy_prop_.*_week\\d+_projections\\.csv$",
                   full.names = TRUE)
  if (!length(fs)) {
    stop("No fantasy_prop_*_week*_projections.csv in outputs/ - run scripts/17 first.",
         call. = FALSE)
  }
  fs[which.max(file.mtime(fs))]
}
proj <- readr::read_csv(proj_path, show_col_types = FALSE)
cat("Projections:", proj_path, "-", nrow(proj), "players\n")

season <- as.integer(proj$season[[1]])
target_week <- as.integer(proj$week[[1]])

## ---------------------------------------------------------------------------
## 2. Salary
##
## Prefer the salary already carried on the projection board (scripts/17 joins
## it for the stack, so it is by construction the same number the projection was
## built with). Fall back to the current salary table for rows the board did not
## carry. Publishing a `value` computed against a different salary than the
## projection used would make the headline metric quietly incoherent.
## ---------------------------------------------------------------------------

if (!"salary" %in% names(proj)) proj$salary <- NA_real_
n_board_salary <- sum(is.finite(proj$salary))

salary_path <- "outputs/dfs_salaries_dk_current.csv"
if (file.exists(salary_path)) {
  sal <- readr::read_csv(salary_path, show_col_types = FALSE, guess_max = 50000) |>
    dplyr::filter(
      toupper(.data$site) == "DK",
      as.integer(.data$season) == season,
      as.integer(.data$week) == target_week,
      is.finite(.data$salary), .data$salary > 0
    ) |>
    dplyr::mutate(
      join_key = paste(normalize_team(.data$team), .data$position,
                       dfs_player_name_key(.data$player_name), sep = "|")
    ) |>
    dplyr::arrange(dplyr::desc(.data$salary)) |>
    dplyr::distinct(.data$join_key, .keep_all = TRUE) |>
    dplyr::select("join_key", fallback_salary = "salary")

  proj <- proj |>
    dplyr::mutate(
      join_key = paste(normalize_team(.data$team), .data$position,
                       dfs_player_name_key(.data$player), sep = "|")
    ) |>
    dplyr::left_join(sal, by = "join_key") |>
    dplyr::mutate(
      salary = dplyr::if_else(is.finite(.data$salary), .data$salary,
                              .data$fallback_salary)
    )
} else {
  cat("No", salary_path, "- relying on board-carried salary only.\n")
}

priced <- proj |> dplyr::filter(is.finite(.data$salary), .data$salary > 0)
cat(sprintf("Salary available for %d of %d players (%d from the board).\n",
            nrow(priced), nrow(proj), n_board_salary))
if (!nrow(priced)) {
  stop("No player has a salary, so points-per-$1k cannot be computed. ",
       "Run scripts/21_capture_current_dk_salaries.R then ",
       "scripts/81_build_current_salary_table.R.", call. = FALSE)
}

## ---------------------------------------------------------------------------
## 3. Build the value items
## ---------------------------------------------------------------------------

items_df <- priced |>
  dplyr::transmute(
    name = as.character(.data$player),
    team = as.character(.data$team),
    position = as.character(.data$position),
    salary = as.integer(round(.data$salary)),
    proj = round(as.numeric(.data$projected_ppr), 2),
    value = round(as.numeric(.data$projected_ppr) / (.data$salary / 1000), 3),
    ceiling = if ("ppr_high" %in% names(priced)) {
      round(as.numeric(.data$ppr_high), 2)
    } else {
      NA_real_
    },
    ## NOT isTRUE(): it is not vectorized - it demands a length-1 TRUE and so
    ## collapses an entire column to one FALSE inside transmute(). That shipped
    ## a feed where Brock Purdy carried a $6,200 salary and a
    ## "model-only (no DK salary in blend)" note at the same time. `%in% TRUE`
    ## is vectorized and NA-safe.
    stacked = if ("salary_stack_applied" %in% names(priced)) {
      as.logical(.data$salary_stack_applied) %in% TRUE
    } else {
      FALSE
    }
  ) |>
  dplyr::filter(is.finite(.data$proj), is.finite(.data$value)) |>
  ## Take the best value plays WITHIN each position, then re-sort the combined
  ## pool by value. See POSITION_CAPS above for why this is not optional.
  dplyr::filter(.data$position %in% names(POSITION_CAPS)) |>
  dplyr::group_by(.data$position) |>
  dplyr::arrange(dplyr::desc(.data$value), .by_group = TRUE) |>
  dplyr::slice_head(n = max(POSITION_CAPS)) |>
  dplyr::filter(dplyr::row_number() <= POSITION_CAPS[[dplyr::first(.data$position)]]) |>
  dplyr::ungroup() |>
  dplyr::arrange(dplyr::desc(.data$value)) |>
  dplyr::mutate(rank = dplyr::row_number())

## Slate date = the modal game date for the week (the Sunday on a normal week).
slate_date <- names(sort(table(as.character(priced$game_date)), decreasing = TRUE))[1]

compact <- function(l) l[!vapply(l, function(x) is.null(x) || (length(x) == 1 && is.na(x)), logical(1))]

values <- lapply(seq_len(nrow(items_df)), function(i) {
  r <- items_df[i, ]
  compact(list(
    rank = as.integer(r$rank),
    name = r$name,
    team = r$team,
    position = r$position,
    salary = as.integer(r$salary),
    proj = as.numeric(r$proj),
    value = as.numeric(r$value),
    ceiling = if (is.finite(r$ceiling)) as.numeric(r$ceiling) else NULL,
    note = if (isTRUE(r$stacked)) NULL else "model-only (no DK salary in blend)"
  ))
})

out <- list(
  contract_version = CONTRACT_VERSION,
  source = "nfl-modeling",
  sport = "nfl",
  site = "draftkings",
  slate_date = slate_date,
  slate_label = "Main",
  generated_at = format(as.POSIXct(Sys.time(), tz = "UTC"),
                        "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  values = values
)

cat("\n=== Preview: top 15 by points per $1k ===\n")
print(as.data.frame(
  items_df |>
    dplyr::select("rank", "name", "position", "team", "salary", "proj", "value") |>
    head(15)
), row.names = FALSE)

cat(sprintf("\nSlate date: %s | week %d | %d items\n",
            slate_date, target_week, length(values)))

if (!execute) {
  cat("\nDry run. Add --execute to write the feed file.\n")
  quit(save = "no", status = 0)
}

outdir <- file.path(feed_root(), "nfl-modeling", "nfl")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
path <- file.path(outdir, sprintf("values_%s.json", slate_date))
jsonlite::write_json(out, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
cat("Wrote", length(values), "value plays to", path, "\n")
