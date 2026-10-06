# scripts/59_emit_dfs_projection_json.R
# ============================================================================
# Emit the weekly fantasy-prop projections as the DFS ENGINE ingestion contract.
#
# One object per player carrying the PROJECTED COMPONENTS (+ generic PPR + p10/p90
# bands + status), NOT a single pre-scored number. The DFS ENGINE re-scores the
# components per site — DraftKings (full PPR + yardage bonuses) vs FanDuel
# (half-PPR) — so this file is site-agnostic. It carries NO salary: the engine
# joins salaries from its own live DK slate scrape (their salary table is DK-only,
# main-slate-only, and keyed by DK id, not GSIS).
#
# CONTRACT 1.1 — read this before consuming proj_ppr.
#
# scripts/17 now blends DK salary into the headline projection (see
# R/fantasy_salary_stack.R: worth +0.0144 R2 on total PPR, per scripts/80).
# That blend happens at the TOTAL-POINTS level only - there is no per-component
# counterpart to a price on the whole player-week. So on any row where the
# stack fired, proj_ppr and the components no longer describe the same number:
#
#   proj_ppr        salary-informed. The number we actually believe.
#   proj_ppr_model  the pure component model, pre-stack. Sums to `components`.
#   components      UNCHANGED by the stack, still site-agnostic.
#   stack_scale     proj_ppr / proj_ppr_model, or null where not applied.
#
# A consumer re-scoring `components` for DraftKings therefore gets the
# PRE-stack number, not the better one. Three honest options, in order of
# preference: (1) use proj_ppr directly for DK, (2) re-score components and
# multiply by stack_scale to carry the salary information across, or (3)
# ignore the stack entirely and use proj_ppr_model for a genuinely
# salary-free projection. Do NOT mix proj_ppr with separately re-scored
# components and assume they agree - that is the bug this block exists to
# prevent. Both numbers are emitted on every row precisely so the choice is
# explicit rather than accidental.
#
# The stack is fit on DK full-PPR scoring, so applying stack_scale to a
# FanDuel re-score is an approximation - defensible, since salary mostly
# encodes expected USAGE rather than DK-specific scoring quirks, but it is an
# approximation and is flagged as one rather than hidden.
#
# Run AFTER scripts/17 writes the projections CSV. Reads the newest
# outputs/fantasy_prop_*_week<N>_projections.csv unless --file= is given, and
# writes the parallel .json plus a stable outputs/dfs_projections_latest.json.
#
#   & $rscript scripts/59_emit_dfs_projection_json.R
#   & $rscript scripts/59_emit_dfs_projection_json.R --file=outputs/fantasy_prop_2026_week1_projections.csv
# ============================================================================
source("R/utilities.R")
source("R/dfs_salaries.R")
source("R/dfs_value_backtest.R")  # dfs_player_name_key(), for the ownership join
source("R/dfs_ownership.R")
assert_packages()
ensure_directories()

CONTRACT_VERSION <- "1.1"   # 1.1 adds proj_ppr_model / stack_scale (see header)

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", flag, "="), "", hit[[1]])
}

pick_csv <- function() {
  f <- arg_value("--file")
  if (!is.null(f)) {
    if (!file.exists(f)) stop("--file not found: ", f, call. = FALSE)
    return(f)
  }
  fs <- list.files("outputs", pattern = "^fantasy_prop_.*_week\\d+_projections\\.csv$",
                   full.names = TRUE)
  if (!length(fs)) stop("No fantasy_prop_*_week*_projections.csv in outputs/ (run scripts/17 first).",
                        call. = FALSE)
  fs[which.max(file.mtime(fs))]
}

csv_path <- pick_csv()
proj <- readr::read_csv(csv_path, show_col_types = FALSE)
stopifnot(nrow(proj) > 0, "projected_ppr" %in% names(proj))

num <- function(x) { x <- suppressWarnings(as.numeric(x)); if (length(x) && is.finite(x)) round(x, 4) else NA_real_ }
chr <- function(x) if (length(x) && !is.na(x)) as.character(x) else NA_character_

season <- proj$season[[1]]
week   <- proj$week[[1]]

# --------------------------------------------------------------------------
# Ownership (optional, heuristic only)
#
# This file otherwise carries no salary, on purpose - the engine does its own
# live DK scrape (see the file header). The ownership heuristic needs salary
# though, so it only runs when the caller hands it one:
#
#   --salary_file=path/to/current_week_dk_salaries.csv
#
# Accepts either a raw DraftKings site export (the "Name + ID / Salary" sheet
# format scripts/50 and parse_manual_dfs_salary_file() already parse) or this
# project's own salary schema. Without --salary_file, ownership fields are
# omitted from the JSON entirely rather than emitted as nulls, so a consumer
# checking for the key's presence can tell "not computed" apart from "computed
# as zero".
#
# ownership_source is always "heuristic_v1" on every row that has one - never
# imply a calibrated probability. See R/dfs_ownership.R for the methodology
# and its limits, and outputs/dfs_engine_handoff.md for the real-ownership
# capture pipeline this is meant to be replaced by.
salary_file <- arg_value("--salary_file")
ownership <- NULL
if (!is.null(salary_file)) {
  if (!file.exists(salary_file)) {
    stop("--salary_file not found: ", salary_file, call. = FALSE)
  }
  salary_raw <- tryCatch(
    parse_manual_dfs_salary_file(salary_file, season = season, week = week),
    error = function(e) readr::read_csv(salary_file, show_col_types = FALSE)
  )
  # proj may already carry its own "salary" column (scripts/17's DK join for
  # the salary stack, CONTRACT 1.1). The incoming lookup used to be named
  # "salary" too, which collided in the left_join below - dplyr silently
  # suffixed both to salary.x/salary.y, so `joined$salary` did not exist and
  # every downstream ownership lookup failed. Named distinctly here and
  # reconciled by coalesce() instead: prefer proj's own (already validated,
  # same normalizer) and fall back to this file for anything it missed.
  if (!"salary" %in% names(proj)) proj$salary <- NA_real_
  salary_lookup <- salary_raw |>
    dplyr::transmute(
      player_name_key = dfs_player_name_key(.data$player_name),
      position = toupper(.data$position),
      salary_from_file = suppressWarnings(as.numeric(.data$salary))
    ) |>
    dplyr::filter(is.finite(.data$salary_from_file)) |>
    dplyr::distinct(.data$player_name_key, .data$position, .keep_all = TRUE)

  joined <- proj |>
    dplyr::mutate(player_name_key = dfs_player_name_key(.data$player)) |>
    dplyr::left_join(salary_lookup, by = c("player_name_key", "position")) |>
    dplyr::mutate(salary = dplyr::coalesce(
      suppressWarnings(as.numeric(.data$salary)), .data$salary_from_file
    ))
  match_rate <- mean(is.finite(joined$salary))
  cat(sprintf("Ownership: matched salary for %.1f%% of players (%d of %d).\n",
              100 * match_rate, sum(is.finite(joined$salary)), nrow(joined)))

  ownership <- dfs_ownership_heuristic(dplyr::filter(joined, is.finite(.data$salary))) |>
    dplyr::select("player_id", "ownership_heuristic_score", "ownership_tier",
                 "ownership_source")
} else {
  cat("No --salary_file given; ownership fields omitted from this file.\n")
}
ownership_for <- function(player_id) {
  if (is.null(ownership)) return(NULL)
  hit <- ownership[ownership$player_id == player_id, ]
  if (!nrow(hit)) return(NULL)
  list(
    heuristic_score = num(hit$ownership_heuristic_score[[1]]),
    tier = chr(hit$ownership_tier[[1]]),
    source = "heuristic_v1"
  )
}

# main-slate date = modal game_date (the Sunday for a normal week)
slate_date <- as.character(names(sort(table(as.character(proj$game_date)), decreasing = TRUE))[1])

players <- lapply(seq_len(nrow(proj)), function(i) {
  r <- proj[i, ]
  list(
    player_id  = chr(r$player_id),        # nflverse GSIS id (join key)
    name       = chr(r$player),
    pos        = chr(r$position),
    team       = chr(r$team),
    opponent   = chr(r$opponent_team),
    game_id    = chr(r$game_id),
    game_date  = chr(r$game_date),
    proj_ppr   = num(r$projected_ppr),     # salary-stacked where available
    # Pre-stack model number. This is what `components` sums to; proj_ppr is
    # not, on any row where salary_stack_applied is true. See the header.
    proj_ppr_model = if ("projected_ppr_model" %in% names(r)) {
      num(r$projected_ppr_model)
    } else {
      num(r$projected_ppr)
    },
    salary_stack_applied = if ("salary_stack_applied" %in% names(r)) {
      isTRUE(as.logical(r$salary_stack_applied))
    } else {
      FALSE
    },
    stack_scale = {
      m <- if ("projected_ppr_model" %in% names(r)) {
        suppressWarnings(as.numeric(r$projected_ppr_model))
      } else {
        NA_real_
      }
      p <- suppressWarnings(as.numeric(r$projected_ppr))
      if (length(m) && is.finite(m) && m > 0 && is.finite(p)) round(p / m, 4) else NA_real_
    },
    ppr_low    = num(r$ppr_low),           # p10 / p90 of PPR residual, by position
    ppr_high   = num(r$ppr_high),
    # P(does not play), graded from this week's injury report (scripts/83).
    # Consumers previously had no choice but to assume a flat rate; DFS ENGINE
    # hardcoded 0.03 for every player, pricing a Questionable RB identically to
    # an ironman starter. Absent for a player only if the injury feed was
    # unavailable at generation time - omitted rather than defaulted, so a
    # consumer can tell "unknown" from "known to be low risk".
    p_zero     = if ("p_zero" %in% names(r)) num(r$p_zero) else NA_real_,
    injury_status = if ("injury_status" %in% names(r)) chr(r$injury_status) else NA_character_,
    # Player-specific spread (scripts/84). ppr_low/ppr_high above are now
    # per-player rather than one fixed width per position, and sim_sd is derived
    # from that same span so the simulator and the displayed band agree. A
    # consumer deriving sd from the bands itself will get the same answer.
    sim_sd     = if ("sim_sd" %in% names(r)) num(r$sim_sd) else NA_real_,
    components = list(                      # expected values -> engine scores per site
      pass_yds      = num(r$projected_passing_yards),
      pass_td       = num(r$projected_passing_tds),
      interceptions = num(r$projected_interceptions),
      rush_yds      = num(r$projected_rushing_yards),
      rush_td       = num(r$projected_rushing_tds),
      receptions    = num(r$projected_receptions),
      rec_yds       = num(r$projected_receiving_yards),
      rec_td        = num(r$projected_receiving_tds),
      fumbles_lost  = num(r$projected_fumbles_lost),
      two_pt        = num(r$projected_two_point_conversions)),
    status      = chr(r$projection_status),
    role_status = chr(r$role_status),
    ownership   = ownership_for(chr(r$player_id)))
})

out <- list(
  source           = "nfl-modeling",
  sport            = "nfl",
  contract_version = CONTRACT_VERSION,
  # "components" still describes the components block, but proj_ppr is now
  # salary-informed while the components are not - hence the explicit flag
  # rather than leaving a consumer to infer it from a version bump.
  scoring          = "components",
  headline_is_salary_stacked = TRUE,
  ownership_source = if (is.null(ownership)) "none" else "heuristic_v1",
  season           = season,
  week             = week,
  slate_date       = slate_date,
  generated_at     = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
  n_players        = length(players),
  players          = players)

out_path <- file.path("outputs", sub("\\.csv$", ".json", basename(csv_path)))
jsonlite::write_json(out, out_path, auto_unbox = TRUE, pretty = TRUE, na = "null")
# stable pointer so the engine never has to guess the week
jsonlite::write_json(out, file.path("outputs", "dfs_projections_latest.json"),
                     auto_unbox = TRUE, pretty = TRUE, na = "null")

cat(sprintf("Wrote %s + dfs_projections_latest.json\n  %d players | season %s week %s | slate %s\n",
            out_path, length(players), season, week, slate_date))
