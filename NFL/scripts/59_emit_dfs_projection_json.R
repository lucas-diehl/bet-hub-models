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
# Run AFTER scripts/17 writes the projections CSV. Reads the newest
# outputs/fantasy_prop_*_week<N>_projections.csv unless --file= is given, and
# writes the parallel .json plus a stable outputs/dfs_projections_latest.json.
#
#   & $rscript scripts/59_emit_dfs_projection_json.R
#   & $rscript scripts/59_emit_dfs_projection_json.R --file=outputs/fantasy_prop_2026_week1_projections.csv
# ============================================================================
source("R/utilities.R")
assert_packages()
ensure_directories()

CONTRACT_VERSION <- "1.0"

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
    proj_ppr   = num(r$projected_ppr),     # generic full-PPR (reference only)
    ppr_low    = num(r$ppr_low),           # p10 / p90 of PPR residual, by position
    ppr_high   = num(r$ppr_high),
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
    role_status = chr(r$role_status))
})

out <- list(
  source           = "nfl-modeling",
  sport            = "nfl",
  contract_version = CONTRACT_VERSION,
  scoring          = "components",   # engine re-scores per site; no salary in this file
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
