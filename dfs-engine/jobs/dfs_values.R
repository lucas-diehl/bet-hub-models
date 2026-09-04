#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — jobs/dfs_values.R
# Writes the top-value feed the bet site's /dfs page reads (docs/sources/dfs-values.md).
# One JSON per sport per slate day:
#   <FEED_DIR>/dfs-engine/<sport>/values_<YYYY-MM-DD>.json
#
# PRIMARY VIEW = optimizer EXPOSURE: run the spine GPP optimizer at 40% max exposure
# for 20 entries, tally each player's exposure = (#lineups with player)/20, emit the
# top 10 by exposure (desc) with an `exposure` field (0..1) + value/proj/ownership.
# FALLBACK (no slate today / optimizer fails) = the value view (top-10 by proj-per-$1k,
# no `exposure` field); per the contract the page then falls back to showing `value`.
#
#   Rscript jobs/dfs_values.R              # write all sports
# Sourced by jobs/run_all.R (DFS_VALUES_SOURCE_ONLY=1) which then calls write_dfs_values().
# ==============================================================================
local({
  if (!exists("DFS_ROOT")) {
    a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
    root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
    bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
    source(bp); dfs_load_spine()
  }
})
suppressWarnings(suppressPackageStartupMessages({ library(data.table); library(DBI); library(jsonlite) }))

VALUES_CONTRACT_VERSION <- "1.0"
VALUE_MAX_SAL_PCTILE    <- 0.80          # value fallback: exclude priciest ~20%
N_VALUES                <- 10L           # top N players emitted
N_ENTRIES               <- 20L           # GPP entries for the exposure build
MAX_EXPOSURE            <- 0.40          # per-player exposure cap
DFS_SPORTS              <- c("golf", "wnba", "tennis", "nfl", "ncaaf")
dfs_feed_dir <- function() Sys.getenv("FEED_DIR", "C:/Users/ljdie/OneDrive/Documents/dashboard_feed")

# "Last, First" -> "First Last" (golf stores names comma-first)
.disp_name <- function(x) { x <- as.character(x)
  ifelse(grepl(",", x), sub("^\\s*([^,]+),\\s*(.*)$", "\\2 \\1", trimws(x)), x) }
.dfs_slate_date  <- function(slate_id) sub("^.*-[a-z]+-(\\d{4}-\\d{2}-\\d{2})-.*$", "\\1", slate_id)  # site-agnostic (dk|fd)
.dfs_slate_label <- function(slate_id) { s <- sub("^.*-\\d{4}-\\d{2}-\\d{2}-", "", slate_id)
  if (!nchar(s)) "Main" else paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s))) }

# ── EXPOSURE view (primary) ───────────────────────────────────────────────────
# greedy exposure-capped GPP portfolio from graded candidates (res from run_slate).
# Picks the best-EV lineup that keeps every player at/under the 40% cap, up to 20.
.exposure_counts <- function(res, n = N_ENTRIES, max_exp = MAX_EXPOSURE) {
  res <- as.data.table(res)
  if (!nrow(res) || !"idx" %in% names(res)) return(NULL)
  if ("gpp_ev" %in% names(res)) res <- res[order(-gpp_ev)]
  cap <- max(1L, floor(n * max_exp))                       # 8 lineups/player at 20 & 0.40
  expo <- integer(0); seen <- character(0); built <- 0L
  cnt <- function(k) { v <- expo[k]; if (is.na(v)) 0L else v }
  for (i in seq_len(nrow(res))) {
    if (built >= n) break
    idx <- res$idx[[i]]; key <- paste(sort(idx), collapse = "-")
    if (key %in% seen) next
    if (!all(vapply(idx, function(j) cnt(as.character(j)) < cap, logical(1)))) next
    seen <- c(seen, key); built <- built + 1L
    for (j in idx) { k <- as.character(j); expo[k] <- cnt(k) + 1L }
  }
  if (!length(expo)) return(NULL)
  data.table(idx = as.integer(names(expo)), cnt = as.integer(expo), n_built = built)
}

.dfs_exposure_items <- function(pool, res, sport) {
  ec <- .exposure_counts(res); if (is.null(ec)) return(NULL)
  pool <- as.data.table(pool)
  ec[, exposure := cnt / N_ENTRIES]
  ec <- ec[order(-exposure, -cnt)][seq_len(min(N_VALUES, .N))]
  individual <- sport %in% c("golf", "tennis")
  nmcol <- if ("player_name" %in% names(pool)) "player_name" else "name"
  lapply(seq_len(nrow(ec)), function(i) { r <- pool[ec$idx[i]]
    sal <- as.numeric(r$salary); pr <- as.numeric(r$proj)
    item <- list(rank = i, name = .disp_name(r[[nmcol]] %||% ""),
                 team = if (individual) "" else as.character(r$team %||% ""),
                 position = if (individual) "" else as.character(r$position %||% ""),
                 salary = as.integer(sal), proj = round(pr, 1),
                 value = round(pr / (sal / 1000), 2), exposure = round(ec$exposure[i], 4))
    if ("ceil" %in% names(r) && is.finite(r$ceil)) item$ceiling   <- round(as.numeric(r$ceil), 1)
    if ("own"  %in% names(r) && is.finite(r$own))  item$ownership <- round(as.numeric(r$own), 4)  # pool own is 0..1
    item })
}

# ── BY-POSITION view (team sports: NFL / WNBA) ────────────────────────────────
# The bet site's DFS page shows "2 options per position" for football. Golf/tennis are
# individual (no positions) so this is skipped there. Groups the exposure portfolio by
# roster position, top-2 per position by EXPOSURE (the optimizer's actual lean), value as
# the tiebreak; a FLEX group (top-2 flex-eligible not already shown) when the roster has one.
.dfs_by_position <- function(pool, res, rr, sport) {
  ec <- .exposure_counts(res); if (is.null(ec)) return(NULL)
  pool <- as.data.table(pool); ec[, exposure := cnt / N_ENTRIES]
  nmcol <- if ("player_name" %in% names(pool)) "player_name" else "name"
  P <- pool[ec$idx]
  P[, `:=`(.exp = ec$exposure, .row = .I,
           .val = round(as.numeric(proj) / pmax(as.numeric(salary) / 1000, 0.1), 2),
           .pos = toupper(trimws(as.character(position))))]
  mk <- function(sub) lapply(seq_len(nrow(sub)), function(i) { r <- sub[i]
    item <- list(rank = i, name = .disp_name(r[[nmcol]] %||% ""),
                 team = as.character(r$team %||% ""), position = as.character(r$position %||% ""),
                 salary = as.integer(r$salary), proj = round(as.numeric(r$proj), 1),
                 value = r$.val, exposure = round(r$.exp, 4))
    if ("ceil" %in% names(r) && is.finite(r$ceil)) item$ceiling   <- round(as.numeric(r$ceil), 1)
    if ("own"  %in% names(r) && is.finite(r$own))  item$ownership <- round(as.numeric(r$own), 4)
    item })
  out <- list(); used <- integer(0)
  for (pos in sort(unique(P$.pos))) {
    if (is.na(pos) || !nzchar(pos)) next
    sub <- P[.pos == pos][order(-.exp, -.val)][seq_len(min(2L, .N))]
    if (nrow(sub)) { out[[pos]] <- mk(sub); used <- c(used, sub$.row) }
  }
  flexpos <- toupper(rr$flex$positions %||% character(0))          # RB/WR/TE for NFL
  if (length(flexpos)) {
    sub <- P[.pos %in% flexpos & !(.row %in% used)][order(-.exp, -.val)][seq_len(min(2L, .N))]
    if (nrow(sub)) out[["FLEX"]] <- mk(sub)
  }
  if (!length(out)) NULL else out
}

# ── VALUE view (fallback) ─────────────────────────────────────────────────────
.dfs_latest_slate_id <- function(sport, site = "dk") {
  q <- "SELECT slate_id FROM projections WHERE slate_id LIKE ?
        GROUP BY slate_id ORDER BY MAX(version_ts) DESC LIMIT 1"
  r <- tryCatch(with_db(function(con) DBI::dbGetQuery(con, q, params = list(paste0(sport, "-", site, "-%-main"))),
                        read_only = TRUE), error = function(e) NULL)
  if (is.null(r) || !nrow(r)) NULL else r$slate_id[1]
}
# Does a salaries slate exist for this sport+site today-ish? (FD ingest writes salaries,
# not projections — run_slate makes projections on the fly — so the FD gate keys off salaries.)
.dfs_has_slate <- function(sport, site = "fd") {
  q <- "SELECT 1 FROM salaries WHERE slate_id LIKE ? GROUP BY slate_id LIMIT 1"
  r <- tryCatch(with_db(function(con) DBI::dbGetQuery(con, q, params = list(paste0(sport, "-", site, "-%-main"))),
                        read_only = TRUE), error = function(e) NULL)
  !is.null(r) && nrow(r) > 0
}
.dfs_value_pool <- function(slate_id) {
  q <- "WITH latest AS (
          SELECT player_id, proj_mean, ceil,
                 ROW_NUMBER() OVER (PARTITION BY player_id ORDER BY version_ts DESC) rn
          FROM projections WHERE slate_id = ?)
        SELECT sa.player_id, pl.name, sa.team, sa.position, sa.salary,
               l.proj_mean AS proj, l.ceil, ow.projected_pct AS own
        FROM salaries sa
        JOIN latest l ON l.player_id = sa.player_id AND l.rn = 1
        LEFT JOIN players pl ON pl.player_id = sa.player_id AND pl.sport = sa.sport
        LEFT JOIN (SELECT slate_id, player_id, MAX(projected_pct) projected_pct
                   FROM ownership GROUP BY slate_id, player_id) ow
             ON ow.slate_id = sa.slate_id AND ow.player_id = sa.player_id
        WHERE sa.slate_id = ? AND sa.salary > 0"
  as.data.table(with_db(function(con) DBI::dbGetQuery(con, q, params = list(slate_id, slate_id)),
                        read_only = TRUE))
}
.dfs_value_items <- function(pool, sport) {
  d <- pool[is.finite(proj) & proj > 0 & is.finite(salary) & salary > 0]
  if (!nrow(d)) return(list())
  individual <- sport %in% c("golf", "tennis")
  cap <- as.numeric(quantile(d$salary, VALUE_MAX_SAL_PCTILE, na.rm = TRUE, names = FALSE))
  d[, value := proj / (salary / 1000)]
  v <- head(d[salary <= cap][order(-value)], N_VALUES)
  lapply(seq_len(nrow(v)), function(i) { r <- v[i]
    item <- list(rank = i, name = .disp_name(r$name %||% ""),
                 team = if (individual) "" else as.character(r$team %||% ""),
                 position = if (individual) "" else as.character(r$position %||% ""),
                 salary = as.integer(r$salary), proj = round(as.numeric(r$proj), 1),
                 value = round(as.numeric(r$value), 2))
    if (is.finite(r$ceil)) item$ceiling   <- round(as.numeric(r$ceil), 1)
    if (is.finite(r$own))  item$ownership <- round(as.numeric(r$own) / 100, 4)   # DB 0-100 -> 0..1
    item })
}

# ── write one sport's file (site = "dk" or "fd") ──────────────────────────────
write_dfs_values_sport <- function(sport, site = "dk") {
  site_label <- if (tolower(site) %in% c("fd", "fanduel")) "fanduel" else "draftkings"
  vals <- NULL; bypos <- NULL; slate_date <- NULL; slate_label <- "Main"
  # PRIMARY: run the GPP optimizer for today's slate at this site (read-only: never persists)
  rs <- tryCatch(run_slate(sport, Sys.Date(), site = site, n_sims = 2500L, n_cand = 400L,
                           n_lineups = 8L, field_n = 500L, persist = FALSE),   # n>=3 (build_portfolio needs n_bal>=0)
                 error = function(e) { msg("dfs values:", sport, site_label, "- optimizer:", conditionMessage(e)); NULL })
  if (!is.null(rs) && !is.null(rs$res)) {
    vals <- tryCatch(.dfs_exposure_items(rs$pool, rs$res, sport), error = function(e) NULL)
    if (!is.null(vals) && length(vals)) { slate_date <- .dfs_slate_date(rs$slate_id); slate_label <- .dfs_slate_label(rs$slate_id) }
    # team sports (NFL/WNBA) also get the position-grouped "2 per position" view
    if (!(sport %in% c("golf", "tennis")) && !is.null(vals) && length(vals))
      bypos <- tryCatch(.dfs_by_position(rs$pool, rs$res, rs$rr, sport), error = function(e) NULL)
  }
  # FALLBACK: value view from the freshest logged slate at this site (no exposure field)
  if (is.null(vals) || !length(vals)) {
    sid <- .dfs_latest_slate_id(sport, site)
    if (is.null(sid)) { vals <- list(); slate_date <- as.character(Sys.Date()) }
    else { vals <- .dfs_value_items(.dfs_value_pool(sid), sport)
           slate_date <- .dfs_slate_date(sid); slate_label <- .dfs_slate_label(sid) }
  }
  out <- list(contract_version = VALUES_CONTRACT_VERSION, source = "dfs-engine", sport = sport,
              site = site_label, slate_date = slate_date, slate_label = slate_label,
              generated_at = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
              values = vals)
  if (!is.null(bypos) && length(bypos)) out$by_position <- bypos   # team sports only
  dir <- file.path(dfs_feed_dir(), "dfs-engine", sport); dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(dir, sprintf("values_%s_%s.json", slate_date, site_label))   # site-suffixed: no DK/FD collision
  old <- file.path(dir, sprintf("values_%s.json", slate_date))                # retire the pre-suffix DK name
  if (site_label == "draftkings" && file.exists(old)) try(file.remove(old), silent = TRUE)
  txt <- as.character(jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE, null = "null"))
  if (!length(vals)) txt <- sub('"values"\\s*:\\s*\\{\\s*\\}', '"values": []', txt)   # [] not {} when empty
  writeLines(txt, f, useBytes = TRUE)
  view <- if (length(vals) && !is.null(vals[[1]]$exposure)) "exposure" else "value"
  msg("dfs values:", sport, site_label, "->", basename(f), "(", length(vals), "names,", view, "view )")
  invisible(f)
}

# ── ROUND-SPECIFIC exposure feed (golf single-round DFS) ──────────────────────
# Same top-10-by-exposure view as the tournament feed, but for ONE round, written to a
# SEPARATE file with clear round headers (slate_type="single_round", round, round_label)
# so the bet site never confuses it with the full-tournament board. Auto-detects the live
# DK single-round slate; uses the v2-round projection (per-tee-time wind + FRL) via the
# adapter's single-round path. Distinct slate id (golf-<site>-<date>-r<N>) -> never
# collides with the tournament slate's salaries/lineups.
write_dfs_round_values <- function(round = NULL, site = "dk") {
  site_label <- if (tolower(site) %in% c("fd", "fanduel")) "fanduel" else "draftkings"
  lr <- tryCatch(golf_live_round(), error = function(e) NULL)
  if (is.null(lr)) { msg("dfs round values: no live DK single-round golf slate"); return(invisible(NULL)) }
  rnd <- round %||% lr$round
  extra <- list(single_round = TRUE, round = rnd, draft_group_id = lr$draft_group_id)
  rs <- tryCatch(run_slate("golf", Sys.Date(), slate = paste0("r", rnd), site = site,
                           n_sims = 2500L, n_cand = 400L, n_lineups = 8L, field_n = 500L,
                           persist = FALSE, extra = extra),
                 error = function(e) { msg("dfs round values: optimizer:", conditionMessage(e)); NULL })
  vals <- if (!is.null(rs) && !is.null(rs$res))
            tryCatch(.dfs_exposure_items(rs$pool, rs$res, "golf"), error = function(e) NULL) else NULL
  if (is.null(vals) || !length(vals)) { msg("dfs round values: no exposure items for R", rnd); return(invisible(NULL)) }
  # event name (best-effort) from the exported round projection meta
  event <- tryCatch({ d <- Sys.getenv("GOLF_MODEL_DIR", ""); if (!nzchar(d)) d <- file.path(DFS_ROOT, "..", "golf-modeling")
    readRDS(file.path(d, "golf_picks", "dfs_round_projections.rds"))$meta$event %||% "" }, error = function(e) "")
  slate_date <- as.character(Sys.Date())
  rlabel <- if (rnd == 1L) "Round 1 (First-Round Leader)" else sprintf("Round %d", rnd)
  out <- list(contract_version = VALUES_CONTRACT_VERSION, source = "dfs-engine", sport = "golf",
              slate_type = "single_round", round = rnd, round_label = rlabel, event = event,
              site = site_label, slate_date = slate_date,
              generated_at = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
              values = vals)
  dir <- file.path(dfs_feed_dir(), "dfs-engine", "golf"); dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(dir, sprintf("round_values_%s_%s.json", slate_date, site_label))
  writeLines(as.character(jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE, null = "null")), f, useBytes = TRUE)
  msg(sprintf("dfs round values: golf %s %s -> %s (%d names, exposure)", site_label, rlabel, basename(f), length(vals)))
  invisible(f)
}

write_dfs_values <- function(sports = DFS_SPORTS, sites = c("dk", "fd")) {
  invisible(lapply(sports, function(sp) {
    if ("dk" %in% sites)
      tryCatch(write_dfs_values_sport(sp, "dk"), error = function(e) msg("dfs values", sp, "dk error:", conditionMessage(e)))
    # FanDuel only when FD projections exist for this sport (avoids daily empty-FD files
    # until the FD projection pipeline lands). Once fd_scrape + FD projections run, it emits.
    if ("fd" %in% sites && .dfs_has_slate(sp, "fd"))
      tryCatch(write_dfs_values_sport(sp, "fd"), error = function(e) msg("dfs values", sp, "fd error:", conditionMessage(e)))
  }))
}

if (!nzchar(Sys.getenv("DFS_VALUES_SOURCE_ONLY"))) {
  a <- commandArgs(trailingOnly = TRUE); i <- which(a == "--sports")
  sports <- if (length(i)) strsplit(a[i + 1L], ",")[[1]] else DFS_SPORTS   # e.g. --sports tennis,wnba
  write_dfs_values(sports)
}
