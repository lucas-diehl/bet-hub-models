# ==============================================================================
# DFS ENGINE — slate catalog (EVERY DK slate per day, not just "main")
# dk_find_slates() already returns all draft groups for a sport; this records each
# one in slate_catalog and scrapes its player pool (cheap — API only, no sim), so
# the web app can browse every slate and build lineups on-demand when one is opened.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# Mark a cataloged slate as built (UPDATE only — never inserts a partial row).
mark_slate_built <- function(slate_id) {
  tryCatch(with_db(function(con) DBI::dbExecute(con,
    "UPDATE slate_catalog SET lineups_built = TRUE, updated_ts = ? WHERE slate_id = ?",
    params = list(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), as.character(slate_id)))),
    error = function(e) invisible(0L))
}

# Catalog (and optionally scrape the pools of) every slate for a sport today.
# The dk_main_slate draft group is named "main" (the daily job's default); all others
# are named "dg<draftGroupId>" so each slate_id is stable and unique. Returns the
# catalog data.table. lineups_built is intentionally NOT written here so an earlier
# build's flag is preserved across re-catalogs.
catalog_slates <- function(sport, date = Sys.Date(), scrape_pools = TRUE) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Golf is a single DataGolf "slate" (no DK draft groups); record one main row.
  if (sport == "golf") {
    row <- data.frame(slate_id = make_slate_id("golf", "dk", date, "main"), sport = "golf",
      slate_date = as.character(date), draft_group_id = NA_character_, name = "main",
      game_count = NA_integer_, is_showdown = FALSE, top_contest_id = NA_character_,
      pool_scraped = TRUE, updated_ts = ts)
    db_upsert("slate_catalog", row, keys = "slate_id")
    return(invisible(as.data.table(row)))
  }

  s <- tryCatch(dk_find_slates(sport), error = function(e) NULL)
  if (is.null(s) || !nrow(s)) { msg("  no", sport, "slates posted yet"); return(invisible(NULL)) }
  main_dg <- tryCatch(dk_main_slate(sport)$draft_group_id, error = function(e) NA)

  rows <- rbindlist(lapply(seq_len(nrow(s)), function(i) {
    dg <- s$dg[i]
    nm <- if (!is.na(main_dg) && dg == main_dg) "main" else paste0("dg", dg)
    is_sd <- isTRUE(s$GameCount[i] == 1L) || grepl("showdown|captain", s$top_name[i] %||% "", ignore.case = TRUE)
    sid <- make_slate_id(sport, "dk", date, nm)
    scraped <- FALSE
    if (scrape_pools) scraped <- tryCatch({
      p <- dk_salary_path(sport, date, nm)
      if (!dk_salary_csv_ready(p, date))
        scrape_dk_salaries(sport, date = date, slate = nm, draft_group_id = dg)
      dk_salary_csv_rows(p) >= 1L
    }, error = function(e) { msg("    scrape", nm, "failed:", conditionMessage(e)); FALSE })
    data.table(slate_id = sid, sport = sport, slate_date = as.character(date),
      draft_group_id = as.character(dg), name = nm,
      game_count = suppressWarnings(as.integer(s$GameCount[i])), is_showdown = is_sd,
      top_contest_id = as.character(s$top_contest_id[i]), pool_scraped = scraped, updated_ts = ts)
  }), fill = TRUE)

  db_upsert("slate_catalog", as.data.frame(rows), keys = "slate_id")
  msg(sprintf("  cataloged %d %s slate(s); %d pool(s) scraped", nrow(rows), sport, sum(rows$pool_scraped)))
  invisible(rows)
}

# Catalog all sports for a date (used by the daily run + the app's refresh).
catalog_all_slates <- function(sports = c("golf", "wnba", "tennis"), date = Sys.Date(), scrape_pools = TRUE) {
  for (sp in sports) { msg("catalog:", sp); tryCatch(catalog_slates(sp, date, scrape_pools),
    error = function(e) msg("  catalog", sp, "error:", conditionMessage(e))) }
  invisible(TRUE)
}
