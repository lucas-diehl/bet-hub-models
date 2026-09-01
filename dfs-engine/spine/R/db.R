# ==============================================================================
# DFS ENGINE — local store (DuckDB)
# Single-file embedded DB at data/dfs.duckdb. No server. Helpers used by every
# sport: connect, bootstrap schema, idempotent upsert, read.
# ==============================================================================

suppressPackageStartupMessages({ library(DBI) })

# DB path. Defaults to data/dfs.duckdb, but set env DFS_DB_PATH to a LOCAL (non-OneDrive)
# path to eliminate OneDrive file-sync locks entirely — the cleanest fix if locks persist.
DFS_DB_PATH <- { p <- Sys.getenv("DFS_DB_PATH", ""); if (nzchar(p)) p else dfs_path("data", "dfs.duckdb") }

# Open a connection, RETRYING on lock contention. The .duckdb lives in a OneDrive folder,
# so OneDrive sync (or a concurrent R process / scheduled run) can briefly lock it; without
# a retry, persist_* calls wrapped in tryCatch() fail SILENTLY and projections go unlogged.
db_connect <- function(read_only = FALSE, retries = 10L, wait = 0.6) {
  if (!requireNamespace("duckdb", quietly = TRUE))
    stop("install.packages('duckdb')")
  dir.create(dirname(DFS_DB_PATH), recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(retries)) {
    con <- tryCatch(DBI::dbConnect(duckdb::duckdb(), dbdir = DFS_DB_PATH, read_only = read_only),
                    error = function(e) e)
    if (!inherits(con, "error")) return(con)
    locked <- grepl("another process|being used|Conflicting lock|Could not set lock|IO Error",
                    conditionMessage(con), ignore.case = TRUE)
    if (!locked || i == retries) stop(con)      # not a lock (real error) or out of retries
    Sys.sleep(wait * i)                          # linear backoff while OneDrive/other process releases
  }
}

db_disconnect <- function(con) {
  tryCatch(DBI::dbDisconnect(con, shutdown = TRUE), error = function(e) NULL)
}

# Run a function with an open connection, guaranteeing disconnect.
with_db <- function(fn, read_only = FALSE) {
  con <- db_connect(read_only = read_only)
  on.exit(db_disconnect(con))
  fn(con)
}

# Create all tables from schema.sql (idempotent). Run once at setup, safe to repeat.
db_bootstrap <- function() {
  sql <- paste(readLines(dfs_path("spine", "R", "schema.sql"), warn = FALSE), collapse = "\n")
  stmts <- Filter(function(s) nchar(trimws(s)) > 0, strsplit(sql, ";", fixed = TRUE)[[1]])
  with_db(function(con) {
    for (s in stmts) DBI::dbExecute(con, s)
    # seed the sports lookup (no baseball/softball — ever)
    for (sp in list(c("golf","Golf"), c("wnba","WNBA"), c("tennis","Tennis"),
                    c("nfl","NFL"), c("ncaaf","NCAAF"), c("nba","NBA"))) {
      DBI::dbExecute(con,
        "INSERT INTO sports VALUES (?, ?) ON CONFLICT (sport) DO NOTHING",
        params = list(sp[1], sp[2]))
    }
    invisible(DBI::dbListTables(con))
  })
}

# Idempotent upsert of a data.frame into `table`, keyed by `keys` (the PK columns).
# Uses DuckDB INSERT ... ON CONFLICT DO UPDATE. Non-key columns are overwritten.
db_upsert <- function(table, df, keys) {
  if (nrow(df) == 0) return(invisible(0L))
  df <- as.data.frame(df)
  cols <- names(df)
  stopifnot(all(keys %in% cols))
  upd_cols <- setdiff(cols, keys)
  with_db(function(con) {
    tmp <- paste0("_stg_", table, "_", as.integer(runif(1, 1, 1e9)))
    DBI::dbWriteTable(con, tmp, df, temporary = TRUE, overwrite = TRUE)
    set_clause <- if (length(upd_cols))
      paste0("DO UPDATE SET ", paste(sprintf("%s = excluded.%s", upd_cols, upd_cols), collapse = ", "))
      else "DO NOTHING"
    sql <- sprintf(
      "INSERT INTO %s (%s) SELECT %s FROM %s ON CONFLICT (%s) %s",
      table, paste(cols, collapse = ", "), paste(cols, collapse = ", "),
      tmp, paste(keys, collapse = ", "), set_clause)
    n <- DBI::dbExecute(con, sql)
    DBI::dbRemoveTable(con, tmp)
    invisible(n)
  })
}

# Convenience read: parameterized query -> data.frame.
db_query <- function(sql, params = list()) {
  with_db(function(con) DBI::dbGetQuery(con, sql, params = params), read_only = FALSE)
}

# Deterministic slate id from its natural key, so re-ingests are idempotent.
make_slate_id <- function(sport, site, date, name = "main") {
  tolower(gsub("[^A-Za-z0-9]+", "-", paste(sport, site, as.character(date), name, sep = "_")))
}

# Normalize a player name for cross-source joins (DK CSV name -> internal player).
# Lowercase, strip accents/punctuation/suffixes, collapse spaces. Handles
# "Last, First" -> "first last".
norm_name <- function(x) {
  x <- as.character(x)
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  # "last, first" -> "first last"
  swap <- grepl(",", x)
  x[swap] <- sub("^\\s*([^,]+),\\s*(.+)$", "\\2 \\1", x[swap])
  x <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", x)   # drop generational suffixes
  x <- gsub("[^a-z ]", "", x)                      # drop punctuation/apostrophes
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Stable surrogate player_id from a normalized name (for ownership logged before a
# sport is modeled / before a real id mapping exists). Negative to avoid colliding
# with real source ids. 31-bit so it fits BIGINT comfortably.
surrogate_player_id <- function(norm) {
  vapply(norm, function(s) {
    h <- sum(utf8ToInt(s) * seq_along(strsplit(s, "")[[1]])) %% .Machine$integer.max
    -as.numeric(abs(h) + 1)
  }, numeric(1))
}
