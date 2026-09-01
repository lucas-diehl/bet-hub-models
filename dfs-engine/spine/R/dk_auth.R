# ==============================================================================
# DFS ENGINE — DraftKings authenticated standings download (actual ownership)
# Actual %Drafted only comes from a contest's standings export, which DK serves to
# logged-in entrants. You enter free/cheap contests for data access; this downloads
# their standings after settlement using YOUR session cookie, and the importer logs
# the ownership. Personal-use access to your own contests' data.
#
# SETUP (one time): log into draftkings.com in a browser, open DevTools ->
# Application -> Cookies -> copy the cookie header (or just the auth cookies) into
# config/dk_session.txt (gitignored). Or set the DK_COOKIE env var. Re-paste when it
# expires. The cookie is a secret — it is never printed or logged.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

dk_session_cookie <- function() {
  ck <- Sys.getenv("DK_COOKIE", "")
  if (!nchar(ck)) { f <- dfs_path("config", "dk_session.txt")
    if (file.exists(f)) ck <- paste(grep("^\\s*#", readLines(f, warn = FALSE), invert = TRUE, value = TRUE), collapse = "; ") }
  trimws(ck)
}
dk_authed <- function() nchar(dk_session_cookie()) > 20

.dk_auth_req <- function(url) {
  ck <- dk_session_cookie(); if (!nchar(ck)) stop("No DK session cookie configured.")
  httr2::request(url) |>
    httr2::req_headers(Cookie = ck, Accept = "*/*") |>
    httr2::req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)") |>
    httr2::req_timeout(45)
}

.looks_like_html <- function(s) grepl("^\\s*<", substr(s, 1, 50))

# Download a settled contest's standings CSV (with %Drafted) -> inbox. Returns the
# path on success, NULL if not authed / not entered / not settled (HTML returned).
dk_download_standings <- function(contest_id, dest = NULL) {
  url <- sprintf("https://www.draftkings.com/contest/exportfullstandingscsv/%s", contest_id)
  resp <- tryCatch(httr2::req_perform(.dk_auth_req(url)), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) != 200) return(NULL)
  raw <- tryCatch(httr2::resp_body_raw(resp), error = function(e) NULL)
  if (is.null(raw) || !length(raw)) return(NULL)
  # DK returns the standings either as a raw CSV or, more often now, a ZIP archive
  # (magic bytes "PK"). Unzip and read the CSV inside when it's zipped.
  if (length(raw) >= 2 && raw[1] == as.raw(0x50) && raw[2] == as.raw(0x4b)) {
    tz <- tempfile(fileext = ".zip"); on.exit(unlink(tz), add = TRUE); writeBin(raw, tz)
    inside <- tryCatch(unzip(tz, list = TRUE)$Name, error = function(e) character(0))
    csvn <- grep("\\.csv$", inside, value = TRUE, ignore.case = TRUE)[1]
    if (is.na(csvn)) return(NULL)
    con <- unz(tz, csvn); body <- paste(readLines(con, warn = FALSE), collapse = "\n"); close(con)
  } else body <- rawToChar(raw)
  # empty body / login wall / no ownership column => not entered or not settled yet
  if (!nchar(body) || .looks_like_html(body) || !grepl("Drafted", body)) return(NULL)
  if (is.null(dest)) { d <- dfs_path("data", "ownership_inbox"); dir.create(d, recursive = TRUE, showWarnings = FALSE)
    dest <- file.path(d, paste0("contest-standings-", contest_id, ".csv")) }
  writeLines(body, dest); dest
}

# Best-effort discovery of YOUR entered contest ids from the authenticated
# my-contests page (so you don't list them by hand). VERIFY parsing with your
# session; if it returns nothing, use config/contest_watchlist.txt as a fallback.
dk_my_contests <- function() {
  resp <- tryCatch(httr2::req_perform(.dk_auth_req("https://www.draftkings.com/mycontests")),
                   error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) != 200) return(character(0))
  html <- httr2::resp_body_string(resp)
  pats <- c('"ContestId"\\s*:\\s*(\\d+)', '"contestId"\\s*:\\s*(\\d+)', 'contest/gamecenter/(\\d+)')
  ids <- unlist(lapply(pats, function(p) regmatches(html, gregexpr(p, html))[[1]]))
  ids <- unique(gsub("\\D", "", ids)); ids[nchar(ids) >= 6]
}

# SETTLED contests from the authenticated history feed (JSON). This is the key source
# for actual ownership: the live /mycontests page only embeds upcoming/live contests,
# but standings (with %Drafted) only exist AFTER settlement. The history feed lists the
# settled ones, so syncing from here reliably captures every contest you entered.
dk_settled_contests <- function(max_ids = 60L) {
  resp <- tryCatch(httr2::req_perform(.dk_auth_req("https://www.draftkings.com/mycontests/history")),
                   error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) != 200) return(character(0))
  body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  ids <- regmatches(body, gregexpr('"ContestId"\\s*:\\s*\\d+', body, ignore.case = TRUE))[[1]]
  ids <- unique(gsub("\\D", "", ids)); ids <- ids[nchar(ids) >= 6]
  head(ids, max_ids)
}

# contest ids the user always wants tracked (URLs or ids, one per line; optional).
dk_watchlist <- function() {
  f <- dfs_path("config", "contest_watchlist.txt"); if (!file.exists(f)) return(character(0))
  ln <- grep("^\\s*#", readLines(f, warn = FALSE), invert = TRUE, value = TRUE)
  ids <- vapply(ln, dk_parse_contest_id, character(1)); unique(ids[nchar(ids) >= 6])
}

# Sync actual ownership: download standings for entered+settled contests not yet
# logged, drop them in the inbox (the importer logs them). Skips silently if no
# session is configured, so the daily run still works.
sync_ownership <- function(extra_ids = NULL) {
  if (!dk_authed()) { msg("DK session not configured -> skipping actual-ownership sync (set config/dk_session.txt)."); return(invisible(0L)) }
  # settled contests (history feed) are the ones with downloadable standings; live +
  # watchlist are added so a just-entered contest is retried until it settles.
  ids <- unique(c(dk_settled_contests(), dk_my_contests(), dk_watchlist(), extra_ids))
  if (!length(ids)) { msg("No entered/tracked contests found to sync."); return(invisible(0L)) }
  already <- tryCatch(unique(db_query("SELECT contest_id FROM ownership WHERE actual_pct IS NOT NULL")$contest_id),
                      error = function(e) character(0))
  ids <- setdiff(ids, as.character(already))
  n <- 0L
  for (id in ids) { f <- dk_download_standings(id)
    if (!is.null(f)) { msg("  synced standings for contest", id); n <- n + 1L } }
  msg("ownership sync: downloaded", n, "new standings")
  invisible(n)
}
