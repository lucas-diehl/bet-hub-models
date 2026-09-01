# ==============================================================================
# DFS ENGINE — results / P&L ledger (the MEASUREMENT layer)
# Turns downloaded DK contest standings into a tracked win/loss + profit/loss record
# for the user's OWN entries. Without this every gate is blind — you can't tell edge
# from variance. Flow:
#   1. find YOUR entries in a standings CSV by display name (config/dk_user.txt),
#      handling multi-entry names like "lukediehl (3/20)".
#   2. read each entry's finishing rank + fantasy points from the standings.
#   3. fetch the contest's real entry fee + prize-by-rank table (dk_contest).
#   4. write user_entries + entry_results (pnl = prize - entry_fee), idempotently.
# Backfill everything already downloaded with ingest_all_results(); it also runs daily.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# the user's DK display name(s) — one per line in config/dk_user.txt (aliases allowed).
dk_usernames <- function() {
  f <- dfs_path("config", "dk_user.txt")
  if (!file.exists(f)) return(character(0))
  u <- trimws(grep("^\\s*#", readLines(f, warn = FALSE), invert = TRUE, value = TRUE))
  tolower(u[nzchar(u)])
}

# does an entry name belong to the user? matches "name" or "name (k/n)" (multi-entry).
.is_user_entry <- function(entry_name, users) {
  e <- tolower(trimws(gsub('"', "", entry_name)))
  base <- sub("\\s*\\(\\d+/\\d+\\)\\s*$", "", e)      # strip the "(k/n)" multi-entry suffix
  base %in% users
}

# parse ALL entrant rows from a DK standings CSV: rank, entry_id (unique DK id),
# entry_name, points (the entrant's lineup total), lineup string. Player-block columns
# are ignored. Golfer/player names carry no commas in the lineup field, so a plain
# comma split is safe for the left (entrant) block; rows that don't start with
# rank,EntryId are skipped (guards against a rare comma inside an entry name).
parse_standings_entrants <- function(csv_path) {
  L <- readLines(csv_path, warn = FALSE); if (length(L) < 2) return(NULL)
  L <- L[-1]
  out <- lapply(L, function(x) {
    p <- strsplit(x, ",", fixed = TRUE)[[1]]
    if (length(p) < 6) return(NULL)
    rank <- suppressWarnings(as.integer(gsub("[^0-9]", "", p[1])))     # "T5" -> 5
    if (is.na(rank) || !grepl("^\\s*\\d{6,}\\s*$", p[2])) return(NULL)  # need rank + EntryId
    list(rank = rank, entry_id = trimws(p[2]), entry_name = trimws(p[3]),
         points = suppressWarnings(as.numeric(p[5])), lineup = trimws(p[6]))
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) return(NULL)
  rbindlist(out)
}

# prize a finishing rank won, from a DK payout table (min_rank, max_rank, prize).
# (Ties are approximated by the band prize for that rank — close enough for tracking.)
.prize_for_rank <- function(prizes, rank) {
  if (is.null(prizes) || !nrow(prizes) || is.na(rank)) return(NA_real_)
  hit <- prizes[min_rank <= rank & rank <= max_rank]
  if (nrow(hit)) hit$prize[1] else 0
}

# cash vs gpp from the contest name + payout shape (flat payouts = cash, top-heavy = gpp).
.contest_type <- function(name, prizes) {
  nm <- tolower(name %||% "")
  if (grepl("double.?up|50-?50|50/50|fifty|\\bcash\\b|head.?to.?head|\\bh2h\\b", nm)) return("cash")
  pos <- if (!is.null(prizes)) prizes$prize[prizes$prize > 0] else numeric(0)
  if (length(pos) >= 2 && max(pos) / min(pos) < 2) return("cash")
  "gpp"
}

# normalize DK's sport field + contest name to our sport keys (WNBA files under "NBA").
.norm_sport <- function(sport, name) {
  s <- tolower(sport %||% ""); nm <- tolower(name %||% "")
  if (grepl("wnba", nm)) return("wnba")
  if (grepl("\\bpga\\b|\\bgolf\\b", nm) || s == "golf") return("golf")
  if (grepl("\\bten\\b|tennis", nm) || s == "ten") return("tennis")
  if (grepl("soccer|\\bepl\\b|\\bucl\\b|premier", nm) || s == "soc") return("soccer")
  if (grepl("nascar", nm)) return("nascar")
  if (grepl("\\bnfl\\b", nm) || s == "nfl") return("nfl")
  if (grepl("\\bnba\\b", nm) || s == "nba") return("nba")
  if (nzchar(s)) s else "other"
}

# contest meta: prefer the live DK detail (has the prize table); fall back to a cached
# contest_results row (entry fee only -> pnl unknown).
.contest_meta <- function(contest_id) {
  cc <- tryCatch(dk_contest(contest_id), error = function(e) NULL)
  if (!is.null(cc)) return(list(
    name = cc$name, sport = .norm_sport(cc$sport, cc$name), entry_fee = cc$entry_fee,
    field_size = cc$field_size %||% cc$entries, total_payouts = cc$total_payouts,
    max_entries = cc$max_entries_per_user, prizes = cc$prizes,
    contest_type = .contest_type(cc$name, cc$prizes),
    date = if (!is.null(cc$date) && !is.na(cc$date)) cc$date else Sys.Date()))
  cr <- tryCatch(db_query(sprintf("SELECT * FROM contest_results WHERE contest_id='%s'", contest_id)),
                 error = function(e) NULL)
  if (!is.null(cr) && nrow(cr)) return(list(
    name = cr$name[1], sport = .norm_sport(cr$sport[1], cr$name[1]), entry_fee = cr$entry_fee[1],
    field_size = cr$field_size[1], total_payouts = cr$total_prizes[1], max_entries = cr$max_entries[1],
    prizes = NULL, contest_type = cr$contest_type[1] %||% "gpp", date = Sys.Date()))
  NULL
}

# ingest ONE contest's standings into user_entries + entry_results (idempotent).
ingest_contest_results <- function(contest_id, csv_path, users = dk_usernames(), force = FALSE) {
  ent <- parse_standings_entrants(csv_path); if (is.null(ent)) return(0L)
  mine <- ent[.is_user_entry(entry_name, users)]
  if (!nrow(mine)) return(0L)
  if (!force) {
    done <- tryCatch(as.character(db_query(sprintf(
      "SELECT entry_id FROM entry_results WHERE contest_id='%s'", contest_id))$entry_id),
      error = function(e) character(0))
    if (length(done) && all(mine$entry_id %in% done)) return(0L)
  }
  meta <- .contest_meta(contest_id); if (is.null(meta)) return(0L)
  fee <- as.numeric(meta$entry_fee %||% NA); fs <- as.integer(meta$field_size %||% NA)
  ts  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  db_upsert("contest_results", data.frame(
    contest_id = contest_id, slate_id = NA_character_, sport = meta$sport, site = "dk",
    name = meta$name %||% NA_character_, contest_type = meta$contest_type %||% "gpp",
    entry_fee = fee, field_size = fs, max_entries = as.integer(meta$max_entries %||% NA),
    total_prizes = as.numeric(meta$total_payouts %||% NA), payout_curve_id = NA_character_,
    cash_line = NA_real_, stringsAsFactors = FALSE), keys = "contest_id")

  db_upsert("user_entries", data.frame(
    entry_id = mine$entry_id, contest_id = contest_id, slate_id = NA_character_, sport = meta$sport,
    entry_name = mine$entry_name, entry_fee = fee, lineup = mine$lineup, source = "auto",
    entered_date = as.Date(meta$date), status = "settled", captured_ts = ts,
    stringsAsFactors = FALSE), keys = "entry_id")

  prize <- vapply(mine$rank, function(r) .prize_for_rank(meta$prizes, r), numeric(1))
  db_upsert("entry_results", data.frame(
    entry_id = mine$entry_id, contest_id = contest_id, slate_id = NA_character_, sport = meta$sport,
    rank = as.integer(mine$rank), field_size = fs, points = mine$points, prize = prize,
    entry_fee = fee, pnl = prize - fee, cashed = !is.na(prize) & prize > 0, settled_ts = ts,
    stringsAsFactors = FALSE), keys = "entry_id")
  nrow(mine)
}

# backfill / daily: ingest every downloaded standings file (idempotent; only fetches
# contest detail for contests not already logged).
ingest_all_results <- function(force = FALSE, sleep = 0.2) {
  users <- dk_usernames()
  if (!length(users)) { msg("results: no DK username in config/dk_user.txt -> skipping P&L ingest."); return(invisible(0L)) }
  dirs <- c(dfs_path("data", "ownership_inbox", "processed"), dfs_path("data", "ownership_inbox"))
  files <- unique(unlist(lapply(dirs, function(d)
    if (dir.exists(d)) list.files(d, pattern = "contest-standings-\\d+\\.csv$", full.names = TRUE) else character(0))))
  n <- 0L; nc <- 0L
  for (f in files) {
    cid <- gsub("\\D", "", basename(f))
    k <- tryCatch(ingest_contest_results(cid, f, users, force = force),
                  error = function(e) { msg("  results err", cid, ":", conditionMessage(e)); 0L })
    if (k > 0) { n <- n + k; nc <- nc + 1L; if (sleep > 0) Sys.sleep(sleep) }
  }
  msg(sprintf("results: logged %d entries across %d newly-settled contests", n, nc))
  invisible(n)
}

# the ledger: overall + by-sport + by-contest-type ROI, cash rate, biggest hits/misses.
pnl_summary <- function(print = TRUE) {
  r <- tryCatch(as.data.table(db_query("SELECT * FROM entry_results")), error = function(e) NULL)
  if (is.null(r) || !nrow(r)) { if (print) msg("No settled entries logged yet. Run ingest_all_results()."); return(invisible(NULL)) }
  agg <- function(d) d[, .(entries = .N, buyins = round(sum(entry_fee, na.rm = TRUE), 2),
    winnings = round(sum(prize, na.rm = TRUE), 2), net = round(sum(pnl, na.rm = TRUE), 2),
    roi_pct = round(100 * sum(pnl, na.rm = TRUE) / max(sum(entry_fee, na.rm = TRUE), 1e-9), 1),
    cash_pct = round(100 * mean(cashed, na.rm = TRUE), 1))]
  overall <- agg(r)
  by_sport <- r[, .(entries = .N, buyins = round(sum(entry_fee, na.rm = TRUE), 2),
    net = round(sum(pnl, na.rm = TRUE), 2), roi_pct = round(100 * sum(pnl, na.rm = TRUE) / max(sum(entry_fee, na.rm = TRUE), 1e-9), 1),
    cash_pct = round(100 * mean(cashed, na.rm = TRUE), 1)), by = sport][order(net)]
  ct <- tryCatch(as.data.table(db_query("SELECT contest_id, contest_type, name FROM contest_results")), error = function(e) NULL)
  if (!is.null(ct)) r <- merge(r, ct, by = "contest_id", all.x = TRUE)
  if (print) {
    cat("\n============ P&L LEDGER (", nrow(r), "settled entries ) ============\n")
    print(overall)
    cat("\n-- by sport --\n"); print(by_sport)
    if (!is.null(ct)) { cat("\n-- by contest type --\n")
      print(r[, .(entries = .N, net = round(sum(pnl, na.rm = TRUE), 2),
        roi_pct = round(100 * sum(pnl, na.rm = TRUE) / max(sum(entry_fee, na.rm = TRUE), 1e-9), 1)), by = contest_type][order(net)]) }
    cat("\n-- 5 biggest cashes --\n")
    print(head(r[order(-pnl), .(sport, contest_id, rank, points, prize, fee = entry_fee, pnl)], 5))
    cat("\n-- 5 biggest losses --\n")
    print(head(r[order(pnl), .(sport, contest_id, rank, points, prize, fee = entry_fee, pnl)], 5))
  }
  invisible(list(overall = overall, by_sport = by_sport, entries = r))
}
