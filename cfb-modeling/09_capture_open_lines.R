# ============================================================================
# 09_capture_open_lines.R  —  capture & FREEZE early-week (opening) lines
# ----------------------------------------------------------------------------
# The validated edge (`proj_vs_open`, FEATURE_REGISTRY.md 2026-09-03/04) is that
# the model beats the market's OPENING spread post-2023 (~53-54%, CLV+0.31pt),
# but is fully encompassed by the CLOSE. The number sharpens toward kickoff, so
# the edge decays with time — it must be captured EARLY and HELD, not re-read
# from the current line at bet time.
#
# This script pulls live DK/FanDuel/Caesars spreads+totals from The Odds API,
# matches each event to a CFBD game_id, and APPENDS any game not already in the
# ledger (data_cache/opening_lines.csv). Once a game_id+book is captured, it is
# FROZEN — a later run never overwrites it (mirrors the dashboard's posted-bet
# freeze logic). Designed to run early in the week (Sun night / Mon morning,
# before the number sharpens) via a dedicated scheduled task — see
# cfb_open_capture_task.xml. Live odds pull costs ~2 API credits per run
# (regions=1 x markets=2); NOT the expensive historical endpoint.
#
# Env: ODDS_API_KEY (from .Renviron), FEED_DIR unused here (writes to data_cache
# only — this is model-internal state, not a dashboard feed file).
# ============================================================================

source("00_ppp_common.R")
suppressWarnings(suppressMessages({ library(httr); library(jsonlite) }))

LEDGER <- file.path(CACHE, "opening_lines.csv")
BOOKS  <- c(draftkings = "DraftKings", fanduel = "FanDuel", williamhill_us = "Caesars")

readRenviron(".Renviron")
KEY <- Sys.getenv("ODDS_API_KEY")
if (!nzchar(KEY)) stop("ODDS_API_KEY not set (check .Renviron)")

cat(sprintf("STEP 9: CAPTURE OPENING LINES  %s\n", format(Sys.time())))

# ---- pull live odds (cheap: ~2 credits) -------------------------------------
uri <- sprintf(
  "https://api.the-odds-api.com/v4/sports/americanfootball_ncaaf/odds?apiKey=%s&regions=us&markets=spreads,totals&bookmakers=%s&oddsFormat=american",
  KEY, paste(names(BOOKS), collapse = ","))
resp <- tryCatch(GET(uri, timeout(30)), error = function(e) NULL)
if (is.null(resp) || status_code(resp) != 200) {
  cat(sprintf("  API call failed (status %s) — aborting, ledger unchanged.\n",
              if (is.null(resp)) "NA" else status_code(resp)))
  quit(save = "no", status = 0)   # non-fatal: a missed capture window isn't a pipeline failure
}
remaining <- headers(resp)[["x-requests-remaining"]]
events <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = FALSE)
cat(sprintf("  pulled %d events | credits remaining: %s\n", length(events), remaining))
if (length(events) == 0) { cat("  no events returned — nothing to capture.\n"); quit(save = "no", status = 0) }

# ---- flatten to one row per (event, book) -----------------------------------
rows <- list()
for (e in events) {
  for (bk in e$bookmakers) {
    book <- BOOKS[[bk$key]]; if (is.null(book)) next
    sp <- NA_real_; tot <- NA_real_
    for (mk in bk$markets) {
      if (mk$key == "spreads") {
        o <- Filter(function(x) x$name == e$home_team, mk$outcomes)
        if (length(o)) sp <- o[[1]]$point
      } else if (mk$key == "totals") {
        o <- Filter(function(x) x$name == "Over", mk$outcomes)
        if (length(o)) tot <- o[[1]]$point
      }
    }
    if (is.na(sp) && is.na(tot)) next
    rows[[length(rows) + 1]] <- data.frame(
      odds_home = e$home_team, odds_away = e$away_team,
      commence  = e$commence_time, book = book,
      home_spread_open = sp, total_open = tot,
      book_last_update = bk$last_update, stringsAsFactors = FALSE)
  }
}
if (length(rows) == 0) { cat("  no priced spread/total rows in response.\n"); quit(save = "no", status = 0) }
pulled <- do.call(rbind, rows)

# ---- match Odds API team names to CFBD schools (longest-prefix match) ------
gi <- read_rds_retry(file.path(CACHE, "game_info.rds"))
gi2 <- gi %>% mutate(game_id = if ("game_id" %in% names(.)) game_id else id,
                     season  = if ("season"  %in% names(.)) season  else year,
                     gdate   = as.Date(substr(start_date, 1, 10)))
schools <- sort(unique(c(gi2$home_team, gi2$away_team)))
match_school <- function(name) {
  cands <- schools[startsWith(name, schools)]
  if (length(cands) == 0) return(NA_character_)
  cands[which.max(nchar(cands))]
}
un <- unique(c(pulled$odds_home, pulled$odds_away))
lut <- setNames(vapply(un, match_school, character(1)), un)
pulled$home_team <- lut[pulled$odds_home]
pulled$away_team <- lut[pulled$odds_away]
pulled$gdate     <- as.Date(substr(pulled$commence, 1, 10))

matched <- pulled %>% filter(!is.na(home_team), !is.na(away_team)) %>%
  inner_join(gi2 %>% select(game_id, season, week, home_team, away_team, gdate),
             by = c("home_team", "away_team", "gdate"))
# late-kickoff UTC rollover: retry unmatched rows against gdate-1
unmatched <- pulled %>% filter(!is.na(home_team), !is.na(away_team)) %>%
  anti_join(matched, by = c("odds_home","odds_away","book","commence")) %>% mutate(gdate = gdate - 1)
matched2 <- unmatched %>% inner_join(gi2 %>% select(game_id, season, week, home_team, away_team, gdate),
                                     by = c("home_team","away_team","gdate"))
matched <- bind_rows(matched, matched2) %>%
  transmute(game_id = as.character(game_id), season, week, home_team, away_team, book,
            home_spread_open, total_open, book_last_update,
            captured_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
cat(sprintf("  matched to CFBD games: %d of %d pulled rows\n", nrow(matched), nrow(pulled)))

# ---- append-only FREEZE: never overwrite an already-captured (game_id, book) --
if (file.exists(LEDGER)) {
  existing <- read.csv(LEDGER, stringsAsFactors = FALSE, colClasses = c(game_id = "character"))
  new_rows <- matched %>% anti_join(existing, by = c("game_id", "book"))
} else {
  existing <- matched[0, ]
  new_rows <- matched
}
if (nrow(new_rows) > 0) {
  write.table(new_rows, LEDGER, sep = ",", row.names = FALSE,
              col.names = !file.exists(LEDGER), append = file.exists(LEDGER))
  cat(sprintf("  captured %d NEW (game, book) opening lines (frozen):\n", nrow(new_rows)))
  for (i in seq_len(nrow(new_rows))) with(new_rows[i,], cat(sprintf(
    "    wk%d %s @ %s | %s spread=%s total=%s\n", week, away_team, home_team, book,
    ifelse(is.na(home_spread_open),"NA",home_spread_open), ifelse(is.na(total_open),"NA",total_open))))
} else {
  cat("  nothing new — all pulled games already frozen in the ledger.\n")
}
cat(sprintf("✓ capture done. ledger now has %d rows total.\n", nrow(existing) + nrow(new_rows)))
