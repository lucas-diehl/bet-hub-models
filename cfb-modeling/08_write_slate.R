# ============================================================================
# 08_write_slate.R  —  full-slate model board for the Bet Hub "Extras" tab
# ----------------------------------------------------------------------------
# Writes ONE row per FBS-vs-FBS game for the target week with the model's
# numbers for EVERY game (not just the ones we bet): kickoff, teams, market
# spread/total, projected margin/total, projected score, the model's ATS &
# totals lean, the edge in points, and a confidence bucket.
#
# This is DISPLAY-ONLY (a reference board), a NEW feed type: slate_<date>.json
# (top-level `games` array). It never grades or stakes — the picks feed (07)
# remains the source of truth for actual bets. Emitted alongside picks so the
# Extras tab can show "the day's slate + a pick for each game".
#
# Reuses 07's ratings + calibration exactly, so the slate's numbers match the
# picks feed. Env: PPP_SEASON, PPP_WEEK, PPP_MODE (PAPER), FEED_DIR.
# ============================================================================

source("00_ppp_common.R")
suppressWarnings(suppressMessages(library(jsonlite)))
set.seed(42)

FEED_DIR  <- Sys.getenv("FEED_DIR", "C:/Users/ljdie/OneDrive/Documents/dashboard_feed")
TS        <- as.integer(Sys.getenv("PPP_SEASON", "2026"))
TW        <- suppressWarnings(as.integer(Sys.getenv("PPP_WEEK", "1")))
MODE      <- Sys.getenv("PPP_MODE", "PAPER")
MODEL_VER <- "ppp-ppd-2026"
# proj_vs_open ATS is the validated edge (beats the OPENING line post-2023, ~53-54%,
# CLV-confirmed). ATS_EDGE_MIN mirrors the backtest's disagreement threshold.
ATS_EDGE_MIN <- 3.0; UNDER_EDGE <- 4.0; DOG_EDGE <- 3.0
# The margin/spread projection is over-extreme AND unreliable early season (ratings are
# preseason-seeded until ~2 weeks of games de-noise them — week-1 flattens Alabama -28 to a
# pick'em). Totals are sane from week 1 (and the UNDER edge is directional, not accuracy).
# So we SUPPRESS margin/spread predictions until MARGIN_MIN_WEEK; totals always show.
MARGIN_MIN_WEEK <- as.integer(Sys.getenv("PPP_MARGIN_MIN_WEEK", "3"))
MARGIN_OK <- TW >= MARGIN_MIN_WEEK

slug    <- function(x) gsub("[^a-z0-9]", "", tolower(x))
compact <- function(l) l[!vapply(l, is.null, logical(1))]
outdir  <- file.path(FEED_DIR, "cfb-modeling", "cfb"); dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
now_iso <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

cat(sprintf("STEP 8: WRITE SLATE  season %d wk %d  mode %s\n", TS, TW, MODE))

# ---- load + build game table (same as 07) ----------------------------------
asof <- read_rds_retry(file.path(CACHE,"asof_ratings.rds"))
bl   <- read_rds_retry(file.path(CACHE,"betting_lines.rds"))
gi   <- read_rds_retry(file.path(CACHE,"game_info.rds"))
sp   <- read_rds_retry(file.path(CACHE,"sp_ratings.rds"))
fbs  <- make_fbs_mapper(sp)
gi2  <- gi %>% mutate(game_id = if ("game_id" %in% names(.)) game_id else id,
                      season  = if ("season"  %in% names(.)) season  else year)
start_map <- gi2 %>% transmute(game_id,
  start_utc = as.POSIXct(sub("\\..*$","", start_date), format="%Y-%m-%dT%H:%M:%S", tz="UTC"))
gd <- gi2 %>% transmute(game_id, season, week, home_team, away_team,
  home_score = as.numeric(home_points), away_score = as.numeric(away_points),
  total_points = home_score + away_score)

md <- attach_ratings(build_model_data(bl, gd, include_upcoming = TRUE), asof, fbs) %>%
  filter(home_team != "FCS", away_team != "FCS") %>%
  left_join(book_lines(bl), by = "game_id") %>%
  left_join(start_map, by = "game_id")

hist <- md %>% filter(!is.na(actual_margin), season < TS | (season == TS & week < TW))
if (nrow(hist) < 500) stop("Not enough completed history to calibrate.")
cal_t <- lm(total_points ~ proj_total_A, data = hist); cal_m <- lm(actual_margin ~ proj_margin_A, data = hist)

# ---- target-week slate: EVERY FBS-vs-FBS game ------------------------------
slate <- md %>% filter(season == TS, week == TW) %>%
  mutate(proj_total  = predict(cal_t, .), proj_margin = predict(cal_m, .),
         completed   = !is.na(actual_margin),
         slate_date  = format(start_utc, "%Y-%m-%d", tz="America/New_York"),
         event_start = ifelse(is.na(start_utc), NA_character_, format(start_utc,"%Y-%m-%dT%H:%M:%SZ", tz="UTC")),
         event       = paste0(away_team, " @ ", home_team),
         # market (home perspective): margin = -spread ; total as-is
         mkt_margin  = -bk_spread,
         ats_edge    = proj_margin - mkt_margin,                 # >0 model likes HOME
         ats_pick    = if_else(ats_edge > 0, home_team, away_team),
         ats_side    = if_else(ats_edge > 0, "home", "away"),
         ats_line    = if_else(ats_edge > 0, bk_spread, -bk_spread),
         ats_mag     = abs(ats_edge),
         tot_edge    = proj_total - bk_total,                    # <0 model likes UNDER
         tot_pick    = if_else(tot_edge < 0, "Under", "Over"),
         tot_side    = if_else(tot_edge < 0, "under", "over"),
         tot_mag     = abs(tot_edge),
         proj_home   = (proj_total + proj_margin)/2,
         proj_away   = (proj_total - proj_margin)/2) %>%
  filter(!is.na(proj_margin))

# a game is a "play" if it clears a deployed threshold (shown as a badge)
mkconf <- function(m) ifelse(m>=6,"high", ifelse(m>=3,"medium","low"))

# ---- read the posted picks back so the board and the bet slip CANNOT disagree ----
# 07_write_feed.R runs before 08 for the same week (weekly_update.R), so its output is
# the single source of truth for what we actually bet. Re-deriving the gate here was a
# bug on both markets: ATS badges were hard-disabled (the comment below dated from before
# Arm O went live, so 19 posted bets showed 0 plays), and the UNDER badge tested only
# `tot_edge <= -4` so it silently missed every UNDER3_DOG bet 07 emits at the 3.0
# threshold. Reading the feed means a threshold change in 07 can never desync the board.
posted <- list()
for (d in unique(slate$slate_date)) {
  pf <- file.path(outdir, sprintf("picks_%s.json", d))
  if (!file.exists(pf)) next
  pj <- tryCatch(fromJSON(pf, simplifyDataFrame = FALSE), error = function(e) NULL)
  for (b in pj$bets) posted[[paste(d, b$event, b$market, sep = "|")]] <- b
}
pget <- function(r, mkt) posted[[paste(r$slate_date, r$event, mkt, sep = "|")]]

mk_game <- function(r) { pb <- pget(r, "spread"); tb <- pget(r, "total"); compact(list(
  game_id    = as.character(r$game_id),
  event      = r$event,
  event_start= if (is.na(r$event_start)) NULL else r$event_start,
  home_team  = r$home_team, away_team = r$away_team,
  book       = if (is.na(r$book)) NULL else r$book,
  spread     = if (is.na(r$bk_spread)) NULL else round(r$bk_spread,1),   # home market spread (always shown)
  total      = if (is.na(r$bk_total))  NULL else round(r$bk_total,1),
  proj_total = round(r$proj_total,1),                                     # totals = our edge, always shown
  # margin/spread projection: only when ratings are reliable (week >= MARGIN_MIN_WEEK)
  proj_margin= if (MARGIN_OK) round(r$proj_margin,1) else NULL,           # home perspective
  proj_home_score = if (MARGIN_OK) round(r$proj_home,1) else NULL,
  proj_away_score = if (MARGIN_OK) round(r$proj_away,1) else NULL,
  # A POSTED bet always wins over the informational view and is always shown, even before
  # MARGIN_MIN_WEEK: the week gate exists to hide our noisy raw margin projection, but a bet
  # we actually placed must never be invisible on the board. Terms are the FROZEN OPENING
  # line the bet was graded against, not the current market number shown in `spread` above.
  ats_pick   = if (!is.null(pb)) pb$selection else if (MARGIN_OK && !is.na(r$bk_spread)) r$ats_pick else NULL,
  ats_line   = if (!is.null(pb)) pb$line else if (MARGIN_OK && !is.na(r$bk_spread)) round(r$ats_line,1) else NULL,
  ats_edge   = if (!is.null(pb)) pb$details$edge_pts else if (MARGIN_OK && !is.na(r$bk_spread)) round(r$ats_mag,1) else NULL,
  ats_conf   = if (!is.null(pb)) pb$confidence else if (MARGIN_OK && !is.na(r$bk_spread)) mkconf(r$ats_mag) else NULL,
  ats_play   = if (!is.null(pb)) TRUE else NULL,
  ats_book   = if (!is.null(pb)) pb$book else NULL,
  ats_stake  = if (!is.null(pb)) pb$stake_units else NULL,
  ats_consensus = if (!is.null(pb)) pb$details$dvoa_consensus else NULL,
  total_pick = if (!is.null(tb)) tb$selection else if (is.na(r$bk_total)) NULL else r$tot_pick,
  total_edge = if (is.na(r$bk_total)) NULL else round(r$tot_mag,1),
  total_conf = if (!is.null(tb)) tb$confidence else if (is.na(r$bk_total)) NULL else mkconf(r$tot_mag),
  total_play = if (!is.null(tb)) TRUE else NULL,
  total_stake = if (!is.null(tb)) tb$stake_units else NULL,
  completed  = r$completed,
  final_margin = if (isTRUE(r$completed)) r$actual_margin else NULL,
  final_total  = if (isTRUE(r$completed)) r$total_points else NULL)) }

games <- lapply(seq_len(nrow(slate)), function(i) { g <- mk_game(slate[i,]); g$.date <- slate$slate_date[i]; g })

# ---- write one slate file per game day -------------------------------------
dates <- sort(unique(vapply(games, function(g) g$.date, character(1))))
for (d in dates) {
  day <- Filter(function(g) identical(g$.date, d), games)
  # order by kickoff then event
  ord <- order(vapply(day, function(g) if (is.null(g$event_start)) "9" else g$event_start, character(1)))
  day <- day[ord]
  note <- if (MARGIN_OK) "Model total + UNDER lean is the edge; spread shown vs market."
          else sprintf("Early season: spread/margin projections warm up at week %d (ratings de-noise). Totals shown now.", MARGIN_MIN_WEEK)
  out <- list(contract_version="1.0", source="cfb-modeling", sport="cfb", slate_date=d,
    generated_at=now_iso, model_version=MODEL_VER, mode=MODE, event_context=sprintf("Week %d", TW),
    notes=note, games=lapply(day, function(g) g[!startsWith(names(g),".")]))
  write_json(out, file.path(outdir, sprintf("board_%s.json", d)), auto_unbox=TRUE, pretty=TRUE, null="null")
  cat(sprintf("  board_%s.json (%d games, %d ATS plays, %d UNDER plays)\n", d, length(day),
    sum(vapply(day, function(g) isTRUE(g$ats_play), logical(1))),
    sum(vapply(day, function(g) isTRUE(g$total_play), logical(1)))))
}
cat(sprintf("✓ slate done (%d games)\n", length(games)))
