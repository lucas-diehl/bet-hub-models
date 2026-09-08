# ============================================================================
# 07_write_feed.R  —  write ATS + Totals picks/results to the Bet Hub feed
# ----------------------------------------------------------------------------
# Contract 1.0 (dashboard_feed/_schema/{picks,results}.schema.json). Two markets,
# PAPER until forward-validated:
#   total  -> UNDER-only asymmetry
#   spread -> early-season 4th-down aggressiveness ATS
#
# Lines come from ONE book we actually bet — DraftKings (FanDuel auto-used if it
# ever appears in the data); games neither book prices are skipped. `book` is set
# to the sourced book on every bet.
#
# Post/grade split (so we grade the number we actually bet + capture CLV):
#   * picks  are (re)written only for games that have NOT started yet
#   * results are graded from the PREVIOUSLY-POSTED picks file (source of truth),
#     with closing_odds_american + clv_pct (posted line vs the book's closing line)
#   * no backfill: a completed game is graded only if it was previously posted
#
# SIMULATE=1 : historical test mode — treats the target week's completed games as
#   "posted at the opening line", writes picks + graded results in one pass.
#
# Env: PPP_SEASON, PPP_WEEK, PPP_MODE (PAPER), FEED_DIR, SIMULATE
# ============================================================================

source("00_ppp_common.R")
suppressWarnings(suppressMessages(library(jsonlite)))
set.seed(42)

FEED_DIR  <- Sys.getenv("FEED_DIR", "C:/Users/ljdie/OneDrive/Documents/dashboard_feed")
TS        <- as.integer(Sys.getenv("PPP_SEASON", "2025"))
TW        <- suppressWarnings(as.integer(Sys.getenv("PPP_WEEK", "3")))
MODE      <- Sys.getenv("PPP_MODE", "PAPER")
SIMULATE  <- nzchar(Sys.getenv("SIMULATE"))
MODEL_VER <- "ppp-ppd-2026"
ODDS      <- -110L
MKT_PROB  <- round(110/210, 4)
UNDER_EDGE <- 4.0; DOG_EDGE <- 3.0; AGG_GAP <- 0.10; N4_MIN <- 20; EARLY_WEEKS <- 1:4
ATS_OPEN_EDGE_MIN <- as.numeric(Sys.getenv("PPP_ATS_OPEN_EDGE_MIN", "3"))

slug     <- function(x) gsub("[^a-z0-9]", "", tolower(x))
compact  <- function(l) l[!vapply(l, is.null, logical(1))]
amer_ev  <- function(p) round(p * PAYOUT - (1 - p), 4)
outdir   <- file.path(FEED_DIR, "cfb-modeling", "cfb"); dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
now_iso  <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

cat(sprintf("STEP 7B: WRITE FEED  season %d wk %d  mode %s%s\n", TS, TW, MODE, if (SIMULATE) " [SIMULATE]" else ""))

# ---- load + build game table ------------------------------------------------
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
  left_join(book_lines(bl), by = "game_id") %>%              # DK/FD lines
  left_join(start_map, by = "game_id")

hist <- md %>% filter(!is.na(actual_margin), season < TS | (season == TS & week < TW))
if (nrow(hist) < 500) stop("Not enough completed history to calibrate.")
cal_t <- lm(total_points ~ proj_total_A, data = hist); cal_m <- lm(actual_margin ~ proj_margin_A, data = hist)
sig_t <- sd(hist$total_points  - predict(cal_t, hist), na.rm = TRUE)
sig_m <- sd(hist$actual_margin - predict(cal_m, hist), na.rm = TRUE)
agg   <- build_asof_aggressiveness()   # game_id-keyed (completed games)
agg_t <- latest_aggressiveness(TS)     # team-keyed latest -> fires early-ATS pre-game

# ---- target-week slate (book-priced games only) ----------------------------
# join both: game_id value for completed games, team-latest for UPCOMING games.
slate <- md %>% filter(season == TS, week == TW, !is.na(book)) %>%
  left_join(agg   %>% rename(h4g=asof_4d_go, h_n4g=n4), by=c("season","game_id","home_team"="team")) %>%
  left_join(agg   %>% rename(a4g=asof_4d_go, a_n4g=n4), by=c("season","game_id","away_team"="team")) %>%
  left_join(agg_t %>% rename(h4t=asof_4d_go, h_n4t=n4), by=c("season","home_team"="team")) %>%
  left_join(agg_t %>% rename(a4t=asof_4d_go, a_n4t=n4), by=c("season","away_team"="team")) %>%
  mutate(h4 = coalesce(h4g, h4t), h_n4 = coalesce(h_n4g, h_n4t),
         a4 = coalesce(a4g, a4t), a_n4 = coalesce(a_n4g, a_n4t),
         proj_total = predict(cal_t, .), proj_margin = predict(cal_m, .),
         completed  = !is.na(actual_margin),
         slate_date = format(start_utc, "%Y-%m-%d", tz="America/New_York"),
         event_start= ifelse(is.na(start_utc), NA_character_, format(start_utc,"%Y-%m-%dT%H:%M:%SZ", tz="UTC")),
         event      = paste0(away_team, " @ ", home_team),
         gkey       = paste0(slug(away_team), "|", slug(home_team)),
         # decision line = what we bet against (open in SIMULATE, else current book line)
         dec_total  = if (SIMULATE) coalesce(bk_total_open,  bk_total)  else bk_total,
         dec_spread = if (SIMULATE) coalesce(bk_spread_open, bk_spread) else bk_spread)

if (nrow(slate) == 0) {
  sched <- gi2 %>% filter(season==TS, week==TW)
  d <- if (nrow(sched)>0) { st <- as.POSIXct(sub("\\..*$","",sched$start_date),format="%Y-%m-%dT%H:%M:%S",tz="UTC")
    format(max(st,na.rm=TRUE),"%Y-%m-%d",tz="America/New_York") } else format(Sys.Date(),"%Y-%m-%d")
  write_json(list(contract_version="1.0", source="cfb-modeling", sport="cfb", slate_date=d,
    generated_at=now_iso, model_version=MODEL_VER, mode=MODE, event_context=sprintf("Week %d",TW),
    notes="No DraftKings/FanDuel lines yet — no plays.", bets=list()),
    file.path(outdir, sprintf("picks_%s.json", d)), auto_unbox=TRUE, pretty=TRUE, null="null")
  cat(sprintf("  no book-priced games for %d wk %d -> empty picks_%s.json\n", TS, TW, d)); quit(save="no", status=0)
}

# ---- CLV helpers (posted line vs closing line -> probability gained) --------
clv_total <- function(posted, closing) if (is.na(posted)||is.na(closing)) NA else round(pnorm((posted-closing)/sig_t)-0.5, 4)
clv_ats   <- function(posted, closing) if (is.na(posted)||is.na(closing)) NA else round(pnorm((posted-closing)/sig_m)-0.5, 4)

# game lookup for grading (by away|home slug): outcome + book CLOSING line
glu <- slate %>% transmute(gkey, completed, total_points, actual_margin,
                           close_total = bk_total, close_home_spread = bk_spread) %>%
  distinct(gkey, .keep_all = TRUE)
glu_l <- split(glu, glu$gkey)

# ---- build qualifying candidate bets (used for posting) --------------------
tot <- slate %>%
  mutate(under_edge = dec_total - proj_total, dog_ats = (!is.na(dec_spread) & dec_spread>0 & (proj_margin+dec_spread)>0) |
                                                        (!is.na(dec_spread) & dec_spread<0 & (proj_margin+dec_spread)<0),
         p_under = 1 - prob_over(proj_total, dec_total, sig_t),
         b4 = under_edge>=UNDER_EDGE, bd = under_edge>=DOG_EDGE & dog_ats,
         take = !is.na(dec_total) & (b4|bd) & (p_under*PAYOUT-(1-p_under) > 0),
         strategy = if_else(b4&bd,"UNDER4+DOG", if_else(b4,"UNDER4","UNDER3_DOG")),
         conf = if_else(under_edge>=6,"high", if_else(under_edge>=4,"medium","low"))) %>%
  filter(take)
ats <- slate %>% filter(week %in% EARLY_WEEKS, !is.na(h4), !is.na(a4), h_n4>=N4_MIN, a_n4>=N4_MIN,
                        abs(h4-a4)>=AGG_GAP, !is.na(dec_spread)) %>%
  mutate(pick_home=(h4-a4)>0, pick_team=if_else(pick_home,home_team,away_team),
         pick_side=if_else(pick_home,"home","away"), pick_line=if_else(pick_home,dec_spread,-dec_spread),
         gap=abs(h4-a4), tier=if_else(gap>=0.20,"STRONG","std"),
         mp=if_else(gap>=0.20,0.593,0.546),   # beta-binomial shrunk toward 0.524 (was raw 0.60/0.55)
         stake=if_else(gap>=0.20,1.0,0.5), is_dog=pick_line>0)

# ---- proj_vs_open ATS: bet the model vs the FROZEN OPENING line (09_capture_open_lines.R) --
# Validated 2026-09: the market fully encompasses proj_margin vs the CLOSE (p=0.76) but NOT vs
# the OPEN post-2023 (coef 0.138, p=0.004, clean Odds API data; CLV-confirmed +0.31pt @mag>=3;
# mechanism = recency lag, edge builds through the season). Distinct edge from the retired
# early-season 4th-down `ats` arm above — different mechanism, own gate, own bet_id suffix.
# Requires the ledger `data_cache/opening_lines.csv` (09_capture_open_lines.R, run early-week
# BEFORE the number sharpens toward the model). No ledger row for a game = no bet (never falls
# back to the current/live spread — that would silently re-introduce the close, which is dead).
ledger_path <- file.path(CACHE, "opening_lines.csv")
if (file.exists(ledger_path)) {
  ledger <- read.csv(ledger_path, stringsAsFactors = FALSE, colClasses = c(game_id = "character")) %>%
    mutate(pref = case_when(book=="DraftKings"~1L, book=="FanDuel"~2L, TRUE~NA_integer_)) %>%
    filter(!is.na(pref), !is.na(home_spread_open)) %>%
    arrange(pref) %>% distinct(game_id, .keep_all = TRUE) %>%    # DK preferred; Caesars captured but not bettable (book-list convention)
    transmute(game_id, open_book = book, open_spread = home_spread_open, open_total = total_open, captured_at)
  ats_open <- slate %>% mutate(game_id = as.character(game_id)) %>% inner_join(ledger, by = "game_id") %>%
    mutate(open_margin = -open_spread, ats_open_edge = proj_margin - open_margin, mag = abs(ats_open_edge),
           pick_home = ats_open_edge > 0, pick_team = if_else(pick_home, home_team, away_team),
           pick_side = if_else(pick_home, "home", "away"), pick_line = if_else(pick_home, open_spread, -open_spread),
           is_dog = pick_line > 0,
           # deliberately modest & shrunk toward breakeven (0.524) — backtest was 53.4%/53.6% at these
           # thresholds, not the 57%+ that got the OLD ats arm removed for overclaiming; see registry.
           mp = if_else(mag>=6, 0.535, 0.525), stake = 0.5) %>%
    filter(mag >= ATS_OPEN_EDGE_MIN)
} else { ats_open <- slate[0, ] %>% mutate(open_book=character(), open_spread=numeric(), open_total=numeric(),
    captured_at=character(), open_margin=numeric(), ats_open_edge=numeric(), mag=numeric(), pick_home=logical(),
    pick_team=character(), pick_side=character(), pick_line=numeric(), is_dog=logical(), mp=numeric(), stake=numeric()) }

mk_total_obj <- function(r) { tags <- c("UNDER", r$strategy); if (isTRUE(r$dog_ats)) tags <- c(tags,"dog")
  compact(list(bet_id=sprintf("cfb-modeling-%d-wk%d-%s-%s-under", TS, TW, slug(r$away_team), slug(r$home_team)),
    event=r$event, event_start=if(is.na(r$event_start)) NULL else r$event_start, market="total",
    selection=sprintf("Under %s", round(r$dec_total,1)), side="under", line=round(r$dec_total,1),
    odds_american=ODDS, book=r$book, model_prob=round(r$p_under,4), market_prob=MKT_PROB,
    edge=round(r$p_under-MKT_PROB,4), ev_pct=amer_ev(r$p_under), stake_units=1.0, confidence=r$conf,
    tags=as.list(tags), details=list(proj_total=round(r$proj_total,1), under_edge=round(r$under_edge,1), strategy=r$strategy,
      status="paper", prob_source="modelled",                       # NOTE: model_prob is over-confident in the tail (see outputs/totals_calibration.csv)
      posted_total=round(r$dec_total,1),
      min_total=round(r$proj_total + (if (grepl("UNDER4", r$strategy)) UNDER_EDGE else DOG_EDGE), 1)))) }  # self-expire if book_total drops below this
mk_ats_obj <- function(r) { tags <- c("early_ats", if(isTRUE(r$is_dog))"dog" else "fav", if(r$tier=="STRONG")"strong")
  compact(list(bet_id=sprintf("cfb-modeling-%d-wk%d-%s-%s-ats", TS, TW, slug(r$away_team), slug(r$home_team)),
    event=r$event, event_start=if(is.na(r$event_start)) NULL else r$event_start, market="spread",
    selection=r$pick_team, side=r$pick_side, line=round(r$pick_line,1), odds_american=ODDS, book=r$book,
    model_prob=r$mp, market_prob=MKT_PROB, edge=round(r$mp-MKT_PROB,4), ev_pct=amer_ev(r$mp),
    stake_units=r$stake, confidence=if(r$tier=="STRONG")"medium" else "low", tier=r$tier, tags=as.list(tags),
    details=list(go_rate_pick=round(if(r$pick_home)r$h4 else r$a4,3), go_rate_opp=round(if(r$pick_home)r$a4 else r$h4,3),
                 gap=round(r$gap,3), week=TW,
                 status="paper", prob_source="empirical_insample",   # 0.593/0.546 are shrunk in-sample tier rates, NOT a calibrated model output
                 posted_spread=round(r$pick_line,1), min_line=round(r$pick_line - 2, 1)))) }
mk_ats_open_obj <- function(r) { tags <- c("OPEN_ATS", if(isTRUE(r$is_dog))"dog" else "fav")
  compact(list(bet_id=sprintf("cfb-modeling-%d-wk%d-%s-%s-atsopen", TS, TW, slug(r$away_team), slug(r$home_team)),
    event=r$event, event_start=if(is.na(r$event_start)) NULL else r$event_start, market="spread",
    selection=r$pick_team, side=r$pick_side, line=round(r$pick_line,1), odds_american=ODDS, book=r$open_book,
    model_prob=r$mp, market_prob=MKT_PROB, edge=round(r$mp-MKT_PROB,4), ev_pct=amer_ev(r$mp),
    stake_units=r$stake, confidence=if(r$mag>=6)"medium" else "low", tags=as.list(tags),
    details=list(proj_margin=round(r$proj_margin,1), open_spread=round(r$open_spread,1), edge_pts=round(r$mag,1),
                 opened=r$captured_at, week=TW,
                 status="paper", prob_source="empirical_insample",   # 0.525/0.535 shrunk near breakeven; see FEATURE_REGISTRY proj_vs_open
                 mechanism="beats the OPENING line only (encompassed by the true close); regime-dependent (post-2023), monitor decay",
                 posted_spread=round(r$pick_line,1), min_line=round(r$pick_line - 2, 1)))) }  # don't chase >2pts worse than posted

# ATS is REMOVED from the live feed (2026-09): the encompassing test showed the market
# fully encompasses our margin projection AND its components (all p>0.35) — ATS is closed,
# and the early-4th-down niche is unconfirmed (flat CLV, fails multiplicity, H2-only). The
# `ats` build + mk_ats_obj stay in place for research/backtest; set PPP_EMIT_ATS=1 to re-emit
# if a genuine ATS edge is ever established. Totals (UNDER) continue to flow.
EMIT_ATS <- nzchar(Sys.getenv("PPP_EMIT_ATS"))
# proj_vs_open is a NEW, independently-validated edge (CLV-confirmed) — defaults ON (opt OUT
# with PPP_EMIT_ATS_OPEN=0), unlike the retired early-4th-down `ats` arm above which defaults off.
EMIT_ATS_OPEN <- Sys.getenv("PPP_EMIT_ATS_OPEN", "1") != "0"
cand <- lapply(seq_len(nrow(tot)), function(i){ o<-mk_total_obj(tot[i,]); o$.date<-tot$slate_date[i]; o$.gkey<-tot$gkey[i]; o$.completed<-tot$completed[i]; o })
if (EMIT_ATS)
  cand <- c(cand, lapply(seq_len(nrow(ats)), function(i){ o<-mk_ats_obj(ats[i,]); o$.date<-ats$slate_date[i]; o$.gkey<-ats$gkey[i]; o$.completed<-ats$completed[i]; o }))
if (EMIT_ATS_OPEN && nrow(ats_open) > 0)
  cand <- c(cand, lapply(seq_len(nrow(ats_open)), function(i){ o<-mk_ats_open_obj(ats_open[i,]); o$.date<-ats_open$slate_date[i]; o$.gkey<-ats_open$gkey[i]; o$.completed<-ats_open$completed[i]; o }))

# ---- grade one posted bet (returns a results-entry or NULL) ----------------
grade_bet <- function(b) {                       # b: a posted bet list (bet_id, market, side, line, stake_units, event, .gkey?)
  key <- if (!is.null(b$.gkey)) b$.gkey else { p <- strsplit(b$event, " @ ", fixed=TRUE)[[1]]; paste0(slug(p[1]),"|",slug(p[2])) }
  g <- glu_l[[key]]; if (is.null(g) || !isTRUE(g$completed[1])) return(NULL)
  stake <- if (!is.null(b$stake_units)) b$stake_units else 1
  if (b$market == "total") {
    posted <- b$line; res <- if (g$total_points < posted) "win" else if (g$total_points > posted) "loss" else "push"
    clv <- clv_total(posted, g$close_total)
    actual <- list(total_points = g$total_points, line = posted, closing_line = round(g$close_total,1))
  } else {                                        # spread
    posted <- b$line; home_sp <- if (b$side == "home") posted else -posted
    cover_home <- (g$actual_margin + home_sp)
    res <- if (abs(cover_home) < 1e-9) "push" else {
      home_ok <- cover_home > 0; team_ok <- if (b$side=="home") home_ok else !home_ok; if (team_ok) "win" else "loss" }
    close_team <- if (b$side == "home") g$close_home_spread else -g$close_home_spread
    clv <- clv_ats(posted, close_team)
    actual <- list(actual_margin = g$actual_margin, cover_line = posted, closing_line = round(close_team,1))
  }
  pnl <- if (res=="win") round(stake*PAYOUT,2) else if (res=="loss") -stake else 0
  compact(list(bet_id=b$bet_id, result=res, closing_odds_american=ODDS, clv_pct=clv, pnl_units=pnl, actual=actual))
}

# ---- write picks + results per game day ------------------------------------
dates <- sort(unique(vapply(cand, function(b) b$.date, character(1))))
for (d in dates) {
  day <- Filter(function(b) identical(b$.date, d), cand)
  pf  <- file.path(outdir, sprintf("picks_%s.json", d)); rf <- file.path(outdir, sprintf("results_%s.json", d))

  if (SIMULATE) {
    posted <- day
    write_json(list(contract_version="1.0", source="cfb-modeling", sport="cfb", slate_date=d, generated_at=now_iso,
      model_version=MODEL_VER, mode=MODE, event_context=sprintf("Week %d",TW),
      notes="[SIMULATE] historical open-line replay.", bets=lapply(day, function(b) b[!startsWith(names(b),".")])),
      pf, auto_unbox=TRUE, pretty=TRUE, null="null")
    cat(sprintf("  [sim] picks_%s.json (%d)\n", d, length(day)))
  } else {
    up <- Filter(function(b) !isTRUE(b$.completed), day)         # only post games not yet started
    if (length(up) > 0) {
      write_json(list(contract_version="1.0", source="cfb-modeling", sport="cfb", slate_date=d, generated_at=now_iso,
        model_version=MODEL_VER, mode=MODE, event_context=sprintf("Week %d",TW),
        notes="UNDER-only totals + early-season 4th-down ATS. PAPER: forward-validating.",
        bets=lapply(up, function(b) b[!startsWith(names(b),".")])),
        pf, auto_unbox=TRUE, pretty=TRUE, null="null")
      cat(sprintf("  picks_%s.json (%d upcoming)\n", d, length(up)))
    }
    # grade from the PREVIOUSLY-POSTED file (source of truth), if present
    posted <- if (file.exists(pf)) fromJSON(pf, simplifyVector=FALSE)$bets else list()
  }

  results <- Filter(Negate(is.null), lapply(posted, grade_bet))
  if (length(results) > 0) {
    write_json(list(contract_version="1.0", source="cfb-modeling", sport="cfb", slate_date=d, graded_at=now_iso,
      results=results), rf, auto_unbox=TRUE, pretty=TRUE, null="null")
    cat(sprintf("  results_%s.json (%d graded)\n", d, length(results)))
  }
}
cat(sprintf("✓ feed done (%s, %d bets built)\n", if (SIMULATE) "SIMULATE" else "LIVE", length(cand)))
