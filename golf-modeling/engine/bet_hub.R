#!/usr/bin/env Rscript
# ==============================================================================
# GOLF v2 — engine/bet_hub.R   Write 72-hole matchup picks to the Bet Hub feed
#
# After the model produces its board, price the LIVE 72-hole matchups from the v2
# tournament sim (our validated edge: H2H AUC .564 > market .525; +7-12% ROI at
# EV>=3-7%), select the +EV plays, and write ONE schema-compliant JSON to
#   <FEED_DIR>/golf-modeling/pga/picks_<slate_date>.json
# plus a results_<slate_date>.json after the tournament settles. mode=PAPER until
# the CLV gate clears. See golf-modeling.md (the feed contract).
#
# The picks file is CUMULATIVE per slate: every run merges into it, so the card is
# the week's full ledger and --results grades every bet the tournament ever emitted.
#
#   Rscript engine/bet_hub.R              # live: pull matchups, price, merge picks
#   Rscript engine/bet_hub.R --rebuild    # ... but start this slate's card over
#   Rscript engine/bet_hub.R --live       # ... and mark the slate real money (sticky)
# Manual settlements (WD/DQ/void/dead-heat) go in golf_picks/settlements.csv as
# bet_id,result,note -- they override the leaderboard on every regrade.
#   Rscript engine/bet_hub.R --test 525   # offline sample from a historical event
#   Rscript engine/bet_hub.R --results <slate_date>   # grade after settle (add --force
#                                                     # to grade despite an event mismatch)
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(httr2) }))
if (Sys.getenv("ENGINE_WD_SET") == "") local({
  win <- "c:/Users/ljdie/OneDrive/Documents/golf-modeling"
  if (dir.exists(win)) setwd(win) else {  # CI/Linux: find the golf root from --file
    m <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(m)) { d <- dirname(normalizePath(sub("^--file=", "", m[1])))
      while (!dir.exists(file.path(d, "golf_picks")) && dirname(d) != d) d <- dirname(d)
      if (dir.exists(file.path(d, "golf_picks"))) setwd(d) } }
})
Sys.setenv(ENGINE_SOURCE_ONLY="1", SKILL_SOURCE_ONLY="1", COURSEFIT_SOURCE_ONLY="1",
           MARKET_SOURCE_ONLY="1", SIMULATE_SOURCE_ONLY="1", PROJECT_SOURCE_ONLY="1", OWNERSHIP_SOURCE_ONLY="1")
if (!exists("project_mu")) source("engine/project.R")
emsg <- get0("emsg", ifnotfound=function(...) cat(..., "\n")); OUT <- "golf_picks"
`%||%` <- function(a,b) if(is.null(a)||length(a)==0||(length(a)==1&&is.na(a))) b else a
FEED <- Sys.getenv("FEED_DIR", "C:/Users/ljdie/OneDrive/Documents/dashboard_feed")
KEY  <- Sys.getenv("DATAGOLF_API_KEY")
EV_THRESH <- 0.03; MODEL_VERSION <- "v2-2026.08"      # validated +EV zone (H2H)
EV_THRESH_T10 <- 0.05; MAX_DEC_T10 <- 6.0             # top-10: +EV>=5% AND odds <= +500
# (walk-forward verified profitable 2024-26; the >+900 tail is -EV and excluded)

.norm  <- function(x){ x<-tolower(trimws(gsub("[^A-Za-z, ]","",x)))
  ifelse(grepl(",",x), sub("^(.*),\\s*(.*)$","\\2 \\1",x), x) }
.am2dec <- function(am){ am<-as.numeric(am); fifelse(am>0,am/100+1,100/abs(am)+1) }
.dec2am <- function(d){ d<-as.numeric(d); as.integer(round(fifelse(d>=2,(d-1)*100,-100/(d-1)))) }
# PROPER unit allocation: quarter-Kelly (1u = 1% bankroll), edge- AND odds-aware, so
# a longshot is staked far less than a favourite at the same EV. Capped 3u, floor 0.25u.
.kelly_units <- function(ev, dec, kf=0.25, upct=0.01, cap=3, fl=0.25)
  round(pmin(pmax(kf * pmax(ev,0) / pmax(dec-1,1e-6) / upct, fl), cap), 2)
gj <- function(u) tryCatch(request(u)|>req_timeout(30)|>req_perform()|>resp_body_json(simplifyVector=TRUE), error=function(e) NULL)
dg <- function(ep,ps=list()){ ps$key<-KEY; ps$file_format<-"json"
  gj(paste0("https://feeds.datagolf.com/",ep,"?",paste(names(ps),unlist(ps),sep="=",collapse="&"))) }

# ── field rank sim (mu + round_sd -> 72-hole rank) -> P(a finishes ahead of b) ─
sim_event <- function(pool, n_sims=8000L, cut_n=65L, cond_sd=0.35, spread_scale=1.0, seed=1L) {
  set.seed(seed); pl <- as.data.table(pool)[is.finite(mu)&is.finite(round_sd)]
  P <- nrow(pl); if (P<3) return(NULL); sd <- pmax(pl$round_sd,0.4)*spread_scale
  draw <- function() matrix(rnorm(P*n_sims, pl$mu, sd),P,n_sims)+rep(rnorm(n_sims,0,cond_sd*spread_scale),each=P)
  h36<-draw()+draw(); score<-h36+draw()+draw()
  made <- apply(h36,2L,function(c) c >= sort(c,decreasing=TRUE)[min(cut_n,length(c))]-1e-9)
  score[!made] <- h36[!made]-1000
  list(ids=pl$player_id, rank=apply(score,2L,function(c) frank(-c,ties.method="first")))
}
h2h_prob <- function(s,a,b){ ia<-match(a,s$ids); ib<-match(b,s$ids)
  if(is.na(ia)||is.na(ib)) return(NA_real_); mean(s$rank[ia,]<s$rank[ib,]) }

# ── live 72-hole matchup odds from DataGolf betting-tools ──────────────────────
# match_list rows have p1/p2 player names + per-book american odds (odds.<book>.p1/p2).
# We price what FanDuel, DraftKings, or bet365 offers: for each side take the best
# (highest-decimal) across those books and record which one it came from. Matchups
# offered on none of them (both sides NA) are dropped. ids resolved by the caller.
dg_live_matchups <- function(tour="pga") {
  raw <- dg("betting-tools/matchups", list(tour=tour, market="tournament_matchups", odds_format="american"))
  ml <- raw$match_list
  if (is.null(ml) || is.character(ml) || (is.data.frame(ml) && !nrow(ml))) return(list(event=raw$event_name, m=data.table()))
  d <- as.data.table(ml)
  nm <- names(d)
  p1n <- d[[grep("p1.*player_name|p1_player_name|player_name.*p1", nm, value=TRUE)[1]]] %||% d$p1_player_name
  p2n <- d[[grep("p2.*player_name|p2_player_name", nm, value=TRUE)[1]]] %||% d$p2_player_name
  col <- function(book, side) { cn <- paste0("odds.", book, ".", side)     # book price column, or all-NA
    if (cn %in% nm) suppressWarnings(as.numeric(d[[cn]])) else rep(NA_real_, nrow(d)) }
  # candidate books (display label -> DataGolf column key). bet365 carries most of
  # the feed right now, so including it dramatically expands the gradeable pool.
  BOOKS <- c(FanDuel = "fanduel", DraftKings = "draftkings", bet365 = "bet365")
  side_best <- function(side) {                          # elementwise best price across books + its label
    n <- nrow(d); am <- rep(NA_real_, n); bk <- rep(NA_character_, n); best <- rep(-Inf, n)
    for (lab in names(BOOKS)) { a <- col(BOOKS[[lab]], side); dec <- .am2dec(a)
      better <- is.finite(dec) & dec > best                # strict: FD > DK > bet365 on ties
      am[better] <- a[better]; bk[better] <- lab; best[better] <- dec[better] }
    list(am = am, book = bk) }
  s1 <- side_best("p1"); s2 <- side_best("p2")
  out <- data.table(p1_name=p1n, p2_name=p2n,
                    p1_am=s1$am, p1_book=s1$book, p2_am=s2$am, p2_book=s2$book)
  list(event=raw$event_name, m=out[is.finite(p1_am) & is.finite(p2_am)])    # both sides must be FD/DK-priced
}

# ── price offered matchups + select the +EV side ──────────────────────────────
# nameid: data.table(player_id, player_name) for the field (to resolve ids)
build_matchup_picks <- function(sim, m, nameid, event, event_start=NULL, ev_thresh=EV_THRESH,
                                slate_date=NULL) {
  if (!nrow(m)) return(list())
  sd_id <- slate_date %||% attr(sim,"date") %||% as.character(Sys.Date())   # keyed to the SLATE, not the run day
  nid <- as.data.table(nameid)[, .(norm=.norm(player_name), player_id, player_name)]
  m <- as.data.table(copy(m)); m[, `:=`(n1=.norm(p1_name), n2=.norm(p2_name))]
  m <- merge(m, nid[,.(n1=norm, id1=player_id, nm1=player_name)], by="n1", all.x=TRUE)
  m <- merge(m, nid[,.(n2=norm, id2=player_id, nm2=player_name)], by="n2", all.x=TRUE)
  m <- m[is.finite(id1)&is.finite(id2)]
  m[, p1_win := mapply(function(a,b) h2h_prob(sim,a,b), id1, id2)]
  m <- m[is.finite(p1_win)]
  bets <- list()
  for (i in seq_len(nrow(m))) {
    r <- m[i]; d1<-.am2dec(r$p1_am); d2<-.am2dec(r$p2_am)
    ev1 <- r$p1_win*d1 - 1; ev2 <- (1-r$p1_win)*d2 - 1
    if (max(ev1,ev2) <= ev_thresh) next
    p1side <- ev1 >= ev2
    disp <- function(x) if(grepl(",",x)) sub("^(.*),\\s*(.*)$","\\2 \\1",trimws(x)) else x  # "Last, First"->"First Last"
    sel<-disp(if(p1side) r$nm1 else r$nm2); opp<-disp(if(p1side) r$nm2 else r$nm1)
    am<-if(p1side) r$p1_am else r$p2_am; dec<-if(p1side) d1 else d2
    bk<-if("p1_book" %in% names(m)) (if(p1side) r$p1_book else r$p2_book) else NA_character_  # FanDuel / DraftKings
    prob<-if(p1side) r$p1_win else 1-r$p1_win; ev<-if(p1side) ev1 else ev2
    mkt<-1/dec; opp_imp<-1/(if(p1side) d2 else d1); fair<-mkt/(mkt+opp_imp)   # de-vig
    slug<-function(x) gsub("[^a-z0-9]","",tolower(x))
    # STABLE id: slate date + event + the unordered player pair. Deliberately free of
    # book and of which side we took, so a re-run that flips sides (different book best
    # price, new sim draw) re-emits the SAME logical bet instead of a duplicate.
    pair <- sort(c(slug(sel), slug(opp)))
    b <- list(
      bet_id = sprintf("golf-modeling-%s-%s-matchup-%s-vs-%s", sd_id,
                       slug(substr(event,1,10)), pair[1], pair[2]),
      event = event, event_start = event_start, market = "matchup",
      market_label = "72-Hole Matchup", selection = sel,
      odds_american = as.integer(am),
      model_prob = round(prob,4), market_prob = round(fair,4),
      edge = round(prob-fair,4), ev_pct = round(ev,4),
      stake_units = .kelly_units(ev, dec),                   # quarter-Kelly (edge + odds aware)
      confidence = if(ev>=0.08) "high" else if(ev>=0.04) "medium" else "low",
      tags = list("matchup","72_hole"),
      details = list(opponent = opp, matchup_type = "72_hole"))
    if (!is.na(bk) && nzchar(bk)) b$book <- bk         # exact book the price came from
    bets[[length(bets)+1]] <- b
  }
  if (!length(bets)) return(bets)
  bets <- bets[order(-vapply(bets, function(b) b$ev_pct, 0))]                 # best EV first
  bets[!duplicated(vapply(bets, function(b) b$bet_id, ""))]                   # one bet per unordered pair
}

# ── live TOP-10 outright odds: best of FanDuel/DraftKings/bet365 per player ────
dg_live_outrights <- function(tour="pga", market="top_10") {
  raw <- dg("betting-tools/outrights", list(tour=tour, market=market, odds_format="american"))
  od <- raw$odds
  if (is.null(od) || is.character(od) || (is.data.frame(od) && !nrow(od))) return(list(event=raw$event_name, m=data.table()))
  d <- as.data.table(od); nm <- names(d)
  BOOKS <- c(FanDuel="fanduel", DraftKings="draftkings", bet365="bet365")
  am <- rep(NA_real_, nrow(d)); bk <- rep(NA_character_, nrow(d)); bestd <- rep(-Inf, nrow(d))
  for (lab in names(BOOKS)) { cn <- BOOKS[[lab]]
    a  <- if (cn %in% nm) suppressWarnings(as.numeric(d[[cn]])) else rep(NA_real_, nrow(d))
    de <- .am2dec(a); use <- is.finite(de) & de > bestd
    am[use] <- a[use]; bk[use] <- lab; bestd[use] <- de[use] }
  out <- data.table(dg_id=suppressWarnings(as.integer(d$dg_id)), player_name=d$player_name, am=am, book=bk)
  list(event=raw$event_name, m=out[is.finite(dg_id) & is.finite(am)])
}

# ── price TOP-10 finishes from the sim; keep the +EV plays in the profitable odds
# band (<= +500), Kelly-staked. Emitted alongside the matchup card. ─────────────
build_top10_picks <- function(sim, od, event, event_start=NULL, sd=NULL,
                              ev_thresh=EV_THRESH_T10, max_dec=MAX_DEC_T10) {
  if (is.null(od) || !nrow(od)) return(list())
  p10 <- data.table(dg_id=as.integer(sim$ids), p_top10=rowMeans(sim$rank <= 10L))
  m <- merge(as.data.table(od), p10, by="dg_id")
  m <- m[is.finite(am) & is.finite(p_top10)]; if (!nrow(m)) return(list())
  m[, dec := .am2dec(am)]; m[, ev := p_top10*dec - 1]
  m <- m[ev > ev_thresh & dec <= max_dec][order(-ev)]                         # band + EV filter
  if (!nrow(m)) return(list())
  disp <- function(x) if(grepl(",",x)) sub("^(.*),\\s*(.*)$","\\2 \\1",trimws(x)) else x
  slug <- function(x) gsub("[^a-z0-9]","",tolower(x))
  lapply(seq_len(nrow(m)), function(i){ r <- m[i]; sel <- disp(r$player_name)
    b <- list(
      bet_id = sprintf("golf-modeling-%s-%s-top10-%s",                        # stable: slate + player
                       sd %||% as.character(Sys.Date()), slug(substr(event,1,10)), slug(sel)),
      event = event, event_start = event_start, market = "top_10", market_label = "Top 10 Finish",
      selection = sel, odds_american = as.integer(r$am),
      model_prob = round(r$p_top10,4), market_prob = round(1/r$dec,4),
      edge = round(r$p_top10 - 1/r$dec,4), ev_pct = round(r$ev,4),
      stake_units = .kelly_units(r$ev, r$dec),
      confidence = if(r$ev>=0.15) "high" else if(r$ev>=0.08) "medium" else "low",
      tags = list("outright","top_10"), details = list(matchup_type = "top_10"))
    if (!is.na(r$book) && nzchar(r$book)) b$book <- r$book
    b })
}

# ── bets you've struck off: golf_picks/excluded_bets.csv (bet_id,note) ────────
# A cumulative card can't otherwise honour a removal: the model re-emits the same
# +EV pick on the next run and it walks straight back on. Anything listed here is
# dropped from the card AND from grading, permanently.
EXCLUDE_FILE <- file.path(OUT, "excluded_bets.csv")
.excluded <- function(f=EXCLUDE_FILE) {
  if (!file.exists(f)) return(character())
  e <- tryCatch(fread(f, colClasses="character"), error=function(z) NULL)
  if (is.null(e) || !nrow(e) || !"bet_id" %in% names(e)) return(character())
  unique(e[nzchar(bet_id)]$bet_id)
}

# Backups live OUTSIDE the feed dir: that folder is a contract surface the bet site
# scans, and a stray picks_*.json.bak next to the real card is asking to be ingested.
.feed_backup <- function(f, ext="bak") {
  bd <- file.path(OUT, "feed_backups"); dir.create(bd, recursive=TRUE, showWarnings=FALSE)
  to <- file.path(bd, sprintf("%s.%s.%s", basename(f), format(Sys.time(), "%Y%m%dT%H%M%S"), ext))
  file.copy(f, to, overwrite=TRUE); to
}

# ── write the picks JSON (schema-compliant) ───────────────────────────────────
# CUMULATIVE + IMMUTABLE per slate. The card is the tournament's LEDGER, not a
# snapshot: we run several times a week, and a bet that cleared the EV bar on
# Tuesday is a bet we made — it must survive Thursday's run pricing it below the
# bar or the book pulling the market. So each run MERGES into the existing file:
#   * a bet_id already on file keeps its original price/EV/stake (never restated,
#     or CLV and P&L would be rewritten after the fact); only last_seen updates
#   * genuinely new bet_ids are appended
#   * nothing is ever dropped -> --results grades every bet emitted all week
# Pass rebuild=TRUE (CLI: --rebuild) to deliberately start the slate's card over.
# mode: explicit arg > BETHUB_MODE env > whatever the slate's card already says > PAPER.
# Sticky by design — once a slate is flipped to LIVE, the daily re-run must not quietly
# demote it back to PAPER just because it didn't pass a mode.
write_picks_feed <- function(bets, event, slate_date, mode=NULL, notes=NULL, rebuild=FALSE) {
  bid  <- function(x) vapply(x, function(b) b$bet_id %||% "", "")
  today <- as.character(Sys.Date())
  if (length(bets)) {                              # hard guarantee: one row per logical bet_id
    ids  <- bid(bets)
    if (any(duplicated(ids))) emsg("dropped ", sum(duplicated(ids)), " duplicate bet_id(s)")
    bets <- bets[!duplicated(ids) & nzchar(ids)]
    bets <- lapply(bets, function(b) {                   # provenance (free-form per schema);
      d <- b$details %||% list()                         # a caller that knows the real dates wins
      if (is.null(d$first_seen)) d$first_seen <- today
      if (is.null(d$last_seen))  d$last_seen  <- today
      b$details <- d; b })
  }
  dir <- file.path(FEED,"golf-modeling","pga"); dir.create(dir,recursive=TRUE,showWarnings=FALSE)
  f <- file.path(dir, sprintf("picks_%s.json", slate_date))
  prev <- NULL
  if (file.exists(f)) {
    prev <- tryCatch(jsonlite::fromJSON(f, simplifyVector=FALSE), error=function(e) NULL)
    if (is.null(prev)) {                           # unreadable: keep a copy, never silently destroy it
      emsg("could not parse ", f, " -> saved ", .feed_backup(f, "bad"))
      if (!length(bets)) { emsg("SKIP write ", f, ": unreadable card and 0 new picks"); return(invisible(f)) }
    }
  }
  if (rebuild && !is.null(prev) && length(prev$bets)) {
    emsg("--rebuild: replacing ", length(prev$bets), " existing picks (backup at ", .feed_backup(f), ")")
    prev <- NULL
    # results carry-forward would otherwise resurrect a bet this rebuild deliberately
    # removed, so retire the slate's graded file too (backed up, regenerated next grade).
    rf <- file.path(dir, sprintf("results_%s.json", slate_date))
    if (file.exists(rf)) { .feed_backup(rf); file.remove(rf)
      emsg("--rebuild: retired ", basename(rf), " (backup kept); re-run --results to regrade") }
  }
  if (!is.null(prev) && length(prev$bets)) {       # MERGE: existing bets are immutable
    keep <- prev$bets; pid <- bid(keep); keep <- keep[!duplicated(pid) & nzchar(pid)]; pid <- bid(keep)
    nid  <- bid(bets)
    keep <- lapply(keep, function(b) { if (b$bet_id %in% nid) b$details$last_seen <- today; b })  # still offered

    add  <- bets[!(nid %in% pid)]
    if (length(add)) emsg("merged ", length(add), " new pick(s) into ", length(keep), " already on file")
    else emsg("no new picks; ", length(keep), " already on file preserved")
    bets <- c(keep, add)
  }
  ex <- .excluded()                                # struck-off bets never come back
  if (length(ex) && length(bets)) {
    drop <- bid(bets) %in% ex
    if (any(drop)) emsg("excluded ", sum(drop), " struck-off bet(s) per ", EXCLUDE_FILE)
    bets <- bets[!drop]
  }
  if (length(bets) > 1)                            # deterministic display order (frozen EV -> stable)
    bets <- bets[order(-vapply(bets, function(b) as.numeric(b$ev_pct %||% NA), 0), na.last=TRUE)]
  if (!length(bets) && is.null(prev)) emsg("note: no picks and no existing card for ", slate_date)
  mode <- mode %||% (if (nzchar(Sys.getenv("BETHUB_MODE"))) Sys.getenv("BETHUB_MODE")
                     else prev$mode %||% "PAPER")
  if (!mode %in% c("LIVE","PAPER")) { emsg("bad mode '", mode, "' -> PAPER"); mode <- "PAPER" }
  if (!identical(mode, prev$mode %||% mode)) emsg("mode: ", prev$mode, " -> ", mode)
  picks <- list(contract_version="1.0", source="golf-modeling", sport="pga",
                slate_date=slate_date,
                generated_at=format(as.POSIXct(Sys.time(),tz="UTC"),"%Y-%m-%dT%H:%M:%SZ"),
                model_version=MODEL_VERSION, mode=mode, event_context=(prev$event_context %||% event),
                notes=notes %||% "72-hole matchups from the v2 sim. PAPER until CLV proves positive.",
                bets=bets)
  write_json(picks, f, auto_unbox=TRUE, pretty=TRUE, null="null")
  emsg("wrote ", f, " (", length(bets), " picks, mode ", mode, ")")
  invisible(f)
}

# ── finishes table from any leaderboard payload (in-play, master, …) ──────────
# DEFENSIVE by design: the in-play feed is dark between events and dg() returns NULL
# on an API hiccup, so an absent/empty/short payload must yield an empty table, never
# an "object 'player_name' not found" error that kills the whole grading run.
.finish_table <- function(d, name_col="player_name", pos_col="current_pos") {
  empty <- data.table(norm=character(), finish=integer())
  if (is.null(d)) return(empty)
  d <- tryCatch(as.data.table(d), error=function(e) NULL)
  if (is.null(d) || !nrow(d) || !all(c(name_col, pos_col) %in% names(d))) return(empty)
  f <- data.table(norm=.norm(as.character(d[[name_col]])), pos=as.character(d[[pos_col]]))
  f[, finish := suppressWarnings(as.integer(gsub("[^0-9]","", pos)))]
  f[grepl("CUT|^MC$", pos, ignore.case=TRUE) & is.na(finish), finish := 999L]  # MC = behind every made cut
  f[, pos := NULL]                                                             # WD/DQ stay NA -> pending
  unique(f[nzchar(norm)], by="norm")
}
# accept an already-built finishes table (norm/finish) or nothing at all; return it
# de-duplicated and keyed for lookup. Missing/short input -> empty keyed table.
.finish_table_norm <- function(f) {
  f <- tryCatch(as.data.table(f), error=function(e) NULL)
  if (is.null(f) || !nrow(f) || !all(c("norm","finish") %in% names(f)))
    f <- data.table(norm=character(), finish=integer())
  f <- unique(f[!is.na(norm) & nzchar(norm), .(norm, finish=as.integer(finish))], by="norm")
  setkey(f, norm); f
}
# live leaderboard for the tournament currently in play (event name comes back too,
# so the caller can refuse to grade slate A against tournament B).
dg_live_finishes <- function(tour="pga") {
  ip <- dg("preds/in-play", list(tour=tour))
  list(event = tryCatch(ip$info$event_name, error=function(e) NULL) %||% NA_character_,
       fin   = .finish_table(if (is.null(ip)) NULL else ip$data))
}

# ── manual settlements: golf_picks/settlements.csv (bet_id,result,note) ───────
# The leaderboard can't tell us how a book actually settled a bet — WDs, DQs, voided
# markets, dead-heat calls. Recording those here (rather than hand-editing the results
# JSON) means they survive every regrade instead of reverting to "pending" next run.
SETTLE_FILE <- file.path(OUT, "settlements.csv")
.settlements <- function(f=SETTLE_FILE) {
  if (!file.exists(f)) return(NULL)
  s <- tryCatch(fread(f, colClasses="character"), error=function(e) NULL)
  if (is.null(s) || !nrow(s) || !all(c("bet_id","result") %in% names(s))) return(NULL)
  s <- s[nzchar(bet_id) & result %in% c("win","loss","push","void","pending")]
  if (!nrow(s)) return(NULL)
  s <- unique(s, by="bet_id", fromLast=TRUE)                 # last row wins
  setNames(split(s, seq_len(nrow(s))), s$bet_id)
}
.apply_settlements <- function(results, ov=.settlements()) {
  if (is.null(ov) || !length(results)) return(results)
  n <- 0L
  results <- lapply(results, function(r) { o <- ov[[r$bet_id]]
    if (is.null(o) || identical(r$result, o$result)) return(r)
    r$actual <- c(r$actual %||% list(),
                  list(settled_manually=TRUE, graded_as=r$result,                # what the leaderboard said
                       settlement_note=if ("note" %in% names(o) && nzchar(o$note)) o$note else NULL))
    r$result <- o$result; n <<- n + 1L; r })
  if (n) emsg("applied ", n, " manual settlement(s) from ", SETTLE_FILE)
  results
}

# ── grade the picks after the tournament settles -> results_<date>.json ───────
# Grading is driven ENTIRELY by the picks file: each bet's `selection` (and, for
# matchups, `details$opponent`) is looked up in the finishes table. Nothing here
# reads a `player_name` column off the picks side.
# finishes: data.table(norm, finish); closing: optional named list bet_id -> closing dec
write_results_feed <- function(slate_date, finishes, closing_dec=NULL, source_event=NULL, force=FALSE) {
  pf <- file.path(FEED,"golf-modeling","pga", sprintf("picks_%s.json", slate_date))
  if (!file.exists(pf)) { emsg("no picks file for ", slate_date); return(invisible()) }
  picks <- jsonlite::fromJSON(pf, simplifyVector=FALSE)
  if (!length(picks$bets)) { emsg("picks_", slate_date, ".json has no bets; nothing to grade"); return(invisible()) }
  # never grade one slate's card against a different tournament's leaderboard
  pe <- picks$event_context %||% ""
  if (!force && !is.null(source_event) && !is.na(source_event) && nzchar(pe) &&
      !identical(tolower(trimws(pe)), tolower(trimws(source_event)))) {
    emsg("event mismatch: picks are '", pe, "' but finishes are '", source_event,
         "' -- not grading ", slate_date, " (pass --force to override)")
    return(invisible())
  }
  finishes <- .finish_table_norm(finishes)
  fin <- function(nm){ if (is.null(nm) || !length(nm) || is.na(nm) || !nzchar(nm)) return(NA_integer_)
    v <- finishes[.(.norm(nm)), finish, nomatch=NULL]; if (length(v)) as.integer(v[1]) else NA_integer_ }
  miss <- character()
  results <- lapply(picks$bets, function(b) {
    sf <- fin(b$selection); if (is.na(sf)) miss <<- c(miss, b$selection %||% "?")
    tn <- if (is.character(b$market) && grepl("^top_[0-9]+$", b$market))       # top_5 / top_10 / top_20 …
            as.integer(sub("^top_","", b$market)) else NA_integer_
    if (!is.na(tn)) {                                          # top-N: win iff finish <= N (MC=999 -> loss)
      res <- if (is.na(sf)) "pending" else if (sf <= tn) "win" else "loss"
      r <- list(bet_id=b$bet_id, result=res, actual=list(selection_finish=if(is.na(sf)) NULL else sf))
    } else {                                                   # matchup: sel finishes ahead of opp
      of <- fin(b$details$opponent); if (is.na(of)) miss <<- c(miss, b$details$opponent %||% "?")
      res <- if (is.na(sf)||is.na(of)) "pending" else if (sf<of) "win" else if (sf>of) "loss" else "push"
      r <- list(bet_id=b$bet_id, result=res,
                actual=list(selection_finish=if(is.na(sf)) NULL else sf,
                            opponent_finish =if(is.na(of)) NULL else of))
    }
    if (!is.null(closing_dec)) { cd <- closing_dec[[b$bet_id]]
      if (!is.null(cd) && is.finite(cd)) { r$closing_odds_american <- .dec2am(cd)
        r$clv_pct <- round(.am2dec(b$odds_american)/cd - 1, 4) } }   # +CLV = beat the close
    r
  })
  f <- file.path(FEED,"golf-modeling","pga", sprintf("results_%s.json", slate_date))
  # BELT AND BRACES for cards written before picks files went cumulative: keep every
  # bet_id this slate has ever graded, and never demote an already-settled one back to
  # pending because its bet is missing from today's card or the leaderboard went dark.
  old <- if (file.exists(f)) tryCatch(jsonlite::fromJSON(f, simplifyVector=FALSE)$results,
                                      error=function(e) NULL) else NULL
  if (length(old)) {
    oid <- vapply(old, function(x) x$bet_id %||% "", ""); old <- old[!duplicated(oid) & nzchar(oid)]
    names(old) <- vapply(old, function(x) x$bet_id, "")
    results <- lapply(results, function(r) { o <- old[[r$bet_id]]
      if (r$result=="pending" && !is.null(o) && !identical(o$result,"pending")) o else r })
    carry <- old[!(names(old) %in% vapply(results, function(x) x$bet_id, ""))]
    if (length(carry)) emsg("carried forward ", length(carry), " earlier bet(s) no longer on the picks card")
    results <- c(results, unname(carry))
  }
  ex <- .excluded()                           # struck-off bets don't get graded either
  if (length(ex)) results <- results[!(vapply(results, function(x) x$bet_id, "") %in% ex)]
  results <- .apply_settlements(results)      # book's word beats the leaderboard's
  res_of  <- vapply(results, function(x) x$result, "")
  settled <- sum(res_of != "pending")
  # GUARD (mirrors write_picks_feed): never replace a settled card with an all-pending
  # one just because the leaderboard went dark.
  if (!settled) {
    if (length(miss)) emsg("no finish found for: ", paste(unique(miss), collapse=", "))
    emsg("SKIP write ", f, ": 0 of ", length(results), " bets gradeable (empty/stale finishes)")
    return(invisible())
  }
  out <- list(contract_version="1.0", source="golf-modeling", sport="pga", slate_date=slate_date,
              graded_at=format(as.POSIXct(Sys.time(),tz="UTC"),"%Y-%m-%dT%H:%M:%SZ"), results=results)
  jsonlite::write_json(out, f, auto_unbox=TRUE, pretty=TRUE, null="null")
  wn <- sum(res_of == "win"); ls <- sum(res_of == "loss")
  emsg("wrote ", f, " (", settled, "/", length(results), " settled: ", wn, "W-", ls, "L",
       if (settled < length(results)) paste0(", ", length(results)-settled, " pending") else "", ")")
  if (length(miss)) emsg("  no finish found for: ", paste(unique(miss), collapse=", "))
  invisible(f)
}

# ── main ──────────────────────────────────────────────────────────────────────
if (identical(environment(), globalenv()) && !nzchar(Sys.getenv("BETHUB_SOURCE_ONLY"))) {
  a <- commandArgs(trailingOnly=TRUE); rb <- "--rebuild" %in% a   # --rebuild: start the slate's card over
  if ("--live" %in% a) Sys.setenv(BETHUB_MODE="LIVE")             # --live: this slate is real money
  bundle <- readRDS(file.path(OUT,"v2_bundle.rds"))
  M <- as.data.table(readRDS(file.path(OUT,"v2_master.rds"))$master)
  if ("--test" %in% a) {                       # offline: historical event + cached odds
    ti <- which(a=="--test"); eid <- a[ti+1] %||% "100"
    yr <- suppressWarnings(as.integer(a[ti+2])); if (is.na(yr)) yr <- 2024L
    e <- M[event_id==eid & year==yr]; if(!"course_par"%in%names(e)) e[,course_par:=71L]
    Pm <- project_mu(bundle, e); sim <- sim_event(Pm[,.(player_id,mu,round_sd)], spread_scale=bundle$spread_scale)
    attr(sim,"date") <- as.character(e$event_date[1])
    # cached matchup odds for this event (wf_odds_cache is DECIMAL)
    oc <- tryCatch(as.data.table(readRDS(file.path(OUT,"wf_odds_cache",sprintf("ev_%s_%d.rds",eid,yr)))), error=function(e) NULL)
    if (is.null(oc)) { emsg("no cached odds for ", eid); write_picks_feed(list(), paste0("event ",eid), as.character(e$event_date[1])); quit() }
    hh <- oc[bet_type=="72-hole Match" & !is.na(p1_dg_id) & !is.na(p2_dg_id)]
    nm <- unique(e[,.(player_id, player_name)])
    m <- data.table(p1_name=nm[match(as.integer(hh$p1_dg_id),player_id)]$player_name,
                    p2_name=nm[match(as.integer(hh$p2_dg_id),player_id)]$player_name,
                    p1_am=.dec2am(as.numeric(hh$p1_open)), p2_am=.dec2am(as.numeric(hh$p2_open)))
    m <- m[!is.na(p1_name)&!is.na(p2_name)]
    bets <- build_matchup_picks(sim, m, nm, paste0("Event ",eid), ev_thresh=EV_THRESH,
                                slate_date=as.character(e$event_date[1]))
    write_picks_feed(bets, paste0("Event ",eid," (TEST)"), as.character(e$event_date[1]), rebuild=rb,
                     notes="TEST sample from a historical event + cached odds (leakage-free projection).")
  } else if ("--results" %in% a) {             # grade a slate's picks after settle
    ri <- which(a=="--results"); sd <- a[ri+1]
    ei <- which(a=="--event")
    if (!length(ei) && (is.na(sd) || !nzchar(sd) || startsWith(sd, "--"))) {   # no date given -> grade the newest picks slate
      pfs <- list.files(file.path(FEED,"golf-modeling","pga"), pattern="^picks_.*\\.json$")
      if (length(pfs)) { sd <- sub("^picks_(.*)\\.json$","\\1", sort(pfs, decreasing=TRUE)[1]); emsg("--results: defaulting to latest slate ", sd) }
    }
    if (is.na(sd) || !nzchar(sd) || startsWith(sd, "--")) { emsg("--results: no slate to grade"); quit() }
    fc <- "--force" %in% a
    if (length(ei)) {                          # historical/test: finishes + closing from master/cache
      eid<-a[ei+1]; yr<-suppressWarnings(as.integer(a[ei+2])); if(is.na(yr)) yr<-2024L
      e <- M[event_id==eid & year==yr]
      if (!nrow(e)) { emsg("no master rows for event ", eid, " ", yr); quit() }
      finishes <- e[, .(norm=.norm(player_name), finish=as.integer(finish))]
      # closing odds by bet: from cache p1_close (keyed to selection)
      cl <- NULL; oc <- tryCatch(as.data.table(readRDS(file.path(OUT,"wf_odds_cache",sprintf("ev_%s_%d.rds",eid,yr)))), error=function(z) NULL)
      write_results_feed(sd, finishes, cl, force=TRUE)   # event_context is "Event <id> (TEST)"; no name to match
    } else {                                   # LIVE: in-play finishes for the current tournament
      lf <- dg_live_finishes("pga")
      if (!nrow(lf$fin)) emsg("in-play feed returned no leaderboard (between events / API hiccup)")
      write_results_feed(sd, lf$fin, source_event=lf$event, force=fc)
    }
  } else {                                     # LIVE
    lm <- dg_live_matchups("pga"); event <- lm$event
    sl <- assemble_live_slate("pga","main",M); pool <- as.data.table(sl$players)
    pool[, `:=`(event_id="live", year=as.integer(format(Sys.Date(),"%Y")), course_par=71L)]
    mkt <- tryCatch(get_dg_pretourn("pga","baseline_history_fit"), error=function(e) data.table())
    sr  <- tryCatch(get_dg_skill_ratings(), error=function(e) data.table())
    Pm <- project_mu(bundle, pool, if(nrow(mkt)) mkt else NULL, if(nrow(sr)) sr else NULL)
    sim <- sim_event(Pm[,.(player_id,mu,round_sd)], spread_scale=bundle$spread_scale)
    attr(sim,"date") <- as.character(Sys.Date())
    # slate_date = tournament first-round date (from schedule); fall back to today
    sd <- tryCatch({ s<-dg("get-schedule",list(tour="pga")); ss<-as.data.table(s$schedule)
      substr(ss[event_name==event]$start_date[1],1,10) }, error=function(e) NA)
    if (is.na(sd)) sd <- as.character(Sys.Date())
    bets <- build_matchup_picks(sim, lm$m, unique(pool[,.(player_id,player_name)]), event %||% "PGA",
                                ev_thresh=EV_THRESH, slate_date=sd)      # slate-keyed ids: reruns don't duplicate
    ot   <- tryCatch(dg_live_outrights("pga","top_10"), error=function(e) list(m=data.table()))
    t10  <- build_top10_picks(sim, ot$m, event %||% "PGA", sd=sd)              # +EV top-10s (<= +500)
    emsg("matchup picks: ", length(bets), " | top-10 picks: ", length(t10))
    write_picks_feed(c(bets, t10), event %||% "PGA", sd, rebuild=rb,   # merges into this slate's running card
      notes="72-hole matchups + top-10 finishes (favourites/mid band, walk-forward +EV) from the v2 sim. PAPER until CLV proves positive.")
  }
}
