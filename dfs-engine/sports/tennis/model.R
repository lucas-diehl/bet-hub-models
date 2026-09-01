# ==============================================================================
# Tennis plugin — projection MODEL (surface-Elo win prob -> DK-points mixture)
# Tennis DFS points are bimodal: a player either advances (lots of games/sets/
# aces/bonuses) or loses early (little). So we model the two pieces the notes call
# out as the edge:
#   1. WIN PROBABILITY from surface-adjusted Elo (overall + per-surface), updated
#      chronologically so every match's rating is leak-free.
#   2. DK | WIN  and  DK | LOSS, as shrunk player means toward (tour x best_of)
#      league means.  proj = p_win * E[DK|win] + (1-p_win) * E[DK|loss].
#   distribution: mixture variance (between-outcome gap + within-outcome spread)
#   -> sim_sd / ceil (advance scenario) / floor (early-loss scenario) / p_zero.
#
# A SINGLE chronological pass produces a leak-free projection for every match
# (used by the walk-forward backtest) and leaves the final per-player state (used
# for live projection). DK box stats are reconstructed from the Sackmann score
# string so we can score history for both players.
#
# NOTE: validated on synthetic matches in this environment (the network filter
# blocks JeffSackmann's GitHub while allowing sportsdataverse). On an unfiltered
# machine, tennis_train() pulls real ATP/WTA history and the same backtest runs.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

TENNIS_MODEL_PATH <- function() dfs_path("data", "models", "tennis_models.rds")

# ── parse a Sackmann score string (always from the WINNER's perspective) ──────
parse_tennis_score <- function(score) {
  if (is.na(score)) return(NULL)
  s <- trimws(score)
  if (!nchar(s) || grepl("W/?O|walkover|def", s, ignore.case = TRUE)) return(NULL)
  ret  <- grepl("RET|DEF", s, ignore.case = TRUE)
  toks <- strsplit(gsub("(RET|DEF|\\.)", "", s, ignore.case = TRUE), "\\s+")[[1]]
  gw <- gl <- sw <- sl <- bagw <- bagl <- 0L; nset <- 0L
  for (t in toks) {
    t2 <- sub("\\(.*\\)$", "", trimws(t))
    m <- regmatches(t2, regexec("^(\\d+)-(\\d+)$", t2))[[1]]
    if (length(m) != 3) next
    a <- as.integer(m[2]); b <- as.integer(m[3]); nset <- nset + 1L
    gw <- gw + a; gl <- gl + b
    if (a > b) { sw <- sw + 1L; if (b == 0L) bagw <- bagw + 1L }
    else if (b > a) { sl <- sl + 1L; if (a == 0L) bagl <- bagl + 1L }
  }
  if (nset == 0L) return(NULL)
  list(g_for = gw, g_against = gl, s_for = sw, s_against = sl,
       bag_for = bagw, bag_against = bagl, ret = ret)
}

# DK box for one player given the parsed (winner-perspective) score + serve stats.
.tennis_player_box <- function(p, is_winner, ace, df, breaks) {
  if (is_winner) { gwon<-p$g_for; glost<-p$g_against; swon<-p$s_for; slost<-p$s_against; bag<-p$bag_for }
  else           { gwon<-p$g_against; glost<-p$g_for; swon<-p$s_against; slost<-p$s_for; bag<-p$bag_against }
  data.frame(match_played = 1, games_won = gwon, games_lost = glost,
             sets_won = swon, sets_lost = slost, match_won = as.integer(is_winner),
             aces = ace, dfs = df, breaks = breaks, clean_set = bag,
             no_df = as.integer(df == 0), straight_sets = as.integer(is_winner && slost == 0))
}

# ── build the long player-match table with DK points + serve features ─────────
tennis_build_long <- function(matches) {
  M <- as.data.table(matches)
  g <- function(col) if (col %in% names(M)) M[[col]] else rep(NA_real_, nrow(M))
  M[, date := as.Date(as.character(g("tourney_date")), format = "%Y%m%d")]
  rows <- vector("list", nrow(M))
  for (i in seq_len(nrow(M))) {
    p <- parse_tennis_score(M$score[i]); if (is.null(p)) next
    w_breaks <- max(0, (g("l_bpFaced")[i] %||% 0) - (g("l_bpSaved")[i] %||% 0), na.rm = TRUE)
    l_breaks <- max(0, (g("w_bpFaced")[i] %||% 0) - (g("w_bpSaved")[i] %||% 0), na.rm = TRUE)
    bw <- .tennis_player_box(p, TRUE,  g("w_ace")[i] %||% 0, g("w_df")[i] %||% 0, w_breaks)
    bl <- .tennis_player_box(p, FALSE, g("l_ace")[i] %||% 0, g("l_df")[i] %||% 0, l_breaks)
    common <- list(date = M$date[i], match_id = i, tour = (g("tour")[i] %||% "atp"),
                   surface = (M$surface[i] %||% "Hard"), best_of = (g("best_of")[i] %||% 3))
    rows[[i]] <- rbindlist(list(
      as.data.table(c(common, list(player_id = M$winner_id[i], player_name = M$winner_name[i],
        opp_id = M$loser_id[i], won = 1L, dk = tennis_dk_scoring(bw),
        ace = bw$aces, df = bw$dfs, svpt = g("w_svpt")[i] %||% NA, svgms = g("w_SvGms")[i] %||% NA))),
      as.data.table(c(common, list(player_id = M$loser_id[i], player_name = M$loser_name[i],
        opp_id = M$winner_id[i], won = 0L, dk = tennis_dk_scoring(bl),
        ace = bl$aces, df = bl$dfs, svpt = g("l_svpt")[i] %||% NA, svgms = g("l_SvGms")[i] %||% NA)))))
  }
  L <- rbindlist(rows, fill = TRUE)
  L <- L[!is.na(player_id) & !is.na(date)]
  L[, surface := fifelse(surface %in% c("Hard","Clay","Grass","Carpet"), surface, "Hard")]
  # keep each match's two rows adjacent (winner first) and in chronological order,
  # because tennis_fit() processes rows in winner/loser pairs.
  setorder(L, date, match_id, -won)
  L[]
}

.elo_p <- function(diff) 1 / (1 + 10^(-diff / 400))

# ── single chronological pass: leak-free Elo + rolling DK -> per-row projection ─
tennis_fit <- function(L, k_overall = 32, k_surface = 24, shrink = 5) {
  L <- as.data.table(L); setorder(L, date)
  # league means/sds by (tour,best_of) for win/loss outcomes (hyperparams)
  lg <- L[, .(lw = mean(dk[won==1]), ll = mean(dk[won==0]),
              sw = sd(dk[won==1]), sl = sd(dk[won==0])), by = .(tour, best_of)]
  lg[is.na(sw) | sw<=0, sw := 8]; lg[is.na(sl) | sl<=0, sl := 6]
  lgk <- function(tr, bo, col) { v <- lg[tour==tr & best_of==bo][[col]]; if (length(v)) v[1] else lg[[col]][1] }

  eo <- new.env(parent = emptyenv())                       # overall elo
  es <- list(Hard=new.env(parent=emptyenv()), Clay=new.env(parent=emptyenv()),
             Grass=new.env(parent=emptyenv()), Carpet=new.env(parent=emptyenv()))
  rw <- new.env(parent = emptyenv())                       # rolling DK: id -> c(nw,sumw,nl,suml)
  ge <- function(env,id){ v<-get0(id,env); if(is.null(v)) 1500 else v }
  gr <- function(id){ v<-get0(id,rw); if(is.null(v)) c(0,0,0,0) else v }

  proj<-pwin<-simsd<-ceil<-floor<-pz<-numeric(nrow(L))
  # iterate by MATCH (two consecutive rows share a match: winner row + loser row)
  # build a match index: rows come in winner/loser pairs from build_long
  for (i in seq(1, nrow(L), by = 2)) {
    w <- i; l <- i + 1L; if (l > nrow(L)) break
    wid<-as.character(L$player_id[w]); lid<-as.character(L$player_id[l])
    surf<-L$surface[w]; bo<-L$best_of[w]; tr<-L$tour[w]
    eow<-ge(eo,wid); eol<-ge(eo,lid); esw<-ge(es[[surf]],wid); esl<-ge(es[[surf]],lid)
    for (j in c(w,l)) {
      me  <- if (j==w) wid else lid;  op <- if (j==w) lid else wid
      eo_m<-ge(eo,me); eo_o<-ge(eo,op); es_m<-ge(es[[surf]],me); es_o<-ge(es[[surf]],op)
      diff<-0.5*(eo_m-eo_o)+0.5*(es_m-es_o); p<-.elo_p(diff)
      r<-gr(me); pw<-(r[2]+shrink*lgk(tr,bo,"lw"))/(r[1]+shrink); pl<-(r[4]+shrink*lgk(tr,bo,"ll"))/(r[3]+shrink)
      sw<-lgk(tr,bo,"sw"); sl<-lgk(tr,bo,"sl")
      mu<-p*pw+(1-p)*pl
      var<-p*sw^2+(1-p)*sl^2+p*(1-p)*(pw-pl)^2
      proj[j]<-mu; pwin[j]<-p; simsd[j]<-sqrt(var)
      ceil[j]<-pw+0.5*sw; floor[j]<-max(pl-0.5*sl,0); pz[j]<-min(max((1-p)*0.15+0.03,0.02),0.4)
    }
    # updates (after recording, leak-free): Elo + rolling DK
    pexp<-.elo_p(0.5*(eow-eol)+0.5*(esw-esl))
    assign(wid, eow + k_overall*(1-pexp), eo); assign(lid, eol - k_overall*(1-pexp), eo)
    assign(wid, esw + k_surface*(1-pexp), es[[surf]]); assign(lid, esl - k_surface*(1-pexp), es[[surf]])
    rwd<-gr(wid); assign(wid, c(rwd[1]+1,rwd[2]+L$dk[w],rwd[3],rwd[4]), rw)
    rld<-gr(lid); assign(lid, c(rld[1],rld[2],rld[3]+1,rld[4]+L$dk[l]), rw)
  }
  L[, `:=`(proj=proj, p_win=pwin, sim_sd=simsd, ceil=ceil, floor=floor, p_zero=pz)]
  state <- list(eo=eo, es=es, rw=rw, lg=lg, shrink=shrink,
                k_overall=k_overall, k_surface=k_surface,
                trained_on=format(Sys.time(),"%Y-%m-%d"), n=nrow(L))
  list(long = L, state = state)
}

# project one player given current state, their opponent, surface, best_of
tennis_project_one <- function(state, pid, oppid, surface="Hard", best_of=3, tour="atp") {
  ge<-function(env,id){v<-get0(as.character(id),env); if(is.null(v)) 1500 else v}
  gr<-function(id){v<-get0(as.character(id),state$rw); if(is.null(v)) c(0,0,0,0) else v}
  surf <- if (surface %in% names(state$es)) surface else "Hard"
  diff<-0.5*(ge(state$eo,pid)-ge(state$eo,oppid))+0.5*(ge(state$es[[surf]],pid)-ge(state$es[[surf]],oppid))
  p<-.elo_p(diff)
  tr_<-tour; bo_<-best_of
  lg<-state$lg; row<-lg[tour==tr_ & best_of==bo_]; if(!nrow(row)) row<-lg[1]
  r<-gr(pid); sh<-state$shrink
  pw<-(r[2]+sh*row$lw)/(r[1]+sh); pl<-(r[4]+sh*row$ll)/(r[3]+sh)
  mu<-p*pw+(1-p)*pl; var<-p*row$sw^2+(1-p)*row$sl^2+p*(1-p)*(pw-pl)^2
  list(proj=mu, p_win=p, sim_sd=sqrt(var), ceil=pw+0.5*row$sw, floor=max(pl-0.5*row$sl,0),
       p_zero=min(max((1-p)*0.15+0.03,0.02),0.4))
}

# ── orchestrate: load cached matches -> build -> fit -> save state ────────────
tennis_train <- function(seasons = NULL, refresh = FALSE, tours = c("atp","wta")) {
  for (tr in tours) if (refresh || !file.exists(tennis_matches_path(tr))) tennis_ingest(tr, seasons)
  M <- rbindlist(lapply(tours, function(tr) {
    p <- tennis_matches_path(tr); if (file.exists(p)) as.data.table(readRDS(p)) else NULL
  }), fill = TRUE)
  if (!nrow(M)) stop("No tennis match data. Populate ", tennis_src_dir(), " or check network.")
  L <- tennis_build_long(M)
  fit <- tennis_fit(L)
  # name -> id map (most recent) for live DK-name matching
  nm <- L[order(date), .(player_id = player_id[.N]), by = .(norm = norm_name(player_name))]
  fit$state$name_map <- nm
  dir.create(dirname(TENNIS_MODEL_PATH()), recursive = TRUE, showWarnings = FALSE)
  saveRDS(fit$state, TENNIS_MODEL_PATH())
  msg("Saved tennis models ->", TENNIS_MODEL_PATH(), "(", fit$state$n, "player-matches )")
  invisible(fit)
}

tennis_load_models <- function() { p<-TENNIS_MODEL_PATH(); if (file.exists(p)) readRDS(p) else NULL }

# ── walk-forward accuracy backtest (no salaries needed) ──────────────────────
tennis_accuracy_backtest <- function(matches = NULL, holdout_year = NULL) {
  if (is.null(matches)) {
    matches <- rbindlist(lapply(c("atp","wta"), function(tr){ p<-tennis_matches_path(tr)
      if (file.exists(p)) as.data.table(readRDS(p)) else NULL }), fill = TRUE)
  }
  L <- tennis_build_long(matches)
  L[, yr := as.integer(format(date, "%Y"))]
  if (is.null(holdout_year)) holdout_year <- max(L$yr)
  fit <- tennis_fit(L)                                # leak-free proj for every row
  H <- fit$long[as.integer(format(date,"%Y")) == holdout_year & is.finite(proj)]
  H[, `:=`(total_pts = dk, base_proj = ave(dk, player_id, FUN = function(x) mean(x)))]  # crude baseline
  cat("=== TENNIS ACCURACY BACKTEST (holdout", holdout_year, ",", nrow(H), "player-matches) ===\n")
  cat(sprintf("RMSE %.2f  MAE %.2f  corr %.3f\n",
      sqrt(mean((H$proj-H$total_pts)^2)), mean(abs(H$proj-H$total_pts)), cor(H$proj,H$total_pts)))
  cat("win-prob calibration (pred vs actual win rate by decile):\n")
  H[, wb := cut(p_win, seq(0,1,0.2), include.lowest=TRUE)]
  print(H[, .(n=.N, pred=round(mean(p_win),3), actual=round(mean(won),3)), by=wb][order(wb)])
  invisible(H)
}
