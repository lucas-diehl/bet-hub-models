#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS v2 — engine/round_sim.R   (single-round support, e.g. Round 2)
#
# DK posts single-round "Round N PGA TOUR" classic slates (gameTypeId 85): one
# round of scoring, own salaries (split AM/PM wave), NO cut, NO finish bonus.
# This projects R2 fantasy points from the v2 per-round skill mu, FOLDING IN the
# completed R1 result (round-to-round SG persistence ~0.15), then sims one round
# (hole + bogey-free + streak) and optimizes the top-N lineups.
#
#   Rscript engine/round_sim.R --round 2 [--late]
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table); library(httr2) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
if (!nzchar(Sys.getenv("DATAGOLF_API_KEY")))
  for (p in c(".Renviron","../DFS ENGINE/.Renviron")) if (file.exists(p)) readRenviron(p)
Sys.setenv(ENGINE_SOURCE_ONLY="1", SKILL_SOURCE_ONLY="1", COURSEFIT_SOURCE_ONLY="1",
           MARKET_SOURCE_ONLY="1", SIMULATE_SOURCE_ONLY="1", PROJECT_SOURCE_ONLY="1")
if (!exists("project_mu")) source("engine/project.R")
if (!exists("wave_conditions")) source("engine/weather.R")
Sys.setenv(GOLF_DFS_SOURCE_ONLY="1"); if (!exists("make_candidates")) source("dfs_pipeline_v2.R")
emsg <- get0("emsg", ifnotfound=function(...) cat(..., "\n")); OUT <- "golf_picks"
KEY <- Sys.getenv("DATAGOLF_API_KEY")

.norm <- function(x) {                       # "Young, Cameron" | "Cameron Young" -> "cameron young"
  x <- tolower(trimws(gsub("[^A-Za-z, ]", "", x)))
  ifelse(grepl(",", x), sub("^(.*),\\s*(.*)$", "\\2 \\1", x), x)
}
gj <- function(url) tryCatch(request(url)|>req_timeout(30)|>req_user_agent("Mozilla/5.0")|>
  req_perform()|>resp_body_json(simplifyVector=TRUE), error=function(e) NULL)
dg <- function(ep, params=list()) { params$key<-KEY; params$file_format<-"json"
  gj(paste0("https://feeds.datagolf.com/", ep, "?",
            paste(names(params), unlist(params), sep="=", collapse="&"))) }

# DK single-round draft group for round R (Featured wave unless --late)
dk_round_group <- function(round=2, late=FALSE) {
  lob <- gj("https://www.draftkings.com/lobby/getcontests?sport=GOLF")
  dg2 <- as.data.table(lob$DraftGroups)
  tag <- sprintf("%sRound %d PGA TOUR)", if (late) "Late " else "(", round)
  hit <- dg2[GameTypeId==85 & grepl(sprintf("%sRound %d PGA TOUR", if(late)"Late " else "", round),
                                    ContestStartTimeSuffix)]
  if (!nrow(hit)) stop("no DK single-round group for round ", round)
  hit$DraftGroupId[1]
}
dk_round_salaries <- function(dgid) {
  d <- gj(sprintf("https://api.draftkings.com/draftgroups/v1/draftgroups/%d/draftables", dgid))
  dt <- unique(as.data.table(d$draftables)[, .(player_name=displayName, salary, dk_id=draftableId)],
               by="player_name")
  dt[, norm := .norm(player_name)]; dt[]
}

# completed-round per-category SG from DataGolf live-tournament-stats (for the
# category-aware fold). Returns data.table(norm, rs_ott/app/arg/putt) or NULL.
get_live_round_sg <- function(tour = "pga", round = 1L) {
  raw <- dg("preds/live-tournament-stats",
            list(tour = tour, round = round, display = "value",
                 stats = "sg_ott,sg_app,sg_arg,sg_putt"))
  d <- tryCatch(as.data.table(raw$live_stats %||% raw$data %||% raw[[1]]), error = function(e) NULL)
  if (is.null(d) || !nrow(d) || !"player_name" %in% names(d)) return(NULL)
  d[, norm := .norm(player_name)]
  keep <- c(ott="sg_ott", app="sg_app", arg="sg_arg", putt="sg_putt")
  if (!all(keep %in% names(d))) return(NULL)
  d[, .(norm, rs_ott = as.numeric(sg_ott), rs_app = as.numeric(sg_app),
        rs_arg = as.numeric(sg_arg), rs_putt = as.numeric(sg_putt))]
}

# ── single-round PROJECTION (no DK salary): v2 per-round mu, folded with completed
# round(s), per-tee-time weather, single-round sim. Returns Pm + event. Shared by the
# live board (round_projection) and the DFS-ENGINE export (export_round_projection).
.round_project_pm <- function(round=2, late=FALSE, n_sims=6000L) {
  bundle <- readRDS(file.path(OUT, "v2_bundle.rds"))
  master <- as.data.table(readRDS(file.path(OUT,"v2_master.rds"))$master)
  sl <- assemble_live_slate("pga", "main", master)
  pool <- as.data.table(sl$players)
  pool[, `:=`(event_id="live", year=as.integer(format(Sys.Date(),"%Y")), course_par=71L)]
  mkt <- tryCatch(get_dg_pretourn("pga","baseline_history_fit"), error=function(e) data.table())
  if (!nrow(mkt)) mkt <- tryCatch(get_live_market("pga"), error=function(e) data.table())
  sr  <- tryCatch(get_dg_skill_ratings(), error=function(e) data.table())
  Pm <- project_mu(bundle, pool, if (nrow(mkt)) mkt else NULL,
                   if (nrow(sr)) sr else NULL)                   # per-round SG mu (+DG skill)
  Pm[, norm := .norm(player_name)]

  # fold in the COMPLETED round(s): DataGolf in-play 'today' -> realised R-so-far SG.
  # Round 1 has nothing completed to fold (FRL is pure pre-round projection) -> skip.
  ip <- if (round >= 2L) dg("preds/in-play", list(tour="pga")) else NULL
  if (!is.null(ip$data)) {
    x <- as.data.table(ip$data); x[, norm := .norm(player_name)]
    fld <- mean(x$today, na.rm=TRUE)
    x[, r1_sg := (fld - today)]                        # beat field by strokes = +SG
    Pm <- merge(Pm, x[, .(norm, r1_sg, cur_thru=thru, current_pos)], by="norm", all.x=TRUE)
    # CATEGORY-AWARE fold: recent per-CATEGORY SG persists very differently
    # (OTT .20 > ARG .09 > APP .08 > PUTT .04) -> a hot putting round should count
    # ~5x less than hot ball-striking. If live per-category round SG is available we
    # fold it with those weights; else fall back to the validated 0.10 total fold.
    CAT_R1_W <- c(ott=0.20, app=0.08, arg=0.09, putt=0.04)
    lrs <- tryCatch(get_live_round_sg("pga", round - 1L), error=function(e) NULL)
    if (!is.null(lrs) && nrow(lrs)) {
      Pm <- merge(Pm, lrs, by="norm", all.x=TRUE)
      Pm[, cat_fold := CAT_R1_W["ott"]*fcoalesce(rs_ott,0) + CAT_R1_W["app"]*fcoalesce(rs_app,0) +
                       CAT_R1_W["arg"]*fcoalesce(rs_arg,0) + CAT_R1_W["putt"]*fcoalesce(rs_putt,0)]
      Pm[is.finite(cat_fold), mu := mu + cat_fold]
      emsg("folded completed round(s) BY CATEGORY (putting down-weighted)")
    } else {
      Pm[is.finite(r1_sg), mu := 0.90*mu + 0.10*r1_sg]      # fallback: total fold
    }
    emsg("folded R1 form for ", sum(is.finite(Pm$r1_sg)), " players (field avg today ",
         round(fld,1), ", R1 weight 0.10)")
  }

  # live weather: per-wave wind -> SG shift + variance (no-op if feed/forecast missing)
  wc <- tryCatch(wave_conditions("pga", round), error=function(e) NULL)
  if (!is.null(wc) && nrow(wc)) {
    Pm <- merge(Pm, wc[, .(player_id, wave_shift, vol_mult)], by="player_id", all.x=TRUE)
    Pm[is.finite(wave_shift), mu := mu + wave_shift]
    Pm[is.finite(vol_mult),   round_sd := round_sd * vol_mult]
  }
  # single-round DK points sim (no cut, no finish): hole + bogey_free + streak.
  # round_sd scaled by the CALIBRATED single-round spread (walk-forward vs realized
  # round matchups: optimum 0.90, near-perfect reliability; NOT over-confident).
  cal <- bundle$sim_cal; P <- nrow(Pm); S <- n_sims
  rss <- tryCatch(readRDS(file.path(OUT,"v2_round_spread_scale.rds"))$round_spread_scale, error=function(e) 0.90)
  if (!is.finite(rss) || rss <= 0) rss <- 0.90
  SG <- matrix(rnorm(P*S, Pm$mu, pmax(Pm$round_sd,0.4)*rss), P, S) +
        rep(rnorm(S,0,0.30), each=P)                   # wave/conditions common shift
  rp <- .round_points(SG, cal, 71)
  dk <- rp$dk + 5*matrix(rbinom(P*S,1,0.002),P,S)      # rare hole-in-one
  q <- function(m,p) apply(m,1L,stats::quantile,probs=p,names=FALSE)
  Pm[, `:=`(proj=rowMeans(dk), ceil=q(dk,0.90), floor=pmax(q(dk,0.10),0),
            sim_sd=apply(dk,1L,sd))]
  # FRL / round-1 selector: P(this golfer posts the LOW round of the field) from the sim
  if (round == 1L) { cm <- apply(SG, 2L, max); Pm[, p_frl := rowMeans(SG == matrix(cm, P, S, byrow=TRUE))] }
  # round 1 has no completed-round fold -> ensure the columns the merge/print expect exist
  if (!"r1_sg" %in% names(Pm))       Pm[, r1_sg := NA_real_]
  if (!"current_pos" %in% names(Pm)) Pm[, current_pos := NA_character_]
  if (!"p_frl" %in% names(Pm))       Pm[, p_frl := NA_real_]
  if (!"own"   %in% names(Pm))       Pm[, own := 0.001]
  list(Pm = Pm, event = sl$event, round = round)
}

# ── live single-round board: projection + DK single-round salaries + optimize ──
round_projection <- function(round=2, late=FALSE, n_sims=6000L) {
  pm <- .round_project_pm(round, late, n_sims); Pm <- pm$Pm
  # attach the DK single-round salaries by name
  dgid <- dk_round_group(round, late); emsg("DK round group ", dgid)
  sal <- dk_round_salaries(dgid)
  R <- merge(sal, Pm[, .(norm, player_name_dg=player_name, proj, ceil, floor, sim_sd,
                         mu, r1_sg, current_pos, p_frl)], by="norm", all.x=TRUE)
  R <- R[is.finite(proj) & is.finite(salary) & salary>0]
  R[, player_id := dk_id]                              # unique id for the optimizer
  emsg("R", round, " field matched: ", nrow(R), " of ", nrow(sal), " DK players")
  list(proj=R, event=pm$event, round=round)
}

# ── DFS-ENGINE bridge: export the single-round PROJECTION (no salary) in the adapter
# schema (dg_id, player_name, proj/ceil/floor/sim_sd/own [+p_frl]) so the spine's
# single-round exposure build consumes the SAME per-tee-time-wind engine. Mirrors the
# full-tournament engine/export.R -> golf_picks/dfs_projections.rds bridge.
export_round_projection <- function(round=2, late=FALSE, n_sims=6000L) {
  pm <- .round_project_pm(round, late, n_sims); Pm <- pm$Pm
  out <- Pm[is.finite(proj), .(
    dg_id = as.integer(player_id), player_name,
    proj  = round(proj, 2), ceil = round(ceil, 2), floor = round(pmax(floor, 0), 2),
    sim_sd = round(pmax(sim_sd, 4), 3), own = round(fcoalesce(as.numeric(own), 0.001), 4),
    p_frl = round(as.numeric(p_frl), 4))]
  meta <- list(event = pm$event, round = round, generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
               date = as.character(Sys.Date()), n = nrow(out), engine = "v2-round")
  saveRDS(list(projections = out, meta = meta), file.path(OUT, "dfs_round_projections.rds"))
  emsg(sprintf("wrote dfs_round_projections.rds: %s R%d -- %d players (engine v2-round)",
               pm$event, round, nrow(out)))
  invisible(out)
}

if (identical(environment(), globalenv())) {
  a <- commandArgs(trailingOnly=TRUE)
  rnd <- { i<-which(a=="--round"); if(length(i)) as.integer(a[i+1]) else 2L }
  late <- "--late" %in% a
  mx  <- { i<-which(a=="--maxexp"); if(length(i)) as.numeric(a[i+1]) else 0.6 }   # portfolio max exposure
  if ("--export" %in% a) {   # DFS-ENGINE bridge: write the projection file + exit (no DK salary/optimize)
    export_round_projection(rnd, late); quit(save="no", status=0)
  }
  rp <- round_projection(rnd, late)
  R <- rp$proj
  cat(sprintf("\n########## %s — ROUND %d single-round DK slate ##########\n", rp$event, rnd))
  cat("\n-- top projected (single round) --\n")
  if (rnd == 1L)   # Round 1 = FRL board: rank by P(low round of the field)
    print(head(R[order(-p_frl), .(Player=player_name, Sal=salary, Proj=round(proj,1),
      Ceil=round(ceil,1), Floor=round(floor,1), FRL_pct=round(100*p_frl,1))], 15), row.names=FALSE)
  else
    print(head(R[order(-proj), .(Player=player_name, Sal=salary, Proj=round(proj,1),
      Ceil=round(ceil,1), Floor=round(floor,1), R1=round(r1_sg,1), Pos=current_pos)], 15), row.names=FALSE)
  # optimize top-10 (reuse the full-tournament optimizer path)
  if (!exists("top_lineups")) { Sys.setenv(EXPORT_SOURCE_ONLY=""); source("engine/live_lineups.R") }
  top_lineups(R, sprintf("%s — ROUND %d", rp$event, rnd), 10, max_exposure=mx)
}
