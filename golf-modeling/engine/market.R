#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS v2 — engine/market.R  (Phase 3: market-implied skill blend)
#
# Our projection stays the engine; the betting market is one INPUT that calibrates
# the overall skill LEVEL (the market aggregates injuries, travel, insider form and
# course fit the bottoms-up model can miss). We:
#   1. de-vig the WIN market per event -> implied win prob per player,
#   2. map it to a market skill on the mu scale (log-prob z-score, rescaled to the
#      event's model-mu spread), and
#   3. blend  final_mu = (1-w)*adj_mu + w*market_mu,  w chosen by walk-forward.
#
# Historical odds: golf_picks/mclv_outright_odds.rds (win market, 2023-26, ~156
# events, wide X<ev>.<yr>.win.<field>). Live: get_live_market() pulls DataGolf
# pre-tournament win probs (preds/pre-tournament) -- same skill signal, no de-vig.
#
# Source: Sys.setenv(ENGINE_SOURCE_ONLY="1"); source("engine/market.R")
#   -> load_win_odds(), market_skill(), blend_market(), get_live_market()
# Run:    Rscript engine/market.R      (walk-forward: does the blend beat pure?)
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
Sys.setenv(ENGINE_SOURCE_ONLY = "1", SKILL_SOURCE_ONLY = "1", COURSEFIT_SOURCE_ONLY = "1")
if (!exists("build_master"))     source("engine/data.R")
if (!exists("train_skill"))      source("engine/skill.R")
if (!exists("train_coursefit"))  source("engine/coursefit.R")
emsg <- get0("emsg", ifnotfound = function(...) cat(..., "\n"))
OUT  <- "golf_picks"

# American -> decimal -> implied probability
.am_to_prob <- function(am) {
  am <- as.numeric(am)
  dec <- fifelse(am > 0, am / 100 + 1, 100 / abs(am) + 1)
  1 / dec
}

# ── reshape the wide win-odds cache to long + de-vig per event ─────────────────
load_win_odds <- function(path = file.path(OUT, "mclv_outright_odds.rds")) {
  if (!file.exists(path)) return(data.table())
  w <- as.data.table(readRDS(path)); cn <- names(w)
  keys <- unique(sub("\\.(player_id|open_am|close_am)$", "", cn))
  keys <- keys[grepl("\\.win$", keys)]
  long <- rbindlist(lapply(keys, function(k) {
    parts <- strsplit(sub("^X", "", k), ".", fixed = TRUE)[[1]]   # ev, yr, "win"
    data.table(event_id = parts[1], year = as.integer(parts[2]),
               player_id = suppressWarnings(as.integer(w[[paste0(k, ".player_id")]])),
               open_am   = suppressWarnings(as.numeric(w[[paste0(k, ".open_am")]])),
               close_am  = suppressWarnings(as.numeric(w[[paste0(k, ".close_am")]])))
  }), fill = TRUE)
  long <- long[!is.na(player_id) & (is.finite(open_am) | is.finite(close_am))]
  long[, am := fcoalesce(close_am, open_am)]
  long[, p_raw := .am_to_prob(am)]
  long <- long[is.finite(p_raw) & p_raw > 0]
  long[, p_win := p_raw / sum(p_raw), by = .(event_id, year)]   # de-vig (remove overround)
  long <- long[, .(event_id = as.character(event_id), year, player_id, p_win)]
  unique(long, by = c("event_id","year","player_id"))           # one row per player-event
}

# ── market skill on the mu scale ──────────────────────────────────────────────
# within each event: z-score of log(win prob) (spreads favourites sensibly),
# rescaled to the event's model-mu mean/sd so it lives on the projection scale.
market_skill <- function(D_mu, odds = NULL) {
  if (is.null(odds)) odds <- load_win_odds()
  D <- merge(as.data.table(copy(D_mu)), odds, by = c("event_id","year","player_id"), all.x = TRUE)
  D[, lp := log(pmax(p_win, 1e-6))]
  D[, mkt_z := { z <- (lp - mean(lp, na.rm=TRUE)) / sd(lp, na.rm=TRUE); z[!is.finite(z)] <- NA; z },
    by = .(event_id, year)]
  D[, `:=`(mu_m = mean(adj_mu, na.rm=TRUE), mu_s = sd(adj_mu, na.rm=TRUE)),
    by = .(event_id, year)]
  D[, market_mu := mu_m + mkt_z * mu_s]
  D[!is.finite(market_mu), market_mu := NA_real_]
  D[]
}

# blend; where market is missing, keep the model (w effectively 0 for that player)
blend_market <- function(D, w = 0.35) {
  D <- as.data.table(copy(D))
  D[, final_mu := fifelse(is.finite(market_mu), (1 - w) * adj_mu + w * market_mu, adj_mu)]
  D[]
}

# ── live market signal from DataGolf pre-tournament win probabilities ──────────
get_live_market <- function(tour = "pga") {
  key <- Sys.getenv("DATAGOLF_API_KEY"); if (!nzchar(key)) return(data.table())
  dg <- function(endpoint, params = list()) {
    params$key <- key; params$file_format <- "json"
    resp <- tryCatch(httr2::request(paste0("https://feeds.datagolf.com/", endpoint)) |>
                       httr2::req_url_query(!!!params) |> httr2::req_perform(),
                     error = function(e) NULL)
    if (is.null(resp) || httr2::resp_status(resp) != 200) return(NULL)
    httr2::resp_body_json(resp, simplifyVector = TRUE)
  }
  raw <- dg("preds/pre-tournament", list(tour = tour, odds_format = "percent"))
  if (is.null(raw) || is.null(raw$baseline_history_fit) && is.null(raw$baseline)) {
    b <- tryCatch(as.data.table(raw[[1]]), error = function(e) NULL)
  } else b <- as.data.table(raw$baseline)
  if (is.null(b) || !"dg_id" %in% names(b)) return(data.table())
  b[, `:=`(player_id = as.integer(dg_id),
           p_win = suppressWarnings(as.numeric(win)) )]
  b <- b[is.finite(p_win) & p_win > 0]
  b[, p_win := p_win / sum(p_win)]
  b[, .(player_id, p_win)]
}

# ── walk-forward: does the market blend beat pure model? ──────────────────────
if (identical(environment(), globalenv()) && !nzchar(Sys.getenv("MARKET_SOURCE_ONLY"))) {
  emsg("=== engine/market.R — walk-forward market-blend eval ===")
  M <- as.data.table(readRDS(file.path(OUT, "v2_master.rds"))$master)
  odds <- load_win_odds()
  emsg("win-odds events:", uniqueN(paste(odds$event_id, odds$year)),
       "| player-odds rows:", nrow(odds))
  rmse <- function(a,b) sqrt(mean((a-b)^2, na.rm=TRUE))
  co   <- function(a,b) suppressWarnings(cor(a,b, use="complete.obs"))
  ws   <- c(0, 0.15, 0.25, 0.35, 0.5, 0.65)

  for (Y in 2024:2026) {
    tr <- M[year < Y]; te <- M[year == Y & is.finite(t_sg)]
    if (nrow(tr) < 2000 || nrow(te) < 50) next
    sm  <- train_skill(tr); cfm <- train_coursefit(project_skill(sm, tr))
    tef <- project_coursefit(cfm, project_skill(sm, te))
    tem <- market_skill(tef, odds)
    cov <- mean(is.finite(tem$market_mu))
    line <- sprintf(" %d (mkt cov %.0f%%): base cor %.3f/rmse %.3f", Y, 100*cov,
                    co(tem$adj_mu, tem$t_sg), rmse(tem$adj_mu, tem$t_sg))
    best <- NULL
    for (w in ws) { b <- blend_market(tem, w)
      cc <- co(b$final_mu, b$t_sg); rr <- rmse(b$final_mu, b$t_sg)
      if (is.null(best) || cc > best$cc) best <- list(w=w, cc=cc, rr=rr) }
    cat(line, sprintf(" | best w=%.2f -> cor %.3f/rmse %.3f\n", best$w, best$cc, best$rr))
  }
  # also: pure market alone vs pure model (on covered rows) for 2026
  te <- M[year == 2026 & is.finite(t_sg)]
  sm <- train_skill(M[year < 2026]); cfm <- train_coursefit(project_skill(sm, M[year<2026]))
  tem <- market_skill(project_coursefit(cfm, project_skill(sm, te)), odds)[is.finite(market_mu)]
  cat(sprintf("\n2026 covered rows (%d): model cor %.3f | market-only cor %.3f\n",
      nrow(tem), co(tem$adj_mu, tem$t_sg), co(tem$market_mu, tem$t_sg)))
}
