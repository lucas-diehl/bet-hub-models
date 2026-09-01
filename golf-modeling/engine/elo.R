#!/usr/bin/env Rscript
# ==============================================================================
# GOLF DFS v2 — engine/elo.R   Personalized multiplayer golf Elo (leakage-free)
#
# A chronological, per-player skill rating updated after every event. Each event
# is scored as all-pairs comparisons on realised event SG (cleaner than finish:
# ignores cut/WD noise): a player gains rating for beating higher-rated players.
# The rating ENTERING an event (elo_pre) is the leakage-free feature -- it uses
# only completed prior events. New players seed at BASE; ratings regress toward
# BASE between seasons (staleness/inactivity).
#
#   get_elo(K, base, regress) -> data.table(player_id, event_id, year, elo_pre)
#
# Source: Sys.setenv(ENGINE_SOURCE_ONLY="1"); source("engine/elo.R")
# Run:    Rscript engine/elo.R   (build + validate elo predicts next-event SG)
# ==============================================================================
suppressWarnings(suppressPackageStartupMessages({ library(data.table) }))
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/golf-modeling"))
  setwd("c:/Users/ljdie/OneDrive/Documents/golf-modeling")
emsg <- get0("emsg", ifnotfound=function(...) cat(..., "\n")); OUT <- "golf_picks"

get_elo <- function(K = 22, base = 1500, regress = 0.15) {
  long <- as.data.table(readRDS(file.path(OUT, "rich_rounds_long.rds")))
  agg <- long[!is.na(sg_total), .(t_sg = mean(sg_total), date = min(event_date)),
              by = .(player_id = as.integer(player_id),
                     event_id = as.character(event_id), year = as.integer(year))]
  setorder(agg, date, event_id)
  ev <- unique(agg[, .(event_id, year, date)])[order(date, event_id)]
  elo <- new.env(parent = emptyenv())           # player_id -> current rating
  getR <- function(id) { v <- mget(as.character(id), elo, ifnotfound = NA_real_)
    r <- as.numeric(unlist(v)); r[is.na(r)] <- base; r }
  out <- vector("list", nrow(ev)); prev_year <- NA_integer_
  for (i in seq_len(nrow(ev))) {
    pl <- agg[event_id == ev$event_id[i] & year == ev$year[i]]
    N <- nrow(pl); if (N < 3) next
    # season-change regression toward the mean (once per new year, applied lazily)
    if (!is.na(prev_year) && ev$year[i] != prev_year) {
      ids <- ls(elo); if (length(ids)) { r <- getR(ids)
        for (j in seq_along(ids)) assign(ids[j], base + (1 - regress) * (r[j] - base), elo) }
    }
    prev_year <- ev$year[i]
    Ri <- getR(pl$player_id)
    out[[i]] <- data.table(player_id = pl$player_id, event_id = ev$event_id[i],
                           year = ev$year[i], elo_pre = Ri)            # leakage-free feature
    # pairwise expected vs actual on event SG
    E <- 1 / (1 + 10 ^ (outer(Ri, Ri, function(a, b) b - a) / 400)); diag(E) <- 0
    expected <- rowSums(E) / (N - 1)                                   # expected fraction beaten
    actual   <- (frank(pl$t_sg, ties.method = "average") - 1) / (N - 1)
    newR <- Ri + K * (actual - expected)
    for (j in seq_len(N)) assign(as.character(pl$player_id[j]), newR[j], elo)
  }
  rbindlist(out)
}

if (identical(environment(), globalenv()) && !nzchar(Sys.getenv("ELO_SOURCE_ONLY"))) {
  emsg("=== engine/elo.R — build + validate personalized golf Elo ===")
  E <- get_elo()
  emsg("elo rows: ", nrow(E), " | range ", paste(round(range(E$elo_pre)), collapse="-"))
  saveRDS(E, file.path(OUT, "v2_elo.rds"))
  # validation: does elo_pre predict next-event SG? (join to realised t_sg)
  M <- as.data.table(readRDS(file.path(OUT, "v2_master.rds"))$master)
  D <- merge(M[, .(player_id, event_id, year, t_sg, sg_last_24, career_sg)], E,
             by = c("player_id","event_id","year"))
  te <- D[year >= 2025 & is.finite(t_sg)]
  co <- function(a,b) suppressWarnings(cor(a,b,use="complete.obs"))
  cat(sprintf("\nOOS (2025-26) cor with realised event SG:\n"))
  cat(sprintf("  elo_pre        %.3f\n", co(te$elo_pre, te$t_sg)))
  cat(sprintf("  sg_last_24     %.3f\n", co(te$sg_last_24, te$t_sg)))
  cat(sprintf("  career_sg      %.3f\n", co(te$career_sg, te$t_sg)))
  # does elo ADD beyond the existing skill features?
  tr <- D[year < 2025]
  b0 <- lm(t_sg ~ sg_last_24 + career_sg, tr); b1 <- lm(t_sg ~ sg_last_24 + career_sg + elo_pre, tr)
  rmse <- function(m) sqrt(mean((te$t_sg - predict(m, te))^2, na.rm=TRUE))
  cat(sprintf("\n  RMSE  base(sg+career) %.3f  ->  +elo %.3f  (delta %+.4f)\n",
      rmse(b0), rmse(b1), rmse(b1)-rmse(b0)))
  cat(sprintf("  elo coef in joint model: %+.5f (p=%.4f)\n",
      coef(summary(b1))["elo_pre","Estimate"], coef(summary(b1))["elo_pre","Pr(>|t|)"]))
  emsg("saved v2_elo.rds")
}
