#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — simulator/engine parity: fixture + R reference.
# Emits a golden sim payload (same shape build_sim_payload() produces: INTEGER
# draws P×k, INTEGER field grid k×Q, real payout multipliers, players, lineups)
# and the R-computed metrics for each lineup. tests/parity/parity.js then runs the
# REAL JS scorer extracted from spine/assets/dashboard.html on the same fixture and
# asserts agreement — so the in-browser math can't silently drift from the engine.
# Run (from dfs-engine/):  Rscript tests/parity/gen_fixture.R
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(dirname(normalizePath(sub("^--file=", "", m[1]))))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine()
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
set.seed(20260904L)

P <- 14L; k <- 300L; fs <- 2000L
qlev <- SIM_QLEVELS; Q <- length(qlev)
proj <- round(runif(P, 6, 42), 1)
own  <- round(runif(P, 0.02, 0.45), 3)
sd_i <- runif(P, 4, 9)
draws <- lapply(seq_len(P), function(i) as.integer(round(rnorm(k, proj[i], sd_i[i]))))   # P × k ints
base  <- rnorm(k, mean = 6 * mean(proj), sd = 15)                                        # per-sim field center
fsd   <- runif(k, 18, 30)
fgrid <- lapply(seq_len(k), function(s) as.integer(round(base[s] + fsd[s] * qnorm(qlev)))) # k × Q ints, increasing
gpp_mult <- round(payout_multipliers(make_gpp(), fs), 3)
lineups1 <- list(as.integer(c(1,2,3,4,5,6)), as.integer(c(1,3,5,7,9,11)),
                 as.integer(c(8,9,10,11,12,13)), as.integer(c(2,4,6,8,10,14)))

# ── R mirror of the JS scorer (must match dashboard.html gridAt/pctBeaten/simMetrics) ──
js_round <- function(x) floor(x + 0.5)                        # JS Math.round (half-up), not R's banker's
gridAt <- function(g, q, level) { n <- length(q)
  if (level <= q[1]) return(g[1]); if (level >= q[n]) return(g[n])
  for (j in 1:(n-1)) if (level <= q[j+1]) { f <- (level-q[j])/(q[j+1]-q[j]); return(g[j] + (g[j+1]-g[j])*f) }
  g[n] }
pctBeaten <- function(tot, g, q) { n <- length(g)
  if (tot <= g[1]) p <- q[1] * max(0, min(1, tot / max(g[1], 1)))
  else if (tot >= g[n]) p <- q[n] + (1-q[n]) * min(1, (tot-g[n]) / max(1, g[n]-g[n-1]))
  else { p <- q[n]; for (j in 1:(n-1)) if (tot <= g[j+1]) { f <- (tot-g[j])/max(1e-9, g[j+1]-g[j]); p <- q[j] + (q[j+1]-q[j])*f; break } }
  max(0, min(0.9999, p)) }
r_metrics <- function(sel) {
  tot <- Reduce(`+`, lapply(sel, function(i) draws[[i]])); M <- gpp_mult
  cash<-0; top1<-0; win<-0; finsum<-0; gpp<-0; gpp2<-0
  for (s in 1:k) { g <- fgrid[[s]]
    pct <- pctBeaten(tot[s], g, qlev); finsum <- finsum + pct
    if (tot[s] >= gridAt(g, qlev, 0.5)) cash <- cash + 1
    if (pct >= 0.99) top1 <- top1 + 1
    if (pct >= 1 - 1/fs) win <- win + 1
    rank <- js_round((1-pct)*fs); if (rank < 1) rank <- 1; if (rank > fs) rank <- fs
    mult <- if (rank <= length(M)) M[rank] else 0; if (is.na(mult)) mult <- 0
    gpp <- gpp + mult; gpp2 <- gpp2 + mult*mult }
  list(proj = sum(proj[sel]), cash = cash/k, top1 = top1/k, win = win/k, fin = finsum/k,
       gppRoi = gpp/k - 1, roiStd = sqrt(max(gpp2/k - (gpp/k)^2, 0)),
       dupe = prod(pmax(own[sel], 0.004))^(1/length(sel)), own = sum(own[sel])) }
expected <- lapply(lineups1, r_metrics)

fixture <- list(k = k, field_size = fs, qlevels = qlev,
  players = lapply(seq_len(P), function(i) list(own = own[i], proj = proj[i])),
  draws = draws, fgrid = fgrid, gpp_mult = gpp_mult,
  lineups = lapply(lineups1, function(v) as.integer(v - 1L)))          # 0-based for JS
outdir <- file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
if (!dir.exists(outdir)) outdir <- "tests/parity"
writeLines(toJSON(fixture,  auto_unbox = TRUE, digits = 6),  file.path(outdir, "_fixture.json"))
writeLines(toJSON(expected, auto_unbox = TRUE, digits = 10), file.path(outdir, "_expected.json"))
cat(sprintf("wrote fixture (%d players, %d sims, fs=%d) + %d expected lineups -> %s\n",
            P, k, fs, length(expected), outdir))
