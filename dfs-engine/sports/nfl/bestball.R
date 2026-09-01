# ==============================================================================
# NFL plugin — season-long BEST-BALL draft tool
# Best-ball edge is ADP VALUE + STACKING against a soft field, not out-projecting the
# industry. So we combine two free signals:
#   1. MARKET  — best-ball consensus ADP (FantasyPros via DynastyProcess, current).
#   2. TALENT  — prior-season DK production per game (proven role/usage from nflverse).
# The value board flags proven producers being drafted CHEAP (our talent rank >> ADP
# rank) and helps build correlated stacks (same-team pass-catchers). It is honest about
# what it is NOT: a forward projection (rookies/situation changes need more than this).
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

BB_ADP_URL <- function() Sys.getenv("BB_ADP_URL",
  "https://raw.githubusercontent.com/dynastyprocess/data/master/files/db_fpecr_latest.csv")

# Current best-ball consensus ADP (lower = drafted earlier). Returns player/pos/team/adp/bye.
bb_adp <- function(format = "best-overall") {
  d <- tryCatch(as.data.table(fread(BB_ADP_URL(), showProgress = FALSE)), error = function(e) NULL)
  if (is.null(d) || !"page_type" %in% names(d)) { msg("  best-ball ADP unreachable"); return(NULL) }
  a <- d[page_type == format & pos %in% c("QB", "RB", "WR", "TE"),
         .(player, pos, team = tm, adp = ecr, adp_sd = sd, bye)]
  a[, norm := norm_name(player)]
  unique(a[order(adp)], by = "norm")
}

BB_ROSTER_URL <- function(year) sprintf(
  "https://github.com/nflverse/nflverse-data/releases/download/rosters/roster_%d.csv", year)

# Age + recent AVAILABILITY (injury-proneness) per player. Best-ball punishes missed
# games (you need weekly scores all season), and ADP already discounts age/fragility —
# so these explain why a "cheap vs production" player is actually cheap.
bb_player_meta <- function() {
  yr <- as.integer(format(Sys.Date(), "%Y"))
  ros <- NULL
  for (y in c(yr, yr - 1L)) { ros <- tryCatch(as.data.table(fread(BB_ROSTER_URL(y), showProgress = FALSE)), error = function(e) NULL)
    if (!is.null(ros) && "birth_date" %in% names(ros)) break }
  meta <- if (!is.null(ros) && "birth_date" %in% names(ros)) {
    ros <- ros[position %in% c("QB", "RB", "WR", "TE") & !is.na(birth_date)]
    ros[, age := as.numeric(difftime(as.Date(sprintf("%d-09-01", yr)), as.Date(birth_date), units = "days")) / 365.25]
    ros[, norm := norm_name(full_name)]
    unique(ros[order(-season)], by = "norm")[, .(norm, age = round(age, 1), exp = years_exp)]
  } else data.table(norm = character(0), age = numeric(0), exp = integer(0))
  # availability from the last 2 seasons of weekly data (games played / possible)
  W <- as.data.table(readRDS(nfl_weekly_path()))
  last2 <- W[season >= max(season) - 1L & season_type == "REG"]
  av <- last2[, .(gp = .N, ns = uniqueN(season)), by = .(norm = norm_name(player_display_name))]
  av[, avail := pmin(gp / (17 * ns), 1)]
  merge(meta, av[, .(norm, avail = round(avail, 2))], by = "norm", all = TRUE)
}

# Prior-season DK production per player (proven role/usage) from the nflverse store.
bb_prior_production <- function(min_games = 4L) {
  if (!file.exists(nfl_weekly_path())) nfl_ingest()
  W <- as.data.table(readRDS(nfl_weekly_path()))
  ly <- max(W$season)
  s <- W[season == ly & season_type == "REG",
         .(g = .N, dk_pg = mean(dk_pts), dk_total = sum(dk_pts)), by = .(player = player_display_name, pos = position)]
  s[, norm := norm_name(player)][g >= min_games][]
}

# Best-ball VALUE BOARD: join ADP (market) + prior production (talent). Value = how much
# cheaper the market drafts a player than their proven production rank implies.
bb_value_board <- function() {
  adp <- bb_adp(); prod <- bb_prior_production(); meta <- bb_player_meta()
  if (is.null(adp)) return(NULL)
  b <- merge(adp, prod[, .(norm, g, dk_pg)], by = "norm", all.x = TRUE)
  b <- merge(b, meta[, .(norm, age, avail)], by = "norm", all.x = TRUE)
  # position ranks: by ADP (market) and by prior per-game production (talent proxy)
  b[, adp_pos_rank := frank(adp, ties.method = "first"), by = pos]
  b[!is.na(dk_pg), prod_pos_rank := frank(-dk_pg, ties.method = "first"), by = pos]
  b[, value := adp_pos_rank - prod_pos_rank]                 # + => market under-drafts proven prod
  # AGE + INJURY are why a "cheap vs production" player is often cheap for a REASON.
  # Position-specific age cliffs; availability < 0.75 over the last 2 seasons = fragile.
  b[, aged := (pos == "RB" & age >= 27) | (pos %in% c("WR", "TE") & age >= 30) | (pos == "QB" & age >= 34)]
  b[is.na(aged), aged := FALSE]
  b[, fragile := !is.na(avail) & avail < 0.75]
  b[, risk := trimws(paste(fifelse(aged, "OLD", ""), fifelse(fragile, "INJURY", "")))]
  # true VALUE only in the draftable range, with a real sample, AND not explained by age/injury.
  draftable <- b$adp <= 150 & !is.na(b$g) & b$g >= 8
  b[, status := fifelse(is.na(dk_pg), "rookie/newcomer (no prior data — judge separately)",
                fifelse(draftable & value >= 6 & !aged & !fragile, "VALUE (cheap, durable, not aged)",
                fifelse(draftable & value >= 6 & (aged | fragile), "market discount (age/injury — likely justified)",
                fifelse(draftable & value <= -6, "REACH (pricey vs production)", "fair"))))]
  setorder(b, adp)
  b[, .(adp = round(adp, 1), player, pos, team, bye, age, avail, prior_g = g,
        prior_dk_pg = round(dk_pg, 1), value, risk, status)]
}

# Same-team pass-catchers for a QB/team — the correlated best-ball STACK the field underuses.
bb_stack_targets <- function(tm) {
  b <- bb_value_board(); if (is.null(b)) return(NULL)
  b[team == toupper(tm) & pos %in% c("QB", "WR", "TE")][order(adp)]
}

# Write the board to a CSV you can draft from.
bb_export <- function(path = NULL) {
  b <- bb_value_board(); if (is.null(b)) return(invisible(NULL))
  if (is.null(path)) path <- dfs_path("data", "reports", sprintf("bestball_board_%s.csv", Sys.Date()))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  fwrite(b, path); msg("Best-ball board ->", path); invisible(path)
}

# ==============================================================================
# BEST-BALL v2 — FORWARD projection + SEASON SIMULATOR (advance rates)
# Best ball's objective isn't weekly EV, it's finishing top-N of your draft pool over the
# season, and you keep each week's BEST lineup — so CEILING/spikes matter far more than the
# mean. v2 (1) builds a forward per-player weekly distribution (market ADP-implied role +
# proven recent production + age + volatility + boom + games), then (2) simulates whole
# seasons for a roster vs an ADP-drafted field to get its ADVANCE and WIN rate. Honest
# limits: no 2026 depth-chart/scheme modeling, so rookies/role-changes lean on the ADP
# anchor (the market's role expectation).
# ==============================================================================

# Forward per-player weekly projection for the upcoming season.
bb_forward_projection <- function() {
  if (!file.exists(nfl_weekly_path())) nfl_ingest()
  W <- as.data.table(readRDS(nfl_weekly_path())); adp <- bb_adp(); if (is.null(adp)) return(NULL)
  meta <- bb_player_meta(); ly <- max(W$season); reg <- W[season_type == "REG"]

  # recent per-player weekly stats (last 2 seasons, latest weighted 2x): mean, weekly sd, boom
  rec <- reg[season >= ly - 1L]; rec[, wt := fifelse(season == ly, 2, 1)]
  pp <- rec[, { m <- sum(dk_pts * wt) / sum(wt)
    .(g = .N, mu_prod = m, sd_prod = sqrt(sum(wt * (dk_pts - m)^2) / sum(wt)),
      boom_prod = weighted.mean(dk_pts >= 20, wt)) },
    by = .(norm = norm_name(player_display_name), pos = position)]

  # ADP-implied anchor: last season's actual per-game by production rank, mapped onto this
  # season's ADP position rank (the market's expected role at each slot). Handles rookies.
  fin <- reg[season == ly, .(dk_pg = mean(dk_pts), g = .N),
             by = .(norm = norm_name(player_display_name), pos = position)][g >= 4]
  fin[, rk := frank(-dk_pg, ties.method = "first"), by = pos]
  repl <- fin[, .(repl = quantile(dk_pg, 0.1)), by = pos]                  # replacement level per pos

  b <- copy(adp); b[, adp_pos_rank := frank(adp, ties.method = "first"), by = pos]
  b <- merge(b, fin[, .(pos, rk, adp_implied = dk_pg)],
             by.x = c("pos", "adp_pos_rank"), by.y = c("pos", "rk"), all.x = TRUE)
  b <- merge(b, repl, by = "pos", all.x = TRUE)
  b[is.na(adp_implied), adp_implied := repl]                               # beyond ranked list -> replacement
  b <- merge(b, pp, by = c("norm", "pos"), all.x = TRUE)
  b <- merge(b, meta[, .(norm, age, avail)], by = "norm", all.x = TRUE)

  b[, has_prod := !is.na(mu_prod) & g >= 6]
  b[, proj_pg := fifelse(has_prod, 0.55 * mu_prod + 0.45 * adp_implied, adp_implied)]
  # light age curve (best ball punishes decline; ADP already prices most of it)
  b[, proj_pg := proj_pg * fifelse(pos == "RB" & !is.na(age) & age >= 28, 0.90,
                          fifelse(pos %in% c("WR", "TE") & !is.na(age) & age >= 31, 0.93, 1))]
  b[, sd_wk := fifelse(has_prod, pmax(sd_prod, 3), 0.75 * proj_pg + 3)]    # rookie volatility heuristic
  b[, boom  := fifelse(has_prod, boom_prod, pmin(0.35, proj_pg / 42))]
  b[, p_play := fifelse(is.na(avail), 0.90, pmax(0.55, avail))]
  b[order(adp), .(norm, player, pos, team, adp, adp_sd = pmax(adp_sd, 3), bye = as.integer(bye),
                  age, proj_pg = round(proj_pg, 1), sd_wk = round(sd_wk, 1),
                  boom = round(boom, 2), p_play = round(p_play, 2), has_prod)]
}

# right-skewed weekly score (gamma) with target mean/sd -> captures best-ball ceilings
.bb_rgamma <- function(mu, sd, n) {
  mu <- pmax(mu, 0.5); sd <- pmax(sd, 0.5); shape <- (mu / sd)^2; rate <- mu / sd^2
  rgamma(n, shape = shape, rate = rate)
}

# best-ball weekly lineup total per sim (columns), from a players x sims score matrix
.bb_best_lineup <- function(sc, pos, lineup) {
  ns <- ncol(sc); contrib <- numeric(ns); flex <- list()
  for (p in setdiff(names(lineup), "FLEX")) {
    rows <- which(pos == p); if (!length(rows)) next
    S <- sc[rows, , drop = FALSE]
    srt <- matrix(if (nrow(S) == 1) S else apply(S, 2, sort, decreasing = TRUE), ncol = ns)
    k <- min(lineup[[p]], nrow(srt)); contrib <- contrib + colSums(srt[seq_len(k), , drop = FALSE])
    if (p %in% c("RB", "WR", "TE") && nrow(srt) > lineup[[p]]) flex[[length(flex) + 1]] <- srt[lineup[[p]] + 1, ]
  }
  if (!is.null(lineup[["FLEX"]]) && length(flex)) contrib <- contrib + do.call(pmax, flex)
  contrib
}

# one roster's regular-season point total distribution (n_sims draws).
# TWO variance sources (both needed for realistic best-ball advance rates):
#  - SEASON level: each player's true 2026 level is uncertain (breakout/bust), drawn ONCE
#    per sim and applied to all their weeks (correlates a player's season + is the dominant
#    driver of roster spread). Plus a season-ending INJURY risk that zeroes the rest of the year.
#  - WEEK level: game-to-game noise around that season level (right-skewed -> ceilings).
.bb_sim_roster <- function(P, n_sims, weeks, lineup, season_sigma = 0.32) {
  np <- nrow(P); total <- numeric(n_sims); bye <- P$bye
  smul <- matrix(rlnorm(np * n_sims, -0.5 * season_sigma^2, season_sigma), np, n_sims)  # season level
  # season-ending injury week per player-sim (Inf = healthy all year); risk from availability
  ir <- pmin(pmax(1 - P$p_play, 0.02), 0.5)
  hurt_wk <- matrix(ifelse(runif(np * n_sims) < ir,
                           sample.int(weeks, np * n_sims, replace = TRUE), weeks + 1L), np, n_sims)
  for (w in seq_len(weeks)) {
    played <- matrix(runif(np * n_sims) < P$p_play, np, n_sims) & (hurt_wk >= w)
    onbye <- which(!is.na(bye) & bye == w); if (length(onbye)) played[onbye, ] <- FALSE
    sc <- matrix(.bb_rgamma(P$proj_pg, P$sd_wk, np * n_sims), np, n_sims) * smul * played
    total <- total + .bb_best_lineup(sc, P$pos, lineup)
  }
  total
}

# draft `n_teams` opponent rosters from ADP (snake draft with per-pick noise + position caps)
bb_field_rosters <- function(P, n_teams, roster_size, caps = c(QB = 3, RB = 7, WR = 8, TE = 3)) {
  taken <- rep(FALSE, nrow(P)); rost <- replicate(n_teams, integer(0), simplify = FALSE)
  cnt <- replicate(n_teams, c(QB = 0, RB = 0, WR = 0, TE = 0), simplify = FALSE)
  for (rnd in seq_len(roster_size)) {
    ord <- if (rnd %% 2) seq_len(n_teams) else rev(seq_len(n_teams))
    for (tm in ord) {
      av <- which(!taken); if (!length(av)) next
      ok <- cnt[[tm]][P$pos[av]] < caps[P$pos[av]]; av <- av[ok]; if (!length(av)) next
      score <- P$adp[av] + rnorm(length(av), 0, P$adp_sd[av])
      pick <- av[which.min(score)]; taken[pick] <- TRUE
      rost[[tm]] <- c(rost[[tm]], pick); cnt[[tm]][P$pos[pick]] <- cnt[[tm]][P$pos[pick]] + 1L
    }
  }
  rost
}

# SEASON SIMULATOR: a roster's ADVANCE + WIN rate vs an ADP-drafted field of pools.
#   bb_season_sim(c("Ja'Marr Chase","Bijan Robinson", ... 18 names ...))
bb_season_sim <- function(my_players, proj = bb_forward_projection(), n_sims = 1500L,
                          weeks = 14L, n_teams = 12L, advance = 2L, roster_size = 18L,
                          lineup = c(QB = 1, RB = 2, WR = 3, TE = 1, FLEX = 1)) {
  if (is.null(proj)) { msg("no forward projection (ADP unreachable)"); return(NULL) }
  P <- as.data.table(proj); need <- sum(lineup)
  mine <- P[match(norm_name(my_players), norm)]
  unmatched <- my_players[is.na(mine$norm)]
  mine <- mine[!is.na(norm)]
  if (nrow(mine) < need) { msg("roster too small/unmatched — need >=", need, "known players"); return(NULL) }
  # opponents draft from the pool MINUS my roster (my players are taken) -> a realistic pool
  oppP <- P[!(norm %in% mine$norm)]
  field <- bb_field_rosters(oppP, n_teams - 1L, roster_size)
  my_tot  <- .bb_sim_roster(mine, n_sims, weeks, lineup)
  opp_tot <- vapply(field, function(idx) .bb_sim_roster(oppP[idx], n_sims, weeks, lineup), numeric(n_sims))
  worse_than <- rowSums(opp_tot > matrix(my_tot, n_sims, ncol(opp_tot)))    # # opponents ahead of me
  list(advance_pct = round(100 * mean(worse_than < advance), 1),
       win_pct     = round(100 * mean(worse_than == 0), 1),
       baseline_advance_pct = round(100 * advance / n_teams, 1),
       mean_pts = round(mean(my_tot)), ceiling_pts = round(quantile(my_tot, 0.9, names = FALSE)),
       floor_pts = round(quantile(my_tot, 0.1, names = FALSE)),
       n_players = nrow(mine), unmatched = unmatched,
       roster = mine[order(adp), .(player, pos, adp, proj_pg, boom, bye)])
}
