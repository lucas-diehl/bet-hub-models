# ==============================================================================
# DFS ENGINE — interactive dashboard generator
# Runs the pipeline for each requested sport and bakes all results into ONE
# self-contained HTML app (spine/assets/dashboard.html) — no server, opens in a
# browser. Lets you switch sports, filter contests (cash/gpp), browse sortable
# player tables, and inspect lineups + the staking plan. Sports with no slate or
# no plugin (football, until September) render as graceful placeholders.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table); library(jsonlite) })

DASH_SPORT_NAMES <- c(wnba="WNBA", tennis="Tennis", golf="Golf", `golf_m80`="Golf (80% Model)",
                      `golf_opp`="Golf — 2nd Event", `golf_opp_m80`="Golf 2nd (80% Model)",
                      `golf_round`="Golf — Live Round (20-max GPP)",
                      `golf_captain`="Golf — Captain Showdown (20-max GPP)", nfl="NFL", ncaaf="NCAAF", nba="NBA")
DASH_PLACEHOLDER <- c("ncaaf","nba")   # plugins not built yet (NFL now live via sports/nfl/project.R)
# golf TABS -> the extra args passed to the golf plugin. "main" = the PGA event, "opp" =
# opposite-field; golf_round = the LIVE single-round (R1..R4, auto-detected) 1-day contest
# with its own DK salaries, built as a 20-lineup 20-max GPP set.
DASH_GOLF_MAP <- list(golf = list(variant="default", tournament="main"),
                      golf_m80 = list(variant="m80", tournament="main"),
                      golf_opp = list(variant="default", tournament="opp"),
                      golf_opp_m80 = list(variant="m80", tournament="opp"),
                      golf_round = list(single_round=TRUE))   # round resolved live at build time

# publish the interactive simulator (self-contained dashboard HTML `f`) to the bet-site feed so
# the site serves it behind a password, auto-updated. Called from BOTH run_all (10am/1/4/7pm) and
# refresh_dfs (3/8/11pm) so the simulator never lags the values feed.
publish_simulator <- function(f) {
  if (is.null(f) || !file.exists(f)) return(invisible(FALSE))
  fd <- Sys.getenv("FEED_DIR", "C:/Users/ljdie/OneDrive/Documents/dashboard_feed")
  sd <- file.path(fd, "dfs-engine", "simulator"); dir.create(sd, recursive = TRUE, showWarnings = FALSE)
  file.copy(f, file.path(sd, "dashboard.html"), overwrite = TRUE)
  writeLines(as.character(jsonlite::toJSON(list(
    updated = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    date = as.character(Sys.Date())), auto_unbox = TRUE)), file.path(sd, "meta.json"))
  msg("published simulator ->", file.path(sd, "dashboard.html")); invisible(TRUE)
}

# Quantile grid for the embedded field score distribution (denser at the TOP so the
# in-browser simulator resolves top-1% / win% finishes, not just the median). These are
# CDF levels: fgrid[s][k] = the field score at percentile SIM_QLEVELS[k] in sim s.
SIM_QLEVELS <- c(0.02,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,
                 0.70,0.75,0.80,0.84,0.88,0.90,0.92,0.94,0.95,0.96,0.97,0.98,0.985,0.99,
                 0.993,0.996,0.998,0.999)

# Build the compact Monte-Carlo payload the browser simulator scores lineups against.
# We DOWN-SAMPLE the slate sim to K columns and summarize the field per sim as a quantile
# grid, so the whole thing is a few hundred KB. JS math mirrors grade_candidates() exactly:
#   lineup total per sim = colSums(draws[picks])
#   pct beaten per sim   = interpolate total into that sim's field quantile grid
#   cash% = P(total >= per-sim field median); top1% = P(pct >= .99); EV via payout mult.
# Grade the browser candidate lineups (0-based `u`, 1-based pool idx) on a FULL 50k-sim draw,
# mirroring the JS simMetrics() math exactly, so the Optimize/Top-ROI *selection* rests on a
# stable 50,000-sim basis (not the down-sampled K draws the live manual scorer uses — those
# stay light to keep the HTML small). Returns one metrics list per candidate, aligned to `u`.
dash_cand_metrics_hi <- function(r, u, NBIG = 50000L, seed = 99L) {
  if (!length(u)) return(NULL)
  load <- tryCatch(get_loadings(r$pool, r$sport), error = function(e) NULL); if (is.null(load)) return(NULL)
  tl   <- tryCatch(get_team_loadings(r$pool, r$sport), error = function(e) NULL)
  simB <- tryCatch(slate_sim(r$pool, load, team_loadings = tl, n_sims = NBIG, seed = seed), error = function(e) NULL)
  if (is.null(simB) || is.null(simB$scores)) return(NULL)
  scB <- simB$scores; K <- ncol(scB)
  fmat <- score_field(r$field, scB)                              # F × K
  qlev <- SIM_QLEVELS; Q <- length(qlev)
  fq <- apply(fmat, 2, quantile, probs = qlev, names = FALSE)    # Q × K  (field score grid per sim)
  rm(fmat); gc(FALSE)
  g1 <- fq[1, ]; gn <- fq[Q, ]; gnm1 <- fq[Q - 1, ]
  # gridAt(level) -> field score at CDF `level`, per sim (used for the cash line at 0.5)
  gridAtLevel <- function(level) {
    if (level <= qlev[1]) return(g1); if (level >= qlev[Q]) return(gn)
    for (j in 1:(Q - 1)) if (level <= qlev[j + 1]) { f <- (level - qlev[j]) / (qlev[j + 1] - qlev[j]); return(fq[j, ] + (fq[j + 1, ] - fq[j, ]) * f) }
    gn
  }
  fmed <- gridAtLevel(0.5)
  pctBeatenVec <- function(tot) {                                # fraction of field beaten, per sim
    p <- rep(qlev[Q], K); done <- logical(K)
    lo <- tot <= g1; p[lo] <- qlev[1] * pmax(0, pmin(1, tot[lo] / pmax(g1[lo], 1))); done[lo] <- TRUE
    hi <- (!done) & (tot >= gn); p[hi] <- qlev[Q] + (1 - qlev[Q]) * pmin(1, (tot[hi] - gn[hi]) / pmax(1, gn[hi] - gnm1[hi])); done[hi] <- TRUE
    for (j in 1:(Q - 1)) { sel <- (!done) & (tot <= fq[j + 1, ]); if (any(sel)) {
      f <- (tot[sel] - fq[j, sel]) / pmax(1e-9, fq[j + 1, sel] - fq[j, sel]); p[sel] <- qlev[j] + (qlev[j + 1] - qlev[j]) * f; done[sel] <- TRUE } }
    pmax(0, pmin(0.9999, p))
  }
  fs <- length(r$field$idx); gmult <- payout_multipliers(make_gpp(), fs)
  P <- as.data.table(r$pool); own <- if ("own" %in% names(P)) pmax(P$own, 1e-4) else rep(0.05, nrow(P))
  proj <- P$proj
  lapply(u, function(ix) {
    tot <- colSums(scB[ix, , drop = FALSE]); pct <- pctBeatenVec(tot)
    rank <- pmax(1L, pmin(fs, round((1 - pct) * fs))); mult <- gmult[rank]; mult[is.na(mult)] <- 0
    list(gppRoi = round(mean(mult) - 1, 4), roiStd = round(sqrt(max(mean(mult * mult) - mean(mult)^2, 0)), 4),
         cash = round(mean(tot >= fmed), 4), top1 = round(mean(pct >= 0.99), 4),
         win = round(mean(pct >= 1 - 1 / fs), 5), fin = round(mean(pct), 4),
         dupe = round(prod(pmax(own[ix], 0.004))^(1 / length(ix)), 4),
         own = round(sum(own[ix]), 3), proj = round(sum(proj[ix]), 1)) })
}

# Contest options for the simulator dropdown: the LIVE DK contests available for this
# sport (real name/fee/field, payout shape by type) + robust presets as a fallback.
# Each option carries a payout-multiplier vector at the SIM field resolution `fs` so the
# browser can re-grade EV/ROI for the chosen contest with no re-run. Fully fallback-safe.
dash_contest_options <- function(sport, fs) {
  fs <- as.integer(fs); if (!is.finite(fs) || fs < 2L) fs <- 1000L
  mk <- function(name, fee, field, kind, curve)
    list(name = name, fee = suppressWarnings(as.numeric(fee)),
         field = suppressWarnings(as.integer(field)), kind = kind,
         mult = round(payout_multipliers(curve, fs), 3))
  presets <- list(
    mk("Preset · 20-entry large GPP (your format)", NA, NA, "gpp", make_gpp(alpha = 1.4, paid_frac = 0.20)),
    mk("Preset · GPP — large field (top-heavy)", NA, NA, "gpp", make_gpp(alpha = 1.4)),
    mk("Preset · GPP — small field",             NA, NA, "gpp", make_gpp(alpha = 1.0)),
    mk("Preset · Double-Up / 50-50 (cash)",      NA, NA, "cash", make_double_up()),
    mk("Preset · Single-Entry GPP",              NA, NA, "gpp", make_gpp(paid_frac = 0.25, alpha = 1.2)))
  live <- tryCatch({
    psport <- if (exists("DASH_GOLF_MAP") && sport %in% names(DASH_GOLF_MAP)) "golf" else sport
    dks <- .DK_SPORT[[psport]] %||% toupper(psport)
    j <- .dk_get_json(paste0("https://www.draftkings.com/lobby/getcontests?sport=", dks))
    ct <- as.data.table(j$Contests)
    lg <- if ("attr.League" %in% names(ct)) ct[["attr.League"]] else rep(NA_character_, nrow(ct))
    mf <- DK_SPORT_MATCH[[psport]]; if (is.null(mf)) mf <- function(n, l) grepl(psport, n, ignore.case = TRUE)
    ct <- ct[which(mf(ct$n, lg))]
    ct[, `:=`(fee = suppressWarnings(as.numeric(a)), field = suppressWarnings(as.integer(m)))]
    du <- if ("attr.IsDoubleUp" %in% names(ct)) ct[["attr.IsDoubleUp"]] %in% TRUE else rep(FALSE, nrow(ct))
    ff <- if ("attr.IsFiftyfifty" %in% names(ct)) ct[["attr.IsFiftyfifty"]] %in% TRUE else rep(FALSE, nrow(ct))
    ct[, cash := du | ff]
    ct[, cap := if ("mec" %in% names(ct)) suppressWarnings(as.integer(mec)) else NA_integer_]
    # real fields only (drop 2-3 seat high-roller satellites), sane entry fees
    ct <- ct[is.finite(fee) & is.finite(field) & field >= 100L & fee <= 2000]
    sel <- list()
    g20 <- ct[cash == FALSE & is.finite(cap) & cap >= 6L & cap <= 25L & field >= 1000L][order(-field)]
    if (nrow(g20)) sel <- c(sel, list(g20[1]))                     # a real 20-max large-field GPP (your format)
    g <- ct[cash == FALSE][order(-field)]
    if (nrow(g))    sel <- c(sel, list(g[1]))                       # biggest-field GPP
    if (nrow(g) > 1) sel <- c(sel, list(g[.N]))                     # smallest real-field GPP
    cc <- ct[cash == TRUE][order(-field)]
    if (nrow(cc))   sel <- c(sel, list(cc[1]))                      # a double-up / cash
    lapply(sel, function(x) {
      curve <- if (isTRUE(x$cash)) make_double_up()
               else make_gpp(alpha = if (isTRUE(x$field >= 20000L)) 1.4 else 1.0)
      mk(sprintf("%s  ($%s · %s)", substr(x$n, 1, 34),
                 format(x$fee, trim = TRUE), format(x$field, big.mark = ",", trim = TRUE)),
         x$fee, x$field, if (isTRUE(x$cash)) "cash" else "gpp", curve)
    })
  }, error = function(e) list())
  c(live, presets)
}

build_sim_payload <- function(r, K = 1000L, opt_sims = 50000L) {
  scores <- r$sim$scores
  if (is.null(scores) || !nrow(scores)) return(NULL)
  Kc <- min(as.integer(K), ncol(scores))
  sc <- scores[, seq_len(Kc), drop = FALSE]                 # P × K player draws
  fmat <- score_field(r$field, sc)                          # field_n × K lineup totals
  fq <- apply(fmat, 2, quantile, probs = SIM_QLEVELS, names = FALSE)  # Q × K
  P <- as.data.table(r$sim$pool)
  own <- if ("own" %in% names(P)) pmax(P$own, 1e-4) else rep(0.05, nrow(P))
  fs <- length(r$field$idx)
  u <- r$field$idx[!duplicated(r$field$keys)]; if (length(u) > 2500L) u <- u[seq_len(2500L)]  # candidate set (1-based)
  cand_metrics <- tryCatch(dash_cand_metrics_hi(r, u, NBIG = opt_sims), error = function(e) NULL)
  rr <- r$rr
  # positional structure (flat-n for golf/tennis; slot-based for team sports)
  slots <- if (!is.null(rr$slots)) as.list(rr$slots) else NULL
  flex  <- if (!is.null(rr$flex)) list(positions = rr$flex$positions, count = rr$flex$count) else NULL
  list(
    k = Kc, field_size = fs, qlevels = SIM_QLEVELS,
    roster = list(n = rr$n %||% (if (!is.null(slots)) sum(unlist(slots)) + (flex$count %||% 0) else NA),
                  cap = rr$cap, floor = rr$floor %||% 0, slots = slots, flex = flex,
                  team_limit = rr$team_limit, max_per_game = rr$max_per_game),
    players = lapply(seq_len(nrow(P)), function(i) list(
      name = P$player_name[i], team = (P$team[i] %||% NA), pos = (P$position[i] %||% "G"),
      salary = as.integer(P$salary[i]), proj = round(P$proj[i], 1), own = round(own[i], 4))),
    draws = lapply(seq_len(nrow(sc)), function(i) as.integer(round(sc[i, ]))),  # P arrays of K
    fgrid = lapply(seq_len(Kc), function(s) as.integer(round(fq[, s]))),        # K arrays of Q
    gpp_mult  = round(payout_multipliers(make_gpp(), fs), 3),
    cash_mult = round(payout_multipliers(make_double_up(), fs), 3),
    # candidate lineups for the "top N" optimizer = the unique competitive field lineups
    # (span chalk -> leverage); 0-based player indices for the browser to score + rank.
    cands = lapply(u, function(ix) as.integer(ix - 1L)),
    # per-candidate metrics graded on a FULL 50k-sim draw (aligned to cands) — the Optimize
    # and Top-ROI selection use these so ranking rests on 50,000 sims, not the light K draws.
    opt_k = if (!is.null(cand_metrics)) opt_sims else Kc,
    cand_metrics = cand_metrics)
}

# LIGHTWEIGHT daily projection logging — for each sport with a live slate, project the
# pool and persist projections + projected ownership + salaries, SKIPPING the slow sim/
# optimizer. Run this early (before the full dashboard build) so the projections needed to
# grade accuracy are logged every day even if the heavy build later fails, times out, or a
# DB lock trips it. Cheap: seconds per sport. Default = the events we grade accuracy on.
log_projections <- function(sports = c("wnba", "tennis", "golf", "golf_opp"), date = Sys.Date()) {
  n <- 0L
  for (sport in sports) {
    ok <- tryCatch({
      psport <- if (sport %in% names(DASH_GOLF_MAP)) "golf" else sport
      slate <- "main"; extra <- list()
      if (sport %in% names(DASH_GOLF_MAP)) { if (sport != "golf") slate <- sport; extra <- DASH_GOLF_MAP[[sport]] }
      dfs_load_sport(psport)
      if (psport != "golf") {                       # non-golf: need a live DK slate scraped first
        ms <- tryCatch(dk_main_slate(psport), error = function(e) NULL)
        if (is.null(ms) || isTRUE(ms$is_showdown)) return(FALSE)
        if (!dk_salary_csv_ready(dk_salary_path(psport, date, slate), date))
          scrape_dk_salaries(psport, date = date, slate = slate, draft_group_id = ms$draft_group_id)
      }
      spec <- get_sport(psport); if (is.null(spec$project_players)) return(FALSE)
      sid <- make_slate_id(psport, "dk", date, slate)
      slate_obj <- c(list(sport = psport, site = "dk", date = as.character(date), name = slate, slate_id = sid), extra)
      pool <- validate_projection(as.data.table(spec$project_players(slate_obj)))
      pool <- tryCatch(apply_inactives(pool, psport, date), error = function(e) pool)  # drop inactives
      tryCatch(persist_projected_ownership(pool, sid, psport), error = function(e) NULL)
      persist_projections(pool, sid, psport)
      TRUE
    }, error = function(e) { msg("  log_projections", sport, "skipped:", conditionMessage(e)); FALSE })
    if (isTRUE(ok)) { n <- n + 1L; msg("  logged projections:", sport) }
  }
  msg("log_projections: logged", n, "of", length(sports), "sport-slates for", as.character(date))
  invisible(n)
}

# build the data card for one sport (ready / no_slate / placeholder / error)
build_sport_card <- function(sport, date, slate = "main", n_lineups = 8L,
                             opts = load_bankroll_opts(), extra = list()) {
  if (identical(sport, "golf_captain")) return(build_golf_captain_card(date, opts))
  # golf tabs share the golf plugin; each maps to a (blend, tournament) with its own slate id
  psport <- if (sport %in% names(DASH_GOLF_MAP)) "golf" else sport
  gm <- if (sport %in% names(DASH_GOLF_MAP)) DASH_GOLF_MAP[[sport]] else NULL
  if (!is.null(gm)) { if (sport != "golf") slate <- sport; extra <- c(extra, gm) }
  # LIVE single-round golf: auto-detect the current DK round (R1..R4) + build the 20-lineup
  # 20-max GPP set. No hardcoded round — advances as the tournament progresses.
  if (isTRUE(extra$single_round) && is.null(extra$round)) {
    lr <- tryCatch(golf_live_round(), error = function(e) NULL)
    if (is.null(lr)) return(list(sport = sport, name = DASH_SPORT_NAMES[[sport]] %||% "Golf — Live Round",
      status = "no_slate", message = "No live DK single-round golf slate posted yet. DK posts each round's slate a few hours before it starts — re-run closer to the round."))
    extra$round <- lr$round; extra$draft_group_id <- lr$draft_group_id
    n_lineups <- 20L                                   # 20-max GPP format (fully-entered set)
  }
  disp <- DASH_SPORT_NAMES[[sport]] %||% toupper(sport)
  if (psport == "golf") { ev <- tryCatch(golf_event_name(gm$tournament %||% "main"), error = function(e) NULL)
    if (!is.null(ev)) disp <- paste0(ev, if (isTRUE(extra$single_round)) sprintf(" — Round %d (20-max GPP)", extra$round %||% 2L)
                                          else if (grepl("m80", sport)) " (80% model)" else "") }
  base <- list(sport = sport, name = disp)

  if (sport %in% DASH_PLACEHOLDER)
    return(c(base, list(status = "placeholder",
      message = sprintf("%s plugin is in development — live for the 2026 season (kickoff September). The spine, optimizer, sim and reporting are ready; only the %s projection plugin remains.", disp, disp))))

  # golf (+golf_m80) pull from DataGolf/your model; others auto-detect today's DK slate
  if (psport != "golf") {
    ms <- tryCatch(dk_main_slate(sport), error = function(e) NULL)
    if (is.null(ms))
      return(c(base, list(status = "no_slate",
        message = sprintf("No live %s slate on DraftKings for %s yet. DK posts slates a few hours before lock — re-run closer to game time.", disp, date))))
    if (ms$is_showdown) {                              # single-game / Captain Mode
      card <- tryCatch(build_contest_card(ms$contest_id, sport = sport, date = date,
                                          n_lineups = min(n_lineups, 6L), opts = opts),
                       error = function(e) NULL)
      if (is.null(card)) return(c(base, list(status = "no_slate",
        message = sprintf("%s Showdown slate detected but not draftable yet (%s). Re-run closer to lock.", disp, ms$name))))
      card$sport <- sport; card$name <- paste0(disp, " (Showdown)")
      return(card)
    }
    if (!dk_salary_csv_ready(dk_salary_path(sport, date, slate), date)) {  # classic: scrape detected group
      ok <- tryCatch({ scrape_dk_salaries(sport, date = date, slate = slate,
                                          draft_group_id = ms$draft_group_id); TRUE },
                     error = function(e) { msg("  scrape failed:", conditionMessage(e)); FALSE })
      if (!ok) return(c(base, list(status = "no_slate",
        message = sprintf("%s slate found but salaries not draftable yet. Re-run closer to lock.", disp))))
    }
  }

  # lighter sim than run_slate's defaults so the interactive dashboard builds fast — grade
  # stays stable at these sizes, and the embedded simulator subsamples 1000 cols regardless.
  # 20-max GPP is the user's format across ALL sports -> use the backtest-tuned 20-entry
  # build (proj-ranked, ~0.5 exposure) with a 20-lineup set on every classic card.
  n_lineups <- 20L
  r <- tryCatch(run_slate(psport, date = date, slate = slate, n_lineups = n_lineups, extra = extra,
                          n_sims = 4000L, field_n = 1200L, n_cand = 300L, gpp_mode = "gpp20"),
                error = function(e) e)
  if (inherits(r, "error"))
    return(c(base, list(status = "error",
      message = sprintf("Could not build %s: %s", disp, conditionMessage(r)))))

  P <- .player_table(r$pool)
  ceilcol <- if ("ceil" %in% names(P)) P$ceil else P$proj
  floorcol <- if ("floor" %in% names(P)) P$floor else pmax(P$proj - 1.2 * (if ("sim_sd" %in% names(P)) P$sim_sd else 0), 0)
  players <- data.frame(
    name = P$player_name, team = if ("team" %in% names(P)) P$team else NA,
    pos = if ("position" %in% names(P)) P$position else NA,
    salary = as.integer(P$salary), proj = round(P$proj, 1), ceil = round(ceilcol, 1),
    floor = round(floorcol, 1), own = round(if ("own" %in% names(P)) P$own else 0, 4),
    lev = round(P$lev, 3), value = round(P$value, 2), stringsAsFactors = FALSE)

  plan <- recommend_plan(r$picks, r$gates, opts)
  plan_list <- lapply(seq_len(nrow(plan)), function(i) { x <- plan[i]
    list(contest = x$Contest, type = x$Type, entries = x$Entries, fee = x$FeePerEntry,
         total = x$TotalStake, status = x$Status, why = x$Rationale) })

  lineups <- lapply(seq_along(r$picks), function(i) { p <- r$picks[[i]]; ix <- p$idx[[1]]
    list(n = i, role = p$role, live = isTRUE(p$live), salary = as.integer(p$salary),
         proj = round(p$proj, 1), ceil = round(sum(ceilcol[ix]), 1), own = round(p$avg_own, 4),
         reason = p$reason,
         players = lapply(ix, function(j) list(name = P$player_name[j],
           pos = P$position[j] %||% "", salary = as.integer(P$salary[j]),
           proj = round(P$proj[j], 1), own = round((if ("own" %in% names(P)) P$own[j] else 0), 4)))) })

  sim_payload <- tryCatch(build_sim_payload(r), error = function(e) { msg("  sim payload skipped:", conditionMessage(e)); NULL })
  if (!is.null(sim_payload))                                        # contest dropdown options
    sim_payload$contests <- tryCatch(dash_contest_options(sport, sim_payload$field_size),
                                     error = function(e) NULL)

  c(base, list(status = "ready", slate_id = r$slate_id,
    gates = list(cash = isTRUE(r$gates$cash_enabled), gpp = isTRUE(r$gates$gpp_enabled),
                 validated_on = r$gates$validated_on %||% NA),
    bankroll = opts$bankroll, daily_budget = round(attr(plan, "daily_budget"), 2),
    live_total = attr(plan, "live_total"), plan = plan_list, lineups = lineups, players = players,
    sim = sim_payload))
}

# Card for a specific DK contest (real payouts; Showdown -> captain board).
build_contest_card <- function(contest_id, sport = NULL, date = Sys.Date(),
                               n_lineups = 6L, opts = load_bankroll_opts()) {
  r <- run_contest(contest_id, sport = sport, date = date, n_lineups = n_lineups)
  P <- .player_table(r$base_pool)
  ceilcol <- if ("ceil" %in% names(P)) P$ceil else P$proj
  players <- data.frame(
    name = P$player_name, team = if ("team" %in% names(P)) P$team else NA,
    pos = if ("position" %in% names(P)) P$position else NA,
    salary = as.integer(P$salary), proj = round(P$proj, 1), ceil = round(ceilcol, 1),
    floor = round(if ("floor" %in% names(P)) P$floor else P$proj, 1),
    own = round(if ("own" %in% names(P)) P$own else 0, 4),
    lev = round(P$lev, 3), value = round(P$value, 2), stringsAsFactors = FALSE)

  plan <- recommend_contest_plan(r$contest, r$picks, r$gates, opts)
  plan_list <- lapply(seq_len(nrow(plan)), function(i) { x <- plan[i]
    list(contest = x$Contest, type = x$Type, entries = x$Entries, fee = x$FeePerEntry,
         total = x$TotalStake, status = x$Status, why = x$Rationale) })

  EP <- as.data.table(r$pool)
  epc <- if ("ceil" %in% names(EP)) EP$ceil else EP$proj
  lineups <- lapply(seq_along(r$picks), function(i) { p <- r$picks[[i]]; ix <- p$idx[[1]]
    cpt <- if (r$showdown) ix[EP$slot[ix] == "CPT"] else integer(0)
    ord <- c(cpt, setdiff(ix, cpt))
    list(n = i, role = p$role, live = isTRUE(p$live), salary = as.integer(p$salary),
         proj = round(p$proj, 1), ceil = round(sum(epc[ix]), 1), own = round(p$avg_own, 4),
         captain = if (length(cpt)) EP$player_name[cpt[1]] else NA, reason = p$reason,
         players = lapply(ord, function(j) list(name = EP$player_name[j],
           slot = if (r$showdown) EP$slot[j] else (EP$position[j] %||% ""),
           salary = as.integer(EP$salary[j]), proj = round(EP$proj[j], 1)))) })

  captains <- NULL
  if (!is.null(r$captain_board)) captains <- lapply(seq_len(nrow(r$captain_board)), function(i) {
    x <- r$captain_board[i]; list(player = x$player, team = x$team, cpt_salary = x$cpt_salary,
      cpt_proj = x$cpt_proj, cpt_ceil = x$cpt_ceil, cpt_own = x$cpt_own, leverage = x$leverage) })

  ct <- r$contest; first <- if (!is.null(ct$prizes) && nrow(ct$prizes)) max(ct$prizes$prize) else NA
  list(sport = paste0("ct_", ct$contest_id), name = paste0(toupper(r$sport),
         if (r$showdown) " SD" else "", " $", ct$entry_fee), title = ct$name,
       status = "ready", mode = if (r$showdown) "showdown" else "classic",
       contest = list(fee = ct$entry_fee, field = ct$field_size, max_user = ct$max_entries_per_user,
                      prizes = ct$total_payouts, first = first),
       slate_id = r$slate_id,
       gates = list(cash = isTRUE(r$gates$cash_enabled), gpp = isTRUE(r$gates$gpp_enabled)),
       bankroll = opts$bankroll, daily_budget = round(attr(plan, "daily_budget"), 2),
       live_total = attr(plan, "live_total"), plan = plan_list, captains = captains,
       lineups = lineups, players = players)
}

# Golf CAPTAIN MODE SHOWDOWN card (1 CPT @1.5x + 5 FLEX, $50k). Auto-detects the live DK
# captain slate + round (R4 blends finishing/placement points), builds the captain board +
# a 20-entry showdown GPP set (with the per-player exposure caps). Returns the same shape
# as build_contest_card so the dashboard renders it as a showdown tab.
build_golf_captain_card <- function(date = Sys.Date(), opts = load_bankroll_opts()) {
  nm0 <- "Golf — Captain Showdown"
  tryCatch(dfs_load_sport("golf"), error = function(e) NULL)
  cg <- tryCatch(golf_dk_captain_group(), error = function(e) NULL)
  if (is.null(cg)) return(list(sport = "golf_captain", name = nm0, status = "no_slate",
    message = "No live DK golf Captain Mode Showdown slate posted yet. DK posts it a few hours before the round — re-run closer to lock."))
  base <- tryCatch(golf_captain_base(cg, date), error = function(e) { msg("  captain base error:", conditionMessage(e)); NULL })
  if (is.null(base) || nrow(base) < 6L) return(list(sport = "golf_captain", name = nm0, status = "no_slate",
    message = sprintf("Captain slate detected (Round %s) but projections/salaries not ready yet. Re-run closer to lock.", cg$round %||% "?")))
  set.seed(2026L); cpt_mult <- 1.5
  rr <- showdown_roster_rules(50000L)
  exp_pool <- expand_showdown_pool(base, cpt_mult)
  bsim  <- slate_sim(base, get_loadings(base, "golf"), team_loadings = get_team_loadings(base, "golf"), n_sims = 4000L, seed = 2026L)
  esim  <- showdown_sim(base, exp_pool, bsim, cpt_mult)
  field <- simulate_field(exp_pool, rr, field_n = 1200L, own = exp_pool$own)
  cands <- make_candidates(exp_pool, rr, n_cand = 300L)
  res   <- grade_candidates(cands, esim, field, curve_gpp = make_gpp(), curve_cash = make_double_up())
  gates <- tryCatch(load_gates("golf"), error = function(e) list(gpp_enabled = FALSE, cash_enabled = FALSE))
  picks <- build_gpp20(res, gates, n = 20L, pool = exp_pool,
                       caps = tryCatch(load_exposure_overrides("golf"), error = function(e) NULL))
  cb <- captain_board(base, cpt_mult, n = 12L)
  ev <- tryCatch(golf_event_name("main"), error = function(e) NULL)
  disp <- paste0(ev %||% "Golf", sprintf(" — Round %s Captain Showdown (20-max GPP)", cg$round %||% "?"))

  P <- .player_table(base); ceilcol <- if ("ceil" %in% names(P)) P$ceil else P$proj
  players <- data.frame(name = P$player_name, team = if ("team" %in% names(P)) P$team else NA,
    pos = if ("position" %in% names(P)) P$position else NA, salary = as.integer(P$salary),
    proj = round(P$proj, 1), ceil = round(ceilcol, 1),
    floor = round(if ("floor" %in% names(P)) P$floor else P$proj, 1),
    own = round(if ("own" %in% names(P)) P$own else 0, 4),
    lev = round(P$lev, 3), value = round(P$value, 2), stringsAsFactors = FALSE)

  EP <- as.data.table(exp_pool); epc <- if ("ceil" %in% names(EP)) EP$ceil else EP$proj
  lineups <- lapply(seq_along(picks), function(i) { p <- picks[[i]]; ix <- p$idx[[1]]
    cpt <- ix[EP$slot[ix] == "CPT"]; ord <- c(cpt, setdiff(ix, cpt))
    list(n = i, role = p$role, live = isTRUE(p$live), salary = as.integer(p$salary),
         proj = round(p$proj, 1), ceil = round(sum(epc[ix]), 1), own = round(p$avg_own %||% 0, 4),
         captain = if (length(cpt)) EP$player_name[cpt[1]] else NA, reason = p$reason,
         players = lapply(ord, function(j) list(name = EP$player_name[j], slot = EP$slot[j],
           salary = as.integer(EP$salary[j]), proj = round(EP$proj[j], 1)))) })
  captains <- lapply(seq_len(nrow(cb)), function(i) { x <- cb[i]
    list(player = x$player, team = x$team, cpt_salary = x$cpt_salary, cpt_proj = x$cpt_proj,
         cpt_ceil = x$cpt_ceil, cpt_own = x$cpt_own, leverage = x$leverage) })

  list(sport = "golf_captain", name = disp, title = cg$name, status = "ready", mode = "showdown",
       slate_id = make_slate_id("golf", "dk", date, paste0("captain_r", cg$round %||% "x")),
       gates = list(cash = isTRUE(gates$cash_enabled), gpp = isTRUE(gates$gpp_enabled)),
       bankroll = opts$bankroll, daily_budget = 0, live_total = 0, plan = list(),
       captains = captains, lineups = lineups, players = players)
}

# Scorecard: fuse the P&L ledger + projection-accuracy scorecard into one per-sport
# "where's my edge" table with a PLAY / CAUTION / AVOID verdict. This is the daily
# decision layer — play where we're accurate AND profitable, avoid where we're neither.
build_scorecard <- function() {
  pnl <- tryCatch(pnl_summary(print = FALSE), error = function(e) NULL)
  acc <- tryCatch(projection_accuracy(print = FALSE), error = function(e) NULL)
  ps <- if (!is.null(pnl)) as.data.table(pnl$by_sport) else NULL
  ac <- if (!is.null(acc)) as.data.table(acc$by_sport) else NULL
  if ((is.null(ps) || !nrow(ps)) && (is.null(ac) || !nrow(ac))) return(NULL)
  sports <- union(if (!is.null(ps)) ps$sport else character(0),
                  if (!is.null(ac)) ac$sport else character(0))
  g <- function(dt, sp, col) { if (is.null(dt) || !nrow(dt)) return(NA); v <- dt[sport == sp][[col]]; if (length(v)) v[1] else NA }
  rows <- lapply(sports, function(sp) {
    net <- g(ps, sp, "net"); roi <- g(ps, sp, "roi_pct"); ent <- g(ps, sp, "entries")
    corr <- g(ac, sp, "corr"); scorr <- g(ac, sp, "salary_corr"); nn <- g(ac, sp, "n")
    hasmodel <- !is.na(corr)
    # "edge" = our projection beats the naive salary baseline by a MEANINGFUL margin
    beats <- hasmodel && !is.na(scorr) && (corr - scorr) >= 0.08
    verdict <- if (!is.na(net) && net > 0) "PLAY"           # profitable -> play
               else if (hasmodel && beats) "CAUTION"        # real model edge, losing to variance
               else "AVOID"                                 # no edge / marginal + losing
    list(sport = sp, entries = as.integer(ent %||% 0), net = round(net %||% NA, 2),
         roi = round(roi %||% NA, 1), cash = round(g(ps, sp, "cash_pct") %||% NA, 1),
         slates = as.integer(g(ac, sp, "slates") %||% 0), n = as.integer(nn %||% 0),
         corr = round(corr %||% NA, 2), rmse = round(g(ac, sp, "rmse") %||% NA, 1),
         bias = round(g(ac, sp, "bias") %||% NA, 1), salary_corr = round(scorr %||% NA, 2),
         beats_salary = isTRUE(beats), verdict = verdict)
  })
  rows <- rows[order(-vapply(rows, function(r) r$net %||% -1e9, numeric(1)))]
  ov <- if (!is.null(pnl)) pnl$overall else NULL
  list(overall = if (!is.null(ov)) list(entries = ov$entries, buyins = ov$buyins,
         winnings = ov$winnings, net = ov$net, roi = ov$roi_pct, cash = ov$cash_pct) else NULL,
       sports = rows)
}

# Best-ball DRAFT BOARD for the dashboard: forward per-player projection (proj/game,
# boom, play rate, bye) fused with the ADP value board (value vs market + age/injury risk).
# Season-long draft prep, so it's a standalone tab (not a daily slate). The advance-rate
# season simulator stays in the CLI (jobs/bestball.R --roster ...).
build_bestball <- function(max_players = 300L) {
  tryCatch(dfs_load_sport("nfl"), error = function(e) NULL)
  fp <- tryCatch(as.data.table(bb_forward_projection()), error = function(e) NULL)
  if (is.null(fp) || !nrow(fp)) return(NULL)
  vb <- tryCatch(as.data.table(bb_value_board()), error = function(e) NULL)
  if (!is.null(vb) && nrow(vb)) {
    vb[, nm := norm_name(player)]
    fp <- merge(fp, vb[, .(nm = nm, value, risk, status)], by.x = "norm", by.y = "nm", all.x = TRUE)
  }
  fp <- fp[order(adp)][seq_len(min(max_players, .N))]
  rows <- lapply(seq_len(nrow(fp)), function(i) { x <- fp[i]
    list(player = x$player, pos = x$pos, team = x$team, adp = round(x$adp, 1),
         proj_pg = x$proj_pg, boom = round(100 * x$boom), play = round(100 * x$p_play),
         bye = x$bye, age = x$age, value = x$value %||% NA_integer_,
         status = x$status %||% "", risk = x$risk %||% "") })
  list(players = rows, n = length(rows), updated = as.character(Sys.Date()))
}

# ---- Bet Hub optimizer pool feed ---------------------------------------------
# Emit each ready sport card's SaberSim `sim` payload as
# pool_<sport>_<date>_<site>.json (+ _r<round> for single-round golf) for the Bet
# Hub browser optimizer. Reuses the card's already-computed sim — no extra sims.
POOL_SPORT_MAP <- list(
  wnba       = list(sport = "wnba",   slate_type = NA_character_),
  tennis     = list(sport = "tennis", slate_type = NA_character_),
  golf       = list(sport = "golf",   slate_type = NA_character_),
  golf_round = list(sport = "golf",   slate_type = "single_round"),
  nfl        = list(sport = "nfl",    slate_type = NA_character_)
)
publish_pools <- function(cards, date, site = "draftkings") {
  feed <- Sys.getenv("FEED_DIR", "C:/Users/ljdie/OneDrive/Documents/dashboard_feed")
  outdir <- file.path(feed, "dfs-engine", "pools"); dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  now_iso <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  compact <- function(l) l[!vapply(l, function(x) is.null(x) || (length(x) == 1L && is.na(x)), logical(1))]
  n <- 0L
  for (card in cards) {
    m <- POOL_SPORT_MAP[[card$sport %||% ""]]
    if (is.null(m) || !identical(card$status, "ready") || is.null(card$sim)) next
    sid <- card$slate_id %||% ""
    sdate <- sub("^.*-dk-(\\d{4}-\\d{2}-\\d{2})-.*$", "\\1", sid)
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", sdate)) sdate <- as.character(date)
    s <- sub("^.*-\\d{4}-\\d{2}-\\d{2}-", "", sid)
    slabel <- if (!nchar(s) || s == sid) "Main" else paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
    round <- NULL
    if (identical(m$slate_type, "single_round")) {
      rm <- regmatches(card$name %||% "", regexpr("Round\\s*\\d", card$name %||% ""))
      if (length(rm)) round <- suppressWarnings(as.integer(gsub("\\D", "", rm)))
      if (length(round) == 0L || is.na(round)) round <- NULL
    }
    meta <- compact(list(
      contract_version = "1.0", source = "dfs-engine", sport = m$sport, site = site,
      slate_date = sdate, slate_label = slabel,
      slate_type = if (is.na(m$slate_type)) NULL else m$slate_type, round = round,
      event = card$name, generated_at = now_iso))
    out <- c(meta, card$sim)
    fn <- if (!is.null(round)) sprintf("pool_%s_%s_%s_r%d.json", m$sport, sdate, site, round)
          else sprintf("pool_%s_%s_%s.json", m$sport, sdate, site)
    txt <- as.character(toJSON(out, auto_unbox = TRUE, na = "null", null = "null", digits = 4))
    writeLines(txt, file.path(outdir, fn), useBytes = TRUE)
    n <- n + 1L
    msg("pool:", m$sport, site, "->", fn, "(", length(card$sim$players %||% list()), "players )")
  }
  msg("published", n, "pool file(s)")
  invisible(n)
}

build_dashboard <- function(sports = c("wnba","tennis","golf","golf_round","golf_m80","golf_opp","golf_opp_m80","nfl","ncaaf"),
                            contests = NULL, date = Sys.Date(), bankroll = NULL,
                            n_lineups = 8L, extra = list()) {
  opts <- load_bankroll_opts(); if (!is.null(bankroll)) opts$bankroll <- as.numeric(bankroll)
  cards <- lapply(sports, function(s) {
    msg("dashboard: building", s)
    tryCatch(build_sport_card(s, date, n_lineups = n_lineups, opts = opts, extra = extra),
             error = function(e) list(sport = s, name = DASH_SPORT_NAMES[[s]] %||% toupper(s),
                                      status = "error", message = conditionMessage(e)))
  })
  for (cid in (contests %||% character(0))) {
    msg("dashboard: building contest", cid)
    card <- tryCatch(build_contest_card(cid, date = date, n_lineups = min(n_lineups, 6L), opts = opts),
                     error = function(e) list(sport = paste0("ct_", cid), name = paste0("Contest ", cid),
                                              status = "error", message = conditionMessage(e)))
    cards <- c(cards, list(card))
  }
  tryCatch(publish_pools(cards, date), error = function(e) msg("publish_pools error:", conditionMessage(e)))
  scorecard <- tryCatch(build_scorecard(), error = function(e) { msg("scorecard skipped:", conditionMessage(e)); NULL })
  bestball  <- tryCatch(build_bestball(),  error = function(e) { msg("bestball skipped:", conditionMessage(e)); NULL })
  top <- list(date = as.character(date), bankroll = opts$bankroll, sports = cards,
              scorecard = scorecard, bestball = bestball)
  json <- toJSON(top, auto_unbox = TRUE, dataframe = "rows", na = "null", null = "null", digits = 4)

  tmpl <- paste(readLines(dfs_path("spine", "assets", "dashboard.html"), warn = FALSE), collapse = "\n")
  parts <- strsplit(tmpl, "__DFS_DATA__", fixed = TRUE)[[1]]   # safe inject (no regex backrefs)
  html <- paste0(parts[1], json, parts[2])
  outdir <- dfs_path("data", "reports"); dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(outdir, sprintf("dashboard_%s.html", date))
  writeLines(html, f)
  msg("Dashboard ->", f)
  invisible(f)
}
