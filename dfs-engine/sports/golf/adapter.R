# ==============================================================================
# Golf plugin — adapter onto the existing DataGolf engine
# Your mature golf engine lives in golf-modeling/ (R targets pipeline). Rather than
# move a working pipeline, this adapter exposes golf through the SAME plugin
# contract the spine uses for every sport, so the spine never depends on golf's
# working directory (the bug in the detached copies).
#
# Live projections come straight from DataGolf's DFS defaults (the same source the
# golf pipeline blends), normalized into the spine's pool format. The richer
# xgboost model + gates still live in golf-modeling and can be blended in later by
# pointing dg_blend at its outputs. Golf is low inter-player correlation -> no
# game factors (correlation = NULL).
#
# Requires env var DATAGOLF_API_KEY.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

GOLF_ROSTER <- list(
  n          = 6L,
  cap        = 50000L,
  floor      = 49400L,               # matches the golf pipeline's salary floor
  slots      = NULL,                 # flat-n
  slot_labels= paste0("G", 1:6),
  team_limit = NULL, max_per_game = NULL
)

# FanDuel golf: 6 golfers, $60,000 cap (no floor). Same flat-n roster as DK; the
# difference that matters is the salary scale (FD prices) + FD's finish-weighted
# scoring, which the projection side supplies per site.
GOLF_ROSTER_FD <- list(
  n          = 6L,
  cap        = 60000L,
  floor      = NULL,
  slots      = NULL,
  slot_labels= paste0("G", 1:6),
  team_limit = NULL, max_per_game = NULL
)

.golf_dg_get <- function(endpoint, params = list()) {
  key <- dfs_require_key("DATAGOLF_API_KEY")
  params$key <- key; params$file_format <- "json"
  url <- paste0("https://feeds.datagolf.com/", endpoint)
  resp <- tryCatch(httr2::request(url) |> httr2::req_url_query(!!!params) |> httr2::req_perform(),
                   error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) != 200) return(NULL)
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

# Where the user's golf-modeling project lives (produces dfs_projections.rds via its
# dfs_export.R). Override with env GOLF_MODEL_DIR; default = sibling of the DFS root.
golf_model_dir <- function() {
  d <- Sys.getenv("GOLF_MODEL_DIR", "")
  if (nzchar(d) && dir.exists(d)) return(normalizePath(d))
  cand <- file.path(DFS_ROOT, "..", "golf-modeling")
  if (dir.exists(cand)) return(normalizePath(cand)) else NA_character_
}

# Projection file name for a (tournament, variant, site): tournament "main" = the PGA
# event, "opp" = the opposite-field event; variant "default" = backtest-tuned blend,
# "m80" = 80% model / 20% DataGolf; site "fd" appends _fd (FanDuel-scored). e.g.
# dfs_projections_opp_m80.rds / dfs_projections_fd.rds.
.golf_proj_file <- function(tournament = "main", variant = "default", site = "dk")
  sprintf("dfs_projections%s%s%s.rds", if (identical(tournament, "opp")) "_opp" else "",
          if (identical(variant, "m80")) "_m80" else "",
          if (tolower(site) %in% c("fd", "fanduel")) "_fd" else "")

# Read the user's model projections IF generated for today (else NULL -> regenerate).
.golf_read_model <- function(dir, variant = "default", tournament = "main", site = "dk") {
  f <- file.path(dir, "golf_picks", .golf_proj_file(tournament, variant, site))
  if (!file.exists(f)) return(NULL)
  x <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(x) || is.null(x$projections)) return(NULL)
  if (!identical(x$meta$date, as.character(Sys.Date()))) return(NULL)  # stale -> regenerate
  p <- as.data.table(x$projections); attr(p, "blend") <- x$meta$blend_w_model
  attr(p, "event") <- x$meta$event; p
}

# FanDuel salary map for this slate: the ingested FD CSV snapshot (spine/fd_scrape.R writes
# it to the site='fd' salary path). Returns data.table(norm, salary) keyed by normalized
# name so we can swap the projection file's DataGolf DK salaries for real FD prices.
.golf_fd_salary_map <- function(slate) {
  f <- tryCatch(dk_salary_path("golf", as.Date(slate$date), slate$name %||% "main", "fd"),
                error = function(e) NA_character_)
  if (is.na(f) || !file.exists(f)) return(NULL)
  s <- tryCatch(as.data.table(fread(f)), error = function(e) NULL)
  if (is.null(s) || !all(c("Name", "Salary") %in% names(s))) return(NULL)
  s <- s[, .(norm = norm_name(Name), salary = suppressWarnings(as.integer(Salary)))]
  unique(s[is.finite(salary) & salary > 0], by = "norm")
}

# Event name for a golf tournament slot (for dashboard tab labels), from the file meta.
golf_event_name <- function(tournament = "main") {
  dir <- golf_model_dir(); if (is.na(dir)) return(NULL)
  f <- file.path(dir, "golf_picks", .golf_proj_file(tournament, "default"))
  if (!file.exists(f)) return(NULL)
  tryCatch(readRDS(f)$meta$event, error = function(e) NULL)
}

# Run golf-modeling's dfs_export.R (trains the model, ~1-2 min) to (re)generate today's
# projections. Best-effort; subprocess inherits DATAGOLF_API_KEY from this session.
.golf_run_export <- function(dir) {
  if (!file.exists(file.path(dir, "dfs_export.R"))) return(FALSE)
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  msg("  golf: (re)generating today's model projections via dfs_export.R — trains the model, ~1-2 min...")
  old <- getwd(); on.exit(setwd(old)); setwd(dir)
  code <- tryCatch(system2(rscript, "dfs_export.R", stdout = FALSE, stderr = FALSE),
                   error = function(e) 1L)
  identical(as.integer(code), 0L)
}

# Estimate DK ownership from projection + value when the feed provides none (common for
# opposite-field events, where DataGolf publishes no ownership). Shape mirrors a real golf
# slate: chalk concentrates on studs + value, long tail, normalized so ownership fractions
# sum to the roster size (6) — the same total a real DK single-entry field produces.
.golf_synth_own <- function(proj, salary, n_roster = 6L) {
  ok <- is.finite(proj) & is.finite(salary) & salary > 0
  if (sum(ok) < 2) return(rep(n_roster / max(length(proj), 1), length(proj)))
  z    <- function(v) { s <- stats::sd(v); if (!is.finite(s) || s == 0) return(v * 0); (v - mean(v)) / s }
  val  <- proj / pmax(salary / 1000, 1)                 # points per $1k salary
  score <- 0.7 * z(proj) + 0.9 * z(val)                 # ownership ~ studs + value plays
  w    <- exp(1.15 * score); w[!ok] <- 0
  own  <- n_roster * w / sum(w)
  pmin(own, 0.45)                                        # cap any single-player ownership
}

# Normalize the model projections into the spine pool format.
.golf_model_pool <- function(p, slate) {
  p <- as.data.table(p)
  p[, `:=`(
    player_id = as.integer(dg_id), dk_id = NA_character_, team = NA_character_,
    game_id = NA_character_, position = "G", salary = as.numeric(salary),
    proj = as.numeric(proj), sim_sd = pmax(as.numeric(sim_sd), 4),
    ceil = as.numeric(ceil), floor = pmax(as.numeric(floor), 0),
    own = pmax(as.numeric(own), 0.001), p_zero = 0.01)]          # own already a fraction
  p <- p[!is.na(salary) & salary > 0 & is.finite(proj)]
  # Degenerate ownership (no feed data -> everything at the floor): synthesize an estimate
  # so the field sim / leverage / dupe metrics are meaningful for this event.
  if (nrow(p) && (max(p$own) <= 0.0015 || sum(p$own) < 0.5 * GOLF_ROSTER$n)) {
    p[, own := .golf_synth_own(proj, salary, GOLF_ROSTER$n)]
    msg(sprintf("  golf: feed had no ownership for this event -> using projection/value estimate (top %.0f%%)",
                100 * max(p$own)))
  }
  persist_salaries(p, slate$slate_id, "golf")
  p[, .(player_id, player_name, dk_id, team, game_id, position,
        salary, proj, sim_sd, ceil, floor, p_zero, own)]
}

# DataGolf DFS defaults — the FALLBACK when the user's model isn't available.
.golf_datagolf_pool <- function(slate) {
  raw <- .golf_dg_get("preds/fantasy-projection-defaults",
                      list(tour = "pga", site = "draftkings", slate = "main"))
  if (is.null(raw) || !("projections" %in% names(raw)))
    stop("Could not fetch DataGolf DFS slate (check DATAGOLF_API_KEY / tour).")
  p <- as.data.table(raw$projections)
  p[, `:=`(
    player_id   = as.integer(dg_id), player_name = player_name,
    dk_id       = NA_character_, team = NA_character_, game_id = NA_character_,
    position    = "G", salary = as.numeric(salary),
    proj        = as.numeric(proj_points_total),
    sim_sd      = pmax(as.numeric(std_dev), 4),
    own         = pmax(fcoalesce(as.numeric(proj_ownership), 0.1), 0.1) / 100)]
  p <- p[!is.na(salary) & salary > 0 & !is.na(proj)]
  p[, ceil  := proj + 0.84 * sim_sd]
  p[, floor := pmax(proj - 1.2 * sim_sd, 0)]
  p[, p_zero := 0.01]
  persist_salaries(p, slate$slate_id, "golf")
  p[, .(player_id, player_name, dk_id, team, game_id, position,
        salary, proj, sim_sd, ceil, floor, p_zero, own)]
}

# ── SINGLE-ROUND (Round 2/3/4) support ────────────────────────────────────────
# A 1-day golf contest scores ONLY that round — no tournament finish/placement bonus and
# its own DK salary structure (split by tee wave). DK posts "Round N PGA TOUR" (classic
# gameTypeId 85) + a "Late Round N" wave. Find the classic single-round draft group.
golf_dk_round_group <- function(round = 2L, late = FALSE) {
  j <- tryCatch(httr2::request("https://www.draftkings.com/lobby/getcontests?sport=GOLF") |>
        httr2::req_user_agent("Mozilla/5.0") |> httr2::req_timeout(30) |> httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(j) || is.null(j$DraftGroups) || !nrow(as.data.table(j$DraftGroups))) return(NULL)
  g <- as.data.table(j$DraftGroups)
  # match against BOTH label fields (round + tour live in different ones across events)
  d1 <- if ("DraftGroupSeriesDescription" %in% names(g)) g$DraftGroupSeriesDescription else rep("", nrow(g))
  d2 <- if ("ContestStartTimeSuffix" %in% names(g)) g$ContestStartTimeSuffix else rep("", nrow(g))
  d1[is.na(d1)] <- ""; d2[is.na(d2)] <- ""; desc <- trimws(paste(d1, d2))
  # DK single-round golf = a FLAT 6-golfer "Round N PGA TOUR" slate. Its GameTypeId VARIES
  # by round/wave (seen 84, 85, 86, Late 153...), so don't whitelist IDs — match the name,
  # REQUIRE "PGA TOUR" (not DP World / LPGA / LIV), and exclude the Snake (191) + Birdies /
  # Single-Stat formats. Late waves handled via the `late` flag.
  keep <- grepl(sprintf("Round %d\\b", round), desc, ignore.case = TRUE) &
          grepl("PGA TOUR", desc, ignore.case = TRUE) &
          !grepl("Birdies|Snake|Single Stat", desc, ignore.case = TRUE) &
          (as.integer(g$GameTypeId) != 191L) &
          (grepl("Late", desc, ignore.case = TRUE) == late)
  if (!any(keep)) return(NULL)
  nm <- if (nzchar(d2[keep][1])) d2[keep][1] else d1[keep][1]
  list(draft_group_id = as.character(g$DraftGroupId[keep][1]), name = trimws(nm))
}

# Auto-detect the live DK single-round slate (the round feed uses this): the only
# single-round group posted is the upcoming round, so scan R1..R4 and take the first live.
golf_live_round <- function() {
  for (r in 1:4) { rg <- tryCatch(golf_dk_round_group(r, late = FALSE), error = function(e) NULL)
    if (!is.null(rg)) return(list(round = r, draft_group_id = rg$draft_group_id, name = rg$name)) }
  NULL
}

# Detect the live DK golf CAPTAIN MODE SHOWDOWN group (GameType name ~ "Captain Mode
# Showdown", GameTypeId seen 153). Returns list(draft_group_id, round, name) or NULL.
golf_dk_captain_group <- function() {
  j <- tryCatch(httr2::request("https://www.draftkings.com/lobby/getcontests?sport=GOLF") |>
        httr2::req_user_agent("Mozilla/5.0") |> httr2::req_timeout(30) |> httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(j) || is.null(j$DraftGroups) || !nrow(as.data.table(j$DraftGroups))) return(NULL)
  g <- as.data.table(j$DraftGroups)
  gt <- if (!is.null(j$GameTypes)) as.data.table(j$GameTypes) else NULL
  gtname <- if (!is.null(gt) && all(c("GameTypeId","Name") %in% names(gt)))
    gt$Name[match(as.integer(g$GameTypeId), as.integer(gt$GameTypeId))] else rep("", nrow(g))
  gtname[is.na(gtname)] <- ""
  d2 <- if ("ContestStartTimeSuffix" %in% names(g)) g$ContestStartTimeSuffix else rep("", nrow(g))
  d2[is.na(d2)] <- ""
  keep <- grepl("captain", gtname, ignore.case = TRUE) & grepl("PGA TOUR", d2, ignore.case = TRUE)
  if (!any(keep)) return(NULL)
  # prefer the latest round posted (R4 over R3 when both live)
  rnd <- suppressWarnings(as.integer(sub(".*Round\\s+(\\d+).*", "\\1", d2[keep])))
  ord <- order(-fcoalesce(rnd, 0L)); i <- which(keep)[ord][1]
  list(draft_group_id = as.character(g$DraftGroupId[i]),
       round = suppressWarnings(as.integer(sub(".*Round\\s+(\\d+).*", "\\1", d2[i]))) %||% 4L,
       name = trimws(d2[i]))
}

# Base (UTIL) salaries from a captain-mode draftables feed: each golfer is listed twice
# (CPT row @1.5x + UTIL row @base) -> take the LOWER salary per player = the base price.
.golf_captain_util_salaries <- function(draft_group_id) {
  j <- tryCatch(httr2::request(sprintf("https://api.draftkings.com/draftgroups/v1/draftgroups/%s/draftables", draft_group_id)) |>
        httr2::req_user_agent("Mozilla/5.0") |> httr2::req_timeout(30) |> httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(j) || is.null(j$draftables) || !nrow(as.data.table(j$draftables))) return(NULL)
  d <- as.data.table(j$draftables)[!is.na(salary) & salary > 0]
  s <- d[, .(salary = as.integer(min(salary))), by = .(player_name = displayName)]
  s[, norm := norm_name(player_name)][]
}

# Build the base pool for the golf CAPTAIN showdown: captain-slate base salaries joined to
# the round-scoring model projections for the detected round (R4 blends finish/placement
# points via .golf_round_model_pool). Order/accent-insensitive name match. NULL if unready.
golf_captain_base <- function(cg, date = Sys.Date()) {
  cs <- .golf_captain_util_salaries(cg$draft_group_id)
  if (is.null(cs) || !nrow(cs)) return(NULL)
  rnd <- cg$round; if (is.null(rnd) || is.na(rnd)) rnd <- 4L
  rg  <- tryCatch(golf_dk_round_group(rnd), error = function(e) NULL)
  if (is.null(rg)) rg <- tryCatch(golf_dk_round_group(rnd, late = TRUE), error = function(e) NULL)
  slate <- list(sport = "golf", site = "dk", date = as.character(date), round = rnd,
                slate_type = "single_round", tournament = "main",
                slate_id = make_slate_id("golf", "dk", date, paste0("captain_r", rnd)))
  pp <- tryCatch(.golf_round_model_pool(if (!is.null(rg)) rg$draft_group_id else cg$draft_group_id, slate, rnd),
                 error = function(e) NULL)
  if (is.null(pp) || !nrow(pp)) return(NULL)
  pp <- as.data.table(pp)
  .ckey <- function(x) vapply(as.character(x), function(s) { s <- iconv(s, to = "ASCII//TRANSLIT")
    if (is.na(s) || !nzchar(s)) s <- ""
    t <- strsplit(gsub("[^a-z ]", " ", tolower(s)), "\\s+")[[1]]; t <- t[nzchar(t)]
    paste(sort(t), collapse = " ") }, character(1))
  pp[, k := .ckey(player_name)]; cs2 <- copy(cs)[, k := .ckey(player_name)]
  keep <- setdiff(names(pp), c("salary", "team", "norm"))
  base <- merge(pp[, ..keep], cs2[, .(k, salary)], by = "k")
  if (!nrow(base)) return(NULL)
  base[, `:=`(k = NULL, player_id = .I, position = "G", team = "")]
  base[]
}

# DK single-round salaries for a round draft group -> data.table(player_name, salary, norm).
.golf_round_dk_salaries <- function(draft_group_id) {
  j <- tryCatch(httr2::request(sprintf("https://api.draftkings.com/draftgroups/v1/draftgroups/%s/draftables", draft_group_id)) |>
        httr2::req_user_agent("Mozilla/5.0") |> httr2::req_timeout(30) |> httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(j) || is.null(j$draftables) || !nrow(as.data.table(j$draftables))) return(NULL)
  d <- unique(as.data.table(j$draftables)[!is.na(salary) & salary > 0,
        .(player_name = displayName, salary = as.integer(salary))], by = "player_name")
  d[, norm := norm_name(player_name)][]
}

# (re)generate the v2-round projection export (per-tee-time wind + FRL) via golf-modeling's
# engine/round_sim.R --export, so the single-round pool uses the good engine even from the
# dashboard. Best-effort subprocess; ~30s. Returns TRUE on success.
.golf_run_round_export <- function(dir, round, late = FALSE) {
  rsim <- file.path(dir, "engine", "round_sim.R"); if (!file.exists(rsim)) return(FALSE)
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  msg(sprintf("  golf: exporting v2-round projection (round %d) — per-tee-time wind, ~30s...", round))
  old <- getwd(); on.exit(setwd(old)); setwd(dir)
  code <- tryCatch(system2(rscript, c(shQuote(rsim), "--round", round, "--export",
                                       if (late) "--late" else character(0)), stdout = FALSE, stderr = FALSE),
                   error = function(e) 1L)
  identical(as.integer(code), 0L)
}

# PREFERRED single-round pool: the exported v2-round projection (per-tee-time wind + FRL,
# from golf-modeling engine/round_sim.R export_round_projection) joined to DK single-round
# salaries. NULL if the export is missing / stale / for a different round -> caller falls
# back to the DataGolf round pool. This is the single-round analog of .golf_model_pool.
.golf_round_model_pool <- function(draft_group_id, slate, round = 2L) {
  dir <- golf_model_dir(); if (is.na(dir)) return(NULL)
  # EVERY round uses its own ROUND-SCORING model (round_sim, per-tee-time wind, modest form
  # fold) as the PRIMARY driver — so the board is NOT just the current standings leaders.
  f <- file.path(dir, "golf_picks", "dfs_round_projections.rds")
  if (!file.exists(f)) return(NULL)
  x <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(x) || is.null(x$projections)) return(NULL)
  if (!identical(x$meta$date, as.character(Sys.Date())) ||
      !identical(as.integer(x$meta$round), as.integer(round))) return(NULL)   # stale / wrong round
  p <- as.data.table(x$projections)
  # R4 also scores tournament FINISH position -> blend only a MODEST 30% finish signal from the
  # finish-aware tournament projection; round scoring stays 70% (leaders don't dominate).
  if (as.integer(round) >= 4L) {
    xt <- tryCatch(readRDS(file.path(dir, "golf_picks", "dfs_projections.rds")), error = function(e) NULL)
    if (!is.null(xt) && !is.null(xt$projections)) {
      p <- merge(p, as.data.table(xt$projections)[, .(dg_id, tproj = proj)], by = "dg_id", all.x = TRUE)
      .z <- function(v) { s <- sd(v, na.rm = TRUE); if (!is.finite(s) || s == 0) v * 0 else (v - mean(v, na.rm = TRUE)) / s }
      p[is.finite(tproj), proj := mean(proj) + sd(proj) * (0.70 * .z(proj) + 0.30 * .z(tproj))]
    }
  }
  sal <- .golf_round_dk_salaries(draft_group_id); if (is.null(sal) || !nrow(sal)) return(NULL)
  p[, norm := norm_name(player_name)]
  d <- merge(sal, p[, setdiff(names(p), c("player_name", "salary", "tproj")), with = FALSE], by = "norm")  # DK name + DK single-round salary
  d <- d[is.finite(proj) & salary > 0]; if (!nrow(d)) return(NULL)
  d[, `:=`(player_id = surrogate_player_id(norm), dk_id = NA_character_, team = NA_character_,
           game_id = NA_character_, position = "G", sim_sd = pmax(as.numeric(sim_sd), 4),
           ceil = as.numeric(ceil), floor = pmax(as.numeric(floor), 0),
           own = pmax(as.numeric(own), 0.001), p_zero = 0.01)]
  if (max(d$own) <= 0.0015 || sum(d$own) < 0.5 * GOLF_ROSTER$n)
    d[, own := .golf_synth_own(proj, salary, GOLF_ROSTER$n)]
  msg(sprintf("  golf: single-round (Round %d) using YOUR v2-round model (%s) — %d golfers, per-tee-time wind%s",
              round, x$meta$event %||% "slate", nrow(d), if (round == 1L) " + FRL" else ""))
  persist_salaries(d, slate$slate_id, "golf")
  d[, .(player_id, player_name, dk_id, team, game_id, position, salary, proj, sim_sd, ceil, floor, p_zero, own)]
}

# Round >= 2: fold COMPLETED-round results into the per-round projection using DataGolf's
# live in-play feed. Golfers who beat the field in the rounds already played get a modest,
# REGRESSED boost (hot form + revealed course fit); laggards a small discount. For R3/R4 we
# also drop anyone who missed the cut / withdrew. The R1->R2 signal is weak, so the weight is
# deliberately small and capped ±20%. Best-effort: any schema/feed miss leaves the
# pre-tournament projection untouched (falls back to talent-only scoring).
.golf_round_form_adjust <- function(d, round) {
  if (round < 2L) return(d)
  ip  <- tryCatch(.golf_dg_get("preds/in-play", list(tour = "pga")), error = function(e) NULL)
  ipd <- tryCatch(as.data.table(ip$data), error = function(e) NULL)
  if (is.null(ipd) || !nrow(ipd) || !"player_name" %in% names(ipd)) { msg("  golf: no in-play feed — completed-round form NOT applied"); return(d) }
  ipd[, norm := norm_name(player_name)]
  score_col <- intersect(c("current_score", "total", "score"), names(ipd))[1]
  pos_col   <- intersect(c("current_pos", "position", "pos"), names(ipd))[1]
  if (is.na(score_col)) { msg("  golf: in-play feed missing score column — form NOT applied"); return(d) }
  ipd[, par := suppressWarnings(as.numeric(get(score_col)))]
  ipd[, posx := if (!is.na(pos_col)) toupper(as.character(get(pos_col))) else NA_character_]
  d <- merge(d, ipd[, .(norm, par, posx)], by = "norm", all.x = TRUE)
  if (round >= 3L) d <- d[is.na(posx) | !grepl("CUT|WD|DQ", posx)]       # cut/withdrawn are out for R3/R4
  fld <- mean(d$par, na.rm = TRUE)
  if (!is.finite(fld) || sum(!is.na(d$par)) < 10L) { d[, c("par", "posx") := NULL]; return(d) }
  d[, form := fcoalesce(par - fld, 0)]                                    # strokes vs field so far (neg = beating field)
  W <- 0.04                                                               # ~4% proj swing per stroke, capped ±20%
  d[, proj := round(pmax(proj * (1 - pmax(pmin(W * form, 0.20), -0.20)), 0.5), 2)]
  msg(sprintf("  golf: applied completed-round form (through R%d) to %d golfers from in-play feed", round - 1L, sum(!is.na(d$par))))
  d[, c("par", "posx", "form") := NULL]; d[]
}

# Build the single-round pool: DK single-round SALARIES (from the round draft group) +
# per-round projection from DataGolf's SCORING points (proj_points_scoring / 4 — no finish
# bonus). One round is boom/bust, so relative variance is higher than the full tournament.
.golf_round_pool <- function(draft_group_id, slate, round = 2L) {
  j <- tryCatch(httr2::request(sprintf("https://api.draftkings.com/draftgroups/v1/draftgroups/%s/draftables", draft_group_id)) |>
        httr2::req_user_agent("Mozilla/5.0") |> httr2::req_timeout(30) |> httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(j) || is.null(j$draftables) || !nrow(as.data.table(j$draftables))) stop("no DK draftables for round group ", draft_group_id)
  d <- unique(as.data.table(j$draftables)[!is.na(salary) & salary > 0,
        .(player_name = displayName, salary = as.integer(salary))], by = "player_name")
  d[, norm := norm_name(player_name)]
  raw <- .golf_dg_get("preds/fantasy-projection-defaults", list(tour = "pga", site = "draftkings", slate = "main"))
  if (is.null(raw) || is.null(raw$projections)) stop("no DataGolf projections for single-round")
  dg <- as.data.table(raw$projections); dg[, norm := norm_name(player_name)]
  d <- merge(d, dg[, .(norm, sc = as.numeric(proj_points_scoring), own = as.numeric(proj_ownership))], by = "norm", all.x = TRUE)
  d <- d[!is.na(sc) & sc > 0]
  d[, `:=`(player_id = surrogate_player_id(norm), dk_id = NA_character_, team = NA_character_,
           game_id = NA_character_, position = "G", proj = round(sc / 4, 2))]
  d <- .golf_round_form_adjust(d, round)                       # R>=2: fold completed-round results into proj
  d[, sim_sd := pmax(0.55 * proj, 4)]                          # single round = higher relative variance
  d[, `:=`(ceil = proj + 0.9 * sim_sd, floor = pmax(proj - 1.1 * sim_sd, 0), p_zero = 0.01)]
  d[, own := pmax(fcoalesce(own, 0.1), 0.1) / 100]             # DataGolf tournament ownership as a proxy
  # feed had no per-round ownership -> synthesize if degenerate (same guard as opp events)
  if (nrow(d) && (max(d$own) <= 0.0015 || sum(d$own) < 0.5 * GOLF_ROSTER$n))
    d[, own := .golf_synth_own(proj, salary, GOLF_ROSTER$n)]
  msg(sprintf("  golf: single-round (Round %d) slate — %d golfers, DK single-round salaries", round, nrow(d)))
  persist_salaries(d, slate$slate_id, "golf")
  d[, .(player_id, player_name, dk_id, team, game_id, position, salary, proj, sim_sd, ceil, floor, p_zero, own)]
}

# Projections: prefer the USER'S golf-modeling DFS model (its backtest-tuned model+DG
# blend); regenerate today's file if stale; fall back to DataGolf defaults if the model
# can't be produced. Set GOLF_MODEL_AUTORUN=0 to skip the auto-regeneration subprocess.
golf_project_players <- function(slate) {
  # single-round (Round 2/3/4) 1-day contest — its own DK salaries + per-round projection
  if (isTRUE(slate$single_round)) {
    rg <- if (!is.null(slate$draft_group_id)) list(draft_group_id = slate$draft_group_id)
          else golf_dk_round_group(slate$round %||% 2L, late = isTRUE(slate$late))
    if (is.null(rg)) { msg(sprintf("  golf: no DK Round %d slate posted yet", slate$round %||% 2L)); return(NULL) }
    rnd <- slate$round %||% 2L
    # prefer the exported v2-round model (per-tee-time wind + FRL); auto-(re)export if stale;
    # fall back to the DataGolf round pool if the export can't be produced.
    pool <- .golf_round_model_pool(rg$draft_group_id, slate, rnd)
    if (is.null(pool) && !identical(Sys.getenv("GOLF_MODEL_AUTORUN"), "0")) {
      dir <- golf_model_dir()
      if (!is.na(dir) && .golf_run_round_export(dir, rnd, isTRUE(slate$late)))
        pool <- .golf_round_model_pool(rg$draft_group_id, slate, rnd)
    }
    if (is.null(pool)) pool <- .golf_round_pool(rg$draft_group_id, slate, rnd)
    return(pool)
  }
  variant    <- slate$variant %||% "default"
  tournament <- slate$tournament %||% "main"           # "main" = PGA event, "opp" = opposite-field
  fd         <- tolower(slate$site %||% "dk") %in% c("fd", "fanduel")
  dir <- golf_model_dir()
  # FanDuel: prefer the FD-scored projections file; fall back to the DK-scored file (proxy)
  # if the FD re-score hasn't run yet. DK path is unchanged (site = "dk").
  m <- if (!is.na(dir)) .golf_read_model(dir, variant, tournament, if (fd) "fd" else "dk") else NULL
  if (is.null(m) && !is.na(dir) && !identical(Sys.getenv("GOLF_MODEL_AUTORUN"), "0")) {
    if (.golf_run_export(dir))
      m <- .golf_read_model(dir, variant, tournament, if (fd) "fd" else "dk")  # writes all events x blends x sites
  }
  fd_scored <- fd && !is.null(m) && nrow(m)
  if (fd && is.null(m)) {                                # FD re-score unavailable -> DK-scored proxy
    m <- if (!is.na(dir)) .golf_read_model(dir, variant, tournament, "dk") else NULL
    if (!is.null(m) && nrow(m)) msg("  golf(FanDuel): FD-scored file not ready -> using DK-scored proxy (rankings valid, scale is DK points)")
  }
  if (!is.null(m) && nrow(m)) {
    if (fd) {                                            # swap DataGolf DK salaries for FD prices
      sm <- .golf_fd_salary_map(slate)
      if (is.null(sm)) { msg("  golf(FanDuel): no ingested FD salary CSV for this slate — run fd_ingest_latest('golf')"); return(NULL) }
      m <- as.data.table(copy(m)); m[, norm := norm_name(player_name)]
      m <- merge(m[, setdiff(names(m), "salary"), with = FALSE], sm, by = "norm")[, norm := NULL]
      if (!nrow(m)) { msg("  golf(FanDuel): no projection<->FD-salary name matches"); return(NULL) }
    }
    msg(sprintf("  golf: using YOUR DFS model [%s/%s/%s] — %s, %d players (%s)",
                tournament, variant, if (fd) "FanDuel" else "DraftKings",
                attr(m, "event") %||% "slate", nrow(m),
                if (fd) (if (fd_scored) "FD-scored" else "DK-scored proxy") else "DK-scored"))
    return(.golf_model_pool(m, slate))
  }
  # DataGolf fallback: only the MAIN PGA event is available via the default endpoint.
  if (identical(tournament, "opp")) { msg("  golf(opp): no opposite-field model file yet — skipping"); return(NULL) }
  msg("  golf: model projections unavailable -> DataGolf defaults (fallback)")
  .golf_datagolf_pool(slate)
}

register_sport("golf", list(
  ingest          = NULL,                  # ingestion handled by golf-modeling
  project_players = golf_project_players,
  correlation     = NULL,                  # low inter-player correlation
  roster_rules    = GOLF_ROSTER,
  roster_rules_fd = GOLF_ROSTER_FD,
  dk_scoring      = NULL
))
