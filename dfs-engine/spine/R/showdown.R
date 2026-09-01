# ==============================================================================
# DFS ENGINE — Showdown / Captain Mode (single-game contests)
# DK Showdown: 1 CAPTAIN (1.5x points AND 1.5x salary) + 5 FLEX, all from ONE game.
# The captain pick is the whole game — it's the biggest leverage decision in DFS.
#
# We expand the base pool into CPT + FLEX rows, simulate on the BASE players (so a
# captain's score is exactly 1.5x that player's real outcome), optimize/grade with
# the group-uniqueness constraint (no one captained AND flexed), and surface a
# CAPTAIN BOARD ranking who to put in the CPT slot.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

showdown_roster_rules <- function(cap = 50000L) {
  list(n = 6L, cap = as.integer(cap), slots = c(CPT = 1L, FLEX = 5L),
       group_unique = "base_id",
       slot_labels = c("CPT", "FLEX", "FLEX", "FLEX", "FLEX", "FLEX"))
}

# expand a projected base pool -> CPT + FLEX rows (CPT scaled by cpt_mult).
expand_showdown_pool <- function(base, cpt_mult = 1.5) {
  B <- as.data.table(copy(base))
  if (!"ceil"  %in% names(B)) B[, ceil := proj]
  if (!"floor" %in% names(B)) B[, floor := pmax(proj - 1.2 * sim_sd, 0)]
  if (!"own"   %in% names(B)) B[, own := pmax(proj / sum(proj), 1e-3)]
  if (!"p_zero" %in% names(B)) B[, p_zero := 0.03]

  flex <- copy(B)[, `:=`(slot = "FLEX", base_id = player_id, real_pos = position, position = "FLEX")]
  cpt  <- copy(B)[, `:=`(slot = "CPT", base_id = player_id, real_pos = position, position = "CPT",
                         salary = as.integer(round(cpt_mult * salary)),
                         proj = cpt_mult * proj, sim_sd = cpt_mult * sim_sd,
                         ceil = cpt_mult * ceil, floor = cpt_mult * floor)]
  # ownership: FLEX ~ base own; CPT ~ projection share (exactly one captain per lineup)
  cpt[,  own := pmax(proj / sum(proj), 1e-3)]
  E <- rbind(flex, cpt, fill = TRUE)
  E[, player_id := .I]                       # distinct row ids for sim/field/dupe
  E[]
}

# sim on the BASE players, then expand: CPT row = cpt_mult x that player's draw.
showdown_sim <- function(base, exp_pool, base_sim, cpt_mult = 1.5) {
  bi <- match(exp_pool$base_id, base$player_id)
  mult <- ifelse(exp_pool$slot == "CPT", cpt_mult, 1)
  list(scores = base_sim$scores[bi, , drop = FALSE] * mult,
       pool = exp_pool, n_sims = base_sim$n_sims)
}

# rank who to CAPTAIN. Returns a board: best raw captains (ceiling) + leverage.
captain_board <- function(base, cpt_mult = 1.5, n = 10L) {
  P <- as.data.table(copy(base))
  if (!"ceil" %in% names(P)) P[, ceil := proj]
  P[, `:=`(cpt_salary = as.integer(round(cpt_mult * salary)),
           cpt_proj = round(cpt_mult * proj, 1), cpt_ceil = round(cpt_mult * ceil, 1),
           cpt_own = round(100 * pmax(proj / sum(proj), 1e-3), 1))]
  P[, leverage := round(frank(cpt_ceil) / .N - frank(cpt_own) / .N, 3)]
  setorder(P, -cpt_ceil)
  P[seq_len(min(n, .N)), .(player = player_name, team, base_salary = salary, cpt_salary,
     cpt_proj, cpt_ceil, cpt_own, leverage)]
}

# the captain in a built showdown lineup (the CPT row).
lineup_captain <- function(pool, idx) {
  P <- as.data.table(pool); cpt <- idx[P$slot[idx] == "CPT"]
  if (length(cpt)) P$player_name[cpt[1]] else NA_character_
}

infer_sport_from_contest <- function(contest) {
  nm <- tolower(paste(contest$name %||% "", contest$sport %||% ""))
  if (grepl("wnba", nm)) return("wnba")
  if (grepl("ncaaf|cfb|college foot", nm)) return("ncaaf")
  if (grepl("\\bnfl\\b", nm)) return("nfl")
  if (grepl("tennis|\\bten\\b", nm)) return("tennis")   # DK abbreviates tennis as "TEN $..."
  if (grepl("pga|golf", nm)) return("golf")
  if (grepl("nba", nm)) return("nba")
  tolower(contest$sport %||% "")
}

# Full contest pipeline: real payouts + Showdown captain optimization (or classic).
run_contest <- function(contest_id, sport = NULL, date = Sys.Date(), n_lineups = 8L,
                        n_sims = 10000L, n_cand = 300L, field_n = 1500L, seed = 2026L) {
  set.seed(seed)
  contest <- dk_contest(contest_id)
  sport <- sport %||% infer_sport_from_contest(contest)
  if (sport %in% c("", "mlb", "baseball", "softball"))
    stop("Unsupported/blocked sport for this contest: '", sport, "'")
  dfs_load_sport(sport); spec <- get_sport(sport); gates <- load_gates(sport)
  if (is.null(spec$project_players)) stop("Sport '", sport, "' has no project_players() yet.")

  pool_raw <- dk_contest_pool(contest, sport, date, slate = contest$contest_id)
  showdown <- isTRUE(attr(pool_raw, "showdown")); cpt_mult <- attr(pool_raw, "cpt_mult") %||% 1.5
  write_dk_snapshot_csv(pool_raw, sport, date, slate = contest$contest_id)
  slate_obj <- list(sport = sport, site = "dk", date = as.character(date), name = contest$contest_id,
                    slate_id = make_slate_id(sport, "dk", date, contest$contest_id))
  base  <- validate_projection(as.data.table(spec$project_players(slate_obj)))
  tryCatch(persist_projected_ownership(base, slate_obj$slate_id, sport), error = function(e) NULL)
  curve <- dk_contest_curve(contest)
  fsize <- if (is.finite(contest$field_size)) contest$field_size else field_n

  if (showdown) {
    rr <- showdown_roster_rules(cap = 50000L)
    exp_pool <- expand_showdown_pool(base, cpt_mult)
    bsim  <- slate_sim(base, get_loadings(base, sport), team_loadings = get_team_loadings(base, sport), n_sims = n_sims, seed = seed)
    esim  <- showdown_sim(base, exp_pool, bsim, cpt_mult)
    field <- simulate_field(exp_pool, rr, field_n = field_n, own = exp_pool$own)
    cands <- make_candidates(exp_pool, rr, n_cand = n_cand)
    res   <- grade_candidates(cands, esim, field, curve_gpp = curve, curve_cash = curve, field_size = fsize)
    picks <- build_portfolio(res, gates, n = n_lineups, max_overlap = 5L)
    cap_board <- captain_board(base, cpt_mult)
    pool_out <- exp_pool
  } else {
    rr <- spec$roster_rules
    sim   <- slate_sim(base, get_loadings(base, sport), team_loadings = get_team_loadings(base, sport), n_sims = n_sims, seed = seed)
    field <- simulate_field(base, rr, field_n = field_n, own = base$own)
    cands <- make_candidates(base, rr, n_cand = n_cand)
    res   <- grade_candidates(cands, sim, field, curve_gpp = curve, curve_cash = make_double_up(), field_size = fsize)
    picks <- build_portfolio(res, gates, n = n_lineups)
    cap_board <- NULL; pool_out <- base
  }
  msg(sprintf("Contest %s (%s%s): %d lineups, field %s, $%s entry",
              contest$contest_id, sport, if (showdown) " SHOWDOWN" else "",
              length(picks), fsize, contest$entry_fee))
  list(contest = contest, sport = sport, showdown = showdown, slate_id = slate_obj$slate_id,
       pool = pool_out, base_pool = base, picks = picks, res = res, gates = gates, rr = rr,
       captain_board = cap_board, curve = curve)
}
