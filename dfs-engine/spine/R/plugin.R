# ==============================================================================
# DFS ENGINE — sport-plugin contract + registry
# The ONE abstraction that lets one person run many sports. The spine core
# (slate_sim / field_sim / optimizer / validate) calls ONLY these functions.
# If adding a sport forces a change to the core, the abstraction is wrong.
# ==============================================================================

.DFS_SPORTS <- new.env(parent = emptyenv())

# A sport registers a list with this contract:
#
#   ingest          = function(date_or_range) -> writes raw + normalized tables, returns slate_id(s)
#   project_players = function(slate) -> data.table(player_id, salary, team, game_id,
#                                                    proj, sim_sd, ceil, floor, p_zero,
#                                                    own (optional projected ownership))
#   correlation     = function(slate) -> data.table(player_id, game_id, factor_load)
#                       (loading on that game's shared environment factor; opposite signs
#                        encode anti-correlation, e.g. tennis opponents). NULL = independent.
#   roster_rules    = list(
#                       n          = total roster size,
#                       cap        = salary cap,
#                       floor      = optional min salary,
#                       slots      = named int vector of position -> count, or NULL for flat-n,
#                       flex       = optional char vector of positions a FLEX can fill,
#                       team_limit = optional max players per team,
#                       max_per_game = optional cap (e.g. tennis: <=1 per match) )
#   dk_scoring      = function(box) -> numeric DK fantasy points (box = data.frame of stats)
#
# Only `roster_rules` and at least one of {project_players} are strictly required to
# run the optimizer/sim; the rest enable ingestion and grading.

register_sport <- function(sport, spec) {
  required <- "roster_rules"
  miss <- setdiff(required, names(spec))
  if (length(miss)) stop(sprintf("Sport '%s' missing contract field(s): %s",
                                  sport, paste(miss, collapse = ", ")))
  rr <- spec$roster_rules
  if (is.null(rr$n) || is.null(rr$cap))
    stop(sprintf("Sport '%s' roster_rules must include n and cap", sport))
  spec$sport <- sport
  assign(sport, spec, envir = .DFS_SPORTS)
  invisible(spec)
}

get_sport <- function(sport) {
  if (!exists(sport, envir = .DFS_SPORTS, inherits = FALSE)) {
    # try to load it on demand
    if (exists("dfs_load_sport")) dfs_load_sport(sport)
  }
  if (!exists(sport, envir = .DFS_SPORTS, inherits = FALSE))
    stop(sprintf("Sport '%s' is not registered. Did its plugin call register_sport()?", sport))
  get(sport, envir = .DFS_SPORTS, inherits = FALSE)
}

list_sports <- function() ls(.DFS_SPORTS)

# ── site-aware accessors (DraftKings default; FanDuel opt-in) ──────────────────
# A plugin may register FanDuel variants as `roster_rules_fd` and `fd_scoring`
# alongside the DK `roster_rules` / `dk_scoring`. These helpers return the right one
# for the slate's site so the core (run_slate/optimizer/scoring) stays site-agnostic.
sport_roster <- function(sport, site = "dk") {
  spec <- get_sport(sport)
  if (tolower(site) %in% c("fd", "fanduel") && !is.null(spec$roster_rules_fd)) spec$roster_rules_fd
  else spec$roster_rules
}
sport_scoring <- function(sport, site = "dk") {
  spec <- get_sport(sport)
  if (tolower(site) %in% c("fd", "fanduel") && !is.null(spec$fd_scoring)) spec$fd_scoring
  else spec$dk_scoring
}

# Validate that a projection table satisfies the contract before it hits the sim.
# Mirrors the golf pipeline's defensive `fixc` guard: no NA/NaN/Inf may reach the
# simulator or ILP objective.
validate_projection <- function(dt) {
  req <- c("player_id", "salary", "proj", "sim_sd")
  miss <- setdiff(req, names(dt))
  if (length(miss)) stop("projection table missing columns: ", paste(miss, collapse = ", "))
  for (col in c("proj", "sim_sd", "ceil", "floor")) {
    if (col %in% names(dt)) {
      v <- dt[[col]]
      if (any(!is.finite(v))) {
        fb <- stats::median(v[is.finite(v)]); if (!is.finite(fb)) fb <- 0
        dt[[col]][!is.finite(dt[[col]])] <- fb
      }
    }
  }
  if ("sim_sd" %in% names(dt)) dt[["sim_sd"]][dt[["sim_sd"]] <= 0] <- stats::median(dt[["sim_sd"]][dt[["sim_sd"]] > 0])
  dt
}
