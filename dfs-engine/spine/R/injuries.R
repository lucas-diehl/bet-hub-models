# ==============================================================================
# DFS ENGINE — injury / status feed (FREE, no key)
# ESPN's public injuries endpoint gives current player status (Out / Day-To-Day /
# Questionable) per team. Two uses: (1) never roster a confirmed-OUT player the DK
# feed hasn't caught yet, and (2) MINUTES REDISTRIBUTION — when a starter sits, the
# minutes-first model under-projects the teammates who absorb their minutes/usage.
# Generic across sports (WNBA now; NBA/NFL when those plugins land).
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# ESPN status strings that mean "will not play" (DROP) vs "uncertain" (DISCOUNT).
.INJ_OUT <- c("out", "injured reserve", "ir", "suspension", "suspended", "not with team",
              "personal", "doubtful", "inactive", "o")
.INJ_Q   <- c("questionable", "day-to-day", "game-time decision", "gtd", "q")

# Pull current injuries for a sport -> data.table(norm, player_name, team_abbr,
# status, is_out). Uses the same .ESPN_SPORT map as the Vegas feed.
injury_report <- function(sport) {
  path <- if (sport %in% names(.ESPN_SPORT)) .ESPN_SPORT[[sport]] else NULL
  if (is.null(path)) return(NULL)
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/%s/injuries", path)
  resp <- tryCatch(httr2::request(url) |> httr2::req_user_agent("DFS-ENGINE/1.0") |>
                     httr2::req_timeout(30) |> httr2::req_perform(), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) != 200) return(NULL)
  j <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(j) || is.null(j$injuries)) return(NULL)
  rows <- rbindlist(lapply(j$injuries, function(team) {
    ta <- team$abbreviation %||% team$displayName %||% NA_character_
    rbindlist(lapply(team$injuries %||% list(), function(inj) {
      nm <- inj$athlete$displayName %||% NA_character_
      st <- tolower(inj$status %||% "")
      data.table(player_name = nm, team_abbr = ta, status = inj$status %||% "",
                 is_out = st %in% .INJ_OUT)
    }), fill = TRUE)
  }), fill = TRUE)
  if (is.null(rows) || !nrow(rows)) return(NULL)
  rows <- rows[!is.na(player_name)]
  rows[, norm := norm_name(player_name)]
  rows[, st := tolower(trimws(status))]
  rows[, cat := fifelse(st %in% .INJ_OUT, "out", fifelse(st %in% .INJ_Q, "questionable", "active"))]
  unique(rows, by = "norm")
}

# THE INACTIVES SAFEGUARD — the #1 live error across team sports is projecting a DK-slate
# player who won't play (WNBA game-time scratches, NFL Sunday inactives). This applies the
# FRESHEST status feed to a projection pool: DROP confirmed-inactive players (Out/Doubtful/
# IR/suspended) so they can't be rostered, and DISCOUNT the uncertain (Questionable/GTD) by
# cutting their projection + raising DNP risk. Sport-agnostic and a NO-OP when there's no
# feed (golf/tennis). Match is by normalized name (team disambiguation when both sides carry
# a team). Runs in the pipeline for every sport, so it protects WNBA and NFL alike.
#   Effectiveness depends on FRESHNESS — build/refresh close to lock so inactives are known.
# Manual OUT overrides (config/manual_scratches.json) for sports with no auto injury feed
# (tennis/golf WDs posted by DK). Returns a character vector of normalized names, or NULL.
load_manual_scratches <- function(sport) {
  f <- dfs_path("config", "manual_scratches.json")
  if (!file.exists(f)) return(NULL)
  j <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  o <- if (!is.null(j)) j[[sport]] else NULL
  if (is.null(o) || !length(o)) return(NULL)
  v <- unlist(o, use.names = FALSE); v <- v[nzchar(v)]
  if (length(v)) norm_name(v) else NULL
}

apply_inactives <- function(pool, sport, as_of = Sys.Date(), q_discount = 0.6) {
  inj <- tryCatch(injury_report(sport), error = function(e) NULL)
  man <- tryCatch(load_manual_scratches(sport), error = function(e) NULL)
  if ((is.null(inj) || !nrow(inj)) && is.null(man)) return(pool)
  pool <- as.data.table(pool)
  if (!"norm" %in% names(pool)) pool[, norm := norm_name(player_name)]
  out_norms <- unique(c(if (!is.null(inj) && nrow(inj)) inj[cat == "out", norm] else character(0),
                        man %||% character(0)))
  q_norms   <- if (!is.null(inj) && nrow(inj)) inj[cat == "questionable", norm] else character(0)

  drop <- pool$norm %in% out_norms
  dropped <- if (any(drop)) pool$player_name[drop] else character(0)
  pool <- pool[!drop]

  qi <- pool$norm %in% q_norms
  if (any(qi)) {
    for (c in intersect(c("proj", "ceil", "floor"), names(pool)))
      pool[[c]][qi] <- pool[[c]][qi] * q_discount
    if ("p_zero" %in% names(pool)) pool[["p_zero"]][qi] <- pmin(pool[["p_zero"]][qi] + 0.30, 0.65)
  }
  if (length(dropped) || any(qi))
    msg(sprintf("  inactives[%s]: dropped %d OUT%s, discounted %d questionable (feed as of build time)",
                sport, length(dropped),
                if (length(dropped)) paste0(" (", paste(head(dropped, 5), collapse = ", "),
                                            if (length(dropped) > 5) ", …" else "", ")") else "",
                sum(qi)))
  pool[]
}
