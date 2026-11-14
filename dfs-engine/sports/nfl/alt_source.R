# ==============================================================================
# NFL plugin — SECOND-OPINION projections (Sleeper, free public API) (2026-11-13)
#
# MobyWhaler builds his pool off one site's projections and sims it through another's,
# because sites disagree a lot (especially on Thursday) and the disagreements are where
# news hasn't been priced in yet. We don't BLEND (our model is validated, theirs isn't, on
# our data) -- we FLAG: the build report lists players where the two sources differ by
# >= max(4 pts, 35%). Real example that motivated it, 2026-11-13: Sleeper had Corey Kiner
# at 11.7 PPR on 13.6 carries and no projection at all for Rhamondre Stevenson (i.e. they
# treat him as out); we had Kiner 5.5.
#
# Sleeper scores PPR without DK's yardage bonuses -- close enough for a disagreement flag.
# Cached ~6h; NULL on any failure (never blocks a build).
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

nfl_alt_projections <- function(max_age_hours = 6) {
  p <- dfs_path("data", "raw", "nfl_sleeper_proj.rds")
  fresh <- file.exists(p) && difftime(Sys.time(), file.info(p)$mtime, units = "hours") < max_age_hours
  if (!fresh) {
    get <- function(u) tryCatch(httr2::request(u) |> httr2::req_user_agent("DFS-ENGINE/1.0") |>
                                  httr2::req_timeout(30) |> httr2::req_perform() |>
                                  httr2::resp_body_json(simplifyVector = FALSE), error = function(e) NULL)
    st <- get("https://api.sleeper.app/v1/state/nfl")
    if (!is.null(st) && !is.null(st$week) && !is.null(st$season)) {
      stype <- if (identical(st$season_type, "post")) "post" else "regular"
      u <- sprintf("https://api.sleeper.app/projections/nfl/%s/%s?season_type=%s&position[]=QB&position[]=RB&position[]=WR&position[]=TE",
                   st$season, st$week, stype)
      j <- get(u)
      if (!is.null(j) && length(j)) {
        d <- rbindlist(lapply(j, function(x) {
          pl <- x$player %||% list(); s <- x$stats %||% list()
          data.table(name = paste(pl$first_name %||% "", pl$last_name %||% ""), team = pl$team %||% NA_character_,
                     position = pl$position %||% NA_character_, alt = suppressWarnings(as.numeric(s$pts_ppr %||% NA)))
        }), fill = TRUE)
        d <- d[nzchar(trimws(name))][, norm := norm_name(name)]
        setorder(d, -alt, na.last = TRUE); d <- unique(d, by = "norm")
        attr(d, "week") <- st$week
        dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); saveRDS(d, p)
      }
    }
  }
  if (!file.exists(p)) return(NULL)
  tryCatch(as.data.table(readRDS(p)), error = function(e) NULL)
}
