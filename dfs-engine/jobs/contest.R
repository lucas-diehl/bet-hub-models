#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — analyze a specific DK contest (real payouts; Showdown -> captains).
#   Rscript jobs/contest.R --url "https://www.draftkings.com/draft/contest/191610408"
#   Rscript jobs/contest.R --contest 191610408 [--sport wnba] [--nlineups 6] [--bankroll 300]
# Prints the captain board + lineups and adds the contest as a tab in the dashboard.
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine()
suppressPackageStartupMessages(library(data.table))

parse_args <- function(args = commandArgs(TRUE)) {
  out <- list(); i <- 1L
  while (i <= length(args)) { if (startsWith(args[i], "--")) { out[[sub("^--", "", args[i])]] <- args[i + 1L]; i <- i + 2L } else i <- i + 1L }
  out
}

if (!interactive()) {
  a <- parse_args()
  id <- a$contest %||% a$url
  if (is.null(id)) { cat("Usage: Rscript jobs/contest.R --url <contest URL> | --contest <id> [--sport S] [--nlineups N]\n"); quit(status = 1) }
  r <- run_contest(id, sport = a$sport, n_lineups = as.integer(a$nlineups %||% 6))
  cat("\n========== ", r$contest$name, " ==========\n", sep = "")
  cat(sprintf("%s | $%s entry | %s field | up to %s entries/user\n",
      if (r$showdown) "SHOWDOWN" else "Classic", r$contest$entry_fee, r$contest$field_size, r$contest$max_entries_per_user))
  if (!is.null(r$captain_board)) { cat("\nCAPTAIN BOARD (1.5x CPT slot — the key Showdown decision):\n"); print(r$captain_board) }
  P <- as.data.table(r$pool)
  cat("\nLINEUPS:\n")
  for (i in seq_along(r$picks)) { p <- r$picks[[i]]; ix <- p$idx[[1]]
    cpt <- if (r$showdown) ix[P$slot[ix] == "CPT"] else integer(0)
    flex <- setdiff(ix, cpt)
    cat(sprintf("  #%d [%s] %s%s\n", i, p$role,
        if (length(cpt)) paste0("CPT ", P$player_name[cpt], " | ") else "",
        paste(P$player_name[flex], collapse = ", "))) }
  # also (re)build the dashboard with this contest as a tab
  build_dashboard(contests = r$contest$contest_id, bankroll = a$bankroll,
                  n_lineups = as.integer(a$nlineups %||% 6))
}
