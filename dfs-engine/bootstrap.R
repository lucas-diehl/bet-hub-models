# ==============================================================================
# DFS ENGINE — bootstrap
# Source this at the top of every entry script:  source("bootstrap.R")
# It resolves the project root WITHOUT depending on the launch directory (the bug
# in the golf scripts, which hardcoded setwd / relative source paths), wires the
# spine onto the search path, and exposes shared helpers + config.
# ==============================================================================

# ── resolve project root ──────────────────────────────────────────────────────
# Priority: DFS_ENGINE_ROOT env var > walk up from getwd() for the sentinel file >
# the location of this file (when sourced with a known path).
.dfs_find_root <- function() {
  env <- Sys.getenv("DFS_ENGINE_ROOT", "")
  if (nchar(env) && file.exists(file.path(env, ".dfs_engine_root"))) return(normalizePath(env))
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in 1:8) {
    if (file.exists(file.path(d, ".dfs_engine_root"))) return(d)
    parent <- dirname(d); if (parent == d) break; d <- parent
  }
  # last resort: this machine's known location
  fallback <- "c:/Users/ljdie/OneDrive/Documents/DFS ENGINE"
  if (file.exists(file.path(fallback, ".dfs_engine_root"))) return(normalizePath(fallback))
  stop("Could not locate DFS ENGINE project root. Set DFS_ENGINE_ROOT or run from inside the project.")
}

DFS_ROOT <- .dfs_find_root()

# absolute path helper, e.g. dfs_path("data", "dfs.duckdb")
dfs_path <- function(...) file.path(DFS_ROOT, ...)

# ── shared logging ────────────────────────────────────────────────────────────
msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S]"), ..., "\n", sep = " ")

# null/empty-coalescing helper used across scripts
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# ── secrets / env ─────────────────────────────────────────────────────────────
# Loads KEY=VALUE lines from a gitignored .env at the project root (if present),
# without overwriting vars already set in the environment.
.dfs_load_env <- function() {
  f <- dfs_path(".env")
  if (!file.exists(f)) return(invisible())
  for (ln in readLines(f, warn = FALSE)) {
    ln <- trimws(ln); if (!nchar(ln) || startsWith(ln, "#") || !grepl("=", ln)) next
    k <- trimws(sub("=.*$", "", ln)); v <- sub("^[^=]*=", "", ln)
    if (!nchar(Sys.getenv(k))) do.call(Sys.setenv, setNames(list(v), k))
  }
  invisible()
}
.dfs_load_env()

dfs_require_key <- function(name) {
  v <- Sys.getenv(name, "")
  if (!nchar(v)) stop(sprintf("Env var %s is not set (put it in %s or your environment).",
                              name, dfs_path(".env")))
  v
}

# ── source the spine ──────────────────────────────────────────────────────────
# Order matters: db + plugin first, then the core modules that depend on them.
.dfs_spine_order <- c(
  "db.R", "plugin.R", "slate_io.R", "dk_scrape.R", "fd_scrape.R", "payouts.R", "dk_contest.R", "contest_select.R",
  "vegas.R", "injuries.R", "slates.R", "correlation.R", "slate_sim.R", "field_sim.R", "optimizer.R",
  "lineup_eval.R", "portfolio.R", "pipeline.R", "showdown.R", "ownership.R", "dk_auth.R",
  "results.R", "proj_accuracy.R", "diagnostics.R", "ownership_model.R", "io_export.R", "staking.R", "report.R", "dashboard.R", "validate.R"
)
dfs_load_spine <- function() {
  for (f in .dfs_spine_order) {
    p <- dfs_path("spine", "R", f)
    if (file.exists(p)) sys.source(p, envir = globalenv())
  }
  invisible()
}

# load a sport plugin's files (ingest/project/correlate/roster, or adapter.R)
dfs_load_sport <- function(sport) {
  dir <- dfs_path("sports", sport)
  if (!dir.exists(dir)) stop("Unknown sport: ", sport)
  for (f in list.files(dir, pattern = "\\.R$", full.names = TRUE)) sys.source(f, envir = globalenv())
  invisible()
}

invisible(msg("DFS ENGINE root:", DFS_ROOT))
