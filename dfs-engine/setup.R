#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — one-time setup
#   Rscript setup.R
# Installs missing packages (binary, no compile), bootstraps the DuckDB schema,
# and reports environment readiness. Safe to re-run.
# ==============================================================================
options(install.packages.compile.from.source = "never")

needed <- c(
  core   = "DBI", store = "duckdb", dt = "data.table", xgb = "xgboost",
  ilp    = "lpSolve", xlsx = "openxlsx", http = "httr2", json = "jsonlite",
  yaml   = "yaml", readr = "readr",
  wnba   = "wehoop", nfl = "nflreadr", ncaaf = "cfbfastR", nba = "hoopR")

ip <- rownames(installed.packages())
miss <- setdiff(unname(needed), ip)
if (length(miss)) {
  cat("Installing:", paste(miss, collapse = ", "), "\n")
  install.packages(miss, repos = "https://cloud.r-project.org", type = "binary", quiet = TRUE)
}

ip <- rownames(installed.packages())
cat("\nPackage status:\n")
for (p in unname(needed))
  cat(sprintf("  %-12s %s\n", p, if (p %in% ip) as.character(packageVersion(p)) else "MISSING"))

if ("xgboost" %in% ip && packageVersion("xgboost") < "2.0")
  cat("\nNote: xgboost < 2.0 -> quantile ceiling uses the mean+0.84*sd fallback",
      "(same as golf today). Upgrade to >=2.0 for the true quantile model.\n")

# bootstrap DB
root <- tryCatch(normalizePath(getwd()), error = function(e) getwd())
bp <- file.path(root, "bootstrap.R")
if (file.exists(bp)) {
  source(bp); dfs_load_spine()
  cat("\nBootstrapping DuckDB schema...\n")
  print(db_bootstrap())
  cat("\nRegistered sports:\n")
  for (s in c("golf","wnba","tennis")) { tryCatch({ dfs_load_sport(s) }, error = function(e) NULL) }
  print(list_sports())
} else cat("\n(Run from the project root to bootstrap the DB.)\n")

cat("\nSetup complete.\n")
cat("Secrets: put DATAGOLF_API_KEY and ODDS_API_KEY in a .env file at the project root.\n")
