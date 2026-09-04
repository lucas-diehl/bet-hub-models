#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — schema contract tests for validate_projection() (spine/R/plugin.R).
# Zero extra dependencies (base R + data.table). Run:
#   Rscript tests/test_validate_projection.R
# Exits non-zero on any failure so CI fails loudly. This guards the exact class of
# bug that let baseline projections ship to production undetected: every plugin's
# project_players() output flows through validate_projection(), so its contract is
# the schema boundary for the whole engine.
# ==============================================================================
local({
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  root <- if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
  bp <- file.path(root, "bootstrap.R"); if (!file.exists(bp)) bp <- file.path(getwd(), "bootstrap.R")
  source(bp)
})
dfs_load_spine()
suppressPackageStartupMessages(library(data.table))

# ── tiny harness ──────────────────────────────────────────────────────────────
.pass <- 0L; .fail <- 0L
ok <- function(desc, cond) {
  if (isTRUE(cond)) { .pass <<- .pass + 1L; cat("  ok   - ", desc, "\n", sep = "") }
  else              { .fail <<- .fail + 1L; cat("  FAIL - ", desc, "\n", sep = "") }
}
throws <- function(expr) inherits(tryCatch(expr, error = function(e) e), "error")
# a healthy pool with every column validate_projection() touches
good <- function() data.table(
  player_id = 1:4, salary = c(5000L, 6000L, 7000L, 8000L),
  proj = c(10, 20, 30, 40), sim_sd = c(4, 5, 6, 7),
  ceil = c(18, 30, 42, 55), floor = c(3, 8, 15, 22))

cat("validate_projection() schema contract\n")

# 1. accepts a valid pool, preserves rows + required columns
{
  d <- validate_projection(good())
  ok("accepts a valid pool", is.data.frame(d) &&
       all(c("player_id", "salary", "proj", "sim_sd") %in% names(d)))
  ok("row count preserved", nrow(d) == 4L)
}

# 2. each REQUIRED column missing -> hard error (the gap-#2 guard)
for (col in c("player_id", "salary", "proj", "sim_sd")) {
  d <- good(); d[[col]] <- NULL
  ok(sprintf("errors when required column '%s' is missing", col), throws(validate_projection(d)))
}

# 3. non-finite proj/ceil/floor -> repaired to finite
for (col in c("proj", "ceil", "floor")) {
  d <- good(); d[[col]][2] <- NA_real_; d[[col]][3] <- Inf
  r <- validate_projection(d)
  ok(sprintf("repairs non-finite '%s' to finite", col), all(is.finite(r[[col]])))
}

# 4. sim_sd <= 0 -> replaced with a positive value (sims require sd > 0)
{
  d <- good(); d$sim_sd[c(1, 3)] <- c(0, -2)
  r <- validate_projection(d)
  ok("sim_sd <= 0 replaced with positive", all(r$sim_sd > 0))
}

# 5. non-finite sim_sd -> finite AND positive
{
  d <- good(); d$sim_sd[2] <- NA_real_
  r <- validate_projection(d)
  ok("non-finite sim_sd repaired to finite+positive", all(is.finite(r$sim_sd) & r$sim_sd > 0))
}

# 6. output invariants on a healthy pool
{
  r <- validate_projection(good())
  ok("output invariant: all proj finite", all(is.finite(r$proj)))
  ok("output invariant: all sim_sd > 0", all(r$sim_sd > 0))
}

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
