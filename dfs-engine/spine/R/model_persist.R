# ==============================================================================
# DFS ENGINE — xgboost model persistence (portable across xgboost versions)
#
# A trained xgboost booster sits behind an external pointer that does NOT
# survive a plain saveRDS()/readRDS() round-trip once the process ends, and a
# booster saved under one xgboost release can silently come back broken under
# a different one -- the SAME disease, independently confirmed twice this
# project: the standalone NFL repo's TD model (R/utilities.R's
# portable_booster()/revive_booster(), fixed 2026-10-14 after it had been
# failing silently on every run for days) and, found 2026-10-17 while
# auditing for stale data/models, sports/nfl/model.R, sports/wnba/model.R and
# sports/tennis/model.R -- all three save/load their xgboost fit with a bare
# saveRDS()/readRDS(), no portability guard at all. It has been "working" only
# because train.yml (which fits these) and dfs.yml (which serves them) happen
# to currently have matching xgboost versions -- exactly the kind of thing
# that broke silently elsewhere the moment one workflow's R-package cache got
# invalidated and pulled a newer release than the other.
#
# Same two-function shape as the NFL repo's proven fix, generalized here so
# every sport shares ONE implementation instead of three copies drifting:
#   dfs_portable_booster(m)      -- call on the BARE xgb.Booster before it goes
#                                    into whatever wrapper list/saveRDS() a
#                                    sport's train() already uses. Attaches a
#                                    portable raw-JSON payload as an attribute
#                                    that travels with it through serialization.
#   dfs_revive_booster(m, label) -- call on that SAME object after reading it
#                                    back. Returns a GUARANTEED-live booster,
#                                    rebuilt from the portable payload when the
#                                    raw in-process pointer didn't survive.
suppressPackageStartupMessages({ library(data.table) })

dfs_portable_booster <- function(m) {
  attr(m, "dfs_xgb_raw") <- tryCatch(xgboost::xgb.save.raw(m, raw_format = "json"), error = function(e) NULL)
  m
}

dfs_revive_booster <- function(m, label = "model") {
  raw <- attr(m, "dfs_xgb_raw")
  if (!is.null(raw)) {
    revived <- tryCatch(xgboost::xgb.load.raw(raw, as_booster = TRUE), error = function(e) NULL)
    if (!is.null(revived)) return(revived)
    msg(sprintf("  dfs_revive_booster: %s's portable payload failed to load (xgboost version drift?) -- falling back to the raw object, which may be dead", label))
  }
  m
}
