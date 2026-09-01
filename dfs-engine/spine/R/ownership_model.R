# ==============================================================================
# DFS ENGINE — TRAINED OWNERSHIP MODEL (Phase 1 of the elite roadmap)
# Ownership is the #1 edge SaberSim/Stokastic have: it drives leverage, duplication, the
# field sim and therefore GPP EV. We replace the value-rank HEURISTIC (own = 0.02 + k·r²)
# with a model trained on the logged ACTUAL %Drafted, per sport. Validated leave-one-slate-
# out (WNBA, 6 slates): corr 0.40 -> 0.70, RMSE -26% vs the heuristic. Falls back to the
# heuristic (predict returns NULL) until a sport has enough logged slates, so it never
# regresses a data-thin sport. Retrain as ownership accrues: train_ownership_model(sport).
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

OWN_MODEL_PATH <- function(sport) dfs_path("data", "models", sprintf("own_model_%s.rds", sport))
OWN_FEATS <- c("vr", "sr", "pr", "value", "salary")

# assemble training data: logged actual %Drafted + salary + latest projection per slate/player
ownership_training_data <- function(sport = NULL) {
  where <- if (!is.null(sport)) sprintf("AND a.sport = '%s'", sport) else ""
  D <- tryCatch(as.data.table(db_query(sprintf("
    WITH act AS (SELECT sport, slate_id, player_id, MAX(actual_pct) actual FROM ownership
                 WHERE contest_id <> '_projected' AND actual_pct IS NOT NULL GROUP BY 1,2,3),
         sal AS (SELECT slate_id, player_id, MAX(salary) salary FROM salaries GROUP BY 1,2),
         prj AS (SELECT slate_id, player_id, proj_mean AS proj FROM projections
                 QUALIFY ROW_NUMBER() OVER (PARTITION BY slate_id, player_id ORDER BY version_ts DESC)=1)
    SELECT a.sport, a.slate_id, a.player_id, s.salary, p.proj, a.actual
    FROM act a JOIN sal s ON a.slate_id=s.slate_id AND a.player_id=s.player_id
               JOIN prj p ON a.slate_id=p.slate_id AND a.player_id=p.player_id
    WHERE s.salary > 0 AND p.proj > 0 AND a.actual >= 0 %s", where))), error = function(e) NULL)
  D
}

# per-slate features (identical in train + predict): value + within-slate ranks
.own_features <- function(D) {
  D <- as.data.table(copy(D))
  if (!"slate_id" %in% names(D)) D[, slate_id := "_one"]
  D[, value := proj / pmax(salary / 1000, 0.1)]
  D[, `:=`(vr = frank(value) / .N, sr = frank(salary) / .N, pr = frank(proj) / .N), by = slate_id]
  D
}

# Train + cache a per-sport ownership model. Returns NULL (no model) if data is too thin,
# so the caller keeps the heuristic for that sport.
train_ownership_model <- function(sport, min_slates = 5L, min_rows = 60L) {
  D <- ownership_training_data(sport)
  if (is.null(D) || !nrow(D)) { msg("  ownership: no data for", sport); return(NULL) }
  if (uniqueN(D$slate_id) < min_slates || nrow(D) < min_rows) {
    msg(sprintf("  ownership: %s too thin (%d slates, %d rows) -> keep heuristic", sport, uniqueN(D$slate_id), nrow(D)))
    return(NULL)
  }
  D <- .own_features(D)
  fit <- lm(stats::as.formula(paste("actual ~", paste(OWN_FEATS, collapse = " + "))), D)
  m <- list(model = fit, sport = sport, n = nrow(D), slates = uniqueN(D$slate_id),
            coef = coef(fit), trained_on = format(Sys.time(), "%Y-%m-%d"))
  dir.create(dirname(OWN_MODEL_PATH(sport)), recursive = TRUE, showWarnings = FALSE)
  saveRDS(m, OWN_MODEL_PATH(sport))
  msg(sprintf("  ownership model saved: %s (%d slates, %d rows)", sport, m$slates, m$n))
  invisible(m)
}

# Predict ownership FRACTIONS for a pool (player_name/salary/proj), matching the actual
# %Drafted scale the field sim wants. Returns NULL if no trained model -> caller uses the
# heuristic. Capped; non-negative.
predict_ownership <- function(pool, sport) {
  p <- OWN_MODEL_PATH(sport); if (!file.exists(p)) return(NULL)
  m <- tryCatch(readRDS(p), error = function(e) NULL); if (is.null(m)) return(NULL)
  P <- .own_features(pool)
  pct <- tryCatch(pmax(as.numeric(predict(m$model, P)), 0), error = function(e) NULL)  # predicted %Drafted
  # guard: fall back to the heuristic (NULL) on degenerate output (all-zero / any NA)
  if (is.null(pct) || length(pct) != nrow(as.data.table(pool)) || anyNA(pct) || sum(pct) <= 0) return(NULL)
  pmin(pct / 100, 0.7)                                        # -> fraction, capped at 70%
}
