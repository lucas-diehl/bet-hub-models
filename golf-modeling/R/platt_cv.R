# R/platt_cv.R
library(data.table)
library(rsample)
library(tidymodels)

# Robust CV-based Platt calibration.
# If a fold's model fit fails (e.g., glmnet matrix col issue after recipe),
# fall back to a simple GLM baseline for that fold so calibration can proceed.
platt_fit_cv <- function(dt_train, fit_fun, v = 5, seed = 20250220) {
  set.seed(seed)
  
  dt <- as.data.table(dt_train)
  if (!("top20" %in% names(dt))) stop("dt_train missing top20")
  
  folds <- vfold_cv(dt, v = v, strata = top20)
  
  oof <- rbindlist(lapply(seq_len(nrow(folds)), function(i) {
    split <- folds$splits[[i]]
    tr <- as.data.table(analysis(split))
    va <- as.data.table(assessment(split))
    
    # Helper: baseline GLM workflow that will not fail due to matrix columns
    fit_baseline_glm <- function(tr_dt) {
      tr_dt <- as.data.table(tr_dt)
      tr_dt[, top20 := factor(as.integer(top20), levels = c(0, 1))]
      
      # Prefer a simple always-available predictor if present and not all-NA
      base_pred <- intersect(c("field_size", "major_flag", "is_alt"), names(tr_dt))
      base_pred <- base_pred[base_pred %in% names(tr_dt) && !all(is.na(tr_dt[[base_pred[1]]]))]
      
      if (length(base_pred) >= 1) {
        fml <- as.formula(paste("top20 ~", base_pred[1]))
        tr_fit <- tr_dt
      } else {
        # workflows formula interface forbids explicit "~ 1"
        # so create a dummy constant predictor instead
        tr_fit <- copy(tr_dt)
        tr_fit[, dummy := 1]
        fml <- top20 ~ dummy
      }
      
      workflow() |>
        add_model(logistic_reg() |> set_engine("glm")) |>
        add_formula(fml) |>
        fit(data = tr_fit)
    }
    
    # Try primary model; if it fails, fall back
    wf_i <- tryCatch(
      fit_fun(tr),
      error = function(e) {
        message(sprintf("platt_fit_cv(): fold %d model failed; using baseline GLM. Error: %s", i, e$message))
        fit_baseline_glm(tr)
      }
    )
    
    # Predict probabilities on validation fold
    p_raw <- tryCatch(
      {
        va_pred <- data.table::copy(va)
        if (!("dummy" %in% names(va_pred))) va_pred[, dummy := 1]
        
        pred_tbl <- predict(wf_i, new_data = va_pred, type = "prob")
        
        # robust: use .pred_1 if present else take last column
        if (".pred_1" %in% names(pred_tbl)) {
          pred_tbl[[".pred_1"]]
        } else {
          pred_tbl[[ncol(pred_tbl)]]
        }
      },
      error = function(e) {
        message(sprintf("platt_fit_cv(): fold %d predict failed; using 0.5 probs. Error: %s", i, e$message))
        rep(0.5, nrow(va))
      }
    )
    
    data.table(
      y = as.integer(as.character(factor(va$top20, levels = c(0, 1)))),
      p_raw = as.numeric(p_raw)
    )
  }), use.names = TRUE, fill = TRUE)
  
  # Fit Platt on out-of-fold predictions
  platt_fit(oof$p_raw, oof$y)
}