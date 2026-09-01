# R/model_top20.R
library(tidymodels)
library(data.table)

fit_top20_glmnet <- function(dt_train) {
  set.seed(20250220)
  
  candidate_preds <- c(
    "sg12","sg24","sg50","sg100","sg_sd24",
    "z_sg24","z_sg100",
    "pct_sg24","pct_sg100",
    "delta_sg24","delta_sg100",
    "event_base_rate",
    "field_size","is_alt","major_flag"
  )
  
  preds <- intersect(candidate_preds, names(dt_train))
  
  df <- as.data.table(dt_train)[, c("top20", preds), with = FALSE]
  df[, top20 := factor(as.integer(top20), levels = c(0,1))]
  
  # If somehow we truly have no predictors, fail loudly
  if (length(preds) == 0) {
    stop("No predictors available for model fit.")
  }
  
  rec <- recipe(top20 ~ ., data = df) |>
    step_impute_median(all_numeric_predictors()) |>
    step_zv(all_predictors()) |>
    step_normalize(all_numeric_predictors())
  
  spec <- logistic_reg(penalty = 0.002, mixture = 1) |>
    set_engine("glmnet")
  
  workflow() |>
    add_recipe(rec) |>
    add_model(spec) |>
    fit(data = df)
}

# Platt scaling (calibration)
platt_fit <- function(p_raw, y) {
  y <- as.integer(y)
  p_raw <- pmin(pmax(as.numeric(p_raw), 1e-6), 1 - 1e-6)
  df <- data.frame(y = y, lp = qlogis(p_raw))
  glm(y ~ lp, data = df, family = binomial())
}

platt_apply <- function(platt_model, p_raw) {
  p_raw <- pmin(pmax(as.numeric(p_raw), 1e-6), 1 - 1e-6)
  df <- data.frame(lp = qlogis(p_raw))
  as.numeric(predict(platt_model, newdata = df, type = "response"))
}