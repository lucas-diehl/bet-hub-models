library(data.table)

logloss <- function(y, p) {
  p <- pmin(pmax(p, 1e-6), 1 - 1e-6)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

brier <- function(y, p) mean((p - y)^2)

top20_prob_metrics <- function(pred_dt) {
  dt <- as.data.table(pred_dt)
  y <- if (is.factor(dt$top20)) as.integer(as.character(dt$top20)) else as.integer(dt$top20)
  p <- as.numeric(dt$p_top20)
  
  data.table(
    n = nrow(dt),
    base_rate = mean(y),
    logloss = logloss(y, p),
    brier = brier(y, p)
  )
}