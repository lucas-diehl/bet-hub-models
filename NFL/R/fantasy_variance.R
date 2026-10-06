## ---------------------------------------------------------------------------
## R/fantasy_variance.R
##
## Player-specific projection spread, replacing the position-constant bands.
##
## What was wrong. ppr_low / ppr_high were built as `projected_ppr + p10/p90 of
## that POSITION's residual` - one constant width for every player at a
## position, added regardless of projection size. Measured on 2023-2025
## walk-forward residuals, that conditions on the wrong variable entirely:
##
##   residual sd by projection octile:  2.88 -> 8.39  (proj 1.9 -> 16.8)
##   residual sd by position, holding projection at 8-14:
##       QB 7.70 | RB 7.23 | TE 6.96 | WR 7.38
##
## Spread is driven overwhelmingly by HOW BIG the projection is, and barely at
## all by position once level is controlled. The shipped bands used the weak
## signal and ignored the strong one, so they ran far too wide at the bottom of
## the board and too narrow at the top.
##
## For DFS this is not cosmetic. The optimizer samples proj + sim_sd * z, so a
## boom/bust deep threat and a possession receiver at the same mean were being
## simulated as the same player - which is precisely the distinction GPP lineup
## construction is supposed to exploit.
##
## Method - two stages, so both heteroscedasticity and skew are handled:
##
##   1. SCALE. Gamma GLM with a log link predicting |residual| from the
##      projection, the player's own trailing PPR volatility (ppr_sd5), recent
##      opportunity, and position. Log link keeps the scale positive and makes
##      the fit multiplicative, which matches the near-proportional relationship
##      between level and spread.
##   2. SHAPE. Standardise residuals by that scale (z = resid / scale) and take
##      EMPIRICAL quantiles of z per position. Fantasy scoring is right-skewed -
##      touchdowns are lumpy - so assuming normality and multiplying by 1.2533
##      would understate the upside tail, which is the tail tournaments are won
##      in. Empirical z-quantiles inherit the real shape.
##
## The interval is then proj + q * scale, per player. sim_sd is derived from the
## same p10-p90 span so the number handed to the simulator is consistent with
## the band shown on the board.
## ---------------------------------------------------------------------------

## Features the scale model uses. Kept deliberately small: these are the ones
## that actually correlated with |residual| (0.37 / 0.27 / 0.25), and a wide
## feature set here would overfit the spread rather than describe it.
fantasy_variance_features <- function() {
  c("ppr_model", "fantasy_points_ppr_sd5", "opportunity_r5")
}

## Projection-level bucket, using boundaries frozen at fit time.
.fv_level_bucket <- function(x, cuts) {
  ifelse(x <= cuts[[1]], "lo", ifelse(x <= cuts[[2]], "mid", "hi"))
}

fit_fantasy_variance <- function(d, min_rows = 500L) {
  need <- c("ppr_actual", "ppr_model", "position")
  if (!all(need %in% names(d))) {
    stop("fit_fantasy_variance() needs: ", paste(need, collapse = ", "),
         call. = FALSE)
  }
  feats <- intersect(fantasy_variance_features(), names(d))

  d <- d[is.finite(d$ppr_actual) & is.finite(d$ppr_model), , drop = FALSE]
  for (f in feats) d[[f]][!is.finite(d[[f]])] <- stats::median(d[[f]], na.rm = TRUE)
  d$abs_res <- abs(d$ppr_actual - d$ppr_model)
  ## Gamma needs strictly positive; an exact zero residual is measure-zero noise.
  d$abs_res <- pmax(d$abs_res, 0.05)
  if (nrow(d) < min_rows) return(NULL)

  form <- stats::as.formula(
    paste("abs_res ~", paste(c(feats, "position"), collapse = " + "))
  )
  scale_fit <- try(
    stats::glm(form, family = stats::Gamma(link = "log"), data = d),
    silent = TRUE
  )
  if (inherits(scale_fit, "try-error")) return(NULL)

  d$scale <- stats::predict(scale_fit, newdata = d, type = "response")
  d$scale <- pmax(d$scale, 0.5)
  d$z <- (d$ppr_actual - d$ppr_model) / d$scale

  ## Shape varies with LEVEL, not just position. Pooling z-quantiles across all
  ## projection levels left the middle of the board under-covered (0.72 against
  ## a 0.80 target) while the extremes were fine: a 2-point projection is far
  ## more right-skewed in relative terms than a 16-point one, because a single
  ## touchdown is a much larger multiple of the former. Quantiles are therefore
  ## taken within (position x projection tercile).
  ##
  ## Tercile cuts are stored so scoring uses the same boundaries the fit did -
  ## recomputing them on a single week's board would put players in different
  ## buckets than training and silently shift the intervals.
  cuts <- stats::quantile(d$ppr_model, c(1 / 3, 2 / 3), na.rm = TRUE)
  d$lvl <- .fv_level_bucket(d$ppr_model, cuts)

  z_tab <- do.call(rbind, lapply(split(d, list(d$position, d$lvl), drop = TRUE), function(g) {
    if (nrow(g) < 40) return(NULL)
    q <- stats::quantile(g$z, c(0.10, 0.90), na.rm = TRUE)
    data.frame(position = g$position[[1]], lvl = g$lvl[[1]],
               z10 = unname(q[[1]]), z90 = unname(q[[2]]),
               n = nrow(g), stringsAsFactors = FALSE)
  }))
  rownames(z_tab) <- NULL

  ## Position-level fallback for any (position, tercile) cell too thin to fit.
  z_pos <- do.call(rbind, lapply(split(d, d$position), function(g) {
    q <- stats::quantile(g$z, c(0.10, 0.90), na.rm = TRUE)
    data.frame(position = g$position[[1]], z10 = unname(q[[1]]),
               z90 = unname(q[[2]]), stringsAsFactors = FALSE)
  }))
  rownames(z_pos) <- NULL

  list(
    scale_fit = scale_fit,
    z_quantiles = z_tab,
    z_position = z_pos,
    level_cuts = cuts,
    features = feats,
    medians = vapply(feats, function(f) stats::median(d[[f]], na.rm = TRUE), numeric(1)),
    n = nrow(d),
    fitted_at = Sys.time()
  )
}

## Apply to a board. Adds ppr_low / ppr_high / sim_sd, all player-specific.
## Returns the board unchanged (with a message) if no model is available, so a
## missing artifact degrades to the caller's existing bands rather than silently
## producing nonsense.
apply_fantasy_variance <- function(board, model, proj_col = "projected_ppr") {
  if (is.null(model)) {
    message("Variance model: none available; leaving existing bands in place.")
    return(board)
  }
  d <- board
  d$ppr_model <- d[[proj_col]]
  for (f in model$features) {
    if (!f %in% names(d)) d[[f]] <- unname(model$medians[[f]])
    d[[f]][!is.finite(d[[f]])] <- unname(model$medians[[f]])
  }
  ## Unseen position -> fall back to the most common one in the fit.
  known <- model$z_quantiles$position
  d$position_fit <- ifelse(d$position %in% known, d$position, known[[1]])
  fitdat <- d
  fitdat$position <- fitdat$position_fit

  scale <- stats::predict(model$scale_fit, newdata = fitdat, type = "response")
  scale <- pmax(as.numeric(scale), 0.5)

  ## (position x level) quantiles, falling back to position-only for any cell
  ## that was too thin at fit time.
  lvl <- .fv_level_bucket(d[[proj_col]], model$level_cuts)
  key <- paste(d$position_fit, lvl, sep = "|")
  zq <- model$z_quantiles
  zkey <- paste(zq$position, zq$lvl, sep = "|")
  idx <- match(key, zkey)
  z10 <- zq$z10[idx]
  z90 <- zq$z90[idx]

  miss <- is.na(idx)
  if (any(miss)) {
    pidx <- match(d$position_fit[miss], model$z_position$position)
    z10[miss] <- model$z_position$z10[pidx]
    z90[miss] <- model$z_position$z90[pidx]
  }

  board$ppr_low <- pmax(0, d[[proj_col]] + z10 * scale)
  board$ppr_high <- pmax(board$ppr_low, d[[proj_col]] + z90 * scale)
  ## p10..p90 spans 2.563 sd for a normal; used only to hand the simulator a
  ## single sd consistent with the band actually shown.
  board$sim_sd <- pmax((board$ppr_high - board$ppr_low) / 2.563, 1)
  board$proj_scale <- scale
  board
}

## Coverage + sharpness. The only honest way to compare two interval methods:
## at equal coverage, narrower wins; wider coverage at the same width wins.
fantasy_interval_report <- function(actual, lo, hi, group = NULL) {
  ok <- is.finite(actual) & is.finite(lo) & is.finite(hi)
  actual <- actual[ok]; lo <- lo[ok]; hi <- hi[ok]
  g <- if (is.null(group)) rep("all", length(actual)) else group[ok]
  inside <- actual >= lo & actual <= hi
  width <- hi - lo
  stats::aggregate(
    cbind(coverage = inside, width = width) ~ g,
    FUN = mean
  ) |> setNames(c("group", "coverage", "mean_width"))
}
