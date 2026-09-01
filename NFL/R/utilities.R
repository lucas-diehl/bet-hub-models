if (dir.exists(".Rlib")) {
  .libPaths(c(normalizePath(".Rlib"), .libPaths()))
}

required_packages <- c(
  "nflfastR", "nflreadr", "dplyr", "tidyr", "purrr", "slider", "readr",
  "stringr", "lubridate", "yaml", "jsonlite", "httr2", "ranger", "xgboost", "nnet",
  "ggplot2"
)

assert_packages <- function(packages = required_packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Missing packages: ", paste(missing, collapse = ", "),
      ". Run scripts/00_install.R first.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

read_config <- function(path = "config.yml") {
  yaml::read_yaml(path)
}

ensure_directories <- function() {
  dirs <- c("data/raw", "data/processed", "outputs")
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

normalize_team <- function(x) {
  x <- toupper(trimws(x))
  aliases <- c(
    ARZ = "ARI", CRD = "ARI", SLC = "ARI",
    BLT = "BAL", CLV = "CLE", HST = "HOU",
    JAC = "JAX",
    SD = "LAC", SDG = "LAC",
    LA = "LAR", RAM = "LAR", STL = "LAR",
    OAK = "LV", LVR = "LV", LAD = "LV",
    WAS = "WAS", WSH = "WAS"
  )
  hit <- x %in% names(aliases)
  x[hit] <- unname(aliases[x[hit]])
  x
}

safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

# xgboost keeps the trained booster behind an external pointer. That pointer
# does not survive saveRDS/readRDS into a later session, and a booster written
# by xgboost 2.x unserializes under 1.7 as an empty list with its class
# attribute dropped, silently. Persist the portable raw payload next to every
# fit and rebuild the handle from it on load.
portable_booster <- function(fit) {
  attr(fit, "raw_model") <- xgboost::xgb.save.raw(fit, raw_format = "json")
  fit
}

revive_booster <- function(fit, label = "model") {
  raw_model <- attr(fit, "raw_model")
  if (!is.null(raw_model)) {
    return(xgboost::xgb.load.raw(raw_model, as_booster = TRUE))
  }
  if (inherits(fit, "xgb.Booster")) return(fit)
  stop(
    "The stored '", label, "' booster did not survive serialization and has ",
    "no portable payload. Retrain it with the installed xgboost version.",
    call. = FALSE
  )
}

# Cheap predictability probe for validation scripts: a structurally intact
# artifact whose booster cannot score is worse than a missing one, because the
# failure only surfaces at deployment time. Feature names are not carried in
# the raw payload, so the caller supplies the model's own feature vector.
booster_is_live <- function(fit, features) {
  tryCatch({
    revived <- revive_booster(fit)
    probe <- matrix(0, nrow = 1L, ncol = length(features),
                    dimnames = list(NULL, features))
    is.finite(as.numeric(stats::predict(revived, probe))[[1]])
  }, error = function(e) FALSE, warning = function(w) FALSE)
}

rmse_vec <- function(truth, estimate) {
  sqrt(mean((truth - estimate)^2, na.rm = TRUE))
}

mae_vec <- function(truth, estimate) {
  mean(abs(truth - estimate), na.rm = TRUE)
}

r_squared_vec <- function(truth, estimate) {
  ok <- stats::complete.cases(truth, estimate)
  if (sum(ok) < 3 || stats::var(truth[ok]) == 0) return(NA_real_)
  stats::cor(truth[ok], estimate[ok])^2
}

american_profit <- function(result, odds = -110, stake = 1) {
  win_profit <- if (odds < 0) 100 / abs(odds) else odds / 100
  dplyr::case_when(
    result > 0 ~ stake * win_profit,
    result < 0 ~ -stake,
    TRUE ~ 0
  )
}

signed_result <- function(x, tolerance = 1e-9) {
  ifelse(abs(x) <= tolerance, 0, sign(x))
}
