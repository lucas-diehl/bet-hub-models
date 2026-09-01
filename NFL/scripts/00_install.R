repos <- c(CRAN = "https://cloud.r-project.org")
dir.create(".Rlib", showWarnings = FALSE)
.libPaths(c(normalizePath(".Rlib"), .libPaths()))
packages <- c(
  "nflfastR", "nflreadr", "dplyr", "tidyr", "purrr", "slider", "readr",
  "stringr", "lubridate", "yaml", "jsonlite", "httr2", "ranger", "xgboost", "nnet",
  "ggplot2"
)
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, repos = repos, lib = ".Rlib", dependencies = TRUE)
}
message("All required packages are installed.")
