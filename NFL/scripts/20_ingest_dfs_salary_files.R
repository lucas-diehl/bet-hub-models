source("R/utilities.R")
source("R/dfs_salaries.R")
assert_packages()
ensure_directories()

config <- yaml::read_yaml("config/dfs_salaries.yml")
historical <- if (file.exists("data/processed/dfs_salaries.rds")) {
  readRDS("data/processed/dfs_salaries.rds")
} else {
  empty_dfs_salaries()
}
manual <- ingest_manual_dfs_salary_directory(
  config$current$manual_drop_directory
)
combined <- deduplicate_dfs_salaries(dplyr::bind_rows(historical, manual))

saveRDS(combined, "data/processed/dfs_salaries.rds")
readr::write_csv(combined, "outputs/dfs_salaries.csv")

cat(
  sprintf(
    "Ingested %s manual rows; consolidated file now has %s rows.\n",
    format(nrow(manual), big.mark = ","),
    format(nrow(combined), big.mark = ",")
  )
)

