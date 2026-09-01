steps <- c(
  "scripts/01_download.R",
  "scripts/02_build_features.R",
  "scripts/03_backtest.R",
  "scripts/05_analyze.R",
  "scripts/06_percentage_edges_interactions.R",
  "scripts/07_build_2025_history.R",
  "scripts/04_report.R"
)

for (step in steps) {
  message("\n=== Running ", step, " ===")
  source(step, local = new.env(parent = globalenv()))
}
message("\nPipeline complete.")
