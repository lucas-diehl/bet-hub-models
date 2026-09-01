steps <- c(
  "scripts/11_download_td_player_data.R",
  "scripts/12_build_td_features.R",
  "scripts/13_train_td_model.R",
  "scripts/14_backtest_td_model.R"
)

for (step in steps) {
  message("\nRunning ", step)
  source(step, local = new.env(parent = globalenv()))
}
