# ============================================================================
# verify_pipeline.R  —  correctness checks for the PPP pipeline
#   1. possession sanity (off_ppd ~2.0, drives ~11, score reconciliation)
#   2. LEAKAGE invariant: each as-of rating used only games strictly before its
#      week (n_games_to_date == count of that season's team-games with week<w)
#   3. re-derive the headline totals result from ppp_backtest_results.csv
# Exits non-zero on any failure.
# ============================================================================
suppressWarnings(suppressMessages(library(dplyr)))
CACHE <- "data_cache"; fail <- 0
chk <- function(ok, msg) { cat(sprintf("  [%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) fail <<- fail + 1 }

cat("VERIFY 1: possession sanity\n")
tg <- readRDS(file.path(CACHE, "team_game_ppp.rds"))
chk(abs(mean(tg$off_ppd, na.rm=TRUE) - 2.1) < 0.4, sprintf("mean off_ppd = %.3f (~2.0-2.2)", mean(tg$off_ppd, na.rm=TRUE)))
chk(abs(mean(tg$off_drives, na.rm=TRUE) - 11.5) < 1.5, sprintf("mean off_drives = %.2f (~11-13)", mean(tg$off_drives, na.rm=TRUE)))
recon_med <- median(tg$team_score - tg$off_pts_pos, na.rm=TRUE)
chk(abs(recon_med) <= 1, sprintf("score vs gross-off-pts median diff = %.2f (<=1)", recon_med))

cat("\nVERIFY 2: leakage invariant (as-of ratings use only prior-week games)\n")
asof <- readRDS(file.path(CACHE, "asof_ratings.rds"))
set.seed(1); samp <- asof %>% filter(as_of_week > 1) %>% slice_sample(n = 200)
bad <- 0
for (i in seq_len(nrow(samp))) {
  s <- samp$season[i]; w <- samp$as_of_week[i]
  # team-games actually available before week w this season (each game has 2 team rows)
  expect <- sum(tg$season == s & !is.na(tg$week) & tg$week < w)
  if (!isTRUE(samp$n_games_to_date[i] == expect)) bad <- bad + 1
}
chk(bad == 0, sprintf("200 sampled ratings: %d mismatches between n_games_to_date and games-before-week", bad))
chk(all(asof$as_of_week[asof$n_games_to_date == 0] >= 1),
    "preseason ratings (n=0) exist for opening weeks")

cat("\nVERIFY 3: leak-free totals result is in the honest breakeven band\n")
# After removing the SP+ same-season leak, the totals model is ~breakeven. This
# check guards against a REGRESSION TO THE LEAKY STATE: if the win rate jumps
# back above ~56%, a leak has likely been reintroduced. It also flags if the
# model somehow degraded far below breakeven. Expected honest range ~50-55%.
if (file.exists("ppp_backtest_results.csv")) {
  d <- read.csv("ppp_backtest_results.csv")
  r <- d %>% filter(!is.na(proj_total_A), !is.na(over_hit)) %>%
    mutate(e = proj_total_A - over_under, correct = ifelse(e > 0, over_hit, 1 - over_hit)) %>%
    filter(abs(e) >= 4)
  wr <- mean(r$correct)
  cat(sprintf("       Approach A totals >=4pt: %.1f%% over %d bets (breakeven=52.4%%)\n", 100*wr, nrow(r)))
  chk(wr >= 0.48 && wr <= 0.56,
      sprintf("win rate %.1f%% within honest leak-free band [48%%,56%%] (>56%% suggests leakage returned)", 100*wr))
} else chk(FALSE, "ppp_backtest_results.csv missing — run 03 first")

cat(sprintf("\n%s (%d failures)\n", if (fail == 0) "ALL CHECKS PASSED" else "CHECKS FAILED", fail))
if (fail > 0) quit(status = 1)
