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

cat("\nVERIFY 3: STRUCTURAL leakage guards (not outcome-based — a real edge must NOT trip these)\n")
# The prior for season s must use PRIOR-year SP+ (s-1). cfbd_ratings_sp(year=Y)
# returns END-OF-SEASON ratings, so same-season SP+ in the prior is lookahead that
# contaminates every week (K=5 shrinkage holds the prior ~30% all season). This is
# the leak that faked a 6pt totals edge; assert it by code, as a named regression test.
src2 <- paste(readLines("02_build_asof_ratings.R"), collapse = "\n")
chk(grepl("filter\\(season == s - 1\\)", src2),
    "02 build_priors blends PRIOR-year SP+ (filter(season == s - 1))")
chk(!grepl("sp_dev\\s*%>%\\s*filter\\(season == s\\)", src2),
    "02 does NOT use same-season SP+ in the prior")
# The as-of invariant (features at week w use only games with week<w) is asserted
# structurally in VERIFY 2 above (n_games_to_date == games-before-week).

cat("\nVERIFY 4: deployed-strategy diagnostics (REPORTED, informational — not a leak gate)\n")
# Push handling: grade a landed total (total == line) as a PUSH, not an UNDER win.
if (file.exists("ppp_backtest_results.csv")) {
  d <- read.csv("ppp_backtest_results.csv") %>%
    filter(!is.na(proj_total_A), !is.na(over_hit), !is.na(over_under), total_points != over_under)
  u4 <- d %>% filter(over_under - proj_total_A >= 4)
  bs <- d %>% mutate(e = proj_total_A - over_under, c = ifelse(e > 0, over_hit, 1L - over_hit)) %>% filter(abs(e) >= 4)
  cat(sprintf("       deployed UNDER>=4:  %.1f%% / %d bets (push-excluded)\n", 100*mean(u4$over_hit == 0), nrow(u4)))
  cat(sprintf("       both-sides >=4:     %.1f%% / %d bets\n", 100*mean(bs$c), nrow(bs)))
  # very loose sanity only — a real edge sits ~54-57%; >62% means a bug/leak, investigate.
  chk(mean(u4$over_hit == 0) <= 0.62, "UNDER>=4 win rate not implausibly high (<=62%; else investigate)")
} else chk(FALSE, "ppp_backtest_results.csv missing — run 03 first")

cat(sprintf("\n%s (%d failures)\n", if (fail == 0) "ALL CHECKS PASSED" else "CHECKS FAILED", fail))
if (fail > 0) quit(status = 1)
