suppressPackageStartupMessages(library(tidyverse))

bl <- readRDS("data_cache/betting_lines.rds")

# ── Is spread the opening or closing line? ─────────────────────────────────
cat("=== SPREAD VS SPREAD_OPEN ===\n")
bl %>%
  filter(!is.na(spread_open), !is.na(spread), spread_open != spread) %>%
  mutate(movement = spread - spread_open) %>%
  summarise(
    games_with_movement = n(),
    avg_movement        = round(mean(movement), 3),
    sd_movement         = round(sd(movement), 3),
    pct_moved           = round(n() / nrow(bl) * 100, 1)
  ) %>%
  print()

cat("\nLine movement sample (2023):\n")
bl %>%
  filter(season == 2023, !is.na(spread_open), !is.na(spread),
         spread_open != spread) %>%
  transmute(week, home_team, away_team, provider,
            spread_open, spread_close = spread,
            movement = spread - spread_open) %>%
  head(12) %>%
  print()

# ── SP+ — is it preseason or end-of-season? ───────────────────────────────
cat("\n=== SP+ TIMING CHECK ===\n")
cat("Does cfbd_ratings_sp support week=0 (preseason)?\n")
sp <- readRDS("data_cache/sp_ratings.rds")
cat("Rows per team-year:\n")
sp %>% group_by(year, team) %>% summarise(n=n(),.groups="drop") %>%
  count(n) %>% print()

# Compare SP+ 2022 end-of-season vs AP preseason 2023 for top teams
cat("\nSP+ ratings used for 2023 games (via slice(1)):\n")
sp %>% filter(year == 2023) %>% arrange(ranking) %>%
  select(year, team, rating, ranking) %>%
  head(10) %>% print()

cat("\nFor reference — 2022 end-of-season SP+ (what we SHOULD use for 2023 games):\n")
sp %>% filter(year == 2022) %>% arrange(ranking) %>%
  select(year, team, rating, ranking) %>%
  head(10) %>% print()
