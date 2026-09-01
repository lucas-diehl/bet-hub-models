suppressPackageStartupMessages(library(tidyverse))

# ── 1. SP+ timing ──────────────────────────────────────────────────────────
sp <- readRDS("data_cache/sp_ratings.rds")
cat("SP+ columns:", paste(names(sp), collapse = ", "), "\n\n")

cat("SP+ rows per team-year (2023 sample):\n")
sp %>%
  filter(year == 2023) %>%
  group_by(team) %>%
  summarise(n_rows = n(), .groups = "drop") %>%
  count(n_rows) %>%
  print()

cat("\nSP+ 2023 — first rows for sample of teams:\n")
sp %>%
  filter(year == 2023, team %in% c("Alabama", "Georgia", "Ohio State", "Michigan")) %>%
  arrange(team) %>%
  print()

# ── 2. Betting lines — what do the raw rows look like? ────────────────────
cat("\n\nBETTING LINES columns:\n")
bl <- readRDS("data_cache/betting_lines.rds")
cat(paste(names(bl), collapse = ", "), "\n\n")

cat("Sample rows (2023 week 5):\n")
bl %>%
  filter(season == 2023, week == 5) %>%
  select(any_of(c("game_id","season","week","home_team","away_team",
                  "provider","spread","over_under","open_spread",
                  "formatted_spread","start_date","lines_date"))) %>%
  head(10) %>%
  print()

cat("\nDistinct provider counts by year:\n")
bl %>%
  group_by(season, provider) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = provider, values_from = n, values_fill = 0) %>%
  print(width = 120)
