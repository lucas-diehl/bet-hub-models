# ============================================================================
# 05_build_features.R  —  coaching / sequencing PBP features (raw team-game)
# ----------------------------------------------------------------------------
# Computes behavioral features the "establishment" rate-stat systems miss, at
# team-game level, from the cached play-by-play. The test harness (test_features.R)
# builds AS-OF (week N uses <N) and coach-persistent versions and checks whether
# each BEATS THE MARKET (adds info beyond the line) with season consistency.
#
# Discipline (per the leakage audit):
#   * garbage-time filter is defined HERE, before features are computed, using
#     the same score/period rule as 01_build_possessions.R (never tuned to results)
#   * this script only computes raw per-game aggregates; all lagging/as-of logic
#     lives in the test harness so there is no lookahead baked into the cache
#
# Features (offense = pos_team):
#   entropy inputs : run/pass counts in 9 cells (down 1-3 x dist S/M/L) -> predictability
#   4th-down       : go-for-it vs kick counts in "decision range" -> aggressiveness
#   2nd-and-short  : pass rate & EPA on 2nd-and-<=3 -> free-play aggression
#   pace/context   : offensive plays (non-garbage)
#
# Output: data_cache/team_game_features.rds
# ============================================================================

suppressWarnings(suppressMessages({ library(dplyr); library(tidyr) }))
set.seed(42)
CACHE <- "data_cache"
GARB_H2_MARGIN <- 28; GARB_Q4_MARGIN <- 21   # same rule as 01

read_rds_retry <- function(path, tries = 4, wait = 2) {
  for (i in seq_len(tries)) { out <- tryCatch(readRDS(path), error = function(e) e)
    if (!inherits(out, "error")) return(out); if (i < tries) Sys.sleep(wait) }
  stop(sprintf("Failed to read %s", path))
}

cat("BUILD FEATURES: coaching/sequencing PBP\n"); cat(strrep("-",70), "\n")
cat("  loading pbp (large)...\n")
pbp_full <- read_rds_retry(file.path(CACHE, "pbp_data.rds"))
need <- c("season","week","game_id","pos_team","def_pos_team","down","distance",
          "yards_to_goal","pass","rush","play_type","EPA","period","pos_score_diff")
p <- pbp_full %>% select(any_of(need)); rm(pbp_full); invisible(gc())
# FBS mapper (as in 01/02) so team names match the ratings universe
sp <- read_rds_retry(file.path(CACHE,"sp_ratings.rds"))
fbs <- split(sp$team, as.integer(sp$year))
to_fbs <- function(s,t) ifelse(mapply(function(a,b){st<-fbs[[as.character(a)]];!is.null(st)&&b%in%st},s,t),t,"FCS")

# garbage-time flag (defined before features; not tuned to results)
p <- p %>% mutate(
  garbage = !is.na(period) & !is.na(pos_score_diff) &
            ((period>=3 & abs(pos_score_diff)>GARB_H2_MARGIN) |
             (period>=4 & abs(pos_score_diff)>GARB_Q4_MARGIN)),
  is_rp = (pass==1 | rush==1) %in% TRUE,
  dist_bkt = case_when(distance<=3 ~ "S", distance<=7 ~ "M", TRUE ~ "L"),
  cell = paste0("d", down, dist_bkt))
pc <- p %>% filter(!garbage, is_rp, down %in% 1:3, !is.na(distance)) %>%
  mutate(pos_team = to_fbs(season, pos_team))

# ---- entropy inputs: pass/run counts per team-game per cell (wide) ----------
cells <- as.vector(outer(paste0("d",1:3), c("S","M","L"), paste0))
ent <- pc %>% group_by(season, game_id, team = pos_team, cell) %>%
  summarise(np = sum(pass==1, na.rm=TRUE), nr = sum(rush==1, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(names_from = cell, values_from = c(np, nr), values_fill = 0)
# ensure all cell columns exist
for (c0 in cells) for (pre in c("np_","nr_")) if (!paste0(pre,c0) %in% names(ent)) ent[[paste0(pre,c0)]] <- 0

# ---- 4th-down aggressiveness: go vs kick in decision range ------------------
fourth <- p %>% filter(!garbage, down==4, !is.na(distance), distance<=7,
                       yards_to_goal>=5, yards_to_goal<=70) %>%
  mutate(pos_team = to_fbs(season, pos_team),
         go   = (pass==1 | rush==1) %in% TRUE,
         kick = grepl("Punt|Field Goal", play_type, ignore.case=TRUE)) %>%
  filter(go | kick) %>%
  group_by(season, game_id, team = pos_team) %>%
  summarise(n4_go = sum(go), n4_tot = sum(go|kick), .groups="drop")

# ---- 2nd-and-short aggression ----------------------------------------------
short2 <- pc %>% filter(down==2, distance<=3) %>%
  group_by(season, game_id, team = pos_team) %>%
  summarise(n2s_pass = sum(pass==1, na.rm=TRUE), n2s_tot = n(),
            n2s_epa = sum(EPA, na.rm=TRUE), .groups="drop")

# ---- offensive plays (non-garbage) for context -----------------------------
plays <- pc %>% group_by(season, game_id, team = pos_team) %>%
  summarise(off_rp_plays = n(), off_epa_sum = sum(EPA, na.rm=TRUE), .groups="drop")

feats <- plays %>%
  left_join(ent,    by = c("season","game_id","team")) %>%
  left_join(fourth, by = c("season","game_id","team")) %>%
  left_join(short2, by = c("season","game_id","team")) %>%
  mutate(week = NA_integer_)
# attach week from any play row
wk <- p %>% distinct(season, game_id, week)
feats <- feats %>% select(-week) %>% left_join(wk, by = c("season","game_id"))

saveRDS(feats, file.path(CACHE, "team_game_features.rds"))
cat(sprintf("\n  team-game feature rows: %d  cols: %d\n", nrow(feats), ncol(feats)))
cat(sprintf("  league 4th-down go rate (decision range): %.3f\n",
            sum(feats$n4_go, na.rm=TRUE)/sum(feats$n4_tot, na.rm=TRUE)))
cat(sprintf("  league 2nd-&-short pass rate: %.3f\n",
            sum(feats$n2s_pass, na.rm=TRUE)/sum(feats$n2s_tot, na.rm=TRUE)))
cat("✓ saved data_cache/team_game_features.rds\n")
