# Marginal (exact-k, not cumulative >=k) win rate per DVOA consensus vote
# count — the number that actually matters for a "size the stake by votes"
# decision. Reuses 11's already-computed `test`/`qualifying` build exactly.
source("00_ppp_common.R")
set.seed(42)

asof <- read_rds_retry(file.path(CACHE, "asof_ratings.rds"))
bl   <- read_rds_retry(file.path(CACHE, "betting_lines.rds"))
gi   <- read_rds_retry(file.path(CACHE, "game_info.rds"))
sp   <- read_rds_retry(file.path(CACHE, "sp_ratings.rds"))
dvr  <- read_rds_retry(file.path(CACHE, "dvoa_asof_variants.rds"))
fbs  <- make_fbs_mapper(sp)
gi2 <- gi %>% mutate(game_id = if ("game_id" %in% names(.)) game_id else id,
                     season  = if ("season"  %in% names(.)) season  else year)
gd <- gi2 %>% transmute(game_id, season, week, home_team, away_team,
  home_score = as.numeric(home_points), away_score = as.numeric(away_points),
  total_points = home_score + away_score)
md <- attach_ratings(build_model_data(bl, gd), asof, fbs) %>%
  filter(home_team != "FCS", away_team != "FCS", !is.na(spread_open))
DVOA_VARIANTS <- c("v3","v4","v5","v6","v7")
dvh <- dvr %>% rename_with(~paste0("h_", .x), -c(season, as_of_week, team)) %>% rename(home_team = team)
dva <- dvr %>% rename_with(~paste0("a_", .x), -c(season, as_of_week, team)) %>% rename(away_team = team)
md <- md %>% left_join(dvh, by = c("season","week"="as_of_week","home_team")) %>%
             left_join(dva, by = c("season","week"="as_of_week","away_team"))
for (v in DVOA_VARIANTS)
  md[[paste0("dvoa_", v)]] <- (md[[paste0("h_aoff_", v)]] + md[[paste0("a_adef_", v)]]) -
                              (md[[paste0("a_aoff_", v)]] + md[[paste0("h_adef_", v)]])
md$open_margin <- -md$spread_open
train <- md %>% filter(season %in% 2021:2023, !is.na(actual_margin))
test  <- md %>% filter(season %in% 2024:2025, !is.na(actual_margin))
cal_ppd <- lm(actual_margin ~ proj_margin_A, data = train)
cal_dvoa <- list()
for (v in DVOA_VARIANTS) {
  h2 <- train[!is.na(train[[paste0("dvoa_", v)]]), ]
  cal_dvoa[[v]] <- if (nrow(h2) >= 200) lm(as.formula(paste0("actual_margin ~ dvoa_", v)), data = h2) else NULL
}
test <- test %>% mutate(
  proj_margin = predict(cal_ppd, .), ppd_edge = proj_margin - open_margin,
  ppd_mag = abs(ppd_edge), ppd_pick_home = ppd_edge > 0,
  covered_home_open = as.integer((actual_margin + spread_open) > 0),
  push_open = (actual_margin + spread_open) == 0)
vote_mat <- sapply(DVOA_VARIANTS, function(v) {
  if (is.null(cal_dvoa[[v]])) return(rep(0L, nrow(test)))
  pv <- suppressWarnings(predict(cal_dvoa[[v]], test)); e <- pv - test$open_margin
  as.integer(!is.na(e) & abs(e) >= 3 & sign(e) == sign(test$ppd_edge))
})
test$votes <- rowSums(vote_mat)
qualifying <- test %>% filter(ppd_mag >= 3)

cat("=== MARGINAL win rate at EXACTLY k votes (not cumulative) ===\n")
marg <- lapply(0:5, function(k) {
  d <- qualifying %>% filter(votes == k, !push_open)
  win <- ifelse(d$ppd_pick_home, d$covered_home_open == 1, d$covered_home_open == 0)
  tibble(votes = k, n = length(win), win_pct = round(100*mean(win), 1))
})
print(as.data.frame(bind_rows(marg)))

cat("\nBreakeven at -110 = 52.4%\n")
readr::write_csv(bind_rows(marg), "outputs/dvoa_marginal_vote_rates.csv")
