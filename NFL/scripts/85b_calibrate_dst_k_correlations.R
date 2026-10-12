## ---------------------------------------------------------------------------
## 85b_calibrate_dst_k_correlations.R
##
## Extends 85_calibrate_correlations.R to DST and K. That script's own header
## says why it stopped at QB/RB/WR/TE: "DST is NOT calibrated - the NFL repo
## does not project team defenses... there are no residuals to fit." Same is
## true of K. sports/nfl/correlate.R shipped hand-set values for both (DST
## game=0.35/team=0.20, K falling through to the generic "other" fallback
## 0.15/0.0) that were never checked against real data.
##
## Confirmed live 2026-10-12: those hand-set DST/K loadings were visibly
## distorting GPP lineup construction - showdown optimizer output was loading
## up on kickers/DST as "safe" correlated-upside plays, because +0.35 told the
## simulator a kicker/DST moves WITH a shootout. It doesn't: DST scoring comes
## from sacks/INTs/points-allowed, i.e. the OPPOSING offense failing, which is
## close to the opposite of what lifts the shared game factor for QB/WR/TE.
##
## No projection model exists for DST or K, so there's no "residual" the way
## QB/RB/WR/TE have one. Real DK fantasy points for both are computed directly
## from nflreadr team_stats (standard DK scoring: sacks=1, INT=2, fumble
## rec=2, safety=2, blocked kick=2, def/ST TD=6, points-allowed tiers for DST;
## FG-distance tiers, -1 per miss for K), then z-scored within-season as the
## residual stand-in - the same role apply_fantasy_variance()'s projection
## scale plays for the skill positions.
##
## QB/RB/WR/TE loadings are NOT refit here - held fixed at their already-
## calibrated, already-shipped values (85_calibrate_correlations.R, 2026-09-15)
## so this can't regress the stacking structure that's already working. Only
## DST and K's 4 parameters (game + team load each) are fit, by the same
## weighted-least-squares method, against every within-game pair that touches
## either position.
##
## Run:  & $rscript scripts/85b_calibrate_dst_k_correlations.R
## ---------------------------------------------------------------------------

source("R/utilities.R")
suppressWarnings(suppressMessages({ library(dplyr); library(tidyr); library(nflreadr) }))
source("R/dfs_value_backtest.R")
source("R/fantasy_salary_stack.R")
source("R/fantasy_variance.R")
assert_packages()
ensure_directories()

MIN_PAIRS <- 300L
SEASONS <- 2021:2025

## Held fixed - not refit (sports/nfl/correlate.R, 2026-09-15 calibration).
FIXED_GAME <- c(QB = 0.408, WR = 0.175, TE = 0.228, RB = 0.062)
FIXED_TEAM <- c(QB = 0.619, WR = 0.229, TE = 0.180, RB = -0.011)

## ---------------------------------------------------------------------------
## 1. Real DK DST / K fantasy points, from nflreadr team_stats
## ---------------------------------------------------------------------------

team_stats <- load_team_stats(SEASONS, summary_level = "week") |> filter(season_type == "REG")
sched <- load_schedules(SEASONS) |> filter(game_type == "REG") |>
  select(game_id, season, week, home_team, away_team, home_score, away_score)
sched_long <- bind_rows(
  sched |> transmute(season, week, team = home_team, pts_allowed = away_score, game_id),
  sched |> transmute(season, week, team = away_team, pts_allowed = home_score, game_id)
)

pa_bonus <- function(pa) case_when(
  pa == 0 ~ 10, pa <= 6 ~ 7, pa <= 13 ~ 4, pa <= 20 ~ 1, pa <= 27 ~ 0, pa <= 34 ~ -1, TRUE ~ -4
)
dst_raw <- team_stats |>
  transmute(season, week, team,
            sacks = coalesce(def_sacks, 0), ints = coalesce(def_interceptions, 0),
            fum_rec = coalesce(fumble_recovery_opp, 0), safeties = coalesce(def_safeties, 0),
            blocks = coalesce(def_punt_blocks,0) + coalesce(def_pat_blocks,0) + coalesce(def_fg_blocks,0),
            def_tds = coalesce(def_tds, 0)) |>
  inner_join(sched_long, by = c("season","week","team")) |>
  mutate(dst_dk = sacks*1 + ints*2 + fum_rec*2 + safeties*2 + blocks*2 + def_tds*6 + pa_bonus(pts_allowed)) |>
  group_by(season) |> mutate(z_dst = (dst_dk - mean(dst_dk)) / sd(dst_dk)) |> ungroup()

k_raw <- team_stats |>
  transmute(season, week, team,
            fg_0_39 = coalesce(fg_made_0_19,0) + coalesce(fg_made_20_29,0) + coalesce(fg_made_30_39,0),
            fg_40_49 = coalesce(fg_made_40_49, 0),
            fg_50p = coalesce(fg_made_50_59,0) + coalesce(fg_made_60_,0),
            fg_miss = coalesce(fg_missed,0) + coalesce(fg_blocked,0)) |>
  mutate(k_dk = fg_0_39*3 + fg_40_49*4 + fg_50p*5 - fg_miss*1) |>
  left_join(sched_long |> select(season, week, team, game_id), by = c("season","week","team")) |>
  group_by(season) |> mutate(z_k = (k_dk - mean(k_dk)) / sd(k_dk)) |> ungroup()

message("DST games: ", nrow(dst_raw), " | mean DK pts: ", round(mean(dst_raw$dst_dk), 2))
message("K   games: ", nrow(k_raw),   " | mean DK pts: ", round(mean(k_raw$k_dk), 2))

## ---------------------------------------------------------------------------
## 2. Skill-position residuals (identical to 85_calibrate_correlations.R)
## ---------------------------------------------------------------------------

fr <- fantasy_salary_training_frame(verbose = FALSE) |>
  filter(is.finite(.data$ppr_actual), is.finite(.data$ppr_model),
         .data$position %in% c("QB", "RB", "WR", "TE"))
feats <- readRDS("data/processed/fantasy_prop_features.rds") |>
  select(any_of(c("game_id", "player_id", "fantasy_points_ppr_sd5", "opportunity_r5"))) |>
  distinct(.data$game_id, .data$player_id, .keep_all = TRUE)
d <- fr |> left_join(feats, by = c("game_id", "player_id"))
vm <- readRDS("data/processed/fantasy_variance_model.rds")
d <- apply_fantasy_variance(d |> mutate(projected_ppr = .data$ppr_model), vm)
d <- d |>
  mutate(z = (.data$ppr_actual - .data$ppr_model) / pmax(.data$proj_scale, 0.5)) |>
  filter(is.finite(.data$z), !is.na(.data$team), !is.na(.data$game_id)) |>
  filter(.data$ppr_model >= 5)
lim <- stats::quantile(d$z, c(0.005, 0.995), na.rm = TRUE)
d <- d |> mutate(z = pmin(pmax(.data$z, lim[[1]]), lim[[2]]))
skill <- d |> transmute(game_id, player_id, team, position, z)

dst2 <- dst_raw |> transmute(game_id, player_id = paste0("DST_", team), team, position = "DST", z = z_dst)
k2 <- k_raw |> filter(!is.na(game_id)) |>
  transmute(game_id, player_id = paste0("K_", team), team, position = "K", z = z_k)

## ---------------------------------------------------------------------------
## 3. All within-game pairs touching DST and/or K
## ---------------------------------------------------------------------------

allp <- bind_rows(skill, dst2, k2)
slim <- allp |> select(game_id, player_id, team, position, z)
pairs <- slim |>
  inner_join(slim, by = "game_id", relationship = "many-to-many", suffix = c("_i", "_j")) |>
  filter(.data$player_id_i < .data$player_id_j) |>
  mutate(same_team = team_i == team_j,
         pos_a = pmin(position_i, position_j), pos_b = pmax(position_i, position_j),
         pair = paste(pos_a, pos_b, sep = "-"))
emp <- pairs |> group_by(pair, same_team) |>
  summarise(n = n(), corr = stats::cor(z_i, z_j), .groups = "drop") |>
  filter(is.finite(corr))

touch_dk <- emp |> filter(grepl("DST|K", pair))
## DST-DST (opposing) is structurally unfittable by this model for the same
## reason 85_calibrate_correlations.R excludes same-team QB-QB: cross-team
## same-position correlation = game_ld^2, always >= 0, but two different
## teams' defenses measure -0.299 - no loading vector can produce that.
fit_cells <- touch_dk |> filter(n >= MIN_PAIRS, !(pair == "DST-DST" & !same_team))
message("DST/K cells used for fit: ", nrow(fit_cells), " of ", nrow(touch_dk))

## ---------------------------------------------------------------------------
## 4. Fit DST/K's 4 parameters (QB/RB/WR/TE held fixed)
## ---------------------------------------------------------------------------

all_loads <- function(gl2, tl2) list(game = c(FIXED_GAME, gl2), team = c(FIXED_TEAM, tl2))
implied <- function(pair, same_team, loads) {
  p <- strsplit(pair, "-", fixed = TRUE)[[1]]
  a <- loads$game[[p[[1]]]] * loads$game[[p[[2]]]]
  if (!same_team) return(a)
  a + loads$team[[p[[1]]]] * loads$team[[p[[2]]]]
}

MAX_FACTOR <- 0.55   # same cap as 85_calibrate_correlations.R
obj <- function(par) {
  gl2 <- c(DST = par[1], K = par[2]); tl2 <- c(DST = par[3], K = par[4])
  loads <- all_loads(gl2, tl2)
  pred <- mapply(implied, fit_cells$pair, fit_cells$same_team, MoreArgs = list(loads = loads))
  pen <- sum(pmax(gl2^2 + tl2^2 - MAX_FACTOR, 0)^2) * 1000
  sum(fit_cells$n * (pred - fit_cells$corr)^2) / sum(fit_cells$n) + pen
}

lo <- c(g_DST = -0.95, g_K = -0.95, t_DST = -0.95, t_K = -0.95)
hi <- c(g_DST =  0.95, g_K =  0.95, t_DST =  0.95, t_K =  0.95)
best <- NULL
for (s in seq_len(20)) {
  set.seed(3000 + s)
  init <- stats::runif(4, lo, hi)
  o <- try(stats::optim(init, obj, method = "L-BFGS-B", lower = lo, upper = hi,
                        control = list(maxit = 3000)), silent = TRUE)
  if (inherits(o, "try-error")) next
  if (is.null(best) || o$value < best$value) best <- o
}
if (is.null(best)) stop("DST/K correlation fit failed.", call. = FALSE)

gl2_fit <- setNames(round(best$par[1:2], 3), c("DST", "K"))
tl2_fit <- setNames(round(best$par[3:4], 3), c("DST", "K"))
loads_fit <- all_loads(gl2_fit, tl2_fit)

## ---------------------------------------------------------------------------
## 5. Report
## ---------------------------------------------------------------------------

emp$implied_fitted <- mapply(implied, emp$pair, emp$same_team, MoreArgs = list(loads = loads_fit))
report <- emp |>
  filter(grepl("DST|K", pair) | pair %in% c("QB-WR", "QB-TE", "QB-RB", "RB-WR")) |>
  transmute(pair, teams = ifelse(same_team, "same", "opposing"), n,
           measured = round(corr, 3), fitted = round(implied_fitted, 3),
           err = round(abs(implied_fitted - corr), 3)) |>
  arrange(desc(teams), desc(n))

cat("\n================ DST/K CORRELATION CALIBRATION ================\n")
cat("\n-- measured vs fitted (skill positions held fixed) --\n")
print(as.data.frame(report), row.names = FALSE)

used <- fit_cells
pred_u <- mapply(implied, used$pair, used$same_team, MoreArgs = list(loads = loads_fit))
cat(sprintf("\n-- weighted mean absolute error over %d fitted cells --\n  %.4f\n",
            nrow(used), stats::weighted.mean(abs(pred_u - used$corr), used$n)))

cat("\n-- loadings to ship (sports/nfl/correlate.R) --\n")
print(data.frame(position = c("DST", "K"), game_fitted = unname(gl2_fit), team_fitted = unname(tl2_fit)))

cat("\n-- vs. the hand-set values being replaced --\n")
cat("  DST: game 0.35 -> ", gl2_fit[["DST"]], " | team 0.20 -> ", tl2_fit[["DST"]], "\n", sep = "")
cat("  K:   game 0.15 -> ", gl2_fit[["K"]],   " | team 0.00 -> ", tl2_fit[["K"]],   "\n", sep = "")

OUT_CSV <- "outputs/nfl_dst_k_correlation_calibration.csv"
OUT_RDS <- "data/processed/nfl_dst_k_correlation_loadings.rds"
readr::write_csv(report, OUT_CSV)
saveRDS(list(game_load = gl2_fit, team_load = tl2_fit, report = report, fitted_at = Sys.time(),
             min_pairs = MIN_PAIRS, fixed_skill_game = FIXED_GAME, fixed_skill_team = FIXED_TEAM),
        OUT_RDS)
cat("\nWrote ", OUT_CSV, " and ", OUT_RDS, "\n", sep = "")
