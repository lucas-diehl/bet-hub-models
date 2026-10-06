## ---------------------------------------------------------------------------
## 85_calibrate_correlations.R
##
## Measures the real within-game correlation structure of fantasy residuals and
## fits DFS ENGINE's two-factor loadings to it.
##
## Why: sports/nfl/correlate.R says so itself - "game_ld/team_ld values are
## hand-set DFS-community assumptions, never calibrated". Only the blowout
## regime-shrink was ever measured. Correlation drives GPP lineup construction
## more than projection accuracy does (it decides what stacks together), so
## guessed loadings are a bigger exposure than a slightly-off point projection.
##
## The model being calibrated, from that file's own header:
##   z_i = a_i * G(game) + b_i * T(team) + idiosyncratic
##   same-team corr(i,j)  = a_i*a_j + b_i*b_j
##   cross-team corr(i,j) = a_i*a_j          (opposing teams -> team factors differ)
##
## A concrete prior suspicion worth stating up front, because it is the kind of
## thing a pure positive-loading factor model gets structurally wrong: the
## current numbers imply same-team WR-WR = 0.45^2 + 0.50^2 = 0.45. Two receivers
## on one team COMPETE for the same targets, so their residual correlation
## should be near zero or negative. If the measurement agrees, the shipped
## loadings have been over-stacking same-team pass catchers.
##
## Residuals are standardised by the SAME scale model the simulator uses
## (R/fantasy_variance.R), so the z here is the z that gets sampled - not a
## separately-invented normalisation that would calibrate to the wrong thing.
##
## Run:  & $rscript scripts/85_calibrate_correlations.R
## ---------------------------------------------------------------------------

source("R/utilities.R")
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})
source("R/dfs_value_backtest.R")
source("R/fantasy_salary_stack.R")
source("R/fantasy_variance.R")
assert_packages()
ensure_directories()

OUT_CSV <- "outputs/nfl_correlation_calibration.csv"
OUT_RDS <- "data/processed/nfl_correlation_loadings.rds"
MIN_PAIRS <- 300L          # cells thinner than this are reported but not fitted

## Current shipped values, for the side-by-side.
CURRENT_GAME <- c(QB = 0.50, WR = 0.45, TE = 0.45, RB = 0.42)
CURRENT_TEAM <- c(QB = 0.58, WR = 0.50, TE = 0.44, RB = -0.28)
POS <- c("QB", "RB", "WR", "TE")

## ---------------------------------------------------------------------------
## 1. Standardised residuals
## ---------------------------------------------------------------------------

fr <- fantasy_salary_training_frame(verbose = FALSE) |>
  filter(is.finite(.data$ppr_actual), is.finite(.data$ppr_model),
         .data$position %in% POS)

feats <- readRDS("data/processed/fantasy_prop_features.rds") |>
  select(any_of(c("game_id", "player_id", "fantasy_points_ppr_sd5",
                  "opportunity_r5"))) |>
  distinct(.data$game_id, .data$player_id, .keep_all = TRUE)

d <- fr |> left_join(feats, by = c("game_id", "player_id"))
vm <- readRDS("data/processed/fantasy_variance_model.rds")
d <- apply_fantasy_variance(d |> mutate(projected_ppr = .data$ppr_model), vm)
d <- d |>
  mutate(z = (.data$ppr_actual - .data$ppr_model) / pmax(.data$proj_scale, 0.5)) |>
  filter(is.finite(.data$z), !is.na(.data$team), !is.na(.data$game_id))

## ROSTERABLE PLAYERS ONLY. This restriction is not a nicety - calibrating on
## the full player universe produces the wrong answer, and by a lot.
##
## Measured both ways, same-team QB-WR:
##   every QB-WR pair on the roster            0.222
##   QB1 with his WR1                          0.327
##   ... decaying by depth: WR2 0.329, WR3 0.228, WR4 0.115
##
## The full-universe number averages a genuine stack together with QB-to-WR5
## pairs that will never appear in a lineup, dragging it toward zero. Loadings
## fitted on that would tell the optimizer stacking barely matters. The
## population that belongs here is the one that can actually be rostered, so
## rows are gated on a meaningful projection - roughly the DK pool's usable
## depth rather than every name on a 53-man roster.
MIN_PROJ <- 5
n_all <- nrow(d)
d <- d |> filter(.data$ppr_model >= MIN_PROJ)
message("Rosterable filter (proj >= ", MIN_PROJ, "): ",
        format(nrow(d), big.mark = ","), " of ", format(n_all, big.mark = ","),
        " player-games")

## Trim the extreme tail so a handful of 50-point outliers cannot dominate a
## correlation. 0.5% each side; the structure being measured is not in the tail.
lim <- stats::quantile(d$z, c(0.005, 0.995), na.rm = TRUE)
d <- d |> mutate(z = pmin(pmax(.data$z, lim[[1]]), lim[[2]]))
message("Player-games: ", format(nrow(d), big.mark = ","),
        " across ", format(dplyr::n_distinct(d$game_id), big.mark = ","), " games")

## ---------------------------------------------------------------------------
## 2. All within-game pairs, classified
## ---------------------------------------------------------------------------

slim <- d |> select("game_id", "player_id", "team", "position", "z")
pairs <- slim |>
  inner_join(slim, by = "game_id", relationship = "many-to-many",
             suffix = c("_i", "_j")) |>
  filter(.data$player_id_i < .data$player_id_j) |>
  mutate(
    same_team = .data$team_i == .data$team_j,
    ## Unordered position pair, so QB-WR and WR-QB are one cell.
    pos_a = pmin(.data$position_i, .data$position_j),
    pos_b = pmax(.data$position_i, .data$position_j),
    pair = paste(.data$pos_a, .data$pos_b, sep = "-")
  )
message("Within-game pairs: ", format(nrow(pairs), big.mark = ","))

emp <- pairs |>
  group_by(.data$pair, .data$same_team) |>
  summarise(n = n(), corr = stats::cor(.data$z_i, .data$z_j), .groups = "drop") |>
  filter(is.finite(.data$corr))

## What the CURRENT loadings imply for each cell.
implied <- function(pair, same_team, gl, tl) {
  p <- strsplit(pair, "-", fixed = TRUE)[[1]]
  a <- gl[[p[[1]]]] * gl[[p[[2]]]]
  if (!same_team) return(a)
  a + tl[[p[[1]]]] * tl[[p[[2]]]]
}
emp$implied_current <- mapply(implied, emp$pair, emp$same_team,
                              MoreArgs = list(gl = CURRENT_GAME, tl = CURRENT_TEAM))

## ---------------------------------------------------------------------------
## 3. Fit loadings to the measured cells
##
## Weighted least squares over cells, weighted by pair count so a thin cell
## cannot outvote a dense one. Constraint a^2 + b^2 <= 0.95 keeps idiosyncratic
## variance positive - without it the optimiser will happily push a player to
## fully explained by the factors, which makes the simulator degenerate.
## ---------------------------------------------------------------------------

## Same-team QB-QB is excluded, on two independent grounds:
##
##   1. STRUCTURALLY UNFITTABLE. Same-team, same-position correlation under this
##      model is a_i*a_i + b_i*b_i = a^2 + b^2, which is always POSITIVE. The
##      measured value is -0.483 (a starter and his backup are a substitution:
##      when one plays, the other does not). No loading vector can produce it,
##      so including the cell just drags every other parameter toward an
##      extreme trying - it was pushing QB to a^2+b^2 = 0.95, i.e. a quarterback
##      with zero idiosyncratic variance.
##   2. IRRELEVANT. DK classic has one QB slot, so two same-team QBs can never
##      appear in a lineup together. Fitting a correlation nothing can roster is
##      spending accuracy where it cannot be used.
fit_cells <- emp |>
  filter(.data$n >= MIN_PAIRS, !(.data$pair == "QB-QB" & .data$same_team))
message("Cells used for the fit: ", nrow(fit_cells), " of ", nrow(emp))

## MAX_FACTOR caps the share of a player's variance the two factors may explain,
## leaving the rest idiosyncratic. Without a real cap the optimiser drives a
## position to the boundary and the simulator then treats that player's outcome
## as fully determined by game and team - no player-specific noise at all, which
## is both wrong and would collapse lineup diversity. The measured correlations
## top out near 0.27, so a modest factor share is all the data actually asks for.
MAX_FACTOR <- 0.55

obj <- function(par) {
  gl <- setNames(par[1:4], POS)
  tl <- setNames(par[5:8], POS)
  pred <- mapply(implied, fit_cells$pair, fit_cells$same_team,
                 MoreArgs = list(gl = as.list(gl), tl = as.list(tl)))
  pen <- sum(pmax(gl^2 + tl^2 - MAX_FACTOR, 0)^2) * 1000
  sum(fit_cells$n * (pred - fit_cells$corr)^2) / sum(fit_cells$n) + pen
}

## Team loadings are constrained non-negative for QB/WR/TE and non-positive for
## RB. Products of two negatives reproduce the same correlations, so the fit is
## sign-degenerate; pinning the convention keeps the shipped numbers readable
## against the file they replace ("positive = benefits when the team scores",
## RB negative for game script) instead of an equivalent all-negative solution
## that means the same thing but reads as nonsense.
lo <- c(rep(0, 4), 0, -0.95, 0, 0)      # game: all >= 0 | team: QB,WR,TE >= 0; RB <= 0
hi <- c(rep(0.95, 4), 0.95, 0, 0.95, 0.95)
names(lo) <- names(hi) <- c(paste0("g_", POS), paste0("t_", POS))

start <- pmin(pmax(c(CURRENT_GAME[POS], CURRENT_TEAM[POS]), lo), hi)
best <- NULL
for (s in seq_len(12)) {
  set.seed(1000 + s)
  init <- if (s == 1) start else stats::runif(8, lo, hi)
  o <- try(stats::optim(init, obj, method = "L-BFGS-B",
                        lower = lo, upper = hi,
                        control = list(maxit = 2000)), silent = TRUE)
  if (inherits(o, "try-error")) next
  if (is.null(best) || o$value < best$value) best <- o
}
if (is.null(best)) stop("Correlation fit failed.", call. = FALSE)

gl_fit <- setNames(round(best$par[1:4], 3), POS)
tl_fit <- setNames(round(best$par[5:8], 3), POS)

emp$implied_fitted <- mapply(implied, emp$pair, emp$same_team,
                             MoreArgs = list(gl = as.list(gl_fit),
                                             tl = as.list(tl_fit)))

## ---------------------------------------------------------------------------
## 4. Report
## ---------------------------------------------------------------------------

report <- emp |>
  transmute(
    pair = .data$pair,
    teams = ifelse(.data$same_team, "same", "opposing"),
    n = .data$n,
    measured = round(.data$corr, 3),
    current = round(.data$implied_current, 3),
    fitted = round(.data$implied_fitted, 3),
    err_current = round(abs(.data$implied_current - .data$corr), 3),
    err_fitted = round(abs(.data$implied_fitted - .data$corr), 3)
  ) |>
  arrange(.data$teams, desc(.data$n))

cat("\n================ CORRELATION CALIBRATION ================\n")
cat("\n-- measured vs shipped vs fitted --\n")
print(as.data.frame(report), row.names = FALSE)

used <- report |> filter(.data$n >= MIN_PAIRS)
cat(sprintf(
  "\n-- weighted mean absolute error over %d fitted cells --\n  current: %.4f\n  fitted:  %.4f\n",
  nrow(used),
  stats::weighted.mean(used$err_current, used$n),
  stats::weighted.mean(used$err_fitted, used$n)))

cat("\n-- loadings --\n")
print(data.frame(position = POS,
                 game_current = unname(CURRENT_GAME[POS]),
                 game_fitted = unname(gl_fit[POS]),
                 team_current = unname(CURRENT_TEAM[POS]),
                 team_fitted = unname(tl_fit[POS])), row.names = FALSE)

cat("\n-- headline stacking correlations --\n")
show_pair <- function(pa, pb, same) {
  key <- paste(pmin(pa, pb), pmax(pa, pb), sep = "-")
  r <- report |> filter(.data$pair == key,
                        .data$teams == ifelse(same, "same", "opposing"))
  if (!nrow(r)) return(invisible())
  cat(sprintf("  %-22s measured %+0.3f | current %+0.3f | fitted %+0.3f  (n=%s)\n",
              paste0(pa, "-", pb, ifelse(same, " same", " opp")),
              r$measured[[1]], r$current[[1]], r$fitted[[1]],
              format(r$n[[1]], big.mark = ",")))
}
show_pair("QB", "WR", TRUE); show_pair("QB", "TE", TRUE)
show_pair("QB", "RB", TRUE); show_pair("WR", "WR", TRUE)
show_pair("RB", "WR", TRUE); show_pair("QB", "WR", FALSE)
show_pair("QB", "QB", FALSE)

readr::write_csv(report, OUT_CSV)
saveRDS(list(game_load = gl_fit, team_load = tl_fit, report = report,
             fitted_at = Sys.time(), min_pairs = MIN_PAIRS), OUT_RDS)
cat("\nWrote ", OUT_CSV, " and ", OUT_RDS, "\n", sep = "")
