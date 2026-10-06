## ---------------------------------------------------------------------------
## 74_td_empty_board_diagnostic.R  (numbered 71 in the original draft; 71 is
## already taken in this repo by scripts/71_dfs_salary_margin_significance.R)
##
## Why did the Week 1 board return 0 CORE / 0 EXPANDED?
##
## Three candidate causes, which look identical on the bet card but call for
## completely different responses:
##
##   A. BLEND SATURATION — stage 3 put ~all weight on market_logit, so
##      model_probability is a near-copy of consensus and cannot disagree with
##      it by 5%. If this is the cause, NO amount of fundamental feature work
##      (interactions included) reaches the board. The fundamental signal is
##      being zeroed out after the features are computed, not before.
##
##   B. ELIGIBILITY GATE — edges exist but the low_total <= 42 / TE /
##      spread <= -6 conditions filter them out. These are fixed constants in
##      config/td_2026.yml derived from a discovery-era backtest and, per
##      handoff §6, never revisited. Week 1 totals in the modern NFL rarely sit
##      at or below 42, so this gate may be near-vacuous by construction.
##
##   C. GENUINELY TIGHT MARKET — books agree with each other and with the
##      model. Nothing to bet. Correct, boring, and a real possible answer.
##
## This script tells you which. It changes no thresholds and produces no bets.
##
## Run: & $rscript scripts/74_td_empty_board_diagnostic.R
## ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

BOARD_PATH <- "outputs/td_2026_bet_card.csv"
MODEL_PATH <- "data/processed/td_deployment_model.rds"

board <- read_csv(BOARD_PATH, show_col_types = FALSE)
cat("Board rows:", nrow(board), "\n\n")

## ===========================================================================
## CAUSE A — is the blend saturated?
## ===========================================================================

cat("=========== A. BLEND SATURATION ===========\n\n")

mdl <- readRDS(MODEL_PATH)

## The market calibrator is a glm somewhere in the deployment object; find it
## rather than assuming a slot name.
find_market_cal <- function(x, depth = 0) {
  if (depth > 4 || is.null(x)) return(NULL)
  if (inherits(x, "glm") &&
      any(grepl("market_logit", names(coef(x)), fixed = TRUE))) return(x)
  if (is.list(x)) {
    for (el in x) {
      hit <- find_market_cal(el, depth + 1)
      if (!is.null(hit)) return(hit)
    }
  }
  NULL
}

mc <- find_market_cal(mdl)

if (is.null(mc)) {
  cat("Could not locate the market calibrator in", MODEL_PATH, "\n")
  cat("Print str(mdl, max.level = 2) and tell me the slot name.\n\n")
} else {
  cf <- coef(mc)
  print(round(cf, 4))
  cat("\nRead this as follows:\n")
  cat("  market_logit near 1.0 and fundamental_logit near 0.0\n")
  cat("    -> the blend has learned 'just use the market'. CAUSE A confirmed.\n")
  cat("    -> the fundamental model is decorative at deployment. Feature work\n")
  cat("       cannot reach the board until this coefficient is non-trivial.\n")
  cat("  fundamental_logit materially above ~0.15\n")
  cat("    -> the fundamental signal is genuinely surviving the blend;\n")
  cat("       the empty board is coming from B or C instead.\n\n")

  fund_coef <- cf[grepl("fundamental", names(cf))]
  if (length(fund_coef) && abs(fund_coef) < 0.15) {
    cat(">>> fundamental_logit =", round(fund_coef, 4),
        "-- blend is effectively pure market.\n\n")
  }
}

## Direct empirical version of the same question, on tonight's actual board.
if (all(c("model_probability", "consensus_probability") %in% names(board))) {
  d <- board %>% filter(!is.na(model_probability), !is.na(consensus_probability))
  r  <- cor(d$model_probability, d$consensus_probability)
  fit <- lm(qlogis(pmin(pmax(d$model_probability, .005), .995)) ~
              qlogis(pmin(pmax(d$consensus_probability, .005), .995)))
  cat(sprintf("cor(model, consensus) on live board = %.4f\n", r))
  cat(sprintf("logit slope = %.3f, residual SD = %.4f\n",
              coef(fit)[2], sd(resid(fit))))
  cat("  cor > 0.995 and residual SD < 0.05 means the model is a\n")
  cat("  reparameterization of the market. No disagreement is possible.\n\n")
}

## ===========================================================================
## CAUSE B — is the eligibility gate binding, independent of edge?
## ===========================================================================

cat("=========== B. ELIGIBILITY GATE ===========\n\n")

stopifnot(all(c("total_line", "position", "team_spread") %in% names(board)))

elig <- board %>%
  mutate(
    low_total      = total_line <= 42,
    tight_end      = position == "TE",
    heavy_favorite = team_spread <= -6,
    core_elig      = low_total | tight_end,
    exp_elig       = low_total | tight_end | heavy_favorite
  )

cat("Rows satisfying each condition, IGNORING edge entirely:\n")
print(elig %>% summarise(
  n              = n(),
  low_total      = sum(low_total, na.rm = TRUE),
  tight_end      = sum(tight_end, na.rm = TRUE),
  heavy_favorite = sum(heavy_favorite, na.rm = TRUE),
  core_eligible  = sum(core_elig, na.rm = TRUE),
  exp_eligible   = sum(exp_elig, na.rm = TRUE)
))

cat("\nTotal-line distribution on this slate:\n")
print(summary(elig$total_line))
cat("\n  If low_total is ~0 and core_eligible is carried entirely by TEs,\n")
cat("  the 42 threshold is near-vacuous on a modern slate and the gate is\n")
cat("  doing something nobody designed it to do.\n\n")

## ===========================================================================
## Which constraint is actually binding? Cross-tabulate.
## ===========================================================================

cat("=========== BINDING CONSTRAINT ===========\n\n")

cat("Counts by (edge threshold) x (eligibility):\n")
print(
  elig %>%
    summarise(
      edge_2pct_only      = sum(relative_edge >= 0.02, na.rm = TRUE),
      edge_5pct_only      = sum(relative_edge >= 0.05, na.rm = TRUE),
      elig_only_core      = sum(core_elig, na.rm = TRUE),
      both_core           = sum(relative_edge >= 0.05 & core_elig, na.rm = TRUE),
      both_expanded       = sum(relative_edge >= 0.02 & exp_elig, na.rm = TRUE)
    )
)

cat("\n  edge_2pct_only ~ 0  -> CAUSE A or C. The gate is irrelevant;\n")
cat("                         there are no edges to gate.\n")
cat("  edge_2pct_only large but both_expanded ~ 0 -> CAUSE B. Edges exist\n")
cat("                         and the eligibility conditions are killing them.\n\n")

cat("relative_edge distribution:\n")
print(round(quantile(elig$relative_edge,
                     c(0, .05, .25, .5, .75, .90, .95, .99, 1),
                     na.rm = TRUE), 4))

## ===========================================================================
## CAUSE C — is there price-capture opportunity, model-free?
##
## This is the one number that matters most, because handoff §4/§7.4 says the
## real return has come from book disagreement, not from out-forecasting.
## It is computed WITHOUT model_probability, so it is unaffected by A.
## ===========================================================================

cat("\n=========== C. BOOK DISPERSION (MODEL-FREE) ===========\n\n")

if (all(c("best_implied_probability", "consensus_probability") %in% names(board))) {
  disp <- board %>%
    filter(!is.na(best_implied_probability), !is.na(consensus_probability)) %>%
    mutate(capture = (consensus_probability - best_implied_probability) /
             consensus_probability)

  cat("Price-capture available (consensus vs best price), as a fraction:\n")
  print(round(quantile(disp$capture, c(.5, .75, .90, .95, .99, 1),
                       na.rm = TRUE), 4))
  cat("\n  n with >= 5% capture:", sum(disp$capture >= 0.05, na.rm = TRUE), "\n")
  cat("  n with >= 8% capture:", sum(disp$capture >= 0.08, na.rm = TRUE), "\n\n")
  cat("  Healthy dispersion here alongside zero model edge is the signature\n")
  cat("  of A: the books disagree with EACH OTHER, but the model has no\n")
  cat("  independent opinion about which side of that disagreement is right.\n")
  cat("  That is the §7.4 problem, and it needs a different label, not a\n")
  cat("  better P(anytime_td).\n\n")
}

cat("=========================================\n")
