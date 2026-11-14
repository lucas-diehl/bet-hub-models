# Unit tests: NFL injury pricing + vacated-opportunity redistribution (sports/nfl/vacated.R)
# and the slate_sim handcuff coupling / proj_active mean (spine/R/slate_sim.R).
# Run: Rscript tests/test_nfl_vacated.R  (from dfs-engine/)
suppressPackageStartupMessages(library(data.table))
source("bootstrap.R"); dfs_load_spine(); dfs_load_sport("nfl")
msg <- function(...) invisible(NULL)
n_ok <- 0L; n_bad <- 0L
ok <- function(cond, label) { if (isTRUE(cond)) { n_ok <<- n_ok + 1L; cat("ok   -", label, "\n") } else { n_bad <<- n_bad + 1L; cat("FAIL -", label, "\n") } }

mk <- function() data.table(
  player_id = c("rb1", "rb2", "rb3", "wr1", "wr2", "te1", "qb1"),
  player_name = c("Starter Back", "Change Back", "Third Back", "Alpha Wr", "Beta Wr", "Tight End", "Quarter Back"),
  norm = c("starter back", "change back", "third back", "alpha wr", "beta wr", "tight end", "quarter back"),
  team = "NE", game_id = "NE@DET", position = c("RB", "RB", "RB", "WR", "WR", "TE", "QB"),
  salary = 5000L, proj = c(12, 5.5, 2.4, 14, 9, 7, 18), sim_sd = 5, ceil = c(12, 5.5, 2.4, 14, 9, 7, 18) + 4,
  floor = 1, p_zero = c(0.27, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03))
last_first <- data.table(norm = c("starter back", "gone back"), team = "NE", played_last = TRUE)   # both played last game

# 1. ONE penalty for a Questionable starter: proj = proj_active * (1 - p)
P <- nfl_price_and_redistribute(mk(), ext = NULL, inj = NULL, last = last_first)
s <- P[player_id == "rb1"]
ok(abs(s$proj - 12 * (1 - 0.27)) < 1e-9, "Questionable starter priced once: proj = 12 * 0.73")
ok(abs(s$proj_active - 12) < 1e-9, "proj_active keeps value-if-active (12)")
ok(isTRUE(s$inj_priced), "starter flagged inj_priced (apply_inactives will not discount again)")

# 2. coupled handcuff: RB2 gets bump b = 0.52 * 12 * 0.79 only when the starter sits
c2 <- P[player_id == "rb2"]; b <- 0.52 * 12 * (0.79 / (0.79 + 0.16))   # 2 remaining RBs -> shares renormalised
ok(identical(c2$cuff_of, "rb1"), "RB2 coupled to the Questionable starter")
ok(abs(c2$cuff_bump - b) < 1e-6, sprintf("RB2 coupled bump = %.2f (52%% x 12 x renormalised rank-1 share)", b))
ok(abs(c2$proj - (5.5 + 0.27 * b)) < 1e-6, "RB2 expected proj = 5.5 + P(starter out) * bump")
ok(abs(c2$proj_active - 5.5) < 1e-9, "RB2 proj_active excludes the coupled bump (added only in sims)")

# 3. sim: RB2 scores +b exactly when the starter is zeroed; starter mean = 12 * 0.73
sim <- slate_sim(P, loadings = rep(0, nrow(P)), n_sims = 40000L, seed = 1)
sc <- sim$scores; i1 <- which(P$player_id == "rb1"); i2 <- which(P$player_id == "rb2")
sits <- sc[i1, ] == 0
ok(abs(mean(sits) - 0.27) < 0.01, sprintf("starter sits in %.1f%% of sims (p_zero 27%%)", 100 * mean(sits)))
Pt <- copy(P)[player_id %in% c("rb1", "rb2"), sim_sd := 0.3]   # tight spread: a 0 can only mean "sat"
sct <- slate_sim(Pt, loadings = rep(0, nrow(Pt)), n_sims = 40000L, seed = 1)$scores
st1 <- sct[i1, ] == 0; live2 <- sct[i2, ] > 0
d <- mean(sct[i2, st1 & live2]) - mean(sct[i2, !st1 & live2])
ok(abs(d - b) < 0.1, sprintf("RB2 scores +%.2f more in sims where the starter sits (target +%.2f)", d, b))
ok(abs(mean(sc[i1, ]) - 12 * 0.73) < 0.25, sprintf("starter sim mean %.2f ~ 12*0.73 = %.2f (DNP applied once)", mean(sc[i1, ]), 12 * 0.73))

# 4. OUT player only in ext (DK already dropped him): deterministic bump, continuing absence
ext <- data.table(norm = "gone back", team = "NE", position = "RB", e_proj = 10, e_pzero = 0.97)
last_cont <- data.table(norm = "gone back", team = "NE", played_last = FALSE)
P2 <- nfl_price_and_redistribute(mk()[, p_zero := 0.03], ext = ext, inj = NULL, last = last_cont)
r1 <- P2[player_id == "rb1"]; exp_b <- 0.97 * 0.27 * 10 * 0.71 / (0.71 + 0.20 + 0.09)
ok(is.na(r1$cuff_of) && abs(r1$proj - (12 + exp_b)) < 1e-6, sprintf("OUT RB (continuing) -> starter +%.2f deterministic", exp_b))
wr <- P2[player_id == "wr1"]
ok(wr$proj > 14 && wr$proj < 14 + 0.10 * 10, "RB out spills a small share to pass-catchers (continuing 10%)")

# 5. cap: a huge vacated load can't more than max(1x own, 8) a recipient
ext3 <- data.table(norm = "gone back", team = "NE", position = "RB", e_proj = 60, e_pzero = 0.97)
P3 <- nfl_price_and_redistribute(mk()[, p_zero := 0.03], ext = ext3, inj = NULL, last = data.table(norm = "gone back", team = "NE", played_last = TRUE))
ok(P3[player_id == "rb3", vac_bump] <= 8 + 1e-9, "bump capped at max(1x own proj, 8)")

# 6. healthy pool, no ext -> unchanged projections
P4 <- nfl_price_and_redistribute(mk()[, p_zero := 0.03], ext = NULL, inj = NULL, last = last_first)
ok(isTRUE(all.equal(P4$proj, mk()$proj)), "no injuries -> projections untouched")

# 7. apply_inactives respects inj_priced (no second discount)
P5 <- copy(P); before <- P5[player_id == "rb1", proj]
inj_fake <- function(sport) data.table(norm = "starter back", cat = "questionable")
local({ injury_report <<- inj_fake; P6 <- apply_inactives(P5, "nfl"); ok(abs(P6[player_id == "rb1", proj] - before) < 1e-9, "apply_inactives skips an already-priced Questionable") })

cat(sprintf("\n%d passed, %d failed\n", n_ok, n_bad)); if (n_bad) quit(status = 1) else cat("ALL PASS\n")
