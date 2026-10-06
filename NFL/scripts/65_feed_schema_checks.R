source("R/utilities.R")
source("R/dashboard_feed.R")
assert_packages()

# Checks on the publish-time guarantees added for the site schema. These run in
# seconds and need no model fit, so they can gate every publish.

failures <- 0L
check <- function(label, expr) {
  ok <- isTRUE(tryCatch(expr, error = function(e) FALSE))
  cat(if (ok) "  ok   " else "  FAIL ", label, "\n", sep = "")
  if (!ok) failures <<- failures + 1L
}
expect_error <- function(expr) {
  inherits(tryCatch(expr, error = function(e) e), "error")
}

cat("Strategy registry\n")
registry <- nfl_bet_strategies()
check("every strategy has a status",
      all(registry$status %in% feed_status_levels()))
check("the three yardage-prop rules are rejected",
      all(registry$status[registry$market == "prop"] == "rejected"))
check("the game-level portfolio is funded",
      registry$status[registry$tag == "portfolio"] == "funded")
check("unknown tags resolve to rejected",
      feed_strategy_status("not_a_strategy") == "rejected")
check("best status across a mixed bet is the strongest",
      feed_best_status(c("paper", "funded")) == "funded")

cat("\nPrice floors\n")
check("break-even floor sits below a favourable quote",
      feed_min_price(200, 0.40) < 200)
check("a 50% model prob implies an even-money floor",
      abs(feed_min_price(150, 0.5)) == 100)
check("no model prob still yields a floor",
      is.finite(feed_min_price(-110, NA_real_)))
check("the floor is never better than the quote",
      american_to_prob(feed_min_price(-110, NA_real_)) >=
        american_to_prob(-110))
check("floors are vectorised",
      length(feed_min_price(c(120, -130), c(0.5, NA_real_))) == 2)

cat("\nBook restriction\n")
check("the permitted four pass",
      isTRUE(feed_assert_books(unname(feed_books()))))
check("lower-case keys pass", isTRUE(feed_assert_books(names(feed_books()))))
check("an offshore book is rejected", expect_error(feed_assert_books("LowVig.ag")))
check("a best-of-eight book is rejected", expect_error(feed_assert_books("Bovada")))

cat("\nPublish gate\n")
good <- tibble::tibble(
  bet_id = "a", market = "spread", stat = NA_character_, line = -3.5,
  odds_american = -110L, min_price = -125L, book = "DraftKings",
  status = "funded"
)
check("a well-formed row passes", isTRUE(feed_assert_publishable(good)))
check("a missing line is rejected",
      expect_error(feed_assert_publishable(dplyr::mutate(good, line = NA_real_))))
check("anytime TD is allowed to have no line",
      isTRUE(feed_assert_publishable(dplyr::mutate(
        good, market = "prop", stat = "anytime_td", line = NA_real_
      ))))
check("a rejected status is refused",
      expect_error(feed_assert_publishable(dplyr::mutate(good, status = "rejected"))))
check("an unknown status is refused",
      expect_error(feed_assert_publishable(dplyr::mutate(good, status = "live"))))
check("a missing book is refused",
      expect_error(feed_assert_publishable(dplyr::mutate(good, book = NA_character_))))
check("a missing price floor is refused",
      expect_error(feed_assert_publishable(dplyr::mutate(good, min_price = NA_integer_))))
check("an excluded book is refused",
      expect_error(feed_assert_publishable(dplyr::mutate(good, book = "Pinnacle"))))
check("a floor better than the quote is refused",
      expect_error(feed_assert_publishable(dplyr::mutate(good, min_price = 100L))))
check("a feed with no status column is refused",
      expect_error(feed_assert_publishable(dplyr::select(good, -"status"))))

cat("\nDeduping drops rejected strategies\n")
candidates <- tibble::tibble(
  bet_key = c("g1|spread", "g2|prop"),
  bet_id = c("a", "b"), strategy = c("spread_rf6", "prop_ev18"),
  game_id = c("g1", "g2"), event = "X @ Y", event_start = NA_character_,
  market = c("spread", "prop"), market_label = NA_character_,
  selection = c("Y", "P Over 40.5"), side = c("home", "over"),
  line = c(-3.5, 40.5), edge = c(6.5, 0.2), odds_american = c(-110, -115),
  book = "DraftKings", model_prob = c(NA_real_, 0.56),
  stat = c(NA_character_, "rec_yds"), player = c(NA_character_, "P"),
  team = c(NA_character_, "Y")
)
deduped <- suppressMessages(dedupe_feed_bets(candidates))
check("the prop candidate is dropped", nrow(deduped) == 1L)
check("the spread candidate survives", deduped$bet_key[[1]] == "g1|spread")
check("the survivor is marked funded", deduped$status[[1]] == "funded")
check("the survivor carries a price floor", is.finite(deduped$min_price[[1]]))
check("a prop-only slate collapses to nothing",
      nrow(suppressMessages(dedupe_feed_bets(candidates[2, ]))) == 0L)

cat("\n", if (failures) sprintf("%d checks FAILED\n", failures) else
    "All checks passed.\n", sep = "")
quit(save = "no", status = if (failures) 1L else 0L)
