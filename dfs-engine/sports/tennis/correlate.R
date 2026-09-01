# ==============================================================================
# Tennis plugin — correlation
# The two players in a match are strongly ANTI-correlated: one advances, the other
# is out. We give each match a shared environment factor with OPPOSITE-sign
# loadings for the two sides (rho ~ 0.5 -> pairwise corr ~ -0.5). Different matches
# are independent. This is the distinctive case the spine's factor model exists to
# handle, and the reason rostering both sides of a match is usually a trap.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

tennis_correlation <- function(pool) {
  pool <- as.data.table(pool)
  if (!"game_id" %in% names(pool)) return(NULL)
  pool[, .sgn := { s <- rep(1, .N); if (.N > 1) s[seq(2, .N, by = 2)] <- -1; s }, by = game_id]
  signs <- stats::setNames(pool$.sgn, as.character(pool$player_id))
  loadings_from_rho(pool$player_id, pool$game_id, rho = 0.5, signs = signs)
}
