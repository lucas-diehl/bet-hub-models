# ==============================================================================
# DFS ENGINE — CONTEST SELECTION (P3): which GPPs to enter, ranked by +EV structure
#
# Projections + lineup construction decide WHO you play; contest selection decides
# WHERE you play them — and it's a huge, under-exploited EV lever (only ~85% of entry
# fees return as prizes; a minority of entries are +EV once rake is paid). This scores
# every live DK contest on a slate by the structural levers the sharp literature agrees
# on: RAKE (avoid >~14%), ENTRY CAP / field softness (single- & small-max fields are
# softer than 150-max mass-multi-entry), FIELD SIZE, OVERLAY (guaranteed pools filling
# slowly = free EV), and payout TOP-HEAVINESS (from the real prize table for the
# shortlist). All from DK's free public lobby JSON. No projection needed — pure structure.
#
#   score_contests("wnba")                 # rank today's WNBA GPPs
#   score_contests("golf", fee_max = 25)   # cap the buy-in
# Sourced by bootstrap after dk_scrape.R (reuses .dk_get_json / .DK_SPORT / DK_SPORT_MATCH)
# + dk_contest.R (real payout table for top-heaviness).
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# ── list + normalize the live DK contests for a sport ─────────────────────────
# The lobby getcontests?sport= param is ignored (returns ALL sports), so filter by the
# sport's name/league matcher (same as dk_find_slates). Optionally restrict to one slate.
dk_list_contests <- function(sport, draft_group = NULL) {
  dks <- .DK_SPORT[[sport]] %||% toupper(sport)
  j <- .dk_get_json(paste0("https://www.draftkings.com/lobby/getcontests?sport=", dks))
  if (is.null(j) || is.null(j$Contests)) return(NULL)
  ct <- as.data.table(j$Contests)
  lg <- if ("attr.League" %in% names(ct)) ct[["attr.League"]] else rep(NA_character_, nrow(ct))
  mf <- DK_SPORT_MATCH[[sport]]; if (is.null(mf)) mf <- function(n, l) grepl(sport, n, ignore.case = TRUE)
  ct <- ct[which(mf(ct$n, lg))]
  if (!nrow(ct)) return(NULL)
  g    <- function(col, f = as.numeric) if (col %in% names(ct)) suppressWarnings(f(ct[[col]])) else rep(NA, nrow(ct))
  flag <- function(col) if (col %in% names(ct)) ct[[col]] %in% TRUE else rep(FALSE, nrow(ct))
  out <- data.table(
    contest_id = as.character(ct$id), name = as.character(ct$n), dg = g("dg", as.integer),
    fee = g("a"), field = g("m", as.integer), entries = g("nt", as.integer),
    cap = g("mec", as.integer), prize = g("po"),
    guaranteed = flag("attr.IsGuaranteed"), double_up = flag("attr.IsDoubleUp"),
    fifty = flag("attr.IsFiftyfifty"), wta = flag("attr.IsWinnerTakeAll"),
    qualifier = flag("attr.IsQualifier"), game_type = as.character(g("gameType", as.character)),
    start = as.character(g("sdstring", as.character)))
  if (!is.null(draft_group)) out <- out[dg == as.integer(draft_group)]
  out <- out[is.finite(fee) & fee > 0 & is.finite(field) & field > 0 & is.finite(prize)]
  # GPP = not cash/50-50/double-up/satellite/qualifier/WTA (those are separate games).
  # Some 50-50s/double-ups lack the attr flag, so also catch them by name.
  out[, gpp := !double_up & !fifty & !qualifier & !wta &
        !grepl("satellite|qualifier|single stat|winner take all|freeroll|50-?50|50/50|double[ -]?up|top 50%",
               name, ignore.case = TRUE)]
  unique(out, by = "contest_id")[]
}

# real payout top-heaviness for one contest (first-place share of pool + paid fraction),
# from the actual prize table. One extra API call — used only for the shortlist.
.contest_topheavy <- function(contest_id, field) {
  c <- tryCatch(dk_contest(contest_id), error = function(e) NULL)
  if (is.null(c) || is.null(c$prizes) || !nrow(c$prizes)) return(list(first_pct = NA_real_, paid_pct = NA_real_))
  pz <- as.data.table(c$prizes)
  pool <- sum(pz$prize * (pz$max_rank - pz$min_rank + 1L), na.rm = TRUE)
  first <- pz[min_rank == 1L, prize][1]
  paid  <- max(pz$max_rank, na.rm = TRUE)
  list(first_pct = if (is.finite(pool) && pool > 0) first / pool else NA_real_,
       paid_pct  = if (is.finite(field) && field > 0) paid / field else NA_real_)
}

# ── score + rank the GPPs on a slate ──────────────────────────────────────────
# Score (0-100, higher = better structure) blends: rake (45%), entry-cap softness
# (30%), field size (10%), overlay signal (15%). Top-heaviness is REPORTED for the
# shortlist (informs entry count / uniqueness) but not scored — it's a style choice.
# cap_min/cap_max filter your FORMAT: cap_max=1 = single-entry only (softest),
# cap_min=20 = large-field multi-entry tournaments only. Default spans both.
score_contests <- function(sport, draft_group = NULL, fee_min = 0, fee_max = Inf,
                           cap_min = 1L, cap_max = Inf, gpp_only = TRUE, min_field = 5L,
                           shortlist = 12L, fetch_topheavy = TRUE) {
  ct <- dk_list_contests(sport, draft_group)
  if (is.null(ct) || !nrow(ct)) { msg("contest-select: no live ", sport, " contests"); return(NULL) }
  if (gpp_only) ct <- ct[gpp == TRUE]
  ct <- ct[field >= min_field & fee >= fee_min & fee <= fee_max &
             cap >= cap_min & cap <= cap_max]
  if (!nrow(ct)) { msg("contest-select: no ", sport, " GPPs in fee/field/cap range"); return(NULL) }

  ct[, rake := round(pmax(0, 1 - prize / (field * fee)), 4)]
  ct[, fill := round(entries / field, 3)]                         # current fill (overlay watch)
  # component scores in 0..1
  ct[, s_rake  := pmax(0, pmin(1, (0.25 - rake) / (0.25 - 0.10)))]            # 1 @ <=10%, 0 @ >=25%
  ct[, s_cap   := fifelse(cap <= 3L, 1.0, fifelse(cap <= 20L, 0.80,           # softer, less mass-multi-entry
                   fifelse(cap <= 150L, 0.50, 0.30)))]
  ct[, s_field := pmax(0, pmin(1, 1 - (log10(pmax(field, 10)) - 2) / 3))]     # smaller field slightly better
  ct[, s_over  := fifelse(guaranteed & is.finite(fill) & fill < 0.60, 0.70,   # guaranteed + slow fill = overlay watch
                   fifelse(guaranteed, 0.35, 0.10))]
  ct[, score := round(100 * (0.45*s_rake + 0.30*s_cap + 0.10*s_field + 0.15*s_over), 1)]
  setorder(ct, -score, rake)

  # real top-heaviness for the shortlist (extra API calls)
  ct[, `:=`(first_pct = NA_real_, paid_pct = NA_real_)]
  if (fetch_topheavy && nrow(ct)) {
    for (i in seq_len(min(shortlist, nrow(ct)))) {
      th <- .contest_topheavy(ct$contest_id[i], ct$field[i])
      set(ct, i, "first_pct", th$first_pct); set(ct, i, "paid_pct", th$paid_pct)
    }
  }
  ct[, verdict := .contest_verdict(rake, cap, field, guaranteed, fill, first_pct)]
  attr(ct, "sport") <- sport
  ct[]
}

# short human-readable why-this-rank
.contest_verdict <- function(rake, cap, field, guaranteed, fill, first_pct) {
  vapply(seq_along(rake), function(i) {
    f <- character(0)
    f <- c(f, if (rake[i] <= 0.12) sprintf("low rake %.0f%%", 100*rake[i])
              else if (rake[i] >= 0.18) sprintf("HIGH rake %.0f%% (avoid)", 100*rake[i])
              else sprintf("rake %.0f%%", 100*rake[i]))
    f <- c(f, if (cap[i] <= 3L) "single/small-entry (soft field)"
              else if (cap[i] <= 20L) "<=20-max (soft-ish)"
              else sprintf("%d-max (mass-multi-entry field)", cap[i]))
    if (isTRUE(guaranteed[i]) && is.finite(fill[i]) && fill[i] < 0.6)
      f <- c(f, sprintf("GTD %.0f%% full -> overlay watch", 100*fill[i]))
    if (is.finite(first_pct[i]))
      f <- c(f, if (first_pct[i] >= 0.20) sprintf("top-heavy (1st %.0f%% of pool)", 100*first_pct[i])
                else sprintf("flatter (1st %.0f%%)", 100*first_pct[i]))
    paste(f, collapse = "; ")
  }, character(1))
}

# ── YOUR entered contests: fetch the live/upcoming ones you're in + score their
# structure (rake / field / entry-cap) so you see at a glance whether you're in +EV
# spots or the -EV mega-fields. Sport-agnostic (golf, nfl, ...). Needs the DK session
# cookie (config/dk_session.txt via dk_auth.R).
my_contest_report <- function(max_n = 50L) {
  if (!exists("dk_authed") || !dk_authed()) {
    msg("DK session not configured (config/dk_session.txt) — cannot fetch your entries."); return(NULL) }
  ids <- unique(c(tryCatch(dk_my_contests(), error = function(e) character(0)),
                  tryCatch(dk_watchlist(),  error = function(e) character(0))))
  if (!length(ids)) { msg("No entered/upcoming contests found (cookie may be stale)."); return(NULL) }
  rows <- rbindlist(lapply(head(ids, max_n), function(id) {
    c <- tryCatch(dk_contest(id), error = function(e) NULL); if (is.null(c)) return(NULL)
    rake <- if (is.finite(c$total_payouts) && is.finite(c$field_size) && is.finite(c$entry_fee) && c$entry_fee > 0)
              max(0, 1 - c$total_payouts / (c$field_size * c$entry_fee)) else NA_real_
    data.table(contest_id = id, sport = c$sport, name = c$name %||% "", fee = c$entry_fee,
               field = c$field_size, cap = c$max_entries_per_user, rake = round(rake, 4),
               is_showdown = isTRUE(c$is_showdown))
  }), fill = TRUE)
  if (is.null(rows) || !nrow(rows)) { msg("Could not fetch contest details for your entries."); return(NULL) }
  rows[, structure := fifelse(cap <= 3L, "single/small (soft)",
                       fifelse(cap <= 20L, "<=20-max", paste0(cap, "-max mega (shark-heavy)")))]
  rows[, flag := fifelse(is.finite(rake) & rake >= 0.16, "HIGH rake -> avoid",
                 fifelse(is.finite(rake) & rake >= 0.14, "rake high", "rake ok"))]
  setorder(rows, sport, -rake)
  rows[]
}
print_my_contests <- function(r) {
  if (is.null(r) || !nrow(r)) { cat("no entered contests to show\n"); return(invisible()) }
  cat(sprintf("\n== YOUR ENTERED CONTESTS (%d) ==\n", nrow(r)))
  for (i in seq_len(nrow(r))) cat(sprintf("  %-4s $%-5s %-40s | field %-7s %-26s | rake %2.0f%% (%s)\n",
      toupper(r$sport[i]), r$fee[i], substr(r$name[i], 1, 40), r$field[i], r$structure[i],
      100 * (r$rake[i] %||% NA), r$flag[i]))
  cat(sprintf("\n  mean rake %.0f%% | %d in mega-fields (cap>20) | %d single/small\n",
      100 * mean(r$rake, na.rm = TRUE), sum(r$cap > 20L, na.rm = TRUE), sum(r$cap <= 3L, na.rm = TRUE)))
  invisible(r)
}

# pretty-print the ranked recommendation
print_contest_board <- function(sc, top = 10L) {
  if (is.null(sc) || !nrow(sc)) { cat("no contests to rank\n"); return(invisible()) }
  cat(sprintf("\n== CONTEST SELECTION — %s (%d GPPs; ranked by structure) ==\n",
              toupper(attr(sc, "sport") %||% ""), nrow(sc)))
  s <- head(sc, top)
  for (i in seq_len(nrow(s))) cat(sprintf("  [%4.1f] $%-6s %-38s | field %-6s cap %-4s rake %2.0f%% | %s\n",
      s$score[i], s$fee[i], substr(s$name[i],1,38), s$field[i], s$cap[i], 100*s$rake[i], s$verdict[i]))
  invisible(sc)
}
