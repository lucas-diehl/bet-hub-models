# ==============================================================================
# DFS ENGINE — DraftKings CONTEST loader (real payouts + Showdown detection)
# Given a DK contest id (or URL), pulls the real contest details: entry fee, field
# size, max entries per user, the REAL prize-by-rank payout table, the draft group,
# and whether it's a Showdown / Captain Mode single-game contest. Feeds the actual
# payout curve into grading and routes Showdown slates to the captain optimizer.
# All from DK's public JSON (free).
# ==============================================================================

suppressPackageStartupMessages({ library(data.table) })

# accept a raw id or a full /draft/contest/<id> URL
dk_parse_contest_id <- function(x) {
  x <- as.character(x)
  m <- regmatches(x, regexec("contest/(\\d+)", x))[[1]]
  if (length(m) == 2) return(m[2])
  gsub("\\D", "", x)
}

# Fetch contest detail -> normalized list incl. a real payout table.
dk_contest <- function(contest_id) {
  cid <- dk_parse_contest_id(contest_id)
  j <- .dk_get_json(sprintf("https://api.draftkings.com/contests/v1/contests/%s?format=json", cid))
  cd <- if (!is.null(j)) j$contestDetail else NULL
  if (is.null(cd)) stop("Could not fetch DK contest ", cid, " (private/expired or blocked).")

  ps <- cd$payoutSummary
  prizes <- NULL
  if (!is.null(ps) && length(ps)) {
    ps <- as.data.table(ps)
    valof <- function(i) {
      pd <- ps$payoutDescriptions[[i]]
      if (is.data.frame(pd) && "value" %in% names(pd)) return(suppressWarnings(as.numeric(pd$value[1])))
      if (is.data.frame(pd) && "payoutDescription" %in% names(pd))
        return(suppressWarnings(as.numeric(gsub("[^0-9.]", "", pd$payoutDescription[1]))))
      v <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(unlist(pd)[1]))))
      if (length(v)) v[1] else NA_real_
    }
    prizes <- data.table(
      min_rank = as.integer(ps$minPosition), max_rank = as.integer(ps$maxPosition),
      prize = vapply(seq_len(nrow(ps)), valof, numeric(1)))
    prizes <- prizes[is.finite(prize) & prize > 0]
  }

  is_show <- grepl("showdown|captain|single game|single-game", tolower(cd$name %||% "")) ||
             isTRUE((cd$gameTypeId %||% 0) %in% DK_SHOWDOWN_GAMETYPES)

  start <- cd$contestStartTime %||% cd$ContestStartTime %||% NA
  cdate <- tryCatch(as.Date(substr(as.character(start), 1, 10)), error = function(e) NA)
  list(contest_id = cid, name = cd$name, sport = tolower(cd$sport %||% ""),
       start_time = start, date = if (is.na(cdate)) Sys.Date() else cdate,
       entry_fee = as.numeric(cd$entryFee %||% NA),
       field_size = as.integer(cd$maximumEntries %||% NA),
       entries = as.integer(cd$entries %||% NA),
       max_entries_per_user = as.integer(cd$maximumEntriesPerUser %||% NA),
       total_payouts = as.numeric(cd$totalPayouts %||% NA),
       draft_group_id = as.integer(cd$draftGroupId %||% NA),
       game_type_id = cd$gameTypeId %||% NA,
       is_showdown = is_show, prizes = prizes)
}

# Known DK Showdown/Captain gameTypeIds (extend as seen; detection also uses the
# draftables CPT/FLEX structure which is authoritative).
DK_SHOWDOWN_GAMETYPES <- c(96, 159, 175, 211, 286)

# Build a real payout curve object for a contest (fallback to representative GPP).
dk_contest_curve <- function(contest) {
  if (!is.null(contest$prizes) && nrow(contest$prizes) &&
      is.finite(contest$field_size) && is.finite(contest$entry_fee) && contest$entry_fee > 0)
    return(make_table_payout(contest$prizes, contest$field_size, contest$entry_fee))
  make_gpp()
}

# Pull the player pool for a contest's draft group. Detects Showdown from the
# draftables (two roster slots, ~1.5x salary ratio) and returns the BASE (FLEX)
# pool plus is_showdown + cpt_mult. Salaries persisted; CSV snapshot written.
dk_contest_pool <- function(contest, sport = NULL, date = Sys.Date(), slate = "main") {
  if (is.null(sport)) sport <- contest$sport
  dg <- contest$draft_group_id
  url <- sprintf("https://api.draftkings.com/draftgroups/v1/draftgroups/%s/draftables", dg)
  j <- .dk_get_json(url); if (is.null(j) || is.null(j$draftables)) stop("No draftables for group ", dg)
  d <- as.data.table(j$draftables)
  if (!nrow(d)) stop("Contest draft group ", dg, " has no players yet (slate not populated).")

  slots <- d[, .(msal = median(salary, na.rm = TRUE), n = .N), by = rosterSlotId][order(msal)]
  showdown <- contest$is_showdown || nrow(slots) == 2
  cpt_mult <- 1.5
  if (showdown && nrow(slots) >= 2) {
    flex_slot <- slots$rosterSlotId[1]; cpt_slot <- slots$rosterSlotId[nrow(slots)]
    cpt_mult <- round(slots$msal[nrow(slots)] / max(slots$msal[1], 1), 2)
    d <- d[rosterSlotId == flex_slot]                  # base pool = FLEX rows
  } else {
    d <- unique(d, by = if ("playerDkId" %in% names(d)) "playerDkId" else "playerId")
  }

  comp <- d$competition; gname <- if (!is.null(comp) && "name" %in% names(comp)) comp$name else NA
  pool <- data.table(
    player_name = as.character(d$displayName),
    dk_id = as.character(if ("playerDkId" %in% names(d)) d$playerDkId else d$playerId),
    position = as.character(d$position), salary = as.integer(d$salary),
    team = as.character(if ("teamAbbreviation" %in% names(d)) d$teamAbbreviation else NA),
    game_id = gsub("\\s+", "", as.character(gname)),
    dk_avg = NA_real_, status = as.character(if ("status" %in% names(d)) d$status else NA))
  pool <- pool[!is.na(salary) & salary > 0]
  # drop DK-ruled-OUT players so an injured star (e.g. A'ja Wilson) is never rostered
  pool <- pool[is.na(status) | !toupper(trimws(status)) %in% c("OUT","O","IL","INJ","SUSP","DNP","NA")]
  pool[, norm := norm_name(player_name)][, player_id := surrogate_player_id(norm)][, sport := sport]
  attr(pool, "showdown") <- showdown
  attr(pool, "cpt_mult") <- cpt_mult
  attr(pool, "game") <- gsub("\\s+", "", as.character(gname[1]))
  pool[]
}
