#!/usr/bin/env Rscript
# ==============================================================================
# DFS ENGINE — exploratory: real field lineup stack-composition from actual DK CFB
# contest exports (data/ownership_inbox/processed/*.csv, "Lineup" column). Per
# https://x.com/AlexBlickle1/status/2096644199198097515 idea #2: ownership % alone
# doesn't reproduce a realistic field — you need the joint structure of how people
# actually build lineups (single/double/mini-stack rates, which pairs get stacked).
#
# Method: parse real entered lineups, compute QB-skill-player CO-OCCURRENCE LIFT =
# P(both in an entry) / (P(QB) * P(skill)). Needs NO team-roster mapping — pairs that
# co-occur far above chance are, with high confidence, real teammates being
# deliberately stacked. This is a genuinely new signal (never extracted from these
# files before), not a re-derivation of our own sim's assumptions.
#
# STATUS: exploratory / proof-of-method, NOT wired into field_sim.R. Only 2 CFB
# slates / 4,178 entries so far — enough to prove the method works and find real
# signal, not enough to safely calibrate production field construction. The
# stack-size % breakdown below is also sensitive to the lift>=3 "presumed teammate"
# threshold — treat it as a first-pass estimate, not a hard calibration target.
# Extend to more slates (and ideally NFL/WNBA once real lineup exports exist for them)
# before using this to change field_sim.R.
#
# Run: Rscript tests/explore_field_stack_lift.R
# ==============================================================================
suppressPackageStartupMessages(library(data.table))

inbox <- "C:/Users/ljdie/OneDrive/Documents/DFS ENGINE/data/ownership_inbox/processed"
files <- file.path(inbox, c("contest-standings-194855609.csv", "contest-standings-194855619.csv"))
files <- files[file.exists(files)]
if (!length(files)) { cat("No matching contest export files found in", inbox, "\n"); quit(status = 0) }

parse_lineup <- function(s) {
  toks <- strsplit(trimws(s), "\\s+")[[1]]
  slotnames <- c("QB", "RB", "WR", "FLEX", "S-FLEX")
  out <- list(); cur_slot <- NA; cur_name <- c()
  flush <- function() if (!is.na(cur_slot) && length(cur_name))
    out[[length(out) + 1]] <<- list(pos = cur_slot, name = paste(cur_name, collapse = " "))
  for (t in toks) { if (t %in% slotnames) { flush(); cur_slot <- t; cur_name <- c() } else cur_name <- c(cur_name, t) }
  flush(); rbindlist(out)
}

all_entries <- list(); eid <- 0L
for (f in files) {
  raw <- fread(f, header = FALSE, skip = 1, fill = TRUE, sep = ",", colClasses = "character")
  lineup_col <- raw[[6]]
  lineup_col <- lineup_col[!is.na(lineup_col) & nzchar(lineup_col)]
  for (i in seq_along(lineup_col)) {
    L <- tryCatch(parse_lineup(lineup_col[i]), error = function(e) NULL)
    if (!is.null(L) && nrow(L)) L <- L[name != "LOCKED"]     # drop only the not-yet-locked slot
    if (is.null(L) || !nrow(L) || !"QB" %in% L$pos) next
    eid <- eid + 1L
    all_entries[[eid]] <- L[, entry := eid]
  }
}
E <- rbindlist(all_entries)
cat(sprintf("parsed %d real entries, %d unique players (from %d file(s))\n",
            uniqueN(E$entry), uniqueN(E$name), length(files)))

qb <- E[pos == "QB", .(entry, qb = name)]
skill <- E[pos %in% c("RB", "WR", "FLEX", "S-FLEX"), .(entry, skill = name)]
skill <- skill[!skill %in% qb$qb]
M <- merge(qb, skill, by = "entry", allow.cartesian = TRUE)

n_entries <- uniqueN(E$entry)
p_qb    <- qb[,    .N, by = qb][,    .(qb, p = N / n_entries)]
p_skill <- skill[, .N, by = skill][, .(skill, p = N / n_entries)]
co <- M[, .N, by = .(qb, skill)][, p_co := N / n_entries]
co <- merge(co, p_qb, by = "qb", suffixes = c("", "_qb"))
co <- merge(co, p_skill, by = "skill", suffixes = c("", "_skill"))
co[, lift := p_co / (p * p_skill)]
co <- co[N >= 15]

cat("\n=== TOP co-occurrence LIFT pairs (QB + skill player) — real, deliberate stacks ===\n")
print(co[order(-lift)][1:min(15, .N), .(qb, skill, N, lift = round(lift, 1))])

qb_list <- unique(co$qb)
stack_rows <- rbindlist(lapply(qb_list, function(q) {
  sub <- co[qb == q][order(-lift)]
  if (!nrow(sub) || sub$lift[1] < 3) return(NULL)
  teammates <- sub[lift >= 3]$skill
  ent <- M[qb == q, unique(entry)]
  n_stacked <- sapply(ent, function(e) sum(M[entry == e & qb == q]$skill %in% teammates))
  data.table(qb = q, entries = length(ent), n0 = sum(n_stacked == 0), n1 = sum(n_stacked == 1),
             n2 = sum(n_stacked == 2), n3plus = sum(n_stacked >= 3))
}))
if (nrow(stack_rows)) {
  tot <- stack_rows[, lapply(.SD, sum), .SDcols = c("entries", "n0", "n1", "n2", "n3plus")]
  cat("\n=== first-pass stack-size % (lift>=3 teammate proxy — see caveats above) ===\n")
  cat(sprintf("  no-stack: %.1f%% | single: %.1f%% | double: %.1f%% | mini(3+): %.1f%%\n",
              100 * tot$n0 / tot$entries, 100 * tot$n1 / tot$entries,
              100 * tot$n2 / tot$entries, 100 * tot$n3plus / tot$entries))
}
