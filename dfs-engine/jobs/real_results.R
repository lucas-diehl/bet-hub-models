# ==============================================================================
# DFS ENGINE — jobs/real_results.R  (score OUR entries against the REAL field)
#
# The only trustworthy GPP scorecard (see memory: sim-based EV is not): after a contest
# settles, read its DK standings export (every entry's lineup + points, every player's
# %Drafted and actual DK points) and report, for the user's entries:
#   field cut lines (median / top 20% / 10% / 1% / 0.1% / winner), our ranks + percentiles,
#   cashes and REAL winnings from DK's payout table, ROI, how many of our lineups were
#   duplicated by other entrants, and (optional) our projected ownership vs actual.
#
# Our entries are found by matching lineups from --lineups (the READABLE csv the build
# writes); the DK username that owns them is then inferred, so entries the user edited
# or late-swapped are still counted.
#
# Usage (standings must be in the ownership inbox -- jobs/sync_ownership.R puts them there):
#   Rscript jobs/real_results.R --contest 197662961 --lineups path/to/READABLE.csv [--fee 0.5]
#                               [--own-csv path/with/player,own] [--inbox dir]
# ==============================================================================
source(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])), "..", "bootstrap.R"))
dfs_load_spine()
suppressPackageStartupMessages(library(data.table))

args <- commandArgs(TRUE)
arg <- function(k, d = NULL) { i <- match(k, args); if (is.na(i) || i == length(args)) d else args[i + 1] }
cid   <- arg("--contest"); lu_f <- arg("--lineups"); own_f <- arg("--own-csv")
inbox <- arg("--inbox", dfs_path("data", "ownership_inbox"))
if (is.null(cid) || is.null(lu_f)) stop("usage: --contest <id> --lineups <READABLE.csv> [--fee x] [--own-csv f] [--inbox dir]")

nn <- function(x) { x <- tolower(iconv(x, to = "ASCII//TRANSLIT")); x <- gsub("[^a-z ]", "", x)
  x <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", x); gsub("\\s+", " ", trimws(x)) }
f <- list.files(inbox, pattern = sprintf("contest-standings-%s\\.csv$", cid), recursive = TRUE, full.names = TRUE)[1]
if (is.na(f)) { f <- tryCatch(dk_download_standings(cid), error = function(e) NULL); if (is.null(f)) stop("no standings for contest ", cid) }
# DK's file = entries table (cols 1-6) beside a player table (cols 8-11); sep must be forced
E <- fread(f, select = 1:6, header = TRUE, sep = ",", fill = TRUE, colClasses = "character", showProgress = FALSE)
setnames(E, c("rank", "entry_id", "entry_name", "time", "points", "lineup")); E <- E[nzchar(entry_id)]
E[, `:=`(rank = as.integer(rank), points = as.numeric(points))]
P <- fread(f, select = 8:11, header = TRUE, sep = ",", fill = TRUE, colClasses = "character", showProgress = FALSE)
setnames(P, c("player", "rpos", "drafted", "fpts")); P <- P[nzchar(player)][, `:=`(own = as.numeric(sub("%", "", drafted)) / 100, fpts = as.numeric(fpts))]
showdown <- any(P$rpos == "CPT")
tags <- if (showdown) c("CPT", "FLEX") else c("QB", "RB", "WR", "TE", "FLEX", "DST")
# "QB Josh Allen RB ... DST Jets" -> sorted name key (vectorised: split on slot tokens)
pat <- sprintf("(^| )(%s) ", paste(tags, collapse = "|"))
parts <- lapply(strsplit(gsub(pat, "|", paste0(" ", E$lineup, " ")), "|", fixed = TRUE), function(p) { p <- trimws(p); p[nzchar(p)] })
u <- unique(unlist(parts)); nmap <- setNames(nn(u), u)                     # normalise each distinct name once
E[, key := vapply(parts, function(p) paste(sort(unname(nmap[p])), collapse = ";"), "")]
n <- nrow(E)
L <- fread(lu_f); cols <- setdiff(names(L), c("lineup", "salary", "proj", "est_dupes"))
L[, key := apply(.SD, 1, function(v) paste(sort(nn(v)), collapse = ";")), .SDcols = cols]
M <- merge(L[, .(key)], E[, .(key, entry_name)], by = "key")
user <- if (nrow(M)) names(sort(table(sub(" \\(.*$", "", M$entry_name)), decreasing = TRUE))[1] else NA_character_
ours <- if (!is.na(user)) E[sub(" \\(.*$", "", entry_name) == user] else E[0]
pz <- tryCatch(as.data.table(dk_contest(cid)$prizes), error = function(e) NULL)
prize_of <- function(r) { if (is.null(pz) || !nrow(pz)) return(NA_real_); k <- which(pz$min_rank <= r & pz$max_rank >= r); if (length(k)) as.numeric(pz$prize[k[1]]) else 0 }
fee <- as.numeric(arg("--fee", NA)); if (!is.finite(fee)) fee <- tryCatch(as.numeric(dk_contest(cid)$entry_fee), error = function(e) NA_real_)
ours[, `:=`(pct = 1 - rank / n, prize = vapply(rank, prize_of, 0))]
dups <- E[, .N, by = key]; ours[dups, on = "key", copies := i.N]
q <- quantile(E$points, c(0.5, 0.8, 0.9, 0.99, 0.999), na.rm = TRUE)
cat(sprintf("\n== contest %s (%s, %s entries) ==\n", cid, if (showdown) "showdown" else "classic", format(n, big.mark = ",")))
cat(sprintf("field: median %.1f | top20%% %.1f | top10%% %.1f | top1%% %.1f | top0.1%% %.1f | winner %.1f\n", q[1], q[2], q[3], q[4], q[5], max(E$points, na.rm = TRUE)))
cat(sprintf("our entries (user '%s'): %d | lineups from the file matched: %d of %d\n", user, nrow(ours), uniqueN(M$key), nrow(L)))
if (nrow(ours)) {
  cat(sprintf("best %.1f (rank %s, top %.2f%%) | median %.1f vs field %.1f | mean percentile %.1f\n", max(ours$points), format(min(ours$rank), big.mark = ","),
              100 * min(ours$rank) / n, median(ours$points), q[1], 100 * mean(ours$pct)))
  cat(sprintf("finishes: top20%% %d | top10%% %d | top1%% %d | top0.1%% %d (expected at random: %.1f / %.1f / %.2f / %.3f)\n",
              sum(ours$pct >= 0.8), sum(ours$pct >= 0.9), sum(ours$pct >= 0.99), sum(ours$pct >= 0.999),
              0.2 * nrow(ours), 0.1 * nrow(ours), 0.01 * nrow(ours), 0.001 * nrow(ours)))
  cat(sprintf("cashed %d | won $%.2f on $%.2f | ROI %+.0f%%\n", sum(ours$prize > 0, na.rm = TRUE), sum(ours$prize, na.rm = TRUE), fee * nrow(ours),
              100 * (sum(ours$prize, na.rm = TRUE) / (fee * nrow(ours)) - 1)))
  cat(sprintf("duplication: %d of our %d entries had at least one exact copy in the field (max copies %d)\n",
              sum(ours$copies > 1, na.rm = TRUE), nrow(ours), max(ours$copies, na.rm = TRUE)))
}
if (!is.null(own_f) && file.exists(own_f)) {
  O <- fread(own_f)[, .(norm = nn(player), ours = as.numeric(own))]
  A <- P[, .(actual = max(own), fpts = max(fpts, na.rm = TRUE)), by = .(norm = nn(player))]
  X <- merge(O, A, by = "norm")
  cat(sprintf("\nownership: corr %.3f | MAE %.2f%% | chalk (actual >= 20%%) bias %+.1f pts over %d players\n",
              cor(X$ours, X$actual), 100 * mean(abs(X$ours - X$actual)), 100 * mean((X$ours - X$actual)[X$actual >= 0.2]), sum(X$actual >= 0.2)))
  print(X[order(-actual)][1:10, .(player = norm, actual = round(100 * actual, 1), ours = round(100 * ours, 1), dk_pts = fpts)])
}
