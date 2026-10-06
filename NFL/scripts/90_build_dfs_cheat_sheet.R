## ---------------------------------------------------------------------------
## 90_build_dfs_cheat_sheet.R
##
## Weekly NFL DFS cheat sheet -> an Excel workbook in the NFL project root:
##   - Cheat Sheet: every projected player, grouped by position, with the
##     player's own recent usage, the opponent defense's recent form by
##     position (rush/pass EPA allowed + a plain-English matchup read), and
##     our low/avg/high projection.
##   - My Rosters: just the players owned across your 3 Sleeper leagues.
##   - Top Value: best points-per-$1000 salary plays.
##
## PREREQUISITES (run before this, same day, so the data is current):
##   Rscript scripts/17_build_fantasy_prop_models.R --week=N --execute
##   Rscript scripts/59_emit_dfs_projection_json.R --salary_file=outputs/dfs_salaries_dk_current.csv
##   Rscript scripts/79_build_matchup_features.R      (defense EPA -- run weekly, pulls fresh PBP)
##   Rscript scripts/73_td_redzone_features.R         (redzone defense -- run weekly)
##
## Sleeper: no API key needed (public read-only API). Update SLEEPER_USERNAME
## and LEAGUE_IDS below if your leagues change.
##
## Run:  Rscript scripts/90_build_dfs_cheat_sheet.R
## ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table); library(jsonlite); library(openxlsx); library(httr2)
})
if (Sys.getenv("ENGINE_WD_SET") == "" && dir.exists("c:/Users/ljdie/OneDrive/Documents/NFL"))
  setwd("c:/Users/ljdie/OneDrive/Documents/NFL")
msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S]"), ..., "\n")
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

SLEEPER_USERNAME <- "lukejdiehl"
LEAGUE_IDS <- c("Pals Family" = "1389110936425488384",
                "Bachelors 2026" = "1388295731642638336",
                "Beta Dynasty" = "1385354546183700480")

## ---- Sleeper: resolve leagues -> owned player names ------------------------
sleeper_get <- function(path) {
  httr2::request(paste0("https://api.sleeper.app/v1/", path)) |>
    httr2::req_timeout(30) |> httr2::req_perform() |> httr2::resp_body_json(simplifyVector = TRUE)
}
msg("Pulling Sleeper rosters for", SLEEPER_USERNAME, "...")
me <- sleeper_get(paste0("user/", SLEEPER_USERNAME))
sp_players <- sleeper_get("players/nfl")   # ~13k players, ~15MB -- one call, cache-worthy but fine weekly
sp_dt <- rbindlist(lapply(names(sp_players), function(id) {
  p <- sp_players[[id]]
  data.table(sleeper_id = id, full_name = p$full_name %||% paste(p$first_name, p$last_name))
}), fill = TRUE)

owned <- rbindlist(lapply(names(LEAGUE_IDS), function(lname) {
  rosters <- sleeper_get(paste0("league/", LEAGUE_IDS[[lname]], "/rosters"))
  mine <- rosters[rosters$owner_id == me$user_id, ]
  ids <- unlist(mine$players)
  if (!length(ids)) return(NULL)
  m <- sp_dt[sleeper_id %in% ids]
  m[, league := lname]
  m
}), fill = TRUE)
owned[, norm := tolower(gsub("[^a-z]", "", full_name))]
own_wide <- dcast(owned, norm ~ league, value.var = "league", fun.aggregate = function(x) "OWNED", fill = "")
for (lg in names(LEAGUE_IDS)) if (!lg %in% names(own_wide)) own_wide[, (lg) := ""]
msg("  ", uniqueN(owned$norm), "unique owned players across", length(LEAGUE_IDS), "leagues.")

## ---- 1. Projections (this week) --------------------------------------------
j <- fromJSON("outputs/dfs_projections_latest.json")
pl <- as.data.table(j$players)
pl[, norm := tolower(gsub("[^a-z]", "", name))]
msg("Projections: week", j$week, "| slate", j$slate_date, "|", nrow(pl), "players.")

## ---- 2. Salaries -------------------------------------------------------------
sal <- as.data.table(read.csv("outputs/dfs_salaries_dk_current.csv"))
sal <- sal[week == j$week]
sal[, norm := tolower(gsub("[^a-z]", "", player_name))]
sal_u <- unique(sal[, .(norm, salary)], by = "norm")

## ---- 3. Player's own recent-season stats ------------------------------------
stats <- as.data.table(readRDS("data/raw/player_stats_2021_2026.rds"))
s26 <- stats[season == 2026 & week < j$week]
setorder(s26, player_id, week)
recent <- s26[, .(
  games_played = .N,
  avg_targets = round(mean(targets, na.rm=TRUE), 1), avg_rec = round(mean(receptions, na.rm=TRUE), 1),
  avg_rec_yds = round(mean(receiving_yards, na.rm=TRUE), 1), avg_carries = round(mean(carries, na.rm=TRUE), 1),
  avg_rush_yds = round(mean(rushing_yards, na.rm=TRUE), 1), avg_ppr = round(mean(fantasy_points_ppr, na.rm=TRUE), 1),
  last_game_ppr = round(fantasy_points_ppr[.N], 1),
  trend = if (.N >= 2) round(fantasy_points_ppr[.N] - mean(fantasy_points_ppr[1:(.N-1)]), 1) else NA_real_
), by = .(player_id, player_display_name, position, team)]
recent[, norm := tolower(gsub("[^a-z]", "", player_display_name))]

## ---- 4. Opponent defense, recent form by position ---------------------------
mf <- readRDS("data/processed/nfl_matchup_features.rds")
dp <- as.data.table(mf$def_pos_game)
setorder(dp, defteam, position, season, week)
def_recent <- dp[season == 2026, .SD[max(1, .N-2):.N], by = .(defteam, position)][
  , .(def_rush_epa = round(mean(def_rush_epa_allowed, na.rm=TRUE), 3),
      def_pass_epa = round(mean(def_pass_epa_allowed, na.rm=TRUE), 3)), by = .(defteam, position)]
def_recent[, rush_epa_pctile := round(frank(def_rush_epa, ties.method="average", na.last="keep") / sum(!is.na(def_rush_epa)), 2), by = position]
def_recent[, pass_epa_pctile := round(frank(def_pass_epa, ties.method="average", na.last="keep") / sum(!is.na(def_pass_epa)), 2), by = position]

## ---- 5. Redzone defense ------------------------------------------------------
rz <- readRDS("data/processed/td_redzone_features.rds")
dg <- as.data.table(rz$def_game)
rz_def <- dg[season == 2026, .(rz_epa_allowed = round(mean(def_xtd_allowed, na.rm=TRUE), 3)), by = defteam]

## ---- ASSEMBLE ----------------------------------------------------------------
d <- merge(pl, sal_u, by = "norm", all.x = TRUE)
d <- merge(d, recent[, .(norm, games_played, avg_targets, avg_rec, avg_rec_yds, avg_carries, avg_rush_yds, avg_ppr, last_game_ppr, trend)], by = "norm", all.x = TRUE)
d <- merge(d, def_recent, by.x = c("opponent", "pos"), by.y = c("defteam", "position"), all.x = TRUE)
d <- merge(d, rz_def, by.x = "opponent", by.y = "defteam", all.x = TRUE)
d <- merge(d, own_wide, by = "norm", all.x = TRUE)

d[, matchup_note := {
  is_rb <- pos == "RB"; is_pc <- pos %in% c("WR","TE")
  rush_edge <- fifelse(is.na(rush_epa_pctile), "", fifelse(rush_epa_pctile >= 0.7, "Great run matchup", fifelse(rush_epa_pctile <= 0.3, "Tough run D", "Mid run D")))
  pass_edge <- fifelse(is.na(pass_epa_pctile), "", fifelse(pass_epa_pctile >= 0.7, "Great pass matchup", fifelse(pass_epa_pctile <= 0.3, "Tough pass D", "Mid pass D")))
  fifelse(is_rb, paste0(rush_edge, fifelse(nzchar(rush_edge) & nzchar(pass_edge), " / ", ""), fifelse(avg_targets >= 2 & nzchar(pass_edge), pass_edge, "")),
    fifelse(is_pc, pass_edge, ""))
}]
for (col in names(LEAGUE_IDS)) { if (!col %in% names(d)) d[, (col) := ""]; d[is.na(get(col)), (col) := ""] }
d[, my_team := fifelse(Reduce(`|`, lapply(names(LEAGUE_IDS), function(c) d[[c]] == "OWNED")), "MY GUY", "")]

out <- d[!is.na(proj_ppr) & proj_ppr > 0, .(
  Player = name, Pos = pos, Team = team, Opp = opponent, MyGuy = my_team,
  Salary = salary, Floor = round(ppr_low,1), Proj = round(proj_ppr,1), Ceiling = round(ppr_high,1),
  ProjOwn = round(ownership.heuristic_score, 1),
  GP = games_played, AvgTgt = avg_targets, AvgRec = avg_rec, AvgRecYd = avg_rec_yds,
  AvgCar = avg_carries, AvgRshYd = avg_rush_yds, AvgPPR = avg_ppr, LastGm = last_game_ppr, Trend = trend,
  OppRushEPA = def_rush_epa, OppPassEPA = def_pass_epa, RZDefEPA = rz_epa_allowed,
  Matchup = matchup_note,
  PalsFamily = `Pals Family`, Bachelors2026 = `Bachelors 2026`, BetaDynasty = `Beta Dynasty`,
  Injury = injury_status
)]
out[, Value := round(Proj / pmax(Salary, 1) * 1000, 2)]
msg("Assembled", nrow(out), "players,", sum(out$MyGuy=="MY GUY"), "owned.")

## ---- WORKBOOK ------------------------------------------------------------
wb <- createWorkbook()
hdr_style <- createStyle(fgFill = "#1F3864", fontColour = "white", textDecoration = "bold", halign = "center", border = "TopBottom")
myguy_style <- createStyle(fgFill = "#FFF2CC")
title_style <- createStyle(fontSize = 14, textDecoration = "bold")
write_sheet <- function(wb, name, df, freeze_col = 6) {
  addWorksheet(wb, name)
  writeData(wb, name, df, headerStyle = hdr_style)
  freezePane(wb, name, firstActiveRow = 2, firstActiveCol = freeze_col)
  if ("MyGuy" %in% names(df)) {
    rows <- which(df$MyGuy == "MY GUY") + 1
    if (length(rows)) addStyle(wb, name, myguy_style, rows = rows, cols = seq_along(df), gridExpand = TRUE, stack = TRUE)
  }
  if ("Proj" %in% names(df) && nrow(df) > 0) {
    pc <- which(names(df) == "Proj")
    conditionalFormatting(wb, name, cols = pc, rows = 2:(nrow(df)+1), style = c("#F8696B","#FFEB84","#63BE7B"), type = "colourScale")
  }
  setColWidths(wb, name, cols = seq_along(df), widths = "auto")
}
by_pos <- copy(out); setorder(by_pos, Pos, -Proj)
write_sheet(wb, "Cheat Sheet", by_pos)
my_teams <- out[MyGuy == "MY GUY"]; setorder(my_teams, Pos, -Proj); my_teams[, MyGuy := NULL]
write_sheet(wb, "My Rosters", my_teams, freeze_col = 5)
topval <- out[Salary >= 3000][order(-Value)][seq_len(min(40, .N))]
write_sheet(wb, "Top Value", topval)

addWorksheet(wb, "Read Me", tabColour = "#1F3864")
writeData(wb, "Read Me", data.frame(x = c(
  "NFL DFS Weekly Cheat Sheet", "",
  paste0("Week ", j$week, " (slate ", j$slate_date, ") -- generated ", format(Sys.time(), "%Y-%m-%d %H:%M")), "",
  "Tabs:",
  "  Cheat Sheet - full player pool, grouped by position, sorted by projection.",
  "  My Rosters  - your Sleeper-owned guys across Pals Family / Bachelors 2026 / Beta Dynasty.",
  "  Top Value   - best projected points per $1000 salary (min $3000 salary).", "",
  "Columns:",
  "  Floor/Proj/Ceiling   - our low/average/high point projection this week.",
  "  ProjOwn              - our projected DK ownership % (higher = more chalk).",
  "  AvgTgt/AvgRec/etc    - the player's own real per-game averages this season so far.",
  "  LastGm / Trend       - last game's actual points, and vs their own average.",
  "  OppRushEPA/OppPassEPA- opponent D's recent EPA allowed vs this position (higher = worse D = better matchup).",
  "  RZDefEPA             - opponent's red-zone TD efficiency allowed (higher = leakier).",
  "  Matchup              - plain-English read of the EPA matchup.",
  "  Yellow rows          - you own this player in at least one Sleeper league.", "",
  "Rebuild any time:  Rscript scripts/90_build_dfs_cheat_sheet.R",
  "(run 17/59 for fresh projections, and 79/73 for fresh defense form, first if it's been a while)"
)), colNames = FALSE)
setColWidths(wb, "Read Me", cols = 1, widths = 110)
addStyle(wb, "Read Me", title_style, rows = 1, cols = 1)
worksheetOrder(wb) <- c(which(names(wb)=="Read Me"), which(names(wb)=="Cheat Sheet"), which(names(wb)=="My Rosters"), which(names(wb)=="Top Value"))

outfile <- sprintf("NFL_DFS_Cheat_Sheet_%s.xlsx", Sys.Date())
saveWorkbook(wb, outfile, overwrite = TRUE)
msg("Saved:", normalizePath(outfile))
