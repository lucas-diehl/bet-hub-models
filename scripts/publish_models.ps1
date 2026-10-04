# publish_models.ps1 — sync locally-produced model weights AND projection artifacts into
# bet-hub-models and push, so the GitHub Actions (which run the COMMITTED files) use the
# fresh ones.
#
# Training/projection runs locally (DFS-Engine-Train for the DFS models; NFL's
# run_weekly_feed.ps1 -Job publish for the NFL fantasy-prop models + projection JSON).
# The daily DFS Action uses what is COMMITTED here, so without this step a retrain never
# reaches the live site. Run this after training (a scheduled task does it daily; it is a
# cheap no-op when nothing changed). Requires git credential.helper=store set up once
# (see README) so the push is non-interactive.
#
# 2026-10-02: added the NFL/outputs sync. This was the gap that silently served stale
# projections for days. The NGS fix (disabling tracking features that had collapsed every
# WR/TE projection) was committed as CODE and retrained locally, but the DFS engine does
# not run the NFL model -- it reads the committed ARTIFACT
# NFL/outputs/dfs_projections_latest.json. That file was never published, so the live site
# kept serving pre-fix numbers: DK Metcalf 6.79 instead of 11.55, Harold Fannin 6.77
# instead of 11.51, Denzel Boston 6.76 instead of 10.85, best WR in the NFL 9.84 instead
# of 20.38. At ~6.7 those players project BELOW both kickers, so the optimizer correctly
# refused to roster them and they vanished from every lineup. Syncing weights alone was
# never enough -- the projections are what the site actually consumes.
$ErrorActionPreference = "Stop"
$git  = "C:\tools\mingit\cmd\git.exe"
$docs = "C:\Users\ljdie\OneDrive\Documents"
$repo = "C:\dev\bet-hub-models"

# robocopy signals success with exit codes 0-7 (1 = files copied, 2 = extra files, ...);
# only >= 8 is a real failure. Left unchecked a SUCCESSFUL copy leaves $LASTEXITCODE = 1,
# which became this script's exit code and made Task Scheduler report the run as failed
# even when it worked. Wrap it so the caller gets a clean boolean.
function Invoke-Robocopy {
  param([string]$From, [string]$To, [string[]]$Files = @())
  if (-not (Test-Path $From)) { Write-Output "  skip (missing): $From"; return $true }
  New-Item -ItemType Directory -Force -Path $To | Out-Null
  $rcArgs = @($From, $To) + $Files + @("/XO", "/NFL", "/NDL", "/NJH", "/NJS", "/NP", "/R:1", "/W:1")
  & robocopy @rcArgs | Out-Null
  $code = $LASTEXITCODE
  $global:LASTEXITCODE = 0          # don't let robocopy's success codes poison our exit
  if ($code -ge 8) { Write-Output "  ROBOCOPY FAILED ($code): $From -> $To"; return $false }
  return $true
}

$ok = $true
# 1. DFS ENGINE trained model weights (recursive — nested model dirs)
$ok = (Invoke-Robocopy "$docs\DFS ENGINE\data\models" "$repo\dfs-engine\data\models" @("/E")) -and $ok

# 2. NFL projection ARTIFACTS — what sports/nfl/project.R actually reads at build time.
#    dfs_projections_latest.json is the contract file; the week-stamped JSON and the CSVs
#    are published alongside it so the board/props stay consistent with it.
$ok = (Invoke-Robocopy "$docs\NFL\outputs" "$repo\NFL\outputs" @(
  "dfs_projections_latest.json",
  "fantasy_prop_2026_week*_projections.json",
  "fantasy_prop_2026_latest_projections.csv",
  "fantasy_prop_2026_latest_long.csv",
  "td_2026_bet_card.csv"
)) -and $ok

# Golf bundles retrain rarely; uncomment to sync them too:
# $ok = (Invoke-Robocopy "$docs\golf-modeling\golf_picks" "$repo\golf-modeling\golf_picks" @("*.rds", "/E")) -and $ok

# NFL/outputs is in .gitignore (it holds a lot of large intermediates we do NOT want in
# git), so these specific published artifacts need -f. That ignore rule is exactly why
# this script could never publish the projections before: `git add` silently refused the
# path, the status check then saw no change, and the script reported "nothing to publish"
# while the live site kept serving a stale JSON. Force ONLY the contract files below --
# the rest of outputs/ stays ignored on purpose.
$paths = @("dfs-engine/data/models",
           "NFL/outputs/dfs_projections_latest.json",
           "NFL/outputs/fantasy_prop_2026_latest_projections.csv",
           "NFL/outputs/fantasy_prop_2026_latest_long.csv",
           "NFL/outputs/td_2026_bet_card.csv")
# NOTE: do NOT pipe git's stderr (2>&1) here. git emits a benign "LF will be replaced by
# CRLF" warning on these files, and under $ErrorActionPreference='Stop' PowerShell turns a
# redirected native-stderr line into a terminating NativeCommandError -- which made this
# script exit 1 on a run that actually succeeded.
& $git -C $repo add -f -- $paths
# ONLY the week the latest projection file is actually for. Publishing every
# fantasy_prop_2026_week*_projections.json broke the engine: sports/nfl/project.R selects
# from that glob and picked up week2 instead of week4, silently projecting the wrong week.
# Older weeks stay untracked (they are still on disk locally for backtests).
$wk = $null
try { $wk = (Get-Content "$repo\NFL\outputs\dfs_projections_latest.json" -Raw | ConvertFrom-Json).week } catch {}
if ($wk) {
  $wkFile = "fantasy_prop_2026_week${wk}_projections.json"
  if (Test-Path "$repo\NFL\outputs\$wkFile") {
    & $git -C $repo add -f -- "NFL/outputs/$wkFile"
    $paths += "NFL/outputs/$wkFile"
    Write-Output "  current week = $wk -> $wkFile"
  }
  # drop any other week that is still tracked, so the glob can never resolve to a stale one
  (& $git -C $repo ls-files "NFL/outputs/fantasy_prop_2026_week*_projections.json") |
    Where-Object { $_ -and $_ -ne "NFL/outputs/$wkFile" } |
    ForEach-Object { & $git -C $repo rm --cached -- $_ | Out-Null; $paths += $_; Write-Output "  untracked stale $_" }
}
$changed = & $git -C $repo status --porcelain -- $paths
if ($changed) {
  $summary = ($changed | Measure-Object).Count
  & $git -C $repo -c user.name="Lucas Diehl" -c user.email="ljdiehlo22@gmail.com" `
    commit -m "dfs: publish retrained models + NFL projection artifacts $(Get-Date -Format yyyy-MM-dd)" | Out-Null
  & $git -C $repo push origin main
  if ($LASTEXITCODE -ne 0) { Write-Output "[$(Get-Date -Format 'u')] PUSH FAILED"; exit 1 }
  Write-Output "[$(Get-Date -Format 'u')] published $summary changed file(s)"
} else {
  Write-Output "[$(Get-Date -Format 'u')] nothing to publish"
}

if (-not $ok) { exit 1 }
exit 0
