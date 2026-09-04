# publish_models.ps1 — sync locally-retrained models into bet-hub-models and push,
# so the GitHub Actions (which run the COMMITTED models) use the fresh weights.
#
# Training runs locally (DFS-Engine-Train, heavy) and writes OneDrive\...\data\models.
# The daily DFS Action uses the models committed here, so without this step a retrain
# never reaches the live site. Run this after training (a scheduled task does it daily;
# it is a cheap no-op when nothing changed). Requires git credential.helper=store set up
# once (see README) so the push is non-interactive.
$ErrorActionPreference = "Stop"
$git  = "C:\tools\mingit\cmd\git.exe"
$docs = "C:\Users\ljdie\OneDrive\Documents"
$repo = "C:\dev\bet-hub-models"

# Copy only newer files (/XO skips models the repo already has at >= that time).
$rc = @("/E", "/XO", "/NFL", "/NDL", "/NJH", "/NJS", "/NP", "/R:1", "/W:1")
robocopy "$docs\DFS ENGINE\data\models" "$repo\dfs-engine\data\models" @rc | Out-Null
# Golf bundles retrain rarely; uncomment to sync them too:
# robocopy "$docs\golf-modeling\golf_picks" "$repo\golf-modeling\golf_picks" "*.rds" @rc | Out-Null

& $git -C $repo add dfs-engine/data/models
$changed = & $git -C $repo status --porcelain -- dfs-engine/data/models
if ($changed) {
  & $git -C $repo -c user.name="Lucas Diehl" -c user.email="ljdiehlo22@gmail.com" `
    commit -m "dfs: publish retrained models $(Get-Date -Format yyyy-MM-dd)" | Out-Null
  & $git -C $repo push origin main
  Write-Output "[$(Get-Date -Format 'u')] published updated DFS models"
} else {
  Write-Output "[$(Get-Date -Format 'u')] no model changes to publish"
}
