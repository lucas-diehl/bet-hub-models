# ==============================================================================
# DFS ENGINE — register Windows Scheduled Tasks so everything runs automatically.
# Run ONCE (normal PowerShell, your user account):
#     powershell -ExecutionPolicy Bypass -File scripts\schedule_windows.ps1 -Bankroll 300
# Re-run to update. Remove with: Unregister-ScheduledTask -TaskName "DFS-Engine-*"
#
# Creates:
#   DFS-Engine-Daily  -> jobs/run_all.R   at 4:00 PM and 7:00 PM daily
#                        (auto-scrapes slates, builds dashboard, logs ownership inbox)
#   DFS-Engine-Train  -> jobs/train_models.R --sport wnba   Mondays 6:00 AM
# ==============================================================================
param([double]$Bankroll = 200)

$ErrorActionPreference = "Stop"
$root = Split-Path -Parent $PSScriptRoot
$rscript = (Get-ChildItem "C:\Program Files\R\R-*\bin\Rscript.exe" -ErrorAction SilentlyContinue |
            Sort-Object FullName -Descending | Select-Object -First 1).FullName
if (-not $rscript) { throw "Rscript.exe not found under C:\Program Files\R\. Install R or edit this script." }
Write-Host "Rscript:" $rscript
Write-Host "Project:" $root

function Register-DfsTask($name, $argline, $triggers) {
  $action  = New-ScheduledTaskAction -Execute $rscript -Argument $argline -WorkingDirectory $root
  $settings= New-ScheduledTaskSettingsSet -StartWhenAvailable -DontStopOnIdleEnd -ExecutionTimeLimit (New-TimeSpan -Hours 1)
  try { Unregister-ScheduledTask -TaskName $name -Confirm:$false -ErrorAction SilentlyContinue } catch {}
  Register-ScheduledTask -TaskName $name -Action $action -Trigger $triggers -Settings $settings `
    -Description "DFS Engine automation" | Out-Null
  Write-Host "registered:" $name
}

# Daily run: two times to catch slates as DK posts them (afternoon + evening).
$daily = @(
  (New-ScheduledTaskTrigger -Daily -At 4:00PM),
  (New-ScheduledTaskTrigger -Daily -At 7:00PM)
)
Register-DfsTask "DFS-Engine-Daily" "jobs/run_all.R --bankroll $Bankroll" $daily

# Weekly model retrain (keeps projections sharp; box also auto-refreshes daily).
$weekly = New-ScheduledTaskTrigger -Weekly -DaysOfWeek Monday -At 6:00AM
Register-DfsTask "DFS-Engine-Train" "jobs/train_models.R --sport wnba" $weekly

Write-Host "`nDone. View with: Get-ScheduledTask -TaskName 'DFS-Engine-*'"
Write-Host "Run now to test: Start-ScheduledTask -TaskName 'DFS-Engine-Daily'"
