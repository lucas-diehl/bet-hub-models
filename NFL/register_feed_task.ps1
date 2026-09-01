# Registers the "NFL-Feed-Weekly" scheduled task that runs run_weekly_feed.ps1.
# Run ONCE, from an elevated PowerShell (Run as administrator):
#     powershell -ExecutionPolicy Bypass -File .\register_feed_task.ps1
#
# Cadence: daily at 8:00 AM. The driver's idle gate makes off-season runs a cheap
# no-op, and in-season runs are idempotent (ingest upserts by bet_id), so a daily
# post/grade just keeps lines + results fresh. Adjust the trigger if you prefer
# only Tue+Mon (post the week / grade after Monday Night Football).
$ErrorActionPreference = "Stop"
$projectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$launcher = Join-Path $projectRoot "run_weekly_feed.ps1"

$action  = New-ScheduledTaskAction -Execute "powershell.exe" `
  -Argument "-NoProfile -ExecutionPolicy Bypass -File `"$launcher`"" `
  -WorkingDirectory $projectRoot
$trigger = New-ScheduledTaskTrigger -Daily -At 8:00AM
$settings = New-ScheduledTaskSettingsSet -StartWhenAvailable `
  -DontStopOnIdleEnd -ExecutionTimeLimit (New-TimeSpan -Hours 2) `
  -MultipleInstances IgnoreNew
# S4U = run whether or not the user is logged on, without storing a password.
$principal = New-ScheduledTaskPrincipal -UserId $env:USERNAME -LogonType S4U -RunLevel Limited

Register-ScheduledTask -TaskName "NFL-Feed-Weekly" -Action $action -Trigger $trigger `
  -Settings $settings -Principal $principal -Force |
  Select-Object TaskName, State

Write-Host "Registered NFL-Feed-Weekly (daily 8:00 AM). Test it now with:"
Write-Host "    Start-ScheduledTask -TaskName 'NFL-Feed-Weekly'"
Write-Host "    Get-Content .\weekly_feed.log -Tail 20"
