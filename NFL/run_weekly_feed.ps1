param(
  [ValidateSet("publish", "capture-close", "grade")]
  [string]$Job = "publish",
  [int]$Week = 0
)

# Scheduler entry point for the Bet Hub feed.
#
#   publish        Tuesday   - score the upcoming week and append new picks
#   capture-close  Sunday am - snapshot lines for closing-line value
#   grade          Tuesday   - grade finished games and write results
#
# Each job is safe to run repeatedly: publishing is append-only against the
# ledger, closing capture keeps the first snapshot per bet, and grading only
# adds games that have a final score.

$ErrorActionPreference = "Stop"
$projectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
. (Join-Path $projectRoot "scripts\resolve_runtimes.ps1")

Push-Location $projectRoot
try {
  $stamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
  Write-Output "[$stamp] job=$Job"

  switch ($Job) {
    "publish" {
      # The touchdown board is refreshed first so the publisher reads a current
      # bet card. It applies its own 10-day window, so outside that it simply
      # writes an empty card and the publisher emits game-level bets only.
      Invoke-Native $rscript @("scripts/15_build_2026_td_board.R", "--execute")
      if ($LASTEXITCODE -ne 0) {
        Write-Output "Touchdown board refresh failed; publishing game-level bets only."
      }

      $jobArgs = @("scripts/57_publish_weekly_picks.R", "--execute")
      if ($Week -gt 0) { $jobArgs += "--week=$Week" }
      Invoke-Native $rscript $jobArgs
      if ($LASTEXITCODE -ne 0) { throw "Weekly publish failed." }
    }
    "capture-close" {
      Invoke-Native $rscript @(
        "scripts/58_grade_published_picks.R", "--capture-close", "--execute"
      )
      if ($LASTEXITCODE -ne 0) { throw "Closing-line capture failed." }
    }
    "grade" {
      Invoke-Native $rscript @(
        "scripts/58_grade_published_picks.R", "--grade", "--execute"
      )
      if ($LASTEXITCODE -ne 0) { throw "Grading failed." }
    }
  }

  Write-Output "[$stamp] $Job complete."
}
finally {
  Pop-Location
}

exit 0
