param(
  [switch]$RefreshStats
)

$ErrorActionPreference = "Stop"
$projectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
. (Join-Path $projectRoot "scripts\resolve_runtimes.ps1")

Push-Location $projectRoot
try {
  $modelArguments = @("scripts/15_build_2026_td_board.R", "--execute")
  if ($RefreshStats) {
    $modelArguments += "--refresh-stats"
  }
  Invoke-Native $rscript $modelArguments
  if ($LASTEXITCODE -ne 0) {
    throw "The 2026 touchdown-board refresh failed."
  }

  $startedAt = Get-Date
  Invoke-Native $node @("scripts/build_td_2026_workbook.mjs")
  Assert-FreshArtifact `
    -Path "outputs/td_2026/NFL_Touchdown_Betting_Setup_2026.xlsx" `
    -NotBefore $startedAt `
    -What "2026 workbook"
}
finally {
  Pop-Location
}

# Every failure path above throws, so reaching here means the run succeeded.
# Exit explicitly rather than inheriting the workbook builder's exit code,
# which is nonzero even on success because it crashes during teardown.
exit 0

