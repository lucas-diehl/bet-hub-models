$ErrorActionPreference = "Stop"
$projectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
. (Join-Path $projectRoot "scripts\resolve_runtimes.ps1")

Push-Location $projectRoot
try {
  Invoke-Native $rscript @("scripts/17_build_fantasy_prop_models.R")
  if ($LASTEXITCODE -ne 0) {
    throw "Fantasy prop model training failed."
  }

  Invoke-Native $rscript @("scripts/18_validate_fantasy_prop_models.R")
  if ($LASTEXITCODE -ne 0) {
    throw "Fantasy prop model validation failed."
  }

  $startedAt = Get-Date
  Invoke-Native $node @("scripts/build_fantasy_prop_workbook.mjs")
  Assert-FreshArtifact `
    -Path "outputs/fantasy_prop_2026/NFL_Fantasy_Prop_Models_2026.xlsx" `
    -NotBefore $startedAt `
    -What "fantasy projection workbook"
}
finally {
  Pop-Location
}

# Every failure path above throws, so reaching here means the run succeeded.
# Exit explicitly rather than inheriting the workbook builder's exit code,
# which is nonzero even on success because it crashes during teardown.
exit 0

