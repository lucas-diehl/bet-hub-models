param(
    [switch]$Backfill,
    [switch]$CaptureCurrent,
    [switch]$IngestFiles
)

$ErrorActionPreference = "Stop"
$projectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
. (Join-Path $projectRoot "scripts\resolve_runtimes.ps1")

if (-not ($Backfill -or $CaptureCurrent -or $IngestFiles)) {
    $Backfill = $true
}

if ($Backfill) {
    Invoke-Native $rscript @("scripts/19_backfill_dfs_salaries.R")
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}
if ($CaptureCurrent) {
    Invoke-Native $rscript @("scripts/21_capture_current_dk_salaries.R")
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}
if ($IngestFiles) {
    Invoke-Native $rscript @("scripts/20_ingest_dfs_salary_files.R")
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}

exit 0

