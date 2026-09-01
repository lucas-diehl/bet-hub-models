# Locates the R and Node runtimes used by the pipeline runners.
#
# Hardcoding an absolute interpreter path breaks on every upgrade and on any
# machine with a different user profile. Resolution order for each runtime is
# an explicit environment override, then PATH, then the conventional install
# root with the highest installed version.
#
# Dot-source this file, then use $rscript and $node.

function Resolve-Rscript {
    if ($env:NFL_RSCRIPT) {
        if (Test-Path -LiteralPath $env:NFL_RSCRIPT) { return $env:NFL_RSCRIPT }
        throw "NFL_RSCRIPT is set to '$env:NFL_RSCRIPT' but that file does not exist."
    }

    $onPath = Get-Command Rscript.exe -ErrorAction SilentlyContinue
    if ($onPath) { return $onPath.Source }

    $candidates = @()
    foreach ($root in @("$env:ProgramFiles\R", "${env:ProgramFiles(x86)}\R")) {
        if (-not (Test-Path -LiteralPath $root)) { continue }
        foreach ($dir in Get-ChildItem -LiteralPath $root -Directory) {
            $exe = Join-Path $dir.FullName 'bin\Rscript.exe'
            if (-not (Test-Path -LiteralPath $exe)) { continue }
            $parsed = $null
            if ($dir.Name -match 'R-(\d+\.\d+\.\d+)') {
                $parsed = [version]$Matches[1]
            }
            $candidates += [pscustomobject]@{ Version = $parsed; Path = $exe }
        }
    }
    $best = $candidates | Sort-Object Version -Descending | Select-Object -First 1
    if ($best) { return $best.Path }

    throw "Rscript.exe was not found. Install R, add it to PATH, or set NFL_RSCRIPT."
}

function Resolve-Node {
    if ($env:NFL_NODE) {
        if (Test-Path -LiteralPath $env:NFL_NODE) { return $env:NFL_NODE }
        throw "NFL_NODE is set to '$env:NFL_NODE' but that file does not exist."
    }

    $onPath = Get-Command node.exe -ErrorAction SilentlyContinue
    if ($onPath) { return $onPath.Source }

    $fallback = Join-Path $env:ProgramFiles 'nodejs\node.exe'
    if (Test-Path -LiteralPath $fallback) { return $fallback }

    throw "node.exe was not found. Install Node.js, add it to PATH, or set NFL_NODE."
}

# R and Node write progress and warnings to stderr. Under
# $ErrorActionPreference = "Stop", PowerShell 5.1 wraps native stderr in
# ErrorRecords and raises a terminating NativeCommandError as soon as the caller
# redirects output, which kills an otherwise healthy run partway through
# (`.\run_fantasy_props.ps1 > log.txt 2>&1` was enough to trigger it). The exit
# code is the real success signal, so suppress that behaviour.
#
# This deliberately returns nothing: the command's own output must flow through
# to the caller's stdout so redirection still captures it. Check the global
# $LASTEXITCODE immediately after calling.
function Invoke-Native {
    param(
        [Parameter(Mandatory = $true)][string]$Exe,
        [Parameter(Mandatory = $true)][string[]]$Arguments
    )
    $previous = $ErrorActionPreference
    $ErrorActionPreference = "Continue"
    try {
        & $Exe @Arguments
    }
    finally {
        $ErrorActionPreference = $previous
    }
}

# The workbook builder writes every output and then crashes during interpreter
# teardown inside the bundled spreadsheet runtime, so its exit code is not a
# usable success signal. Verify the artifact was rewritten by this run instead.
function Assert-FreshArtifact {
    param(
        [Parameter(Mandatory = $true)][string]$Path,
        [Parameter(Mandatory = $true)][datetime]$NotBefore,
        [string]$What = "artifact"
    )
    if (-not (Test-Path -LiteralPath $Path)) {
        throw "The $What was not created at $Path."
    }
    $written = (Get-Item -LiteralPath $Path).LastWriteTime
    if ($written -lt $NotBefore) {
        throw "The $What at $Path is stale (last written $written); the build did not rewrite it."
    }
}

$rscript = Resolve-Rscript
$node = Resolve-Node
