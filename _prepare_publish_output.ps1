# ============================================================
# _prepare_publish_output.ps1
#
# Creates a clean publish folder from output/ by removing files
# that are useful locally but unnecessary for public hosting.
#
# USAGE:
#   .\_prepare_publish_output.ps1
# ============================================================

param(
    [string]$SourceDir = (Join-Path $PSScriptRoot "output"),
    [string]$PublishDir = (Join-Path $PSScriptRoot ".publish-output"),
    [switch]$KeepFullIgZip
)

if (-not (Test-Path $SourceDir)) {
    Write-Host "ERROR: Source folder not found: $SourceDir" -ForegroundColor Red
    exit 1
}

if (Test-Path $PublishDir) {
    Remove-Item $PublishDir -Recurse -Force
}

New-Item -ItemType Directory -Path $PublishDir | Out-Null

# Copy output/ into .publish-output/
try {
    Copy-Item -Path (Join-Path $SourceDir '*') -Destination $PublishDir -Recurse -Force
} catch {
    Write-Host "ERROR: Failed to copy output to publish folder: $($_.Exception.Message)" -ForegroundColor Red
    exit 1
}

# Remove local-only artifacts from publish folder
$removeList = @(
    "qa.json",
    "qa-time-report.json",
    "qa-time-report.tsv",
    "package.db"
)

if (-not $KeepFullIgZip) {
    $removeList += "full-ig.zip"
}

foreach ($name in $removeList) {
    $path = Join-Path $PublishDir $name
    if (Test-Path $path) {
        Remove-Item $path -Force
        Write-Host "Removed: $name" -ForegroundColor Yellow
    }
}

$fileCount = (Get-ChildItem $PublishDir -Recurse -File | Measure-Object).Count
$totalMb = [math]::Round(((Get-ChildItem $PublishDir -Recurse -File | Measure-Object Length -Sum).Sum / 1MB), 2)

Write-Host "Prepared publish folder: $PublishDir" -ForegroundColor Green
Write-Host "Files: $fileCount | Size: $totalMb MB" -ForegroundColor Cyan
