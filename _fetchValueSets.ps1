# ============================================================
# _fetchValueSets.ps1
#
# Scans all FSH files for external ValueSet URLs and fetches
# them into input/resources/ so the IG Publisher renders them
# as readable HTML instead of linking to raw JSON.
#
# USAGE:
#   .\_fetchValueSets.ps1              # uses default flag below
#   .\_fetchValueSets.ps1 -Fetch $true # force fetch
#   .\_fetchValueSets.ps1 -Fetch $false # skip fetch
#
# Set $DEFAULT_FETCH to $true to always fetch on build.
# ============================================================

param(
    [bool]$Fetch = $true   # <-- change to $false to disable by default
)

if (-not $Fetch) {
    Write-Host "ValueSet fetching is disabled. Skipping." -ForegroundColor Yellow
    exit 0
}

$rootDir     = $PSScriptRoot
$fshDir      = Join-Path $rootDir "input\fsh"
$resourcesDir = Join-Path $rootDir "input\resources"

# Ensure output directory exists
New-Item -ItemType Directory -Force -Path $resourcesDir | Out-Null

# Scan all FSH files for external ValueSet URLs
# Matches patterns like: from http://... (required|extensible|preferred|example)
$pattern = 'from\s+(https?://\S+?)\s*\('
$urls = @()

Get-ChildItem -Path $fshDir -Filter "*.fsh" -Recurse | ForEach-Object {
    $content = Get-Content $_.FullName -Raw
    $regexMatches = [regex]::Matches($content, $pattern)
    foreach ($match in $regexMatches) {
        $url = $match.Groups[1].Value.TrimEnd(')')
        if ($url -notin $urls) {
            $urls += $url
        }
    }
}

if ($urls.Count -eq 0) {
    Write-Host "No external ValueSet URLs found in FSH files." -ForegroundColor Cyan
    exit 0
}

Write-Host "Found $($urls.Count) external ValueSet(s) to fetch:" -ForegroundColor Cyan

foreach ($url in $urls) {
    $fetchUrl = "$url`?_format=json"
    # Derive a safe filename from the last path segment
    $id = ($url -split '/')[-1] -replace '[^a-zA-Z0-9._-]', '-'
    $outFile = Join-Path $resourcesDir "ValueSet-$id.json"

    Write-Host "  Fetching: $url" -ForegroundColor White
    try {
        Invoke-WebRequest -Uri $fetchUrl -OutFile $outFile -UseBasicParsing -AllowInsecureRedirect -ErrorAction Stop
        Write-Host "  Saved to: input/resources/ValueSet-$id.json" -ForegroundColor Green
    } catch {
        Write-Host "  FAILED: $_" -ForegroundColor Red
    }
}

Write-Host "`nDone. Run 'sushi .' and '.\_genonce.bat' to rebuild the IG." -ForegroundColor Cyan
