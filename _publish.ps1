# ============================================================
# _publish.ps1
#
# Publishes the generated IG to Azure Static Website storage.
# Target: https://dlswesanhipsp04.z6.web.core.windows.net/ig/
#
# USAGE:
#   .\_publish.ps1
# ============================================================

$STORAGE_ACCOUNT = "dlswesanhipsp04"
$TARGET_PATH     = "ig"
$OUTPUT_DIR      = Join-Path $PSScriptRoot "output"

# Check output folder exists
if (-not (Test-Path $OUTPUT_DIR)) {
    Write-Host "ERROR: output/ folder not found. Run sushi and _genonce.bat first." -ForegroundColor Red
    exit 1
}

# Check az CLI is available
if (-not (Get-Command az -ErrorAction SilentlyContinue)) {
    Write-Host "ERROR: Azure CLI (az) is not installed. Install via: choco install azure-cli" -ForegroundColor Red
    exit 1
}

# Check if already logged in, otherwise trigger login
Write-Host "Checking Azure login..." -ForegroundColor Cyan
$account = az account show 2>$null | ConvertFrom-Json
if (-not $account) {
    Write-Host "Not logged in. Starting az login..." -ForegroundColor Yellow
    az login
    $account = az account show 2>$null | ConvertFrom-Json
    if (-not $account) {
        Write-Host "ERROR: Login failed." -ForegroundColor Red
        exit 1
    }
}
Write-Host "Logged in as: $($account.user.name) (subscription: $($account.name))" -ForegroundColor Green

# Upload output/ to $web/ig/
Write-Host "`nUploading IG to https://$STORAGE_ACCOUNT.z6.web.core.windows.net/$TARGET_PATH/ ..." -ForegroundColor Cyan

az storage blob upload-batch `
    --account-name $STORAGE_ACCOUNT `
    --source $OUTPUT_DIR `
    --destination '$web' `
    --destination-path $TARGET_PATH `
    --overwrite `
    --auth-mode login

if ($LASTEXITCODE -eq 0) {
    Write-Host "`nPublished successfully!" -ForegroundColor Green
    Write-Host "View at: https://$STORAGE_ACCOUNT.z6.web.core.windows.net/$TARGET_PATH/index.html" -ForegroundColor Cyan
} else {
    Write-Host "`nUpload failed. Check that you have Storage Blob Data Contributor role on the storage account." -ForegroundColor Red
    exit 1
}
