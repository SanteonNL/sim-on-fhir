# ============================================================
# _publish.ps1
#
# Publishes the generated IG to Azure Static Website storage.
# Auto-detects git branch to determine target path:
#   main              -> ig/        (CarePlan)
#   feature/cb-ibd-ig -> ig/ibd     (IBD)
#
# USAGE:
#   .\_publish.ps1
# ============================================================

$STORAGE_ACCOUNT = "dlswesanhipsp04"

# Auto-detect target path based on git branch
# To add a new IG (e.g. COPD), create a feature branch and add it here:
#   "feature/cb-copd-ig" { $TARGET_PATH = "ig/copd" }
$branch = git rev-parse --abbrev-ref HEAD 2>$null
switch ($branch) {
    "main"              { $TARGET_PATH = "ig" }
    "feature/cb-ibd-ig" { $TARGET_PATH = "ig/ibd" }
    default {
        Write-Host "ERROR: Unknown branch '$branch'. Add it to the switch in _publish.ps1 first." -ForegroundColor Red
        Write-Host "Example:  `"$branch`" { `$TARGET_PATH = `"ig/<name>`" }" -ForegroundColor Yellow
        exit 1
    }
}
Write-Host "Branch: $branch -> publishing to $TARGET_PATH/" -ForegroundColor Cyan

$OUTPUT_DIR      = Join-Path $PSScriptRoot "output"

# Check output folder exists
if (-not (Test-Path $OUTPUT_DIR)) {
    Write-Host "ERROR: output/ folder not found. Run _genonce.bat first." -ForegroundColor Red
    exit 1
}

# Check az CLI is available
if (-not (Get-Command az -ErrorAction SilentlyContinue)) {
    Write-Host "ERROR: Azure CLI (az) is not installed. Install via: winget install Microsoft.AzureCLI" -ForegroundColor Red
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

# Upload output/ to $web/ibd/
Write-Host "`nUploading IG to https://$STORAGE_ACCOUNT.z6.web.core.windows.net/$TARGET_PATH/ ..." -ForegroundColor Cyan

az storage blob upload-batch `
    --account-name $STORAGE_ACCOUNT `
    --source $OUTPUT_DIR `
    --destination '$web' `
    --destination-path $TARGET_PATH `
    --overwrite `
    --auth-mode login

if ($LASTEXITCODE -eq 0) {
    # Re-upload HTML files with correct content-type
    Write-Host "`nSetting content-type for HTML files..." -ForegroundColor Cyan
    az storage blob upload-batch `
        --account-name $STORAGE_ACCOUNT `
        --source $OUTPUT_DIR `
        --destination '$web' `
        --destination-path $TARGET_PATH `
        --overwrite `
        --auth-mode login `
        --content-type "text/html" `
        --pattern "*.html"

    Write-Host "`nPublished successfully!" -ForegroundColor Green
    Write-Host "View at: https://$STORAGE_ACCOUNT.z6.web.core.windows.net/$TARGET_PATH/en/index.html" -ForegroundColor Cyan
} else {
    Write-Host "`nUpload failed. Check that you have Storage Blob Data Contributor role on the storage account." -ForegroundColor Red
    exit 1
}
