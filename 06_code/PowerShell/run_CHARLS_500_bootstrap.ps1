# PowerShell script to launch parallel bootstrap using independent Rscript processes
# Uses ALL logical CPU cores

$ErrorActionPreference = "Stop"
$projectRoot = "E:\公共数据库\中国数据库\医养结合政策DID_CHFS_CFPS"
$rscriptPath = "Rscript.exe"

# ============================================================
# 1. Detect CPU cores
# ============================================================
$nLogical = (Get-CimInstance Win32_Processor).NumberOfLogicalProcessors
$nPhysical = (Get-CimInstance Win32_Processor).NumberOfCores
$nWorkers = $nLogical

Write-Host "=== CHARLS 500-Rep Bootstrap ==="
Write-Host "Logical cores: $nLogical"
Write-Host "Physical cores: $nPhysical"
Write-Host "Workers to launch: $nWorkers"

# ============================================================
# 2. Prepare bootstrap data (if not already done)
# ============================================================
$prepScript = Join-Path $projectRoot "06_code\R\40_prepare_bootstrap_data.R"
if (!(Test-Path (Join-Path $projectRoot "CHARLS_bootstrap_500_seed_list.csv"))) {
    Write-Host "Preparing bootstrap data..."
    & $rscriptPath $prepScript
}

# ============================================================
# 3. Load seed list and create assignments
# ============================================================
$seedList = Import-Csv (Join-Path $projectRoot "CHARLS_bootstrap_500_seed_list.csv")
$nTotal = $seedList.Count
Write-Host "Total seeds: $nTotal"

# Check for existing results
$checkpointDir = Join-Path $projectRoot "07_results\bootstrap_checkpoints"
$existingResults = @()
if (Test-Path $checkpointDir) {
    Get-ChildItem $checkpointDir -Filter "worker_*_results.rds" | ForEach-Object {
        $existingResults += $_.Name
    }
}
$nExisting = $existingResults.Count
$nRemaining = $nTotal - $nExisting
Write-Host "Existing results: $nExisting"
Write-Host "Remaining: $nRemaining"

if ($nRemaining -le 0) {
    Write-Host "All 500 replications already complete!"
    exit 0
}

# ============================================================
# 4. Create worker assignments
# ============================================================
$assignmentsPerWorker = [math]::Ceiling($nRemaining / $nWorkers)
Write-Host "Assignments per worker: $assignmentsPerWorker"

# Filter out already-completed replications
$completedReps = @()
foreach ($f in $existingResults) {
    # Extract replicate IDs from result files (simplified)
    $completedReps += 0  # Placeholder - actual extraction needed
}

# Create assignment files
$assignmentDir = Join-Path $projectRoot "07_results\bootstrap_assignments"
New-Item -ItemType Directory -Path $assignmentDir -Force | Out-Null

for ($w = 1; $w -le $nWorkers; $w++) {
    $startIdx = ($w - 1) * $assignmentsPerWorker + 1
    $endIdx = [math]::Min($w * $assignmentsPerWorker, $nTotal)

    if ($startIdx -gt $nTotal) { break }

    $workerSeeds = $seedList[($startIdx-1)..($endIdx-1)]
    $assignmentFile = Join-Path $assignmentDir "worker_${w}_assignment.csv"
    $workerSeeds | Export-Csv -Path $assignmentFile -NoTypeInformation

    Write-Host "Worker $w: reps $startIdx to $endIdx ($($workerSeeds.Count) reps)"
}

# ============================================================
# 5. Launch workers
# ============================================================
Write-Host "`nLaunching $nWorkers Rscript processes..."

$outputDir = Join-Path $projectRoot "07_results\bootstrap_checkpoints"
$dataFile = Join-Path $projectRoot "CHARLS_bootstrap_analysis_dataset.rds"
$targetFile = Join-Path $projectRoot "CHARLS_bootstrap_standardisation_target.rds"
$workerScript = Join-Path $projectRoot "06_code\R\41_bootstrap_worker.R"

$processes = @()
for ($w = 1; $w -le $nWorkers; $w++) {
    $assignmentFile = Join-Path $assignmentDir "worker_${w}_assignment.csv"
    if (!(Test-Path $assignmentFile)) { continue }

    $logFile = Join-Path $outputDir "worker_${w}.log"
    $errFile = Join-Path $outputDir "worker_${w}_error.log"

    $argList = "`"$workerScript`" --args $w `"$assignmentFile`" `"$dataFile`" `"$targetFile`" `"$outputDir`""

    $proc = Start-Process -FilePath $rscriptPath -ArgumentList $argList `
        -PassThru -NoNewWindow `
        -RedirectStandardOutput $logFile `
        -RedirectStandardError $errFile

    $processes += $proc
    Write-Host "Worker $w launched (PID: $($proc.Id))"
    Start-Sleep -Milliseconds 500  # Small delay to avoid disk contention
}

Write-Host "`nAll $nWorkers workers launched."
Write-Host "Waiting for completion..."

# ============================================================
# 6. Wait for all workers
# ============================================================
$processes | Wait-Process -Timeout 3600

# Check exit codes
$failed = 0
foreach ($proc in $processes) {
    if ($proc.ExitCode -ne 0) {
        Write-Host "Worker PID $($proc.Id) exited with code $($proc.ExitCode)"
        $failed++
    }
}

Write-Host "Workers completed. Failed: $failed / $nWorkers"

# ============================================================
# 7. Aggregate results
# ============================================================
Write-Host "`nAggregating results..."
$aggScript = Join-Path $projectRoot "06_code\R\42_aggregate_bootstrap_results.R"
& $rscriptPath $aggScript

Write-Host "`nBootstrap pipeline complete."
