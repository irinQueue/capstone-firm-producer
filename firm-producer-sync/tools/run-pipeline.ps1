# Firm-Producer Sync: build, then run INTAKE -> UPDATE -> REPORTS.
# A later program runs only if the one before it ended with return code 0.
# Run from the project root:  powershell -ExecutionPolicy Bypass -File tools/run-pipeline.ps1
$env:PATH = "C:\msys64\ucrt64\bin;C:\msys64\usr\bin;" + $env:PATH
New-Item -ItemType Directory -Force -Path bin, out | Out-Null
Remove-Item out/* -ErrorAction SilentlyContinue

foreach ($p in "INTAKE", "UPDATE", "REPORTS") {
    cobc -x -I src/copy -o "bin/$p" "src/$p.cbl"
    if ($LASTEXITCODE -ne 0) { Write-Host "BUILD FAILED: $p"; exit 1 }
}
Write-Host "BUILD OK"

foreach ($p in "INTAKE", "UPDATE", "REPORTS") {
    & "bin/$p"
    $rc = $LASTEXITCODE
    Write-Host "$p RC=$rc"
    if ($rc -ne 0) { Write-Host "PIPELINE STOPPED AFTER $p"; exit $rc }
}
Write-Host "PIPELINE COMPLETE"
