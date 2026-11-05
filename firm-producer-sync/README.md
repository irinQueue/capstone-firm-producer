# Firm-Producer Sync capstone (new code set)

Three GnuCOBOL batch programs that run in this order:

| Program | Reads | Writes | Return codes |
|---|---|---|---|
| INTAKE  | data/LNA.DAT | out/VALID.DAT, out/REJECT.DAT, out/INTAKE.RPT | 0 accepted, 8 file rejected, 12 open failed |
| UPDATE  | out/VALID.DAT, data/MASTER.DAT | out/NEWMASTER.DAT, out/ACTIVITY.DAT | 0, 12 |
| REPORTS | data/MASTER.DAT, out/NEWMASTER.DAT | out/FIRMPROD.RPT, out/TINCHG.RPT | 0, 12 |

Quick run (PowerShell, from this folder):

    $env:PATH = "C:\msys64\ucrt64\bin;C:\msys64\usr\bin;" + $env:PATH
    powershell -ExecutionPolicy Bypass -File tools/run-pipeline.ps1
    sh tools/check-all.sh

Or in VS Code: Ctrl+Shift+P > Tasks: Run Task > 1 to 7.

All data is fictional. A local GnuCOBOL run is a smoke test, not a z/OS result.
