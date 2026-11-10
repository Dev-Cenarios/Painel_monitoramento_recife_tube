param(
  [Parameter(Position=0, Mandatory=$true)]
  [string]$Disease,

  [Parameter(Position=1)]
  [int]$Port = 0,

  [switch]$Force,

  [string]$Bind = "127.0.0.1",

  [switch]$NoRenv
)

$ErrorActionPreference = "Stop"

function Get-RscriptExe {
  $cmd = Get-Command Rscript.exe -ErrorAction SilentlyContinue
  if ($cmd -and (Test-Path $cmd.Source)) { return $cmd.Source }

  $reg = Get-ItemProperty "HKCU:\SOFTWARE\R-core\R" -ErrorAction SilentlyContinue
  if ($reg -and $reg.InstallPath) {
    $cand = Join-Path $reg.InstallPath "bin\Rscript.exe"
    if (Test-Path $cand) { return $cand }
  }

  $reg = Get-ItemProperty "HKLM:\SOFTWARE\R-core\R" -ErrorAction SilentlyContinue
  if ($reg -and $reg.InstallPath) {
    $cand = Join-Path $reg.InstallPath "bin\Rscript.exe"
    if (Test-Path $cand) { return $cand }
  }

  $reg = Get-ItemProperty "HKLM:\SOFTWARE\WOW6432Node\R-core\R" -ErrorAction SilentlyContinue
  if ($reg -and $reg.InstallPath) {
    $cand = Join-Path $reg.InstallPath "bin\Rscript.exe"
    if (Test-Path $cand) { return $cand }
  }

  $common = @(
    "C:\\Program Files\\R",
    "C:\\Program Files (x86)\\R"
  )
  foreach ($base in $common) {
    if (-not (Test-Path $base)) { continue }
    $dirs = Get-ChildItem -Path $base -Directory -ErrorAction SilentlyContinue |
      Sort-Object Name -Descending
    foreach ($d in $dirs) {
      $cand = Join-Path $d.FullName "bin\Rscript.exe"
      if (Test-Path $cand) { return $cand }
    }
  }

  throw "Rscript.exe não encontrado. Seu Registry tem InstallPath? (HKCU:\SOFTWARE\R-core\R ou HKLM:\SOFTWARE\R-core\R)"
}

function Test-PortInUse([int]$p) {
  try {
    $c = Get-NetTCPConnection -LocalPort $p -State Listen -ErrorAction SilentlyContinue
    return [bool]$c
  } catch {
    $hit = netstat -ano | Select-String -Pattern (":$p\s")
    return [bool]$hit
  }
}

function Get-FreePort([int]$start=8000) {
  for ($p=$start; $p -lt ($start+500); $p++) {
    if (-not (Test-PortInUse $p)) { return $p }
  }
  throw "Não encontrei porta livre entre $start e " + ($start+499)
}

function Stop-Port([int]$p) {
  $procIds = @()

  try {
    $c = Get-NetTCPConnection -LocalPort $p -State Listen -ErrorAction SilentlyContinue
    if ($c) { $procIds += ($c | Select-Object -ExpandProperty OwningProcess) }
  } catch { }

  if (-not $procIds -or $procIds.Count -eq 0) {
    $lines = netstat -ano | Select-String -Pattern (":$p\s")
    foreach ($ln in $lines) {
      $parts = ($ln.ToString() -split "\s+") | Where-Object { $_ -ne "" }
      if ($parts.Count -ge 5) { $procIds += [int]$parts[-1] }
    }
  }

  $procIds = @($procIds | Sort-Object -Unique)
  foreach ($procId in $procIds) {
    if ($procId -and $procId -gt 0) {
      try { taskkill /PID $procId /F | Out-Null } catch { }
    }
  }
}

$shinyRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$d = $Disease.ToUpper()

$appDir = Join-Path $shinyRoot ("core\apps\" + $d)
$appFile = Join-Path $appDir "app.R"
if (-not (Test-Path $appFile)) { throw "app.R não encontrado em: $appFile" }

$rscript = Get-RscriptExe

if ($Port -le 0) { $Port = Get-FreePort 8000 }
if ($Force) { Stop-Port $Port }
if (Test-PortInUse $Port) { $Port = Get-FreePort ($Port + 1) }

$logsDir = Join-Path $shinyRoot "logs"
New-Item -ItemType Directory -Force -Path $logsDir | Out-Null
$ts = Get-Date -Format "yyyyMMdd_HHmmss"
$logFile = Join-Path $logsDir ("run_{0}_{1}.log" -f $d, $ts)
$errFile = Join-Path $logsDir ("run_{0}_{1}.err.log" -f $d, $ts)

$rootR = $shinyRoot.Replace("\","/")
$appR  = $appDir.Replace("\","/")

# Disable renv by default due to library sync issues - packages are in system library
$renvCmd = ""
$expr = "shiny::runApp('$appR', host='$Bind', port=$Port, launch.browser=FALSE)"

Write-Host ""
Write-Host "[run] Disease=$d  Bind=$Bind  Port=$Port"
Write-Host "[run] Rscript=$rscript"
Write-Host "[run] AppDir=$appDir"
Write-Host "[run] Log=$logFile"
Write-Host "[run] Err=$errFile"
Write-Host "[run] URL=http://$Bind`:$Port"
Write-Host ""

$runnerFile = Join-Path $logsDir ("run_{0}_{1}.R" -f $d, $ts)
[System.IO.File]::WriteAllText($runnerFile, $expr, (New-Object System.Text.UTF8Encoding($false)))

Start-Process -FilePath $rscript `
  -ArgumentList @("--vanilla", $runnerFile) `
  -WorkingDirectory $shinyRoot `
  -RedirectStandardOutput $logFile `
  -RedirectStandardError  $errFile `
  -WindowStyle Normal

Start-Sleep -Milliseconds 700
Start-Process "http://$Bind`:$Port"
