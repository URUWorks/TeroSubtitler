#!/usr/bin/env pwsh
# Builds the Tero Subtitler installer for Windows (installscript_win.iss, Inno Setup 6.5+).
#
#   Full installer (release, works offline, libraries inside):
#     pwsh make_installer.ps1
#   Web installer (nightly, downloads the libraries and dictionaries when installing):
#     pwsh make_installer.ps1 -Web -DicBaseUrl https://raw.githubusercontent.com/URUWorks/TeroSubtitler/<commit>/TeroSubtitler/bin/Dictionaries/
#
# libmpv comes from URUWorks/UW_libmpv (latest release, and "frozen" for Windows 7/8.1),
# OpenSSL 3 and Hunspell from URUWorks/UW_TSLibs (latest release). The latest tags are
# resolved now, so a web installer always downloads the same files it was built with,
# and checks each of them by its SHA-256.
#
# -Arm64Bin: folder with the ARM64 tero.exe; the installer then picks it on ARM64 Windows.

param(
    [string]$Bin = (Join-Path $PSScriptRoot 'bin'),
    [string]$Arm64Bin,
    [switch]$Web,
    [string]$DicBaseUrl,
    [string]$AppVersion,
    [string]$OutputName,
    [string]$OutputDir = $PSScriptRoot,
    [string]$Iscc
)

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

function Find-Iscc {
    if ($Iscc) { return $Iscc }
    $cmd = Get-Command 'ISCC.exe' -ErrorAction SilentlyContinue
    if ($cmd) { return $cmd.Source }
    foreach ($p in "${env:ProgramFiles(x86)}\Inno Setup 6\ISCC.exe",
                   "$env:ProgramFiles\Inno Setup 6\ISCC.exe",
                   "$env:LOCALAPPDATA\Programs\Inno Setup 6\ISCC.exe") {
        if (Test-Path $p) { return $p }
    }
    throw 'ISCC.exe not found (Inno Setup 6.5 or later); pass it with -Iscc'
}

function Get-LatestTag([string]$Repo) {
    $headers = @{ 'User-Agent' = 'TeroSubtitler-make-installer' }
    $token = if ($env:GH_TOKEN) { $env:GH_TOKEN } else { $env:GITHUB_TOKEN }
    if ($token) { $headers.Authorization = "Bearer $token" }
    (Invoke-RestMethod -Headers $headers -Uri "https://api.github.com/repos/URUWorks/$Repo/releases/latest").tag_name
}

$Bin = (Resolve-Path $Bin).Path
if (-not (Test-Path (Join-Path $Bin 'tero.exe'))) { throw "tero.exe not found in $Bin" }
if ($Arm64Bin) {
    $Arm64Bin = (Resolve-Path $Arm64Bin).Path
    if (-not (Test-Path (Join-Path $Arm64Bin 'tero.exe'))) { throw "tero.exe not found in $Arm64Bin" }
}
if ($Web -and -not $DicBaseUrl) {
    $commit = (git -C $PSScriptRoot rev-parse HEAD).Trim()
    $DicBaseUrl = "https://raw.githubusercontent.com/URUWorks/TeroSubtitler/$commit/TeroSubtitler/bin/Dictionaries/"
    Write-Warning "Dictionaries from $DicBaseUrl (that commit must be on GitHub)"
}
if ($DicBaseUrl -and -not $DicBaseUrl.EndsWith('/')) { $DicBaseUrl += '/' }
New-Item -ItemType Directory -Force -Path $OutputDir | Out-Null
$OutputDir = (Resolve-Path $OutputDir).Path

$mpvTag = Get-LatestTag 'UW_libmpv'
$tsTag = Get-LatestTag 'UW_TSLibs'
"UW_libmpv $mpvTag, UW_TSLibs $tsTag"
$gh = 'https://github.com/URUWorks'
$zips = [ordered]@{
    MpvX64    = "$gh/UW_libmpv/releases/download/$mpvTag/libmpv2_win64.zip"
    MpvLegacy = "$gh/UW_libmpv/releases/download/frozen/libmpv2_win64_legacy.zip"
    TSLibsX64 = "$gh/UW_TSLibs/releases/download/$tsTag/tslibs_win64.zip"
}
if ($Arm64Bin) {
    $zips.MpvArm64 = "$gh/UW_libmpv/releases/download/$mpvTag/libmpv2_winarm64.zip"
    $zips.TSLibsArm64 = "$gh/UW_TSLibs/releases/download/$tsTag/tslibs_winarm64.zip"
}

$tmp = Join-Path ([IO.Path]::GetTempPath()) "tero-installer-$PID"
New-Item -ItemType Directory -Force -Path $tmp | Out-Null
try {
    $defines = @("/DSourceBin=$Bin")
    foreach ($k in $zips.Keys) {
        $zip = Join-Path $tmp "$k.zip"
        "Downloading $($zips[$k])"
        Invoke-WebRequest -UseBasicParsing -Uri $zips[$k] -OutFile $zip
        if ($Web) {
            $sha = (Get-FileHash -Algorithm SHA256 $zip).Hash.ToLower()
            $defines += "/D${k}Url=$($zips[$k])", "/D${k}Sha=$sha"
            "  SHA-256 $sha"
        } else {
            Expand-Archive -Force $zip -DestinationPath (Join-Path $tmp $k)
        }
    }

    if ($Web) {
        $defines += '/DWebInstall', "/DDicBaseUrl=$DicBaseUrl"
    } else {
        # One folder per case: libmpv + OpenSSL 3 and Hunspell
        $sets = [ordered]@{ LibsX64 = 'MpvX64', 'TSLibsX64'; LibsLegacy = 'MpvLegacy', 'TSLibsX64' }
        if ($Arm64Bin) { $sets.LibsArm64 = 'MpvArm64', 'TSLibsArm64' }
        foreach ($s in $sets.Keys) {
            $dir = Join-Path $tmp $s
            New-Item -ItemType Directory -Force -Path $dir | Out-Null
            foreach ($k in $sets[$s]) {
                Get-ChildItem (Join-Path $tmp $k) -Recurse -File | Copy-Item -Destination $dir -Force
            }
            "$s`: " + ((Get-ChildItem $dir -Filter *.dll).Name -join ', ')
            $defines += "/D$s=$dir"
        }
    }
    if ($Arm64Bin) { $defines += "/DArm64Bin=$Arm64Bin" }
    if ($AppVersion) { $defines += "/DAppVersion=$AppVersion" }
    if ($OutputName) { $defines += "/DOutputName=$OutputName" }

    $compiler = Find-Iscc
    & $compiler /Qp @defines "/O$OutputDir" (Join-Path $PSScriptRoot 'installscript_win.iss')
    if ($LastExitCode -ne 0) { throw "ISCC failed ($LastExitCode)" }
} finally {
    Remove-Item -Recurse -Force $tmp -ErrorAction SilentlyContinue
}
