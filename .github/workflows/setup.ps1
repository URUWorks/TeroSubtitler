#!/usr/bin/env pwsh
################################################################################

$ErrorActionPreference = 'stop'
Set-PSDebug -Strict
New-Variable -Option Constant -Name VAR -Value @{
    Uri = 'https://download.lazarus-ide.org/Lazarus%20Windows%2064%20bits/Lazarus%204.0/lazarus-4.0-fpc-3.2.2-win64.exe'
    OutFile = (New-TemporaryFile).FullName + '.exe'
}
Invoke-WebRequest @VAR
& $VAR.OutFile.Replace('Temp', 'Temp\.') /SP- /VERYSILENT /NORESTART `
    /SUPPRESSMSGBOXES | Out-Null
$Env:PATH+=';C:\Lazarus'
(Get-Command 'lazbuild').Source | Out-Host
$Env:PATH+=';C:\Lazarus\fpc\3.2.2\bin\x86_64-win64'
(Get-Command 'instantfpc').Source | Out-Host
$Env:INSTANTFPCOPTIONS='-FuC:\Lazarus\components\lazutils'
& instantfpc '.github/workflows/make.pas' build | Out-Host
Exit $LastExitCode
