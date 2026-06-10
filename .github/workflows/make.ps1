#!/usr/bin/env pwsh
#-------------------------------------------------------------------------------
# FILTERS AND FUNCTIONS
#-------------------------------------------------------------------------------

Filter Out-Log {
    $(
        If (! (Test-Path -Path Variable:LastExitCode)) {
            "$(Get-Date -uformat '%y-%m-%d_%T')$([char]27)[33m {0}$([char]27)[0m" -f $_
        } ElseIf ($LastExitCode -eq 0) {
            "$(Get-Date -uformat '%y-%m-%d_%T')$([char]27)[32m {0}$([char]27)[0m" -f $_
        } Else {
            "$(Get-Date -uformat '%y-%m-%d_%T')$([char]27)[31m [{0}]`t{1}$([char]27)[0m" -f $LastExitCode, $_
        }
    ) | Out-Host
}

Filter Get-Url {
    'Get-Url {0}' -f $_ | Out-Log
    $pattern = $_
    $baseUrl = 'https://fossies.org/windows/misc/'
    $baseUrl + (
        (Invoke-WebRequest -Uri $baseUrl -UseBasicParsing).Links.href |
            Where-Object { $_ -match '^{0}.*(exe|msi)$' -f $pattern }
    )[-1]
}

Filter Get-Package {
    $OutFile = '{0}.{1}' -f (New-TemporaryFile).FullName, (Split-Path -Path $_ -Leaf).Split('?')[0]
    Invoke-WebRequest -OutFile $OutFile -Uri $_
    'Get-Package from {0} to {1}' -f $_, $OutFile | Out-Log
    $OutFile
}

Function Install-Package {
    $Input | ForEach-Object {
        If ((Split-Path -Path $_ -Leaf).Split('.')[-1] -eq 'msi') {
            $arguments = @('/passive', '/package', $_ )
            Start-Process -FilePath 'msiexec' -ArgumentList $arguments -Wait -NoNewWindow
        } Else {
            $arguments = @('/SP-', '/VERYSILENT', '/NORESTART', '/SUPPRESSMSGBOXES')
            Start-Process -FilePath $_ -ArgumentList $arguments -Wait -NoNewWindow
        }
        'Install-Package {0}' -f $_ | Out-Log
        Remove-Item $_
    }
}

#-------------------------------------------------------------------------------
# MAIN ENDPOINT
#-------------------------------------------------------------------------------

$ErrorActionPreference = 'stop'
Set-PSDebug -Strict
@(
    'https://download.lazarus-ide.org/Lazarus%20Windows%2064%20bits/Lazarus%204.6/lazarus-4.6-fpc-3.2.2-win64.exe'
    'https://slproweb.com/download/Win64OpenSSL_Light-4_0_0.msi'
) | Get-Package | Install-Package
$env:PATH+=';{0}\OpenSSL-Win64\tools' -f $env:PROGRAMFILES
(Get-Command 'openssl').Source | Out-Log
$env:PATH+=';C:\Lazarus'
(Get-Command 'lazbuild').Source | Out-Log
$env:PATH+=';C:\Lazarus\fpc\3.2.2\bin\x86_64-win64'
(Get-Command 'instantfpc').Source | Out-Log
$env:INSTANTFPCOPTIONS='-FuC:\Lazarus\components\lazutils'
$env:DYLD_LIBRARY_PATH='{0}\OpenSSL-Win64\lib' -f $env:PROGRAMFILES
& instantfpc '.github/workflows/make.pas' build | Out-Log
Exit $LastExitCode