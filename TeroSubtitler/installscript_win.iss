; Inno Setup 6.5+ script for Tero Subtitler (Windows x64 and ARM64)
;
; Two modes, same script. make_installer.ps1 prepares the libraries and passes the defines:
;
;   Full (release, works offline): libmpv, OpenSSL 3 and Hunspell go inside the installer.
;     /DLibsX64=<dir>      DLLs for x64 on Windows 10/11 (UW_libmpv latest + UW_TSLibs)
;     /DLibsLegacy=<dir>   DLLs for x64 on Windows 7/8.1 (UW_libmpv "frozen" + UW_TSLibs)
;     /DLibsArm64=<dir>    DLLs for ARM64, with /DArm64Bin
;     With none of them, the DLLs in SourceBin are installed (as the old script did).
;
;   Web (/DWebInstall, nightly): the installer detects Windows and its architecture and
;   downloads the libraries and the chosen dictionaries, each checked by its SHA-256.
;     /DMpvX64Url /DMpvX64Sha /DMpvLegacyUrl /DMpvLegacySha /DTSLibsX64Url /DTSLibsX64Sha
;     /DMpvArm64Url /DMpvArm64Sha /DTSLibsArm64Url /DTSLibsArm64Sha (with /DArm64Bin)
;     /DDicBaseUrl=<url of the Dictionaries folder, pinned to a commit, ending in />
;
; Common:
;   /DSourceBin=<dir>    tero.exe and its data (default: bin next to this script)
;   /DArm64Bin=<dir>     folder with the ARM64 tero.exe (optional; without it ARM64 runs x64)
;   /DAppVersion=<text>  shown version (default: the version of tero.exe)
;   /DOutputName=<name>  installer file name, without .exe

; /DChannel=<Nightly|Beta|RC>: installed apart from the stable release, with its own
; AppId, folder, shortcut and file types (the nightly installer replaced the stable one)
#ifdef Channel
  #define MyAppName            "Tero Subtitler " + Channel
  #define ProgId               "TeroSubtitler" + Channel
#else
  #define MyAppName            "Tero Subtitler"
  #define ProgId               "TeroSubtitler"
#endif
#define MyAppExeName           "tero.exe"
#define MyAppPublisher         "URUWorks"
#define MyAppURL               "https://github.com/URUWorks/TeroSubtitler/releases"
#define StartingYearCopyright  "2023"
#define EndingCopyrightYear    GetDateTimeString('yyyy','','')

#ifndef SourceBin
  #define SourceBin AddBackslash(SourcePath) + "bin"
#endif
#define ExeVersion GetVersionNumbersString(SourceBin + "\" + MyAppExeName)
#ifndef AppVersion
  #define AppVersion ExeVersion
#endif
#define ShortAppVersion StringChange(ExeVersion, ".", "")
#ifndef OutputName
  #define OutputName "ts" + ShortAppVersion + "_win"
#endif

#ifdef WebInstall
  #ifndef DicBaseUrl
    #error WebInstall needs DicBaseUrl
  #endif
  #ifdef Arm64Bin
    #ifndef MpvArm64Url
      #error WebInstall with Arm64Bin needs MpvArm64Url, TSLibsArm64Url and their SHA-256
    #endif
  #endif
#else
  #ifdef Arm64Bin
    #ifndef LibsArm64
      #error Arm64Bin needs LibsArm64
    #endif
  #endif
#endif

; Dictionaries: one component per <name>.dic in SourceBin\Dictionaries
#define DicDir SourceBin + "\Dictionaries"
#define DicIds "|"
#define FindHandle
#define FindResult
#define DicBase
#define DicBaseOf(str F) Copy(F, 1, Len(F) - 4)
#define DicIdOf(str B) "dic\" + StringChange(StringChange(StringChange(StringChange(B, " ", "_"), "(", ""), ")", ""), "-", "_")
#ifdef DicBaseUrl
  #define DicUrlOf(str B) DicBaseUrl + StringChange(StringChange(StringChange(B, " ", "%20"), "(", "%28"), ")", "%29")
#endif
#define DicLabelOf(str B) B == "bg_BG" ? "Български (bg_BG)" : B == "en_GB" ? "English, United Kingdom (en_GB)" : B == "en_US" ? "English, United States (en_US)" : B == "es_UY" ? "Español (es_UY)" : B == "it_IT" ? "Italiano (it_IT)" : B == "ko_KR" ? "한국어 (ko_KR)" : B == "pt_BR" ? "Português, Brasil (pt_BR)" : B == "pt_PT (pos-AO 1990)" ? "Português, Portugal, pós-AO 1990 (pt_PT)" : B == "pt_PT (pre-AO 1990)" ? "Português, Portugal, pré-AO 1990 (pt_PT)" : B == "ru_RU" ? "Русский (ru_RU)" : B

#sub DicComponent
  #define DicBase DicBaseOf(FindGetFileName(FindHandle))
  #expr DicIds = DicIds + DicIdOf(DicBase) + "|"
Name: "{#DicIdOf(DicBase)}"; Description: "{#DicLabelOf(DicBase)}"; Types: full
#endsub

#ifdef WebInstall
#sub DicFiles
  #define DicBase DicBaseOf(FindGetFileName(FindHandle))
Source: "{tmp}\dic-{#DicBase}.aff"; DestDir: "{app}\Dictionaries"; DestName: "{#DicBase}.aff"; Components: {#DicIdOf(DicBase)}; Flags: external ignoreversion
Source: "{tmp}\dic-{#DicBase}.dic"; DestDir: "{app}\Dictionaries"; DestName: "{#DicBase}.dic"; Components: {#DicIdOf(DicBase)}; Flags: external ignoreversion
#endsub
#else
#sub DicFiles
  #define DicBase DicBaseOf(FindGetFileName(FindHandle))
Source: "{#DicDir}\{#DicBase}.aff"; DestDir: "{app}\Dictionaries"; Components: {#DicIdOf(DicBase)}; Flags: ignoreversion
Source: "{#DicDir}\{#DicBase}.dic"; DestDir: "{app}\Dictionaries"; Components: {#DicIdOf(DicBase)}; Flags: ignoreversion
#endsub
#endif

#ifdef WebInstall
#sub DicDownloads
  #define DicBase DicBaseOf(FindGetFileName(FindHandle))
  if WizardIsComponentSelected('{#DicIdOf(DicBase)}') then
  begin
    QueueDownload('{#DicUrlOf(DicBase)}.aff', 'dic-{#DicBase}.aff', '');
    QueueDownload('{#DicUrlOf(DicBase)}.dic', 'dic-{#DicBase}.dic', '');
  end;
#endsub
#endif

[Setup]
; NOTE: The value of AppId uniquely identifies this application. Do not use the same AppId value in installers for other applications.
#ifdef Channel
AppId={{216B2DAF-093C-4C88-9C82-E34CE46580E5}{#Channel}
#else
AppId={{216B2DAF-093C-4C88-9C82-E34CE46580E5}
#endif
AppName={#MyAppName}
AppVersion={#AppVersion}
AppVerName={#MyAppName} {#AppVersion}

AppCopyright={#StartingYearCopyright}-{#EndingCopyrightYear} {#MyAppPublisher}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}

VersionInfoDescription={#MyAppName} installer
VersionInfoProductName={#MyAppName}
VersionInfoVersion={#ExeVersion}

UninstallDisplayName={#MyAppName}
UninstallDisplayIcon={app}\{#MyAppExeName}

ShowLanguageDialog=yes
UsePreviousLanguage=no
LanguageDetectionMethod=uilanguage

WizardStyle=modern

DefaultDirName={autopf}\{#MyAppName}
ChangesAssociations=yes
; No Start menu folder: only the Tero Subtitler shortcut, uninstalled from Settings or
; with right click > Uninstall, as other apps
DisableProgramGroupPage=yes
PrivilegesRequiredOverridesAllowed=dialog
#ifdef Arm64Bin
; Windows 11 on ARM64 is also x64compatible; Windows 10 on ARM64 only runs the ARM64 build
ArchitecturesAllowed=x64compatible or arm64
ArchitecturesInstallIn64BitMode=x64compatible or arm64
#else
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
#endif
OutputDir={#SourcePath}
OutputBaseFilename={#OutputName}
SetupIconFile={#SourcePath}\images\install.ico
Compression=lzma2/max
SolidCompression=yes
#ifdef WebInstall
; .zip extraction of the downloaded libraries
ArchiveExtraction=full
; libmpv + OpenSSL + Hunspell once extracted
ExtraDiskSpaceRequired=140000000
#endif

; Languages, [Languages] and [CustomMessages], are in a separate file to translate the
; installer without touching this script.
#include "installscript_win_languages.iss"

[Types]
Name: "compact"; Description: "{cm:TypeCompact}"
Name: "full";    Description: "{cm:TypeFull}"
Name: "custom";  Description: "{cm:TypeCustom}"; Flags: iscustom

[Components]
Name: "main"; Description: "{#MyAppName}"; Types: compact full custom; Flags: fixed
Name: "dic";  Description: "{cm:Dictionaries}"; Types: full
#for {FindHandle = FindResult = FindFirst(DicDir + "\*.dic", 0); FindResult; FindResult = FindNext(FindHandle)} DicComponent
#if FindHandle
  #expr FindClose(FindHandle)
#endif

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
#ifdef Channel
; a nightly/beta isn't the default program unless chosen
Name: "fileassoc";   Description: "{cm:AssocSubtitles,{#MyAppName}}"; GroupDescription: "{cm:OtherTasks}"; Flags: unchecked
#else
Name: "fileassoc";   Description: "{cm:AssocSubtitles,{#MyAppName}}"; GroupDescription: "{cm:OtherTasks}"
#endif

[Dirs]
Name: "{app}"; Permissions: everyone-full

[InstallDelete]
; Start menu folder of older versions (now only a shortcut)
Type: filesandordirs; Name: "{autoprograms}\{#MyAppName}"
; OpenSSL 1.1 from older versions (Tero now uses OpenSSL 3)
Type: files; Name: "{app}\libssl-1_1-x64.dll"
Type: files; Name: "{app}\libcrypto-1_1-x64.dll"

[Files]
#ifdef Arm64Bin
Source: "{#Arm64Bin}\{#MyAppExeName}"; DestDir: "{app}"; Check: UseArm64; Flags: ignoreversion
Source: "{#SourceBin}\{#MyAppExeName}"; DestDir: "{app}"; Check: not UseArm64; Flags: ignoreversion
#else
Source: "{#SourceBin}\{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion
#endif
; the default profiles: Tero copies them to the user folder (%LOCALAPPDATA%\tero) the first
; time and uses that copy. Not replaced, the one of older versions can have the user's
; profiles and it is the one copied when updating
Source: "{#SourceBin}\tero.cfs"; DestDir: "{app}"; Flags: onlyifdoesntexist
Source: "{#SourceBin}\Shortcuts\*.key"; DestDir: "{app}\Shortcuts"; Flags: ignoreversion
Source: "{#SourceBin}\OCR\*.ocr"; DestDir: "{app}\OCR"; Flags: ignoreversion
Source: "{#SourceBin}\Languages\*.po"; DestDir: "{app}\Languages"; Flags: ignoreversion
Source: "{#SourceBin}\Languages\*.mo"; DestDir: "{app}\Languages"; Flags: ignoreversion skipifsourcedoesntexist
Source: "{#SourceBin}\Icons\*.ico"; DestDir: "{app}\Icons"; Flags: ignoreversion
Source: "{#SourceBin}\CustomFormat\*.*"; DestDir: "{app}\CustomFormat"; Flags: ignoreversion
Source: "{#DicDir}\*.txt"; DestDir: "{app}\Dictionaries"; Flags: ignoreversion skipifsourcedoesntexist
#for {FindHandle = FindResult = FindFirst(DicDir + "\*.dic", 0); FindResult; FindResult = FindNext(FindHandle)} DicFiles
#if FindHandle
  #expr FindClose(FindHandle)
#endif
; libmpv, OpenSSL 3 and Hunspell
#ifdef WebInstall
Source: "{tmp}\libs\*"; DestDir: "{app}"; Flags: external ignoreversion
#else
  #ifdef LibsX64
Source: "{#LibsX64}\*"; DestDir: "{app}"; Check: UseX64Modern; Flags: ignoreversion
  #else
Source: "{#SourceBin}\*.dll"; DestDir: "{app}"; Check: UseX64Modern; Flags: ignoreversion
  #endif
  #ifdef LibsLegacy
Source: "{#LibsLegacy}\*"; DestDir: "{app}"; Check: UseX64Legacy; Flags: ignoreversion
  #endif
  #ifdef LibsArm64
Source: "{#LibsArm64}\*"; DestDir: "{app}"; Check: UseArm64; Flags: ignoreversion
  #endif
#endif
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Registry]
; Per machine or per user (HKA), so it also works without administrator rights.
; Every extension gets "Open with"; the default program only with the fileassoc task
; (and never for .scr, which is also the screen saver extension).
Root: HKA; Subkey: "Software\Classes\.ass\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.ass"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.ass"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.ass"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.ass"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.ass)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.ass\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\ass.ico"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.ass\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.ssa\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.ssa"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.ssa"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.ssa"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.ssa"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.ssa)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.ssa\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\ssa.ico"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.ssa\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.xas\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.xas"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.xas"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.xas"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.xas"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.xas)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.xas\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\xas.ico"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.xas\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.aqt\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.aqt"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.aqt"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.aqt"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.aqt"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.aqt)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.aqt\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\aqt.ico"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.aqt\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.890\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.890"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.890"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.890"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.890"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.890)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.890\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\890.ico"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.890\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.asc\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.asc"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.asc"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.asc"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.asc"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.asc)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.asc\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\asc.ico"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.asc\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.cap\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.cap"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.cap"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.cap"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.cap"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.cap)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.cap\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.cap\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.dks\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.dks"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.dks"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.dks"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.dks"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.dks)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.dks\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.dks\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.dtc\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.dtc"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.dtc"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.dtc"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.dtc"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.dtc)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.dtc\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.dtc\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.sub\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.sub"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.sub"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.sub"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.sub"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.sub)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.sub\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\sub.ico"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.sub\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.stl\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.stl"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.stl"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.stl"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.stl"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.stl)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.stl\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\stl.ico"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.stl\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.ttxt\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.ttxt"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.ttxt"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.ttxt"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.ttxt"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.ttxt)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.ttxt\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.ttxt\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.itt\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.itt"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.itt"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.itt"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.itt"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.itt)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.itt\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\itt.ico"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.itt\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.lrc\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.lrc"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.lrc"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.lrc"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.lrc"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.lrc)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.lrc\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.lrc\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.vkt\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.vkt"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.vkt"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.vkt"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.vkt"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.vkt)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.vkt\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.vkt\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.scr\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.scr"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\{#ProgId}.scr"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.scr)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.scr\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.scr\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.mpl\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.mpl"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.mpl"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.mpl"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.mpl"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.mpl)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.mpl\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.mpl\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.dfxp\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.dfxp"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.dfxp"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.dfxp"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.dfxp"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.dfxp)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.dfxp\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\dfxp.ico"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.dfxp\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.srt\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.srt"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.srt"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.srt"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.srt"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.srt)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.srt\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\srt.ico"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.srt\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.sbv\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.sbv"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.sbv"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.sbv"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.sbv"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.sbv)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.sbv\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\sbv.ico"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.sbv\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.tero\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.tero"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.tero"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.tero"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.tero"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.tero)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.tero\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.tero\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.tero1\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.tero1"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.tero1"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.tero1"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.tero1"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} project (.tero1)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.tero1\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.tero1\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.vtt\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.vtt"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.vtt"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.vtt"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.vtt"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.vtt)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.vtt\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\vtt.ico"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.vtt\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.webvtt\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}.webvtt"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.webvtt"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}.webvtt"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\{#ProgId}.webvtt"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.webvtt)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}.webvtt\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\vtt.ico"
Root: HKA; Subkey: "Software\Classes\{#ProgId}.webvtt\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""

[Icons]
Name: "{autoprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

[UninstallDelete]
Type: files; Name: "{app}\Tero.cfg"
Type: files; Name: "{app}\Tero.mru"
Type: files; Name: "{app}\yt-dlp.exe"
Type: filesandordirs; Name: "{app}\ytdlp"
Type: filesandordirs; Name: "{app}\PySceneDetect"
Type: filesandordirs; Name: "{app}\Screenshots"
Type: filesandordirs; Name: "{app}\Backup"
Type: filesandordirs; Name: "{app}\ffmpeg"
Type: filesandordirs; Name: "{app}\ShotChanges"
Type: filesandordirs; Name: "{app}\Terminology"
Type: filesandordirs; Name: "{app}\TM"
Type: filesandordirs; Name: "{app}\Waveforms"
Type: filesandordirs; Name: "{app}\Whisper"
Type: filesandordirs; Name: "{app}\Projects"
Type: filesandordirs; Name: "{app}\Tero.act"
Type: filesandordirs; Name: "{app}"

[Code]
// Windows 10 or later runs the current libmpv; Windows 7/8.1 need the "frozen" one.
// The current x64 libmpv also imports vulkan-1.dll (the Vulkan loader, installed with
// the GPU drivers): without it (old PCs, virtual machines, old or missing drivers) it
// doesn't load, so those get the "frozen" one too.
function IsModernWindows: Boolean;
begin
  Result := (GetWindowsVersion >= $0A000000) and FileExists(ExpandConstant('{sys}\vulkan-1.dll'));
  if not Result then
    Log('Using the frozen libmpv: Windows older than 10 or no vulkan-1.dll');
end;

// The ARM64 build, when the installer has it and the machine is ARM64
function UseArm64: Boolean;
begin
#ifdef Arm64Bin
  Result := IsArm64;
#else
  Result := False;
#endif
end;

function HasLegacyLibs: Boolean;
begin
#ifdef WebInstall
  Result := True;
#else
  #ifdef LibsLegacy
  Result := True;
  #else
  Result := False;
  #endif
#endif
end;

function UseX64Legacy: Boolean;
begin
  Result := not UseArm64 and not IsModernWindows and HasLegacyLibs;
end;

function UseX64Modern: Boolean;
begin
  Result := not UseArm64 and not UseX64Legacy;
end;

// Same rule as DicIdOf in the preprocessor
function DicIdOf(B: String): String;
begin
  Result := B;
  StringChangeEx(Result, ' ', '_', True);
  StringChangeEx(Result, '(', '', True);
  StringChangeEx(Result, ')', '', True);
  StringChangeEx(Result, '-', '_', True);
  Result := 'dic\' + Result;
end;

// Dictionary for the Windows display language (en_US when there is none)
function DictionaryForUILanguage: String;
var
  L: Integer;
begin
  L := GetUILanguage;
  case L and $3FF of
    $02: Result := 'bg_BG';
    $09: if L = $0809 then Result := 'en_GB' else Result := 'en_US';
    $0A: Result := 'es_UY';
    $10: Result := 'it_IT';
    $12: Result := 'ko_KR';
    $16: if L = $0816 then Result := 'pt_PT (pos-AO 1990)' else Result := 'pt_BR';
    $19: Result := 'ru_RU';
  else
    Result := 'en_US';
  end;
  Result := DicIdOf(Result);
  if Pos('|' + Result + '|', '{#DicIds}') = 0 then
    Result := DicIdOf('en_US');
  if Pos('|' + Result + '|', '{#DicIds}') = 0 then
    Result := '';
end;

#ifdef WebInstall
var
  DownloadPage: TDownloadWizardPage;
  ExtractionPage: TExtractionWizardPage;
  DlUrls, DlNames, DlShas: TArrayOfString;
#endif

procedure InitializeWizard;
var
  Dic: String;
begin
  // New installs: the dictionary of the Windows language (an upgrade keeps its choice)
  if WizardForm.PrevAppDir = '' then
  begin
    Dic := DictionaryForUILanguage;
    if Dic <> '' then
      WizardSelectComponents(Dic);
  end;
#ifdef WebInstall
  DownloadPage := CreateDownloadPage(SetupMessage(msgWizardPreparing), SetupMessage(msgPreparingDesc), nil);
  DownloadPage.ShowBaseNameInsteadOfUrl := True;
  ExtractionPage := CreateExtractionPage(SetupMessage(msgWizardPreparing), SetupMessage(msgPreparingDesc), nil);
#endif
end;

#ifdef WebInstall
procedure QueueDownload(const Url, BaseName, Sha: String);
var
  N: Integer;
begin
  N := GetArrayLength(DlUrls);
  SetArrayLength(DlUrls, N + 1);
  SetArrayLength(DlNames, N + 1);
  SetArrayLength(DlShas, N + 1);
  DlUrls[N] := Url;
  DlNames[N] := BaseName;
  DlShas[N] := Sha;
end;

procedure QueueDownloads;
begin
  SetArrayLength(DlUrls, 0);
  SetArrayLength(DlNames, 0);
  SetArrayLength(DlShas, 0);
  if UseArm64 then
  begin
  #ifdef Arm64Bin
    QueueDownload('{#MpvArm64Url}', 'libmpv.zip', '{#MpvArm64Sha}');
    QueueDownload('{#TSLibsArm64Url}', 'openssl-hunspell.zip', '{#TSLibsArm64Sha}');
  #endif
  end
  else
  begin
    if IsModernWindows then
      QueueDownload('{#MpvX64Url}', 'libmpv.zip', '{#MpvX64Sha}')
    else
      QueueDownload('{#MpvLegacyUrl}', 'libmpv.zip', '{#MpvLegacySha}');
    QueueDownload('{#TSLibsX64Url}', 'openssl-hunspell.zip', '{#TSLibsX64Sha}');
  end;
  // Dictionaries, from the same commit as this build
#for {FindHandle = FindResult = FindFirst(DicDir + "\*.dic", 0); FindResult; FindResult = FindNext(FindHandle)} DicDownloads
#if FindHandle
  #expr FindClose(FindHandle)
#endif
end;

// One file at a time, so the page says which one ("2 of 5") and a failed one
// (a 504 from the server, a dropped connection) is tried again, up to 4 times
procedure DownloadQueued;
var
  I, Attempt: Integer;
  Done: Boolean;
begin
  for I := 0 to GetArrayLength(DlUrls) - 1 do
  begin
    Attempt := 0;
    Done := False;
    repeat
      Attempt := Attempt + 1;
      DownloadPage.Clear;
      DownloadPage.Add(DlUrls[I], DlNames[I], DlShas[I]);
      if Attempt = 1 then
        DownloadPage.SetText(FmtMessage(CustomMessage('DownloadingFile'), [IntToStr(I + 1), IntToStr(GetArrayLength(DlUrls))]), DlNames[I])
      else
        DownloadPage.SetText(FmtMessage(CustomMessage('DownloadRetry'), [IntToStr(I + 1), IntToStr(GetArrayLength(DlUrls)), IntToStr(Attempt)]), DlNames[I]);
      try
        DownloadPage.Download;
        Done := True;
      except
        Log(Format('Download of %s failed (try %d): %s', [DlUrls[I], Attempt, GetExceptionMessage]));
        if DownloadPage.AbortedByUser or (Attempt >= 4) then
          RaiseException(DlNames[I] + ': ' + GetExceptionMessage);
        Sleep(Attempt * 3000);
      end;
    until Done;
  end;
end;

// Called in silent installs too: downloads, checks the SHA-256 and extracts the libraries
function PrepareToInstall(var NeedsRestart: Boolean): String;
var
  Libs: String;
begin
  Result := '';
  QueueDownloads;
  DownloadPage.Show;
  try
    try
      DownloadQueued;
    except
      Result := AddPeriod(GetExceptionMessage);
      Log('Download failed: ' + Result);
    end;
  finally
    DownloadPage.Hide;
  end;
  if Result <> '' then
    Exit;

  Libs := ExpandConstant('{tmp}\libs');
  ForceDirectories(Libs);
  ExtractionPage.Clear;
  ExtractionPage.Add(ExpandConstant('{tmp}\libmpv.zip'), Libs, False);
  ExtractionPage.Add(ExpandConstant('{tmp}\openssl-hunspell.zip'), Libs, False);
  ExtractionPage.Show;
  try
    try
      ExtractionPage.Extract;
    except
      Result := AddPeriod(GetExceptionMessage);
      Log('Extraction failed: ' + Result);
    end;
  finally
    ExtractionPage.Hide;
  end;
end;
#endif
