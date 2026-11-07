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

#define MyAppName              "Tero Subtitler"
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
    DownloadPage.Add('{#DicUrlOf(DicBase)}.aff', 'dic-{#DicBase}.aff', '');
    DownloadPage.Add('{#DicUrlOf(DicBase)}.dic', 'dic-{#DicBase}.dic', '');
  end;
#endsub
#endif

[Setup]
; NOTE: The value of AppId uniquely identifies this application. Do not use the same AppId value in installers for other applications.
AppId={{216B2DAF-093C-4C88-9C82-E34CE46580E5}
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
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
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

[Languages]
Name: "english";             MessagesFile: "compiler:Default.isl"
Name: "spanish";             MessagesFile: "compiler:Languages\Spanish.isl"
Name: "brazilianportuguese"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"
Name: "bulgarian";           MessagesFile: "compiler:Languages\Bulgarian.isl"
Name: "german";              MessagesFile: "compiler:Languages\German.isl"
Name: "italian";             MessagesFile: "compiler:Languages\Italian.isl"
Name: "russian";             MessagesFile: "compiler:Languages\Russian.isl"
Name: "slovenian";           MessagesFile: "compiler:Languages\Slovenian.isl"
Name: "turkish";             MessagesFile: "compiler:Languages\Turkish.isl"

[CustomMessages]
Dictionaries=Spell checking dictionaries
OtherTasks=Other tasks:
AssocSubtitles=Associate subtitle files with %1
spanish.Dictionaries=Diccionarios ortográficos
spanish.OtherTasks=Otras tareas:
spanish.AssocSubtitles=Asociar los archivos de subtítulos con %1
brazilianportuguese.Dictionaries=Dicionários de verificação ortográfica
brazilianportuguese.OtherTasks=Outras tarefas:
brazilianportuguese.AssocSubtitles=Associar os arquivos de legenda ao %1
bulgarian.Dictionaries=Речници за проверка на правописа
bulgarian.OtherTasks=Други задачи:
bulgarian.AssocSubtitles=Асоцииране на файловете със субтитри с %1
german.Dictionaries=Wörterbücher für die Rechtschreibprüfung
german.OtherTasks=Weitere Aufgaben:
german.AssocSubtitles=Untertiteldateien mit %1 verknüpfen
italian.Dictionaries=Dizionari per il controllo ortografico
italian.OtherTasks=Altre attività:
italian.AssocSubtitles=Associa i file di sottotitoli a %1
russian.Dictionaries=Словари для проверки орфографии
russian.OtherTasks=Другие задачи:
russian.AssocSubtitles=Связать файлы субтитров с %1
slovenian.Dictionaries=Slovarji za preverjanje črkovanja
slovenian.OtherTasks=Druga opravila:
slovenian.AssocSubtitles=Poveži datoteke podnapisov s programom %1
turkish.Dictionaries=Yazım denetimi sözlükleri
turkish.OtherTasks=Diğer görevler:
turkish.AssocSubtitles=Altyazı dosyalarını %1 ile ilişkilendir

[Types]
Name: "compact"; Description: "{cm:CompactInstallation}"
Name: "full";    Description: "{cm:FullInstallation}"
Name: "custom";  Description: "{cm:CustomInstallation}"; Flags: iscustom

[Components]
Name: "main"; Description: "{#MyAppName}"; Types: compact full custom; Flags: fixed
Name: "dic";  Description: "{cm:Dictionaries}"; Types: full
#for {FindHandle = FindResult = FindFirst(DicDir + "\*.dic", 0); FindResult; FindResult = FindNext(FindHandle)} DicComponent
#if FindHandle
  #expr FindClose(FindHandle)
#endif

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "fileassoc";   Description: "{cm:AssocSubtitles,{#MyAppName}}"; GroupDescription: "{cm:OtherTasks}"

[Dirs]
Name: "{app}"; Permissions: everyone-full

[InstallDelete]
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
Source: "{#SourceBin}\tero.cfs"; DestDir: "{app}"; Flags: ignoreversion
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
Root: HKA; Subkey: "Software\Classes\.ass\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.ass"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.ass"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.ass"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.ass"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.ass)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.ass\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\ass.ico"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.ass\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.ssa\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.ssa"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.ssa"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.ssa"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.ssa"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.ssa)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.ssa\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\ssa.ico"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.ssa\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.xas\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.xas"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.xas"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.xas"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.xas"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.xas)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.xas\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\xas.ico"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.xas\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.aqt\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.aqt"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.aqt"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.aqt"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.aqt"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.aqt)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.aqt\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\aqt.ico"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.aqt\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.890\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.890"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.890"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.890"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.890"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.890)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.890\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\890.ico"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.890\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.asc\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.asc"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.asc"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.asc"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.asc"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.asc)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.asc\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\asc.ico"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.asc\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.cap\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.cap"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.cap"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.cap"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.cap"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.cap)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.cap\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.cap\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.dks\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.dks"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.dks"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.dks"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.dks"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.dks)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.dks\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.dks\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.dtc\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.dtc"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.dtc"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.dtc"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.dtc"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.dtc)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.dtc\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.dtc\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.sub\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.sub"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.sub"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.sub"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.sub"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.sub)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.sub\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\sub.ico"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.sub\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.stl\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.stl"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.stl"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.stl"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.stl"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.stl)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.stl\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\stl.ico"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.stl\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.ttxt\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.ttxt"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.ttxt"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.ttxt"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.ttxt"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.ttxt)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.ttxt\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.ttxt\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.itt\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.itt"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.itt"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.itt"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.itt"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.itt)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.itt\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\itt.ico"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.itt\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.lrc\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.lrc"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.lrc"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.lrc"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.lrc"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.lrc)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.lrc\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.lrc\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.vkt\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.vkt"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.vkt"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.vkt"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.vkt"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.vkt)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.vkt\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.vkt\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.scr\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.scr"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.scr"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.scr)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.scr\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.scr\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.mpl\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.mpl"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.mpl"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.mpl"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.mpl"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.mpl)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.mpl\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.mpl\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.dfxp\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.dfxp"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.dfxp"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.dfxp"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.dfxp"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.dfxp)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.dfxp\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\dfxp.ico"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.dfxp\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.srt\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.srt"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.srt"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.srt"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.srt"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.srt)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.srt\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\srt.ico"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.srt\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.sbv\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.sbv"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.sbv"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.sbv"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.sbv"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.sbv)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.sbv\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\sbv.ico"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.sbv\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.tero\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.tero"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.tero"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.tero"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.tero"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.tero)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.tero\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.tero\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.vtt\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.vtt"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.vtt"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.vtt"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.vtt"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.vtt)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.vtt\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\vtt.ico"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.vtt\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.webvtt\OpenWithProgids"; ValueType: string; ValueName: "TeroSubtitler.webvtt"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.webvtt"; ValueType: string; ValueName: ""; ValueData: "TeroSubtitler.webvtt"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.webvtt"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName} (.webvtt)"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.webvtt\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Icons\vtt.ico"
Root: HKA; Subkey: "Software\Classes\TeroSubtitler.webvtt\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\{cm:ProgramOnTheWeb,{#MyAppName}}"; Filename: "{#MyAppURL}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
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
// Windows 10 or later runs the current libmpv; Windows 7/8.1 need the "frozen" one
function IsModernWindows: Boolean;
begin
  Result := GetWindowsVersion >= $0A000000;
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
#endif
end;

#ifdef WebInstall
procedure AddDownloads;
begin
  if UseArm64 then
  begin
  #ifdef Arm64Bin
    DownloadPage.Add('{#MpvArm64Url}', 'libmpv.zip', '{#MpvArm64Sha}');
    DownloadPage.Add('{#TSLibsArm64Url}', 'tslibs.zip', '{#TSLibsArm64Sha}');
  #endif
  end
  else
  begin
    if IsModernWindows then
      DownloadPage.Add('{#MpvX64Url}', 'libmpv.zip', '{#MpvX64Sha}')
    else
      DownloadPage.Add('{#MpvLegacyUrl}', 'libmpv.zip', '{#MpvLegacySha}');
    DownloadPage.Add('{#TSLibsX64Url}', 'tslibs.zip', '{#TSLibsX64Sha}');
  end;
  // Dictionaries, from the same commit as this build
#for {FindHandle = FindResult = FindFirst(DicDir + "\*.dic", 0); FindResult; FindResult = FindNext(FindHandle)} DicDownloads
#if FindHandle
  #expr FindClose(FindHandle)
#endif
end;

// Called in silent installs too: downloads, checks the SHA-256 and extracts the libraries
function PrepareToInstall(var NeedsRestart: Boolean): String;
var
  Libs: String;
begin
  Result := '';
  DownloadPage.Clear;
  AddDownloads;
  DownloadPage.Show;
  try
    try
      DownloadPage.Download;
      Libs := ExpandConstant('{tmp}\libs');
      ForceDirectories(Libs);
      ExtractArchive(ExpandConstant('{tmp}\libmpv.zip'), Libs, '', False, nil);
      ExtractArchive(ExpandConstant('{tmp}\tslibs.zip'), Libs, '', False, nil);
    except
      Result := AddPeriod(GetExceptionMessage);
      Log('Download or extraction failed: ' + Result);
    end;
  finally
    DownloadPage.Hide;
  end;
end;
#endif
