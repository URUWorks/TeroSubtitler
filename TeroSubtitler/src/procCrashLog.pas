{*
 *  URUWorks Tero Subtitler
 *
 *  The contents of this file are used with permission, subject to
 *  the Mozilla Public License Version 2.0 (the "License"); you may
 *  not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/2.0.html
 *
 *  Software distributed under the License is distributed on an
 *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing
 *  rights and limitations under the License.
 *
 *  Copyright (C) 2023-2026 URUWorks, uruworks@gmail.com.
 *}

unit procCrashLog;

// -----------------------------------------------------------------------------
// DIAGNOSTIC: every unhandled exception of the main thread (the
// "Access violation. Press OK to ignore..." dialog) is written, with its stack,
// to a log file before the usual dialog is shown:
//   macOS   : ~/Library/Logs/TeroSubtitler-crash.log
//   others  : <temp folder>/TeroSubtitler-crash.log
// Each frame has its address, the library it belongs to (tero, libmpv, AppKit...)
// and the offset inside it, so it can be resolved with atos/addr2line even if
// the executable has no line info. Also what is needed to reproduce it: the
// versions (Tero, OS, LCL, libmpv), the form/control with the focus, the
// forms closed just before (the access violations of LCL Cocoa on a freed
// form come seconds after closing it) and the player state.
// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}
{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms;

procedure InstallCrashLog;
function CrashLogFileName: String;
// what is needed in a bug report (Help > Copy diagnostic information)
function DiagnosticInfo: String;

// -----------------------------------------------------------------------------

implementation

uses
  {$IFDEF UNIX}dl,{$ENDIF} {$IFDEF DARWIN}CocoaAll,{$ENDIF}
  Controls, InterfaceBase, LCLPlatformDef, LCLVersion, fileinfo,
  winpeimagereader, elfreader, machoreader, MPVPlayer, formMain, procTypes,
  procConfig;

const
  MaxLogLines    = 2000; // the oldest reports are removed beyond this
  MaxClosedForms = 5;    // forms closed just before the exception

type
  TCrashLogHandler = class
    procedure AppException(Sender: TObject; E: Exception);
    procedure RemoveForm(Sender: TObject; AForm: TCustomForm);
  end;

  TClosedForm = record
    Name : String;
    Tick : QWord;
  end;

var
  Handler     : TCrashLogHandler = NIL;
  SystemInfo  : String = ''; // the versions, the same for every report
  ClosedForms : array[0..MaxClosedForms-1] of TClosedForm;
  ClosedCount : Integer = 0;

// -----------------------------------------------------------------------------

function CrashLogFileName: String;
begin
  {$IFDEF DARWIN}
  Result := ConcatPaths([GetUserDir, 'Library', 'Logs', 'TeroSubtitler' + ChannelSuffix + '-crash.log']);
  {$ELSE}
  Result := ConcatPaths([GetTempDir, 'TeroSubtitler' + ChannelSuffix + '-crash.log']);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function GetProgramVersion: String;
var
  FileVerInfo: TFileVersionInfo;
begin
  Result := '?';
  try
    FileVerInfo := TFileVersionInfo.Create(NIL);
    try
      FileVerInfo.ReadFileInfo;
      Result := FileVerInfo.VersionStrings.Values['FileVersion'];
    finally
      FileVerInfo.Free;
    end;
  except
  end;
end;

// -----------------------------------------------------------------------------

function GetOSDescription: String;
{$IFDEF LINUX}
var
  SL: TStringList;
{$ENDIF}
begin
  Result := '';
  try
    {$IF DEFINED(DARWIN)}
    Result := 'macOS ' + NSProcessInfo.processInfo.operatingSystemVersionString.UTF8String;
    {$ELSEIF DEFINED(WINDOWS)}
    Result := Format('Windows %d.%d.%d', [Win32MajorVersion, Win32MinorVersion, Win32BuildNumber]);
    {$ELSEIF DEFINED(LINUX)}
    SL := TStringList.Create;
    try
      if FileExists('/etc/os-release') then
      begin
        SL.LoadFromFile('/etc/os-release');
        Result := SL.Values['PRETTY_NAME'].DeQuotedString('"');
      end;
    finally
      SL.Free;
    end;
    if Result = '' then Result := 'Linux';
    {$ELSE}
    Result := {$I %FPCTARGETOS%};
    {$ENDIF}
  except
  end;
  Result += ' (' + {$I %FPCTARGETCPU%} + ')';
end;

// -----------------------------------------------------------------------------

function GetSystemInfo: String;
begin
  Result := 'Tero ' + GetProgramVersion + '  ' + GetOSDescription;
  try
    if Assigned(WidgetSet) then
      Result += '  ' + LCLPlatformDisplayNames[WidgetSet.LCLPlatform];
  except
  end;
  Result += '  LCL ' + lcl_version + '  FPC ' + {$I %FPCVERSION%};
  try
    if Assigned(frmMain) and Assigned(frmMain.MPV) then
      Result += '  ' + frmMain.MPV.GetVersionString;
  except
  end;
  {$IFDEF DARWIN}
  // the access violations of LCL Cocoa scroll bars happen only with legacy ones
  try
    if NSScroller.preferredScrollerStyle = NSScrollerStyleLegacy then
      Result += '  scroll bars: legacy'
    else
      Result += '  scroll bars: overlay';
  except
  end;
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function DescribeAddr(const Addr: CodePointer): String;
{$IFDEF UNIX}
var
  Info: dl_info;
{$ENDIF}
var
  s: String;
  p: Integer;
begin
  Result := Format('$%p', [Pointer(Addr)]);
  {$IFDEF UNIX}
  FillChar(Info, SizeOf(Info), 0);
  if (Addr <> NIL) and (dladdr(Pointer(Addr), @Info) <> 0) then
  begin
    if Info.dli_fname <> NIL then
      Result += '  ' + ExtractFileName(StrPas(Info.dli_fname)) +
        Format(' (+$%x)', [PtrUInt(Addr) - PtrUInt(Info.dli_fbase)]);
    if Info.dli_sname <> NIL then
      Result += '  ' + StrPas(Info.dli_sname);
  end;
  {$ENDIF}
  // file:line when the executable has debug info (-gl). Without it, it's only
  // the address again: not repeated
  try
    s := Trim(BackTraceStrFunc(Addr));
    if s.StartsWith('$') then
    begin
      p := Pos(' ', s);
      if p > 0 then
        s := Trim(Copy(s, p+1, MaxInt))
      else
        s := '';
    end;
    if s <> '' then Result += '  ' + s;
  except
  end;
end;

// -----------------------------------------------------------------------------

function MSToStr(const AMS: Int64): String;
begin
  Result := Format('%.2d:%.2d:%.2d.%.3d', [AMS div 3600000, (AMS div 60000) mod 60,
    (AMS div 1000) mod 60, AMS mod 1000]);
end;

// -----------------------------------------------------------------------------

function DescribeComponent(const AComponent: TComponent): String;
begin
  if AComponent = NIL then
    Result := '-'
  else if AComponent.Name <> '' then
    Result := AComponent.Name + ': ' + AComponent.ClassName
  else
    Result := AComponent.ClassName;
end;

// -----------------------------------------------------------------------------

// the oldest reports go, the file keeps the last ones (instead of emptying it)
procedure TrimLog(const SL: TStringList);
begin
  if SL.Count <= MaxLogLines then Exit;
  while (SL.Count > MaxLogLines div 2) do
    SL.Delete(0);
  while (SL.Count > 0) and not SL[0].StartsWith('==== ') do
    SL.Delete(0);
end;

// -----------------------------------------------------------------------------

procedure TCrashLogHandler.RemoveForm(Sender: TObject; AForm: TCustomForm);
var
  i: Integer;
begin
  if AForm = NIL then Exit;
  if ClosedCount = MaxClosedForms then
  begin
    for i := 1 to MaxClosedForms-1 do
      ClosedForms[i-1] := ClosedForms[i];
    Dec(ClosedCount);
  end;
  ClosedForms[ClosedCount].Name := DescribeComponent(AForm);
  ClosedForms[ClosedCount].Tick := GetTickCount64;
  Inc(ClosedCount);
end;

// -----------------------------------------------------------------------------

function DiagnosticInfo: String;

  function FileState(const AFileName: String): String;
  begin
    if AFileName = '' then
      Result := '(none)'
    else if FileExists(AFileName) then
      Result := AFileName
    else
      Result := AFileName + ' (not found)';
  end;

var
  SL: TStringList;
  Dir: String;
begin
  SL := TStringList.Create;
  try
    SL.Add(GetSystemInfo);
    SL.Add('Language: ' + AppOptions.GUILanguage + '  Dictionary: ' + AppOptions.HunspellLanguage);
    SL.Add('Settings: ' + FileState(SettingsFileName));
    SL.Add('Profiles: ' + FileState(ConventionsFileName));
    SL.Add('Crash log: ' + FileState(CrashLogFileName));
    SL.Add('libmpv: ' + FileState(libMPVFileName));
    SL.Add('yt-dlp: ' + FileState(Tools.YTDLP));
    SL.Add('Deno: ' + FileState(DenoFileName));
    SL.Add('FFmpeg: ' + FileState(Tools.FFmpeg));
    SL.Add('whisper.cpp: ' + FileState(Tools.WhisperCPP));
    SL.Add('Faster-Whisper: ' + FileState(Tools.FasterWhisper));
    SL.Add('PySceneDetect: ' + FileState(Tools.PySceneDetect));
    SL.Add('Tesseract: ' + FileState(Tools.Tesseract));
    for Dir in DictionaryFolders do
      SL.Add('Dictionaries: ' + Dir + BoolToStr(DirectoryExists(Dir), '', ' (not found)'));
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TCrashLogHandler.AppException(Sender: TObject; E: Exception);
var
  SL: TStringList;
  Frames: PCodePointer;
  i: Integer;
  LogName, s: String;
  Now64: QWord;
begin
  try
    SL := TStringList.Create;
    try
      LogName := CrashLogFileName;
      if FileExists(LogName) then
        SL.LoadFromFile(LogName);
      TrimLog(SL);

      if SystemInfo = '' then
        SystemInfo := GetSystemInfo;

      SL.Add('==== ' + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ' ====');
      SL.Add(SystemInfo);
      SL.Add('Exception: ' + E.ClassName + ': ' + E.Message);
      // always TApplication for the exceptions of the message loop
      if (Sender <> NIL) and not (Sender is TApplication) then
        SL.Add('Sender: ' + DescribeComponent(TComponent(Sender)));
      SL.Add('Address: ' + DescribeAddr(ExceptAddr));
      SL.Add('Stack:');
      Frames := ExceptFrames;
      for i := 0 to ExceptFrameCount - 1 do
        SL.Add('  #' + IntToStr(i) + ' ' + DescribeAddr(Frames[i]));

      // where the user was
      try
        SL.Add('Form: ' + DescribeComponent(Screen.ActiveCustomForm) +
          '  Control: ' + DescribeComponent(Screen.ActiveControl));
      except
      end;

      // forms closed just before: a queued call on a freed form fails later
      if ClosedCount > 0 then
      begin
        Now64 := GetTickCount64;
        s := '';
        for i := ClosedCount-1 downto 0 do
        begin
          if s <> '' then s += ', ';
          s += Format('%s (%.1f s ago)', [ClosedForms[i].Name, (Now64 - ClosedForms[i].Tick) / 1000]);
        end;
        SL.Add('Closed: ' + s);
      end;

      // the player, to compare with the steps
      try
        if Assigned(frmMain) and Assigned(frmMain.MPV) and frmMain.MPV.IsMediaLoaded then
          with frmMain.MPV do
          begin
            SL.Add(Format('Media: %s  (%dx%d, %s)',
              [FileName, GetVideoWidth, GetVideoHeight, MSToStr(GetMediaPosInMs)]));
            // only when the Auto-crop is on: 0,0,0,0 said nothing
            if IsVideoCropped then
              with GetVideoCrop do
                SL.Add(Format('Crop: %d,%d,%d,%d', [Left, Top, Right, Bottom]));
          end;
      except
      end;
      SL.Add('');
      SL.SaveToFile(LogName);
    finally
      SL.Free;
    end;
  except
    // the log must never be a new problem
  end;

  // the usual dialog
  Application.ShowException(E);
end;

// -----------------------------------------------------------------------------

procedure InstallCrashLog;
begin
  if Handler = NIL then
  begin
    Handler := TCrashLogHandler.Create;
    Screen.AddHandlerRemoveForm(@Handler.RemoveForm);
  end;

  Application.OnException := @Handler.AppException;
end;

// -----------------------------------------------------------------------------

finalization
  if Handler <> NIL then
  begin
    if Assigned(Screen) then
      Screen.RemoveHandlerRemoveForm(@Handler.RemoveForm);
    FreeAndNil(Handler);
  end;

// -----------------------------------------------------------------------------

end.
