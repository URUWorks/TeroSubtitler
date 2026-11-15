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
// the executable has no line info. Also a few lines of the player state.
// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms;

procedure InstallCrashLog;
function CrashLogFileName: String;

// -----------------------------------------------------------------------------

implementation

uses
  {$IFDEF UNIX}dl,{$ENDIF} MPVPlayer, formMain;

type
  TCrashLogHandler = class
    procedure AppException(Sender: TObject; E: Exception);
  end;

var
  Handler: TCrashLogHandler = NIL;

// -----------------------------------------------------------------------------

function CrashLogFileName: String;
begin
  {$IFDEF DARWIN}
  Result := ConcatPaths([GetUserDir, 'Library', 'Logs', 'TeroSubtitler-crash.log']);
  {$ELSE}
  Result := ConcatPaths([GetTempDir, 'TeroSubtitler-crash.log']);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function DescribeAddr(const Addr: CodePointer): String;
{$IFDEF UNIX}
var
  Info: dl_info;
{$ENDIF}
begin
  Result := Format('$%p', [Pointer(Addr)]);
  {$IFDEF UNIX}
  FillChar(Info, SizeOf(Info), 0);
  if (Addr <> NIL) and (dladdr(Pointer(Addr), @Info) <> 0) then
  begin
    if Info.dli_fname <> NIL then
      Result += '  ' + ExtractFileName(StrPas(Info.dli_fname)) +
        Format(' (base $%p, +$%x)', [Info.dli_fbase, PtrUInt(Addr) - PtrUInt(Info.dli_fbase)]);
    if Info.dli_sname <> NIL then
      Result += '  ' + StrPas(Info.dli_sname);
  end;
  {$ENDIF}
  // file:line when the executable has debug info (-gl)
  try
    Result += '  ' + Trim(BackTraceStrFunc(Addr));
  except
  end;
end;

// -----------------------------------------------------------------------------

procedure TCrashLogHandler.AppException(Sender: TObject; E: Exception);
var
  SL: TStringList;
  Frames: PCodePointer;
  i: Integer;
  LogName: String;
begin
  try
    SL := TStringList.Create;
    try
      LogName := CrashLogFileName;
      if FileExists(LogName) then
        SL.LoadFromFile(LogName);
      if SL.Count > 2000 then SL.Clear; // don't grow forever

      SL.Add('==== ' + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ' ====');
      SL.Add('Exception: ' + E.ClassName + ': ' + E.Message);
      if Sender <> NIL then
        SL.Add('Sender: ' + Sender.ClassName);
      SL.Add('Address: ' + DescribeAddr(ExceptAddr));
      SL.Add('Stack:');
      Frames := ExceptFrames;
      for i := 0 to ExceptFrameCount - 1 do
        SL.Add('  #' + IntToStr(i) + ' ' + DescribeAddr(Frames[i]));

      // a little of the state, to compare with the steps
      try
        if Assigned(frmMain) and Assigned(frmMain.MPV) then
          with frmMain.MPV do
          begin
            SL.Add('Media: ' + FileName);
            SL.Add(Format('Crop: %d,%d,%d,%d  Video: %dx%d  Pos: %d ms',
              [GetVideoCrop.Left, GetVideoCrop.Top, GetVideoCrop.Right, GetVideoCrop.Bottom,
               GetVideoWidth, GetVideoHeight, GetMediaPosInMs]));
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
    Handler := TCrashLogHandler.Create;

  Application.OnException := @Handler.AppException;
end;

// -----------------------------------------------------------------------------

finalization
  FreeAndNil(Handler);

// -----------------------------------------------------------------------------

end.
