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

unit procSingleInstance;

// -----------------------------------------------------------------------------
// [Windows] Only one Tero per user session (as on macOS), unless "Allow
// running Tero more than once" is checked in Settings: starting Tero again, or
// opening a file from Explorer while Tero is running, brings the running Tero
// to the front and opens the file there instead of starting a second copy
// (each copy had its own Welcome window and they shared the settings).
// The command line tools (-convert, -translate, -help) still run on their own.
// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

{ True: go on starting this Tero. False: the running Tero took over, quit }
function SingleInstanceStart: Boolean;

// -----------------------------------------------------------------------------

implementation

{$IFDEF WINDOWS}

uses
  Windows, Classes, SysUtils, Forms, ExtCtrls, XMLConf, procTypes, procFiles,
  procWorkspace, procConfig, formMain, formWelcome, formWizard;

const
  csMutexName     = 'Local\URUWorks.TeroSubtitler.Instance';
  csReceiverClass = 'URUWorks.TeroSubtitler.Instance';
  CopyDataId      = $54455231; // 'TER1'
  HWND_MESSAGE_   = HWND(-3);

type
  TInstanceReceiver = class
    procedure Process(Data: PtrInt);
    procedure RetryTimer(Sender: TObject);
  end;

var
  Mutex: THandle = 0;
  ReceiverWnd: HWND = 0;
  Receiver: TInstanceReceiver = NIL;
  PendingFiles: TStringList = NIL;
  // the files that arrived while a dialog was open: opened when it's closed
  Retry: TTimer = NIL;

function _AllowSetForegroundWindow(dwProcessId: DWORD): BOOL; stdcall;
  external 'user32.dll' name 'AllowSetForegroundWindow';
function _SendMessageTimeoutA(hWnd: HWND; Msg: UINT; wParam: WPARAM;
  lParam: LPARAM; fuFlags, uTimeout: UINT; lpdwResult: PPtrUInt): LRESULT; stdcall;
  external 'user32.dll' name 'SendMessageTimeoutA';

// -----------------------------------------------------------------------------

{ Running Tero }

// -----------------------------------------------------------------------------

procedure TInstanceReceiver.Process(Data: PtrInt);
var
  Target: TForm;
  Wnd: HWND;
  FileName: String;
  WelcomeOpen, CanOpen: Boolean;
begin
  if (frmMain = NIL) or Application.Terminated then Exit;

  // the window the user is working with: at startup the main form is still
  // hidden behind the Welcome window or the Setup Wizard
  if (frmWizard <> NIL) and frmWizard.Visible then
    Target := frmWizard
  else if (frmWelcome <> NIL) and frmWelcome.Visible then
    Target := frmWelcome
  else
    Target := frmMain;

  // to the front when a file arrives (Data = 0), not on every retry (Data = 1)
  if (Data = 0) and Target.HandleAllocated then
  begin
    if IsIconic(Target.Handle) then
      ShowWindow(Target.Handle, SW_RESTORE);
    // a dialog open over it
    Wnd := GetLastActivePopup(Target.Handle);
    if Wnd = 0 then Wnd := Target.Handle;
    SetForegroundWindow(Wnd);
  end;

  WelcomeOpen := (frmWelcome <> NIL) and frmWelcome.Visible;
  // not over the Setup Wizard nor over another dialog (Settings, Batch
  // convert...): the file is not opened, Tero just comes to the front
  CanOpen := not ((frmWizard <> NIL) and frmWizard.Visible) and
    ((WelcomeOpen and (Application.ModalLevel <= 1)) or
     (not WelcomeOpen and (Application.ModalLevel = 0)));

  // over a dialog: they wait (before, they were lost) and Tero tries again
  // every half second until the dialog is closed
  if not CanOpen then
  begin
    for FileName in PendingFiles do
      if FileName <> '' then
      begin
        if (Retry = NIL) then
        begin
          Retry := TTimer.Create(NIL);
          Retry.Interval := 500;
          Retry.OnTimer := @RetryTimer;
        end;
        Retry.Enabled := True;
        Break;
      end;
    Exit;
  end;
  if Retry <> NIL then Retry.Enabled := False;

  while PendingFiles.Count > 0 do
  begin
    FileName := PendingFiles[0];
    PendingFiles.Delete(0);
    if (FileName = '') or
      not (FileExists(FileName) or DirectoryExists(FileName)) then
      Continue;

    // as when a recent file is chosen in the Welcome window
    if (frmWelcome <> NIL) and frmWelcome.Visible then
    begin
      frmWelcome.Hide;
      SetViewMode(vmList);
    end;

    DropFilesProcess([FileName]);

    if frmWelcome <> NIL then
      frmWelcome.Close;
  end;
end;

// -----------------------------------------------------------------------------

procedure TInstanceReceiver.RetryTimer(Sender: TObject);
begin
  Retry.Enabled := False;
  Process(1); // enables it again if the dialog is still open
end;

// -----------------------------------------------------------------------------

function ReceiverWndProc(Wnd: HWND; Msg: UINT; WParam: WPARAM; LParam: LPARAM): LRESULT; stdcall;
var
  cds: PCopyDataStruct;
  s: String;
begin
  if Msg = WM_COPYDATA then
  begin
    cds := PCopyDataStruct(LParam);
    if (cds <> NIL) and (cds^.dwData = CopyDataId) then
    begin
      s := '';
      if (cds^.cbData > 0) and (cds^.lpData <> NIL) then
      begin
        SetLength(s, cds^.cbData);
        Move(cds^.lpData^, s[1], cds^.cbData);
      end;
      PendingFiles.Add(s);
      // processed by the main loop (or by the loop of the Welcome window)
      Application.QueueAsyncCall(@Receiver.Process, 0);
      Exit(1);
    end;
  end;
  Result := DefWindowProcA(Wnd, Msg, WParam, LParam);
end;

// -----------------------------------------------------------------------------

procedure CreateReceiver;
var
  wc: TWndClassA;
begin
  PendingFiles := TStringList.Create;
  Receiver     := TInstanceReceiver.Create;

  FillChar(wc, SizeOf(wc), 0);
  wc.lpfnWndProc   := WNDPROC(@ReceiverWndProc);
  wc.hInstance     := HInstance;
  wc.lpszClassName := csReceiverClass;
  if Windows.RegisterClassA(wc) = 0 then Exit;

  // message-only window: no taskbar button, not listed by EnumWindows
  ReceiverWnd := CreateWindowExA(0, csReceiverClass, NIL, 0, 0, 0, 0, 0,
    HWND_MESSAGE_, 0, HInstance, NIL);
end;

// -----------------------------------------------------------------------------

{ Another Tero }

// -----------------------------------------------------------------------------

function SendToRunningInstance(const FileName: String): Boolean;
var
  Wnd: HWND;
  Deadline: QWord;
  pid: DWORD;
  cds: TCopyDataStruct;
  res: PtrUInt;
begin
  Result := False;
  // the running Tero creates its window right after the mutex, but it may
  // have been started a moment ago
  Deadline := GetTickCount64 + 10000;
  repeat
    Wnd := FindWindowExA(HWND_MESSAGE_, 0, csReceiverClass, NIL);
    if Wnd <> 0 then Break;
    Sleep(100);
  until GetTickCount64 > Deadline;
  if Wnd = 0 then Exit;

  // we are the process the user just started, so we can let the running
  // Tero take the foreground
  pid := 0;
  GetWindowThreadProcessId(Wnd, @pid);
  if pid <> 0 then _AllowSetForegroundWindow(pid);

  cds.dwData := CopyDataId;
  cds.cbData := Length(FileName);
  if FileName <> '' then
    cds.lpData := PChar(FileName)
  else
    cds.lpData := NIL;

  res := 0;
  // a hung Tero (a crash dialog...) doesn't block this one forever
  Result := _SendMessageTimeoutA(Wnd, WM_COPYDATA, 0, LPARAM(@cds),
    SMTO_ABORTIFHUNG, 5000, @res) <> 0;
  Result := Result and (res <> 0);
end;

// -----------------------------------------------------------------------------

// the settings are loaded later (main form): read only this option
function MultipleInstancesAllowed: Boolean;
begin
  Result := AppOptions.AllowMultipleInstances; // default value
  if not FileExists(SettingsFileName) then Exit;
  try
    with TXMLConfig.Create(NIL) do
    try
      FileName := SettingsFileName;
      Result := GetValue('Settings/AllowMultipleInstances', Result);
    finally
      Free;
    end;
  except
    // unreadable settings: as by default
  end;
end;

// -----------------------------------------------------------------------------

function SingleInstanceStart: Boolean;
var
  i: Integer;
  Files: TStringList;
begin
  Result := True;
  // command line tools (-convert, -translate, -help) run on their own
  if (ParamCount > 0) and not (FileExists(ParamStr(1)) or DirectoryExists(ParamStr(1))) then
    Exit;
  if MultipleInstancesAllowed then
    Exit;

  Mutex := CreateMutexA(NIL, False, csMutexName);
  if Mutex = 0 then Exit;

  if GetLastError <> ERROR_ALREADY_EXISTS then
  begin
    // the first Tero
    CreateReceiver;
    Exit;
  end;

  CloseHandle(Mutex);
  Mutex := 0;

  // all the files (before, only the first one: the others of a selection
  // opened from Explorer were lost)
  Files := TStringList.Create;
  try
    for i := 1 to ParamCount do
      if FileExists(ParamStr(i)) or DirectoryExists(ParamStr(i)) then
        Files.Add(ExpandFileName(ParamStr(i)));
    if Files.Count = 0 then Files.Add(''); // only to the front

    // if the running Tero doesn't answer, this one starts as before. If it
    // takes the first file, it takes the others (a later one that fails is
    // only that file)
    Result := not SendToRunningInstance(Files[0]);
    if not Result then
      for i := 1 to Files.Count-1 do
        SendToRunningInstance(Files[i]);
  finally
    Files.Free;
  end;
end;

// -----------------------------------------------------------------------------

finalization
  Retry.Free;
  if ReceiverWnd <> 0 then DestroyWindow(ReceiverWnd);
  if Mutex <> 0 then CloseHandle(Mutex);
  Receiver.Free;
  PendingFiles.Free;

{$ELSE}

function SingleInstanceStart: Boolean;
begin
  Result := True;
end;

{$ENDIF}

// -----------------------------------------------------------------------------

end.
