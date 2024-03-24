{*
 *  URUWorks Lazarus Process
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
 *  Copyright (C) 2001-2023 URUWorks, uruworks@gmail.com.
 *}

unit UWSystem.ThreadProcess;

// -----------------------------------------------------------------------------

interface

uses
  Forms, Classes, SysUtils, Process;

type

  TOnDataReceived = procedure(Output: String; var ATerminate: Boolean);
  TOnTimeElapsed  = procedure(const ATime: Double);

  TUWThreadProcess = class(TThread)
  private
    FFileName  : String;
    FParams    : TStringArray;
    FOnProcess : TOnDataReceived;
    procedure DataReceived;
  protected
    procedure Execute; override;
  public
    TerminateProcess : Boolean;
    OutputString     : String;
    Environment      : TStringArray;
    TimeElapsed      : Double;
    constructor Create(const AFileName: String; const AParams: TStringArray; AOnDataReceived: TOnDataReceived = NIL; AOnDone: TNotifyEvent = NIL);
    property Terminated;
  end;

function ExecuteThreadProcess(const AAppFileName: String; const AParams: TStringArray; const AOnDataReceived: TOnDataReceived = NIL; const AOnTimeElapsed: TOnTimeElapsed = NIL): Boolean; overload;
function ExecuteThreadProcess(const AAppFileName: String; const AParams: TStringArray; const AEnvironment: TStringArray; const AOnDataReceived: TOnDataReceived = NIL; const AOnTimeElapsed: TOnTimeElapsed = NIL): Boolean; overload;

// -----------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

constructor TUWThreadProcess.Create(const AFileName: String; const AParams: TStringArray; AOnDataReceived: TOnDataReceived = NIL; AOnDone: TNotifyEvent = NIL);
begin
  inherited Create(True);

  FFileName        := AFileName;
  FParams          := AParams;
  TerminateProcess := False;
  FOnProcess       := AOnDataReceived;
  OnTerminate      := AOnDone;
  OutputString     := '';
  TimeElapsed      := 0;
  FreeOnTerminate  := True;
  SetLength(Environment, 0);
end;

// -----------------------------------------------------------------------------

procedure TUWThreadProcess.Execute;
const
  BUF_SIZE = 2048; // Buffer size for reading the output in chunks

var
  AProcess  : TProcess;
  BytesRead : LongInt;
  Buffer    : array[1..BUF_SIZE] of Byte;
  i         : Integer;
begin
  AProcess := TProcess.Create(NIL);
  try
    // Set up the process
    AProcess.ShowWindow := swoHide;
    AProcess.Executable := FFileName;

    // Process option poUsePipes has to be used so the output can be captured.
    // Process option poWaitOnExit can not be used because that would block
    // this program, preventing it from reading the output data of the process.
    AProcess.Options := [poUsePipes, poStderrToOutPut];

    // Params
    for i := 0 to High(FParams) do
      AProcess.Parameters.Add(FParams[i]);

    // Environment
    for i := 1 to GetEnvironmentVariableCount do
      AProcess.Environment.Add(GetEnvironmentString(i));

    if Length(Environment) > 0 then
      for i := 0 to High(Environment) do
        if Environment[i] <> '' then
          AProcess.Environment.Add(Environment[i]);

    try
      // Start the process
      AProcess.Execute;

      // All generated output from AProcess is read in a loop until no more data is available
      while AProcess.Running and not TerminateProcess do
      begin
        // Get the new data from the process to a maximum of the buffer size that was allocated.
        // Note that all read(...) calls will block except for the last one, which returns 0 (zero).
        BytesRead := AProcess.Output.Read(Buffer, BUF_SIZE);
        SetLength(OutputString, BytesRead);
        if BytesRead > 0 then
          Move(Buffer[1], OutputString[1], BytesRead);

        if Assigned(FOnProcess) then
          Synchronize(@DataReceived);

        if BytesRead = 0 then Break; // Stop if no more data is available
      end;
    except
      on e: Exception do
      begin
      end;
    end;
  finally
    // The process has finished so it can be cleaned up
    AProcess.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWThreadProcess.DataReceived;
begin
  if Assigned(FOnProcess) then
    FOnProcess(OutputString, TerminateProcess);
end;

// -----------------------------------------------------------------------------

function ExecuteThreadProcess(const AAppFileName: String; const AParams: TStringArray; const AOnDataReceived: TOnDataReceived = NIL; const AOnTimeElapsed: TOnTimeElapsed = NIL): Boolean;
begin
  Result := False;
  with TUWThreadProcess.Create(AAppFileName, AParams, AOnDataReceived) do
  begin
    TimeElapsed := 0;
    Start;
    while not Finished do
    begin
      Application.ProcessMessages;
      Sleep(100);
      TimeElapsed += 0.1;
      if Assigned(AOnTimeElapsed) then
        AOnTimeElapsed(TimeElapsed);
    end;
    Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function ExecuteThreadProcess(const AAppFileName: String; const AParams: TStringArray; const AEnvironment: TStringArray; const AOnDataReceived: TOnDataReceived = NIL; const AOnTimeElapsed: TOnTimeElapsed = NIL): Boolean;
begin
  Result := False;
  with TUWThreadProcess.Create(AAppFileName, AParams, AOnDataReceived) do
  begin
    Environment := AEnvironment;
    TimeElapsed := 0;
    Start;
    while not Finished do
    begin
      Application.ProcessMessages;
      Sleep(100);
      TimeElapsed += 0.1;
      if Assigned(AOnTimeElapsed) then
        AOnTimeElapsed(TimeElapsed);
    end;
    Result := True;
  end;
end;

// -----------------------------------------------------------------------------

end.
