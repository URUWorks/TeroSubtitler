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

unit UWSystem.Process;

// -----------------------------------------------------------------------------

interface

uses
  Forms, Classes, SysUtils, Process;

type

  TUWProcessCB   = procedure(const TimeElapsed: Double; var Cancel: Boolean);
  TUWProcessCBEx = procedure(Output: String; var Cancel: Boolean);

function ExecuteApp(const AAppFileName: String; const AParams: TStringArray; const AHidden: Boolean = True; const AWaitOnExit: Boolean = True; const ACB: TUWProcessCB = NIL): Boolean;
function ExecuteAppEx(const AAppFileName: String; const AParams: TStringArray; const ACB: TUWProcessCBEx = NIL): Boolean;

function ExecuteAppLoop(const AAppFileName: String; const AParams: TStringArray; var Output: TStringList; const ACB: TOnRunCommandEvent = NIL): Boolean;

// -----------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

function ExecuteApp(const AAppFileName: String; const AParams: TStringArray; const AHidden: Boolean = True; const AWaitOnExit: Boolean = True; const ACB: TUWProcessCB = NIL): Boolean;
var
  AProcess: TProcess;
  ACancel: Boolean;
  ATimeElapsed: Double;
  i: Integer;
begin
  Result := False;
  AProcess := TProcess.Create(NIL);
  try
    if AHidden then AProcess.ShowWindow := swoHide;
    AProcess.Executable := AAppFileName;

    for i := 0 to High(AParams) do
      AProcess.Parameters.Add(AParams[i]);

    try
      AProcess.Execute;

      if AWaitOnExit then // wait for the launched application to finish
      begin
        ACancel      := False;
        ATimeElapsed := 0;
        while AProcess.Running do
        begin
          Application.ProcessMessages;
          Sleep(100);
          ATimeElapsed += 0.1;
          if Assigned(ACB) then
          begin
            ACB(ATimeElapsed, ACancel);
            Application.ProcessMessages;
            if ACancel then AProcess.Terminate(-1);
          end;
        end;
      end;
      Result := True;
    except
      on e: Exception do
      begin
        Result := False;
      end;
    end;
  finally
    AProcess.Free;
  end;
end;

// -----------------------------------------------------------------------------

function ExecuteAppEx(const AAppFileName: String; const AParams: TStringArray; const ACB: TUWProcessCBEx = NIL): Boolean;
const
  BUF_SIZE = 2048; // Buffer size for reading the output in chunks

var
  AProcess  : TProcess;
  BytesRead : LongInt;
  Buffer    : array[1..BUF_SIZE] of Byte;
  ACancel   : Boolean;
  i         : Integer;
  s         : String;
begin
  Result := False;
  AProcess := TProcess.Create(NIL);
  try
    // Set up the process
    AProcess.ShowWindow := swoHide;
    AProcess.Executable := AAppFileName;

    // Process option poUsePipes has to be used so the output can be captured.
    // Process option poWaitOnExit can not be used because that would block
    // this program, preventing it from reading the output data of the process.
    AProcess.Options := [poUsePipes, poStderrToOutPut];

    // Params
    for i := 0 to High(AParams) do
      AProcess.Parameters.Add(AParams[i]);

    ACancel := False;
    try
      // Start the process
      AProcess.Execute;

      // All generated output from AProcess is read in a loop until no more data is available
      repeat
        // Get the new data from the process to a maximum of the buffer size that was allocated.
        // Note that all read(...) calls will block except for the last one, which returns 0 (zero).
        BytesRead := AProcess.Output.Read(Buffer, BUF_SIZE);
        SetLength(s, BytesRead);
        Move(Buffer[1], s[1], BytesRead);
        //
        if Assigned(ACB) then
        begin
          ACB(s, ACancel);
          Application.ProcessMessages;
          if ACancel then AProcess.Terminate(-1);
        end;
      until BytesRead = 0; // Stop if no more data is available

      Result := True;
    except
      on e: Exception do
      begin
        Result := False;
      end;
    end;
  finally
    // The process has finished so it can be cleaned up
    AProcess.Free;
  end;
end;

// -----------------------------------------------------------------------------

function ExecuteAppLoop(const AAppFileName: String; const AParams: TStringArray; var Output: TStringList; const ACB: TOnRunCommandEvent = NIL): Boolean;
var
  AProcess  : TProcess;
  sOutput   : String;
  StdError  : String;
  i, Status : Integer;
begin
  AProcess := TProcess.Create(NIL);
  try
    AProcess.ShowWindow := swoHide;
    AProcess.Options := AProcess.Options + [poRunIdle, poStderrToOutPut];
    AProcess.Executable := AAppFileName;
    AProcess.OnRunCommandEvent := ACB;

    for i := 0 to High(AParams) do
      AProcess.Parameters.Add(AParams[i]);

    if not Assigned(Output) then
      Output := TStringList.Create;

    Result := AProcess.RunCommandLoop(sOutput, StdError, Status) = 0;
    Output.Text := sOutput;
  finally
    AProcess.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
