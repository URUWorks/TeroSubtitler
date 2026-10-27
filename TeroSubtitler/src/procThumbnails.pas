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

unit procThumbnails;

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}
 
interface
 
uses
  Forms, Classes, Graphics, SysUtils, Types, Process;

type

  { TGenerateThumbnails }

  TThumbnailReadyEvent = procedure (const ATimeMS: Integer; const AFileName: String) of object;

  TGenerateThumbnails = class(TThread)
  private
    FFileName  : String;
    FTimes     : TIntegerDynArray; // snapshot of the scene changes (main thread)
    FOffsetMS  : Integer;
    FTempDir   : String;
    FOnReady   : TThumbnailReadyEvent;
    FCurTime   : Integer;
    FCurFile   : String;
    procedure DoReadySync;
    procedure RunAndWait(const AProcess: TProcess);
  protected
    procedure Execute; override;
  public
    constructor Create(const AVideoFile: String; const ATimes: TIntegerDynArray; const AFPS: Single; AOnReady: TThumbnailReadyEvent; AOnTerminate: TNotifyEvent = NIL);
    property TempDir: String read FTempDir write FTempDir;
  end;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, Math;

// -----------------------------------------------------------------------------

{ TGenerateThumbnails }

// -----------------------------------------------------------------------------

constructor TGenerateThumbnails.Create(const AVideoFile: String; const ATimes: TIntegerDynArray; const AFPS: Single; AOnReady: TThumbnailReadyEvent; AOnTerminate: TNotifyEvent = NIL);
var
  i: Integer;
begin
  inherited Create(True);

  FFileName       := AVideoFile;
  FOnReady        := AOnReady;
  OnTerminate     := AOnTerminate;
  FTempDir        := ConcatPaths([GetTempDir, ChangeFileExt(ExtractFileName(FFileName), '')]);
  FreeOnTerminate := True;

  // half a frame, so ffmpeg doesn't give the last frame of the previous shot
  if AFPS > 0 then
    FOffsetMS := Round(500 / AFPS)
  else
    FOffsetMS := 0;

  // the thread must not read the scene change list while the user edits it,
  // so copy the times it needs now, from the main thread
  SetLength(FTimes, Length(ATimes));
  for i := 0 to High(ATimes) do
    FTimes[i] := ATimes[i];
end;

// -----------------------------------------------------------------------------

procedure TGenerateThumbnails.DoReadySync;
begin
  // TBitmap is not thread safe: it's loaded in the main thread
  if Assigned(FOnReady) then
    FOnReady(FCurTime, FCurFile);
end;

// -----------------------------------------------------------------------------

procedure TGenerateThumbnails.RunAndWait(const AProcess: TProcess);
begin
  // wait without spinning a core at 100%
  AProcess.Execute;
  while AProcess.Running and not Terminated do
    Sleep(10);
  if AProcess.Running then
    AProcess.Terminate(0);
end;

// -----------------------------------------------------------------------------

procedure TGenerateThumbnails.Execute;
var
  AProcess : TProcess;
  AParamArray : TStringArray;
  i, x : Integer;
  s : String;
begin
  if Length(FTimes) = 0 then Exit;

  AProcess := TProcess.Create(NIL);
  try
    AProcess.ShowWindow := swoHide;
    AProcess.Executable := Tools.FFmpeg;
    AParamArray := FFMPEG_Thumbnail.Split(' ');

    if not DirectoryExists(FTempDir) then
      CreateDir(FTempDir);

    try
      for i := 0 to High(FTimes) do
      begin
        if Terminated then Exit;

        // the files are named by time, so they are also a cache ("t" = 24-bit,
        // the old 16-bit ones looked wrong on macOS)
        s := ConcatPaths([FTempDir, FTimes[i].ToString + 't.bmp']);

        if not FileExists(s) then
        begin
          AProcess.Parameters.Clear;
          for x := 0 to High(AParamArray) do
          begin
            if AParamArray[x] = '%timestr' then
              AProcess.Parameters.Add(Max(FTimes[i] + FOffsetMS, 0).ToString + 'ms')
            else if AParamArray[x].Contains('%input') then
              AProcess.Parameters.Add(FFileName)
            else if AParamArray[x].Contains('%output') then
              AProcess.Parameters.Add(s)
            else
              AProcess.Parameters.Add(AParamArray[x]);
          end;

          RunAndWait(AProcess);
          if Terminated then Exit;
        end;

        if FileExists(s) then
        begin
          FCurTime := FTimes[i];
          FCurFile := s;
          Synchronize(@DoReadySync);
        end;
      end;
    except
    end;
  finally
    AProcess.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
