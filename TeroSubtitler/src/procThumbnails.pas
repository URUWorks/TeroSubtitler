{*
 *  URUWorks
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
 *  Copyright (C) 2023-2024 URUWorks, uruworks@gmail.com.
 *}

unit procThumbnails;

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}
 
interface
 
uses
  Forms, Classes, Graphics, SysUtils, Types, Process;

type

  { TGenerateThumbnails }

  TGenerateThumbnailMode = (gtmArray, gtmSubtitle, gtmShotchange);

  TGenerateThumbnails = class(TThread)
  private
    FFileName: String;
    FDuration: Integer;
    FMode: TGenerateThumbnailMode;
    FShotChanges: TIntegerDynArray;
    FTempDir: String;
  protected
    procedure Execute; override;
  public
    constructor Create(const AVideoFile: String; const AVideoDuration: Integer; const AMode: TGenerateThumbnailMode; AOnTerminate: TNotifyEvent = NIL; const AShotChanges: TIntegerDynArray = NIL);
    property TempDir: String read FTempDir write FTempDir;
  end;

  { TLoadThumbnails }

  TLoadThumbnails = class(TThread)
  private
    FMode: TGenerateThumbnailMode;
    FShotChanges: TIntegerDynArray;
    FTempDir: String;
  protected
    procedure Execute; override;
  public
    constructor Create(const AMode: TGenerateThumbnailMode; const ATempDir: String; const AShotChanges: TIntegerDynArray = NIL);
    property TempDir: String read FTempDir write FTempDir;
  end;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, WAVDisplayer, Math;

// -----------------------------------------------------------------------------

{ TGenerateThumbnails }

// -----------------------------------------------------------------------------

constructor TGenerateThumbnails.Create(const AVideoFile: String; const AVideoDuration: Integer; const AMode: TGenerateThumbnailMode; AOnTerminate: TNotifyEvent = NIL; const AShotChanges: TIntegerDynArray = NIL);
begin
  inherited Create(True);

  FFileName       := AVideoFile;
  FDuration       := AVideoDuration - 1000;
  FMode           := AMode;
  FShotChanges    := AShotChanges;
  OnTerminate     := AOnTerminate;
  FTempDir        := ConcatPaths([GetTempDir, ChangeFileExt(ExtractFileName(FFileName), '')]);
  FreeOnTerminate := True;
end;

// -----------------------------------------------------------------------------

procedure TGenerateThumbnails.Execute;
var
  AProcess : TProcess;
  AParamArray : TStringArray;
  i, x : Integer;
  s : String;
begin
  AProcess := TProcess.Create(NIL);
  try
    AProcess.ShowWindow := swoHide;
    AProcess.Executable := Tools.FFmpeg;
    AParamArray := FFMPEG_Thumbnail.Split(' ');

    if not DirectoryExists(FTempDir) then
      CreateDir(FTempDir);

    case FMode of
      gtmArray:
        begin
          s := ChangeFileExt(GetTempFileName, '.bmp');
          try
            for i := 1 to 100 do
            begin
              AProcess.Parameters.Clear;
              for x := 0 to High(AParamArray) do
              begin
                if AParamArray[x] = '%timestr' then
                  AProcess.Parameters.Add(IntToStr(Max(Round((FDuration / DefaultThumbnailsCount) * i), 0)) + 'ms')
                else if AParamArray[x].Contains('%input') then
                  AProcess.Parameters.Add(FFileName)
                else if AParamArray[x].Contains('%output') then
                  AProcess.Parameters.Add(s)
                else
                  AProcess.Parameters.Add(AParamArray[x]);
              end;

              AProcess.Execute;
              while AProcess.Running and not Terminated do;

              if FileExists(s) then
              try
                WAVEOptions.Thumbnails[i-1].LoadFromFile(s);
                DeleteFile(s);
              except
              end;

              if Terminated then Exit;
            end;
          except
          end;
        end;

      gtmSubtitle:
        if Subtitles.Count > 0 then
        begin
          try
            for i := 0 to Subtitles.Count-1 do
            begin
              s := ConcatPaths([FTempDir, IntToStr(i) + '.bmp']);
              AProcess.Parameters.Clear;

              for x := 0 to High(AParamArray) do
              begin
                if AParamArray[x] = '%timestr' then
                  AProcess.Parameters.Add(IntToStr(Subtitles[i].InitialTime) + 'ms')
                else if AParamArray[x].Contains('%input') then
                  AProcess.Parameters.Add(FFileName)
                else if AParamArray[x].Contains('%output') then
                  AProcess.Parameters.Add(s)
                else
                  AProcess.Parameters.Add(AParamArray[x]);
              end;

              AProcess.Execute;
              while AProcess.Running and not Terminated do;
              if Terminated then Exit;
            end;
          except
          end;
        end;

      gtmShotchange:
        if Assigned(FShotChanges) and (Length(FShotChanges) > 0) then
        begin
          try
            for i := Low(FShotChanges) to High(FShotChanges) do
            begin
              AParamArray := FFMPEG_Thumbnail.Split(' ');
              s := ConcatPaths([FTempDir, IntToStr(FShotChanges[i]) + '.bmp']);
              AProcess.Parameters.Clear;

              for x := 0 to High(AParamArray) do
              begin
                if AParamArray[x] = '%timestr' then
                  AProcess.Parameters.Add(IntToStr(FShotChanges[i]) + 'ms')
                else if AParamArray[x].Contains('%input') then
                  AProcess.Parameters.Add(FFileName)
                else if AParamArray[x].Contains('%output') then
                  AProcess.Parameters.Add(s)
                else
                  AProcess.Parameters.Add(AParamArray[x]);
              end;

              AProcess.Execute;
              while AProcess.Running and not Terminated do;
              if Terminated then Exit;
            end;
          except
          end;
        end;
    end;
  finally
    AProcess.Free;
  end;
end;

// -----------------------------------------------------------------------------

{ TLoadThumbnails }

// -----------------------------------------------------------------------------

constructor TLoadThumbnails.Create(const AMode: TGenerateThumbnailMode; const ATempDir: String; const AShotChanges: TIntegerDynArray = NIL);
begin
  inherited Create(True);

  FMode           := AMode;
  FShotChanges    := AShotChanges;
  FTempDir        := ATempDir;
  FreeOnTerminate := True;
end;

// -----------------------------------------------------------------------------

procedure TLoadThumbnails.Execute;
var
  i : Integer;
  s : String;
begin
  case FMode of
    gtmSubtitle:
      if Subtitles.Count > 0 then
      begin
        try
          for i := 0 to Subtitles.Count-1 do
          begin
            s := ConcatPaths([FTempDir, IntToStr(i) + '.bmp']);
            if FileExists(s) then
            begin
            end;
          end;
        except
        end;
      end;

    gtmShotchange:
      if Assigned(FShotChanges) and (Length(FShotChanges) > 0) then
      begin
        try
          for i := Low(FShotChanges) to High(FShotChanges) do
          begin
            s := ConcatPaths([FTempDir, IntToStr(FShotChanges[i]) + '.bmp']);
            if FileExists(s) then
            begin
            end;
          end;
        except
        end;
      end;
  end;
end;

// -----------------------------------------------------------------------------

end.
