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
  Forms, Classes, Graphics, SysUtils, StrUtils, Process;

type

  TGenerateThumbnails = class(TThread)
  private
    FFileName: String;
    FDuration: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(const AVideoFile: String; const AVideoDuration: Integer; AOnTerminate: TNotifyEvent = NIL);
  end;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, WAVDisplayer, Math;

// -----------------------------------------------------------------------------

constructor TGenerateThumbnails.Create(const AVideoFile: String; const AVideoDuration: Integer; AOnTerminate: TNotifyEvent = NIL);
begin
  inherited Create(True);

  FFileName       := AVideoFile;
  FDuration       := AVideoDuration - 1000;
  OnTerminate     := AOnTerminate;
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
    s := ChangeFileExt(GetTempFileName, '.bmp');

    try
      for i := 1 to 100 do
      begin
        AProcess.Parameters.Clear;
        for x := 0 to High(AParamArray) do
        begin
          if AParamArray[x] = '%timestr' then
            AProcess.Parameters.Add(IntToStr(Max(Round((FDuration / DefaultThumbnailsCount) * i), 0)) + 'ms')
          else
          begin
            AParamArray[x] := StringsReplace(AParamArray[x], ['%input', '%output'], [FFileName, s], []);
            AProcess.Parameters.Add(AParamArray[x]);
          end;
        end;

        AProcess.Execute;
        while AProcess.Running do
        begin
        end;

        if FileExists(s) then
        try
          WAVEOptions.Thumbnails[i-1].LoadFromFile(s);
          DeleteFile(s);
        except
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
