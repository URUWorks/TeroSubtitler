{*
 *  URUWorks Subtitle API
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

unit UWSubtitleAPI.Formats.DVDJunior;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWDVDJunior }

  TUWDVDJunior = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Tags, UWSystem.StrUtils, UWSystem.SysUtils;

// -----------------------------------------------------------------------------

function TUWDVDJunior.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWDVDJunior.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfDVDJunior;
end;

// -----------------------------------------------------------------------------

function TUWDVDJunior.Extension: String;
begin
  Result := '*.txt';
end;

// -----------------------------------------------------------------------------

function TUWDVDJunior.HasStyleSupport: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWDVDJunior.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (AnsiUpperCase(Copy(SubtitleFile[Row], 1, 19)) = 'SUBTITLE FILE MARK:') or
     ((TimeInFormat(Copy(SubtitleFile[Row], 1, 11), 'hh:mm:ss:zz')) and
     (TimeInFormat(Copy(SubtitleFile[Row], 13, 11), 'hh:mm:ss:zz')) and
     (Pos('&', SubtitleFile[Row], 12) = 12) and
     (Pos('#', SubtitleFile[Row], 24) = 24)) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWDVDJunior.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result    := False;
  try
    for i := 0 to SubtitleFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 11));
      FinalTime   := StringToTime(Copy(SubtitleFile[i], 13, 11));
      Text        := ReplaceString(Copy(SubtitleFile[i], 25, Length(SubtitleFile[i])), '<', LineEnding);

      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime, FinalTime, Text, '');
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWDVDJunior.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i    : Integer;
  Text : String;
begin
  Result  := False;

  StringList.Add('Subtitle File Mark:2C:\Untitled.avi', False);

  for i := FromItem to ToItem do
  begin
    Text := RemoveTSTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));
    StringList.Add(TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:zz') + '&' + TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:zz') + '#' + ReplaceEnters(Text, '<'), False);
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
