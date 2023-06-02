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

unit UWSubtitleAPI.Formats.MPlayer;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.StrUtils,
  UWSystem.SysUtils, UWSubtitleAPI.Formats;

type

  { TUWMPlayer }

  TUWMPlayer = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function IsTimeBased: Boolean; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Tags;

// -----------------------------------------------------------------------------

function TUWMPlayer.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWMPlayer.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfMPlayer;
end;

// -----------------------------------------------------------------------------

function TUWMPlayer.Extension: String;
begin
  Result := '*.mpl';
end;

// -----------------------------------------------------------------------------

function TUWMPlayer.IsTimeBased: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWMPlayer.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWMPlayer.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (IsInteger(Copy(SubtitleFile[Row], 1, Pos(',',SubtitleFile[Row]) - 1))) and
     (IsInteger(Copy(SubtitleFile[Row], Pos(',',SubtitleFile[Row]) + 1, Pos(',', SubtitleFile[Row], Pos(',',SubtitleFile[Row]) + 1) - (Pos(',',SubtitleFile[Row]) + 1)))) and
     not IsInteger(ReplaceString(SubtitleFile[Row], ',', '')) and
     (StringCount(',', SubtitleFile[Row]) >= 3) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWMPlayer.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to SubtitleFile.Count-1 do
    begin
      if (IsInteger(Copy(SubtitleFile[i], 1, Pos(',',SubtitleFile[i]) - 1))) and
         (IsInteger(Copy(SubtitleFile[i], Pos(',',SubtitleFile[i]) + 1, Pos(',', SubtitleFile[i], Pos(',',SubtitleFile[i]) + 1) - (Pos(',',SubtitleFile[i]) + 1)))) then
      begin
        InitialTime   := FramesToTime(StrToInt(Copy(SubtitleFile[i], 1, Pos(',',SubtitleFile[i]) - 1)), FPS);
        FinalTime     := FramesToTime(StrToInt(Copy(SubtitleFile[i], Pos(',',SubtitleFile[i]) + 1, Pos(',', SubtitleFile[i], Pos(',',SubtitleFile[i]) + 1) - (Pos(',',SubtitleFile[i]) + 1))), FPS);
        Text          := ReplaceString(Copy(SubtitleFile[i], Pos(',', SubtitleFile[i], Pos(',', SubtitleFile[i], Pos(',', SubtitleFile[i], Pos(',',SubtitleFile[i]) + 1)) + 1) + 1, Length(SubtitleFile[i])), '|', LineEnding);

        Subtitles.Add(InitialTime, FinalTime, Text, '');
      end;
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWMPlayer.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i    : Integer;
  Text : String;
begin
  Result  := False;

  for i := FromItem to ToItem do
  begin
    Text := RemoveTSTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));
    StringList.Add(IntToStr(TimeToFrames(Subtitles[i].InitialTime, FPS)) + ',' + IntToStr(TimeToFrames(Subtitles[i].FinalTime, FPS)) + ',0,' + ReplaceEnters(Text), False);
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
