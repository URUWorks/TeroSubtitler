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

unit UWSubtitleAPI.Formats.KaraokeLyricsLRC;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWKaraokeLyricsLRC }

  TUWKaraokeLyricsLRC = class(TUWSubtitleCustomFormat)
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

function TUWKaraokeLyricsLRC.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWKaraokeLyricsLRC.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfKaraokeLyricsLRC;
end;

// -----------------------------------------------------------------------------

function TUWKaraokeLyricsLRC.Extension: String;
begin
  Result := '*.lrc';
end;

// -----------------------------------------------------------------------------

function TUWKaraokeLyricsLRC.HasStyleSupport: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWKaraokeLyricsLRC.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (TimeInFormat(Copy(SubtitleFile[Row], 2, 8), 'mm:ss.zz')) and
     (Copy(SubtitleFile[Row], 1, 1) = '[') and
     (Copy(SubtitleFile[Row], 10, 1) = ']') then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWKaraokeLyricsLRC.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to SubtitleFile.Count-1 do
      if SubtitleFile[i].StartsWith('[') and (Pos(']', SubtitleFile[i]) = 10) then
      begin
        InitialTime := StringToTime(Copy(SubtitleFile[i], 2, Pos(']', SubtitleFile[i]) - 2), True);
        if i+1 <= (SubtitleFile.Count-1) then
          FinalTime := StringToTime(Copy(SubtitleFile[i+1], 2, Pos(']', SubtitleFile[i+1]) - 2), True)
        else
          FinalTime := InitialTime + 2000;

        Text := Copy(SubtitleFile[i], Pos(']', SubtitleFile[i]) + 1, Length(SubtitleFile[i]));

        if (InitialTime >= 0) and (FinalTime > 0) and not Text.IsEmpty then
          Subtitles.Add(InitialTime, FinalTime, Text, '');
      end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWKaraokeLyricsLRC.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i    : Integer;
  Text : String;
begin
  Result  := False;

  StringList.Add('[ti:Project title]');
  StringList.Add('[ar:Project author]');
  StringList.Add('[la:af]');
  StringList.Add('Project title');
  StringList.Add('');

  for i := FromItem to ToItem do
  begin
    Text := RemoveTSTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));
    StringList.Add('[' + TimeToString(Subtitles.InitialTime[i], 'mm:ss.zz') + ']' + ReplaceEnters(Text, LineEnding, ' '));
    StringList.Add('[' + TimeToString(Subtitles.FinalTime[i], 'mm:ss.zz') + ']');
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
