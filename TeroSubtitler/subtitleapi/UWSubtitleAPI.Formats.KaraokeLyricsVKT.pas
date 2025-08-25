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

unit UWSubtitleAPI.Formats.KaraokeLyricsVKT;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSubtitleAPI.Formats;

type

  { TUWKaraokeLyricsVKT }

  TUWKaraokeLyricsVKT = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Tags, UWSystem.StrUtils, UWSystem.SysUtils;

// -----------------------------------------------------------------------------

function TUWKaraokeLyricsVKT.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWKaraokeLyricsVKT.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfKaraokeLyricsVKT;
end;

// -----------------------------------------------------------------------------

function TUWKaraokeLyricsVKT.Extension: String;
begin
  Result := '*.vkt';
end;

// -----------------------------------------------------------------------------

function TUWKaraokeLyricsVKT.HasStyleSupport: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWKaraokeLyricsVKT.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  //if (IsInteger(Copy(SubtitleFile[Row], 2, 5))) and
  if (Copy(SubtitleFile[Row], 1, 1) = '{') and
    (Copy(SubtitleFile[Row], 7, 1) = ' ') and
    (Copy(SubtitleFile[Row], Length(SubtitleFile[Row]), 1) = '}') then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWKaraokeLyricsVKT.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to SubtitleFile.Count-1 do
      if SubtitleFile[i].StartsWith('{') then
      begin
        InitialTime := StrToInt(Copy(SubtitleFile[i], 2, Pos(' ', SubtitleFile[i]) - 2))*10;
        if (i+1 <= (SubtitleFile.Count-1)) and (Copy(SubtitleFile[i+1], 1, 1) = '{') then
          FinalTime := StrToInt(Copy(SubtitleFile[i+1], 2, Pos(' ', SubtitleFile[i+1]) - 2))*10
        else
          FinalTime := InitialTime + 2000;

        Text := Copy(SubtitleFile[i], Pos(' ', SubtitleFile[i]) + 1, LastDelimiter('}', SubtitleFile[i]) - (Pos(' ', SubtitleFile[i]) + 1));

        if (InitialTime >= 0) and (FinalTime > 0) and not Text.IsEmpty then
          Subtitles.Add(InitialTime, FinalTime, Text, '');
      end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWKaraokeLyricsVKT.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i       : Integer;
  Text    : String;
  DateSep : Char;
  DateFor : String;
begin
  Result  := False;

  StringList.Add('# <HEAD>');
  StringList.Add('# FRAME RATE=MP3');
  StringList.Add('# CREATOR=Project author');
  StringList.Add('# VIDEO SOURCE=C:\Untitled.avi');
  // DATE
  DateSep         := SysUtils.FormatSettings.DateSeparator;
  DateFor         := SysUtils.FormatSettings.ShortDateFormat;
  SysUtils.FormatSettings.DateSeparator   := '-';
  SysUtils.FormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  StringList.Add('# DATE=' + DateToStr(Date));
  SysUtils.FormatSettings.DateSeparator   := DateSep;
  SysUtils.FormatSettings.ShortDateFormat := DateFor;
  //
  StringList.Add('# </HEAD>');
  StringList.Add('#');

  for i := FromItem to ToItem do
  begin
    Text := RemoveTSTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));
    StringList.Add('{' + IntToStr(Subtitles.InitialTime[i] div 10).PadLeft(5, '0') + ' ' + ReplaceEnters(Text, LineEnding, ' ') + '}');
    StringList.Add('{' + IntToStr(Subtitles.FinalTime[i] div 10).PadLeft(5, '0') + ' }');
  end;

  StringList.Add('');
  StringList.Add('#');
  StringList.Add('# THE END.');

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
