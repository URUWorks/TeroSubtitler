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
 *  Copyright (C) 2001-2026 URUWorks, uruworks@gmail.com.
 *}

unit KaraokeLyricsVKT;

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

  // Centésimas de segundo de una línea '{01234 texto}' -> ms, o -1 si la línea
  // no trae un número válido. Antes StrToInt(Copy(s, 2, Pos(' ', s) - 2)): con
  // una línea '{...' sin espacio el Copy daba '' y StrToInt lanzaba
  // EConvertError, abortando toda la carga.
  function LineToMS(const S: String): Integer;
  var
    P, V: Integer;
  begin
    Result := -1;
    P := Pos(' ', S);
    if P <= 2 then Exit;
    V := StrToIntDef(Copy(S, 2, P - 2), -1);
    if (V < 0) or (V > 100000000) then Exit; // *10 no debe desbordar Integer
    Result := V * 10;
  end;

begin
  Result := False;
  try
    for i := 0 to SubtitleFile.Count-1 do
      if SubtitleFile[i].StartsWith('{') then
      begin
        InitialTime := LineToMS(SubtitleFile[i]);
        if InitialTime < 0 then Continue;

        FinalTime := -1;
        if (i+1 <= (SubtitleFile.Count-1)) and (Copy(SubtitleFile[i+1], 1, 1) = '{') then
          FinalTime := LineToMS(SubtitleFile[i+1]);
        if FinalTime < 0 then
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
begin
  Result  := False;

  StringList.Add('# <HEAD>');
  StringList.Add('# FRAME RATE=MP3');
  StringList.Add('# CREATOR=Project author');
  StringList.Add('# VIDEO SOURCE=C:\Untitled.avi');
  // DATE
  // Antes se cambiaban DateSeparator/ShortDateFormat GLOBALES y se
  // restauraban después; FormatDateTime da el mismo 'yyyy-mm-dd' sin tocarlos.
  StringList.Add('# DATE=' + FormatDateTime('yyyy-mm-dd', Date));
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
