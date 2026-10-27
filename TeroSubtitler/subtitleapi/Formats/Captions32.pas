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

unit Captions32;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats, StrUtils;

type

  { TUWCaptions32 }

  TUWCaptions32 = class(TUWSubtitleCustomFormat)
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

uses UWSubtitleAPI.Tags, UWSystem.StrUtils, UWSystem.SysUtils, LazUTF8;

// -----------------------------------------------------------------------------

function TUWCaptions32.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWCaptions32.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfCaptions32;
end;

// -----------------------------------------------------------------------------

function TUWCaptions32.Extension: String;
begin
  Result := '*.txt';
end;

// -----------------------------------------------------------------------------

function TUWCaptions32.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWCaptions32.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (TimeInFormat(Copy(SubtitleFile[Row], 1, 11), 'hh:mm:ss:zz')) and
     (TimeInFormat(Copy(SubtitleFile[Row], 15, 11), 'hh:mm:ss:zz')) and
     (Pos(',', SubtitleFile[Row]) = 13) and
     (Pos(',', SubtitleFile[Row], 14) = 27) and
     (UTF8Copy(SubtitleFile[Row], 62, 1) = '|') then // por CARÁCTER: con un acento, por byte el '|' se corría a la col. 63
     //(Pos('|', SubtitleFile[Row], 15) = 62) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWCaptions32.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text, Txt   : String;
begin
  Result := False;
  try
    for i := 0 to SubtitleFile.Count-1 do
    begin
      // Antes se parseaba CADA línea sin validar los tiempos, y las columnas
      // fijas se cortaban por BYTE sobre texto UTF-8 (un acento corría el corte).
      if not (TimeInFormat(Copy(SubtitleFile[i], 1, 11), 'hh:mm:ss:zz') and
              TimeInFormat(Copy(SubtitleFile[i], 15, 11), 'hh:mm:ss:zz')) then Continue;

      InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 11));
      FinalTime   := StringToTime(Copy(SubtitleFile[i], 15, 11));

      if (InitialTime >= 0) and (FinalTime > 0) then
      begin
        Text := Trim(UTF8Copy(SubtitleFile[i], 29, 33));
        Txt  := Trim(UTF8Copy(SubtitleFile[i], 63, UTF8Length(SubtitleFile[i])-62));
        if Txt <> '' then
          Text := Text + LineEnding + Txt;

        Subtitles.Add(InitialTime, FinalTime, Text, '', NIL);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function TUWCaptions32.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i, c    : Integer;
  Text, s : String;

  // Recorta o rellena con espacios a EXACTAMENTE N caracteres (no bytes).
  function FitR(const S: String; const N: Integer): String;
  begin
    Result := UTF8Copy(S, 1, N);
    if UTF8Length(Result) < N then
      Result := Result + StringOfChar(' ', N - UTF8Length(Result));
  end;

begin
  Result  := False;

  for i := FromItem to ToItem do
  begin
    Text := ReplaceEnters(RemoveTSTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i])));
    s    := Text;
    c    := StringCount('|', s);
    // Columnas fijas medidas en CARACTERES (33 + '|' + hasta 67 en total):
    // antes por BYTE, con acentos el relleno quedaba corto y el '|' dejaba de
    // caer en la columna 62 (el archivo no se re-detectaba).
    if c = 0 then
      Text := FitR(FitR(s, 33) + '|', 67)
    else
    begin
      if c > 1 then
      begin
        c := UTF8Pos('|', s);
        s := UTF8Copy(s, 1, UTF8Pos('|', s, c+1)-1);
      end;

      c    := UTF8Pos('|', s);
      Text := FitR(UTF8Copy(s, 1, c-1), 33) + '|' + UTF8Copy(s, c+1, UTF8Length(s)-c);
      Text := FitR(Text, 67);
    end;

    StringList.Add(TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:zz') + ' , ' + TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:zz') + ' , ' + Text);
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
