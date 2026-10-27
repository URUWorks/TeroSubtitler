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

unit MPlayer2;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.StrUtils, UWSystem.SysUtils, UWSystem.TimeUtils,
  UWSubtitleAPI.Formats;

type

  { TUWMPlayer2 }

  TUWMPlayer2 = class(TUWSubtitleCustomFormat)
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

uses UWSubtitleAPI.Tags;

// -----------------------------------------------------------------------------

// Texto de MPL2 -> formato interno: '|' es salto de línea y una '/' al principio
// de una LÍNEA (no de todo el subtítulo) la pone en cursiva. Antes la '/'
// quedaba como carácter literal en el texto.
function MPL2TextToTS(const S: String): String;
var
  Rest, Line: String;
  p: Integer;
begin
  Result := '';
  Rest := S;
  while True do
  begin
    p := Pos('|', Rest);
    if p = 0 then
    begin
      Line := Rest;
      Rest := '';
    end
    else
    begin
      Line := Copy(Rest, 1, p - 1);
      Delete(Rest, 1, p);
    end;

    if (Line <> '') and (Line[1] = '/') then
      Line := '{\i1}' + Copy(Line, 2, MaxInt) + '{\i0}';

    if Result <> '' then Result := Result + LineEnding;
    Result := Result + Line;
    if p = 0 then Break;
  end;
end;

// -----------------------------------------------------------------------------

// Formato interno -> MPL2: cursiva por línea con '/' (una línea cuyo texto
// visible está entero en cursiva); el resto de los tags no existe en MPL2.
function TSTextToMPL2(const S: String): String;
var
  Lines: TStringArray;
  k, p, q, nChars, nItal: Integer;
  Italic: Boolean;
  Tag, Vis: String;
begin
  Result := '';
  Italic := False;
  Lines := ReplaceString(S, #13#10, #10).Split([#10]);
  for k := 0 to High(Lines) do
  begin
    Vis := '';
    nChars := 0;
    nItal := 0;
    p := 1;
    while p <= Length(Lines[k]) do
    begin
      if (Lines[k][p] = '{') and (p < Length(Lines[k])) and (Lines[k][p+1] = '\') then
      begin
        q := Pos('}', Lines[k], p);
        if q > 0 then
        begin
          Tag := LowerCase(Copy(Lines[k], p, q - p + 1));
          if Tag = '{\i1}' then Italic := True
          else if Tag = '{\i0}' then Italic := False;
          p := q + 1;
          Continue;
        end;
      end;
      Vis := Vis + Lines[k][p];
      if Lines[k][p] <> ' ' then
      begin
        Inc(nChars);
        if Italic then Inc(nItal);
      end;
      Inc(p);
    end;

    if Trim(Vis) = '' then Continue;
    if Result <> '' then Result := Result + '|';
    if (nChars > 0) and (nItal = nChars) then Result := Result + '/';
    Result := Result + Vis;
  end;
end;

// -----------------------------------------------------------------------------

function TUWMPlayer2.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWMPlayer2.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfMPlayer2;
end;

// -----------------------------------------------------------------------------

function TUWMPlayer2.Extension: String;
begin
  Result := '*.mpl';
end;

// -----------------------------------------------------------------------------

function TUWMPlayer2.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWMPlayer2.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
var
  L, A, B: String;
  P2, P4: Integer;
begin
  Result := False;
  L := SubtitleFile[Row];
  if (L = '') or (L[1] <> '[') then Exit;
  P2 := Pos(']', L);
  if (P2 < 3) or (P2 >= Length(L)) or (L[P2 + 1] <> '[') then Exit;
  P4 := Pos(']', Copy(L, P2 + 2, MaxInt));
  if P4 = 0 then Exit;
  A := Copy(L, 2, P2 - 2);
  B := Copy(L, P2 + 2, P4 - 1);
  // '[inicio][fin]texto' en décimas de segundo; el fin puede ir vacío
  // ('[inicio][]texto'). Antes un fin vacío no se reconocía.
  Result := IsInteger(A) and ((B = '') or IsInteger(B));
end;

// -----------------------------------------------------------------------------

function TUWMPlayer2.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text, L, A, B : String;
  P2, P4      : Integer;
begin
  Result := False;
  try
    for i := 0 to SubtitleFile.Count-1 do
    begin
      L := SubtitleFile[i];
      if (L = '') or (L[1] <> '[') then Continue;
      P2 := Pos(']', L);
      if (P2 < 3) or (P2 >= Length(L)) or (L[P2 + 1] <> '[') then Continue;
      P4 := Pos(']', Copy(L, P2 + 2, MaxInt));
      if P4 = 0 then Continue;
      P4 := P4 + P2 + 1; // posición absoluta del ']' del fin

      A := Copy(L, 2, P2 - 2);
      B := Copy(L, P2 + 2, P4 - P2 - 2);
      if not IsInteger(A) then Continue;

      Text := MPL2TextToTS(Copy(L, P4 + 1, MaxInt));

      // Décimas de segundo (Int64: el producto desbordaba un Integer con valores grandes)
      InitialTime := Int64(StrToInt(A)) * 100;
      if IsInteger(B) then
        FinalTime := Int64(StrToInt(B)) * 100
      else
        // Sin tiempo de fin ('[inicio][]texto', que FFmpeg también acepta):
        // duración según el largo del texto (antes 2 s fijos)
        FinalTime := InitialTime + CalculateOptimalDisplayMS(RemoveTSTags(Text));

      if (InitialTime >= 0) and (FinalTime > 0) then
        Subtitles.Add(InitialTime, FinalTime, Text, '');
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWMPlayer2.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i      : Integer;
  Text   : String;
  S1, S2 : Int64;
begin
  Result  := False;

  StringList.Clear;
  for i := FromItem to ToItem do
  begin
    // Cursiva por línea con '/', como el formato (antes se borraba)
    Text := TSTextToMPL2(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));
    if Text = '' then Continue;

    // Décimas de segundo REDONDEADAS (antes se truncaba: hasta 99 ms de
    // error), sin negativos y con el fin posterior al inicio.
    S1 := (Int64(Subtitles.InitialTime[i]) + 50) div 100;
    S2 := (Int64(Subtitles.FinalTime[i]) + 50) div 100;
    if S1 < 0 then S1 := 0;
    if S2 <= S1 then S2 := S1 + 1;

    StringList.Add('[' + IntToStr(S1) + '][' + IntToStr(S2) + ']' + Text);
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
