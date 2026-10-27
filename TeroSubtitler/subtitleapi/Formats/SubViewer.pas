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

unit SubViewer;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWSubViewer }

  TUWSubViewer = class(TUWSubtitleCustomFormat)
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

uses Classes, UWSubtitleAPI.Tags, UWSystem.SysUtils;

// -----------------------------------------------------------------------------

function TUWSubViewer.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWSubViewer.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfSubViewer;
end;

// -----------------------------------------------------------------------------

function TUWSubViewer.Extension: String;
begin
  Result := '*.sbv';
end;

// -----------------------------------------------------------------------------

function TUWSubViewer.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

// Un tiempo de SubViewer: 'h:mm:ss.zzz' (milisegundos, SBV de YouTube) o
// 'hh:mm:ss.cc' (centésimas, SubViewer 2). Horas de 1-2 dígitos y una fracción de
// 1 a 3 dígitos tomada como fracción de segundo ('.5' = 500 ms, '.50' = 500 ms).
// Antes solo se aceptaba el molde 'h:mm:ss.zzz' de 11 caracteres exactos: las
// horas de dos dígitos y las centésimas de SubViewer 2 no se reconocían.
function SVSplitTime(const S: String; out H, M, Sec, Ms: Integer): Boolean;
var
  T, HS, MS2, SS, FS: String;
  P1, P2, P3, k: Integer;

  function AllDigits(const X: String; const MinLen, MaxLen: Integer): Boolean;
  var
    j: Integer;
  begin
    Result := (Length(X) >= MinLen) and (Length(X) <= MaxLen);
    for j := 1 to Length(X) do
      if not (X[j] in ['0'..'9']) then Exit(False);
  end;

begin
  Result := False;
  H := 0; M := 0; Sec := 0; Ms := 0;
  T := Trim(S);
  P1 := Pos(':', T);
  if P1 < 2 then Exit;
  P2 := Pos(':', Copy(T, P1 + 1, MaxInt));
  if P2 = 0 then Exit;
  P2 := P2 + P1;
  P3 := 0;
  for k := P2 + 1 to Length(T) do
    if (T[k] = '.') or (T[k] = ',') then
    begin
      P3 := k;
      Break;
    end;
  if P3 = 0 then Exit;

  HS  := Copy(T, 1, P1 - 1);
  MS2 := Copy(T, P1 + 1, P2 - P1 - 1);
  SS  := Copy(T, P2 + 1, P3 - P2 - 1);
  FS  := Copy(T, P3 + 1, MaxInt);
  if not (AllDigits(HS, 1, 2) and AllDigits(MS2, 2, 2) and AllDigits(SS, 2, 2) and AllDigits(FS, 1, 3)) then Exit;

  while Length(FS) < 3 do FS := FS + '0'; // fracción de segundo
  H := StrToInt(HS);
  M := StrToInt(MS2);
  Sec := StrToInt(SS);
  Ms := StrToInt(FS);
  Result := True;
end;

// -----------------------------------------------------------------------------

// 'inicio,fin' -> milisegundos.
function SVTimingLine(const L: String; out AStart, AEnd: Integer): Boolean;
var
  p, k, H, M, S, Ms: Integer;
begin
  Result := False;
  AStart := -1;
  AEnd := -1;
  p := Pos(',', L);
  if p < 2 then Exit;
  // La coma que separa los dos tiempos es la que va después de una fracción: se
  // busca la que deja dos tiempos válidos (con ',' también como decimal habría
  // más de una).
  while p > 0 do
  begin
    if SVSplitTime(Copy(L, 1, p - 1), H, M, S, Ms) then
    begin
      AStart := ((H * 60 + M) * 60 + S) * 1000 + Ms;
      if SVSplitTime(Copy(L, p + 1, MaxInt), H, M, S, Ms) then
      begin
        AEnd := ((H * 60 + M) * 60 + S) * 1000 + Ms;
        Exit(True);
      end;
    end;
    k := Pos(',', Copy(L, p + 1, MaxInt));
    if k = 0 then Break;
    p := p + k;
  end;
  AStart := -1;
  AEnd := -1;
end;

// -----------------------------------------------------------------------------

function TUWSubViewer.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
var
  a, b: Integer;
begin
  Result := (Pos(',', SubtitleFile[Row]) > 0) and
            SVTimingLine(SubtitleFile[Row], a, b);
end;

// -----------------------------------------------------------------------------

function TUWSubViewer.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
  a, b        : Integer;
begin
  Result := False;
  try
    i := 0;
    while i < SubtitleFile.Count do
    begin
      if SVTimingLine(SubtitleFile[i], InitialTime, FinalTime) then
      begin
        if (InitialTime >= 0) and (FinalTime > 0) then
        begin
          // El texto llega hasta la próxima línea de tiempos. Las cabeceras de
          // SubViewer 2 ([INFORMATION]...[SUBTITLE], [COLF]...) vienen ANTES
          // del primer tiempo y no entran en ningún cue.
          Inc(i);
          Text := '';
          while (i < SubtitleFile.Count) and
                not ((Pos(',', SubtitleFile[i]) > 0) and SVTimingLine(SubtitleFile[i], a, b)) do
          begin
            if Text <> '' then
              Text := Text + LineEnding + SubtitleFile[i]
            else
              Text := SubtitleFile[i];

            Inc(i);
          end;
          Dec(i);

          // '[br]' es el salto de línea de SubViewer 2
          Text := StringReplace(Text, '[br]', LineEnding, [rfReplaceAll, rfIgnoreCase]);
          Text := HTMLTagsToTS(Text);
          Subtitles.Add(InitialTime, FinalTime, Text, '', NIL);
        end;
      end;
      Inc(i);
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSubViewer.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i, k  : Integer;
  Text  : String;
  Lines : TStringList;
begin
  Result  := False;

  StringList.Clear;
  Lines := TStringList.Create;
  try
    for i := FromItem to ToItem do
    begin
      // Sin líneas vacías (una línea en blanco TERMINA el cue) ni cues sin texto
      Lines.Text := TSTagsToHTML(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));
      Text := '';
      for k := 0 to Lines.Count - 1 do
        if Trim(Lines[k]) <> '' then
        begin
          if Text <> '' then Text := Text + sLineBreak;
          Text := Text + Lines[k];
        end;
      if Text = '' then Continue;

      StringList.Add(TimeToString(Subtitles.InitialTime[i], 'h:mm:ss.zzz') + ',' + TimeToString(Subtitles.FinalTime[i], 'h:mm:ss.zzz'));
      StringList.Add(Text);
      StringList.Add('');
    end;
  finally
    Lines.Free;
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
