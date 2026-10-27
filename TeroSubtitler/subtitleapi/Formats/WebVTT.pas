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

unit WebVTT;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats,
  RegExpr, Graphics;

type

  { TUWWebVTT }

  TUWWebVTT = class(TUWSubtitleCustomFormat)
  private
    FColors : TStringList;
    procedure ResetColorsToDefault;
    procedure ParseCueStyles(const CSS: String);
    function FromRepl(ARegExpr: TRegExpr): RegExprString;
    function TagsToTS(const Text: String): String;
    function ToRepl(ARegExpr: TRegExpr): RegExprString;
    function TSToTags(const Text: String): String;
  public
    constructor Create(const AShouldWriteBOM: Boolean = False);
    destructor Destroy; override;
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function IsTimeBased: Boolean; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.ExtraInfo, UWSubtitleAPI.Tags, UWSystem.StrUtils,
  UWSystem.SysUtils, UWSubtitleAPI.Utils;

{
This format allows for time notation of hours, minutes, seconds and milliseconds,
respectively use like this:
00:00:00.000 where the 3 zeros at the end are the milliseconds.

Settings have to placed right after the timing, on the same line, separated with one (or more) space or tabulation.

Vertical text
D:vertical (vertical growing left)
D:vertical-lr (vertical growing right)
Line position
A specific position relative to the video frame:
L:[a number]%, where [a number] is a positive integer.
A line number:
L:[a number], where [a number] is a positive or negative integer.
Text position	T:[a number]%, where [a number] is a positive integer.
Text size	S:[a number]%, where [a number] is a positive integer.
Text alignment	A:start or A:middle or A:end

Cue setting example:
WEBVTT FILE

01:23:45.678 --> 01:23:46.789 D:vertical
Hello world!

01:23:48.910 --> 01:23:49.101 S:50%
Hello
world!

Replacements

& has to be replaced with &amp;
< has to be replaced with &lt;
> has to be replaced with &gt;

Color declaration tags
<c.lime>Hey!</c>

Voice declaration tags

<v.Name>
Example:
01:23:45.678 --> 01:23:46.789
- <v.John>Hey!</v>
- <v.Jane>Hey!</v>
}

type

  TUWWebVTTColor = record
    Name  : String;
    Color : String;
  end;

const

  TUWWebVTTDefColors : array[0..7] of TUWWebVTTColor =
  (
    (Name: 'white'; Color: 'ffffff'),
    (Name: 'lime'; Color: '00ff00'),
    (Name: 'cyan'; Color: 'ffff00'),
    (Name: 'red'; Color: '0000ff'),
    (Name: 'yellow'; Color: '00ffff'),
    (Name: 'magenta'; Color: 'ff00ff'),
    (Name: 'blue'; Color: 'ff0000'),
    (Name: 'black'; Color: '000000')
  );

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function VTTPosFrom(const Sub, S: String; const From: Integer): Integer;
var
  P: Integer;
begin
  Result := 0;
  if (From < 1) or (From > Length(S)) then Exit;
  P := Pos(Sub, Copy(S, From, MaxInt));
  if P > 0 then Result := P + From - 1;
end;

// -----------------------------------------------------------------------------

function VTTIsDigits(const S: String; const MinLen, MaxLen: Integer): Boolean;
var
  i: Integer;
begin
  Result := (Length(S) >= MinLen) and (Length(S) <= MaxLen);
  for i := 1 to Length(S) do
    if not (S[i] in ['0'..'9']) then Exit(False);
end;

// -----------------------------------------------------------------------------

// '[hh:]mm:ss.ttt' -> ms. Antes solo se leía 'hh:mm:ss.zzz' de 12 caracteres:
// las horas son opcionales ('01:05.000' es válido y lo escriben YouTube, Vimeo y HLS)
function VTTParseTime(const S: String; out MS: Integer): Boolean;
var
  T, Head, HStr, MStr, SStr, MsStr: String;
  p, c1, c2: Integer;
begin
  Result := False;
  MS := -1;
  T := Trim(S);
  p := LastDelimiter('.,', T);
  if p < 2 then Exit;
  Head  := Copy(T, 1, p - 1);
  MsStr := Copy(T, p + 1, MaxInt);

  c1 := Pos(':', Head);
  if c1 = 0 then Exit;
  c2 := VTTPosFrom(':', Head, c1 + 1);
  if c2 = 0 then
  begin
    HStr := '0';
    MStr := Copy(Head, 1, c1 - 1);
    SStr := Copy(Head, c1 + 1, MaxInt);
  end
  else
  begin
    HStr := Copy(Head, 1, c1 - 1);
    MStr := Copy(Head, c1 + 1, c2 - c1 - 1);
    SStr := Copy(Head, c2 + 1, MaxInt);
  end;

  if not (VTTIsDigits(HStr, 1, 3) and VTTIsDigits(MStr, 1, 2) and
          VTTIsDigits(SStr, 1, 2) and VTTIsDigits(MsStr, 1, 3)) then Exit;

  while Length(MsStr) < 3 do MsStr := MsStr + '0';
  MS := ((StrToInt(HStr) * 60 + StrToInt(MStr)) * 60 + StrToInt(SStr)) * 1000 + StrToInt(MsStr);
  Result := True;
end;

// -----------------------------------------------------------------------------

// Número de un ajuste de cue ('50%', '-1', '12.5%'); Def si no es un número.
function VTTNum(const S: String; const Def: Integer): Integer;
var
  T: String;
  F: Double;
  FS: TFormatSettings;
begin
  T := Trim(S);
  if (T <> '') and (T[Length(T)] = '%') then SetLength(T, Length(T) - 1);
  if TryStrToInt(T, Result) then Exit;
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  if TryStrToFloat(T, F, FS) then
    Result := Round(F)
  else
    Result := Def;
end;

// -----------------------------------------------------------------------------

// Valor de la propiedad AName ('color') dentro del cuerpo de una regla CSS.
function VTTCSSProperty(const Body, AName: String): String;
var
  S, Decl, Prop: String;
  p: Integer;
begin
  Result := '';
  S := Body;
  while S <> '' do
  begin
    p := Pos(';', S);
    if p = 0 then
    begin
      Decl := S;
      S := '';
    end
    else
    begin
      Decl := Copy(S, 1, p - 1);
      Delete(S, 1, p);
    end;
    p := Pos(':', Decl);
    if p = 0 then Continue;
    Prop := LowerCase(Trim(Copy(Decl, 1, p - 1)));
    if Prop = AName then Exit(Trim(Copy(Decl, p + 1, MaxInt)));
  end;
end;

// -----------------------------------------------------------------------------

function VTTIsHex6(const S: String): Boolean;
var
  i: Integer;
begin
  Result := Length(S) = 6;
  for i := 1 to Length(S) do
    if not (S[i] in ['0'..'9', 'A'..'F', 'a'..'f']) then Exit(False);
end;

// -----------------------------------------------------------------------------

// Color CSS ('rgb(255, 0, 0)', 'rgb(255 0 0)', '#f00', '#ff0000', 'yellow') ->
// 'bbggrr' (el orden del formato interno). '' si no es un color válido.
function VTTCSSColorToBGR(const AValue: String): String;
var
  V, Inner, Part: String;
  p, q, n: Integer;
  C: array[0..2] of Integer;
begin
  Result := '';
  V := LowerCase(Trim(AValue));
  if V = '' then Exit;

  if (Copy(V, 1, 4) = 'rgb(') or (Copy(V, 1, 5) = 'rgba(') then
  begin
    p := Pos('(', V);
    q := Pos(')', V);
    if q <= p then Exit;
    Inner := Copy(V, p + 1, q - p - 1);
    Inner := ReplaceString(ReplaceString(Inner, '/', ','), ' ', ',');
    n := 0;
    while (Inner <> '') and (n < 3) do
    begin
      p := Pos(',', Inner);
      if p = 0 then
      begin
        Part := Inner;
        Inner := '';
      end
      else
      begin
        Part := Copy(Inner, 1, p - 1);
        Delete(Inner, 1, p);
      end;
      Part := Trim(Part);
      if Part = '' then Continue;
      C[n] := StrToIntDef(Part, -1);
      if (C[n] < 0) or (C[n] > 255) then Exit;
      Inc(n);
    end;
    if n < 3 then Exit;
    Result := LowerCase(IntToHex(C[2], 2) + IntToHex(C[1], 2) + IntToHex(C[0], 2));
  end
  else if (Length(V) = 4) and (V[1] = '#') then // #rgb
    Result := V[4] + V[4] + V[3] + V[3] + V[2] + V[2]
  else if (Length(V) = 7) and (V[1] = '#') and VTTIsHex6(Copy(V, 2, 6)) then // #rrggbb
    Result := Copy(V, 6, 2) + Copy(V, 4, 2) + Copy(V, 2, 2)
  else
    for n := 0 to High(TUWWebVTTDefColors) do
      if V = TUWWebVTTDefColors[n].Name then
        Exit(TUWWebVTTDefColors[n].Color);
end;

// -----------------------------------------------------------------------------

// ¿Es una etiqueta válida del texto de un cue de WebVTT (c, i, b, u, v, lang,
// ruby, rt) o una marca de tiempo '<00:01.000>'? Esas no se escapan al escribir.
function VTTIsKnownTag(const T: String): Boolean;
var
  Body, Nm: String;
  k: Integer;
begin
  Result := False;
  if (Length(T) < 3) or (T[1] <> '<') or (T[Length(T)] <> '>') then Exit;
  Body := Copy(T, 2, Length(T) - 2);

  if Body[1] in ['0'..'9'] then
  begin
    Result := True;
    for k := 1 to Length(Body) do
      if not (Body[k] in ['0'..'9', ':', '.']) then Exit(False);
    Exit;
  end;

  if Body[1] = '/' then Delete(Body, 1, 1);
  k := 1;
  while (k <= Length(Body)) and (Body[k] in ['a'..'z', 'A'..'Z']) do Inc(k);
  Nm := LowerCase(Copy(Body, 1, k - 1));
  if (k <= Length(Body)) and not (Body[k] in ['.', ' ', #9]) then Exit;
  Result := (Nm = 'c') or (Nm = 'i') or (Nm = 'b') or (Nm = 'u') or (Nm = 'v') or
            (Nm = 'lang') or (Nm = 'ruby') or (Nm = 'rt');
end;

// -----------------------------------------------------------------------------

// Antes el texto se escribía tal cual: 'a < b' abría una etiqueta y rompía el
// cue. Se escapan '&', '<' y '>' respetando las etiquetas de WebVTT y los tags TS.
function VTTEscapeText(const S: String): String;
var
  i, j: Integer;
begin
  Result := '';
  i := 1;
  while i <= Length(S) do
  begin
    case S[i] of
      '&': Result := Result + '&amp;';
      '>': Result := Result + '&gt;';
      '{':
        if (i < Length(S)) and (S[i + 1] = '\') and (VTTPosFrom('}', S, i + 2) > 0) then
        begin
          j := VTTPosFrom('}', S, i + 2);
          Result := Result + Copy(S, i, j - i + 1);
          i := j;
        end
        else
          Result := Result + '{';
      '<':
        begin
          j := VTTPosFrom('>', S, i + 1);
          if (j > 0) and VTTIsKnownTag(Copy(S, i, j - i + 1)) then
          begin
            Result := Result + Copy(S, i, j - i + 1);
            i := j;
          end
          else
            Result := Result + '&lt;';
        end;
    else
      Result := Result + S[i];
    end;
    Inc(i);
  end;
end;

// -----------------------------------------------------------------------------

// '&', '<' y '>' en el nombre de una voz (<v Nombre>).
function VTTEscapeAll(const S: String): String;
begin
  Result := ReplaceString(S, '&', '&amp;');
  Result := ReplaceString(Result, '<', '&lt;');
  Result := ReplaceString(Result, '>', '&gt;');
  Result := ReplaceString(ReplaceString(Result, #13, ' '), #10, ' ');
end;

// -----------------------------------------------------------------------------

{ TUWWebVTT }

// -----------------------------------------------------------------------------

procedure TUWWebVTT.ResetColorsToDefault;
var
  i: Integer;
begin
  FColors.Clear;
  for i := 0 to High(TUWWebVTTDefColors) do
    FColors.AddPair(TUWWebVTTDefColors[i].Name, TUWWebVTTDefColors[i].Color);
end;

// -----------------------------------------------------------------------------

constructor TUWWebVTT.Create(const AShouldWriteBOM: Boolean = False);
begin
  inherited Create(AShouldWriteBOM);

  FColors := TStringList.Create;
  FColors.Duplicates := dupIgnore;
  ResetColorsToDefault;
end;

// -----------------------------------------------------------------------------

destructor TUWWebVTT.Destroy;
begin
  FColors.Free;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.FromRepl(ARegExpr: TRegExpr): RegExprString;
var
  i, p: Integer;
  s, Cls: String;
begin
  // '<c.yellow>' o '<c.yellow.bg_blue>': se usa la primera clase con color.
  // Antes se buscaba el texto completo y salía '{\c&yellow.bg_blue&}' como basura
  Result := '';
  s := ARegExpr.Match[1];
  while s <> '' do
  begin
    p := Pos('.', s);
    if p > 0 then
    begin
      Cls := Copy(s, 1, p - 1);
      Delete(s, 1, p);
    end
    else
    begin
      Cls := s;
      s := '';
    end;

    i := FColors.IndexOfName(Cls);
    if i >= 0 then Exit('{\c&' + FColors.ValueFromIndex[i] + '&}');

    // nombres escritos por versiones anteriores ('bbggrr') o actuales ('c' + hex)
    if VTTIsHex6(Cls) then Exit('{\c&' + Cls + '&}');
    if (Length(Cls) = 7) and (Cls[1] = 'c') and VTTIsHex6(Copy(Cls, 2, 6)) then
      Exit('{\c&' + Copy(Cls, 2, 6) + '&}');
  end;
end;

// -----------------------------------------------------------------------------

// Reglas '::cue(.nombre) { color: ... }' de un bloque STYLE. Antes la regex
// exigía los espacios exactos de 'rgb(r,g,b)', no entendía '#rrggbb' ni
// nombres, y guardaba el color en orden RGB en vez del BGR interno.
procedure TUWWebVTT.ParseCueStyles(const CSS: String);
var
  a, b, c, d, p, k: Integer;
  Sel, Body, Nm, Val, Hex: String;
begin
  p := 1;
  while True do
  begin
    a := VTTPosFrom('::cue(', CSS, p);
    if a = 0 then Break;
    b := VTTPosFrom(')', CSS, a);
    if b = 0 then Break;
    c := VTTPosFrom('{', CSS, b);
    if c = 0 then Break;
    d := VTTPosFrom('}', CSS, c);
    if d = 0 then Break;

    Sel  := Trim(Copy(CSS, a + 6, b - a - 6));
    Body := Copy(CSS, c + 1, d - c - 1);
    p := d + 1;

    // Solo reglas por clase simple: ::cue(.nombre) o ::cue(c.nombre)
    k := Pos('.', Sel);
    if k = 0 then Continue;
    Nm := Trim(Copy(Sel, k + 1, MaxInt));
    if (Nm = '') or (Pos('.', Nm) > 0) or (Pos(' ', Nm) > 0) then Continue;

    Val := VTTCSSProperty(Body, 'color');
    k := Pos('!', Val);
    if k > 0 then Val := Trim(Copy(Val, 1, k - 1));
    Hex := VTTCSSColorToBGR(Val);
    if Hex = '' then Continue;

    k := FColors.IndexOfName(Nm);
    if k >= 0 then
      FColors.ValueFromIndex[k] := Hex
    else
      FColors.AddPair(Nm, Hex);
  end;
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.TagsToTS(const Text: String): String;
begin
  Result := Text;
  if Result.IsEmpty then Exit;

  if Pos('<c.', Result) > 0 then
    with TRegExpr.Create('\<c\.(.*?)\>') do
    try
       Result := Replace(Result, @FromRepl);
    finally
      Free;
    end;

  Result := ReplaceString(Result, '</c>', '{\c}');
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.ToRepl(ARegExpr: TRegExpr): RegExprString;
var
  i: Integer;
  s, Nm: String;
  b: Boolean;
begin
  s := ARegExpr.Match[1];
  if (s <> '') and (UpCase(s[1]) = 'H') then Delete(s, 1, 1); // '&H0000FF&' (ASS)
  if not VTTIsHex6(s) then
  begin
    Result := ''; // color que no se puede escribir: sin etiqueta
    Exit;
  end;

  b := False;
  Nm := '';
  for i := 0 to FColors.Count-1 do
    if SameText(s, FColors.ValueFromIndex[i]) then
    begin
      Nm := FColors.Names[i];
      b := True;
      Break;
    end;

  if not b then
  begin
    // Antes: '.00a5ff' no es una clase CSS válida (no puede empezar con dígito)
    Nm := 'c' + LowerCase(s);
    FColors.AddPair(Nm, LowerCase(s));
  end;

  Result := '<c.' + Nm + '>';
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.TSToTags(const Text: String): String;
begin
  Result := Text;
  if Result.IsEmpty then Exit;

  if Pos('{\c&', Result) > 0 then
    with TRegExpr.Create('\{\\c&(.*?)\&}') do
    try
       Result := Replace(Result, @ToRepl);
    finally
      Free;
    end;

  Result := ReplaceString(Result, '{\c}', '</c>');
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfWebVTT;
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.Extension: String;
begin
  Result := '*.vtt;*.webvtt';
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.HasStyleSupport: Boolean;
begin
  Result := True; // Has tags and coords
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
var
  L: String;
begin
  Result := False;
  if SubtitleFile.Count = 0 then Exit;
  L := SubtitleFile[0];
  if Copy(L, 1, 3) = #$EF#$BB#$BF then Delete(L, 1, 3); // BOM UTF-8
  // Antes se exigía la línea exacta 'WEBVTT' ('WEBVTT - Título' no se detectaba)
  Result := (Copy(L, 1, 6) = 'WEBVTT') and ((Length(L) = 6) or (L[7] in [' ', #9]));
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i, e, j, k, n, x, t, p: Integer;
  ExtraTime, InitialTime, FinalTime, F: Integer;
  Settings, Text, Voice, Tok, Nm, Val, Rest, First, Block, S: String;
  ExtraInfo: PWebVTT_ExtraInfo;
  HasAlign, HasLine, LineIsPct: Boolean;
  AlignC, LineVal: Integer;
  TheV: TSubtitleVAlign;

  function IsKeyword(const L, Kw: String): Boolean;
  begin
    Result := (L = Kw) or ((Length(L) > Length(Kw)) and (Copy(L, 1, Length(Kw)) = Kw) and
      (L[Length(Kw) + 1] in [' ', #9]));
  end;

  // ¿La línea Idx abre un bloque? Las líneas vacías no llegan acá, pero la
  // lista recuerda cuáles venían después de una. Sin esa información se
  // aproxima: un cue empieza en su línea de tiempos o en el identificador
  // que la precede, y NOTE/STYLE/REGION abren sus propios bloques
  function BlockStartAt(const Idx: Integer): Boolean;
  begin
    if SubtitleFile.HasBlockInfo then
      Result := SubtitleFile.BlockStart(Idx)
    else
      Result := (Pos('-->', SubtitleFile[Idx]) > 0) or
      ((Idx + 1 < SubtitleFile.Count) and (Pos('-->', SubtitleFile[Idx + 1]) > 0)) or
      IsKeyword(SubtitleFile[Idx], 'NOTE') or IsKeyword(SubtitleFile[Idx], 'STYLE') or
      IsKeyword(SubtitleFile[Idx], 'REGION');
  end;

  function JoinLines(const A, B: Integer): String; // líneas A..B
  var
    m: Integer;
  begin
    Result := '';
    for m := A to B do
      if Result = '' then
        Result := SubtitleFile[m]
      else
        Result := Result + sLineBreak + SubtitleFile[m];
  end;

  // Antes la regex solo admitía voces de una palabra ('<v Mr. Smith>' quedaba
  // como texto) y reemplazaba todo el texto por lo que hubiera entre el primer
  // <v> y el último </v>. Se toma la voz del principio y se quitan las demás.
  function ExtractVoice(var T: String): String;
  var
    q, w: Integer;
    Tag: String;
  begin
    Result := '';
    if (Length(T) < 3) or (Copy(T, 1, 2) <> '<v') or not (T[3] in [' ', #9, '.']) then Exit;
    q := Pos('>', T);
    if q = 0 then Exit;
    Tag := Copy(T, 3, q - 3); // '.loud Mr. Smith' o ' Bob'
    w := 1;
    if (Tag <> '') and (Tag[1] = '.') then
      while (w <= Length(Tag)) and not (Tag[w] in [' ', #9]) do Inc(w);
    Result := Trim(Copy(Tag, w, MaxInt));
    Delete(T, 1, q);
    if (Pos('<v', T) > 0) or (Pos('</v', T) > 0) then
      with TRegExpr.Create('</?v(?:\.[^\s>]*)*(?:\s[^>]*)?>') do
      try
        T := Replace(T, '', False);
      finally
        Free;
      end;
  end;

begin
  Result := False;

  // Antes los colores de un archivo quedaban para los siguientes
  ResetColorsToDefault;
  try
    n := SubtitleFile.Count;
    ExtraTime := 0;

    // Cabecera: la línea 0 (WEBVTT ...) y lo que siga hasta el primer bloque.
    // (Antes se leía SubtitleFile[1] a ciegas, fuera de rango con una sola línea)
    j := 1;
    while (j < n) and not BlockStartAt(j) do Inc(j);
    with Subtitles.FormatProperties^.WebVTT do
      if UseXTIMESTAMP then
        for k := 1 to j - 1 do
        begin
          S := SubtitleFile[k];
          if Pos('X-TIMESTAMP-MAP=', S) <> 1 then Continue;
          p := Pos('MPEGTS:', S);
          if p > 0 then
          begin
            Val := Copy(S, p + 7, MaxInt);
            x := Pos(',', Val);
            if x > 0 then Val := Copy(Val, 1, x - 1);
            MPEGTS := StrToIntDef(Trim(Val), 0);
          end;
          p := Pos('LOCAL:', S);
          if p > 0 then
          begin
            Val := Trim(Copy(S, p + 6, MaxInt));
            x := Pos(',', Val);
            if x > 0 then Val := Copy(Val, 1, x - 1);
            if VTTParseTime(Val, F) then
            begin
              LOCAL := F;
              ExtraTime := F;
            end;
          end;
        end;

    // Bloques: NOTE y REGION se ignoran, STYLE se lee por los colores, y el
    // resto son cues, con o sin identificador (cualquier texto, no solo
    // números). Antes un NOTE o un identificador no numérico se pegaba al
    // texto del cue anterior, y un texto numérico cortaba el cue.
    i := j;
    while i < n do
    begin
      e := i + 1;
      while (e < n) and not BlockStartAt(e) do Inc(e);

      First := SubtitleFile[i];
      if IsKeyword(First, 'NOTE') or IsKeyword(First, 'REGION') then
      begin
        // sin texto que cargar
      end
      else if IsKeyword(First, 'STYLE') then
        ParseCueStyles(JoinLines(i + 1, e - 1))
      else
      begin
        t := -1;
        if Pos('-->', SubtitleFile[i]) > 0 then
          t := i
        else if (i + 1 < e) and (Pos('-->', SubtitleFile[i + 1]) > 0) then
          t := i + 1;

        if t < 0 then
        begin
          Block := JoinLines(i, e - 1);
          if Pos('::cue(', Block) > 0 then ParseCueStyles(Block);
        end
        else
        begin
          S := SubtitleFile[t];
          p := Pos('-->', S);
          Rest := Trim(Copy(S, p + 3, MaxInt));
          k := 1;
          while (k <= Length(Rest)) and not (Rest[k] in [' ', #9]) do Inc(k);
          Settings := Trim(Copy(Rest, k + 1, MaxInt));
          Rest := Copy(Rest, 1, k - 1);

          if VTTParseTime(Trim(Copy(S, 1, p - 1)), InitialTime) and
             VTTParseTime(Rest, FinalTime) then
          begin
            Inc(InitialTime, ExtraTime);
            Inc(FinalTime, ExtraTime);

            if (InitialTime >= 0) and (FinalTime > 0) then
            begin
              // Ajustes del cue por pares 'nombre:valor'. Antes se buscaba 'align'
              // o 'line' a ojo dentro de la línea y solo se entendía left/center/right
              HasAlign := False; HasLine := False; LineIsPct := False;
              AlignC := 0; LineVal := 0;

              while Settings <> '' do
              begin
                k := 1;
                while (k <= Length(Settings)) and not (Settings[k] in [' ', #9]) do Inc(k);
                Tok := Copy(Settings, 1, k - 1);
                Settings := TrimLeft(Copy(Settings, k + 1, MaxInt));

                p := Pos(':', Tok);
                if p < 2 then Continue;
                Nm := LowerCase(Copy(Tok, 1, p - 1));
                Val := Copy(Tok, p + 1, MaxInt);
                p := Pos(',', Val); // 'line:10%,end'
                if p > 0 then Val := Copy(Val, 1, p - 1);

                if Nm = 'align' then
                begin
                  Val := LowerCase(Val);
                  HasAlign := True;
                  if (Val = 'left') or (Val = 'start') then AlignC := 1
                  else if (Val = 'center') or (Val = 'middle') then AlignC := 2
                  else if (Val = 'right') or (Val = 'end') then AlignC := 3
                  else AlignC := 0;
                end
                else if Nm = 'line' then
                begin
                  HasLine := True;
                  LineIsPct := (Val <> '') and (Val[Length(Val)] = '%');
                  LineVal := VTTNum(Val, 0);
                end;
              end;

              ExtraInfo := NIL;
              if HasAlign or HasLine then
              begin
                // Antes LinePos quedaba sin inicializar cuando solo había 'align'
                New(ExtraInfo);
                ExtraInfo^.LinePos := 0;
                case AlignC of
                  1: ExtraInfo^.TextAlign := 'left';
                  2: ExtraInfo^.TextAlign := 'center';
                  3: ExtraInfo^.TextAlign := 'right';
                else
                  ExtraInfo^.TextAlign := '';
                end;
                // 'line:N' sin % es un número de línea, no un porcentaje
                if HasLine and LineIsPct then ExtraInfo^.LinePos := LineVal;
              end;

              // Con '%': 0 es arriba y 100 abajo. Sin '%': número de línea
              // (0 y positivas desde arriba, negativas desde abajo). Antes
              // 'line:0' caía abajo, y 40 y 70 exactos también.
              TheV := svaBottom;
              if HasLine then
              begin
                if LineIsPct then
                begin
                  if LineVal < 40 then
                    TheV := svaTop
                  else if LineVal < 70 then
                    TheV := svaCenter;
                end
                else if (LineVal >= 0) and (LineVal < 40) then
                  TheV := svaTop;
              end;

              Text := JoinLines(t + 1, e - 1);
              Voice := ExtractVoice(Text);

              // Final text
              Text := TagsToTS(HTMLTagsToTS(HTMLDecode(Text)));
              x := Subtitles.Add(InitialTime, FinalTime, Text, '', ExtraInfo);
              Subtitles.ItemPointer[x]^.Actor := Voice;
              Subtitles.ItemPointer[x]^.Align := TSubtitleHAlign(AlignC);
              Subtitles.ItemPointer[x]^.VAlign := TheV;
            end;
          end;
        end;
      end;
      i := e;
    end;
  finally
    if Subtitles.Count > 0 then
    begin
      Subtitles.ExtraInfoType := eiWebVTT;
      Result := True;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i, c, k, StyleAt, T1, T2 : Integer;
  Text : String;
  Align : String;
  SL, Lines : TStringList;
  HasWrittenLine : Boolean;
  b : Boolean;
  Rx : TRegExpr;
begin
  Result := False;

  StringList.Add('WEBVTT');

  with Subtitles.FormatProperties^.WebVTT do
    if UseXTIMESTAMP then
      StringList.Add(SysUtils.Format('X-TIMESTAMP-MAP=MPEGTS:%d,LOCAL:%s', [MPEGTS, TimeToString(LOCAL, 'hh:mm:ss.zzz')]));

  StringList.Add('');

  // Antes STYLE se insertaba en la posición fija 2, que con X-TIMESTAMP-MAP
  // cae dentro de la cabecera y el navegador lo ignoraba
  StyleAt := StringList.Count;

  Lines := TStringList.Create;
  Rx := TRegExpr.Create('\{\\[^}]*\}');
  try
    for i := FromItem to ToItem do
    begin
      Align := '';

      case Subtitles[i].Align of
        shaLeft   : Align := ' align:left';
        shaCenter : Align := ' align:center';
        shaRight  : Align := ' align:right';
      end;

      // line: primero el valor exacto del archivo WebVTT de origen
      HasWrittenLine := False;
      if (Subtitles.ExtraInfoType = eiWebVTT) and (Subtitles.ExtraInfo[i] <> NIL) then
        with PWebVTT_ExtraInfo(Subtitles.ExtraInfo[i])^ do
          if (LinePos <> 0) and
            (((LinePos < 40) and (Subtitles[i].VAlign = svaTop)) or
             ((LinePos >= 40) and (LinePos < 70) and (Subtitles[i].VAlign = svaCenter)) or
             ((LinePos >= 70) and (Subtitles[i].VAlign = svaBottom))) then
          begin
            Align := Align + SysUtils.Format(' line:%d%%', [LinePos]);
            HasWrittenLine := True;
          end;

      if (not HasWrittenLine) and (Subtitles[i].VAlign <> svaBottom) then
        case Subtitles[i].VAlign of
          svaCenter : Align := Align + ' line:50%';
          svaTop    : Align := Align + ' line:10%';
        end;

      if Subtitles.FormatProperties^.WebVTT.WriteCueIdentifiers then
        StringList.Add((i+1).ToString);

      // Un tiempo negativo saldría como '-00:00:01.000'
      T1 := Subtitles.InitialTime[i];
      T2 := Subtitles.FinalTime[i];
      if T1 < 0 then T1 := 0;
      if T2 < 0 then T2 := 0;
      StringList.Add(TimeToString(T1, 'hh:mm:ss.zzz') + ' --> ' + TimeToString(T2, 'hh:mm:ss.zzz') + Align);

      // '&', '<' y '>' escapados; los tags sin equivalente se quitan y las
      // líneas vacías también (una línea en blanco termina el cue)
      Text := TSTagsToHTML(TSToTags(VTTEscapeText(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]))));
      Text := Rx.Replace(Text, '', False);

      Lines.Text := Text;
      Text := '';
      for k := 0 to Lines.Count - 1 do
        if Trim(Lines[k]) <> '' then
        begin
          if Text <> '' then Text := Text + sLineBreak;
          Text := Text + Lines[k];
        end;

      if (Text <> '') and (not Subtitles[i].Actor.IsEmpty) then
        Text := '<v ' + VTTEscapeAll(Subtitles[i].Actor) + '>' + Text + '</v>';

      if Text <> '' then StringList.Add(Text);
      StringList.Add('');
    end;
  finally
    Rx.Free;
    Lines.Free;
  end;

  SL := TStringList.Create;
  try
    SL.Duplicates := dupIgnore;
    SL.Clear;

    for i := 0 to FColors.Count-1 do
    begin
      b := False;
      for c := 0 to High(TUWWebVTTDefColors) do
        if FColors.Names[i] = TUWWebVTTDefColors[c].Name then
        begin
          b := True;
          Break;
        end;

      // El valor interno es 'bbggrr': antes se escribía en ese orden dentro de
      // rgb(R,G,B) y el rojo salía azul
      if not b then
        SL.Add(
          SysUtils.Format('::cue(.%s) { color:rgb(%d,%d,%d) }',
            [FColors.Names[i], HexToByte(Copy(FColors.ValueFromIndex[i], 5, 2)), HexToByte(Copy(FColors.ValueFromIndex[i], 3, 2)), HexToByte(Copy(FColors.ValueFromIndex[i], 1, 2))])
        );
    end;

    if SL.Count > 0 then
    begin
      SL.Insert(0, 'STYLE');
      StringList.Insert(StyleAt, SL.Text, False);
    end;
  finally
    SL.Free;
  end;

  try
    StringList.SaveToFile(FileName, TEncoding.UTF8); // must be encoded using UTF-8
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
