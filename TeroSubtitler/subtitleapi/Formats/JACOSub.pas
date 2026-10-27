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

unit JACOSub;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWJACOSub }

  TUWJACOSub = class(TUWSubtitleCustomFormat)
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

{ ============================================================================
  Reescritura completa contra la especificación oficial de JACOsub
  (http://unicorn.us.com/jacosub/jscripts.html), consultada porque nuestra
  confianza previa en la sintaxis exacta de este formato era más baja que
  para ASS/TTML (donde pudimos cruzar contra código ya existente en este
  mismo proyecto). Antes:
    - La carga descartaba TODO lo anterior al primer '' de la línea —
      pero en JACOsub real las llaves {} son solo COMENTARIOS opcionales,
      no delimitan el bloque de directivos (que no llevan llaves). Una
      línea válida sin comentario, como '0:00:10.11 0:00:12.00 D Its
      alive!' (ejemplo textual de la propia especificación), comía casi
      toda la línea con el código anterior.
    - El guardado hacía RemoveTSTags(Text) — borraba negrita/cursiva/
      subrayado sin más, en vez de convertirlos a la sintaxis propia de
      JACOsub (\I \i \B \b \U \u).
  Alcance de esta reescritura: de los ~30 directivos de posición/fuente/
  color/efectos que define el formato, se reconocen los 6 que tienen un
  equivalente directo y sin ambigüedad en nuestro modelo de datos: VT/VM/
  VB (alineación vertical) y JL/JC/JR (alineación horizontal). El resto
  (márgenes, fuente, color por índice de paleta, efectos de transición)
  no tiene un lugar natural en TUWSubtitleItem y se ignora sin intentar
  adivinar una equivalencia — en particular, \Cn es un ÍNDICE de paleta,
  no un color RGB directo, así que no se traduce (inventar una
  equivalencia falsa sería peor que no soportarlo).
  ============================================================================ }

// -----------------------------------------------------------------------------

{ Encuentra dónde termina el directivo (si lo hay) y empieza el texto real.
  Por especificación: el texto debe empezar con un carácter NO alfabético
  si se omite el directivo; el directivo en sí es una corrida de códigos
  sin espacios que termina en el primer espacio/tab. }
procedure SplitJACOSubDirective(const S: String; out DirectivePart, TextPart: String);
var
  P: Integer;
begin
  DirectivePart := '';
  TextPart := S;
  if (Length(S) = 0) or not (S[1] in ['A'..'Z', 'a'..'z']) then Exit;

  P := 1;
  while (P <= Length(S)) and not (S[P] in [' ', #9]) do Inc(P);
  DirectivePart := Copy(S, 1, P - 1);

  while (P <= Length(S)) and (S[P] in [' ', #9]) do Inc(P);
  TextPart := Copy(S, P, MaxInt);
end;

// -----------------------------------------------------------------------------

{ VT/VM/VB y JL/JC/JR: son sub-strings de un directivo que puede traer
  varios códigos concatenados sin separador (ej. 'CF1VT'). No hay
  ambigüedad real: ningún otro código de la especificación contiene estas
  dos letras como substring. }
procedure ParseJACOSubDirective(const D: String; out VA: TSubtitleVAlign; out AL: TSubtitleHAlign);
var
  U: String;
begin
  // Sin directivo explícito se deja el default del ítem (abajo, sin
  // alineación horizontal propia).
  VA := svaBottom;
  AL := shaNone;

  U := UpperCase(D);
  if Pos('VT', U) > 0 then VA := svaTop
  else if Pos('VM', U) > 0 then VA := svaCenter
  else if Pos('VB', U) > 0 then VA := svaBottom;

  if Pos('JL', U) > 0 then AL := shaLeft
  else if Pos('JR', U) > 0 then AL := shaRight
  else if Pos('JC', U) > 0 then AL := shaCenter;
end;

// -----------------------------------------------------------------------------

function JACOSubTextToTS(const S: String): String;
var
  i: Integer;
  BoldOn, ItalicOn, UnderlineOn: Boolean;

  procedure CloseAll;
  begin
    if BoldOn then Result := Result + '{\b0}';
    if ItalicOn then Result := Result + '{\i0}';
    if UnderlineOn then Result := Result + '{\u0}';
    BoldOn := False; ItalicOn := False; UnderlineOn := False;
  end;

begin
  Result := '';
  BoldOn := False; ItalicOn := False; UnderlineOn := False;
  i := 1;
  while i <= Length(S) do
  begin
    if (S[i] = '\') and (i < Length(S)) then
    begin
      // \Cn (color por índice de paleta) y \Fn (número de fuente) llevan
      // un parámetro obligatorio de 1 carácter: son 3 caracteres en
      // total, no 2. Sin este caso aparte, el carácter de parámetro
      // quedaba como texto literal visible.
      if (S[i+1] in ['C', 'c', 'F', 'f']) and (i+2 <= Length(S)) then
      begin
        Inc(i, 3);
        Continue;
      end;

      case S[i+1] of
        'n': Result := Result + LineEnding;
        'N': CloseAll;
        'I': if not ItalicOn    then begin Result := Result + '{\i1}'; ItalicOn := True; end;
        'i': if ItalicOn        then begin Result := Result + '{\i0}'; ItalicOn := False; end;
        'B': if not BoldOn      then begin Result := Result + '{\b1}'; BoldOn := True; end;
        'b': if BoldOn          then begin Result := Result + '{\b0}'; BoldOn := False; end;
        'U': if not UnderlineOn then begin Result := Result + '{\u1}'; UnderlineOn := True; end;
        'u': if UnderlineOn     then begin Result := Result + '{\u0}'; UnderlineOn := False; end;
        '\': Result := Result + '\';
        '{': Result := Result + '{';
        '~': Result := Result + '~';
        // \D \T (fecha/hora actuales): se consumen los 2 caracteres sin
        // insertar nada (son dinámicos, no tienen representación
        // estática razonable).
      end;
      Inc(i, 2);
      Continue;
    end;

    if S[i] = '{' then
    begin
      // Comentario: se descarta hasta el '}' que lo cierra. Por
      // especificación, si un espacio/tab sigue INMEDIATAMENTE al '}' de
      // cierre, ESE espacio también se descarta (uno solo).
      Inc(i);
      while (i <= Length(S)) and (S[i] <> '}') do Inc(i);
      Inc(i);
      if (i <= Length(S)) and (S[i] in [' ', #9]) then Inc(i);
      Continue;
    end;

    if S[i] = '~' then
    begin
      Result := Result + ' ';
      Inc(i);
      Continue;
    end;

    Result := Result + S[i];
    Inc(i);
  end;

  // El formato es por línea, no se acumula entre líneas — si quedó algo
  // abierto (el autor no puso \N al final), se cierra acá.
  CloseAll;
end;

// -----------------------------------------------------------------------------

{ Tags TS -> sintaxis JACOsub. Color/tachado y cualquier otro tag TS sin
  equivalente confiable se descartan (RemoveTSTags), en vez de dejarlos
  como texto literal visible. }
function TSToJACOSubText(const S: String): String;
begin
  Result := S;
  Result := ReplaceString(Result, '{\b1}', '\B', True, True);
  Result := ReplaceString(Result, '{\b0}', '\b', True, True);
  Result := ReplaceString(Result, '{\i1}', '\I', True, True);
  Result := ReplaceString(Result, '{\i0}', '\i', True, True);
  Result := ReplaceString(Result, '{\u1}', '\U', True, True);
  Result := ReplaceString(Result, '{\u0}', '\u', True, True);
  Result := RemoveTSTags(Result);
  Result := ReplaceString(Result, LineEnding, '\n', True, True);
end;

// -----------------------------------------------------------------------------

function TUWJACOSub.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWJACOSub.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfJACOSub;
end;

// -----------------------------------------------------------------------------

function TUWJACOSub.Extension: String;
begin
  Result := '*.js;*.jss';
end;

// -----------------------------------------------------------------------------

function TUWJACOSub.HasStyleSupport: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWJACOSub.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (TimeInFormat(Copy(SubtitleFile[Row], 1, 10), 'h:mm:ss.zz')) and
     (TimeInFormat(Copy(SubtitleFile[Row], 12, 10), 'h:mm:ss.zz')) and
     (Pos(',', SubtitleFile[Row]) <> 12) and
     (Pos(',', SubtitleFile[Row]) <> 11) and
     (Pos(#9, SubtitleFile[Row]) = 0) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWJACOSub.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Rest, DirectivePart, TextPart, Text: String;
  x : Integer;
  VA: TSubtitleVAlign;
  AL: TSubtitleHAlign;
begin
  Result := False;
  try
    for i := 0 to SubtitleFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 10));
      FinalTime   := StringToTime(Copy(SubtitleFile[i], 12, 10));

      // Antes: Copy(SubtitleFile[i], Pos('}', SubtitleFile[i]) + 2, ...) —
      // asumía que siempre hay un comentario {..}, pero en JACOsub las llaves
      // son OPCIONALES y no delimitan el directivo: una línea sin comentario
      // ('0:00:10.11 0:00:12.00 D Its alive!') perdía casi todo el texto.
      Rest := TrimLeft(Copy(SubtitleFile[i], 22, MaxInt));
      SplitJACOSubDirective(Rest, DirectivePart, TextPart);
      Text := JACOSubTextToTS(TextPart);

      if (InitialTime >= 0) and (FinalTime > 0) and not Text.IsEmpty then
      begin
        x := Subtitles.Add(InitialTime, FinalTime, Text, '');
        // Antes no se leía NINGÚN directivo (VT/VM/VB, JL/JC/JR)
        ParseJACOSubDirective(DirectivePart, VA, AL);
        if x >= 0 then
        begin
          Subtitles.ItemPointer[x]^.VAlign := VA;
          Subtitles.ItemPointer[x]^.Align  := AL;
        end;
      end;
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWJACOSub.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i    : Integer;
  Text, Directive : String;
begin
  Result  := False;

  StringList.Add('#T100');
  StringList.Add('');
  StringList.Add('# Directive entries');
  StringList.Add('#D');
  for i := 1 to 29 do StringList.Add('#D'+IntToStr(i));
  StringList.Add('');

  for i := FromItem to ToItem do
  begin
    // Antes: RemoveTSTags(Text) — se borraban negrita/cursiva/subrayado en
    // vez de convertirlos a la sintaxis propia de JACOsub (\B \I \U).
    Text := TSToJACOSubText(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));

    // Alineación como directivo (antes se perdía)
    case Subtitles[i].VAlign of
      svaTop    : Directive := 'VT';
      svaCenter : Directive := 'VM';
    else
      Directive := '';
    end;
    case Subtitles[i].Align of
      shaLeft   : Directive := Directive + 'JL';
      shaCenter : Directive := Directive + 'JC';
      shaRight  : Directive := Directive + 'JR';
    end;
    if Directive <> '' then Directive := ' ' + Directive;

    StringList.Add(TimeToString(Subtitles.InitialTime[i], 'h:mm:ss.zz') + ' ' + TimeToString(Subtitles.FinalTime[i], 'h:mm:ss.zz') + Directive + ' {NTP} ' + Text);
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
