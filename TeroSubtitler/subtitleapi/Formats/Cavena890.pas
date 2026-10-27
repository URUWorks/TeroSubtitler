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

unit Cavena890;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, UWSubtitleAPI, UWSubtitleAPI.Formats;

type

  { TUWCavena890 }

  TUWCavena890 = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function IsTextBased: Boolean; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses
  UWSubtitleAPI.ExtraInfo, UWSystem.SysUtils,
  Cavena890.Types;

const
  CAVENA_TEXT_LEN   = 51;
  CAVENA_BLOCK_SIZE = 128;
  CAVENA_HDR_OFFSET = 455;

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function IsKnownLangID(const LangID: Integer): Boolean;
var
  k: Integer;
begin
  Result := False;
  for k := 0 to High(LanguageId) do
    if LanguageId[k].Hex = LangID then Exit(True);
end;

// -----------------------------------------------------------------------------

// El llamador debe liberar el TEncoding devuelto.
// Antes el case usaba numeros decimales (45..47, 34, 36, 39, 40, 41) que no
// corresponden a los LangId* de este formato ($07, $28, $56...): p.ej. Sueco
// ($28 = 40) se decodificaba como GRIEGO (1253).
function GetEncodingFromLangID(LangID: Integer): TEncoding;
var
  CP: Integer;
begin
  case LangID of
    LangIdRussian, LangIdBulgarian: CP := 1251;
    LangIdHebrew:                   CP := 1255;
    LangIdArabic:                   CP := 1256;
    LangIdChineseTraditional:       CP := 950;
    LangIdChineseSimplified:        CP := 936;
  else
    CP := 1252; // ingles, danes, sueco, espanol, frances, italiano, portugues...
  end;

  try
    Result := TEncoding.GetEncoding(CP);
  except
    Result := TEncoding.GetEncoding(1252);
  end;
end;

// -----------------------------------------------------------------------------

function GetLanguage(const Bytes: TBytes): String;
begin
  if Length(Bytes) < 200 then
    Result := ''
  else
    Result := TEncoding.ASCII.GetString(Bytes, 187, 6);
end;

// -----------------------------------------------------------------------------

function FixText(const Bytes: TBytes; const Start: Integer; const TextLength: Integer; const LangID: Integer): String;

  function RemoveNull(const Input: String): String;
  var
    OutputLen, Index : Integer;
    C : Char;
  begin
    SetLength(Result, Length(Input));
    OutputLen := 0;
    for Index := 1 to Length(Input) do
    begin
      C := Input[Index];
      if C <> #0 then
      begin
        Inc(OutputLen);
        Result[OutputLen] := C;
      end;
    end;
    SetLength(Result, OutputLen);
  end;

  // Función auxiliar para mapear bytes de Ruso/Hebreo a string
  function MapSpecialLang(const B: Byte; const LID: Integer): String;
  var
    K: Integer;
  begin
    Result := '';
    if LID = LangIdRussian then
    begin
      for K := 0 to Length(TRussianCodes)-1 do
        if TRussianCodes[K] = B then Exit(TRussianLetters[K]);
    end
    else if LID = LangIdHebrew then
    begin
      for K := 0 to Length(THebrewCodes)-1 do
        if THebrewCodes[K] = B then Exit(THebrewLetters[K]);
    end;
  end;

var
  Encoding   : TEncoding;
  BChar      : TBytes;
  LocalBytes : TBytes;
  I          : Integer;
  SChar      : String;
  SafeLen    : Integer;
begin
  // Defensa en profundidad: esta función no tiene forma de saber si el
  // llamador calculó bien Start/TextLength contra el tamaño real de
  // Bytes (ver el fix del loop en LoadSubtitle, que SÍ tenía este
  // problema). Clampeamos acá también para que Move() nunca lea fuera
  // de rango, sin importar quién la llame.
  SafeLen := TextLength;
  if (Start < 0) or (Start >= Length(Bytes)) then
    SafeLen := 0
  else if Start + SafeLen > Length(Bytes) then
    SafeLen := Length(Bytes) - Start;

  SetLength(LocalBytes, SafeLen);
  if SafeLen > 0 then
    Move(Bytes[Start], LocalBytes[0], SafeLen);

  I := 0;
  // SafeLen (no TextLength): LocalBytes puede haber quedado más chico
  // que TextLength si el clamp de arriba recortó por seguridad.
  while I < SafeLen do
  begin
    // Buscar secuencias UTF-8 rotas exportadas incorrectamente por "otros" programas
    if (I < SafeLen - 2) and (LocalBytes[I] = $E2) and (LocalBytes[I+1] = $80) then
    begin
      case LocalBytes[I+2] of
        $98, $99, $B2, $B3:
        begin
          LocalBytes[I]   := $27;
          LocalBytes[I+1] := $00;
          LocalBytes[I+2] := $00;
        end;
        $9C, $9D:
        begin
          LocalBytes[I]   := $22;
          LocalBytes[I+1] := $00;
          LocalBytes[I+2] := $00;
        end;
        $93, $94:
        begin
          LocalBytes[I]   := $2D;
          LocalBytes[I+1] := $00;
          LocalBytes[I+2] := $00;
        end;
      end;
      Inc(I, 2);
    end
    else
    begin
      // Buscar bytes anómalos individuales
      case LocalBytes[I] of
        $91, $92, $B4, $60, $A8, $B8: LocalBytes[I] := $27;
        $93, $94, $AB, $BB:           LocalBytes[I] := $22;
        $96, $97:                     LocalBytes[I] := $2D;
      end;
    end;
    Inc(I);
  end;

  Encoding := GetEncodingFromLangID(LangID);

  // Extracción de caracteres según el Idioma
  if (LangID = LangIdRussian) or (LangID = LangIdHebrew) then
  begin
    Result := '';
    for I := 0 to SafeLen - 1 do
    begin
      if LocalBytes[I] = $00 then Continue;
      SChar := MapSpecialLang(LocalBytes[I], LangID);
      if SChar <> '' then
        Result := Result + SChar
      else
      begin
        SetLength(BChar, 1);
        BChar[0] := LocalBytes[I];
        Result := Result + Encoding.GetString(BChar);
      end;
    end;
  end
  else
  begin
    // Proceso Estándar Occidental
    Result := RemoveNull(Encoding.GetString(LocalBytes));
  end;

  // Limpieza de bytes sueltos
  SetLength(BChar, 1);
  BChar[0] := $00; Result := Result.Replace(Encoding.GetString(BChar), '', [rfReplaceAll, rfIgnoreCase]);
  BChar[0] := $7F; Result := Result.Replace(Encoding.GetString(BChar), '', [rfReplaceAll, rfIgnoreCase]);
  BChar[0] := $BE; Result := Result.Replace(Encoding.GetString(BChar), '', [rfReplaceAll, rfIgnoreCase]);

  // CAVENA SDH
  BChar[0] := $1D; Result := Result.Replace(Encoding.GetString(BChar), '[');
  BChar[0] := $1B; Result := Result.Replace(Encoding.GetString(BChar), ']');

  // Mapeo Cavena original (Letras Nórdicas)
  if LangID in [LangIdDanish, LangIdSwedish] then
  begin
    BChar[0] := $1C; Result := Result.Replace(Encoding.GetString(BChar), 'ø');
    BChar[0] := $1E; Result := Result.Replace(Encoding.GetString(BChar), 'Æ');
    BChar[0] := $1F; Result := Result.Replace(Encoding.GetString(BChar), 'Ø');
    BChar[0] := $5B; Result := Result.Replace(Encoding.GetString(BChar), 'Æ');
    BChar[0] := $5C; Result := Result.Replace(Encoding.GetString(BChar), 'Ø');
    BChar[0] := $5D; Result := Result.Replace(Encoding.GetString(BChar), 'Å');
  end
  else
  begin
    Result := Result.Replace('å', '[').Replace('æ', ']');
  end;

  // Mapeo de acentos extendidos
  SetLength(BChar, 2);
  BChar[0] := $86; BChar[1] := $41; Result := Result.Replace(Encoding.GetString(BChar), 'Ä');
  BChar[1] := $61; Result := Result.Replace(Encoding.GetString(BChar), 'ä');
  BChar[1] := $4F; Result := Result.Replace(Encoding.GetString(BChar), 'Ö');
  BChar[1] := $6F; Result := Result.Replace(Encoding.GetString(BChar), 'ö');
  BChar[0] := $8C; BChar[1] := $61; Result := Result.Replace(Encoding.GetString(BChar), 'å');
  BChar[1] := $41; Result := Result.Replace(Encoding.GetString(BChar), 'Å');
  BChar[0] := $81; BChar[1] := $65; Result := Result.Replace(Encoding.GetString(BChar), 'è');
  BChar[0] := $82; BChar[1] := $65; Result := Result.Replace(Encoding.GetString(BChar), 'é');
  BChar[1] := $45; Result := Result.Replace(Encoding.GetString(BChar), 'É');
  BChar[0] := $81; BChar[1] := $45; Result := Result.Replace(Encoding.GetString(BChar), 'È');
  BChar[0] := $82; BChar[1] := $69; Result := Result.Replace(Encoding.GetString(BChar), 'í');
  BChar[1] := $49; Result := Result.Replace(Encoding.GetString(BChar), 'Í');
  BChar[1] := $75; Result := Result.Replace(Encoding.GetString(BChar), 'ó');
  BChar[1] := $55; Result := Result.Replace(Encoding.GetString(BChar), 'Ó');

  // Códigos de control (SOLO idiomas no nórdicos)
  if not (LangID in [LangIdDanish, LangIdSwedish]) then
  begin
    SetLength(BChar, 1);

    BChar[0] := $88; Result := Result.Replace(Encoding.GetString(BChar), '{\i1}');
    BChar[0] := $98; Result := Result.Replace(Encoding.GetString(BChar), '{\i0}');

    BChar[0] := $1C; Result := Result.Replace(Encoding.GetString(BChar), '{\be1}');
    BChar[0] := $1D; Result := Result.Replace(Encoding.GetString(BChar), '{\be0}');

    BChar[0] := $81; Result := Result.Replace(Encoding.GetString(BChar), '{\c&H0000FF&}');
    BChar[0] := $82; Result := Result.Replace(Encoding.GetString(BChar), '{\c&H00FF00&}');
    BChar[0] := $83; Result := Result.Replace(Encoding.GetString(BChar), '{\c&H00FFFF&}');
    BChar[0] := $84; Result := Result.Replace(Encoding.GetString(BChar), '{\c&HFF0000&}');
    BChar[0] := $85; Result := Result.Replace(Encoding.GetString(BChar), '{\c&HFF00FF&}');
    BChar[0] := $86; Result := Result.Replace(Encoding.GetString(BChar), '{\c&HFFFF00&}');
    BChar[0] := $87; Result := Result.Replace(Encoding.GetString(BChar), '{\c&HFFFFFF&}');
    BChar[0] := $89; Result := Result.Replace(Encoding.GetString(BChar), '{\flash1}');
    BChar[0] := $8A; Result := Result.Replace(Encoding.GetString(BChar), '{\flash0}');
  end;

  // Fix de Apóstrofes ("I?d" -> "I'd")
  for I := 2 to Length(Result) - 1 do
  begin
    if Result[I] = '?' then
    begin
      if (Result[I-1] in ['a'..'z', 'A'..'Z']) and
         (Result[I+1] in ['a'..'z', 'A'..'Z']) then
      begin
        Result[I] := '''';
      end;
    end;
  end;

  // AUTO-CLOSE TAGS
  if Result.LastIndexOf('{\i1}') > Result.LastIndexOf('{\i0}') then
    Result := Result + '{\i0}';

  if Result.LastIndexOf('{\be1}') > Result.LastIndexOf('{\be0}') then
    Result := Result + '{\be0}';

  if Result.LastIndexOf('{\flash1}') > Result.LastIndexOf('{\flash0}') then
    Result := Result + '{\flash0}';

  SetLength(BChar, 0);
  SetLength(LocalBytes, 0);
  Encoding.Free; // GetEncodingFromLangID crea una instancia por llamada (antes se fugaba 2 por subtitulo)
end;

// -----------------------------------------------------------------------------

// Solo extrae la alineacion ({\anX}) y quita esas marcas. El resto de tags se
// convierte a bytes en GetTextAsBytes (antes se pasaba por TEncoding.Default:
// en Linux/macOS (UTF-8) los codigos 80h-9Fh daban U+FFFD y se escribia EFh).
function EncodeTags(const Text: String; var AlignByte: Byte): String;
begin
  Result := Text;
  AlignByte := $50; // Por defecto: Inferior Izquierdo / Centro

  if Result.Contains('{\an1}') then AlignByte := $50 else
  if Result.Contains('{\an2}') then AlignByte := $51 else
  if Result.Contains('{\an3}') then AlignByte := $52 else
  if Result.Contains('{\an4}') then AlignByte := $54 else
  if Result.Contains('{\an5}') then AlignByte := $55 else
  if Result.Contains('{\an6}') then AlignByte := $56 else
  if Result.Contains('{\an7}') then AlignByte := $58 else
  if Result.Contains('{\an8}') then AlignByte := $59 else
  if Result.Contains('{\an9}') then AlignByte := $5A;

  Result := Result.Replace('{\an1}','').Replace('{\an2}','').Replace('{\an3}','')
                  .Replace('{\an4}','').Replace('{\an5}','').Replace('{\an6}','')
                  .Replace('{\an7}','').Replace('{\an8}','').Replace('{\an9}','');
end;

// -----------------------------------------------------------------------------

procedure WriteByte(const Stream: TStream; const B: Byte);
begin
  Stream.Write(B, 1);
end;

// -----------------------------------------------------------------------------

procedure WriteTime(const Stream: TStream; const TimeMs: Cardinal; const FPS: Double);
var
  TotalFrames: Integer;
begin
  TotalFrames := Round(TimeMs / (1000.0 / FPS));
  WriteByte(Stream, (TotalFrames shr 16) and $FF);
  WriteByte(Stream, (TotalFrames shr 8) and $FF);
  WriteByte(Stream, TotalFrames and $FF);
end;

// -----------------------------------------------------------------------------

function GetTextAsBytes(const Text: String; const LangID: Integer; const Enc: TEncoding): TBytes;
var
  Idx, I, K, CPLen, TagEnd: Integer;
  CleanText, Current, Tag: String;
  Found, IsCJK: Boolean;
  Chunk: TBytes;
  B: Byte;

  procedure Put(const V: Byte);
  begin
    Result[Idx] := V;
    Inc(Idx);
  end;

begin
  SetLength(Result, CAVENA_TEXT_LEN);
  IsCJK := (LangID = LangIdChineseTraditional) or (LangID = LangIdChineseSimplified);

  if IsCJK then
    FillChar(Result[0], CAVENA_TEXT_LEN, $00)
  else
    FillChar(Result[0], CAVENA_TEXT_LEN, $7F);

  CleanText := Text;
  CleanText := CleanText.Replace('’', '''').Replace('‘', '''').Replace('´', '''').Replace('`', '''');
  CleanText := CleanText.Replace('“', '"').Replace('”', '"');
  CleanText := CleanText.Replace('—', '-').Replace('–', '-');
  CleanText := CleanText.Replace('…', '...');
  CleanText := CleanText.Replace(LineEnding, ' ').Replace(#10, ' ').Replace(#13, '');

  Idx := 0;
  I := 1;
  while (I <= Length(CleanText)) and (Idx < CAVENA_TEXT_LEN - 1) do
  begin
    // Tags {\...}: los conocidos pasan a su byte de control, el resto se descarta
    if (CleanText[I] = '{') and (I < Length(CleanText)) and (CleanText[I + 1] = '\') then
    begin
      TagEnd := Pos('}', CleanText, I);
      if TagEnd = 0 then TagEnd := Length(CleanText);
      Tag := LowerCase(Copy(CleanText, I, TagEnd - I + 1));
      B := 0;
      if Tag = '{\i1}' then B := $88
      else if Tag = '{\i0}' then B := $98
      else if Tag = '{\be1}' then B := $1C
      else if Tag = '{\be0}' then B := $1D
      else if Tag = '{\c&h0000ff&}' then B := $81
      else if Tag = '{\c&h00ff00&}' then B := $82
      else if Tag = '{\c&h00ffff&}' then B := $83
      else if Tag = '{\c&hff0000&}' then B := $84
      else if Tag = '{\c&hff00ff&}' then B := $85
      else if Tag = '{\c&hffff00&}' then B := $86
      else if Tag = '{\c&hffffff&}' then B := $87
      else if Tag = '{\flash1}' then B := $89
      else if Tag = '{\flash0}' then B := $8A;
      if B <> 0 then Put(B);
      I := TagEnd + 1;
      Continue;
    end;

    // un code point UTF-8 completo (antes Substring(I,1) partia los multibyte)
    case Byte(CleanText[I]) of
      $C0..$DF: CPLen := 2;
      $E0..$EF: CPLen := 3;
      $F0..$F7: CPLen := 4;
    else
      CPLen := 1;
    end;
    if I + CPLen - 1 > Length(CleanText) then CPLen := Length(CleanText) - I + 1;
    Current := Copy(CleanText, I, CPLen);
    Inc(I, CPLen);
    Found := False;

    if Current = '[' then
    begin
      Put($1D);
      Found := True;
    end
    else if Current = ']' then
    begin
      Put($1B);
      Found := True;
    end
    else if LangID = LangIdRussian then
    begin
      for K := 0 to Length(TRussianLetters)-1 do
        if TRussianLetters[K] = Current then
        begin
          Put(TRussianCodes[K]);
          Found := True;
          Break;
        end;
    end
    else if LangID = LangIdHebrew then
    begin
      for K := 0 to Length(THebrewLetters)-1 do
        if THebrewLetters[K] = Current then
        begin
          Put(THebrewCodes[K]);
          Found := True;
          Break;
        end;
    end;

    if not Found then
    begin
      // bytes COMPLETOS del caracter en la tabla del idioma (chino = 2 bytes);
      // antes solo se tomaba el primero ([0]) con TEncoding.Default.
      Chunk := Enc.GetBytes(Current);
      if Length(Chunk) = 0 then
        Put(Ord('?'))
      else if Idx + Length(Chunk) > CAVENA_TEXT_LEN - 1 then
        Break
      else
        for K := 0 to High(Chunk) do Put(Chunk[K]);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure WriteText(const Stream: TStream; const Text: String; const IsLast: Boolean; const LangID: Integer; const Enc: TEncoding);
var
  Line1, Line2 : String;
  IdxSeparator : Integer;
  Buffer       : TBytes;
begin
  Line1 := '';
  Line2 := '';

  IdxSeparator := Text.IndexOf(LineEnding);
  if IdxSeparator >= 0 then
  begin
    Line1 := Text.Substring(0, IdxSeparator);
    Line2 := Text.Substring(IdxSeparator + Length(LineEnding));
  end
  else
    Line2 := Text;

  Buffer := GetTextAsBytes(Line1, LangID, Enc);
  Stream.Write(Buffer[0], Length(Buffer));

  SetLength(Buffer, 6);
  FillChar(Buffer[0], 6, 0);
  Stream.Write(Buffer[0], 6);

  Buffer := GetTextAsBytes(Line2, LangID, Enc);
  Stream.Write(Buffer[0], Length(Buffer));

  if not IsLast then
  begin
    SetLength(Buffer, 4);
    FillChar(Buffer[0], 4, 0);
    Stream.Write(Buffer[0], 4);
  end;
end;

// -----------------------------------------------------------------------------

{ TUWCavena890 }

// -----------------------------------------------------------------------------

function TUWCavena890.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWCavena890.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfCavena890;
end;

// -----------------------------------------------------------------------------

function TUWCavena890.Extension: String;
begin
  Result := '*.890';
end;

// -----------------------------------------------------------------------------

function TUWCavena890.IsTextBased: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWCavena890.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWCavena890.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
var
  Sub : TUWSubtitles;
begin
  Result := False;
  if LowerCase(ExtractFileExt(SubtitleFile.FileName)) <> '.890' then Exit;

  Sub := TUWSubtitles.Create;
  try
    Result := LoadSubtitle(SubtitleFile, 25, Sub);
  finally
    Sub.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TUWCavena890.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  FileStream  : TFileStream;
  Bytes       : TBytes;
  I           : Integer;
  LastNum     : Integer;
  StartPos    : Integer;
  CurrentNum  : Integer;
  StartFrame  : Integer;
  EndFrame    : Integer;
  InitialMs   : Integer;
  FinalMs     : Integer;
  Lang1, Lang2: Integer;
  Line1, Line2: String;
  FullText    : String;
begin
  Result := False;
  FileStream := TFileStream.Create(SubtitleFile.FileName, fmOpenRead or fmShareDenyWrite);
  try
    if FileStream.Size < CAVENA_HDR_OFFSET then Exit;

    SetLength(Bytes, FileStream.Size);
    FileStream.Read(Bytes[0], FileStream.Size);

    Lang1 := Bytes[146];
    Lang2 := Bytes[147];

    // Idiomas del archivo en las propiedades (round-trip al guardar)
    Subtitles.FormatProperties^.Cavena890.PrimaryLanguage   := Lang1;
    Subtitles.FormatProperties^.Cavena890.SecondaryLanguage := Lang2;

    I       := CAVENA_HDR_OFFSET;
    LastNum := -1;

    // Antes: "while I < (Length(Bytes) - 20) do" — guard insuficiente.
    // StartPos = I - CAVENA_TEXT_LEN(51); el segundo llamado a FixText
    // lee hasta Bytes[StartPos + CAVENA_TEXT_LEN(51) + 6 + CAVENA_TEXT_LEN(51) - 1]
    // = Bytes[I + 56]. Con el guard viejo, el último I que entraba al
    // loop era Length(Bytes)-21, así que la lectura máxima llegaba a
    // Length(Bytes)+35 — 36 bytes LEÍDOS FUERA DE RANGO en el último
    // bloque de CUALQUIER archivo .890, no solo uno corrupto/truncado.
    // El margen correcto es 56 (no 20).
    while I < (Length(Bytes) - 56) do
    begin
      StartPos   := I - CAVENA_TEXT_LEN;
      CurrentNum := (Bytes[StartPos - 16] shl 8) or Bytes[StartPos - 15];
      StartFrame := (Bytes[StartPos - 14] shl 16) or (Bytes[StartPos - 13] shl 8) or Bytes[StartPos - 12];
      EndFrame   := (Bytes[StartPos - 11] shl 16) or (Bytes[StartPos - 10] shl 8) or Bytes[StartPos - 9];

      Line1 := FixText(Bytes, StartPos, CAVENA_TEXT_LEN, Lang1).Trim;
      Line2 := FixText(Bytes, StartPos + CAVENA_TEXT_LEN + 6, CAVENA_TEXT_LEN, Lang2).Trim;

      if LastNum = CurrentNum then
      begin
        FullText := (Line1 + LineEnding + Line2).Trim;
        if FullText.Length > 0 then
          Subtitles.Text[Subtitles.Count - 1] := FullText;
      end
      else
      begin
        FullText  := (Line1 + LineEnding + Line2).Trim;
        InitialMs := Trunc((1000.0 / FPS) * StartFrame);
        FinalMs   := Trunc((1000.0 / FPS) * EndFrame);
        Subtitles.Add(InitialMs, FinalMs, FullText, '', NIL);
      end;

      LastNum := CurrentNum;
      Inc(I, CAVENA_BLOCK_SIZE);
    end;
  finally
    FileStream.Free;
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWCavena890.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  Stream     : TStream;
  Buffer     : TBytes;
  Header     : TBytes;
  I          : Integer;
  LangID     : Integer;
  Lang2      : Integer;
  BlockNum   : Integer;
  AlignByte  : Byte;
  TextToSave : String;
  Enc        : TEncoding;
begin
  Result := False;

  // Idioma: el del archivo/propiedades si es conocido; ingles por defecto
  LangID := Subtitles.FormatProperties^.Cavena890.PrimaryLanguage;
  if not IsKnownLangID(LangID) then LangID := LangIdEnglish;
  Lang2 := Subtitles.FormatProperties^.Cavena890.SecondaryLanguage;
  if not IsKnownLangID(Lang2) then Lang2 := LangID;

  for I := FromItem to ToItem do
  begin
    TextToSave := iff(SubtitleMode = smText, Subtitles.Text[I], Subtitles.Translation[I]);
    if TextToSave.Contains('的') or
       TextToSave.Contains('是') or
       TextToSave.Contains('啊') or
       TextToSave.Contains('吧') or
       TextToSave.Contains('好') or
       TextToSave.Contains('亲') or
       TextToSave.Contains('爱') or
       TextToSave.Contains('早') or
       TextToSave.Contains('上') then
    begin
      LangID := LangIdChineseSimplified;
      Lang2  := LangID;
      Break;
    end;
  end;

  Enc := GetEncodingFromLangID(LangID);
  try
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      // HEADER (el idioma tambien en los offsets 146/147 que lee LoadSubtitle)
      SetLength(Header, SizeOf(TCavena890_Info));
      Move(Subtitles.FormatProperties^.Cavena890, Header[0], SizeOf(TCavena890_Info));
      Header[146] := LangID;
      Header[147] := Lang2;
      Stream.WriteBuffer(Header[0], Length(Header));

      // Bloques de Subtitulos
      BlockNum := 16;
      for I := FromItem to ToItem do
      begin
        WriteByte(Stream, (BlockNum shr 8) and $FF);
        WriteByte(Stream, BlockNum and $FF);

        WriteTime(Stream, Subtitles.InitialTime[I], FPS);
        WriteTime(Stream, Subtitles.FinalTime[I], FPS);

        TextToSave := EncodeTags(iff(SubtitleMode = smText, Subtitles.Text[I], Subtitles.Translation[I]), AlignByte);

        WriteByte(Stream, AlignByte);

        SetLength(Buffer, SizeOf(TReservedBlock));
        Move(TReservedBlock[0], Buffer[0], SizeOf(TReservedBlock));
        Stream.Write(Buffer[0], Length(Buffer));

        WriteText(Stream, TextToSave, I = ToItem, LangID, Enc);

        Inc(BlockNum, 16);
      end;

      Result := True;
    finally
      Stream.Free;
    end;
  finally
    Enc.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
