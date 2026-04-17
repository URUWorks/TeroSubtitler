{*
 * URUWorks Subtitle API
 *
 * The contents of this file are used with permission, subject to
 * the Mozilla Public License Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/2.0.html
 *
 * Software distributed under the License is distributed on an
 * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * Copyright (C) 2001-2023 URUWorks, uruworks@gmail.com.
 *}

unit UWSubtitleAPI.Formats.Cavena890;

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
  UWSubtitleAPI.ExtraInfo, UWSystem.StrUtils, UWSystem.SysUtils,
  UWSubtitleAPI.Formats.Cavena890.Types;

const
  CAVENA_TEXT_LEN   = 51;
  CAVENA_BLOCK_SIZE = 128;
  CAVENA_HDR_OFFSET = 455;

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function GetEncodingFromLangID(LangID: Integer): TEncoding;
begin
  case LangID of
    45,46,47: Result := TEncoding.GetEncoding(1252); // nórdicos
    34:       Result := TEncoding.GetEncoding(1250); // checo
    36:       Result := TEncoding.GetEncoding(1250); // polaco
    39:       Result := TEncoding.GetEncoding(1251); // ruso
    40:       Result := TEncoding.GetEncoding(1253); // griego
    41:       Result := TEncoding.GetEncoding(1254); // turco
  else
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
begin
  SetLength(LocalBytes, TextLength);
  Move(Bytes[Start], LocalBytes[0], TextLength);

  I := 0;
  while I < TextLength do
  begin
    // Buscar secuencias UTF-8 rotas exportadas incorrectamente por "otros" programas
    if (I < TextLength - 2) and (LocalBytes[I] = $E2) and (LocalBytes[I+1] = $80) then
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
    for I := 0 to TextLength - 1 do
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

  SetLength(BChar, 0);
  SetLength(LocalBytes, 0);
end;

// -----------------------------------------------------------------------------

function EncodeTags(const Text: String; var AlignByte: Byte): String;
var
  Encoding: TEncoding;
  BChar   : TBytes;
begin
  Result := Text;
  AlignByte := $50; // Por defecto: Inferior Izquierdo / Centro
  Encoding := TEncoding.Default;
  SetLength(BChar, 1);

  // Mapeo Alineación ASS {\anX} -> Cavena Alignment
  if Result.Contains('{\an1}') then AlignByte := $50 else
  if Result.Contains('{\an2}') then AlignByte := $51 else
  if Result.Contains('{\an3}') then AlignByte := $52 else
  if Result.Contains('{\an4}') then AlignByte := $54 else
  if Result.Contains('{\an5}') then AlignByte := $55 else
  if Result.Contains('{\an6}') then AlignByte := $56 else
  if Result.Contains('{\an7}') then AlignByte := $58 else
  if Result.Contains('{\an8}') then AlignByte := $59 else
  if Result.Contains('{\an9}') then AlignByte := $5A;

  // Limpiar etiquetas de alineación
  Result := Result.Replace('{\an1}','').Replace('{\an2}','').Replace('{\an3}','')
                  .Replace('{\an4}','').Replace('{\an5}','').Replace('{\an6}','')
                  .Replace('{\an7}','').Replace('{\an8}','').Replace('{\an9}','');

  // Reemplazos Inversos
  BChar[0] := $88; Result := Result.Replace('{\i1}', Encoding.GetString(BChar), [rfReplaceAll, rfIgnoreCase]);
  BChar[0] := $98; Result := Result.Replace('{\i0}', Encoding.GetString(BChar), [rfReplaceAll, rfIgnoreCase]);
  BChar[0] := $1C; Result := Result.Replace('{\be1}', Encoding.GetString(BChar), [rfReplaceAll, rfIgnoreCase]);
  BChar[0] := $1D; Result := Result.Replace('{\be0}', Encoding.GetString(BChar), [rfReplaceAll, rfIgnoreCase]);
  BChar[0] := $81; Result := Result.Replace('{\c&H0000FF&}', Encoding.GetString(BChar), [rfReplaceAll, rfIgnoreCase]);
  BChar[0] := $82; Result := Result.Replace('{\c&H00FF00&}', Encoding.GetString(BChar), [rfReplaceAll, rfIgnoreCase]);
  BChar[0] := $83; Result := Result.Replace('{\c&H00FFFF&}', Encoding.GetString(BChar), [rfReplaceAll, rfIgnoreCase]);
  BChar[0] := $84; Result := Result.Replace('{\c&HFF0000&}', Encoding.GetString(BChar), [rfReplaceAll, rfIgnoreCase]);
  BChar[0] := $85; Result := Result.Replace('{\c&HFF00FF&}', Encoding.GetString(BChar), [rfReplaceAll, rfIgnoreCase]);
  BChar[0] := $86; Result := Result.Replace('{\c&HFFFF00&}', Encoding.GetString(BChar), [rfReplaceAll, rfIgnoreCase]);
  BChar[0] := $87; Result := Result.Replace('{\c&HFFFFFF&}', Encoding.GetString(BChar), [rfReplaceAll, rfIgnoreCase]);
  BChar[0] := $89; Result := Result.Replace('{\flash1}', Encoding.GetString(BChar), [rfReplaceAll, rfIgnoreCase]);
  BChar[0] := $8A; Result := Result.Replace('{\flash0}', Encoding.GetString(BChar), [rfReplaceAll, rfIgnoreCase]);

  SetLength(BChar, 0);
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

function GetTextAsBytes(const Text: String; const LangID: Integer): TBytes;
var
  Idx, I, K  : Integer;
  CleanText  : String;
  Found      : Boolean;
  Encoding   : TEncoding;
  Current    : String;
begin
  SetLength(Result, CAVENA_TEXT_LEN);

  if (LangID = LangIdChineseTraditional) or (LangID = LangIdChineseSimplified) then
    FillChar(Result[0], CAVENA_TEXT_LEN, $00)
  else
    FillChar(Result[0], CAVENA_TEXT_LEN, $7F);

  CleanText := Text;
  CleanText := CleanText.Replace('’', '''').Replace('‘', '''').Replace('´', '''').Replace('`', '''');
  CleanText := CleanText.Replace('“', '"').Replace('”', '"');
  CleanText := CleanText.Replace('—', '-').Replace('–', '-');
  CleanText := CleanText.Replace('…', '...');

  Encoding := TEncoding.Default;
  Idx := 0;

  for I := 0 to CleanText.Length - 1 do
  begin
    if Idx >= (CAVENA_TEXT_LEN - 1) then Break;
    Current := CleanText.Substring(I, 1);
    Found := False;

    // SDH
    if Current = '[' then
    begin
      Result[Idx] := $1D;
      Found := True;
    end
    else if Current = ']' then
    begin
      Result[Idx] := $1B;
      Found := True;
    end
    else if LangID = LangIdRussian then
    begin
      for K := 0 to Length(TRussianLetters)-1 do
      begin
        if TRussianLetters[K] = Current then
        begin
          Result[Idx] := TRussianCodes[K];
          Found := True;
          Break;
        end;
      end;
    end
    else if LangID = LangIdHebrew then
    begin
      for K := 0 to Length(THebrewLetters)-1 do
      begin
        if THebrewLetters[K] = Current then
        begin
          Result[Idx] := THebrewCodes[K];
          Found := True;
          Break;
        end;
      end;
    end;

    if not Found then
      Result[Idx] := Encoding.GetBytes(Current)[0];

    Inc(Idx);
  end;
end;

// -----------------------------------------------------------------------------

procedure WriteText(const Stream: TStream; const Text: String; const IsLast: Boolean; const LangID: Integer);
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

  Buffer := GetTextAsBytes(Line1, LangID);
  Stream.Write(Buffer[0], Length(Buffer));

  SetLength(Buffer, 6);
  FillChar(Buffer[0], 6, 0);
  Stream.Write(Buffer[0], 6);

  Buffer := GetTextAsBytes(Line2, LangID);
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
  Lang1, Lang2: Integer;
  Line1, Line2: String;
  InitialMs   : Integer;
  FinalMs     : Integer;
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

    I       := CAVENA_HDR_OFFSET;
    LastNum := -1;

    while I < (Length(Bytes) - 20) do
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
        InitialMs := Trunc((1000.0 / FPS) * StartFrame);
        FinalMs   := Trunc((1000.0 / FPS) * EndFrame);
        FullText  := (Line1 + LineEnding + Line2).Trim;
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
  I          : Integer;
  LangID     : Integer;
  BlockNum   : Integer;
  AlignByte  : Byte;
  TextToSave : String;
begin
  Result := False;
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    LangID := LangIdEnglish;

    for I := FromItem to ToItem do
    begin
      if Subtitles.Text[I].Contains('的') or Subtitles.Text[I].Contains('是') or
         Subtitles.Text[I].Contains('啊') or Subtitles.Text[I].Contains('吧') or
         Subtitles.Text[I].Contains('好') or Subtitles.Text[I].Contains('亲') or
         Subtitles.Text[I].Contains('爱') or Subtitles.Text[I].Contains('早') or
         Subtitles.Text[I].Contains('上') then
      begin
        LangID := LangIdChineseSimplified;
        Break;
      end;
    end;

    // HEADER
    Stream.WriteBuffer(Subtitles.FormatProperties^.Cavena890, SizeOf(TCavena890_Info));

    // Bloques de Subtítulos
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

      WriteText(Stream, TextToSave, I = ToItem, LangID);

      Inc(BlockNum, 16);
    end;

    Result := True;
  finally
    Stream.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
