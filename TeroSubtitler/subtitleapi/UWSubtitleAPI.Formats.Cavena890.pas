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
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.ExtraInfo, UWSystem.StrUtils, UWSystem.SysUtils,
  UWSubtitleAPI.Formats.Cavena890.Types;

// -----------------------------------------------------------------------------

function GetLanguage(const Bytes: TBytes): String;
begin
  if Length(Bytes) < 200 then
    Result := ''
  else
    TEncoding.ASCII.GetString(Bytes, 187, 6);
end;

// -----------------------------------------------------------------------------

function FixText(const Bytes: TBytes; const Start: Integer; const TextLength: Integer; const LangId: Integer): String;

  function RemoveNull(const Input: String): String;
  var
    OutputLen,
    Index    : Integer;
    C        : Char;
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

var
  Encoding : TEncoding;
  Byte     : TBytes;
begin
  Encoding := TEncoding.Default;
  try
    Result := RemoveNull(Encoding.GetString(Bytes, Start, TextLength));

    SetLength(Byte, 1);
    Byte[0] := $00;
    Result := Result.Replace(Encoding.GetString(Byte), String.Empty, [rfReplaceAll, rfIgnoreCase]);
    Byte[0] := $7F;
    Result := Result.Replace(Encoding.GetString(Byte), String.Empty, [rfReplaceAll, rfIgnoreCase]);
    Byte[0] := $BE;
    Result := Result.Replace(Encoding.GetString(Byte), String.Empty, [rfReplaceAll, rfIgnoreCase]);
    Byte[0] := $1B;
    Result := Result.Replace(Encoding.GetString(Byte), 'æ');
    Byte[0] := $1C;
    Result := Result.Replace(Encoding.GetString(Byte), 'ø');
    Byte[0] := $1D;
    Result := Result.Replace(Encoding.GetString(Byte), 'å');
    Byte[0] := $1E;
    Result := Result.Replace(Encoding.GetString(Byte), 'Æ');
    Byte[0] := $1F;
    Result := Result.Replace(Encoding.GetString(Byte), 'Ø');
    Byte[0] := $5B;
    Result := Result.Replace(Encoding.GetString(Byte), 'Æ');
    Byte[0] := $5C;
    Result := Result.Replace(Encoding.GetString(Byte), 'Ø');
    Byte[0] := $5D;
    Result := Result.Replace(Encoding.GetString(Byte), 'Å');

    // abcdefghijqlmnopqrst
    // 12345678901234567890
    SetLength(Byte, 2);
    Byte[0] := $86;
    Byte[1] := $41;
    Result := Result.Replace(Encoding.GetString(Byte), 'Ä');
    Byte[1] := $61;
    Result := Result.Replace(Encoding.GetString(Byte), 'ä');
    Byte[1] := $4F;
    Result := Result.Replace(Encoding.GetString(Byte), 'Ö');
    Byte[1] := $6F;
    Result := Result.Replace(Encoding.GetString(Byte), 'ö');
    Byte[0] := $8C;
    Byte[1] := $61;
    Result := Result.Replace(Encoding.GetString(Byte), 'å');
    Byte[1] := $41;
    Result := Result.Replace(Encoding.GetString(Byte), 'Å');
    Byte[0] := $81;
    Byte[1] := $65;
    Result := Result.Replace(Encoding.GetString(Byte), 'è');
    Byte[0] := $82;
    Result := Result.Replace(Encoding.GetString(Byte), 'é');
    Byte[0] := $82;
    Byte[1] := $45;
    Result := Result.Replace(Encoding.GetString(Byte), 'É');
    Byte[0] := $81;
    Byte[1] := $65;
    Result := Result.Replace(Encoding.GetString(Byte), 'È');
    Byte[0] := $82;
    Byte[1] := $69;
    Result := Result.Replace(Encoding.GetString(Byte), 'í');
    Byte[1] := $49;
    Result := Result.Replace(Encoding.GetString(Byte), 'Í');
    Byte[1] := $75;
    Result := Result.Replace(Encoding.GetString(Byte), 'ó');
    Byte[1] := $55;
    Result := Result.Replace(Encoding.GetString(Byte), 'Ó');

    Byte[0] := $88;
    Result := Result.Replace(Encoding.GetString(Byte), '{\i1}');
    Byte[0] := $98;
    Result := Result.Replace(Encoding.GetString(Byte), '{\i0}');
  finally
    SetLength(Byte, 0);
  end;
end;

// -----------------------------------------------------------------------------

procedure WriteByte(const Stream: TStream; const bByte: Byte);
begin
  Stream.Write(bByte, 1);
end;

// -----------------------------------------------------------------------------

procedure WriteTime(const Stream: TStream; const Time: Cardinal; const FPS: Single);
var
  Frames: Integer;
begin
  Frames := Round(Time / (1000.0 / FPS));
  WriteByte(Stream, Round(Frames / 256 / 256));
  WriteByte(Stream, Round(Frames / 256));
  WriteByte(Stream, Frames mod 256);
end;

// -----------------------------------------------------------------------------

function GetTextAsBytes(const Text: String; const LangID: Integer): TBytes;
var
  i: Integer;
  Encoding: TEncoding;
  index: Integer;
  current: String;
begin
  SetLength(Result, 51);

  if (langID = LangIdChineseTraditional) or (langID = LangIdChineseSimplified) then
    for i := 0 to Length(Result)-1 do Result[i] := $00
  else
    for i := 0 to Length(Result)-1 do Result[i] := $7F;

  Encoding := TEncoding.Default;
  index := 0;
  for i := 0 to Text.Length-1 do
  begin
    current := Text.Substring(i, 1);
    if index < 50 then
    begin
      Result[index] := Encoding.GetBytes(current)[0];
      inc(index);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure WriteText(const Stream: TStream; const Text: String; const isLast: Boolean; const LangID: Integer);
var
  line1,
  line2  : String;
  x      : Integer;
  Buffer : TBytes;
begin
  line1 := '';
  line2 := '';
  if LineCount(Text, LineEnding) > 1 then
  begin
    x := Pos(LineEnding, Text);
    line1 := Text.Substring(0, x-1);
    line2 := Text.Substring(x+1, Text.Length-x);
  end
  else
    line2 := Text;

    buffer := GetTextAsBytes(line1, langId);
    Stream.Write(Buffer, Length(Buffer));

    SetLength(Buffer, 6);
    for x := 0 to 5 do buffer[x] := $00;
    Stream.Write(Buffer, Length(Buffer));

    buffer := GetTextAsBytes(line2, langId);
    Stream.Write(Buffer, Length(Buffer));

    if not isLast then
    begin
      SetLength(Buffer, 4);
      for x := 0 to 3 do buffer[x] := $00;
      Stream.Write(Buffer, Length(Buffer));
    end;

    SetLength(Buffer, 0);
end;

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
  sub : TUWSubtitles;
begin
  Result := False;
  if LowerCase(ExtractFileExt(SubtitleFile.FileName)) <> '.890' then Exit;

  sub := TUWSubtitles.Create;
  try
    Result := LoadSubtitle(SubtitleFile, 25, sub);
  finally
    sub.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TUWCavena890.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;

const
  TextLength = 51;

var
  FileStream : TFileStream;
  Bytes      : TBytes;
  i,
  lastNumber,
  start,
  number     : Integer;
  startFrame : Double;
  endFrame   : Double;
  langId1,
  langId2    : Integer;
  fontname1,
  fontname2,
  line1,
  line2      : String;

  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  FileStream := TFileStream.Create(SubtitleFile.FileName, fmOpenRead or fmShareDenyWrite);
  try
    if FileStream.Size > 0 then
    begin
      SetLength(Bytes, FileStream.Size);
      FileStream.Read(Bytes[0], FileStream.Size);

      langId1 := Bytes[146];
      langId2 := Bytes[147];

      fontname1 := TEncoding.ASCII.GetString(Bytes, 187, 6);
      fontname2 := TEncoding.ASCII.GetString(Bytes, 246, 6);

      i          := 455;
      lastNumber := -1;

      while i < Length(Bytes)-20 do
      begin
        start  := i - TextLength;
        number := Bytes[start-16] * 256 + Bytes[start-15];

        startFrame := Bytes[start-14] * 256 * 256 + Bytes[start-13] * 256 + Bytes[start-12];
        endFrame   := Bytes[start-11] * 256 * 256 + Bytes[start-10] * 256 + Bytes[start-9];

        line1 := FixText(Bytes, start, textLength, langId1).Trim;
        line2 := FixText(Bytes, start + textLength + 6, textLength, langId2).Trim;

        if LastNumber = Number then
        begin
          Text := (line1 + LineEnding + line2).Trim;
          if Text.Length > 0 then
            Subtitles.Text[Subtitles.Count-1] := Text;
        end
        else
        begin
          InitialTime := Trunc((1000.0 / FPS) * startFrame);
          FinalTime   := Trunc((1000.0 / FPS) * endFrame);
          Text        := (line1 + LineEnding + line2).Trim;
          Subtitles.Add(InitialTime, FinalTime, Text, '', NIL);
        end;

        lastNumber := Number;
        inc(i, 128);
      end;
    end;
  finally
    FileStream.Free;
    if Subtitles.Count > 0 then Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function TUWCavena890.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  Stream : TStream;
  Buffer : TBytes;
  i      : Integer;
  langId : Integer;
  lang   : String;
  number : Integer;
begin
  Result := False;

  Stream := TFileStream.Create(FileName, fmCreate);
  try
    langId := LangIdEnglish;

    for i := FromItem to ToItem do
      if (Subtitles.Text[i].Contains('的') or
          Subtitles.Text[i].Contains('是') or
          Subtitles.Text[i].Contains('啊') or
          Subtitles.Text[i].Contains('吧') or
          Subtitles.Text[i].Contains('好') or
          Subtitles.Text[i].Contains('吧') or
          Subtitles.Text[i].Contains('亲') or
          Subtitles.Text[i].Contains('爱') or
          Subtitles.Text[i].Contains('的') or
          Subtitles.Text[i].Contains('早') or
          Subtitles.Text[i].Contains('上') or
          Subtitles.Text[i].Contains('')) then
    begin
      langId := LangIdChineseSimplified;
      lang   := 'CCKM44';
      Break;
    end;

    // HEADER
    Stream.WriteBuffer(Subtitles.FormatProperties^.Cavena890, SizeOf(TCavena890_Info));

    // Subtitles
    number := 16;
    for i := FromItem to ToItem do
    begin
      WriteByte(Stream, Round(number / 256));
      WriteByte(Stream, number mod 256);

      WriteTime(Stream, Subtitles.InitialTime[i], FPS);
      WriteTime(Stream, Subtitles.FinalTime[i], FPS);

      WriteByte(Stream, $50); // $50 left, $54 center ?

      SetLength(Buffer, SizeOf(TUnkownCodes));
      Move(TUnkownCodes[0], Buffer[0], SizeOf(TUnkownCodes));
      Stream.Write(Buffer, Length(Buffer));

      WriteText(Stream, iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]), i = ToItem, langId);

      Inc(number, 16);
    end;

    Result := True;
  finally
    Stream.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
