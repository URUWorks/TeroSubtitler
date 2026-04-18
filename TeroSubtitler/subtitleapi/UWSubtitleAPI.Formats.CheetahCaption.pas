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
 * Copyright (C) 2001-2026 URUWorks, uruworks@gmail.com.
 *}

unit UWSubtitleAPI.Formats.CheetahCaption;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWCheetahCaption }

  TUWCheetahCaption = class(TUWSubtitleCustomFormat)
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

uses UWSubtitleAPI.ExtraInfo, UWSubtitleAPI.Tags, UWSystem.SysUtils,
  UWSubtitleAPI.Formats.CheetahCaption.Types;

// -----------------------------------------------------------------------------

function DecodeTime(const Bytes: TBytes; const Index: Integer; const FPS: Double): Integer;
begin
  Result := EncodeTime(Bytes[Index], Bytes[Index + 1], Bytes[Index + 2], FramesToTime(Bytes[Index + 3], FPS));
end;

// -----------------------------------------------------------------------------

procedure WriteByte(const Stream: TStream; const bByte: Byte);
begin
  Stream.Write(bByte, 1);
end;

// -----------------------------------------------------------------------------

procedure WriteTime(const Stream: TStream; const Time: Cardinal; const FPS: Double);
var
  HH, MM, SS, MS: Word;
begin
  UWSystem.TimeUtils.DecodeTime(Time, HH, MM, SS, MS);

  WriteByte(Stream, Byte(HH));
  WriteByte(Stream, Byte(MM));
  WriteByte(Stream, Byte(SS));
  WriteByte(Stream, Byte(TimeToFrames(MS, FPS)));
end;

// -----------------------------------------------------------------------------

function TUWCheetahCaption.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWCheetahCaption.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfCheetahCaption;
end;

// -----------------------------------------------------------------------------

function TUWCheetahCaption.Extension: String;
begin
  Result := '*.cap';
end;

// -----------------------------------------------------------------------------

function TUWCheetahCaption.IsTextBased: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWCheetahCaption.HasStyleSupport: Boolean;
begin
  Result := True; // Has tags and coords
end;

// -----------------------------------------------------------------------------

function TUWCheetahCaption.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
var
  sub : TUWSubtitles;
begin
  Result := False;
  if LowerCase(ExtractFileExt(SubtitleFile.FileName)) <> '.cap' then Exit;

  sub := TUWSubtitles.Create;
  try
    Result := LoadSubtitle(SubtitleFile, 0, sub);
  finally
    sub.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TUWCheetahCaption.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;

  function IsLatinChar(const AByte: Byte): Integer;
  var
    c: Integer;
  begin
    Result := -1;
    for c := 0 to Length(TLatinCodes)-1 do
      if AByte = TLatinCodes[c] then
      begin
        Result := c;
        Break;
      end;
  end;

var
  FileStream : TFileStream;
  Bytes      : TBytes;
  i, j,
  Len,
  TextLen,
  start,
  usedBytes,
  index,
  LastIdx,
  Duration,
  InitialTime   : Integer;
  FinalTime, ft : Integer;
  Text          : String;
  italics : Boolean;
begin
  Result := False;
  FileStream := TFileStream.Create(SubtitleFile.FileName, fmOpenRead or fmShareDenyWrite);
  try
    if FileStream.Size > 0 then
    begin
      SetLength(Bytes, FileStream.Size);
      FileStream.Read(Bytes[0], FileStream.Size);

      i := 128;
      while (i < Length(Bytes) - 16) do
      begin
        Len := Bytes[i];
        if Len = 0 then
        begin
          Inc(i);
          Continue;
        end;

        usedBytes := 20; // Default
        InitialTime := DecodeTime(Bytes, i + 2, FPS);

        if Subtitles.Count > 0 then
        begin
          LastIdx := Subtitles.Count - 1;

          if Subtitles.FinalTime[LastIdx] > InitialTime then
            Subtitles.FinalTime[LastIdx] := InitialTime - 100;

          Duration := Subtitles.FinalTime[LastIdx] - Subtitles.InitialTime[LastIdx];

          if (Duration > 5000) or (Duration < 0) then
          begin
            Subtitles.FinalTime[LastIdx] := Subtitles.InitialTime[LastIdx] + CalculateOptimalDisplayMS(Subtitles.Text[LastIdx]);

            if Subtitles.FinalTime[LastIdx] > (InitialTime - 100) then
              Subtitles.FinalTime[LastIdx] := InitialTime - 100;
          end;
        end;

        if (Bytes[i + 6] = 2) and (Bytes[i + 7] = 1) and (Bytes[i + 8] = 0) and (Bytes[i + 9] = 0) then
          usedBytes := 16;

        ft := DecodeTime(Bytes, i + 6, FPS);

        TextLen := Len - usedBytes;
        start   := usedBytes - 1;

        j := 0;
        while (j < 4) and (i + start - 1 < Length(Bytes)) do
        begin
          if Bytes[i + start - 1] > $10 then
          begin
            Dec(start);
            Inc(TextLen);
          end;
          inc(j);
        end;

        if (TextLen > 0) and (Length(Bytes) >= i + start + TextLen) then
        begin
          Text := '';
          j    := 0;
          italics := False;
          while (j < TextLen) do
          begin
            index := i + start + j;

            if Bytes[index] = 0 then
            begin
              if not Text.EndsWith(sLineBreak) then Text := Text + sLineBreak;
            end
            else if IsLatinChar(Bytes[index]) > -1 then
            begin
              Text := Text + TLatinLetters[IsLatinChar(Bytes[index])];
            end
            else if (Bytes[index] >= $C0) or (Bytes[index] <= $14) then
            begin
              if (Bytes[index] = $d0) then italics := True;
            end
            else
            begin
              Text := Text + TEncoding.GetEncoding(SubtitleFile.CodePage).GetString(Bytes, index, 1);
            end;
            inc(j);
          end;

          Text := Text.Trim;
          if italics then Text := '{\i1}' + Text + '{\i0}';

          FinalTime := ft;
          Subtitles.Add(InitialTime, FinalTime, Text, '', NIL);
        end;

        inc(i, Len);
      end;

      if Subtitles.Count > 0 then
      begin
        LastIdx := Subtitles.Count - 1;
        Duration := Subtitles.FinalTime[LastIdx] - Subtitles.InitialTime[LastIdx];

        if (Duration > 5000) or (Duration < 0) then
          Subtitles.FinalTime[LastIdx] := Subtitles.InitialTime[LastIdx] + CalculateOptimalDisplayMS(Subtitles.Text[LastIdx]);
      end;
    end;
  finally
    FileStream.Free;
    if Subtitles.Count > 0 then
    begin
      Subtitles.ExtraInfoType := eiNone;
      Result := True;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TUWCheetahCaption.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  Stream       : TStream;
  i, j, c, idx : Integer;
  HeaderBlock  : THeaderBlock;
  StyleBlock   : TStyleBlock;
  text, Plain  : String;
  len, endpos  : Integer;
  IsItalic     : Boolean;
  TextBytes    : array of Byte;
  ByteCount    : Integer;
begin
  Result := False;

  Stream := TFileStream.Create(FileName, fmCreate);
  try
    // HEADER
    with HeaderBlock do
    begin
      Header[0]        := $EA;
      Header[1]        := $22;
      Header[2]        := 1;
      Header[3]        := 0;
      NumberOfLines[0] := Subtitles.Count mod 256;
      NumberOfLines[1] := Trunc(Subtitles.Count / 256);
      UnknowCodes1[0]  := 9;
      UnknowCodes1[1]  := $A8;
      UnknowCodes1[2]  := $AF;
      UnknowCodes1[3]  := $4F;
      FillByte(UnknowCodes2, SizeOf(UnknowCodes2), $0);
    end;
    Stream.WriteBuffer(HeaderBlock, SizeOf(THeaderBlock));

    for i := FromItem to ToItem do
    begin
      Text := iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]);

      // Detección de cursivas
      IsItalic := (Pos('{\i1}', Text) > 0) or (Pos('<i>', LowerCase(Text)) > 0);

      // Configuración de Posición y Estilo Base
      with StyleBlock do
      begin
        UnknowCodes1[0] := $12;
        UnknowCodes1[1] := 1;
        UnknowCodes1[2] := 0;
        UnknowCodes1[3] := 0;
        UnknowCodes1[4] := 0;
        UnknowCodes1[5] := 0;
        Justification   := 3;
        HorizontalPos   := $F;  // Default: Bottom
        VerticalPos     := $10; // Default: Center
      end;

      if Pos('{\an7}', Text) > 0 then begin StyleBlock.HorizontalPos := $1; StyleBlock.VerticalPos := $05; end
      else if Pos('{\an8}', Text) > 0 then begin StyleBlock.HorizontalPos := $1; StyleBlock.VerticalPos := $10; end
      else if Pos('{\an9}', Text) > 0 then begin StyleBlock.HorizontalPos := $1; StyleBlock.VerticalPos := $1C; end
      else if Pos('{\an4}', Text) > 0 then begin StyleBlock.HorizontalPos := $8; StyleBlock.VerticalPos := $05; end
      else if Pos('{\an5}', Text) > 0 then begin StyleBlock.HorizontalPos := $8; StyleBlock.VerticalPos := $10; end
      else if Pos('{\an6}', Text) > 0 then begin StyleBlock.HorizontalPos := $8; StyleBlock.VerticalPos := $1C; end
      else if Pos('{\an1}', Text) > 0 then begin StyleBlock.HorizontalPos := $F; StyleBlock.VerticalPos := $05; end
      else if Pos('{\an2}', Text) > 0 then begin StyleBlock.HorizontalPos := $F; StyleBlock.VerticalPos := $10; end
      else if Pos('{\an3}', Text) > 0 then begin StyleBlock.HorizontalPos := $F; StyleBlock.VerticalPos := $1E; end;

      Plain := RemoveTSTags(Text) + ' ';

      // Procesamiento en Buffer (calcular longitud exacta con inyecciones $d0)
      SetLength(TextBytes, Plain.Length * 2 + 10);
      ByteCount := 0;

      if IsItalic then
      begin
        TextBytes[ByteCount] := $d0;
        Inc(ByteCount);
      end;

      j := 1;
      while j <= Length(Plain) do
      begin
        if (j < Length(Plain)) and (Plain[j] = #13) and (Plain[j+1] = #10) then
        begin
          TextBytes[ByteCount] := 0; // Null byte para nueva linea Cheetah
          Inc(ByteCount);
          if IsItalic then
          begin
            TextBytes[ByteCount] := $d0; // Reactivar cursiva tras salto
            Inc(ByteCount);
          end;
          Inc(j, 2);
          Continue;
        end;

        idx := -1;
        for c := 0 to Length(TLatinLetters)-1 do
          if Plain[j] = TLatinLetters[c] then
          begin
            idx := c;
            Break;
          end;

        if idx >= 0 then
        begin
          TextBytes[ByteCount] := TLatinCodes[idx];
          Inc(ByteCount);
        end
        else
        begin
          TextBytes[ByteCount] := Encoding.GetBytes(Plain[j])[0];
          Inc(ByteCount);
        end;

        inc(j);
      end;

      len    := ByteCount + 20;
      endpos := Stream.Position + len;

      WriteByte(Stream, Byte(len));
      WriteByte(Stream, $61);

      WriteTime(Stream, Subtitles.InitialTime[i], FPS);
      WriteTime(Stream, Subtitles.FinalTime[i], FPS);

      Stream.WriteBuffer(StyleBlock, SizeOf(TStyleBlock));

      if ByteCount > 0 then
        Stream.WriteBuffer(TextBytes[0], ByteCount);

      while endpos > Stream.Position do WriteByte(Stream, 0);
    end;

    Result := True;
  finally
    Stream.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
