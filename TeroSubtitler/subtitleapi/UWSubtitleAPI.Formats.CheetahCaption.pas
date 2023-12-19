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
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.ExtraInfo, UWSubtitleAPI.Tags, UWSystem.SysUtils,
  UWSubtitleAPI.Formats.CheetahCaption.Types;

// -----------------------------------------------------------------------------

function DecodeTime(const Bytes: TBytes; const Index: Integer; const FPS: Single): Integer;
begin
  Result := EncodeTime(Bytes[Index], Bytes[Index + 1], Bytes[Index + 2], FramesToTime(Bytes[Index + 3], FPS));
end;

// -----------------------------------------------------------------------------

procedure WriteByte(const Stream: TStream; const bByte: Byte);
begin
  Stream.Write(bByte, 1);
end;

// -----------------------------------------------------------------------------

procedure WriteTime(const Stream: TStream; const Time: Cardinal; const FPS: Single);
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

function TUWCheetahCaption.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;

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
  index,
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
      while (i < Length(Bytes) - 20) do
      begin
        Len     := Bytes[i];
        TextLen := Len - 20;
        start   := 19;

        j := 0;
        while j < 4 do
        begin
          if Bytes[i + start - 1] > $10 then
          begin
            Dec(start);
            Inc(TextLen);
          end;
          inc(j);
        end;

        if (TextLen > 0) and (Length(Bytes) >= i + TextLen) then
        begin
          InitialTime := DecodeTime(Bytes, i + 2, FPS);
          if (Bytes[i + 6] = 2) and (Bytes[i + 7] = 1) and (Bytes[i + 8] = 0) and (Bytes[i + 9] = 0) then
            ft := -1
          else
            ft := DecodeTime(Bytes, i + 6, FPS);

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
            else if (Bytes[index] >= $C0) or (Bytes[index] <= $14) then // styles?
            begin
              if (Bytes[index] = $d0) then // italic
                italics := True;
            end
            else
            begin
              Text := Text + TEncoding.GetEncoding(SubtitleFile.CodePage).GetString(Bytes, index, 1);
            end;
            inc(j);
          end;

          Text := Text.Trim;
          if italics then Text := '{\i1}' + Text + '{\i0}';

          if ft >= 0 then
            FinalTime := ft
          else
            FinalTime := InitialTime + CalculateOptimalDisplayMS(Text);

          Subtitles.Add(InitialTime, FinalTime, Text, '', NIL);
        end;
        if Len = 0 then Inc(Len);
        inc(i, Len);
      end;
    end;
  finally
    FileStream.Free;
    if Subtitles.Count > 0 then
    begin
      Subtitles.ExtraInfoType := eiNone; //eiCheetahCaption;
      Result := True;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TUWCheetahCaption.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  Stream       : TStream;
  i, j, c, idx : Integer;
  HeaderBlock  : THeaderBlock;
  StyleBlock   : TStyleBlock;
  text         : String;
  len          : Integer;
  endpos       : Integer;
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

    // Subtitles
    with StyleBlock do
    begin
      UnknowCodes1[0] := $12;
      UnknowCodes1[1] := 1;
      UnknowCodes1[2] := 0;
      UnknowCodes1[3] := 0;
      UnknowCodes1[4] := 0;
      UnknowCodes1[5] := 0;
      Justification   := 3;
      HorizontalPos   := $F;
      VerticalPos     := $10;
    end;

    for i := FromItem to ToItem do
    begin
      Text := RemoveTSTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i])) + ' ';

      len    := Text.Length + 20;
      endpos := Stream.Position + len;

      WriteByte(Stream, Byte(len));
      WriteByte(Stream, $61);

      WriteTime(Stream, Subtitles.InitialTime[i], FPS);
      WriteTime(Stream, Subtitles.FinalTime[i], FPS);

      Stream.WriteBuffer(StyleBlock, SizeOf(TStyleBlock));

      j := 0;
      while j < Text.Length do
      begin
        idx := -1;
        for c := 0 to Length(TLatinLetters)-1 do
          if Text[j] = TLatinLetters[c] then
          begin
            idx := c;
            Break;
          end;

        if idx >= 0 then
          WriteByte(Stream, Byte(TLatinCodes[idx]))
        else
          WriteByte(Stream, Encoding.GetBytes(Text[j])[0]); //TEncoding.GetEncoding(1252).GetBytes(Text[j])[0]);


        inc(j);
      end;

      while endpos > Stream.Position do WriteByte(Stream, 0);
    end;

    Result := True;
  finally
    Stream.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
