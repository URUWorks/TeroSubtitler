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

unit UWSubtitleAPI.Formats.EBU;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, UWSubtitleAPI, UWSystem.TimeUtils,
  UWSystem.SysUtils, UWSubtitleAPI.Formats, StrUtils;

type

  { TUWEBU }

  TUWEBU = class(TUWSubtitleCustomFormat)
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

uses UWSubtitleAPI.ExtraInfo, UWSubtitleAPI.Formats.EBU.Types,
  UWSystem.FileUtils;

// -----------------------------------------------------------------------------

procedure AnsiStringToAnsiChar(var Dest: array of AnsiChar; const Source: AnsiString);
var
  i, MaxLen: Integer;
begin
  MaxLen := High(Dest);
  if MaxLen > Length(Source) then MaxLen := Length(Source)-1;

  for i := 0 to MaxLen do
    Dest[i] := Source[i+1];
end;

// -----------------------------------------------------------------------------

procedure SubtitleNumberToByteArray(var Dest: array of Byte; const Source: Cardinal);
var
  s: String;
begin
  s := IntToHex(Source, 4);
  Dest[0] := HexToByte(Copy(s, 3, 2));
  Dest[1] := HexToByte(Copy(s, 1, 2));
end;

// -----------------------------------------------------------------------------

procedure TimeCodeToByteArray(var Dest: array of Byte; const Source: Cardinal; const FPS: Single);
var
  s: String;
begin
  s := TimeToString(Source, 'hhmmssff', FPS);
  Dest[0] := StrToInt(Copy(s, 1, 2));
  Dest[1] := StrToInt(Copy(s, 3, 2));
  Dest[2] := StrToInt(Copy(s, 5, 2));
  Dest[3] := StrToInt(Copy(s, 7, 2));
end;

// -----------------------------------------------------------------------------

function TUWEBU.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWEBU.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfEBU;
end;

// -----------------------------------------------------------------------------

function TUWEBU.Extension: String;
begin
  Result := '*.stl';
end;

// -----------------------------------------------------------------------------

function TUWEBU.IsTextBased: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWEBU.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWEBU.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
var
  sub : TUWSubtitles;
begin
  Result := False;
  if LowerCase(ExtractFileExt(SubtitleFile.FileName)) <> '.stl' then Exit;

  sub := TUWSubtitles.Create;
  try
    Result := LoadSubtitle(SubtitleFile, 0, sub);
  finally
    sub.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TUWEBU.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  UWFile      : TUWFile;
  GSIBlock    : TGSIBlock;
  TTIBlock    : TTTIBlock;
  Encoding    : TEncoding;
  TextBytes   : TBytes;
  ExtraInfo   : PEBU_ExtraInfo;
  FrameRate   : Single;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  UWFile := TUWFile.Create(SubtitleFile.FileName, fmOpenRead or fmShareDenyWrite);
  try
    if UWFile.Ready then
    begin
      if UWFile.Read(GSIBlock, GSIBlockSize) then
      begin
        with GSIBlock do
        begin
          Encoding := TEncoding.GetEncoding(CharCodeTableToEncoding(CharCodeTableNumber));
          if DiskFormatCode = DFC_25 then
            FrameRate := 1000 / 25
          else
            FrameRate := 1000 / 30;
        end;

        try
          while not UWFile.EOF do
            if UWFile.Read(TTIBlock, TTIBlockSize) then
            with TTIBlock do
            begin
              InitialTime := ((TimeCodeIn[0]*3600)*1000) + ((TimeCodeIn[1]*60)*1000) + (TimeCodeIn[2]*1000) + Round(TimeCodeIn[3]*FrameRate);
              FinalTime   := ((TimeCodeOut[0]*3600)*1000) + ((TimeCodeOut[1]*60)*1000) + (TimeCodeOut[2]*1000) + Round(TimeCodeOut[3]*FrameRate);

              ArrayToTBytes(TextBytes, TextField, 0);
              Text := ReplaceTagsEBU2TS(Encoding.GetString(TextBytes));

              if (InitialTime > -1) and (FinalTime > -1) then
              begin
                //if (MaxDuration > 0) and ((FinalTime + ExtraTime) > MaxDuration) Then
                //  Subtitles.Add(InitialTime + ExtraTime, InitialTime + ExtraTime + MaxDuration, Text)
                //else
                //  Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);

                New(ExtraInfo);
                ExtraInfo^.CumulativeStatus  := CumulativeStatus;
                ExtraInfo^.VerticalPosition  := VerticalPosition;
                ExtraInfo^.JustificationCode := JustificationCode;
                ExtraInfo^.CommentFlag       := CommentFlag;

                Subtitles.Add(InitialTime, FinalTime, Text, '', ExtraInfo, False);
              end;
            end;
          SetLength(TextBytes, 0);
        finally
          Encoding.Free;
        end;
      end;
    end;
  finally
    if Subtitles.Count > 0 then
    begin
      Subtitles.ExtraInfoType := eiEBU;
      Result := True;
    end;
    UWFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TUWEBU.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  Stream   : TStream;
  Buffer   : TBytes;
  GSIBlock : TGSIBlock;
  TTIBlock : TTTIBlock;
  i        : Integer;
begin
  Result := False;

  //if Encoding = NIL then
  //  Encoding := TEncoding.GetEncoding(CharCodeTableToEncoding(''));

  Stream := TFileStream.Create(FileName, fmCreate);
  try
    with Subtitles.FormatProperties^ do
    begin
      FillChar(GSIBlock, SizeOf(TGSIBlock), DSC_Undefined);
      with GSIBlock do
      begin
        AnsiStringToAnsiChar(CodePageNumber, EBU.CodePageNumber);
        if EBU.DiskFormatCode = 0 then
          DiskFormatCode    := DFC_25
        else
          DiskFormatCode    := DFC_30;
        DisplayStandardCode := EBU.DisplayStandardCode;
        AnsiStringToAnsiChar(CharCodeTableNumber, EBU.CharCodeTableNumber);
        AnsiStringToAnsiChar(LanguageCode, EBU.LanguageCode);
        //OriginalProgrammeTitle
        //OriginalEpisodeTitle
        //TranslatedProgrammeTitle
        //TranslatedEpisodeTitle
        //TranslatorName
        //TranslatorContactDetails
        //SubtitleListRefCode
        CreationDate   := '100101';
        RevisionDate   := '100101';
        RevisionNumber := '00';
        AnsiStringToAnsiChar(TotalNumberTTIBlocks, AnsiString(AddChar('0', IntToStr((ToItem-FromItem)+1), 5)));
        AnsiStringToAnsiChar(TotalNumberSubtitles, AnsiString(AddChar('0', IntToStr((ToItem-FromItem)+1), 5)));
        TotalNumberSubtitleGroups := '000';
        AnsiStringToAnsiChar(MaxNumberDisplayableChars, EBU.MaxNumberDisplayableChars);
        AnsiStringToAnsiChar(MaxNumberDisplayableRows, EBU.MaxNumberDisplayableRows);
        TimeCodeStatus      := TCS_Use;
        AnsiStringToAnsiChar(TimeCodeStartProgramme, AnsiString(TimeToString(Subtitles[0].InitialTime, 'hhmmssff', FPS)));
        AnsiStringToAnsiChar(TimeCodeFirstCue, AnsiString(TimeToString(Subtitles[0].InitialTime, 'hhmmssff', FPS)));
        TotalNumberDisks    := $31;
        DiskSequenceNumber  := $31;
        AnsiStringToAnsiChar(CountryOrigin, EBU.CountryOrigin);
        AnsiStringToAnsiChar(Publisher, 'URUWorks');
        AnsiStringToAnsiChar(EditorName, 'URUWorks');
        AnsiStringToAnsiChar(EditorContact, 'uruworks.net');
      end;
      Stream.WriteBuffer(GSIBlock, SizeOf(TGSIBlock));
    end;

    for i := FromItem to ToItem do
    begin
      FillChar(TTIBlock, SizeOf(TTTIBlock), TF_UnusedSpace);
      with TTIBlock do
      begin
        SubtitleGroupNumber := $00;
        SubtitleNumberToByteArray(SubtitleNumber, i);
        ExtBlockNumber      := $FF;
        CumulativeStatus    := CS_Not;
        if PEBU_ExtraInfo(Subtitles.ExtraInfo[i]) <> NIL then
        begin
          VerticalPosition  := PEBU_ExtraInfo(Subtitles.ExtraInfo[i])^.VerticalPosition;
          JustificationCode := PEBU_ExtraInfo(Subtitles.ExtraInfo[i])^.JustificationCode;
        end
        else
        begin
          VerticalPosition  := 0;
          JustificationCode := JC_Unchanged;
        end;
        CommentFlag         := CF_TFHaveData;

        TimeCodeToByteArray(TimeCodeIn, Subtitles.InitialTime[i], FPS);
        TimeCodeToByteArray(TimeCodeOut, Subtitles.FinalTime[i], FPS);
        Buffer := Encoding.GetBytes(ReplaceTagsTS2EBU(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i])));
        FillByte(TextField, SizeOf(TextField), 0);
        TBytesToArray(TextField, Buffer, 0);
      end;
      Stream.WriteBuffer(TTIBlock, SizeOf(TTTIBlock));
    end;
    Result := True;
  finally
    Stream.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
