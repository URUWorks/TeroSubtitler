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

unit UWSubtitleAPI.Formats.TSV;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, StrUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.SysUtils,
  UWSubtitleAPI.Formats;

type

  { TUWTSV }

  TUWTSV = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function IsTimeBased: Boolean; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Tags, csvdocument;

// -----------------------------------------------------------------------------

{
Since the values in the TSV format cannot contain literal tabs or newline characters, a convention is necessary for lossless conversion of text values with these characters. A common convention is to perform the following escapes:[5][6]

escape sequence	meaning
\n	line feed
\t	tab
\r	carriage return
\\	backslash

Another common convention is to use the CSV convention from RFC 4180 and enclose values containing tabs or newlines in double quotes. This can lead to ambiguities.
Another ambiguity is whether records are separated by a line feed, as is typical for Unix platforms, or a carriage return and line feeds, as is typical for Microsoft platforms. Many programs such as LibreOffice expect a carriage return followed by a newline.
}

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function TSVEncode(const AStr: String): String;
begin
  Result := StringsReplace(AStr, [sLineBreak, #9, '\'], ['\r\n', '\t', '\\'], [rfReplaceAll]);
end;

// -----------------------------------------------------------------------------

function TSVDecode(const AStr: String): String;
begin
  Result := StringsReplace(AStr, ['\r\n', '\r', '\n', '\t', '\\'], [sLineBreak, '', sLineBreak, #9, '\'], [rfReplaceAll]);
end;

// -----------------------------------------------------------------------------

{ TUWTSV }

// -----------------------------------------------------------------------------

function TUWTSV.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWTSV.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfTSV;
end;

// -----------------------------------------------------------------------------

function TUWTSV.Extension: String;
begin
  Result := '*.tsv';
end;

// -----------------------------------------------------------------------------

function TUWTSV.IsTimeBased: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWTSV.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWTSV.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (LowerCase(ExtractFileExt(SubtitleFile.FileName)) = '.tsv') and
   (SubtitleFile[Row].CountChar(#9) > 2) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWTSV.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  CSVDoc : TCSVDocument;
  y, x,
  iIT, iFT, iT : Integer;
  InitialTime : Integer;
  FinalTime : Integer;
  Text : String;
begin
  Result := False;
  CSVDoc := TCSVDocument.Create;
  try
    CSVDoc.Delimiter := #9;
    CSVDoc.CSVText := SubtitleFile.Text;

    if CSVDoc.MaxColCount > 2 then // Minimum columns required
    begin
      // find necessary indexes
      for y := 0 to CSVDoc.RowCount-1 do
      begin
        iIT := -1;
        iFT := -1;
        iT := -1;

        for x := 0 to CSVDoc.MaxColCount-1 do
        begin
          if StrToIntDef(CSVDoc.Cells[x, y], -1) > -1 then
          begin
          end
          else if (StringToTime(CSVDoc.Cells[x, y], False, FPS) > 0) and (iIT = -1) and (iFT = -1) then
          begin
            iIT := x;
          end
          else if (StringToTime(CSVDoc.Cells[x, y], False, FPS) > 0) and (iFT = -1) and (iIT > -1) then
          begin
            iFT := x;
          end
          else if (iIT > 0) and (iFT > 0) and (iT < 0) then
          begin
            iT := x;
          end;
        end;

        if (iIT >= 0) and (iFT >= 0) and (iT >= 0) then Break;
      end;

      if (iIT < 0) and (iFT < 0) and (iT < 0) then Exit; // necessary indices were not found

      for y := 0 to CSVDoc.RowCount-1 do
      begin
        InitialTime := -1;
        FinalTime   := -1;
        Text        := '';

        InitialTime := StringToTime(CSVDoc.Cells[iIT, y], False, FPS);
        FinalTime := StringToTime(CSVDoc.Cells[iFT, y], False, FPS);
        Text := HTMLTagsToTS(CSVDoc.Cells[iT, y]);

        if (y < CSVDoc.RowCount-1) and CSVDoc.Cells[iIT, y+1].IsEmpty and CSVDoc.Cells[iFT, y+1].IsEmpty and not CSVDoc.Cells[iT, y+1].IsEmpty then
          Text += sLineBreak + HTMLTagsToTS(CSVDoc.Cells[iT, y+1]);

        if (InitialTime >= 0) and (FinalTime > 0) then
          Subtitles.Add(InitialTime, FinalTime, Text, '');
      end;
    end;
  finally
    CSVDoc.Free;
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWTSV.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i     : Integer;
  Count : Integer;
begin
  Result  := False;

  StringList.Add(SysUtils.Format('No.%sTimecode In%sTimecode Out%sSubtitle', [#9, #9, #9]));
  Count := 1;
  for i := FromItem to ToItem do
  begin
    StringList.Add(SysUtils.Format('%d%s%s%s%s%s%s',
      [Count, #9,
       TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:ff', FPS), #9, //iff(Subtitles.TimeBase = stbMedia, TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss.zzz'), TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:ff', FPS)), #9,
       TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:ff', FPS), #9, //iff(Subtitles.TimeBase = stbMedia, TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss.zzz'), TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:ff', FPS)), #9,
       TSTagsToHTML(TSVEncode(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i])))]));
    Inc(Count);
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
