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

unit UWSubtitleAPI.Formats.CSV;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.SysUtils,
  UWSubtitleAPI.Formats;

type

  { TUWCSV }

  TUWCSV = class(TUWSubtitleCustomFormat)
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

function TUWCSV.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWCSV.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfCSV;
end;

// -----------------------------------------------------------------------------

function TUWCSV.Extension: String;
begin
  Result := '*.csv';
end;

// -----------------------------------------------------------------------------

function TUWCSV.IsTimeBased: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWCSV.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWCSV.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (LowerCase(ExtractFileExt(SubtitleFile.FileName)) = '.csv') and
   ((SubtitleFile[Row].CountChar(';') > 2) or (SubtitleFile[Row].CountChar(',') > 2)) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWCSV.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  CSVDoc      : TCSVDocument;
  i, x        : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  CSVDoc := TCSVDocument.Create;
  try
    if SubtitleFile[0].CountChar(';') > 2 then
      CSVDoc.Delimiter := ';'
    else if SubtitleFile[0].CountChar(',') > 2 then
      CSVDoc.Delimiter := ',';

    CSVDoc.CSVText := SubtitleFile.Text;

    if CSVDoc.MaxColCount > 0 then
    begin
      for i := 0 to CSVDoc.RowCount-1 do
      begin
        InitialTime := -1;
        FinalTime   := -1;
        Text        := '';

        for x := 0 to CSVDoc.MaxColCount-1 do
        begin
          if StrToIntDef(CSVDoc.Cells[x, i], -1) > -1 then
          begin
            //writeln('index: ' + CSVDoc.Cells[x, i]);
          end
          else if (StringToTime(CSVDoc.Cells[x, i], False, FPS) > 0) and (InitialTime = -1) and (FinalTime = -1) then
          begin
            InitialTime := StringToTime(CSVDoc.Cells[x, i], False, FPS);
            //writeln('it: ' + IntToStr(InitialTime));
          end
          else if (StringToTime(CSVDoc.Cells[x, i], False, FPS) > 0) and (FinalTime = -1) and (InitialTime > -1) then
          begin
            FinalTime := StringToTime(CSVDoc.Cells[x, i], False, FPS);
            //writeln('ft: ' + IntToStr(FinalTime));
          end
          else if (InitialTime > 0) and (FinalTime > 0) and Text.IsEmpty then
          begin
            Text := HTMLTagsToTS(CSVDoc.Cells[x, i]);
            //writeln('text: ' + Text);
          end;
        end;

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

function TUWCSV.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i     : Integer;
  Count : Integer;
  Text  : String;
  XY    : String;
begin
  Result  := False;

  StringList.Add('No.,Timecode In,Timecode Out,Subtitle');
  Count := 1;
  for i := FromItem to ToItem do
  begin
    StringList.Add(SysUtils.Format('"%d","%s","%s","%s"',
      [Count,
       TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:ff', FPS), //iff(Subtitles.TimeBase = stbMedia, TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss.zzz'), TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:ff', FPS)),
       TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:ff', FPS), //iff(Subtitles.TimeBase = stbMedia, TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss.zzz'), TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:ff', FPS)),
       TSTagsToHTML(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]))]), False);
    Inc(Count);
  end;

  try
    StringList.SaveToFile(FileName, TEncoding.UTF8); // The character set used by data contained in the file MUST be an 8-bit (UTF-8)
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
