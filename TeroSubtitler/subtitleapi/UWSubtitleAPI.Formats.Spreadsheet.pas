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

unit UWSubtitleAPI.Formats.Spreadsheet;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, Classes, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.SysUtils,
  UWSubtitleAPI.Formats;

type

  { TUWSpreadsheet }

  TUWSpreadsheet = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function IsTextBased: Boolean; override;
    function IsTimeBased: Boolean; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Tags, fpSpreadsheet, fpsAllFormats;

// -----------------------------------------------------------------------------

function TUWSpreadsheet.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWSpreadsheet.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfSpreadsheet;
end;

// -----------------------------------------------------------------------------

function TUWSpreadsheet.Extension: String;
begin
  Result := '*.xls;*.xlsx;*.ods';
end;

// -----------------------------------------------------------------------------

function TUWSpreadsheet.IsTextBased: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWSpreadsheet.IsTimeBased: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWSpreadsheet.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWSpreadsheet.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
var
  s : String;
begin
  s := LowerCase(ExtractFileExt(SubtitleFile.FileName));
  if ((s = '.xls') or (s = '.xlsx') or (s = '.ods')) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWSpreadsheet.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  Workbook    : TsWorkbook;
  Worksheet   : TsWorksheet;
  col, row    : Cardinal;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text, s     : String;
  sl          : TStrings;
begin
  Result := False;

  Workbook := TsWorkbook.Create;
  try
    Workbook.ReadFromFile(SubtitleFile.FileName);
    if Workbook.GetWorksheetCount > 0 then
    begin
      if Assigned(Subtitles.OnLoadDataFunc) and (Workbook.GetWorksheetCount > 1) then
      begin
        try
          sl := TStringList.Create;
          for col := 0 to Workbook.GetWorksheetCount-1 do
            sl.Add(Workbook.GetWorksheetByIndex(col).Name);

          Worksheet := Workbook.GetWorksheetByIndex(Subtitles.OnLoadDataFunc(sl, sl.ClassName));
        finally
          sl.Free;
        end;
      end
      else
        Worksheet := Workbook.GetFirstWorksheet;

      for row := 0 to Worksheet.GetLastRowIndex do
      begin
        InitialTime := -1;
        FinalTime   := -1;
        Text        := '';
        for col := 0 to Worksheet.GetLastColIndex do
        begin
          s := Worksheet.ReadAsText(row, col);
          if not s.IsEmpty then
          begin
            if StrToIntDef(s, -1) > -1 then
            begin
              //writeln('index: ' + s);
            end
            else if (StringToTime(s, False, FPS) > 0) and (InitialTime = -1) and (FinalTime = -1) then
            begin
              InitialTime := StringToTime(s, False, FPS);
              //writeln('it: ' + s);
            end
            else if (StringToTime(s, False, FPS) > 0) and (FinalTime = -1) and (InitialTime > -1) then
            begin
              FinalTime := StringToTime(s, False, FPS);
              //writeln('ft: ' + s);
            end
            else if (InitialTime > 0) and (FinalTime > 0) and Text.IsEmpty then
            begin
              Text := HTMLTagsToTS(s);
              //writeln('text: ' + Text);
            end;
          end;
        end;

        if (InitialTime >= 0) and (FinalTime > 0) then
          Subtitles.Add(InitialTime, FinalTime, Text, '');
      end;
    end;
  finally
    Workbook.Free;
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSpreadsheet.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  Workbook  : TsWorkbook;
  Worksheet : TsWorksheet;
  i         : Integer;
  Count     : Integer;
begin
  Result  := False;

  Workbook := TsWorkbook.Create;
  try
    Worksheet := Workbook.AddWorksheet(ChangeFileExt(ExtractFileName(FileName), ''));
    //Worksheet := Workbook.AddWorksheet('Tero Subtitler');
    Count := 0;
    for i := FromItem to ToItem do
    begin
      Worksheet.WriteNumber(Count, 0, Count+1);
      Worksheet.WriteText(Count, 1, TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:ff', FPS));
      Worksheet.WriteText(Count, 2, TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:ff', FPS));
      Worksheet.WriteText(Count, 3, TSTagsToHTML(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i])));
    end;

    Workbook.WriteToFile(FileName, True);
    Result := True;
  finally
    Workbook.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
