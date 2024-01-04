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

uses UWSubtitleAPI.Tags, fpSpreadsheet, fpsAllFormats, fpsTypes, Math;

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
  Workbook : TsWorkbook;
  Worksheet : TsWorksheet;
  col, row : Cardinal;
  InitialTime : Integer;
  FinalTime : Integer;
  Text : String;
  sl : TStrings;
  iIT, iFT, iT : Integer;
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

      if Worksheet.GetLastColIndex < 3 then Exit; // Minimum columns required

      // find necessary indexes
      for row := 0 to Worksheet.GetLastRowIndex do
      begin
        iIT  := -1;
        iFT  := -1;
        iT   := -1;
        Text := '';

        for col := 0 to Worksheet.GetLastColIndex do
        begin
          Text := Worksheet.ReadAsText(row, col);
          if not Text.IsEmpty then
          begin
            if StrToIntDef(Text, -1) > -1 then
            begin
            end
            else if (StringToTime(Text, False, FPS) > 0) and (iIT = -1) and (iFT = -1) then
            begin
              iIT := col;
            end
            else if (StringToTime(Text, False, FPS) > 0) and (iFT = -1) and (iIT > -1) then
            begin
              iFT := col;
            end
            else if (iIT > 0) and (iFT > 0) and (iT < 0) then
            begin
              iT := col;
            end;
          end;
        end;

        if (iIT >= 0) and (iFT >= 0) and (iT >= 0) then Break;
      end;

      if (iIT < 0) and (iFT < 0) and (iT < 0) then Exit; // necessary indices were not found

      for row := 0 to Worksheet.GetLastRowIndex do
      begin
        InitialTime := -1;
        FinalTime   := -1;
        Text        := '';

        InitialTime := StringToTime(Worksheet.ReadAsText(row, iIT), False, FPS);
        FinalTime := StringToTime(Worksheet.ReadAsText(row, iFT), False, FPS);
        Text := HTMLTagsToTS(Worksheet.ReadAsText(row, iT));

        if (row < Worksheet.GetLastRowIndex) and Worksheet.ReadAsText(row+1, iIT).IsEmpty and Worksheet.ReadAsText(row+1, iFT).IsEmpty and not Worksheet.ReadAsText(row+1, iT).IsEmpty then
          Text += sLineBreak + HTMLTagsToTS(Worksheet.ReadAsText(row+1, iT));

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
  WorkName  : String;
  NewName   : String;
begin
  Result  := False;

  Workbook := TsWorkbook.Create;
  try
    WorkName := ChangeFileExt(ExtractFileName(FileName), '');
    SetLength(WorkName, Min(Length(WorkName), 31)); // The length must be less than or equal to 31 characters
    Worksheet := Workbook.AddWorksheet(WorkName);
    //Worksheet := Workbook.AddWorksheet('Tero Subtitler');
    Count := 0;
    for i := FromItem to ToItem do
    begin
      Worksheet.WriteNumber(Count, 0, Count+1);
      Worksheet.WriteText(Count, 1, TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:ff', FPS));
      Worksheet.WriteText(Count, 2, TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:ff', FPS));
      Worksheet.WriteText(Count, 3, TSTagsToHTML(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i])));
      Inc(Count);
    end;

    NewName := FileName;
    WorkName := LowerCase(ExtractFileExt(NewName));
    if (WorkName <> STR_EXCEL_EXTENSION) or (WorkName <> STR_OOXML_EXCEL_EXTENSION) or (WorkName <> STR_OPENDOCUMENT_CALC_EXTENSION) then
      NewName := ChangeFileExt(NewName, STR_EXCEL_EXTENSION);

    Workbook.WriteToFile(NewName, True);
    Result := True;
  finally
    Workbook.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
