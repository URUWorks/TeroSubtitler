{*
 *  URUWorks
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
 *  Copyright (C) 2023-2024 URUWorks, uruworks@gmail.com.
 *}

unit formStatistics;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf,
  LCLType, LCLTranslator, procLocalize, lazUTF8;

type

  { TfrmStatistics }

  TfrmStatistics = class(TForm)
    btnExport: TButton;
    btnClose: TButton;
    mmoStatistics: TMemo;
    procedure btnCloseClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure GetSubtitleStatics;
  public

  end;

var
  frmStatistics: TfrmStatistics;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procSubtitle, UWSystem.StrUtils, procColorTheme,
  Math, UWSubtitleAPI.Tags;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmStatistics }

// -----------------------------------------------------------------------------

procedure TfrmStatistics.FormCreate(Sender: TObject);
begin
  GetSubtitleStatics;
end;

// -----------------------------------------------------------------------------

procedure TfrmStatistics.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmStatistics := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmStatistics.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmStatistics.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmStatistics.btnExportClick(Sender: TObject);
begin
  with TSaveDialog.Create(NIL) do
  try
    Title  := lngSaveFile;
    Filter := lngscShotChanges + ' (*.txt)|*.txt';
    FilterIndex := 0;
    DefaultExt := '.txt';
    FileName := ChangeFileExt(ExtractFileName(SubtitleInfo.Text.FileName), DefaultExt);
    Options := [ofOverwritePrompt, ofEnableSizing];
    if Execute then
    begin
      if ExtractFileExt(FileName) <> DefaultExt then
        ChangeFileExt(FileName, DefaultExt);

      mmoStatistics.Lines.SaveToFile(FileName);
    end;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmStatistics.GetSubtitleStatics;
var
  NumberOfWords: Integer = 0;
  NumberOfCharacters: Integer = 0;
  EntriesWithoutText: Integer = 0;
  TotalDuration: Integer = 0;
  TotalCharactersPerSecond: Double = 0;
  ShortestLine: Integer = 99999999;
  LongestLine: Integer = 0;
  ShortestLineIdx: Integer = 0;
  LongestLineIdx: Integer = 0;
  ShortestDuration: Integer = 100000000;
  LongestDuration: Integer = 0;
  ShortestDurationIdx: Integer = 0;
  LongestDurationIdx: Integer = 0;
  ShortestCharactersPerSecond: Double = 100000000;
  LongestCharactersPerSecond: Double = 0;
  ShortestCharactersPerSecondIdx: Integer = 0;
  LongestCharactersPerSecondIdx: Integer = 0;
  ShortestWordsPerMinute: Double = 100000000;
  LongestWordsPerMinute: Double = 0;
  ShortestWordsPerMinuteIdx: Integer = 0;
  LongestWordsPerMinuteIdx: Integer = 0;
  TotalWordsPerMinute: Double = 0;
  ShortestGap: Integer = 100000000;
  LongestGap: Integer = 0;
  TotalGap: Integer = 0;
  ShortestGapIdx: Integer = 0;
  LongestGapIdx: Integer = 0;
  i, l: Integer;
  d: Double;
begin
  if Subtitles.Count = 0 then Exit;

  for i := 0 to Subtitles.Count-1 do
  begin
    if Subtitles.Text[i].IsEmpty then
      Inc(EntriesWithoutText);

    NumberOfWords += WordCount(RemoveTSTags(Subtitles.Text[i]));

    l := GetTextLength(Subtitles.Text[i]);
    ShortestLine := Min(l, ShortestLine);
    if ShortestLine = l then ShortestLineIdx := i+1;
    LongestLine := Max(l, LongestLine);
    if LongestLine = l then LongestLineIdx := i+1;
    NumberOfCharacters += l;

    l := Subtitles.Duration[i];
    ShortestDuration := Min(l, ShortestDuration);
    if ShortestDuration = l then ShortestDurationIdx := i+1;
    LongestDuration := Max(l, LongestDuration);
    if LongestDuration = l then LongestDurationIdx := i+1;
    TotalDuration += l;

    d := Subtitles.TextCPS[i, ''];
    ShortestCharactersPerSecond := Min(d, ShortestCharactersPerSecond);
    if ShortestCharactersPerSecond = d then ShortestCharactersPerSecondIdx := i+1;
    LongestCharactersPerSecond := Max(d, LongestCharactersPerSecond);
    if LongestCharactersPerSecond = d then LongestCharactersPerSecondIdx := i+1;
    TotalCharactersPerSecond += d;

    d := Subtitles.TextWPM[i];
    ShortestWordsPerMinute := Min(d, ShortestWordsPerMinute);
    if ShortestWordsPerMinute = d then ShortestWordsPerMinuteIdx := i+1;
    LongestWordsPerMinute := Max(d, LongestWordsPerMinute);
    if LongestWordsPerMinute = d then LongestWordsPerMinuteIdx := i+1;
    TotalWordsPerMinute += d;

    if i < Subtitles.Count-1 then
    begin
      l := Subtitles.Pause[i];
      ShortestGap := Min(l, ShortestGap);
      if ShortestGap = l then ShortestGapIdx := i+1;
      LongestGap := Max(l, LongestGap);
      if LongestGap = l then LongestGapIdx := i+1;
      TotalGap += l;
    end;
  end;

  with mmoStatistics.Lines do
  begin
    BeginUpdate;
    try
      Add(lngstNumberOfEntries, [Subtitles.Count]);
      Add(lngstTotalDuration, [GetTimeStr(TotalDuration, True)]);
      Add(lngstNumberOfWords, [NumberOfWords]);
      Add(lngstNumberOfCharacters, [NumberOfCharacters]);
      Add(lngstTotalCharactersPerSecond, [TotalCharactersPerSecond]);
      Add(lngstEntriesWithoutText, [EntriesWithoutText]);
      Add('');
      Add(lngstShortestLine, [ShortestLine, ShortestLineIdx]);
      Add(lngstLongestLine, [LongestLine, LongestLineIdx]);
      Add('');
      Add(lngstShortestDuration, [GetTimeStr(ShortestDuration, True), ShortestDurationIdx]);
      Add(lngstLongestDuration, [GetTimeStr(LongestDuration, True), LongestDurationIdx]);
      Add(lngstDurationAverage, [GetTimeStr(TotalDuration div Subtitles.Count, True)]);
      Add('');
      Add(lngstShortestCharactersPerSecond, [ShortestCharactersPerSecond, ShortestCharactersPerSecondIdx]);
      Add(lngstLongestCharactersPerSecond, [LongestCharactersPerSecond, LongestCharactersPerSecondIdx]);
      Add(lngstCharactersPerSecondAverage, [TotalCharactersPerSecond / Subtitles.Count]);
      Add('');
      Add(lngstShortestWordsPerMinute, [ShortestWordsPerMinute, ShortestWordsPerMinuteIdx]);
      Add(lngstLongestWordsPerMinute, [LongestWordsPerMinute, LongestWordsPerMinuteIdx]);
      Add(lngstWordsPerMinuteAverage, [TotalWordsPerMinute / Subtitles.Count]);
      Add('');
      Add(lngstShortestGap, [GetTimeStr(ShortestGap, True), ShortestGapIdx]);
      Add(lngstLongestGap, [GetTimeStr(LongestGap, True), LongestGapIdx]);
      Add(lngstGapAverage, [GetTimeStr(TotalGap div Subtitles.Count - 1, True)]);
    finally
      EndUpdate;
    end;
  end;
end;

// -----------------------------------------------------------------------------

end.

