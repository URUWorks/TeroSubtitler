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
 *  Copyright (C) 2023 URUWorks, uruworks@gmail.com.
 *}

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESelectedSubtitleItem(Sender: TObject;
  const Index: Integer; const SubtitleItem: TUWSubtitleItem;
  const IsDynamic: Boolean);
begin
  VSTSelectNode(VST, Index, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESelectedSubtitleItemChange(Sender: TObject);
begin
  UpdateValuesForSubtitle(VSTFocusedNode(VST));
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESelectedSubtitleItemChanged(Sender: TObject;
  const Index: Integer; const OldInitialTime, OldFinalTime: Integer;
  const NeedSort: Boolean);
var
  Item : TUWSubtitleItem;
begin
  Item := Subtitles[Index];
  Item.InitialTime := OldInitialTime;
  Item.FinalTime   := OldFinalTime;

  UndoInstance.AddUndo(utSubtitleChange, Index, Item);
  UpdateValues(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESelectionChange(Sender: TObject);
begin
  //actMediaAddSubtitle.Enabled   := not WAVE.SelectionIsEmpty and WAVE.IsOnlySelection;
  actMediaZoomSelection.Enabled := actMediaAddSubtitle.Enabled;
{  if WAVE.SelectionIsEmpty then
    stbStatus.Panels[1].Text := ''
  else
    stbStatus.Panels[1].Text := Format(Strings.Selection,
      [TimeToString(WAVE.Selection.InitialTime, DefTimeFormat, FPS.FPS, True),
      TimeToString(WAVE.Selection.FinalTime, DefTimeFormat, FPS.FPS, True)]);}
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DoWaveformTimer(Sender: TObject);
begin
  if WAVE.IsMouseDown then Exit;
  TimerWaveform.Enabled := False;

  if WAVE.IsTimeLineEnabled then
  begin
    WAVE.SetPlayCursorMS(MPV.GetMediaPosInMs);
  end;

  TimerWaveform.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVEClick(Sender: TObject);
begin
  if (MPV.GetMediaLenInMs > 0) then
  begin
    WAVE.GenerateDummyPeakTab(MPV.GetMediaLenInMs);
    EnableActionsByTag([TAG_ACTION_AUDIO], True);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVETimeLineClick(Sender: TObject; const Time: Integer);
begin
  if MPV.Initialized then
    MPV.SeekInMs(Time);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVEContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  Handled := not WAVE.IsTimeLineEnabled;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.GoToNextShotChange(const APrevious: Boolean = False);
var
  x: Integer;
begin
  with WAVE do
    if IsTimeLineEnabled then
    begin
      if APrevious then
        x := GetPreviousSceneChange(CursorPosMS-1)
      else
        x := GetNextSceneChange(CursorPosMS+1);

      if x > 0 then
      begin
        if MPV.IsMediaLoaded then
        begin
          if MPV.IsPlaying then MPV.Pause;
          SetCursorMS(x);
          MPV.SetMediaPosInMs(x);
        end
        else
          WAVE.SetPlayCursorMS(x, True);
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DetectDialogSegments;
var
  DialogList: TZonesList;
  i, x: Integer;
  //s: String;
begin
  if not WAVE.IsPeakDataLoaded then
    actWaveExtract.Execute;

  if WAVE.IsPeakDataLoaded and (CustomQuestionDialog(lngDetectDialogSegmentsWarning, lngContinueAnyway, [dbYes, dbNo]) = mrYes) then
    if WAVE.DetectZoneList(DialogList, False, AppOptions.DialogSegmentThreshold) then
    try
      //s := GetCommonString('Entry');

      UndoInstance.IncrementUndoGroup;
      ClearSubtitles(False);
      x := 0;
      for i := 0 to Length(DialogList)-1 do
        with DialogList[i]^ do
        begin
          if (Stop-Start) >= 500 then
          begin
            InsertSubtitle(x, Start, Stop, Format(lngEntry, [x+1]), '', False, False);
            Inc(x);
          end;
        end;

      UndoInstance.IncrementUndoGroup;
      SubtitleChanged(True, True);
      UpdateValues(True);
    finally
      WAVE.ClearZoneList(DialogList);
    end;
end;

// -----------------------------------------------------------------------------

