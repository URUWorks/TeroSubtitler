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

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  ShowAbout;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actHelpExecute(Sender: TObject);
begin
  OpenURL(ProgramHelpURL);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCheckForUpdatesExecute(Sender: TObject);
begin
  CheckForUpdates;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actListModeExecute(Sender: TObject);
begin
  SetViewMode(vmList);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSourceModeExecute(Sender: TObject);
begin
  SetViewMode(vmSource);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTranscriptionModeExecute(Sender: TObject);
begin
  SetViewMode(vmTranscription);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTranslatorModeExecute(Sender: TObject);
begin
  actTranslatorMode.Checked := not actTranslatorMode.Checked;
  SetTranslatorMode(actTranslatorMode.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVideoPreviewExecute(Sender: TObject);
begin
  SetVideoPreview(not actVideoPreview.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actWaveformPreviewExecute(Sender: TObject);
begin
  SetWaveformPreview(not actWaveformPreview.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actNewSubtitleExecute(Sender: TObject);
begin
  NewSubtitle;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actLoadSubtitleExecute(Sender: TObject);
begin
  LoadSubtitleWithDialog;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actLoadTranslationExecute(Sender: TObject);
begin
  LoadSubtitleWithDialog(smTranslation);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSaveSubtitleExecute(Sender: TObject);
begin
  SaveCurrentSubtitle;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSaveSubtitleAsExecute(Sender: TObject);
begin
  SaveSubtitleWithDialog;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSaveTranslationExecute(Sender: TObject);
begin
  SaveCurrentSubtitle(smTranslation);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSaveTranslationAsExecute(Sender: TObject);
begin
  SaveSubtitleWithDialog(smTranslation);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCloseSubtitleExecute(Sender: TObject);
begin
  CloseSubtitle(False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCloseAppExecute(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUndoExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
  begin
    UndoInstance.Undo(VST);
    if WAVE.IsTimeLineEnabled then WAVE.ClearSelection;

    if MPVOptions.SubtitleHandleByMPV then
    begin
      if MPVSaveSubtitleTempTrack then
        MPVReloadSubtitleTempTrack;
    end;

    DoAutoCheckErrors(False, True);
  end
  else if Workspace.ViewMode = vmSource then
    mmoSourceView.Undo
  else
    TranscriptionUndo;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actRedoExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
  begin
    UndoInstance.Redo(VST);
    if WAVE.IsTimeLineEnabled then WAVE.ClearSelection;

    if MPVOptions.SubtitleHandleByMPV then
    begin
      if MPVSaveSubtitleTempTrack then
        MPVReloadSubtitleTempTrack;
    end;

    DoAutoCheckErrors(False, True);
  end
  else if Workspace.ViewMode = vmSource then
  begin
  end
  else
    TranscriptionRedo;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCutExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
    CopyToClipboard(True)
  else if Workspace.ViewMode = vmSource then
    mmoSourceView.CutToClipboard
  else
    TranscriptionCut;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
    CopyToClipboard
  else if Workspace.ViewMode = vmSource then
    mmoSourceView.CopyToClipboard
  else
    TranscriptionCopy;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actPasteExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
    PasteFromClipboard
  else if Workspace.ViewMode = vmSource then
    mmoSourceView.PasteFromClipboard
  else
    TranscriptionPaste;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCopyVideoPosToClipboardExecute(Sender: TObject);
begin
  CopyCurrentVideoPosToClipboard;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actInsertEntryBeforeExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
    VSTInsertEntries(VST, True)
  else if Workspace.ViewMode = vmSource then
    mmoSourceView.Lines.Insert(mmoSourceView.CaretPos.Y, '')
  else
    TranscriptionInsertAbove;
end;

procedure TfrmMain.actInsertEntryExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
    VSTInsertEntries(VST)
  else if Workspace.ViewMode = vmSource then
    mmoSourceView.Lines.Insert(mmoSourceView.CaretPos.Y+1, '')
  else
    TranscriptionInsertBelow;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDeleteEntryExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
  begin
    if AppOptions.AskForDeleteLines then
    begin
      if (MsgDeleteFiles = mrYes) then
        VSTDeleteEntries(VST);
    end
    else
      VSTDeleteEntries(VST);
  end
  else if Workspace.ViewMode = vmSource then
    mmoSourceView.CutToClipboard
  else
    TranscriptionDelete;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTimeModeExecute(Sender: TObject);
begin
  if Workspace.WorkMode <> wmTime then SetWorkMode(wmTime);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFramesModeExecute(Sender: TObject);
begin
  if Workspace.WorkMode <> wmFrames then SetWorkMode(wmFrames);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnDockVideoExecute(Sender: TObject);
begin
  SetDockVideoWindow(not actUnDockVideo.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnDockWaveformExecute(Sender: TObject);
begin
  SetDockWaveformWindow(not actUnDockWaveform.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowActorOnPreviewExecute(Sender: TObject);
begin
  //
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actPreviousSubtitleExecute(Sender: TObject);
begin
  SelectSubtitleAndFocusMemo(False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actNextSubtitleExecute(Sender: TObject);
begin
  SelectSubtitleAndFocusMemo(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontBoldExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
  begin
    if GetMemoFocused = NIL then
      VSTDoLoop(VST, @ApplyFontBold, dlSelected, True, True)
    else
      SetTextTag(tst_Bold);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontItalicExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
  begin
    if GetMemoFocused = NIL then
      VSTDoLoop(VST, @ApplyFontItalic, dlSelected, True, True)
    else
      SetTextTag(tst_Italic);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontUnderlineExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
  begin
    if GetMemoFocused = NIL then
      VSTDoLoop(VST, @ApplyFontUnderline, dlSelected, True, True)
    else
      SetTextTag(tst_Underline);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontStrikeoutExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
  begin
    if GetMemoFocused = NIL then
      VSTDoLoop(VST, @ApplyFontStrikeout, dlSelected, True, True)
    else
      SetTextTag(tst_Strikeout);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontClearExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
  begin
    if GetMemoFocused = NIL then
      VSTDoLoop(VST, @ApplyFontClear, dlSelected, True, True)
    else
      with GetMemoFocused do Text := RemoveTSTags(Text);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontColorDlgExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
    with TColorDialog.Create(Self) do
    try
      if Execute then
      begin
        SubtitleInfo.LastSubtitle.Color := Color;
        actFontColor.Execute;
      end;
    finally
      Free;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontColorExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
  begin
    if GetMemoFocused = NIL then
      VSTDoLoop(VST, @ApplyFontColor, dlSelected, True, True)
    else
      with SubtitleInfo do
        SetTextTagColor(Format('%s%s%s', [IntToHex(GetBValue(LastSubtitle.Color), 2),
          IntToHex(GetGValue(LastSubtitle.Color), 2),
          IntToHex(GetRValue(LastSubtitle.Color), 2)]));
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAddQuotationMarksExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
  begin
    if GetMemoFocused = NIL then
      VSTDoLoop(VST, @ApplyAddQuotationMarks, dlSelected, True, True)
    else
      SetTextCustomTag('"');
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAlignToLeftExecute(Sender: TObject);
begin
  SetAlignTo(shaLeft);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAlignToCenterExecute(Sender: TObject);
begin
  SetAlignTo(shaCenter);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAlignToRightExecute(Sender: TObject);
begin
  SetAlignTo(shaRight);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAlignToNoneExecute(Sender: TObject);
begin
  SetAlignTo(shaNone);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVAlignToBottomExecute(Sender: TObject);
begin
  SetVAlignTo(svaBottom);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVAlignToMiddleExecute(Sender: TObject);
begin
  SetVAlignTo(svaCenter);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVAlignToTopExecute(Sender: TObject);
begin
  SetVAlignTo(svaTop);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaPlayExecute(Sender: TObject);
begin
  MPV.Pause;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaStopExecute(Sender: TObject);
begin
  MPV.Stop;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaAutoScrollExecute(Sender: TObject);
begin
  actMediaAutoScroll.Checked := not actMediaAutoScroll.Checked;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaChangePlayRateExecute(Sender: TObject);
begin
  MPVAlterPlayRate(actMediaChangePlayRate.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaRewindExecute(Sender: TObject);
begin
  MPVSeekTo(False, MPVOptions.SeekTime);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaForwardExecute(Sender: TObject);
begin
  MPVSeekTo(True, MPVOptions.SeekTime);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaPreviousFrameExecute(Sender: TObject);
begin
  MPV.PreviousFrame(MPVOptions.FrameStep);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaNextFrameExecute(Sender: TObject);
begin
  MPV.NextFrame(MPVOptions.FrameStep);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaBack1SecExecute(Sender: TObject);
begin
  MPVSeekTo(False, MSecsPerSec);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaForward1SecExecute(Sender: TObject);
begin
  MPVSeekTo(True, MSecsPerSec);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaBack1MinExecute(Sender: TObject);
begin
  MPVSeekTo(False, MSecsPerSec*SecsPerMin);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaForward1MinExecute(Sender: TObject);
begin
  MPVSeekTo(True, MSecsPerSec*SecsPerMin);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaMoveSubtitleEntryExecute(Sender: TObject);
begin
  if (MPV.GetMediaLenInMs > 0) and actVideoPreview.Checked and (VST.SelectedCount > 0) then
    VSTMoveEntry(VST, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaSetInitialTimeExecute(Sender: TObject);
begin
  if not (MPV.GetMediaLenInMs > 0) or (VSTFocusedNode(VST) < 0) then Exit;
  VSTDoLoop(VST, @ApplySetTimeInitialFromMPV, dlSelected, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaSetFinalTimeExecute(Sender: TObject);
begin
  if not (MPV.GetMediaLenInMs > 0) or (VSTFocusedNode(VST) < 0) then Exit;
  VSTDoLoop(VST, @ApplySetTimeFinalFromMPV, dlSelected, True, True);
  SelectSubtitleAndFocusMemo(True, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaStartSubtitleEntryExecute(Sender: TObject);
begin
  if not (MPV.GetMediaLenInMs > 0) then Exit;
  with SubtitleInfo.LastSubtitle do
  begin
    InitialTime := MPV.GetMediaPosInMs;
    SetStatusBarText(Format(lngEntryStartedAt, [GetTimeStr(InitialTime, True)]));
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaEndSubtitleEntryExecute(Sender: TObject);
var
  i, l: Integer;
begin
  if not (MPV.GetMediaLenInMs > 0) then Exit;

  with SubtitleInfo.LastSubtitle do
  begin
    l := MPV.GetMediaPosInMs;
    i := Subtitles.FindInsertPos(InitialTime, l);

    if l < InitialTime then
      Exit
    else if l = InitialTime then
      l := AppOptions.Conventions.MinDuration;

    InsertSubtitle(i, InitialTime, l, '', '');
    SetStatusBarText(Format(lngEntryEndedAt, [GetTimeStr(l, True)]));
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaPlaySelectionExecute(Sender: TObject);
begin
  ProcMPV.MPVPlay(mpmSelection);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaPlayFromSelectionExecute(Sender: TObject);
begin
  ProcMPV.MPVPlay(mpmFromSelection);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaPlayBeforeSelectionExecute(Sender: TObject);
begin
  ProcMPV.MPVPlay(mpmBeforeSelection);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaPlayAfterSelectionExecute(Sender: TObject);
begin
  ProcMPV.MPVPlay(mpmAfterSelection);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaAddEntryExecute(Sender: TObject);
var
  i: Integer;
begin
  if WAVE.IsTimeLineEnabled then
  begin
    if not WAVE.SelectionIsEmpty and WAVE.IsOnlySelection then
      WAVE.SelectSubtitle(InsertSubtitle(Subtitles.FindInsertPos(WAVE.Selection), WAVE.Selection), True, False)
    else if WAVE.SelectionIsEmpty then
    begin
      i := Subtitles.FindInsertPos(WAVE.CursorPosMS, WAVE.CursorPosMS);
      i := InsertSubtitle(i, WAVE.CursorPosMS, CalcNewSubtitleFinalTime(i-1, WAVE.CursorPosMS), '', '');
      WAVE.SelectSubtitle(i, True, False);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaDeleteEntryExecute(Sender: TObject);
begin
  if WAVE.IsTimeLineEnabled and not WAVE.IsOnlySelection and (WAVE.SelectedItem <> NIL)then
  begin
    if AppOptions.AskForDeleteLines then
    begin
      if (MsgDeleteFiles = mrYes) then
        DeleteSubtitle(Subtitles.IndexOf(WAVE.SelectedItem));
    end
    else
      DeleteSubtitle(Subtitles.IndexOf(WAVE.SelectedItem));
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaSplitEntryAtCursorPositionExecute(Sender: TObject);
var
  Idx, t1, t2 : Integer;
  Item : TUWSubtitleItem;
begin
  Idx := WAVE.GetSubtitleIdxAtCursorPos;
  if (Idx <> -1) then
  begin
    ClearSubtitleItem(Item);
    Item.InitialTime := Subtitles[Idx].InitialTime;
    Item.FinalTime   := Subtitles[Idx].FinalTime;
    Item.Text        := Subtitles[Idx].Text;
    Item.Translation := Subtitles[Idx].Translation;

    t1 := WAVE.CursorPosMS - GetCorrectTime(AppOptions.Conventions.MinPause, AppOptions.Conventions.PauseInFrames);
    t2 := WAVE.CursorPosMS;

    // Make sure there is enough space for the blank time
    if (t1 < Subtitles[Idx].InitialTime) or (t2 > Subtitles[Idx].FinalTime) then
    begin
      // no blank time
      t1 := WAVE.CursorPosMS - 1;
      t2 := WAVE.CursorPosMS;
    end;
    // Make sur there is no overlapping
    if (t1 = t2) then
      t1 := t1 - 1;

    SetSubtitleTime(idx, t1, TAG_CONTROL_FINALTIME, False, False);
    Item.InitialTime := t2;
    InsertSubtitle(idx + 1, Item);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaZoomInExecute(Sender: TObject);
begin
  WAVE.ZoomIn;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaZoomOutExecute(Sender: TObject);
begin
  WAVE.ZoomOut;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaZoomSelectionExecute(Sender: TObject);
begin
  WAVE.ZoomSelection;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actGoToNextEntryAndPlayExecute(Sender: TObject);
begin
  GoToNextEntryAndPlay;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actGoToPreviousEntryAndPlayExecute(Sender: TObject);
begin
  GoToNextEntryAndPlay(False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actGoToCurrentEntryInitialTimeExecute(Sender: TObject);
begin
  GoToCurrentEntryTime;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actGoToCurrentEntryFinalTimeExecute(Sender: TObject);
begin
  GoToCurrentEntryTime(False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actPlayCurrentEntryExecute(Sender: TObject);
begin
  PlayCurrentEntry;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCombineEntriesExecute(Sender: TObject);
begin
  VSTCombineEntries(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAutoBreakEntryExecute(Sender: TObject);
var
  Memo: TUWMemo;
begin
  Memo := GetMemoFocused;
  if Memo = NIL then
    VSTDoLoop(VST, @ApplyAutoBreakSubtitles, dlSelected, True, True)
  else
  begin
    Memo.Text := AutoBreakSubtitle(Memo.Text, AppOptions.Conventions.CPL);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSelectAllExecute(Sender: TObject);
var
  Memo: TUWMemo;
begin
  if Workspace.ViewMode = vmList then
  begin
    if VST.Focused then
    begin
      VST.SelectAll(False);
      UpdateValues;
    end
    else
    begin
      Memo := GetMemoFocused;
      if Memo <> NIL then Memo.SelectAll;
    end;
  end
  else if Workspace.ViewMode = vmSource then
    mmoSourceView.SelectAll
  else
    TranscriptionSelectAll;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actInvertSelectionExecute(Sender: TObject);
begin
  if Workspace.ViewMode <> vmTranscription then
  begin
    VST.InvertSelection(False);
    UpdateValues;
  end
  else
    TranscriptionSelectInverted;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSelectCurrentToBeginningExecute(Sender: TObject);
begin
  VSTSelectNodes(VST, 0, VSTFocusedNode(VST), False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSelectCurrentToEndExecute(Sender: TObject);
begin
  VSTSelectNodes(VST, VSTFocusedNode(VST), VST.RootNodeCount-1, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actModifySelectionExecute(Sender: TObject);
begin
  ShowModifySelection;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actLoadVideoExecute(Sender: TObject);
begin
  LoadVideoWithDialog;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCloseVideoExecute(Sender: TObject);
begin
  MPV.Close;
  WAVE.EmptyText := '';
  WAVE.Close;
  FreeThumbnails;
  actCloseVideo.Enabled  := False;
  actWaveExtract.Enabled := False;
  lblMediaTime.Caption   := '';
  lblMediaLength.Caption := '';
  sbrSeek.Position       := 0;
  mnuVideoAudioStream.Clear;
  popSilentZones.Items.Clear;

  mnuVideoPlayback.Enabled  := False;
  mnuVideoAudioStream.Enabled     := False;
  mnuVideoSubtitles.Enabled := False;

  timerSubtitle.Enabled := False;
  timerWaveform.Enabled := False;

  EnableActionsByTag([TAG_ACTION_VIDEO, TAG_ACTION_AUDIO], False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFindExecute(Sender: TObject);
begin
  ShowFindAndReplace;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFindNextExecute(Sender: TObject);
begin
  VSTFindNext;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFindPreviousExecute(Sender: TObject);
begin
  VSTFindPrevious;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actReplaceExecute(Sender: TObject);
begin
  ShowFindAndReplace(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actGoToExecute(Sender: TObject);
begin
  ShowGoTo;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMultipleReplaceExecute(Sender: TObject);
begin
  ShowMultipleReplace;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDurationLimitsExecute(Sender: TObject);
begin
  ShowDurationLimits;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAutomaticDurationExecute(Sender: TObject);
begin
  ShowAutomaticDurations;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTimeExpanderExecute(Sender: TObject);
begin
  ShowTimeExpander;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSetDelayExecute(Sender: TObject);
begin
  ShowSetDelay;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShiftTimesExecute(Sender: TObject);
begin
  ShowShiftTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShiftTimeMoreExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplyShiftTimeMore, dlSelected, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShiftTimeLessExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplyShiftTimeLess, dlSelected, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actConvertCaseExecute(Sender: TObject);
begin
  ShowConvertCase;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnbreakEntryExecute(Sender: TObject);
var
  Memo: TUWMemo;
begin
  Memo := GetMemoFocused;
  if Memo = NIL then
    VSTDoLoop(VST, @ApplyUnbreakSubtitles, dlSelected, True, True)
  else
  begin
    Memo.Text := UnbreakSubtitles(Memo.Text);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actReverseTextExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplyReverseText, dlSelected, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFixRTLPunctuationExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplyFixRTLPunctuation, dlSelected, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDivideEntryExecute(Sender: TObject);
begin
  VSTDivideEntry(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMarkEntryExecute(Sender: TObject);
begin
  VSTMarkEntries(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnMarkEntryExecute(Sender: TObject);
begin
  VSTMarkEntries(VST, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAdjustSubtitleExecute(Sender: TObject);
begin
  ShowAdjustSubtitle;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actJumpToNextMarkedExecute(Sender: TObject);
begin
  VSTJumpToNextMarkedEntry(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actJumpToPreviousMarkedExecute(Sender: TObject);
begin
  VSTJumpToNextMarkedEntry(VST, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actJumpToNextEmptyExecute(Sender: TObject);
begin
  VSTJumpToNextEmptyEntry(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actJumpToPreviousEmptyExecute(Sender: TObject);
begin
  VSTJumpToNextEmptyEntry(VST, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actJumpToNextNoteExecute(Sender: TObject);
begin
  VSTJumpToNextNoteEntry(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actJumpToPreviousNoteExecute(Sender: TObject);
begin
  VSTJumpToNextNoteEntry(VST, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actEndCueAddOneFrameExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplyEndCueAddOneFrame, dlSelected, True, True);
end;

procedure TfrmMain.actEndCueSubtractOneFrameExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplyEndCueSubtractOneFrame, dlSelected, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actRoundTimesExecute(Sender: TObject);
begin
  ShowRoundTime;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actReadTimingsFromFileExecute(Sender: TObject);
begin
  ReadTimingsFromFileWithDialog;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actReadTextsFromFileExecute(Sender: TObject);
begin
  ReadTextsFromFileWithDialog;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actExtendLengthToPreviousExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplyExtendLengthToPrevious, dlSelected, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actExtendLengthToNextExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplyExtendLengthToNext, dlSelected, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSetAutomaticDurationExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplyAutomaticDuration);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSetDefaultGapExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplyDefaultPause);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSetGappingExecute(Sender: TObject);
begin
  ShowSetPauses;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSwapTextsExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplySwapTexts, dlSelected, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowColumnNumberExecute(Sender: TObject);
begin
  VSTShowColumn(VST, 0, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowColumnTimesExecute(Sender: TObject);
begin
  VSTShowColumn(VST, 1, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowColumnDurationExecute(Sender: TObject);
begin
  VSTShowColumn(VST, 2, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowColumnCPSExecute(Sender: TObject);
begin
  VSTShowColumn(VST, 6, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowColumnWPMExecute(Sender: TObject);
begin
  VSTShowColumn(VST, 7, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowColumnCPLExecute(Sender: TObject);
begin
  VSTShowColumn(VST, 8, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowColumnStyleAndActorExecute(Sender: TObject);
begin
  VSTShowColumn(VST, 3, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actWebReferenceExecute(Sender: TObject);
var
  Memo : TUWMemo;
  s: String;
begin
  Memo := GetMemoFocused;
  if Memo = NIL then Exit;

  s := Memo.SelText;
  if s = '' then s := GetMemoWordUnderCaret(Memo);
  if s <> '' then OpenURL(Format(AppOptions.WebSearchURL, [s]));
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSplitEntryAtPositionExecute(Sender: TObject);
var
  Memo  : TUWMemo;
  idx   : Integer;
  sl    : TStringList;
  x, c,
  t1,t2 : Integer;
begin
  Memo := GetMemoFocused;
  if (Memo = NIL) and (VST.SelectedCount <> 1) then Exit;

  idx := Memo.SelStart;
  if (idx <= 0) or (idx >= Length(Memo.Text)) then Exit;

  x  := VSTFocusedNode(VST);
  sl := TStringList.Create;
  try
    with Subtitles[x] do
      SplitRegExpr('\|\|', DivideLinesAtPosition(Memo.Text, InitialTime, FinalTime, idx, GetCorrectTime(AppOptions.Conventions.MinPause, AppOptions.Conventions.PauseInFrames)), sl);

    DeleteSubtitle(x, False, False);

    while sl.Count >= 3 do
    begin
      t1 := StrToIntDef(sl[0], 0);
      t2 := StrToIntDef(sl[1], 0);
      InsertSubtitle(x, t1, t2, sl[2], '', False, False);
      for c := 1 to 3 do sl.Delete(0);
      inc(x);
    end;
  finally
    sl.Free;
  end;

  UndoInstance.IncrementUndoGroup;
  SubtitleChanged(True, True);
  UpdateValues(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAddNoteExecute(Sender: TObject);
var
  Idx: Integer;
begin
  if VST.SelectedCount = 1 then
  begin
    Idx := VSTFocusedNode(VST);
    with Subtitles.ItemPointer[Idx]^ do
    begin
      UndoInstance.AddUndo(utSubtitleChange, Idx, Subtitles[Idx]);
      Notes := InputDialog(lngAddNote, '', Notes, '', '', 135);
      SubtitleChanged(True, True);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actLoadVideoFromURLExecute(Sender: TObject);
var
  s: String;
begin
  s := InputDialog(lngOpenVideoFromURL, lngURL, '', lngSupportedSites, 'https://github.com/yt-dlp/yt-dlp/blob/master/supportedsites.md');
  if not s.IsEmpty then
  begin
    if not actVideoPreview.Checked then
      actVideoPreview.Execute;

    MPV.Play(s);
    actMediaChangePlayRateExecute(NIL);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTranslateExecute(Sender: TObject);
begin
  ShowTranslate;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actWaveExtractExecute(Sender: TObject);
begin
  ShowWaveExtractor;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSettingsExecute(Sender: TObject);
begin
  ShowSettings;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFormatPropertiesExecute(Sender: TObject);
begin
  ShowFormatProperties;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actRestoreBackupExecute(Sender: TObject);
begin
  ShowRestoreBackup;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFixSubtitlesExecute(Sender: TObject);
begin
  ShowFixSubtitles;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTranslationMemorySettingsExecute(Sender: TObject);
begin
  ShowTranslationMemorySettings;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTranslationMemoryListExecute(Sender: TObject);
begin
  ShowTranslationMemoryList;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTranslationMemoryExecute(Sender: TObject);
begin
  ShowTranslationMemory;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTranslationMemoryCloseExecute(Sender: TObject);
begin
  CloseTranslationMemory;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTM1Execute(Sender: TObject);
begin
  // Copy TM Similarity #1 to Translation
  GetTranslationMemoryAtIndex(0);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTM2Execute(Sender: TObject);
begin
  // Copy TM Similarity #2 to Translation
  GetTranslationMemoryAtIndex(1);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTM3Execute(Sender: TObject);
begin
  // Copy TM Similarity #3 to Translation
  GetTranslationMemoryAtIndex(2);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actValidateTMExecute(Sender: TObject);
begin
  // validate TM, add to tmx and go to next subtitle
  if (VST.SelectedCount = 1) and (mmoText.Text <> '') and (mmoTranslation.Text <> '') then
  begin
    Subtitles.ItemPointer[VSTFocusedNode(VST)]^.Data := TMX.AddItem(mmoText.Text, mmoTranslation.Text, '');
    actNextSubtitle.Execute;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSpellCheckExecute(Sender: TObject);
begin
  ShowSpellCheck;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actInsertCurrentTimeExecute(Sender: TObject);
begin
  if (Workspace.ViewMode = vmTranscription) and (MPV.GetMediaLenInMs > 0) and
    Assigned(Workspace.Transcription.Memo) then
  begin
    Workspace.Transcription.Memo.TextInsertAtCarets(' ('+GetTimeStr(MPV.GetMediaPosInMs)+') ', False, False, False);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actLoadTranscriptionExecute(Sender: TObject);
begin
  LoadTranscriptionWithDialog;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSaveTranscriptionExecute(Sender: TObject);
begin
  SaveTranscriptionWithDialog;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actGoToTimeExecute(Sender: TObject);
begin
  if MPV.GetMediaLenInMs > 0 then
    ShowGoToTime;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShotChangesExecute(Sender: TObject);
begin
  ShowShotChanges;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAudioToTextExecute(Sender: TObject);
begin
  ShowAudioToText;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actBatchConvertExecute(Sender: TObject);
begin
  ShowBatchConvert;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actQualityCheckExecute(Sender: TObject);
begin
  ShowQualityCheck;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCompareExecute(Sender: TObject);
begin
  ShowCompare;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actStatisticsExecute(Sender: TObject);
begin
  ShowStatistics;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actInsertShotChangeExecute(Sender: TObject);
begin
  with WAVE do
    if IsTimeLineEnabled then
      InsertSceneChange(CursorPosMS);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDeleteShotChangeExecute(Sender: TObject);
begin
  with WAVE do
    if IsTimeLineEnabled then
    begin
      if WAVE.SelectionIsEmpty then
        DeleteSceneChange(CursorPosMS, CursorPosMS)
      else
        DeleteSceneChange(Selection.InitialTime, Selection.FinalTime);
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actJumpToNextShotChangeExecute(Sender: TObject);
begin
  GoToNextShotChange;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actJumpToPrevShotChangeExecute(Sender: TObject);
begin
  GoToNextShotChange(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actViewShotChangeExecute(Sender: TObject);
begin
  WAVE.SceneChangeEnabled := actViewShotChange.Checked;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCenterWaveformExecute(Sender: TObject);
begin
  WAVE.CenterPlayCursor := actCenterWaveform.Checked;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSetAsDefaultFormatExecute(Sender: TObject);
begin
  Workspace.DefFormat := TUWSubtitleFormats(cboFormat.ItemIndex+1);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSetAsDefaultEncodingExecute(Sender: TObject);
begin
  Workspace.DefEncoding := cboEncoding.ItemIndex;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDetectSilentZonesExecute(Sender: TObject);
begin
  popSilentZones.Items.Clear;
  FillMenuWithSilentZone(popSilentZones);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDetectDialogSegmentsExecute(Sender: TObject);
begin
  DetectDialogSegments;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowThumbnailsExecute(Sender: TObject);
begin
  WAVE.DrawThumbnail := actShowThumbnails.Checked;
  WAVE.DoUpdate;

  if WAVE.DrawThumbnail and (WAVE.Thumbnails = NIL) then
    GenerateThumbnails;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTBXExecute(Sender: TObject);
begin
  ShowTBX;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTBXListExecute(Sender: TObject);
begin
  ShowTBXList;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTBXSettingsExecute(Sender: TObject);
begin
  ShowTBXSettings;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTBXEditExecute(Sender: TObject);
begin
  ShowTBXEdit;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTBXCloseExecute(Sender: TObject);
begin
  CloseTBX;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actExportMarkedSubtitlesExecute(Sender: TObject);
begin
  if Workspace.TranslatorMode then
    ExportMarkedSubtitleWithDialog(smTranslation)
  else
    ExportMarkedSubtitleWithDialog;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actExportTextOnlyExecute(Sender: TObject);
begin
  if Workspace.TranslatorMode then
    ExportTextOnlySubtitleWithDialog(smTranslation)
  else
    ExportTextOnlySubtitleWithDialog;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actImportSubtitlesExecute(Sender: TObject);
begin
  ImportSubtitleWithDialog;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaSubSizeIncExecute(Sender: TObject);
begin
  MPVOptions.TextSize += 2;
  Constrain(MPVOptions.TextSize, 4, 100);
  MPV.SetTextSize(MPVOptions.TextSize);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaSubSizeDecExecute(Sender: TObject);
begin
  MPVOptions.TextSize -= 2;
  Constrain(MPVOptions.TextSize, 4, 100);
  MPV.SetTextSize(MPVOptions.TextSize);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaVolumeUpExecute(Sender: TObject);
begin
  Inc(MPVOptions.Volume.Percent, 5);
  if MPVOptions.Volume.Percent > 100 then
    MPVOptions.Volume.Percent := 100;

  sbrVolume.Position := MPVOptions.Volume.Percent; //MPV.SetAudioVolume(MPVOptions.Volume.Percent);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaVolumeDownExecute(Sender: TObject);
begin
  if MPVOptions.Volume.Percent >= 5 then
    Dec(MPVOptions.Volume.Percent, 5);

  sbrVolume.Position := MPVOptions.Volume.Percent; //MPV.SetAudioVolume(MPVOptions.Volume.Percent);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaVolumeMuteExecute(Sender: TObject);
begin
  MPVOptions.Volume.Mute := not MPVOptions.Volume.Mute;
  UpdateMuteIcon;
  MPV.SetAudioMute(MPVOptions.Volume.Mute);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowToolbarFileExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolbarMain, ToolBarMainFile, actShowToolbarFile.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowToolbarGeneralExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolbarMain, ToolBarMainGeneral, actShowToolbarGeneral.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowToolbarEntryExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolbarMain, ToolBarMainEntry, actShowToolbarEntry.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowToolbarViewExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolbarMain, ToolBarMainView, actShowToolbarView.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowToolbarFPSExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolbarMain, ToolBarFPS, actShowToolbarFPS.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowToolbarFormatExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolbarMain, ToolBarFormat, actShowToolbarFormat.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowToolbarEncodingExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolbarMain, ToolBarEncoding, actShowToolbarEncoding.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowToolbarQuickActionExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolbarMain, edtQuickAction, actShowToolbarQuickAction.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowWaveformToolbarControlsExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolbarWaveform, ToolBarWaveformControls, actShowWaveformToolbarControls.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowWaveformToolbarEntryExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolbarWaveform, ToolBarWaveformEntry, actShowWaveformToolbarEntry.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowWaveformToolbarZoomExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolbarWaveform, ToolBarWaveformZoom, actShowWaveformToolbarZoom.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowWaveformToolbarShotchangesExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolbarWaveform, ToolBarWaveformShotchanges, actShowWaveformToolbarShotchanges.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowWaveformToolbarOtherExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolbarWaveform, ToolBarWaveformOther, actShowWaveformToolbarOther.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowEditorToolbarBasicExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolBarEditor, ToolBarEditorBasic, actShowEditorToolbarBasic.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowEditorToolbarFormatExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolBarEditor, ToolBarEditorFormat, actShowEditorToolbarFormat.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowEditorToolbarAlignmentExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolBarEditor, ToolBarEditorAlignment, actShowEditorToolbarAlignment.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowEditorToolbarEntryExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolBarEditor, ToolBarEditorEntry, actShowEditorToolbarEntry.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowEditorToolbarLinesExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolBarEditor, ToolBarEditorLines, actShowEditorToolbarLines.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowEditorToolbarWordsExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolBarEditor, ToolBarEditorWords, actShowEditorToolbarWords.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowVideoToolbarControlsExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolBarVideo, ToolBarVideoControls, actShowVideoToolbarControls.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowVideoToolbarEntryExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolBarVideo, ToolBarVideoEntry, actShowVideoToolbarEntry.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowVideoToolbarOtherExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolBarVideo, ToolBarVideoOther, actShowVideoToolbarOther.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowVideoToolbarVolumeExecute(Sender: TObject);
begin
  UpdateCoolBar(CoolBarVideo, ToolBarVideoVolume, actShowVideoToolbarVolume.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSortExecute(Sender: TObject);
begin
  VSTSort(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actExportCustomTextFormatExecute(Sender: TObject);
begin
  ShowCustomTextFormat;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actExportCustomImageFormatExecute(Sender: TObject);
begin
  ShowCustomImageFormat;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actExportSUPExecute(Sender: TObject);
begin
  ShowExportSUP;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actActorExecute(Sender: TObject);
begin
  ShowActors;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actLoadProjectExecute(Sender: TObject);
begin
  LoadProjectWithDialog;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSaveProjectExecute(Sender: TObject);
begin
  SaveProjectWithDialog;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actWebPreviewExecute(Sender: TObject);
begin
  SaveForWebPreview;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actScreenshotToFileExecute(Sender: TObject);
var
  s : String;
begin
  if MPV.IsMediaLoaded then
  begin
    s := Format('%s%s%d.png', [ScreenshotsFolder, ChangeFileExt(ExtractFileName(MPV.FileName), ''), MPV.GetMediaPosInMs]);
    MPV.ScreenshotToFile(s);
    SetStatusBarText(s);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actRemoveLineFromEntryExecute(Sender: TObject);
var
  s : String;
  numbers : TStringArray;
begin
  s := InputDialog(actRemoveLineFromEntry.Caption, lngLine, '', '', '', 93, True);
  if not s.IsEmpty then
  begin
    numbers := s.Split(',');
    try
      VSTDeleteLineFromEntry(VST, numbers);
    finally
      SetLength(numbers, 0);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actPushFirstLineToPreviousEntryExecute(Sender: TObject);
begin
  PushFirstLineToPreviousEntry(VSTFocusedNode(VST));
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actPushLastLineToNextEntryExecute(Sender: TObject);
begin
  PushLastLineToNextEntry(VSTFocusedNode(VST));
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actPullLastLineFromPreviousEntryExecute(Sender: TObject);
begin
  PullLastLineFromPreviousEntry(VSTFocusedNode(VST));
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actPullFirstLineFromNextEntryExecute(Sender: TObject);
begin
  PullFirstLineFromNextEntry(VSTFocusedNode(VST));
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actRemoveUnicodeControlCharsExecute(Sender: TObject);
var
  Memo: TUWMemo;
begin
  Memo := GetMemoFocused;
  if Memo = NIL then
    VSTDoLoop(VST, @ApplyRemoveUnicodeControlChars, dlSelected, True, True)
  else
    Memo.Text := RemoveUnicodeControlChars(Memo.Text);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actPushWordUpExecute(Sender: TObject);
begin
  PushWord(VSTFocusedNode(VST), GetMemoFocusedCaretLineY, pwmUp);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actPushWordDownExecute(Sender: TObject);
begin
  PushWord(VSTFocusedNode(VST), GetMemoFocusedCaretLineY, pwmDown);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actPushWordToPreviousEntryExecute(Sender: TObject);
begin
  PushWord(VSTFocusedNode(VST), GetMemoFocusedCaretLineY, pwmToPrevious);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actPushWordToNextEntryExecute(Sender: TObject);
begin
  PushWord(VSTFocusedNode(VST), GetMemoFocusedCaretLineY, pwmToNext);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMergeWithNextExecute(Sender: TObject);
begin
  MergeWithNext(VSTFocusedNode(VST));
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMergeWithPreviousExecute(Sender: TObject);
begin
  MergeWithNext(VSTFocusedNode(VST), True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actClearTextExecute(Sender: TObject);
var
  Memo : TUWMemo;
begin
  Memo := GetMemoFocused;
  if Memo = NIL then
    VSTDoLoop(VST, @ApplyClearText, dlSelected, True, True)
  else
    Memo.Text := '';
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actGenerateVideoExecute(Sender: TObject);
begin
  ShowGenerateVideo;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actGenerateBlankVideoExecute(Sender: TObject);
begin
  ShowGenerateBlankVideo;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnicodeCtrlLRMExecute(Sender: TObject);
begin
  InsertMemoText(UCC_LRM, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnicodeCtrlRLMExecute(Sender: TObject);
begin
  InsertMemoText(UCC_RLM, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnicodeZWJExecute(Sender: TObject);
begin
  InsertMemoText(UCC_ZWJ, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnicodeCtrlZWNJExecute(Sender: TObject);
begin
  InsertMemoText(UCC_ZWNJ, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnicodeCtrlLREExecute(Sender: TObject);
begin
  InsertMemoText(UCC_LRE, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnicodeCtrlRLEExecute(Sender: TObject);
begin
  InsertMemoText(UCC_RLE, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnicodeCtrlLROExecute(Sender: TObject);
begin
  InsertMemoText(UCC_LRO, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnicodeCtrlRLOExecute(Sender: TObject);
begin
  InsertMemoText(UCC_RLO, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnicodeCtrlPDFExecute(Sender: TObject);
begin
  InsertMemoText(UCC_PDF, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVideoFilterHFlipExecute(Sender: TObject);
begin
  MPVSetVideoFilters;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVideoFilterNegativeExecute(Sender: TObject);
begin
  MPVSetVideoFilters;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVideoFilterBlurExecute(Sender: TObject);
begin
  MPVSetVideoFilters;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVideoFilterEdgeEnhanceExecute(Sender: TObject);
begin
  MPVSetVideoFilters;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVideoFilterEmbossExecute(Sender: TObject);
begin
  MPVSetVideoFilters;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVideoFilterSharpenExecute(Sender: TObject);
begin
  MPVSetVideoFilters;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVideoFilterVFlipExecute(Sender: TObject);
begin
  MPVSetVideoFilters;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVideoFilterVintageExecute(Sender: TObject);
begin
  MPVSetVideoFilters;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVideoFilterDeinterlaceExecute(Sender: TObject);
begin
  MPVSetVideoFilters;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAudioFilterDialoguEnhanceExecute(Sender: TObject);
begin
  MPVSetAudioFilters;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAudioFilterSpeechNormalizerExecute(Sender: TObject);
begin
  MPVSetAudioFilters;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAudioFilterSurroundExecute(Sender: TObject);
begin
  MPVSetAudioFilters;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSetGUILayout0Execute(Sender: TObject);
begin
  SetWorkspaceLayout;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSetGUILayout1Execute(Sender: TObject);
begin
  SetWorkspaceLayout(1);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSetGUILayout2Execute(Sender: TObject);
begin
  SetWorkspaceLayout(2);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVARDefaultExecute(Sender: TObject);
begin
  MPVSetVideoAspectRatio(arDefault);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVAR43Execute(Sender: TObject);
begin
  MPVSetVideoAspectRatio(ar4_3);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVAR169Execute(Sender: TObject);
begin
  MPVSetVideoAspectRatio(ar16_9);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVAR1851Execute(Sender: TObject);
begin
  MPVSetVideoAspectRatio(ar185_1);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVAR2351Execute(Sender: TObject);
begin
  MPVSetVideoAspectRatio(ar235_1);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVARCycleExecute(Sender: TObject);
begin
  MPVCycleVideoAspectRatio;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actPanAndScanExecute(Sender: TObject);
begin
  MPVSetPanAndScan(not actPanAndScan.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVideoInformationExecute(Sender: TObject);
begin
  MPVShowVideoInformation;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFocusInitialTimeExecute(Sender: TObject);
begin
  SetFocusTimeEdit(TAG_CONTROL_INITIALTIME);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFocusFinalTimeExecute(Sender: TObject);
begin
  SetFocusTimeEdit(TAG_CONTROL_FINALTIME);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCharacterMapExecute(Sender: TObject);
begin
  ShowCharacterMap;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTextEffectTypewriterExecute(Sender: TObject);
begin
  VSTTextEffect(VST, fxTypewriter, 0, 0);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTextEffectFlashExecute(Sender: TObject);
begin
  VSTTextEffect(VST, fxFlash, 250, 100);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDistributeEntriesEvenlyExecute(Sender: TObject);
begin
  VSTDistributeEntriesEvenly(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actStreamExtractorExecute(Sender: TObject);
begin
  ShowStreamExtractor;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTTSExecute(Sender: TObject);
begin
  ShowTTS;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVideoDubbingExecute(Sender: TObject);
begin
  ShowVideoDubbing;
end;

// -----------------------------------------------------------------------------

