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
  CloseSubtitle(True);
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

procedure TfrmMain.actInsertSubtitleBeforeExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
    VSTInsertSubtitles(VST, True)
  else if Workspace.ViewMode = vmSource then
    mmoSourceView.Lines.Insert(mmoSourceView.CaretPos.Y, '')
  else
    TranscriptionInsertAbove;
end;

procedure TfrmMain.actInsertSubtitleExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
    VSTInsertSubtitles(VST)
  else if Workspace.ViewMode = vmSource then
    mmoSourceView.Lines.Insert(mmoSourceView.CaretPos.Y+1, '')
  else
    TranscriptionInsertBelow;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDeleteSubtitleExecute(Sender: TObject);
begin
  if Workspace.ViewMode = vmList then
  begin
    if AppOptions.AskForDeleteLines then
    begin
      if (MsgDeleteFiles = mrYes) then
        VSTDeleteSubtitles(VST);
    end
    else
      VSTDeleteSubtitles(VST);
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
      SetTextTag(swt_Bold);
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
      SetTextTag(swt_Italic);
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
      SetTextTag(swt_Underline);
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
      SetTextTag(swt_Strikeout);
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
      VSTDoLoop(VST, @ApplyFontColor)
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
  SetAlignTo(1);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAlignToCenterExecute(Sender: TObject);
begin
  SetAlignTo(2);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAlignToRightExecute(Sender: TObject);
begin
  SetAlignTo(3);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAlignToNoneExecute(Sender: TObject);
begin
  SetAlignTo(0);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVAlignToBottomExecute(Sender: TObject);
begin
  SetVAlignTo(0);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVAlignToMiddleExecute(Sender: TObject);
begin
  SetVAlignTo(1);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVAlignToTopExecute(Sender: TObject);
begin
  SetVAlignTo(2);
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

procedure TfrmMain.actMediaMoveSubtitleExecute(Sender: TObject);
begin
  if (MPV.GetMediaLenInMs > 0) and actVideoPreview.Checked and (VST.SelectedCount > 0) then
    VSTMoveSubtitle(VST, True, True);
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

procedure TfrmMain.actMediaStartSubtitleExecute(Sender: TObject);
begin
  if not (MPV.GetMediaLenInMs > 0) then Exit;
  with SubtitleInfo.LastSubtitle do
  begin
    InitialTime := MPV.GetMediaPosInMs;
    SetStatusBarText(Format(GetCommonString('SubtitleStartedAt'), [GetTimeStr(InitialTime, True)]));
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaEndSubtitleExecute(Sender: TObject);
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
    SetStatusBarText(Format(GetCommonString('SubtitleEndedAt'), [GetTimeStr(l, True)]));
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

procedure TfrmMain.actMediaAddSubtitleExecute(Sender: TObject);
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

procedure TfrmMain.actMediaDeleteSubtitleExecute(Sender: TObject);
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

procedure TfrmMain.actCombineSubtitlesExecute(Sender: TObject);
begin
  VSTCombineSubtitles(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAutoBreakSubtitleExecute(Sender: TObject);
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
  actCloseVideo.Enabled  := False;
  actWaveExtract.Enabled := False;
  lblMediaTime.Caption   := '';
  lblMediaLength.Caption := '';
  sbrSeek.Position       := 0;
  mnuVideoAudio.Clear;
  popSilentZones.Items.Clear;

  mnuVideoPlayback.Enabled  := False;
  mnuVideoAudio.Enabled     := False;
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
  VSTDoLoop(frmMain.VST, @ApplyShiftTimeMore, dlSelected, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShiftTimeLessExecute(Sender: TObject);
begin
  VSTDoLoop(frmMain.VST, @ApplyShiftTimeLess, dlSelected, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actConvertCaseExecute(Sender: TObject);
begin
  ShowConvertCase;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnbreakSubtitleExecute(Sender: TObject);
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

procedure TfrmMain.actSetMaximumLineLengthExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplySetMaximumLineLength, dlSelected, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actReverseTextExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplyReverseText);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFixRTLPunctuationExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplyFixRTLPunctuation);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDivideSubtitleExecute(Sender: TObject);
begin
  VSTDivideSubtitles(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMarkSubtitleExecute(Sender: TObject);
begin
  VSTMarkSubtitles(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnMarkSubtitleExecute(Sender: TObject);
begin
  VSTMarkSubtitles(VST, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAdjustSubtitleExecute(Sender: TObject);
begin
  ShowAdjustSubtitle;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actJumpToNextMarkedExecute(Sender: TObject);
begin
  VSTJumpToNextMarkedSubtitle(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actJumpToPreviousMarkedExecute(Sender: TObject);
begin
  VSTJumpToNextMarkedSubtitle(VST, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actEndCueAddOneFrameExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplyEndCueAddOneFrame);
end;

procedure TfrmMain.actEndCueSubtractOneFrameExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplyEndCueSubtractOneFrame);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actRoundTimesExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplyRoundTimes);
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
  VSTDoLoop(VST, @ApplyExtendLengthToPrevious);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actExtendLengthToNextExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplyExtendLengthToNext);
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

procedure TfrmMain.actSwapTextsExecute(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplySwapTexts);
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

procedure TfrmMain.actSplitSubtitleAtPositionExecute(Sender: TObject);
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
      SplitRegExpr('\|\|', DivideLinesAtPosition(Memo.Text, InitialTime, FinalTime, idx), sl);

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
begin
  if VST.SelectedCount = 1 then
    with Subtitles.ItemPointer[VSTFocusedNode(VST)]^ do
      Notes := InputDialog(GetCommonString('AddNote'), '', Notes, '', '', 135);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actLoadVideoFromURLExecute(Sender: TObject);
var
  s: String;
begin
  s := InputDialog(GetCommonString('OpenVideoFromURL'), GetCommonString('URL'), '', GetCommonString('SupportedSites'), 'https://github.com/yt-dlp/yt-dlp/blob/master/supportedsites.md');
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

procedure TfrmMain.actSwapWorkspaceExecute(Sender: TObject);
begin
  SwapWorkspaceLayout;
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
  MPV.SetAudioVolume(MPVOptions.Volume.Percent);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaVolumeDownExecute(Sender: TObject);
begin
  if MPVOptions.Volume.Percent >= 5 then
    Dec(MPVOptions.Volume.Percent, 5);
  MPV.SetAudioVolume(MPVOptions.Volume.Percent);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaVolumeMuteExecute(Sender: TObject);
begin
  MPVOptions.Volume.Mute := not MPVOptions.Volume.Mute;
  MPV.SetAudioMute(MPVOptions.Volume.Mute);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowToolbarMainExecute(Sender: TObject);
begin
  UpdateCoolBar(0, actShowToolbarMain.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowToolbarFPSExecute(Sender: TObject);
begin
  UpdateCoolBar(1, actShowToolbarFPS.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowToolbarFormatExecute(Sender: TObject);
begin
  UpdateCoolBar(2, actShowToolbarFormat.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShowToolbarEncodingExecute(Sender: TObject);
begin
  UpdateCoolBar(3, actShowToolbarEncoding.Checked);
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

