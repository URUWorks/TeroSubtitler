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
 *  Copyright (C) 2023-2026 URUWorks, uruworks@gmail.com.
 *}

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESelectedSubtitleItem(Sender: TObject;
  const Index: Integer; const SubtitleItem: TUWSubtitleItem;
  const IsDynamic: Boolean);
begin
  // clicking an entry of a multiple selection keeps it (to drag all of them)
  VSTSelectNode(VST, Index, not WAVE.KeepMultiSelection, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESubtitleItemToggle(Sender: TObject; const Index: Integer);
var
  Node: PVirtualNode;
begin
  // Ctrl+click (Cmd+click on macOS) on an entry in the timeline
  Node := VSTGetNodeAtIndex(VST, Index);
  if Node = NIL then Exit;

  if VST.Selected[Node] then
  begin
    VSTSelectingNode := True;
    try
      VST.Selected[Node] := False;
      if VST.FocusedNode = Node then
        VST.FocusedNode := VSTLastSelectedNode(VST);
    finally
      VSTSelectingNode := False;
    end;
    VSTFocusChanged(NIL, NIL, 0);
  end
  else
    VSTSelectNode(VST, Node, False, True, True);

  if VSTFocusedNode(VST) >= 0 then
    WAVE.SelectSubtitle(VSTFocusedNode(VST), True, False)
  else
    WAVE.ClearSelection;

  UpdateWAVEMultiSelection;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESubtitleItemsMoved(Sender: TObject; const Items: TMovedSubtitleItems);
var
  Item  : TUWSubtitleItem;
  Idxs  : TIntegerDynArray;
  i, c, FocusIdx : Integer;
begin
  // several entries dragged together in the timeline: one undo for all
  Idxs := NIL;
  SetLength(Idxs, Length(Items));
  c := 0;
  for i := 0 to High(Items) do
    if Subtitles.ValidIndex(Items[i].Index) then
    begin
      Item := Subtitles[Items[i].Index];
      Item.InitialTime := Items[i].OldInitialTime;
      Item.FinalTime   := Items[i].OldFinalTime;
      UndoInstance.AddUndo(utSubtitleChange, Items[i].Index, Item, False);
      Idxs[c] := Items[i].Index;
      Inc(c);
    end;
  SetLength(Idxs, c);
  UndoInstance.IncrementUndoGroup;

  // the list was sorted: select the moved entries where they are now
  FocusIdx := Subtitles.IndexOf(WAVE.SelectedItem);
  VSTSelectNodes(VST, Idxs, True);
  if FocusIdx >= 0 then
    VSTSelectNode(VST, FocusIdx, False, True, True);

  DoAutoCheckErrors;
  UpdateValues(True);
  SubtitleChanged(True, Workspace.TranslatorMode);
  UpdateWAVEMultiSelection;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESubtitleItemRangeSelect(Sender: TObject; const Index: Integer);
var
  Anchor: Integer;
begin
  // Shift+click on an entry in the timeline: select the entries from the
  // focused one to it, as Shift+click in the list. The focused entry stays
  // (the anchor), so another Shift+click changes the range from it
  if not Subtitles.ValidIndex(Index) then Exit;

  Anchor := VSTFocusedNode(VST);
  if not Subtitles.ValidIndex(Anchor) then Anchor := Index;

  if Anchor <= Index then
    VSTSelectNodes(VST, Anchor, Index, True)
  else
    VSTSelectNodes(VST, Index, Anchor, True);
  VSTSelectNode(VST, Anchor, False, True, True);

  WAVE.SelectSubtitle(Anchor, True, False);
  UpdateWAVEMultiSelection;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.UpdateWAVEMultiSelection;
var
  Node  : PVirtualNode;
  Items : array of PUWSubtitleItem;
  c     : Integer;
begin
  Items := NIL;
  c := 0;
  if VST.SelectedCount > 1 then
  begin
    SetLength(Items, VST.SelectedCount);
    Node := VST.GetFirstSelected;
    while Assigned(Node) and (c <= High(Items)) do
    begin
      Items[c] := Subtitles.ItemPointer[Node^.Index];
      if Items[c] <> NIL then Inc(c);
      Node := VST.GetNextSelected(Node);
    end;
  end;
  SetLength(Items, c);
  WAVE.SetMultiSelection(Items);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  UpdateWAVEMultiSelection;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESelectedSubtitleItemChange(Sender: TObject);
begin
  // while dragging an entry (or one of its edges) in the timeline the times,
  // the CPS and the entry in the list are updated while it moves, not only
  // when the mouse button is released. Not on every mouse move (it made the
  // drag lag), at most every 80 ms; the last values are always shown.
  if not WAVE.IsMouseDown then
  begin
    UpdateWAVEDraggedValues;
    Exit;
  end;

  if FWAVEValuesTimer = NIL then
  begin
    FWAVEValuesTimer := TTimer.Create(Self);
    FWAVEValuesTimer.Enabled  := False;
    FWAVEValuesTimer.Interval := 80;
    FWAVEValuesTimer.OnTimer  := @WAVEValuesTimerTimer;
  end;
  if not FWAVEValuesTimer.Enabled then
    FWAVEValuesTimer.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVEValuesTimerTimer(Sender: TObject);
begin
  FWAVEValuesTimer.Enabled := False;
  UpdateWAVEDraggedValues;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.UpdateWAVEDraggedValues;
var
  Idx: Integer;
begin
  Idx := VSTFocusedNode(VST);
  if not Subtitles.ValidIndex(Idx) then Exit;

  // only the times are read here (nothing is calculated) and the CPS is a
  // division with the length of the text, calculated once for the entry
  UpdateValuesForSubtitle(Idx);
  if VST.SelectedCount = 1 then
    UpdateDraggedCPS(Idx)
  else
    UpdateCPSAndTexts(Idx);
  if Assigned(VST.FocusedNode) then
    VST.InvalidateNode(VST.FocusedNode);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.UpdateDraggedCPS(const AIdx: Integer);

  // same as TUWSubtitles.GetStringCPS, without the duration
  function CPSLength(S: String): Integer;
  var
    x: Integer;
    Skip: String;
  begin
    Skip := AppOptions.Conventions.CPSLineLenStrategy;
    for x := 1 to Skip.Length do
      S := ReplaceString(S, Skip[x], '');
    Result := UTF8Length(ReplaceString(RemoveTSTags(S), sLineBreak, ' '));
  end;

var
  Item: PUWSubtitleItem;
  Dur: Double;
  NewCPS: Double;
begin
  Item := Subtitles.ItemPointer[AIdx];
  if Item = NIL then Exit;

  // (string compare: same string = same pointer, fast)
  if (AIdx <> FDragIdx) or (Item^.Text <> FDragText) or (Item^.Translation <> FDragTrans) then
  begin
    FDragIdx         := AIdx;
    FDragText        := Item^.Text;
    FDragTrans       := Item^.Translation;
    FDragTextLen     := CPSLength(FDragText);
    FDragTransLen    := CPSLength(FDragTrans);
    FDragTextLines   := GetLengthForEachLine(FDragText, '/', '=');
    FDragTransLines  := GetLengthForEachLine(FDragTrans, '/', '=');
  end;

  Dur := 0;
  if (Item^.InitialTime >= 0) and (Item^.FinalTime > 0) then
    Dur := (Item^.FinalTime - Item^.InitialTime) / 1000;

  if Dur > 0 then NewCPS := FDragTextLen / Dur else NewCPS := 0;
  mmoText.LabelMemo.Caption := Format(lngasTextChars, [FDragTextLines, NewCPS], FormatSettings);
  mmoText.CPSBar.SetCPS(NewCPS);

  if Workspace.TranslatorMode then
  begin
    if Dur > 0 then NewCPS := FDragTransLen / Dur else NewCPS := 0;
    mmoTranslation.LabelMemo.Caption := Format(lngasTranslationChars, [FDragTransLines, NewCPS], FormatSettings);
    mmoTranslation.CPSBar.SetCPS(NewCPS);
  end;
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
  DoAutoCheckErrors;
  UpdateValues(True);
  SubtitleChanged(True, Workspace.TranslatorMode);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESelectionChange(Sender: TObject);
var
  it, ft, rit, rft, fr: Integer;
  AFPS: Double;
begin
  DoWAVEPopup(NIL);

  //actMediaAddEntry.Enabled   := not WAVE.SelectionIsEmpty and WAVE.IsOnlySelection;
  actMediaZoomSelection.Enabled := actMediaAddEntry.Enabled;

  // a selection (not an entry) in the timeline: its start/end, duration and
  // frames in the status bar, to know if it is long enough for a new entry
  if not WAVE.SelectionIsEmpty and WAVE.IsOnlySelection then
  begin
    it := WAVE.Selection.InitialTime;
    ft := WAVE.Selection.FinalTime;
    AFPS := Workspace.FPS.OutputFPS.FPS;
    if Workspace.WorkMode = wmFrames then
    begin
      RoundFramesValue(it, ft, AFPS, rit, rft);
      it := rit;
      ft := rft;
    end;
    if AFPS > 0 then
      fr := Round((ft - it) * AFPS / 1000)
    else
      fr := 0;

    SetStatusBarText(Format(lngasTimelineSelection, [GetTimeStr(it), GetTimeStr(ft),
      GetTimeStr(ft - it, False, tcNDF), fr], FormatSettings), 0, False);
    FWAVESelInStatus := True;
  end
  else if FWAVESelInStatus then
  begin
    FWAVESelInStatus := False;
    SetStatusBarText('', 0, False);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVEClick(Sender: TObject);
const
  // without video: at least one minute, and the timeline grows with the
  // entries (the last one + 5 s, see UpdateLengthMS of the timeline)
  NoMediaLengthMS = 60000;
begin
  if WAVE.IsTimeLineEnabled then Exit;

  if (MPV.GetMediaLenInMs > 0) then
  begin
    FTimelineWithoutMedia := False;
    WAVE.GenerateDummyPeakTab(MPV.GetMediaLenInMs);
    EnableActionsByTag([TAG_ACTION_AUDIO], True);
  end
  else
  begin
    // before, without video the click did nothing and the entries couldn't
    // be seen or edited in the timeline
    FTimelineWithoutMedia := True;
    WAVE.GenerateDummyPeakTab(NoMediaLengthMS);
    EnableActionsByTag([TAG_ACTION_AUDIO], True);
    // nothing to play or analyze without media
    actMediaPlaySelection.Enabled       := False;
    actMediaPlayFromSelection.Enabled   := False;
    actMediaPlayBeforeSelection.Enabled := False;
    actMediaPlayAfterSelection.Enabled  := False;
    actDetectSilentZones.Enabled        := False;
    actDetectDialogSegments.Enabled     := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVETimeLineClick(Sender: TObject; const Time: Integer);
begin
  if MPV.Initialized then
    MPV.SeekInMs(EntryToVideoMS(Time));
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
          MPV.SetMediaPosInMs(EntryToVideoMS(x));
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
begin
  if not WAVE.IsPeakDataLoaded then
    actWaveExtract.Execute;

  if WAVE.IsPeakDataLoaded and (CustomQuestionDialog(lngDetectDialogSegmentsWarning, lngContinueAnyway, [dbYes, dbNo]) = mrYes) then
    if WAVE.DetectZoneList(DialogList, False, AppOptions.DialogSegmentThreshold) then
    try
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

// one thumbnail for each scene change, generated in the background; the ones
// already loaded are reused, so only the new scene changes cost a ffmpeg call
procedure TfrmMain.GenerateThumbnails;
var
  sc, NewTimes : TIntegerDynArray;
  NewBitmaps   : TThumbnails;
  i, j, c      : Integer;
  Key          : String;
begin
  if not WAVE.DrawThumbnail or not MPV.IsMediaLoaded then Exit;

  // the loaded thumbnails are reused by time only for the same video:
  // opening another video (without closing the first) mixed them. The
  // Auto-crop of the player doesn't change them (always the full frame).
  Key := MPV.FileName;
  if Key <> FThumbnailsKey then
    FreeThumbnails;
  FThumbnailsKey := Key;

  sc := WAVE.GetSceneChangeList;
  if Length(sc) = 0 then
  begin
    FreeThumbnails;
    Exit;
  end;

  if not FileExists(GetExtractAppFile) then
  begin
    ShowErrorMessageDialog(Format(lngExtractAppError, [FFMPEG_EXE]));
    Exit;
  end;

  // a thread already running was started for the old list
  if Assigned(FThumbnailThread) then
  begin
    FThumbnailThread.OnTerminate := NIL;
    FThumbnailThread.Terminate;
    FThumbnailThread := NIL;
  end;

  // the scene change list is sorted, but it may have repeated times
  SetLength(NewTimes, Length(sc));
  c := 0;
  for i := 0 to High(sc) do
    if (c = 0) or (NewTimes[c-1] <> sc[i]) then
    begin
      NewTimes[c] := sc[i];
      Inc(c);
    end;
  SetLength(NewTimes, c);
  SetLength(NewBitmaps, c);

  // keep the thumbnails of the scene changes that are still there
  with WAVEOptions do
  begin
    for i := 0 to High(NewTimes) do
    begin
      j := BinarySearch_IntArray(ThumbnailTimes, NewTimes[i]);
      if (j >= 0) and (j <= High(ThumbnailTimes)) and (ThumbnailTimes[j] = NewTimes[i]) then
      begin
        NewBitmaps[i] := Thumbnails[j];
        Thumbnails[j] := NIL;
      end
      else
        NewBitmaps[i] := TBitmap.Create;
    end;

    for i := 0 to High(Thumbnails) do
      Thumbnails[i].Free;

    Thumbnails     := NewBitmaps;
    ThumbnailTimes := NewTimes;
  end;

  WAVE.SetThumbnails(WAVEOptions.ThumbnailTimes, WAVEOptions.Thumbnails);

  FThumbnailThread := TGenerateThumbnails.Create(MPV.FileName,
    WAVEOptions.ThumbnailTimes, Workspace.FPS.OutputFPS.FPS, @DoThumbnailReady,
    @DoThumbnailsDone, WAVE.OffsetMs);
  FThumbnailThread.Start;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.FreeThumbnails;
var
  i: Integer;
begin
  if Assigned(FThumbnailThread) then
  begin
    FThumbnailThread.OnTerminate := NIL;
    FThumbnailThread.Terminate;
    FThumbnailThread := NIL;
  end;

  WAVE.SetThumbnails(NIL, NIL);
  FThumbnailsKey := '';

  with WAVEOptions do
  begin
    for i := 0 to High(Thumbnails) do
      Thumbnails[i].Free;

    SetLength(Thumbnails, 0);
    SetLength(ThumbnailTimes, 0);
  end;
end;

// -----------------------------------------------------------------------------

// a thumbnail is ready (in the main thread): load it and redraw only its shot
procedure TfrmMain.DoThumbnailReady(Sender: TObject; const ATimeMS: Integer; const AFileName: String);
var
  i: Integer;
begin
  // a thread already replaced (another video, the scene changes edited...)
  // can still deliver the thumbnail it was making when it was cancelled: it's
  // of another list/video, don't load it
  if (Sender = NIL) or (Sender <> FThumbnailThread) then Exit;

  with WAVEOptions do
  begin
    i := BinarySearch_IntArray(ThumbnailTimes, ATimeMS);
    // the scene change may not be there anymore
    if (i < 0) or (i > High(ThumbnailTimes)) or (ThumbnailTimes[i] <> ATimeMS) or
      (i > High(Thumbnails)) or (Thumbnails[i] = NIL) then Exit;

    try
      Thumbnails[i].LoadFromFile(AFileName);
    except
      // a damaged file: leave the default thumbnail (an empty bitmap) and
      // delete it from the cache, so it's generated again the next time
      try
        Thumbnails[i].Clear;
      except
      end;
      SysUtils.DeleteFile(AFileName);
    end;
    WAVE.ThumbnailChanged(i);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DoThumbnailsDone(Sender: TObject);
begin
  if Sender = FThumbnailThread then
    FThumbnailThread := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVEDynamicEditTime(Sender: TObject; const Time: Integer);
var
  t: Integer;
begin
  // resizing an entry (dragging one of its edges) in the timeline: show that frame
  // in the video. Only when paused, not to interrupt the playback. mpv drops
  // the pending seeks when a new one arrives, so there is no need to throttle.
  if not MPVOptions.PreviewFrameOnDrag or not MPV.IsMediaLoaded or not MPV.IsPaused then Exit;

  t := Time - Workspace.VideoOffset;
  if t < 0 then t := 0;
  if t <> MPV.GetMediaPosInMs then
    MPVSeekTo(t);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVEKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Idx: Integer;
begin
  // the timeline takes the focus when clicked: arrows without modifiers
  // (the ones with modifiers are left to the shortcuts)
  if (Shift <> []) or not WAVE.IsTimeLineEnabled then Exit;

  case Key of
    VK_LEFT, VK_RIGHT:
      if MPV.IsMediaLoaded then
      begin
        // previous/next frame, like the mouse wheel on the video
        if Key = VK_LEFT then
          MPV.PreviousFrame(MPVOptions.FrameStep)
        else
          MPV.NextFrame(MPVOptions.FrameStep);
        Key := 0;
      end;
    VK_UP, VK_DOWN:
      if VST.RootNodeCount > 0 then
      begin
        // previous/next entry
        Idx := VSTFocusedNode(VST);
        if Idx < 0 then
          Idx := 0
        else if (Key = VK_UP) and (Idx > 0) then
          Dec(Idx)
        else if (Key = VK_DOWN) and (Idx < Subtitles.Count - 1) then
          Inc(Idx);

        VSTSelectNode(VST, Idx, True, True, True);
        WAVE.SelectSubtitle(Idx, True, not WAVE.IsTimeInView(Subtitles[Idx].InitialTime));
        UpdateWAVEMultiSelection;
        Key := 0;
      end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESceneChangeListChange(Sender: TObject);
begin
  // scene changes imported/detected/added/deleted: only the new ones are
  // generated, the rest are reused
  if WAVE.DrawThumbnail and MPV.IsMediaLoaded then
    GenerateThumbnails
  else if WAVE.GetSceneChangeCount = 0 then
    FreeThumbnails;
end;

// -----------------------------------------------------------------------------

