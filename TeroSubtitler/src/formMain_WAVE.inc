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
  DoAutoCheckErrors;
  UpdateValues(True);
  SubtitleChanged(True, Workspace.TranslatorMode);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESelectionChange(Sender: TObject);
begin
  DoWAVEPopup(NIL);

  //actMediaAddEntry.Enabled   := not WAVE.SelectionIsEmpty and WAVE.IsOnlySelection;
  actMediaZoomSelection.Enabled := actMediaAddEntry.Enabled;
{  if WAVE.SelectionIsEmpty then
    stbStatus.Panels[1].Text := ''
  else
    stbStatus.Panels[1].Text := Format(Strings.Selection,
      [TimeToString(WAVE.Selection.InitialTime, DefTimeFormat, FPS.FPS, True),
      TimeToString(WAVE.Selection.FinalTime, DefTimeFormat, FPS.FPS, True)]);}
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
  R            : TRect;
begin
  if not WAVE.DrawThumbnail or not MPV.IsMediaLoaded then Exit;

  // the loaded thumbnails are reused by time only for the same video and
  // crop: opening another video (without closing the first) mixed them
  R   := MPV.GetVideoCrop;
  Key := MPV.FileName + Format('|%d,%d,%d,%d', [R.Left, R.Top, R.Right, R.Bottom]);
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
    @DoThumbnailsDone);
  TGenerateThumbnails(FThumbnailThread).Crop := MPV.GetVideoCrop;
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
procedure TfrmMain.DoThumbnailReady(const ATimeMS: Integer; const AFileName: String);
var
  i: Integer;
begin
  with WAVEOptions do
  begin
    i := BinarySearch_IntArray(ThumbnailTimes, ATimeMS);
    // the scene change may not be there anymore
    if (i < 0) or (i > High(ThumbnailTimes)) or (ThumbnailTimes[i] <> ATimeMS) or
      (i > High(Thumbnails)) or (Thumbnails[i] = NIL) then Exit;

    try
      Thumbnails[i].LoadFromFile(AFileName);
      WAVE.ThumbnailChanged(i);
    except
    end;
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

