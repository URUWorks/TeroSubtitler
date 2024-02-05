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

procedure TfrmMain.MPVClick(Sender: TObject);
begin
  actMediaPlay.Execute;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVStartFile(Sender: TObject);
begin
  SetStatusBarText(lngasReadingVideoFile);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVFileLoaded(Sender: TObject);
var
  MRUInfoObject: TMRUInfoObject;
begin
  SetStatusBarText('');
  EnableActionsByTag([TAG_ACTION_VIDEO], True);
  MPVSetFilters;

  SubtitleInfo.LastSubtitle.ShowIndex := 0;
  sbrSeek.Max := MPV.GetMediaLenInMs;
  sbrSeek.Enabled := True;

  if Assigned(frmVideo) then
    frmVideo.Caption := ExtractFileName(MPV.FileName);

  actCloseVideo.Enabled       := True;
  mnuVideoPlayback.Enabled    := True;
  mnuVideoAudioStream.Enabled := True;
  mnuVideoSubtitles.Enabled   := True;

  MRUInfoObject := MRU.GetValues(SubtitleInfo.Text.FileName);

  if (MPVOptions.AutoStartPlaying and MPVOptions.UnDockData.FileName.IsEmpty) or
     (not MPVOptions.UnDockData.FileName.IsEmpty and not MPVOptions.UnDockData.Paused) then
  begin
    if not MPV.IsPlaying then
      actMediaPlay.Execute;
  end;
  //else if not MPVOptions.UnDockData.FileName.IsEmpty then
  PrepareUnDockData(True);

  if (not WAVE.IsPeakDataLoaded and Assigned(MRUInfoObject) and not MRUInfoObject.WaveformFile.IsEmpty) or (not MPVOptions.UnDockData.WAVELoaded) then
    LoadAudio(GetMediaFileNameIfExists(MPV.FileName, TWaveformAudioExts));

  if not WAVE.IsPeakDataLoaded then
    WAVE.EmptyText := lngasWaveformText;

  timerSubtitle.Enabled := True;
  timerWaveform.Enabled := True;

  if MPVOptions.SubtitleHandleByMPV then
  begin
    if MPVSaveSubtitleTempTrack then
      MPVLoadSubtitleTempTrack;
  end;

  if Assigned(MRUInfoObject) then
  begin
    if MPVOptions.UnDockData.FileName.IsEmpty then
      WAVE.SetPlayCursorMS(MRUInfoObject.WAVEPosition);
  end;

  UpdateVideoLengthString;
  RefreshAppTitle;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVPause(Sender: TObject);
begin
  actMediaPlay.ImageIndex := 38;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVPlay(Sender: TObject);
begin
  actMediaPlay.ImageIndex := 39;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVStop(Sender: TObject);
begin
  MPVPause(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVAudioReconfig(Sender: TObject);
begin
  FillMenuWithAudioStreams(mnuVideoAudioStream);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVTimeChanged(ASender: TObject; AParam: Integer);
begin
  MPVDoTimeChanged(AParam);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVDoTimeChanged(const Time: Integer);
begin
  if not sbrSeek.MouseIsDown then
  begin
    sbrSeek.Position := Time;
    if lblMediaTime.Tag = TAG_CONTROL_UPDATE then
      lblMediaTime.Tag := TAG_CONTROL_NORMAL
    else
      lblMediaTime.Caption := GetTimeStr(Time);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
  if ssCtrl in Shift then
    frmMain.actMediaVolumeDown.Execute
  else
    MPV.PreviousFrame(MPVOptions.FrameStep);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
  if ssCtrl in Shift then
    frmMain.actMediaVolumeUp.Execute
  else
    MPV.NextFrame(MPVOptions.FrameStep);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DoSubtitleTimer(Sender: TObject);
var
  mpvPos: Integer;
begin
  TimerSubtitle.Enabled := False;
  mpvPos := MPV.GetMediaPosInMs;

  if MPVOptions.SubtitleHandleByMPV then
  begin
    if actMediaAutoScroll.Checked then
      GetSubtitleIndexAtTime(mpvPos);
  end
  else
  begin
    //MPV.ShowOverlayText(GetSubtitleTextAtTime(MPV.GetMediaPosInMs));
    MPV.ShowText(GetSubtitleTextAtTime(mpvPos), 3600000, '');
  end;

  if SubtitleInfo.LastSubtitle.Selected <> SubtitleInfo.LastSubtitle.ShowIndex then
  begin
    SubtitleInfo.LastSubtitle.Selected := SubtitleInfo.LastSubtitle.ShowIndex;
    if actMediaAutoScroll.Checked then
      VSTSelectNode(VST, SubtitleInfo.LastSubtitle.Selected, True, True);
  end;

  if (MPVOptions.EndTime > 0) and (mpvPos >= MPVOptions.EndTime) then
  begin
    MPV.Pause;
    MPV.SetMediaPosInMs(MPVOptions.EndTime);
    MPVOptions.EndTime := 0;
  end;

  TimerSubtitle.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.sbrSeekMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MPV.ShowOverlayText('');
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.sbrSeekMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MPVSeekTo(sbrSeek.Position);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.sbrSeekMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  with sbrSeek do
    if MouseIsDown then
      lblMediaTime.Caption := GetTimeStr(Range((X * Max) div Width, 0, Max))
    else
      //MPV.ShowText(GetTimeStr((X * Max) div Width));
      MPV.ShowOverlayText(GetTimeStr((X * Max) div Width));
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.sbrSeekMouseLeave(Sender: TObject);
begin
  //MPV.ShowText('');
  MPV.ShowOverlayText('');
end;

// -----------------------------------------------------------------------------

