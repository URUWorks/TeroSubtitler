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

procedure TfrmMain.MPVClick(Sender: TObject);
begin
  actMediaPlay.Execute;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVEndFile(ASender: TObject; AReason, AError: Integer);
begin
  StopURLLoadTimer;
  MPVUpdateMediaTitle(True);
  MPVOptions.EndTime := 0;
  SetStatusBarText('');

  // This runs inside the mpv event thread Synchronize: a modal dialog here
  // blocks that thread, and closing the media meanwhile (it waits for the
  // thread) freezes the app. Show it after the event returns.
  if AReason = MPV_END_FILE_REASON_ERROR then
    Application.QueueAsyncCall(@DoMPVLoadError, AError);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DoAutoExtractWaveform(Data: PtrInt);
begin
  if WAVE.IsPeakDataLoaded or MPV.FileName.IsEmpty or
    not FileExists(GetExtractAppFile) then Exit;

  ShowWaveExtractor(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DoMPVLoadError(Data: PtrInt);
begin
  case Data of
    // Generic error on loading.
    //MPV_ERROR_LOADING_FAILED: ShowErrorMessageDialog(lngUnableToLoad);
    // Initializing the audio output failed.
    MPV_ERROR_AO_INIT_FAILED: ShowErrorMessageDialog(lngVideoErrorAOInitFailed);
    // Initializing the video output failed.
    MPV_ERROR_VO_INIT_FAILED: ShowErrorMessageDialog(lngVideoErrorVOInitFailed);
    // There was no audio or video data to play.
    MPV_ERROR_NOTHING_TO_PLAY: ShowErrorMessageDialog(lngVideoErrorNothingToPlay);
    // The file format could not be determined, or the file was too broken to open it.
    //MPV_ERROR_UNKNOWN_FORMAT: ShowErrorMessageDialog(lngUnableToLoad);
  else
    ShowErrorMessageDialog(lngUnableToLoad);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVStartFile(Sender: TObject);
begin
  MPVOptions.EndTime := 0;
  MPVUpdateMediaTitle(True);

  if MPV.FileName.StartsWith('http') then
  begin
    // yt-dlp gives no progress, show the elapsed time so it doesn't look
    // stuck (it can take a while: JS challenges, format selection...)
    if FURLLoadTimer = NIL then
    begin
      FURLLoadTimer := TTimer.Create(Self);
      FURLLoadTimer.Interval := 1000;
      FURLLoadTimer.OnTimer  := @URLLoadTimerTimer;
    end;
    FURLLoadStart := GetTickCount64;
    URLLoadTimerTimer(NIL);
    FURLLoadTimer.Enabled := True;
  end
  else
    SetStatusBarText(lngasReadingVideoFile);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.URLLoadTimerTimer(Sender: TObject);
begin
  SetStatusBarText(Format(lngasLoadingVideoFromURL,
    [TimeToString(GetTickCount64 - FURLLoadStart, 'mm:ss')]), 0, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.StopURLLoadTimer;
begin
  if Assigned(FURLLoadTimer) and FURLLoadTimer.Enabled then
  begin
    FURLLoadTimer.Enabled := False;
    SetStatusBarText('');
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVFileLoaded(Sender: TObject);
var
  MRUInfoObject: TMRUInfoObject;
begin
  StopURLLoadTimer;
  SetStatusBarText('');
  MPVUpdateMediaTitle;
  RefreshAppTitle;
  EnableActionsByTag([TAG_ACTION_VIDEO], True);
  // apply the text options at runtime too: the start options alone could
  // leave the overlay with mpv's default OSD size (seen on macOS)
  UpdateMPVOptions;
  MPV.ShowOverlayText('');
  MPVSetFilters;
  MPVSetPanAndScan(actPanAndScan.Checked);

  SubtitleInfo.LastSubtitle.ShowIndex := 0;
  sbrSeek.Max := MPV.GetMediaLenInMs;
  sbrSeek.Enabled := True;

  if Assigned(frmVideo) then
    frmVideo.Caption := MPVMediaName;

  actCloseMedia.Enabled       := True;
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
  begin
    WAVE.EmptyText := lngasWaveformText;
    // not inside the mpv event (it's a modal form)
    if MPVOptions.AutoExtractWaveform and not MPV.FileName.StartsWith('http') then
      Application.QueueAsyncCall(@DoAutoExtractWaveform, 0);
  end;

  TimerPlayback.Enabled := True;

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

  if WAVE.DrawThumbnail then
    GenerateThumbnails;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVPause(Sender: TObject);
begin
  actMediaPlay.ImageIndex := 38;
  MacMediaKeysUpdate(False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVPlay(Sender: TObject);
begin
  actMediaPlay.ImageIndex := 39;
  MacMediaKeysUpdate(True);
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
    if sbrSeek.Tag = TAG_CONTROL_NORMAL then
    begin
      sbrSeek.Position := Time;

      if lblMediaTime.Tag = TAG_CONTROL_UPDATE then
        lblMediaTime.Tag := TAG_CONTROL_NORMAL
      else
        lblMediaTime.Caption := GetTimeStr(Time + Workspace.VideoOffset);
    end;
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

procedure TfrmMain.DoPlaybackTimer(Sender: TObject);
var
  CurrentPos: Integer;
  CurrentText: String;
begin
  (Sender as TTimer).Enabled := False;

  try
    // 1. OBTENER TIEMPO
    CurrentPos := MPV.GetMediaPosInMs;

    // A. LÓGICA DE CONTROL (Loop / Auto-Pause)
    if (MPVOptions.EndTime > 0) and (CurrentPos >= MPVOptions.EndTime-1) then
    begin
      MPV.Pause;
      MPV.SetMediaPosInMs(MPVOptions.EndTime);
      MPVOptions.EndTime := 0;
      CurrentPos := MPVOptions.EndTime;
    end;

    // B. ACTUALIZAR WAVEFORM
    if (not WAVE.IsMouseDown) and WAVE.IsTimeLineEnabled then
      WAVE.SetPlayCursorMS(CurrentPos);

    // C. ACTUALIZAR SUBTÍTULOS Y OVERLAY
    if MPVOptions.SubtitleHandleByMPV then
    begin
      if actMediaAutoScroll.Checked then
        GetSubtitleIndexAtTime(CurrentPos + Workspace.VideoOffset);
    end
    else
    begin
      CurrentText := GetSubtitleTextAtTime(CurrentPos + Workspace.VideoOffset);
      //MPV.ShowText(CurrentText, 3600000, '');
      // the size comes from osd-font-size (MPV.FontSize, scaled to the video
      // area), the same one mpv uses for its own subtitles
      MPV.ShowOverlayText(CurrentText, '');
    end;

    // D. ACTUALIZAR GRID (VST)
    if SubtitleInfo.LastSubtitle.Selected <> SubtitleInfo.LastSubtitle.ShowIndex then
    begin
      SubtitleInfo.LastSubtitle.Selected := SubtitleInfo.LastSubtitle.ShowIndex;

      if actMediaAutoScroll.Checked then
        VSTSelectNode(VST, SubtitleInfo.LastSubtitle.Selected, True, True, True);
    end;
  finally
    (Sender as TTimer).Enabled := True;
  end;
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

procedure TfrmMain.sbrSeekMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  p: Integer;
begin
  if MPV.IsMediaLoaded then
    with sbrSeek do
      if MouseIsDown then
        lblMediaTime.Caption := GetTimeStr(Range((X * Max) div Width, 0, Max))
      else
      begin
        p := Range((X * Max) div Width, 0, sbrSeek.Max);
        //MPV.ShowText(GetTimeStr(p));
        ShowCustomHint(GetTimeStr(p));

        if MPV.IsPaused and (ssCtrl in Shift) then
        begin
          sbrSeek.Tag := TAG_CONTROL_UPDATE;
          MPVSeekTo(p);
        end;
      end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.sbrSeekMouseLeave(Sender: TObject);
begin
  if sbrSeek.Tag = TAG_CONTROL_UPDATE then
  begin
    sbrSeek.Tag := TAG_CONTROL_NORMAL;
    MPVSeekTo(sbrSeek.Position);
  end;

  //MPV.ShowText('');
  ShowCustomHint('');
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.sbrVolumeChange(Sender: TObject);
begin
  MPVOptions.Volume.Percent := Byte(sbrVolume.Position);
  MPV.SetAudioVolume(MPVOptions.Volume.Percent);
  sbrVolume.Hint := IntToStr(MPVOptions.Volume.Percent) + '%';
end;

// -----------------------------------------------------------------------------

