{*
 *  URUWorks Tero Subtitler
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

unit procMPV;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, ActnList, procTypes, formMain,
  UWSubtitleAPI, UWSubtitleAPI.Formats, MPVPlayer, UWSystem.TimeUtils;

procedure PrepareMPV;
procedure MPVUpdateMediaTitle(const AClear: Boolean = False);
function MPVMediaName: String;
function MPVMediaBaseName: String;
procedure UpdateMPVStartOptions(const AOldAdditionalOptions: TStringArray);
procedure UpdateMPVOptions;
procedure MPVPlaySelectionOnly;
procedure MPVCheckSelectionEnd(const ACurrentPos: Integer);
procedure MPVPlay(const PlayMode: TMediaPlayMode = mpmAll);
procedure MPVSeekTo(const AForward: Boolean; const MSecsToSeek: Integer); overload;
procedure MPVSeekTo(const Value: Integer; const Play: Boolean = False); overload;
procedure MPVAlterPlayRate(const Value: Boolean);
procedure MPVLoadSubtitleTempTrack;
procedure MPVReloadSubtitleTempTrack(const ASaveBefore: Boolean = False);
procedure MPVRemoveSubtitleTempTrack;
function MPVSaveSubtitleTempTrack: Boolean;
procedure MPVDeleteSubtitleTempTrack;
function PrepareSSAStyleString: AnsiString;
procedure MPVSetVideoFilters;
procedure MPVSetAudioFilters;
procedure MPVSetFilters;
procedure MPVSetVideoAspectRatio(const AValue: TMPVPlayerVideoAspectRatio);
procedure MPVCycleVideoAspectRatio;
procedure MPVSetPanAndScan(const AValue: Boolean);
procedure MPVAutoCrop(const AValue: Boolean);
procedure MPVShowVideoInformation;
function MPVHasAudioTrack: Boolean;

// Video offset (Media > Video offset): the time of the first frame of the
// video. The entries, the timeline and the time shown are in entry time
// (video time + offset); mpv always plays video time
function EntryToVideoMS(const AEntryMS: Integer): Integer;
function VideoToEntryMS(const AVideoMS: Integer): Integer;
function MPVGetEntryPosInMs: Integer;
procedure MPVSeekToEntryTime(const AEntryMS: Integer; const AEndEntryMS: Integer = 0; const APlay: Boolean = False);
procedure MPVSetVideoOffset(const AValue: Integer);
procedure MPVApplySubtitleDelay;

// -----------------------------------------------------------------------------

implementation

uses procConfig, UWSystem.Encoding, UWSystem.SysUtils, MPVPlayer.Filters,
  procSubtitle, formCustomMessageDlg, procLocalize, procDialogs, procWorkspace;

var
  // "Play selection": its end (video time) until its last pass ends
  SelectionLoopEnd: Integer = 0;

var
  MediaTitle: String = ''; // title of an url (media-title of mpv/yt-dlp)

// -----------------------------------------------------------------------------

procedure MPVUpdateMediaTitle(const AClear: Boolean = False);
begin
  MediaTitle := '';
  if not AClear and frmMain.MPV.FileName.StartsWith('http') then
    MediaTitle := Trim(frmMain.MPV.mpv_get_property_string_('media-title'));
end;

// -----------------------------------------------------------------------------

function MPVMediaName: String;
begin
  // the url is not useful to show (e.g. watch?v=...), use its title
  if MediaTitle <> '' then
    Result := MediaTitle
  else
    Result := ExtractFileName(frmMain.MPV.FileName);
end;

// -----------------------------------------------------------------------------

function MPVMediaBaseName: String;
var
  c: Char;
begin
  // to suggest file names (subtitle, screenshots...)
  if MediaTitle <> '' then
  begin
    Result := MediaTitle;
    for c in '\/:*?"<>|' do
      Result := StringReplace(Result, c, '_', [rfReplaceAll]);
  end
  else
    Result := ChangeFileExt(ExtractFileName(frmMain.MPV.FileName), '');
end;

// -----------------------------------------------------------------------------

procedure SetMPVStartOptions;
var
  i: Integer;
begin
  with frmMain.MPV do
  begin
    with MPVOptions do
    begin
      AddOption('osd-color=' + TextColor);
      AddOption('sub-color=' + TextColor);
      AddOption('osd-border-color=' + TextBorderColor);
      AddOption('sub-border-color=' + TextBorderColor);
      if UseTextShadowColor then
      begin
        AddOption('osd-shadow-color=' + TextShadowColor);
        AddOption('sub-shadow-color=' + TextShadowColor);
        AddOption('osd-shadow-offset=' + IntToStr(TextShadowOffset));
        AddOption('sub-shadow-offset=' + IntToStr(TextShadowOffset));
      end;
      if UseTextBackgroundColor then
      begin
        AddOption('osd-back-color=' + TextBackgroundColor);
        AddOption('sub-back-color=' + TextBackgroundColor);
      end;
      AddOption('osd-align-x=center');
      AddOption('osd-align-y=' + TextPosition);
      AddOption('osd-font-size=' + IntToStr(TextSize));
      AddOption('sub-font-size=' + IntToStr(TextSize));
      if TextFont <> '' then
      begin
        AddOption('osd-font=' + TextFont);
        AddOption('sub-font=' + TextFont);
      end;
      AddOption('osd-scale-by-window=yes');
      AddOption('sub-scale-by-window=yes');
      AddOption('sub-scale-with-window=yes');
      AddOption('volume=' + IntToStr(Volume.Percent));
      AddOption('mute=' + SysUtils.BoolToStr(Volume.Mute, 'yes', 'no'));

      //AddOption('log-file='+LogMPVFileName);

      // Additional mpv options
      if Length(AdditionalOptions) > 0 then
        for i := 0 to High(AdditionalOptions) do
          AddOption(AdditionalOptions[i]);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure PrepareMPV;
begin
  with frmMain.MPV do
  begin
    YTDLPFileName := procConfig.YTDLPFileName;
    LogLevel := llNo;
    AutoStartPlayback := False;
    FontSize := MPVOptions.TextSize;
    //SMPTEMode := Workspace.SMPTE;
  end;
  SetMPVStartOptions;
end;

// -----------------------------------------------------------------------------

procedure UpdateMPVStartOptions(const AOldAdditionalOptions: TStringArray);
var
  i: Integer;
begin
  // start options are only applied by mpv when it is initialized (on video
  // load), they were filled once at startup so changes made in Settings
  // (e.g. additional options) needed a restart. Rebuild them here, removing
  // the previous ones first (optional and user ones may have been removed).
  with frmMain.MPV do
  begin
    for i := 0 to High(AOldAdditionalOptions) do
      RemoveOption(AOldAdditionalOptions[i]);

    RemoveOption('osd-shadow-color=');
    RemoveOption('sub-shadow-color=');
    RemoveOption('osd-shadow-offset=');
    RemoveOption('sub-shadow-offset=');
    RemoveOption('osd-back-color=');
    RemoveOption('sub-back-color=');
  end;
  SetMPVStartOptions;
end;

// -----------------------------------------------------------------------------

procedure UpdateMPVOptions;
begin
  with frmMain, MPVOptions do
  begin
    // the player sets osd/sub-font-size itself on every resize and video
    // reconfig (scaled to the video area), from its FontSize property. Setting
    // the mpv options directly was overwritten with its default size (55) at
    // the next resize, the text looked huge until Settings was closed.
    MPV.FontSize := TextSize;
    // the font: the overlay text uses osd-font, the subtitle track sub-font
    // ('sans-serif' is the default of mpv, to undo a font set before)
    MPV.mpv_set_option_string_('osd-font=' + iff(TextFont <> '', TextFont, 'sans-serif'));
    MPV.mpv_set_option_string_('sub-font=' + iff(TextFont <> '', TextFont, 'sans-serif'));
    MPV.mpv_set_option_string_('osd-color=' + TextColor);
    MPV.mpv_set_option_string_('sub-color=' + TextColor);
    MPV.mpv_set_option_string_('osd-border-color=' + TextBorderColor);
    MPV.mpv_set_option_string_('sub-border-color=' + TextBorderColor);

    if UseTextShadowColor then
    begin
      MPV.mpv_set_option_string_('osd-shadow-color=' + TextShadowColor);
      MPV.mpv_set_option_string_('sub-shadow-color=' + TextShadowColor);
      MPV.mpv_set_option_string_('osd-shadow-offset=' + IntToStr(TextShadowOffset));
      MPV.mpv_set_option_string_('sub-shadow-offset=' + IntToStr(TextShadowOffset));
    end;

    if UseTextBackgroundColor then
    begin
      MPV.mpv_set_option_string_('osd-back-color=' + TextBackgroundColor);
      MPV.mpv_set_option_string_('sub-back-color=' + TextBackgroundColor);
    end
    else
    begin
      MPV.mpv_set_option_string_('osd-back-color=#00000000');
      MPV.mpv_set_option_string_('sub-back-color=#00000000');
    end;
    // the subtitle overlay is only sent when its text changes: send it again
    // with the new style (also when paused, the playback timer keeps running)
    MPV.InvalidateOverlayText;
  end;
end;

// -----------------------------------------------------------------------------

procedure MPVPlaySelectionOnly;
begin
  with frmMain do
    if WAVE.IsTimeLineEnabled and (not WAVE.SelectionIsEmpty) then
    begin
      SelectionLoopEnd := EntryToVideoMS(WAVE.Selection.FinalTime);
      MPV.Loop(EntryToVideoMS(WAVE.Selection.InitialTime), SelectionLoopEnd, WAVEOptions.LoopCount - 1);
    end;
end;

// -----------------------------------------------------------------------------

// the passes left of the A-B loop. Since mpv 0.39 the option ab-loop-count
// keeps the value it was given and the count is the property
// remaining-ab-loops: reading ab-loop-count, with 2 or more passes it was
// never 0 and the selection didn't stop at its end (#511). With an older
// libmpv (without remaining-ab-loops) the option itself is decremented
function MPVRemainingABLoops: String;
begin
  with frmMain do
  begin
    Result := MPV.mpv_get_property_string_('remaining-ab-loops');
    if Result = '' then
      Result := MPV.mpv_get_property_string_('ab-loop-count');
  end;
end;

// -----------------------------------------------------------------------------

// called by the playback timer: after the last pass of "Play selection" mpv
// ignores the A-B points (no passes left then) and kept playing after
// the end of the selection, to the end of the video
procedure MPVCheckSelectionEnd(const ACurrentPos: Integer);
begin
  if (SelectionLoopEnd <= 0) then Exit;

  with frmMain do
    if (Workspace.MediaPlayMode <> mpmSelection) then
      SelectionLoopEnd := 0
    else if (ACurrentPos >= SelectionLoopEnd-1) and
      (MPVRemainingABLoops = '0') then
    begin
      MPV.Pause;
      MPV.SetMediaPosInMs(SelectionLoopEnd);
      SelectionLoopEnd := 0;
    end;
end;

// -----------------------------------------------------------------------------

procedure MPVPlayFromSelection(const Value: Integer = 0; const FromInitialTime: Boolean = True);
begin
  with frmMain do
    if WAVE.IsTimeLineEnabled and (not WAVE.SelectionIsEmpty) then
      if FromInitialTime then
        MPVSeekTo(EntryToVideoMS(WAVE.Selection.InitialTime + Value), True)
      else
        MPVSeekTo(EntryToVideoMS(WAVE.Selection.FinalTime + Value), True);
end;

// -----------------------------------------------------------------------------

procedure MPVPlay(const PlayMode: TMediaPlayMode = mpmAll);
begin
  Workspace.MediaPlayMode := PlayMode;

  with frmMain do
  begin
    case PlayMode of
      mpmSelection       : MPVPlaySelectionOnly;
      mpmFromSelection   : MPVPlayFromSelection;
      mpmBeforeSelection : MPVPlayFromSelection(-500);
      mpmAfterSelection  : MPVPlayFromSelection(+500, False);
    else
      MPV.Resume;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure MPVSeekTo(const AForward: Boolean; const MSecsToSeek: Integer);
var
  ct, mt, tt: Integer;
begin
  with frmMain do
  begin
    ct := MPV.GetMediaPosInMs;
    tt := MPV.GetMediaLenInMs;

    if AForward then
      mt := ct + MSecsToSeek
    else
      mt := ct - MSecsToSeek;

    if mt < 0 then
      mt := 0
    else if mt > tt then
      mt := tt;

    MPVSeekTo(mt);
  end;
end;

// -----------------------------------------------------------------------------

function EntryToVideoMS(const AEntryMS: Integer): Integer;
begin
  Result := AEntryMS - Workspace.VideoOffset;
  if Result < 0 then Result := 0;
end;

// -----------------------------------------------------------------------------

function VideoToEntryMS(const AVideoMS: Integer): Integer;
begin
  Result := AVideoMS + Workspace.VideoOffset;
end;

// -----------------------------------------------------------------------------

function MPVGetEntryPosInMs: Integer;
begin
  Result := VideoToEntryMS(frmMain.MPV.GetMediaPosInMs);
end;

// -----------------------------------------------------------------------------

// seeks the video to an entry time (and pauses at AEndEntryMS if > 0)
procedure MPVSeekToEntryTime(const AEntryMS: Integer; const AEndEntryMS: Integer = 0; const APlay: Boolean = False);
begin
  with frmMain do
  begin
    MPV.SetMediaPosInMs(EntryToVideoMS(AEntryMS));
    // EndTime is compared with the player position: video time
    if AEndEntryMS > 0 then
      MPVOptions.EndTime := EntryToVideoMS(AEndEntryMS)
    else
      MPVOptions.EndTime := 0;

    if APlay and not MPV.IsPlaying then
      MPV.Resume(True);
  end;
end;

// -----------------------------------------------------------------------------

// mpv draws the subtitle track at entry time - offset
procedure MPVApplySubtitleDelay;
var
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  with frmMain do
    if MPV.Initialized then
      MPV.mpv_set_option_string_('sub-delay=' + FormatFloat('0.000', -Workspace.VideoOffset / 1000, fs));
end;

// -----------------------------------------------------------------------------

procedure MPVSetVideoOffset(const AValue: Integer);
begin
  if AValue = Workspace.VideoOffset then Exit;

  Workspace.VideoOffset := AValue;
  MPVApplySubtitleDelay;
  with frmMain do
  begin
    // the timeline is in entry time: its scene changes are moved too
    WAVE.OffsetMs := AValue;
    if MPV.Initialized then
    begin
      lblMediaTime.Caption := GetTimeStr(MPVGetEntryPosInMs);
      if WAVE.IsTimeLineEnabled then
        WAVE.SetPlayCursorMS(MPVGetEntryPosInMs);
    end;
  end;
  UpdateVideoLengthString;
end;

// -----------------------------------------------------------------------------

procedure MPVSeekTo(const Value: Integer; const Play: Boolean = False);
begin
  with frmMain do
  begin
    MPVOptions.EndTime := 0;
    MPV.SetMediaPosInMs(Value);
    if Play then MPV.Resume(True);
  end;
end;

// -----------------------------------------------------------------------------

procedure MPVAlterPlayRate(const Value: Boolean);
begin
  with frmMain do
    if Value then
      MPV.SetPlaybackRate(AppOptions.DefChangePlayRate)
    else
      MPV.SetPlaybackRate(100);
end;

// -----------------------------------------------------------------------------

procedure MPVLoadSubtitleTempTrack;
begin
  frmMain.MPV.LoadTrack(ttSubtitle, MPVTempSubFileName);
  MPVApplySubtitleDelay;
end;

// -----------------------------------------------------------------------------

procedure MPVReloadSubtitleTempTrack(const ASaveBefore: Boolean = False);
begin
  if ASaveBefore then MPVSaveSubtitleTempTrack;
  frmMain.MPV.ReloadTrack(ttSubtitle);
end;

// -----------------------------------------------------------------------------

procedure MPVRemoveSubtitleTempTrack;
begin
  frmMain.MPV.RemoveTrack(ttSubtitle);
end;

// -----------------------------------------------------------------------------

function MPVSaveSubtitleTempTrack: Boolean;
var
  AFPS : TFPSInfo;
  OldTimeBase : TSubtitleTimeBase;
  OldStyle    : AnsiString;
begin
  AFPS := Workspace.FPS.OutputFPS;

  // mpv plays media time and the stored times are already media times (the
  // same ones the overlay compares with the player position), so they are
  // written without the SMPTE x1.001 conversion. Before, any frames mode with
  // a non integer FPS forced it (~3.6 s late per hour) and TimeBase stayed
  // changed for the rest of the program.
  OldTimeBase := Subtitles.TimeBase;
  Subtitles.TimeBase := stbMedia;

  // the style is built from the current mpv options when writing, so the
  // track never depends on the call order (before, it used the stored ASS
  // style until UpdateMPVOptions ran, huge text until Settings was closed)
  OldStyle := Subtitles.FormatProperties^.SSA.DefaultStyleSettings;
  Subtitles.FormatProperties^.SSA.DefaultStyleSettings := PrepareSSAStyleString;

  Subtitles.Tag := 1;
  try
    // same text the overlay shows (text or translation)
    Result := Subtitles.SaveToFile(MPVTempSubFileName, AFPS, TEncoding.UTF8, sfAdvancedSubStationAlpha, MPVOptions.SubtitleToShow);
  finally
    Subtitles.Tag := 0;
    Subtitles.TimeBase := OldTimeBase;
    Subtitles.FormatProperties^.SSA.DefaultStyleSettings := OldStyle;
  end;
end;

// -----------------------------------------------------------------------------

procedure MPVDeleteSubtitleTempTrack;
begin
  if FileExists(MPVTempSubFileName) then DeleteFile(MPVTempSubFileName);
end;

// -----------------------------------------------------------------------------

function PrepareSSAStyleString: AnsiString;

  function CorrectColorFormat(const AColor: String): String;
  begin
    Result := Format('&H00%s%s%s', [Copy(AColor, 6, 2), Copy(AColor, 4, 2), Copy(AColor, 2, 2)]);
  end;

var
  FontName: String;
begin
  // a comma would split the fields of the style
  FontName := StringReplace(MPVOptions.TextFont, ',', ' ', [rfReplaceAll]);
  if FontName = '' then FontName := 'Arial';

  with MPVOptions do
    Result := Format('Default,%s,%d,%s,%s,%s,%s,0,0,0,0,100,100,0,0,%d,1,1,2,10,10,10,1', [FontName, TextSize, CorrectColorFormat(TextColor), CorrectColorFormat(TextColor), CorrectColorFormat(TextBorderColor), CorrectColorFormat(TextBorderColor), iff(MPVOptions.UseTextBackgroundColor, 3, 1)]);
end;

// -----------------------------------------------------------------------------

procedure MPVSetVideoFilters;
var
  AVideoFilters: TMPVPlayerVideoFilters;
begin
  AVideoFilters := [];

  with frmMain do
  begin
    if actVideoFilterHFlip.Checked then Include(AVideoFilters, vfHFlip);
    if actVideoFilterVFlip.Checked then Include(AVideoFilters, vfVFlip);
    if actVideoFilterSharpen.Checked then Include(AVideoFilters, vfSharpen);
    if actVideoFilterBlur.Checked then Include(AVideoFilters, vfBlur);
    if actVideoFilterEdgeEnhance.Checked then Include(AVideoFilters, vfEdgeEnhance);
    if actVideoFilterEmboss.Checked then Include(AVideoFilters, vfEmboss);
    if actVideoFilterNegative.Checked then Include(AVideoFilters, vfNegative);
    if actVideoFilterVintage.Checked then Include(AVideoFilters, vfVintage);
    if actVideoFilterDeinterlace.Checked then Include(AVideoFilters, vfDeinterlace);

    MPV.SetVideoFilters(AVideoFilters);
  end;
end;

// -----------------------------------------------------------------------------

procedure MPVSetAudioFilters;
var
  AAudioFilters: TMPVPlayerAudioFilters;
begin
  AAudioFilters := [];

  with frmMain do
  begin
    if actAudioFilterDialoguEnhance.Checked then Include(AAudioFilters, afDialoguEnhance);
    if actAudioFilterSurround.Checked then Include(AAudioFilters, afSurround);
    if actAudioFilterSpeechNormalizer.Checked then Include(AAudioFilters, afSpeechNormalizer);

    MPV.SetAudioFilters(AAudioFilters);
  end;
end;

// -----------------------------------------------------------------------------

procedure MPVSetFilters;
begin
  MPVSetVideoFilters;
  MPVSetAudioFilters;
end;

// -----------------------------------------------------------------------------

function VARtoAction(const AValue: TMPVPlayerVideoAspectRatio): TAction;
begin
  with frmMain do
  begin
    case AValue of
      ar4_3   : Result := actVAR43;
      ar16_9  : Result := actVAR169;
      ar185_1 : Result := actVAR1851;
      ar235_1 : Result := actVAR2351;
    else
      Result := actVARDefault;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure UncheckVARActions;
begin
  with frmMain do
  begin
    actVARDefault.Checked := False;
    actVAR43.Checked := False;
    actVAR169.Checked := False;
    actVAR1851.Checked := False;
    actVAR2351.Checked := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure MPVSetVideoAspectRatio(const AValue: TMPVPlayerVideoAspectRatio);
begin
  with frmMain do
  begin
    UncheckVARActions;
    MPV.SetVideoAspectRatio(AValue);
    VARtoAction(MPV.AspectRatio).Checked := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure MPVCycleVideoAspectRatio;
begin
  with frmMain do
  begin
    UncheckVARActions;
    VARtoAction(MPV.CycleVideoAspectRatio).Checked := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure MPVSetPanAndScan(const AValue: Boolean);
begin
  with frmMain do
  begin
    actPanAndScan.Checked := AValue;
    MPV.mpv_set_option_string_('panscan=' + IntToStr(iff(AValue, 1, 0)));
  end;
end;

// -----------------------------------------------------------------------------

// Media > Aspect ratio > Auto-crop: crops the black bars detected in the
// current frame (the image is centered by mpv). The timeline thumbnails are
// made again with the same crop (the cropped ones are cached apart, so going
// back to the full frame doesn't call ffmpeg again). A thread cancelled by the
// toggle can't load its thumbnail anymore (DoThumbnailReady checks the
// Sender) nor leave a cut .bmp (temporary name per thread).
procedure MPVAutoCrop(const AValue: Boolean);
var
  R: TRect;
begin
  with frmMain do
  begin
    if not MPV.IsMediaLoaded then Exit;

    if AValue then
    begin
      case MPV.DetectVideoCrop(R) of
        cdOk:
          begin
            MPV.SetVideoCrop(R);
            SetStatusBarText(Format(lngAutoCropApplied, [R.Width, R.Height]));
          end;
        cdNoBars:
          begin
            ShowMessageDialog(lngAutoCropNoBars);
            Exit;
          end;
      else
        ShowMessageDialog(lngAutoCropNoImage);
        Exit;
      end;
    end
    else
    begin
      MPV.SetVideoCrop(Rect(0, 0, 0, 0));
      SetStatusBarText('');
    end;

    actVARAutoCrop.Checked := MPV.IsVideoCropped;

    // the Key of GenerateThumbnails includes the crop: it frees the loaded
    // ones and makes them again (from the cache if they were made before)
    if WAVE.DrawThumbnail then
      GenerateThumbnails;
  end;
end;

// -----------------------------------------------------------------------------

procedure MPVShowVideoInformation;
var
  s: String;
  i, tl, tas, tvs, tss: Integer;
begin
  with frmMain do
    if MPV.IsMediaLoaded then
    begin
      tl  := Length(MPV.TrackList);
      tas := 0;
      tvs := 0;
      tss := 0;
      if tl > 0 then
      begin
        for i := 0 to tl-1 do
        begin
          if MPV.TrackList[i].Kind = ttAudio then
            Inc(tas)
          else if MPV.TrackList[i].Kind = ttVideo then
            Inc(tvs)
          else if MPV.TrackList[i].Kind = ttSubtitle then
            Inc(tss);
        end;
      end;

      with TfrmCustomMessageDlg.Create(NIL) do
      try
        lblMessage.Font.Style := [];
        lblMessage.Alignment := taLeftJustify;

        s := Format(
               lngviFile + LineEnding +
               lngviDuration + LineEnding +
               lngviFPS + LineEnding +
               lngviResolution + LineEnding +
               lngviVideoTracks + LineEnding +
               lngviAudioTracks + LineEnding +
               lngviSubtitleTracks,
               [MPVMediaName,
               GetTimeStr(MPV.GetMediaLenInMs), MPV.GetVideoTotalFrames,
               SingleToStr(MPV.GetVideoFPS, FormatSettings),
               MPV.GetVideoWidth, MPV.GetVideoHeight,
               tvs,
               tas,
               tss
               ]);

        Execute(s, '', imInformation);
      finally
        Free;
      end;
    end;
end;

// -----------------------------------------------------------------------------

function MPVHasAudioTrack: Boolean;
var
  i: Integer;
begin
  // the cached track list is filled on the audio/video reconfig events, that
  // can arrive after the file is loaded: read it again from mpv here.
  // An empty list means "don't know" (mpv not ready), never block in that case.
  Result := True;
  with frmMain do
  begin
    if not MPV.IsMediaLoaded then Exit;
    MPV.GetTracks;
    if Length(MPV.TrackList) = 0 then Exit;

    for i := 0 to Length(MPV.TrackList)-1 do
      if MPV.TrackList[i].Kind = ttAudio then Exit;
  end;
  Result := False;
end;

// -----------------------------------------------------------------------------

end.
