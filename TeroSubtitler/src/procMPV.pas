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

unit procMPV;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, procTypes, formMain,
  UWSubtitleAPI, UWSubtitleAPI.Formats;

procedure PrepareMPV;
procedure UpdateMPVOptions;
procedure MPVPlaySelectionOnly;
procedure MPVPlay(const PlayMode: TMediaPlayMode = mpmAll);
procedure MPVSeekTo(const AForward: Boolean; const MSecsToSeek: Integer); overload;
procedure MPVSeekTo(const Value: Integer; const Play: Boolean = False); overload;
procedure MPVAlterPlayRate(const Value: Boolean);
procedure MPVLoadSubtitleTempTrack;
procedure MPVReloadSubtitleTempTrack;
procedure MPVRemoveSubtitleTempTrack;
function MPVSaveSubtitleTempTrack: Boolean;
procedure MPVDeleteSubtitleTempTrack;

// -----------------------------------------------------------------------------

implementation

uses MPVPlayer, procCommon, UWSystem.Encoding;

// -----------------------------------------------------------------------------

procedure PrepareMPV;
begin
  with frmMain.MPV do
  begin
    {$IFNDEF LINUX}
    MPVFileName := libMPVFileName(False);
    {$ENDIF}
    {$IFDEF DARWIN}
    YTDLPFileName := procCommon.YTDLPFileName;
    {$ENDIF}
    LogLevel := llNo;
    AutoStartPlayback := False;
    SMPTEMode := frmMain.actSMPTE.Checked;
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
      AddOption('osd-scale-by-window=yes');
      AddOption('osd-scale-with-window=yes');
      AddOption('sub-scale-by-window=yes');
      AddOption('sub-scale-with-window=yes');
      AddOption('volume=' + IntToStr(Volume.Percent));
      AddOption('mute=' + BoolToStr(Volume.Mute, 'yes', 'no'));

      //AddOption('log-file='+LogMPVFileName);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure UpdateMPVOptions;
begin
  with frmMain, MPVOptions do
  begin
    MPV.mpv_set_option_string_('osd-font-size=' + IntToStr(TextSize));
    MPV.mpv_set_option_string_('sub-font-size=' + IntToStr(TextSize));
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
  end;
end;

// -----------------------------------------------------------------------------

procedure MPVPlaySelectionOnly;
begin
  with frmMain do
    if WAVE.IsTimeLineEnabled and (not WAVE.SelectionIsEmpty) then
      MPV.Loop(WAVE.Selection.InitialTime, WAVE.Selection.FinalTime, WAVEOptions.LoopCount);
end;

// -----------------------------------------------------------------------------

procedure MPVPlayFromSelection(const Value: Integer = 0; const FromInitialTime: Boolean = True);
begin
  with frmMain do
    if WAVE.IsTimeLineEnabled and (not WAVE.SelectionIsEmpty) then
      if FromInitialTime then
        MPVSeekTo(WAVE.Selection.InitialTime + Value, True)
      else
        MPVSeekTo(WAVE.Selection.FinalTime + Value, True);
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

procedure MPVSeekTo(const Value: Integer; const Play: Boolean = False);
begin
  with frmMain do
  begin
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
end;

// -----------------------------------------------------------------------------

procedure MPVReloadSubtitleTempTrack;
begin
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
  AFPS      : Single;
  AEncoding : TEncoding;
begin
  AFPS      := Workspace.FPS.OutputFPS;
  AEncoding := TEncoding.GetEncoding(Encodings[Workspace.DefEncoding].CPID);
  Result    := Subtitles.SaveToFile(MPVTempSubFileName, AFPS, AEncoding, sfAdvancedSubStationAlpha, smText);
end;

// -----------------------------------------------------------------------------

procedure MPVDeleteSubtitleTempTrack;
begin
  if FileExists(MPVTempSubFileName) then DeleteFile(MPVTempSubFileName);
end;

// -----------------------------------------------------------------------------

end.
