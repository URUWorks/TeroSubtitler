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

unit procUnDockVideoControls;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, Controls, SysUtils;

procedure PrepareUnDockData(const Clear: Boolean = False);

procedure UnDockVideoControls;
procedure DockVideoControls(const CalledFromVideoForm: Boolean = False);

procedure UnDockWaveformControls;
procedure DockWaveformControls(const CalledFromWaveForm: Boolean = False);

// -----------------------------------------------------------------------------

implementation

uses
  formMain, formVideo, formWaveform, procWorkspace, procTypes, procConfig;

// -----------------------------------------------------------------------------

procedure PrepareUnDockData(const Clear: Boolean = False);
begin
  if Clear then
    FillByte(MPVOptions.UnDockData, SizeOf(MPVOptions.UnDockData), 0)
  else
  with MPVOptions.UnDockData, frmMain do
  begin
    FileName        := MPV.FileName;
    CurrentPosition := MPV.GetMediaPosInMs;
    Paused          := MPV.IsPaused;

    MPV.Close;
  end;
end;

// -----------------------------------------------------------------------------

procedure UnDockVideoControls;
begin
  if not Assigned(frmVideo) then
    with frmMain do
    begin
      if not MPV.FileName.IsEmpty then
        PrepareUnDockData;

      actUnDockVideo.Checked := False;

      frmVideo := TfrmVideo.Create(Application);
      SplitterVideo.Hide;
      LayoutVideo.Parent := frmVideo;
      LayoutVideo.Align  := alClient;
      CheckColorTheme(frmVideo);
      LoadFormSettings(frmVideo);

      if actVideoPreview.Checked then
        frmVideo.Show;

      with MPVOptions.UnDockData do
        if not FileName.IsEmpty then
        begin
          MPV.Play(FileName, CurrentPosition);
          frmVideo.Caption := ExtractFileName(FileName);
        end;
    end;
end;

// -----------------------------------------------------------------------------

procedure DockVideoControls(const CalledFromVideoForm: Boolean = False);
begin
  if Assigned(frmVideo) then
  begin
    if not CalledFromVideoForm then
      frmVideo.Close
    else
      with frmMain do
      begin
        if not MPV.FileName.IsEmpty then
          PrepareUnDockData;

        actUnDockVideo.Checked := True;

        LayoutVideo.Parent    := frmMain;
        SetWorkspaceLayout(Workspace.Layout);
        LayoutVideo.Visible   := actVideoPreview.Checked;
        SplitterVideo.Visible := actVideoPreview.Checked;

        with MPVOptions.UnDockData do
          if not FileName.IsEmpty then
          begin
            MPV.Play(FileName, CurrentPosition);
            frmVideo.Caption := ExtractFileName(FileName);
          end;
      end;
  end;
end;

// -----------------------------------------------------------------------------

procedure UnDockWaveformControls;
begin
  if not Assigned(frmWaveform) then
    with frmMain do
    begin
      actUnDockWaveform.Checked := False;

      frmWaveform := TfrmWaveform.Create(Application);
      frmWaveform.Caption := ExtractFileName(WAVE.FileName);
      SplitterWaveform.Hide;
      LayoutWaveform.Parent := frmWaveform;
      LayoutWaveform.Align  := alClient;
      LayoutWaveform.Constraints.MaxHeight := 0;
      frmWaveform.Constraints := LayoutWaveform.Constraints;
      CheckColorTheme(frmWaveform);
      LoadFormSettings(frmWaveform);

      if actTimeline.Checked then
        frmWaveform.Show;
    end;
end;

// -----------------------------------------------------------------------------

procedure DockWaveformControls(const CalledFromWaveForm: Boolean = False);
begin
  if Assigned(frmWaveform) then
  begin
    if not CalledFromWaveForm then
      frmWaveform.Close
    else
      with frmMain do
      begin
        actUnDockWaveform.Checked := True;

        LayoutWaveform.Parent    := frmMain;
        LayoutWaveform.Align     := alBottom;
        LayoutWaveform.Constraints.MaxHeight := 300;
        LayoutWaveform.Visible   := actTimeline.Checked;
        SplitterWaveform.Visible := actTimeline.Checked;
        SplitterWaveform.Top     := 0;
      end;
  end;
end;

// -----------------------------------------------------------------------------

end.
