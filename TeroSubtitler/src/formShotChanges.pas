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

unit formShotChanges;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Spin, UWTimeEdit, Types, process, StrUtils, LCLTranslator,procLocalize,
  MPVPlayer.SceneDetect;

type

  { TfrmShotChanges }

  TfrmShotChanges = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    btnImport: TButton;
    btnDetect: TButton;
    btnExport: TButton;
    cboOffset: TComboBox;
    cboTimeCodeImport: TComboBox;
    cboTimeCodeExport: TComboBox;
    cboDetectApp: TComboBox;
    lblOffset: TLabel;
    lblDetectApp: TLabel;
    prbExtracting: TProgressBar;
    spnSensitivity: TFloatSpinEdit;
    lblSensitivity: TLabel;
    lblIdle: TLabel;
    lblTimeCodeIn: TLabel;
    lblTimeCodeOut: TLabel;
    mmoTimes: TMemo;
    tedOffset: TUWTimeEdit;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure btnDetectClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mmoTimesChange(Sender: TObject);
    procedure spnSensitivityChange(Sender: TObject);
  private
    FTimeElapsed: Double;
    FCancel: Boolean;
    FSMPTE: Boolean;
    FFromWave: Boolean; // the times in the memo are the ones of the timeline (ms)
    FDetector: TMPVSceneDetector; // the last native detection: its scores, to change the sensitivity at once
    FFillingMemo: Boolean;
    procedure FillNativeCuts;
    procedure SetControlsEnabled(const AValue: Boolean);
    procedure RunApp_CB(Sender, Context: TObject; Status: TRunCommandEventCode; const Message: String);
    function IsTimeCode(const ACode: Integer): Boolean;
    function IsTimeCodeOut(const ACode: Integer): Boolean;
    function LineToMs(const ALine: String; out AMs: Integer): Boolean;
    function DetectTimeCode(const ALine: String): Integer;
    function FillDialogFilter(const AAllSupportedFiles: Boolean): String;
    function GetDetectApp: Integer;
    procedure SetDetectApp(const AApp: Integer);
    procedure DetectNative;
  public

  end;

var
  frmShotChanges: TfrmShotChanges;

implementation

uses
  procTypes, procWorkspace, procConfig, procDialogs, UWSystem.TimeUtils,
  UWSystem.SysUtils, UWSystem.Process, formMain, UWSubtitleAPI.EDL,
  formCustomSelectDlg;

const
  tcFrames       = 0;
  tcSeconds      = 1;
  tcMilliseconds = 2;
  tcHHMMSSZZZ    = 3;
  tcHHMMSSFF     = 4;

  // cboDetectApp: the app of each item is in Objects (the saved setting is
  // the app, not the item index: PySceneDetect is only listed if installed)
  daFFmpeg       = 0;
  daSceneDetect  = 1;
  daNative       = 2; // UW_MPVPlayer's TMPVSceneDetector (libmpv)

{$R *.lfm}

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

// seconds with or without decimals, always with '.', whatever the system decimal separator
function SecondsToMs(const AText: String; out AMs: Integer): Boolean;
var
  fs: TFormatSettings;
  d: Double;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  fs.ThousandSeparator := #0;
  Result := TryStrToFloat(Trim(AText), d, fs) and (d >= 0) and (d < MaxInt / 1000);
  if Result then
    AMs := Round(d * 1000)
  else
    AMs := 0;
end;

// -----------------------------------------------------------------------------

function MsToSeconds(const AMs: Integer): String;
var
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  Result := FormatFloat('0.000', AMs / 1000, fs);
end;

// -----------------------------------------------------------------------------

// slugs, stills, black, etc.: events without a real clip timecode.
// "CLIP HAD NO TIMECODE TRACK" is not one of them: the clip times are still
// valid, counted from 00:00:00:00
function IsEDLEventWithoutTimecode(const AItem: PItem): Boolean;
begin
  Result := (AItem^.ReelName.ToUpper = 'BL') or (AItem^.ReelName.ToUpper = 'BLACK') or
    (AItem^.ClipName.ToUpper = 'SLUG') or AItem^.Comment.ToUpper.Contains('IS A STILL');
end;

// -----------------------------------------------------------------------------

// the sensitivity is FFmpeg's scene score (0.1..0.9, 0.4 by default), that is
// about the average pixel difference / 100. detect-content uses the average
// HSV difference (0..255, 27 by default): 0.4 gives its default, 0.1 = 7 and
// 0.9 = 61, higher is less sensitive in both
function SceneDetectThreshold(const ASensitivity: Double): String;
var
  fs: TFormatSettings;
  t: Double;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  t := ASensitivity * 67.5;
  if t < 1 then
    t := 1
  else if t > 255 then
    t := 255;
  Result := FloatToStrF(t, ffFixed, 15, 1, fs);
end;

// -----------------------------------------------------------------------------

// the cuts (ms) of a "scenedetect list-scenes" CSV: an optional "Timecode
// List:" line (not with --skip-cuts), the header and a line per scene. The
// columns are found by the header; the start of the first scene (0) isn't a cut
procedure ParseSceneDetectCSV(const ACSV, ATimes: TStrings);
var
  i, j, h, colSecs, colTC, ms: Integer;
  cols: TStringArray;
  fs: TFormatSettings;
  v: Double;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  colSecs := -1;
  colTC   := 2;
  h := -1;
  for i := 0 to ACSV.Count-1 do
    if ACSV[i].StartsWith('Scene Number') then
    begin
      h := i;
      cols := ACSV[i].Split(',');
      for j := 0 to High(cols) do
        if Trim(cols[j]) = 'Start Time (seconds)' then
          colSecs := j
        else if Trim(cols[j]) = 'Start Timecode' then
          colTC := j;
      Break;
    end;

  if h < 0 then Exit;

  for i := h+1 to ACSV.Count-1 do
  begin
    cols := ACSV[i].Split(',');
    ms := 0;
    if (colSecs >= 0) and (colSecs <= High(cols)) and TryStrToFloat(Trim(cols[colSecs]), v, fs) then
      ms := Round(v * 1000)
    else if colTC <= High(cols) then
      ms := StringToTime(Trim(cols[colTC]));

    if ms > 0 then
      ATimes.Add(ms.ToString);
  end;
end;

// -----------------------------------------------------------------------------

{ TfrmShotChanges }

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  try
    with cboDetectApp.Items do
    begin
      BeginUpdate;
      Clear;
      AddObject(lngscBuiltin, TObject(PtrInt(daNative)));
      AddObject('FFmpeg', TObject(PtrInt(daFFmpeg)));
      if FileExists(SceneDetectFileName) then AddObject('PySceneDetect', TObject(PtrInt(daSceneDetect)));
      EndUpdate;
    end;
    with cboTimeCodeImport.Items do
    begin
      BeginUpdate;
      Clear;
      Add(lngscFrames);
      Add(lngscSeconds);
      Add(lngscMilliseconds);
      Add(lngscHHMMSSZZZ);
      Add(lngscHHMMSSFF);
      EndUpdate;
    end;
    cboTimeCodeExport.Items.Assign(cboTimeCodeImport.Items);
    cboTimeCodeImport.ItemIndex := 2;
    cboTimeCodeExport.ItemIndex := 4;
    SetDetectApp(daNative);
  finally
  end;

  FCancel   := False;
  FSMPTE    := False;
  FFromWave := False;
  FDetector := NIL;
  FFillingMemo := False;

  btnDetect.Enabled := frmMain.MPV.IsMediaLoaded;

  with frmMain.WAVE do
    if Length(GetSceneChangeList) > 0 then
    begin
      mmoTimes.Lines.BeginUpdate;
      try
        for i := 0 to High(GetSceneChangeList) do
          mmoTimes.Lines.Add(GetSceneChangeList[i].ToString);
      finally
        mmoTimes.Lines.EndUpdate;
        mmoTimesChange(NIL);
      end;
      FFromWave := True;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveFormSettings(Self, Format('%d,%d,%d', [GetDetectApp, cboTimeCodeImport.ItemIndex, cboTimeCodeExport.ItemIndex]));
  FreeAndNil(FDetector);
  CloseAction := caFree;
  frmShotChanges := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // detecting: cancel it first (the detection loop is running in this form)
  CanClose := btnClose.Tag = 0;
  if not CanClose then
    FCancel := True;
end;

// -----------------------------------------------------------------------------

function TfrmShotChanges.GetDetectApp: Integer;
begin
  if cboDetectApp.ItemIndex >= 0 then
    Result := PtrInt(cboDetectApp.Items.Objects[cboDetectApp.ItemIndex])
  else
    Result := daNative;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.SetDetectApp(const AApp: Integer);
var
  i: Integer;
begin
  for i := 0 to cboDetectApp.Items.Count-1 do
    if PtrInt(cboDetectApp.Items.Objects[i]) = AApp then
    begin
      cboDetectApp.ItemIndex := i;
      Exit;
    end;
  // not available (PySceneDetect no longer installed)
  cboDetectApp.ItemIndex := 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.FormShow(Sender: TObject);
var
  s: String;
  AParamArray: TStringArray;
begin
  CheckColorTheme(Self);
  s := LoadFormSettings(Self);
  if not s.IsEmpty then
  begin
    AParamArray := s.Split(',');
    if Length(AParamArray) = 3 then
    begin
      SetDetectApp(StrToIntDef(AParamArray[0], daNative));
      cboTimeCodeImport.ItemIndex := AParamArray[1].ToInteger;
      cboTimeCodeExport.ItemIndex := AParamArray[2].ToInteger;
    end;
  end;
  // the times of the timeline are in ms, whatever was used the last time
  if FFromWave then
    cboTimeCodeImport.ItemIndex := tcMilliseconds;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.mmoTimesChange(Sender: TObject);
begin
  // edited, imported or detected with another app: the scores of the last
  // native detection are not the list anymore
  // (Sender = NIL: only refreshing the buttons)
  if (Sender <> NIL) and not FFillingMemo and Assigned(FDetector) and not FDetector.IsRunning then
    FreeAndNil(FDetector);
  btnExport.Enabled := (mmoTimes.Lines.Count > 0);
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.btnCloseClick(Sender: TObject);
begin
  if btnClose.Tag = 0 then
    Close
  else
    FCancel := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.btnApplyClick(Sender: TObject);
var
  i, v : Integer;
  s : String;
  SC : TIntegerDynArray;
  Cache : TStringList;
begin
  if mmoTimes.Lines.Count > 0 then
  begin
    for i := mmoTimes.Lines.Count-1 downto 0 do
      if not LineToMs(mmoTimes.Lines[i], v) then
        mmoTimes.Lines.Delete(i);

    SC := NIL;
    SetLength(SC, mmoTimes.Lines.Count);
    // the cache is always in ms (it is loaded with WAVE.LoadSceneChangeFile),
    // whatever the format of the times in the memo, with the offset applied
    Cache := TStringList.Create;
    try
      if FSMPTE then
        Cache.Add('-1');

      for i := 0 to mmoTimes.Lines.Count-1 do
      begin
        LineToMs(mmoTimes.Lines[i], v);

        if tedOffset.Value > 0 then
          v := v + iff(cboOffset.ItemIndex = 0, tedOffset.Value, -tedOffset.Value);

        Cache.Add(v.ToString);

        if FSMPTE and (Workspace.WorkMode = wmTime) then
          v := Round(v * 1.001);

        SC[i] := v;
      end;

      // saved to load them again with the video (only a cache: an error here
      // must not stop applying them)
      if frmMain.MPV.FileName <> '' then
      try
        s := ShotChangesFileName(frmMain.MPV.FileName, '.shotchanges');
        ForceDirectories(ExtractFileDir(s));
        Cache.SaveToFile(s);
      except
      end;

      with frmMain.WAVE do
      begin
        SetSceneChangeList(SC);
        //SaveSceneChangeFile(s);
      end;
    finally
      SetLength(SC, 0);
      Cache.Free;
    end;

    Close;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.btnImportClick(Sender: TObject);
var
  i   : Integer;
  edl : TEDL;
  ATracks: TStrings = NIL;
  Tracks, TrackNames: TStringList;
  Selected, s: String;
  c, x, h, Base: Integer;
  UseSource: Boolean;
  Times: TIntegerDynArray;

  procedure AddTime(const AValue: Integer);
  begin
    SetLength(Times, Length(Times)+1);
    Times[High(Times)] := AValue;
  end;

begin
  with TOpenDialog.Create(Self) do
  try
    Title   := lngOpenFile;
    Filter  := FillDialogFilter(True);
    Options := Options + [ofFileMustExist];

    if Execute then
    begin
      if FileName.ToLower.EndsWith('.edl') or FileName.ToLower.EndsWith('.xml') then
      begin
        edl := TEDL.Create('', '', GetFPS, FormatSettings, TFCM(Integer(Workspace.FPS.InputFPS.Mode)));
        try
          if edl.GetXMLTrackList(FileName, ATracks) and (ATracks.Count > 1) then
            edl.ID := formCustomSelectDlg.ExecuteDialog(lngMultipleTracksDetected, lngSelectTrackToUse, ATracks);

          edl.LoadFromFile(FileName);
          mmoTimes.Lines.BeginUpdate;
          try
            mmoTimes.Clear;
            // edl tracks (V, A, A2, AA, AA/V, B...), video ones first. With
            // more than one, ask which to use (e.g. Final Cut Pro 7 can export
            // all the cuts in the audio track)
            Selected := '';
            TrackNames := TStringList.Create;
            Tracks := TStringList.Create;
            try
              for x := 0 to 1 do
                for i := 0 to edl.Items.Count-1 do
                  with edl.Items[i]^ do
                    if not IsEDLEventWithoutTimecode(edl.Items[i]) and
                      (TrackName <> '') and (TrackNames.IndexOf(TrackName) < 0) and
                      ((x = 0) = ((Pos('V', TrackName) > 0) or (TrackName = 'B'))) then
                      TrackNames.Add(TrackName);

              if TrackNames.Count = 1 then
                Selected := TrackNames[0]
              else if TrackNames.Count > 1 then
              begin
                for x := 0 to TrackNames.Count-1 do
                begin
                  c := 0;
                  for i := 0 to edl.Items.Count-1 do
                    if (edl.Items[i]^.TrackName = TrackNames[x]) and
                      not IsEDLEventWithoutTimecode(edl.Items[i]) then Inc(c);
                  Tracks.Add(Format('%s (%d)', [TrackNames[x], c]));
                end;
                x := formCustomSelectDlg.ExecuteDialog(lngMultipleTracksDetected, lngSelectTrackToUse, Tracks);
                if (x < 0) or (x >= TrackNames.Count) then x := 0;
                Selected := TrackNames[x];
              end;
            finally
              Tracks.Free;
              TrackNames.Free;
            end;

            // the video is the edited program: use the sequence (record)
            // times. Some exporters write broken record times (e.g. Shutter
            // Encoder drops the minutes: 00:00:57:23 -> 00:00:00:01), if they
            // go backwards use the clip (source) times
            UseSource := False;
            c := -1;
            for i := 0 to edl.Items.Count-1 do
              with edl.Items[i]^ do
                if ((Selected = '') or (TrackName = Selected)) and not IsEDLEventWithoutTimecode(edl.Items[i]) then
                begin
                  if (TapeOut < TapeIn) or (Integer(TapeIn) < c) then
                  begin
                    UseSource := True;
                    Break;
                  end;
                  c := TapeIn;
                end;

            // xml: only the clip times
            if FileName.ToLower.EndsWith('.xml') then
              UseSource := True;

            // every edit point (in and out)
            Times := NIL;
            for i := 0 to edl.Items.Count-1 do
              with edl.Items[i]^ do
                if ((Selected = '') or (TrackName = Selected)) and not IsEDLEventWithoutTimecode(edl.Items[i]) then
                begin
                  if UseSource then
                  begin
                    AddTime(ClipIn);
                    AddTime(ClipOut);
                  end
                  else
                  begin
                    AddTime(TapeIn);
                    AddTime(TapeOut);
                  end;
                end;

            if Length(Times) > 0 then
            begin
              // the timecodes usually start at 01:00:00:00: subtract the
              // hour of the first one
              c := MaxInt;
              for i := 0 to High(Times) do
                if Times[i] < c then c := Times[i];

              Base := 0;
              for h := 23 downto 1 do
                if SMPTEStringToMS(Format('%.2d:00:00:00', [h]), GetFPS, edl.Header.FCM = fcmDROPFRAME) <= c then
                begin
                  Base := SMPTEStringToMS(Format('%.2d:00:00:00', [h]), GetFPS, edl.Header.FCM = fcmDROPFRAME);
                  Break;
                end;

              // subtract and sort (insertion, the edl is almost sorted)
              for i := 0 to High(Times) do
              begin
                x := Times[i] - Base;
                c := i - 1;
                while (c >= 0) and (Times[c] > x) do
                begin
                  Times[c+1] := Times[c];
                  Dec(c);
                end;
                Times[c+1] := x;
              end;

              for i := 0 to High(Times) do
                // skip the start of the video and repeated cuts (same cut in
                // several events/tracks)
                if (Times[i] > 0) and ((i = 0) or (Times[i] <> Times[i-1])) then
                  mmoTimes.Lines.Add(Times[i].ToString);
            end;
          finally
            mmoTimes.Lines.EndUpdate;
          end;
          cboTimeCodeImport.ItemIndex := tcMilliseconds;
        finally
          // the edl times are already real times (SMPTEStringToMS uses the
          // real fps), they don't need the 1.001 correction when applying
          FSMPTE := False;
          edl.Free;
          if Assigned(ATracks) then ATracks.Free;
        end;
      end
      else
      begin
        mmoTimes.Lines.LoadFromFile(FileName);
        FFromWave := False;
        FSMPTE := False;
        // "-1": saved by Apply with SMPTE times (see btnApplyClick)
        if (mmoTimes.Lines.Count > 0) and (Trim(mmoTimes.Lines[0]) = '-1') then
        begin
          FSMPTE := True;
          mmoTimes.Lines.Delete(0);
        end;

        // the format of the times (ms, seconds, hh:mm:ss.zzz, hh:mm:ss:ff)
        for i := 0 to mmoTimes.Lines.Count-1 do
          if Trim(mmoTimes.Lines[i]) <> '' then
          begin
            c := DetectTimeCode(mmoTimes.Lines[i]);
            if c >= 0 then
              cboTimeCodeImport.ItemIndex := c;
            Break;
          end;

        // only integers: frames or milliseconds? With the media loaded, the
        // last cut read as frames must be inside the video, if not they are
        // milliseconds (a list of cuts read as ms when they are frames ends
        // at 1/24 of the video). Without media, the selected one.
        if (IsTimeCode(tcFrames) or IsTimeCode(tcMilliseconds)) and frmMain.MPV.IsMediaLoaded and
          (frmMain.MPV.GetMediaLenInMs > 0) then
        begin
          x := 0;
          c := 0;
          for i := 0 to mmoTimes.Lines.Count-1 do
            if Trim(mmoTimes.Lines[i]) <> '' then
            begin
              if not IsInteger(Trim(mmoTimes.Lines[i])) then
              begin
                c := -1;
                Break;
              end;
              if StrToIntDef(Trim(mmoTimes.Lines[i]), 0) > x then x := StrToIntDef(Trim(mmoTimes.Lines[i]), 0);
            end;

          if (c = 0) and (x > 0) then
          begin
            if FramesToTime(x, GetFPS) <= frmMain.MPV.GetMediaLenInMs + 2000 then
              cboTimeCodeImport.ItemIndex := tcFrames
            else
              cboTimeCodeImport.ItemIndex := tcMilliseconds;
          end;
        end;
      end;
    end;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.btnExportClick(Sender: TObject);
var
  i, x, v, c : Integer;
  s    : String;
  edl  : TEDL;
  sl   : TStringList;
begin
  if mmoTimes.Lines.Count > 0 then
    with TSaveDialog.Create(frmMain) do
    try
      Title    := lngSaveFile;
      Filter   := FillDialogFilter(False);
      FileName := '';
      Options  := [ofOverwritePrompt, ofEnableSizing];
      if Execute then
      begin
        if FileName.ToLower.EndsWith('.edl') or FileName.ToLower.EndsWith('.xml') then
        begin
          edl := TEDL.Create('', frmMain.MPV.FileName, GetFPS, FormatSettings);
          try
            x := 0;
            c := 0;
            for i := 0 to mmoTimes.Lines.Count-1 do
              if LineToMs(mmoTimes.Lines[i], v) and (v > x) then
              begin
                Inc(c);
                edl.AddItem(c, '',  totV, tocC, x, v, 0, 0, '', '');
                x := v;
              end;
            edl.SaveToFile(FileName);
          finally
            edl.Free;
          end;
        end
        else
        begin
          //mmoTimes.Lines.SaveToFile(FileName);
          sl := TStringList.Create;
          try
            for i := 0 to mmoTimes.Lines.Count-1 do
            begin
              if not LineToMs(mmoTimes.Lines[i], x) then Continue;

              if IsTimeCodeOut(tcFrames) then
                s := TimeToFrames(x, GetFPS).ToString
              else if IsTimeCodeOut(tcSeconds) then
                s := MsToSeconds(x)
              else if IsTimeCodeOut(tcMilliseconds) then
                s := x.ToString
              else if IsTimeCodeOut(tcHHMMSSFF) then
                s := MSToHHMMSSFFTime(x, GetFPS) // SMPTE (counting frames)
              else
                s := TimeToString(x, 'hh:mm:ss.zzz');

              sl.Add(s);
            end;
            sl.SaveToFile(FileName);
          finally
            sl.Free;
          end;
        end;
      end;
    finally
      Free;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.SetControlsEnabled(const AValue: Boolean);
begin
  lblIdle.Caption := '';
  prbExtracting.Visible := not AValue;
  lblIdle.Visible := not AValue;
  mmoTimes.Enabled := AValue;
  btnImport.Enabled := AValue;
  btnDetect.Enabled := AValue;
  cboDetectApp.Enabled := AValue;
  cboTimeCodeImport.Enabled := AValue;
  cboTimeCodeExport.Enabled := AValue;
  btnApply.Enabled := AValue;
  spnSensitivity.Enabled := AValue;
  cboOffset.Enabled := AValue;
  tedOffset.Enabled := AValue;

  if AValue then
  begin
    btnClose.Caption := lngbtnClose;
    btnClose.Tag := 0;
    mmoTimesChange(NIL);
  end
  else
  begin
    btnClose.Caption := lngbtnCancel;
    btnClose.Tag := 1;
    btnExport.Enabled := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure RunAppTime_CB(const TimeElapsed: Double; var Cancel: Boolean);
begin
  frmShotChanges.lblIdle.Caption := TimeToString(Trunc(TimeElapsed)*1000, 'mm:ss');
  Cancel := frmShotChanges.FCancel;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.btnDetectClick(Sender: TObject);
const
  sc_timeid = 'pts_time:';

var
  AParamArray: TStringArray;
  i, x, sc: Integer;
  sl: TStringList = NIL;
  outfile: String;
begin
  if frmMain.MPV.FileName.StartsWith('http') then
  begin
    ShowErrorMessageDialog(lngFeatureNotAvailableFromURL);
    Exit;
  end;

  FCancel := False;
  SetControlsEnabled(False);
  // the detected times are ms: with "Frames" left selected from a previous
  // import they were read as frames (exported 2.669 s as 00:01:51:05)
  cboTimeCodeImport.ItemIndex := tcMilliseconds;
  FSMPTE := False;
  FFromWave := False;
  try
    if GetDetectApp = daNative then
      DetectNative
    else if GetDetectApp = daFFmpeg then
    begin
      if Tools.FFmpeg_ParamsForShotChanges <> '' then
        AParamArray := Tools.FFmpeg_ParamsForShotChanges.Split(' ')
      else
        AParamArray := FFMPEG_SCParams.Split(' ');
      for i := 0 to High(AParamArray) do
        AParamArray[i] := StringsReplace(AParamArray[i], ['%input', '%value'], [frmMain.MPV.FileName, StringReplace(FloatToStr(spnSensitivity.Value, FormatSettings), FormatSettings.DecimalSeparator, '.', [])], []);

      if not FileExists(GetExtractAppFile) then
      begin
        ShowErrorMessageDialog(Format(lngExtractAppError, [FFMPEG_EXE]))
      end
      else
      begin
        FTimeElapsed := 0;

        if ExecuteAppLoop(GetExtractAppFile, AParamArray, sl, @RunApp_CB) and (sl.Count > 0) then
        try
          //writeln(sl.Text);
          mmoTimes.Lines.BeginUpdate;
          mmoTimes.Clear;
          for i := 0 to sl.Count-1 do
          begin
            x := Pos(sc_timeid, sl[i]);
            if x > 0 then
            begin
              sc := Round(StrToSingle(Copy(sl[i], x+sc_timeid.Length, Pos(' ', sl[i], x+1)-x-sc_timeid.Length).Replace('.', FormatSettings.DecimalSeparator), 0, FormatSettings)*1000);
              if sc > 0 then
                mmoTimes.Lines.Add(IntToStr(sc));
            end;
          end;
        finally
          mmoTimes.Lines.EndUpdate;
          if Assigned(sl) then sl.Free;
        end;
      end;
    end
    else // PySceneDetect
    begin
      outfile := ChangeFileExt(GetTempFileName, '.csv');
      if Tools.PySceneDetect_Params <> '' then
        AParamArray := Tools.PySceneDetect_Params.Split(' ')
      else
        AParamArray := SCENEDETECT_SCParams.Split(' ');
      for i := 0 to High(AParamArray) do
        AParamArray[i] := StringsReplace(AParamArray[i], ['%input', '%output', '%value'], [frmMain.MPV.FileName, outfile, SceneDetectThreshold(spnSensitivity.Value)], []);

      if not FileExists(GetExtractAppFile(False)) then
      begin
        ShowErrorMessageDialog(Format(lngExtractAppError, [SCENEDETECT_EXE]))
      end
      else
      begin
        FTimeElapsed := 0;

        if not ExecuteApp(GetExtractAppFile(False), AParamArray, True, True, @RunAppTime_CB) then
          ShowErrorMessageDialog(Format(lngErrorExecuting, [GetExtractAppFile(False)]))
        else if FileExists(outfile) then
        begin
          sl := TStringList.Create;
          try
            sl.LoadFromFile(outfile);
            mmoTimes.Lines.BeginUpdate;
            mmoTimes.Clear;
            ParseSceneDetectCSV(sl, mmoTimes.Lines);
          finally
            mmoTimes.Lines.EndUpdate;
            sl.Free;
            sl := NIL;
            DeleteFile(outfile);
          end;
        end
        else if not FCancel then
          // it ran but wrote nothing: usually no video backend (pip install
          // scenedetect without opencv-python) or a video it can't open
          ShowErrorMessageDialog(Format(lngSceneDetectNoResults, [SCENEDETECT_EXE]));
      end;
    end;
  finally
    SetControlsEnabled(True);
  end;
end;

// -----------------------------------------------------------------------------

// cuts, dissolves and fades with a second libmpv in its own thread
// (UW_MPVPlayer), frame exact and without external apps

// the sensitivity is FFmpeg's scene score (0.1..0.9, 0.4 by default, higher
// is less sensitive). Here: the minimum gray difference of a cut (0..255) and
// how many times it must be over the frames around it. 0.4 = 8 and 3, the
// defaults (the best F1 in the UW_MPVPlayer benchmark), 0.1 = 2 and 1.5,
// 0.9 = 18 and 5.5

procedure SensitivityToParams(const AValue: Double; out AMinScore, ARatio: Double);
begin
  AMinScore := AValue * 20;
  ARatio    := 1 + AValue * 5;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.FillNativeCuts;
var
  cuts: TIntegerDynArray;
  i: Integer;
begin
  cuts := FDetector.GetCutsMs;
  FFillingMemo := True;
  mmoTimes.Lines.BeginUpdate;
  try
    mmoTimes.Clear;
    for i := 0 to High(cuts) do
      if cuts[i] > 0 then
        mmoTimes.Lines.Add(IntToStr(cuts[i]));
  finally
    mmoTimes.Lines.EndUpdate;
    FFillingMemo := False;
  end;
  mmoTimesChange(NIL);
end;

// -----------------------------------------------------------------------------

// after a native detection the cuts are decided again from its scores, at
// once, without reading the video again

procedure TfrmShotChanges.spnSensitivityChange(Sender: TObject);
var
  ms, r: Double;
begin
  if not Assigned(FDetector) or FDetector.IsRunning or not FDetector.HasScores then Exit;
  SensitivityToParams(spnSensitivity.Value, ms, r);
  if FDetector.Recalculate(ms, r) then
    FillNativeCuts;
end;

// -----------------------------------------------------------------------------

// cuts, dissolves and fades with a second libmpv in its own thread
// (UW_MPVPlayer), frame exact and without external apps

procedure TfrmShotChanges.DetectNative;
var
  t0: QWord;
  ms, r: Double;
begin
  FreeAndNil(FDetector);
  FDetector := TMPVSceneDetector.Create(frmMain.MPV.MPVFileName);
  SensitivityToParams(spnSensitivity.Value, ms, r);
  FDetector.MinScore := ms;
  FDetector.Ratio    := r;

  if not FDetector.Start(frmMain.MPV.FileName) then
  begin
    FreeAndNil(FDetector);
    ShowErrorMessageDialog(Format(lngErrorExecuting, ['libmpv']));
    Exit;
  end;

  prbExtracting.Style    := pbstNormal;
  prbExtracting.Max      := 1000;
  prbExtracting.Position := 0;
  t0 := GetTickCount64;
  try
    while FDetector.IsRunning do
    begin
      prbExtracting.Position := Round(FDetector.GetProgress * 1000);
      lblIdle.Caption := TimeToString(((GetTickCount64 - t0) div 1000) * 1000, 'mm:ss') +
        Format('  %d%%', [Round(FDetector.GetProgress * 100)]);
      Application.ProcessMessages;
      if FCancel then
        FDetector.Cancel;
      Sleep(30);
    end;
    FDetector.WaitFor;
  finally
    prbExtracting.Style := pbstMarquee;
  end;

  case FDetector.Status of
    sdsDone:
      FillNativeCuts; // keeps the detector: its scores for the sensitivity
    sdsError:
      begin
        ShowErrorMessageDialog(Format(lngErrorExecuting, ['libmpv']) + sLineBreak + FDetector.ErrorText);
        FreeAndNil(FDetector);
      end;
  else
    FreeAndNil(FDetector);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.RunApp_CB(Sender, Context: TObject; Status: TRunCommandEventCode; const Message: String);
begin
  lblIdle.Caption := TimeToString(Trunc(FTimeElapsed)*1000, 'mm:ss');
  Sleep(100);
  FTimeElapsed += 0.1;

  if FCancel then
    TProcess(Sender).Terminate(-1);

  Application.ProcessMessages;
end;

// -----------------------------------------------------------------------------

function TfrmShotChanges.IsTimeCode(const ACode: Integer): Boolean;
begin
  Result := cboTimeCodeImport.ItemIndex = ACode;
end;

// -----------------------------------------------------------------------------

function TfrmShotChanges.IsTimeCodeOut(const ACode: Integer): Boolean;
begin
  Result := cboTimeCodeExport.ItemIndex = ACode;
end;

// -----------------------------------------------------------------------------

function TfrmShotChanges.DetectTimeCode(const ALine: String): Integer;
var
  s: String;
  v: Integer;
begin
  // the format of a time by its shape (-1 unknown). An integer can be ms or
  // frames: it keeps the selected one if it is one of them
  s := Trim(ALine);
  if (Length(s.Split(':')) = 4) or (Length(s.Split([':', ';'])) = 4) then
    Result := tcHHMMSSFF
  else if (Pos(':', s) > 0) and TimeInFormat(s, 'hh:mm:ss.zzz') then
    Result := tcHHMMSSZZZ
  else if IsInteger(s) then
  begin
    if IsTimeCode(tcFrames) then
      Result := tcFrames
    else
      Result := tcMilliseconds;
  end
  else if (Pos(':', s) = 0) and SecondsToMs(s, v) then
    Result := tcSeconds
  else
    Result := -1;
end;

// -----------------------------------------------------------------------------

function TfrmShotChanges.LineToMs(const ALine: String; out AMs: Integer): Boolean;
var
  s: String;
begin
  // the time in a line, in the selected format or the one it has (e.g. the
  // times of the timeline are always ms, an imported file can have another)
  s := Trim(ALine);
  AMs := 0;
  Result := True;
  case DetectTimeCode(s) of
    tcFrames       : AMs := FramesToTime(StrToIntDef(s, 0), GetFPS);
    tcMilliseconds : AMs := StrToIntDef(s, 0);
    tcSeconds      : Result := SecondsToMs(s, AMs);
    tcHHMMSSZZZ    : AMs := StringToTime(s);
    tcHHMMSSFF     : AMs := SMPTEStringToMS(s, GetFPS); // SMPTE (counting frames)
  else
    Result := False;
  end;
  Result := Result and (AMs >= 0);
end;

// -----------------------------------------------------------------------------

function TfrmShotChanges.FillDialogFilter(const AAllSupportedFiles: Boolean): String;

  function GetLocalizeStr(const AID: Integer): String;
  begin
    case AID of
      0: Result := lngscEDL;
      1: Result := lngscShotChanges;
      2: Result := lngscXML;
    end;
  end;

var
  i: Integer;
  s: String;
begin
  Result := '';
  s := '';

  for i := 0 to Length(TShotChangeExts)-1 do
  begin
    Result := Result + GetLocalizeStr(i) + ' (*' + TShotChangeExts[i] + ')|*' + TShotChangeExts[i] + '|';
    s := s + '*' + TShotChangeExts[i] + ';';
  end;

  if AAllSupportedFiles then
  begin
    System.Delete(s, Length(s), 1);
    Result := lngAllSupportedFiles + ' (' + s + ')|' + s + '|' + Result;
  end;
end;

// -----------------------------------------------------------------------------

end.

