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

unit formVideoDubbing;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  laz.VirtualTrees, fgl, LCLIntf, LCLType, UWCheckBox, LCLTranslator, Spin,
  UWSystem.TextToSpeech;

type

  { TfrmVideoDubbing }

  TfrmVideoDubbing = class(TForm)
    btnClose: TButton;
    btnBackgroundMusic: TButton;
    btnGenerate: TButton;
    cboVoice: TComboBox;
    chkBackgroundMusic: TUWCheckBox;
    edtBackgroundMusic: TEdit;
    lblTime: TLabel;
    lblStatus: TLabel;
    lblStability: TLabel;
    lblTimeElapsed: TLabel;
    lblVoice: TLabel;
    lblSimilarityBoost: TLabel;
    spnSimilarityBoost: TFloatSpinEdit;
    spnStability: TFloatSpinEdit;
    chkGlobalVoice: TUWCheckBox;
    chkSubtitleTrack: TUWCheckBox;
    VST: TLazVirtualStringTree;
    procedure btnBackgroundMusicClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure cboVoiceChange(Sender: TObject);
    procedure chkBackgroundMusicChange(Sender: TObject);
    procedure chkGlobalVoiceChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VSTAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTHeaderDrawQueryElements(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure VSTResize(Sender: TObject);
  private
    TTS: TTextToSpeech;
    TempDir: String;
    MusicFileName: String;
    OutputFileName: String;
    procedure EnableControls(const AValue: Boolean);
    procedure DoError(Sender: TObject; const ErrorDesc: String; const ErrorID, CurrentJob: Integer; var Cancel: Boolean);
    procedure DoProgress(Sender: TObject; const Index, Total: Integer);
    procedure DoTerminate(Sender: TObject);
    procedure FillComboWithVoices;
    procedure OpenFolderClick(Sender: TObject);
    procedure SetJobTTS;
    function GenerateSubtitleFile(const AFileName: String): Boolean;
    function GenerateAudioFile: Boolean;
    procedure GenerateVideoFile;
  public

  end;

var
  frmVideoDubbing: TfrmVideoDubbing;
  CancelJobs: Boolean;

// -----------------------------------------------------------------------------

implementation

uses
  UWSubtitleAPI, procVST, procSubtitle, procTypes, RegExpr, procWorkspace,
  procColorTheme, procDialogs, procLocalize, UWSystem.InetUtils, FileUtil,
  UWSubtitleAPI.Tags, UWSystem.Process, UWSystem.TimeUtils, procConfig,
  formMain, UWSystem.SysUtils, UWFiles.MPEGAudio, UWSubtitleAPI.Formats,
  StrUtils;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmVideoDubbing }

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.FormCreate(Sender: TObject);
begin
  VSTAddColumn(VST, '#', 50);
  VSTAddColumn(VST, lngvdVoice, 70);
  VSTAddColumn(VST, lnghTimes, 90);
  VSTAddColumn(VST, lnghText, 100, taLeftJustify);

  TTS := TTextToSpeech.Create(Tools.API_KEY_TTS);
  TTS.OnJobsError := @DoError;
  TTS.OnJobsProgress := @DoProgress;
  TTS.OnTerminate := @DoTerminate;

  TempDir := ConcatPaths([GetTempDir, 'tsdubtmp']);
  if not DirectoryExists(TempDir) then
    CreateDir(TempDir);

  FillComboWithVoices;
  SetJobTTS;

  VST.RootNodeCount := Subtitles.Count;
  chkGlobalVoice.Checked := True;
  chkGlobalVoiceChange(NIL);
  chkBackgroundMusicChange(NIL);

  btnGenerate.Enabled := (TTS.api_key <> '') and frmMain.MPV.HasVideoTrack;

  {$IFNDEF WINDOWS}
  PrepareCustomControls(Self);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if DirectoryExists(TempDir) and DeleteDirectory(TempDir, True) then
    RemoveDir(TempDir);

  TTS.Free;
  CloseAction := caFree;
  frmVideoDubbing := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.btnCloseClick(Sender: TObject);
begin
  if cboVoice.Enabled then
    Close
  else
    CancelJobs := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.cboVoiceChange(Sender: TObject);
begin
  if Assigned(VST.FocusedNode) and (cboVoice.Tag = 0) then
  begin
    with TTS.Jobs[VST.FocusedNode^.Index]^ do
    begin
      VoiceID := TTS.Voices[cboVoice.ItemIndex]^.ID;
      VoiceIndex := cboVoice.ItemIndex;
      Similarity := spnSimilarityBoost.Value;
      Stability := spnStability.Value;
    end;
    if not chkGlobalVoice.Checked then
      VST.InvalidateNode(VST.FocusedNode);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.chkBackgroundMusicChange(Sender: TObject);
begin
  edtBackgroundMusic.Enabled := chkBackgroundMusic.Checked;
  btnBackgroundMusic.Enabled := chkBackgroundMusic.Checked;
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.chkGlobalVoiceChange(Sender: TObject);
begin
  VST.Enabled := not chkGlobalVoice.Checked;
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.VSTAdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
begin
  if (hpeBackground in Elements) then
  begin
    PaintInfo.TargetCanvas.Brush.Color := ColorThemeInstance.Colors.Window;

    if Assigned(PaintInfo.Column) then
      DrawFrameControl(PaintInfo.TargetCanvas.Handle, PaintInfo.PaintRectangle, DFC_BUTTON, DFCS_FLAT or DFCS_ADJUSTRECT);

    PaintInfo.TargetCanvas.FillRect(PaintInfo.PaintRectangle);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.VSTHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if ColorThemeInstance.GetRealColorMode = cmDark then
    Elements := [hpeBackground];
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  cboVoice.Tag := 1;
  cboVoice.ItemIndex := TTS.Jobs[Node^.Index]^.VoiceIndex;
  spnSimilarityBoost.Value := TTS.Jobs[Node^.Index]^.Similarity;
  spnStability.Value := TTS.Jobs[Node^.Index]^.Stability;
  cboVoice.Tag := 0
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  case Column of
    0: CellText := IntToStr(Node^.Index+1);
    1: CellText := iff((TTS.Voices.Count > 0) and (TTS.Jobs.Count > 0), TTS.Voices[TTS.Jobs[Node^.Index]^.VoiceIndex]^.Name, '');
    2: CellText := GetInitialTimeStr(Node^.Index);
    3: CellText := Subtitles[Node^.Index].Text;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.VSTResize(Sender: TObject);
begin
  with VST.Header do
    Columns[3].Width := (VST.Width - Columns[2].Width - Columns[1].Width - Columns[0].Width - (GetSystemMetrics(SM_CXVSCROLL)+Columns.Count));
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.btnGenerateClick(Sender: TObject);
var
  SD : TSaveDialog;
  i : Integer;
begin
  SD := TSaveDialog.Create(NIL);
  try
    SD.Title   := lngSaveFile;
    SD.Options := [ofOverwritePrompt, ofEnableSizing];
    SD.Filter  := lngAllSupportedFiles + '|' + '*' + ExtractFileExt(frmMain.MPV.FileName);
    SD.FileName := 'dubbing_' + ExtractFileName(frmMain.MPV.FileName);

    if SD.Execute then
      OutputFileName := SD.FileName
    else
      Exit;
  finally
    SD.Free;
  end;

  if IsInternetAlive then
  begin
    CancelJobs := False;
    EnableControls(False);

    lblStatus.Caption := lngvdGeneratingTTS;
    lblStatus.Update;

    if TTS.Finished then
    begin
      Application.ProcessMessages;
      TTS.Free;
      TTS := TTextToSpeech.Create(Tools.API_KEY_TTS);
      TTS.OnJobsError := @DoError;
      TTS.OnJobsProgress := @DoProgress;
      TTS.OnTerminate := @DoTerminate;
      TTS.GetVoices;
    end;

    if chkGlobalVoice.Checked then
      for i := 0 to TTS.Jobs.Count-1 do
      begin
        TTS.Jobs[i]^.VoiceID := TTS.Voices[cboVoice.ItemIndex]^.ID;
        TTS.Jobs[i]^.VoiceIndex := cboVoice.ItemIndex;
        TTS.Jobs[i]^.Similarity := spnSimilarityBoost.Value;
        TTS.Jobs[i]^.Stability := spnStability.Value;
      end;

    TTS.DoJobs;
  end
  else
    ShowErrorMessageDialog(lngNoInternetConnection);
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.btnBackgroundMusicClick(Sender: TObject);
var
  OD : TOpenDialog;
  i  : Integer;
  s  : String;
begin
  OD := TOpenDialog.Create(NIL);
  try
    s := '';
    for i := 0 to Length(TAudioExts)-1 do s := s + '*' + TAudioExts[i] + ';';
    SetLength(s, s.Length-1);

    OD.Title   := lngOpenFile;
    OD.Filter  := lngAllSupportedFiles + '|' + s;
    OD.Options := OD.Options + [ofFileMustExist];

    if OD.Execute then
      edtBackgroundMusic.Text := OD.FileName;
  finally
    OD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.EnableControls(const AValue: Boolean);
begin
  cboVoice.Enabled           := AValue;
  btnGenerate.Enabled        := AValue;
  spnSimilarityBoost.Enabled := AValue;
  spnStability.Enabled       := AValue;
  chkGlobalVoice.Enabled     := AValue;
  chkSubtitleTrack.Enabled   := AValue;
  chkBackgroundMusic.Enabled := AValue;

  if AValue then
  begin
    btnClose.Caption := lngbtnClose;
    chkGlobalVoiceChange(NIL);
    chkBackgroundMusicChange(NIL);
  end
  else
  begin
    btnClose.Caption := lngbtnCancel;
    VST.Enabled := False;
    edtBackgroundMusic.Enabled := False;
    btnBackgroundMusic.Enabled := False;
  end;

  lblStatus.Caption := '';
  lblTimeElapsed.Caption := '';
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.DoError(Sender: TObject; const ErrorDesc: String; const ErrorID, CurrentJob: Integer; var Cancel: Boolean);
begin
  CancelJobs := True;
  if (ErrorID = JOB_ERROR_QuotaExceeded) then
    ShowErrorMessageDialog(lngvdQuotaExceeded)
  else
    ShowErrorMessageDialog(Format(lngOperationFailed, [CurrentJob, TTS.Jobs.Count, ErrorID]));
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.DoProgress(Sender: TObject; const Index, Total: Integer);
begin
  lblTimeElapsed.Caption := Format('%d/%d', [Index, Total]);
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.DoTerminate(Sender: TObject);
begin
  try
    if CancelJobs then
      ShowErrorMessageDialog(lngOperationCancelled)
    else if not cboVoice.Enabled then
      GenerateVideoFile;
  finally
    EnableControls(True);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.FillComboWithVoices;
var
  i: Integer;
begin
  with cboVoice do
  begin
    Items.BeginUpdate;
    Clear;

    if TTS.GetVoices then
      for i := 0 to TTS.Voices.Count-1 do
        Items.Add(TTS.Voices[i]^.Name);

    if Items.Count > 0 then
      ItemIndex := 0;

    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.OpenFolderClick(Sender: TObject);
begin
  OpenDocument(ExtractFileDir(OutputFileName));
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.SetJobTTS;
var
  job : PJobInfo;
  i : Integer;
begin
  for i := 0 to Subtitles.Count-1 do
  begin
    New(job);
    job^.Text := RemoveTSTags(Subtitles[i].Text);
    job^.FileName := ConcatPaths([TempDir, IntToStr(i+1) + '.mp3']);
    if (TTS.Jobs.Count > 0) and (cboVoice.ItemIndex >= 0) then
      job^.VoiceID := TTS.Voices[cboVoice.ItemIndex]^.ID;
    job^.VoiceIndex := cboVoice.ItemIndex;
    job^.Similarity := spnSimilarityBoost.Value;
    job^.Stability := spnStability.Value;
    TTS.Jobs.Add(job);
  end;
end;

// -----------------------------------------------------------------------------

procedure ProcessCB(const TimeElapsed: Double; var Cancel: Boolean);
begin
  frmVideoDubbing.lblTimeElapsed.Caption := TimeToString(Trunc(TimeElapsed)*1000, 'mm:ss');
  Cancel := CancelJobs;
end;

// -----------------------------------------------------------------------------

function TfrmVideoDubbing.GenerateSubtitleFile(const AFileName: String): Boolean;
var
  Subs : TUWSubtitles;
begin
  Subs := TUWSubtitles.Create;
  try
    Subs.Assign(Subtitles);
    Subs.Add(frmMain.MPV.GetMediaLenInMs-1, frmMain.MPV.GetMediaLenInMs, ' ', '');
    Result := Subs.SaveToFile(AFileName, 0, TEncoding.UTF8, sfSubRip, smText);
  finally
    Subs.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TfrmVideoDubbing.GenerateAudioFile: Boolean;
var
  AParamArray : TStringArray;
  i : Integer;
begin
  lblStatus.Caption := lngExtracting;
  lblStatus.Update;

  AParamArray := FFMPEG_Karaoke.Split(' ');
  for i := 0 to High(AParamArray) do
    AParamArray[i] := StringsReplace(AParamArray[i], ['%input', '%output'], [frmMain.MPV.FileName, MusicFileName], []);

  Result := ExecuteApp(GetExtractAppFile, AParamArray, True, True, @ProcessCB);
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoDubbing.GenerateVideoFile;

  function GetTempo(const AFileName: String; const ADesiredLength: Integer): String;
  begin
    with TUWMPEGAudioTag.Create(AFileName) do
    try
      Result := SingleToStr(Duration / ADesiredLength, '.');
    finally
      Free;
    end;
  end;

var
  AParamArray : TStringArray;
  i, c : Integer;
  sl : TStringList;
  tracks, ids, s : String;
  sub : Boolean;
begin
  if chkBackgroundMusic.Checked then
  begin
    if (edtBackgroundMusic.Text = '') then
    begin
      MusicFileName := ConcatPaths([TempDir, 'a.mp3']);
      GenerateAudioFile;
    end
    else
      MusicFileName := edtBackgroundMusic.Text;
  end;

  lblStatus.Caption := lngvdGeneratingVideo;
  lblStatus.Update;

  sl := TStringList.Create;
  try
    sl.Add('-hide_banner');
    sl.Add('-y');
    sl.Add('-an');
    sl.Add('-i');
    {$IFDEF WINDOWS}
    sl.Add('"'+frmMain.MPV.FileName+'"');
    {$ELSE}
    sl.Add(frmMain.MPV.FileName);
    {$ENDIF}

    tracks := '';
    ids := '';
    c := 0;
    for i := 0 to TTS.Jobs.Count-1 do
      if FileExists(TTS.Jobs[i]^.FileName) then
      begin
        Inc(c);
        sl.Add('-i');
        {$IFDEF WINDOWS}
        sl.Add('"'+TTS.Jobs[i]^.FileName+'"');
        {$ELSE}
        sl.Add(TTS.Jobs[i]^.FileName);
        {$ENDIF}
        tracks += Format('[%d]adelay=delays=%d:all=1[%da];', [c, Subtitles[i].InitialTime, c]);
        //tracks += Format('[%d]adelay=delays=%d:all=1,atempo=%s[%da];', [c, Subtitles[i].InitialTime, GetTempo(TTS.Jobs[i]^.FileName, Subtitles.Duration[i]), c]);
        ids += Format('[%da]', [c]);
      end;

    if chkBackgroundMusic.Checked and FileExists(MusicFileName) then
    begin
      Inc(c);
      sl.Add('-i');
      {$IFDEF WINDOWS}
      sl.Add('"'+MusicFileName+'"');
      {$ELSE}
      sl.Add(MusicFileName);
      {$ENDIF}
      tracks += Format('[%d]volume=0.08[%da];', [c, c]);
      ids += Format('[%da]', [c]);
    end;

    sub := False;
    if chkSubtitleTrack.Checked then
    begin
      s := ConcatPaths([TempDir, 's.srt']);
      if GenerateSubtitleFile(s) then //if Subtitles.SaveToFile(s, 0, TEncoding.UTF8, sfSubRip, smText) then
      begin
        sub := True;
        sl.Add('-i');
        {$IFDEF WINDOWS}
        sl.Add('"'+s+'"');
        {$ELSE}
        sl.Add(s);
        {$ENDIF}
      end;
    end;

    sl.Add('-filter_complex');
    sl.Add(tracks + ids + 'amix=inputs=' + IntToStr(c) + '[a]');
    sl.Add('-map');
    sl.Add('0:v');
    sl.Add('-map');
    sl.Add('[a]');
    sl.Add('-c:v');
    sl.Add('copy');
    sl.Add('-shortest');
    sl.Add('-async');
    sl.Add('1');
    sl.Add('-c:a');
    sl.Add('aac');

    if sub then
    begin
      sl.Add('-map');
      sl.Add(IntToStr(c+1)+':s');
      sl.Add('-c:s');
      sl.Add('mov_text');
    end;

    {$IFDEF WINDOWS}
    sl.Add('"'+OutputFileName+'"');
    {$ELSE}
    sl.Add(OutputFileName);
    {$ENDIF}

    SetLength(AParamArray, sl.Count);
    for i := 0 to sl.Count-1 do
      AParamArray[i] := sl[i];

    if ExecuteApp(GetExtractAppFile, AParamArray, True, True, @ProcessCB) then
    begin
      ShowMessageDialog(lngOperationCompleted, '', lngOpenContainingFolder, @OpenFolderClick);
    end
    else
      ShowErrorMessageDialog(Format(lngErrorExecuting, [ExtractFileName(Tools.FFmpeg)]));
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.

