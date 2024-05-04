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

unit formSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, UWLayout, UWCheckBox, UWHotKey, ActnList, procConventions, LCLProc,
  ComCtrls, UWSubtitleAPI.Formats, LCLTranslator, laz.VirtualTrees, LCLIntf,
  procLocalize;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    btnClose: TButton;
    btnFFMPEG: TButton;
    btnMinus: TButton;
    btnPlus: TButton;
    btnSceneDetect: TButton;
    btnProfile: TButton;
    btnWhisperCPP: TButton;
    btnFasterWhisper: TButton;
    btnYTDLP: TButton;
    btnShortCutApply: TButton;
    btnShortCutSave: TButton;
    btnTesseract: TButton;
    cbnWaveColor: TColorButton;
    cbnWaveEntry: TColorButton;
    cboDefaultFileFormat: TComboBox;
    cboLanguage: TComboBox;
    cboShortCutPreset: TComboBox;
    cboTheme: TComboBox;
    cboListMode: TComboBox;
    cboTimeCodeMode: TComboBox;
    cboDefaultFrameRate: TComboBox;
    cboDefaultFileEncoding: TComboBox;
    chkPromptForInputFPS: TUWCheckBox;
    chkPromptForInputEncoding: TUWCheckBox;
    chkPromptForSaveWithErrors: TUWCheckBox;
    chkPromptForSaveWithErrors1: TUWCheckBox;
    chkWriteBOM: TUWCheckBox;
    chkSubtitleHandleByMPV: TUWCheckBox;
    chkSubBackground: TCheckBox;
    chkDotsOnSplit: TUWCheckBox;
    chkDrawWaveformGAP: TUWCheckBox;
    chkShowCPSBar: TUWCheckBox;
    chkDrawErrors: TUWCheckBox;
    chkDrawTags: TUWCheckBox;
    chkPromptForDeleteSubtitles: TUWCheckBox;
    chkShowSplashWindow: TUWCheckBox;
    chkAutoPlay: TUWCheckBox;
    chkSubShadow: TCheckBox;
    chkUseHWDec: TUWCheckBox;
    chkUseOwnFileDialog: TUWCheckBox;
    cboProfile: TComboBox;
    cboShortcutCat: TComboBox;
    cbnSub: TColorButton;
    cbnSubBorder: TColorButton;
    cbnSubShadow: TColorButton;
    cbnSubBackground: TColorButton;
    cboPauseMode: TComboBox;
    edtAPI_TTS: TEdit;
    edtCPSStrategy: TEdit;
    edtSceneDetect: TEdit;
    edtWhisperCPP: TEdit;
    edtFasterWhisper: TEdit;
    edtYTDLP: TEdit;
    edtTesseract: TEdit;
    imlFileTypes: TImageList;
    lblAdditionalMPVOptions: TLabel;
    lblListFontSize: TLabel;
    lblCPSStrategy: TLabel;
    lblTextBoxFontSize: TLabel;
    lblSceneDetect: TLabel;
    lblDialogSegmentsDetectionThreshold: TLabel;
    lblWhisperCPP: TLabel;
    lblFasterWhisper: TLabel;
    lblYTDLP: TLabel;
    lblSCSnapArea: TLabel;
    lblSCSnapInCues: TLabel;
    lblSCSnapOutCues: TLabel;
    lblChaining: TLabel;
    lblShortCutInUse: TLabel;
    lblShortCutPreset: TLabel;
    lblWaveEntry: TLabel;
    lblWaveColor: TLabel;
    lblSubSize: TLabel;
    lblSubColor: TLabel;
    lblSubBorder: TLabel;
    lblSubShadow: TLabel;
    lblSubBackground: TLabel;
    lblSubtitles: TLabel;
    lblSeekTime: TLabel;
    lblFrameStep: TLabel;
    lblWaveform: TLabel;
    lblTesseract: TLabel;
    lstMPVOptions: TListBox;
    lyoFileTypeAssociations: TUWLayout;
    spnTextBoxFontSize: TSpinEdit;
    spnSCSnapArea: TSpinEdit;
    spnChaining: TSpinEdit;
    spnSCSnapThreshold: TSpinEdit;
    spnSCSnapInCues: TSpinEdit;
    spnSCSnapOutCues: TSpinEdit;
    spnDetectDialogSegment: TSpinEdit;
    spnSubSize: TSpinEdit;
    spnSeekTime: TSpinEdit;
    spnFrameStep: TSpinEdit;
    spnListFontSize: TSpinEdit;
    edtFFMPEG: TEdit;
    edtWebReference: TEdit;
    edtRepeatableChars: TEdit;
    edtProhibitedChars: TEdit;
    lblAPI_TTS: TLabel;
    lblZeroToDisable: TLabel;
    lblDefaultFileFormat: TLabel;
    lblWPM: TLabel;
    lblCPL: TLabel;
    lblMaxLineCount: TLabel;
    lblShortcut: TLabel;
    lblShortcutCat: TLabel;
    lblLanguage: TLabel;
    lblProfile: TLabel;
    lblCPS: TLabel;
    lblTheme: TLabel;
    lblMaxDurationMs: TLabel;
    lblMinDurationMs: TLabel;
    lblNewSubtitleMs: TLabel;
    lblShiftTimeMs: TLabel;
    lblSubtitlePauseMs: TLabel;
    lblRepeatableChars: TLabel;
    lblProhibitedChars: TLabel;
    lblListMode: TLabel;
    lblFFMPEG: TLabel;
    lblTimeCodeMode: TLabel;
    lblDefaultFrameRate: TLabel;
    lblDefaultFileEncoding: TLabel;
    lblAutoBackup: TLabel;
    lblWebReference: TLabel;
    lstTree: TListBox;
    lstShortcuts: TListBox;
    lyoAppearance: TUWLayout;
    lyoTools: TUWLayout;
    lyoShortcuts: TUWLayout;
    spnMaxDurationMs: TSpinEdit;
    spnMaxLineCount: TSpinEdit;
    spnMinDurationMs: TSpinEdit;
    spnMinDurationPerWord: TSpinEdit;
    spnNewSubtitleMs: TSpinEdit;
    spnAutoBackupMinutes: TSpinEdit;
    spnShiftTimeMs: TSpinEdit;
    spnSubtitlePauseMs: TSpinEdit;
    lyoGeneral: TUWLayout;
    lyoConventions: TUWLayout;
    hkShortcut: TUWHotKey;
    spnCPS: TSpinEdit;
    spnWPM: TSpinEdit;
    spnCPL: TSpinEdit;
    lyoMPV: TUWLayout;
    tlbFileTypeIco: TToolBar;
    procedure btnCloseClick(Sender: TObject);
    procedure btnFasterWhisperClick(Sender: TObject);
    procedure btnFFMPEGClick(Sender: TObject);
    procedure btnMinusClick(Sender: TObject);
    procedure btnPlusClick(Sender: TObject);
    procedure btnProfileClick(Sender: TObject);
    procedure btnSceneDetectClick(Sender: TObject);
    procedure btnShortCutApplyClick(Sender: TObject);
    procedure btnShortCutSaveClick(Sender: TObject);
    procedure btnTesseractClick(Sender: TObject);
    procedure btnWhisperCPPClick(Sender: TObject);
    procedure btnYTDLPClick(Sender: TObject);
    procedure cboProfileSelect(Sender: TObject);
    procedure cboShortcutCatSelect(Sender: TObject);
    procedure cboShortCutPresetSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure hkShortcutChange(Sender: TObject);
    procedure lblAPI_TTSClick(Sender: TObject);
    procedure lblFasterWhisperClick(Sender: TObject);
    procedure lblFFMPEGClick(Sender: TObject);
    procedure lblSceneDetectClick(Sender: TObject);
    procedure lblTesseractClick(Sender: TObject);
    procedure lblWhisperCPPClick(Sender: TObject);
    procedure lblYTDLPClick(Sender: TObject);
    procedure lstShortcutsSelectionChange(Sender: TObject; User: boolean);
    procedure lstTreeSelectionChange(Sender: TObject; User: boolean);
    procedure spnConventionChange(Sender: TObject);
  private
    FProfiles: TProfiles;
    FShortCutListIndex: array of Byte;
    FShortCutListCategory: TStrings;
    FLngList: TStrings;
    {$IFDEF WINDOWS}
    FUpdateFileTypes: Boolean;
    procedure PrepareToolbarAndImagelistIcos;
    procedure UpdateFileTypeAssociations;
    procedure FileTypeIconClick(Sender: TObject);
    {$ENDIF}
    function GetActionFromShortCut(const AShortCut: TShortCut): TAction;
    procedure SetLayoutPage(const APage: TUWLayout);
    procedure FillComboWithShortcuts;
  public

  end;

var
  frmSettings: TfrmSettings;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procConfig, procDialogs, UWSystem.SysUtils,
  procColorTheme, formMain, UWSystem.Globalization, formConventions,
  procShortCut, procMPV, UWSystem.TimeUtils
  {$IFDEF WINDOWS}
  , FileUtil, procFileTypes
  {$ENDIF};

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmSettings }

// -----------------------------------------------------------------------------

procedure TfrmSettings.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FLngList := TStringList.Create;
  try
    with lstTree.Items do
    begin
      Add(lngssGeneral);
      Add(lngssConventions);
      Add(lngssAppearance);
      Add(lngssShortcuts);
      Add(lngssMPV);
      Add(lngssTools);
      {$IFDEF WINDOWS}
      Add(lngssFileTypeAssociations);
      {$ENDIF}
    end;
    lstTree.ItemIndex := 0;
    with cboPauseMode.Items do
    begin
      BeginUpdate;
      Add(lngssMilliseconds);
      Add(lngssFrames);
      EndUpdate;
    end;

    hkShortcut.EmptyText  := lngssShortCutNone;
    FShortCutListCategory := TStringList.Create;

    FillComboWithLanguages(cboLanguage, FLngList);
    FillComboWithFPS(cboDefaultFrameRate, Workspace.FPS.DefFPS);
    FillComboWithFormats(cboDefaultFileFormat);
    FillComboWithEncodings(cboDefaultFileEncoding);
    FillComboWithShortcuts;
    FillComboWithShortCutsPreset(cboShortCutPreset);
    {$IFDEF WINDOWS}
    PrepareToolbarAndImagelistIcos;
    {$ENDIF}
    cboDefaultFileEncoding.ItemIndex := Workspace.DefEncoding;
    btnShortCutApply.Enabled := False;

    cboTimeCodeMode.AddItem(lngHHMMSSZZZ, NIL);
    cboTimeCodeMode.AddItem(lngHHMMSSFF, NIL);
    cboTheme.AddItem(lngssAutoMode, NIL);
    cboTheme.AddItem(lngssLightMode, NIL);
    cboTheme.AddItem(lngssDarkMode, NIL);
    cboListMode.AddItem(lngssListMode, NIL);
    cboListMode.AddItem(lngssBlockMode, NIL);
  finally
  end;

  with AppOptions do
  begin
    edtWebReference.Text := WebSearchURL;
    with Conventions do
    begin
      if PauseInFrames then
        cboPauseMode.ItemIndex := 1
      else
        cboPauseMode.ItemIndex := 0;

      edtRepeatableChars.Text     := RepeatableChars;
      edtProhibitedChars.Text     := ProhibitedChars;
      edtCPSStrategy.Text         := CPSLineLenStrategy;
      spnNewSubtitleMs.Value      := NewSubtitleMs;
      spnMinDurationMs.Value      := MinDuration;
      spnMinDurationPerWord.Value := MinDurationPerWord;
      spnMaxDurationMs.Value      := MaxDuration;
      spnMaxLineCount.Value       := MaxLines;
      spnSubtitlePauseMs.Value    := MinPause;
      spnCPS.Value                := MaxCPS;
      spnWPM.Value                := WPM;
      spnCPL.Value                := CPL;
      spnSCSnapArea.Value         := ShotcutSnapArea;
      spnSCSnapThreshold.Value    := ShotcutThreshold;
      spnSCSnapInCues.Value       := ShotcutInCues;
      spnSCSnapOutCues.Value      := ShotcutOutCues;
      spnChaining.Value           := Chaining;
      chkDotsOnSplit.Checked      := DotsOnSplit;
    end;
    spnShiftTimeMs.Value         := ShiftTimeMS;
    spnDetectDialogSegment.Value := DialogSegmentThreshold;
    spnAutoBackupMinutes.Value   := AutoBackupSeconds div 60;
    chkShowSplashWindow.Checked  := ShowWelcomeAtStartup;
    chkUseOwnFileDialog.Checked  := UseOwnFileDialog;
    chkDrawTags.Checked                 := VSTOptions.DrawTags;
    chkDrawErrors.Checked               := VSTOptions.DrawErrors;
    chkShowCPSBar.Checked               := frmMain.mmoText.CPSBar.Visible;
    chkWriteBOM.Checked                 := Subtitles.WriteBOM;
    chkDrawWaveformGAP.Checked          := frmMain.WAVE.DrawGAP;
    chkPromptForDeleteSubtitles.Checked := AskForDeleteLines;
    chkPromptForInputFPS.Checked        := AskForInputFPS;
    chkPromptForInputEncoding.Checked   := AskForInputEncoding;
    chkPromptForSaveWithErrors.Checked  := CheckErrorsBeforeSave;

    cboTheme.ItemIndex := Integer(ColorThemeInstance.ColorMode);
  end;

  spnListFontSize.Value            := frmMain.VST.Font.Size;
  spnTextBoxFontSize.Value         := frmMain.mmoText.Font.Size;
  with MPVOptions do
  begin
    chkAutoPlay.Checked            := AutoStartPlaying;
    chkSubtitleHandleByMPV.Checked := SubtitleHandleByMPV;
    chkUseHWDec.Checked            := frmMain.MPV.UseHWDec;
    spnSeekTime.Value              := SeekTime;
    spnFrameStep.Value             := FrameStep;

    cbnSub.ButtonColor             := HexStrToColor(TextColor);
    cbnSubBorder.ButtonColor       := HexStrToColor(TextBorderColor);
    cbnSubShadow.ButtonColor       := HexStrToColor(TextShadowColor);
    chkSubShadow.Checked           := UseTextShadowColor;
    cbnSubBackground.ButtonColor   := HexStrToColor(TextBackgroundColor);
    chkSubBackground.Checked       := UseTextBackgroundColor;
    spnSubSize.Value               := TextSize;

    if Length(AdditionalOptions) > 0 then
      for i := 0 to High(AdditionalOptions) do
        lstMPVOptions.Items.Add(AdditionalOptions[i]);

    btnMinus.Enabled := lstMPVOptions.Count > 0;
  end;

  with frmMain.WAVE do
  begin
    cbnWaveColor.ButtonColor := CustomColors.Waveform;
    cbnWaveEntry.ButtonColor := CustomColors.Item;
  end;

  edtFFMPEG.Text := Tools.FFmpeg;
  edtSceneDetect.Text := Tools.PySceneDetect;
  edtWhisperCPP.Text := Tools.WhisperCPP;
  edtFasterWhisper.Text := Tools.FasterWhisper;
  edtYTDLP.Text := Tools.YTDLP;
  edtTesseract.Text := Tools.Tesseract;
  edtAPI_TTS.Text := Tools.API_KEY_TTS;

  cboTimeCodeMode.ItemIndex := Integer(Workspace.WorkMode);
  cboListMode.ItemIndex := Integer(VSTOptions.DrawMode);

  FProfiles := TProfiles.Create(ConventionsFileName);
  FillItemsWithConventions(cboProfile.Items, FProfiles);
  cboProfile.Items.Insert(0, lngasCustom);
  i := cboProfile.Items.IndexOf(AppOptions.Conventions.Name);
  if i >= 0 then
    cboProfile.ItemIndex := i
  else
    cboProfile.ItemIndex := 0;

  hkShortcut.HotKey := 0;
  {$IFNDEF WINDOWS}
  cboTheme.Enabled := False;
  PrepareCustomControls(Self);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
begin
  with AppOptions do
  begin
   if (cboLanguage.ItemIndex >= 0) and (Length(FLngList.Strings[cboLanguage.ItemIndex].Split([';'])) > 0) then
      GUILanguage := FLngList.Strings[cboLanguage.ItemIndex].Split([';'])[0]
    else
      GUILanguage := 'en_US';

    WebSearchURL := edtWebReference.Text;
    with Conventions do
    begin
      if cboProfile.ItemIndex > 0 then
        Name             := cboProfile.Text
      else
        Name             := '';

      RepeatableChars    := edtRepeatableChars.Text;
      ProhibitedChars    := edtProhibitedChars.Text;
      CPSLineLenStrategy := edtCPSStrategy.Text;
      PauseInFrames      := cboPauseMode.ItemIndex = 1;
      NewSubtitleMS      := spnNewSubtitleMs.Value;
      MinDuration        := spnMinDurationMs.Value;
      MaxDuration        := spnMaxDurationMs.Value;
      MaxLines           := spnMaxLineCount.Value;
      MinDurationPerWord := spnMinDurationPerWord.Value;
      MinPause           := spnSubtitlePauseMs.Value;
      MaxCPS             := spnCPS.Value;
      WPM                := spnWPM.Value;
      CPL                := spnCPL.Value;
      ShotcutSnapArea    := spnSCSnapArea.Value;
      ShotcutThreshold   := spnSCSnapThreshold.Value;
      ShotcutInCues      := spnSCSnapInCues.Value;
      ShotcutOutCues     := spnSCSnapOutCues.Value;
      Chaining           := spnChaining.Value;
      DotsOnSplit        := chkDotsOnSplit.Checked;

      if PauseInFrames then
        frmMain.WAVE.MinimumBlank := FramesToTime(MinPause, Workspace.FPS.OutputFPS)
      else
        frmMain.WAVE.MinimumBlank := MinPause;

      frmMain.mmoText.CPSBar.Max := MaxCPS;
    end;
    ShiftTimeMS            := spnShiftTimeMs.Value;
    DialogSegmentThreshold := spnDetectDialogSegment.Value;
    AutoBackupSeconds      := spnAutoBackupMinutes.Value*60;
    ShowWelcomeAtStartup   := chkShowSplashWindow.Checked;
    UseOwnFileDialog       := chkUseOwnFileDialog.Checked;
    VSTOptions.DrawTags    := chkDrawTags.Checked;
    VSTOptions.DrawErrors  := chkDrawErrors.Checked;
    frmMain.WAVE.DrawGAP   := chkDrawWaveformGAP.Checked;
    frmMain.mmoText.CPSBar.Visible        := chkShowCPSBar.Checked;
    frmMain.mmoTranslation.CPSBar.Visible := chkShowCPSBar.Checked;
    Subtitles.WriteBOM := chkWriteBOM.Checked;
    AskForDeleteLines   := chkPromptForDeleteSubtitles.Checked;
    AskForInputFPS      := chkPromptForInputFPS.Checked;
    AskForInputEncoding := chkPromptForInputEncoding.Checked;
    CheckErrorsBeforeSave := chkPromptForSaveWithErrors.Checked;
  end; //with

  frmMain.VST.Font.Size            := spnListFontSize.Value;
  frmMain.VST.Canvas.Font.Size     := frmMain.VST.Font.Size;
  frmMain.VST.Header.Font.Size     := frmMain.VST.Font.Size;
  frmMain.mmoText.Font.Size        := spnTextBoxFontSize.Value;
  frmMain.mmoTranslation.Font.Size := frmMain.mmoText.Font.Size;

  with MPVOptions do
  begin
    AutoStartPlaying       := chkAutoPlay.Checked;
    SubtitleHandleByMPV    := chkSubtitleHandleByMPV.Checked;
    frmMain.MPV.UseHWDec   := chkUseHWDec.Checked;
    SeekTime               := spnSeekTime.Value;
    FrameStep              := spnFrameStep.Value;
    TextColor              := ColorToHexStr(cbnSub.ButtonColor);
    TextBorderColor        := ColorToHexStr(cbnSubBorder.ButtonColor);
    TextShadowColor        := ColorToHexStr(cbnSubShadow.ButtonColor);
    UseTextShadowColor     := chkSubShadow.Checked;
    TextBackgroundColor    := ColorToHexStr(cbnSubBackground.ButtonColor);
    UseTextBackgroundColor := chkSubBackground.Checked;
    TextSize               := spnSubSize.Value;

    SetLength(AdditionalOptions, lstMPVOptions.Count);
    if Length(AdditionalOptions) > 0 then
      for i := 0 to High(AdditionalOptions) do
        AdditionalOptions[i] := lstMPVOptions.Items[i];
  end;

  with frmMain.WAVE do
  begin
    CustomColors.Waveform := cbnWaveColor.ButtonColor;
    CustomColors.Item     := cbnWaveEntry.ButtonColor;
  end;

  Tools.FFmpeg := edtFFMPEG.Text;
  Tools.PySceneDetect := edtSceneDetect.Text;
  Tools.WhisperCPP := edtWhisperCPP.Text;
  Tools.FasterWhisper := edtFasterWhisper.Text;
  Tools.YTDLP := edtYTDLP.Text;
  Tools.Tesseract := edtTesseract.Text;
  Tools.API_KEY_TTS := edtAPI_TTS.Text;

  if Workspace.WorkMode <> TWorkMode(cboTimeCodeMode.ItemIndex) then
    SetWorkMode(TWorkMode(cboTimeCodeMode.ItemIndex));

  Workspace.FPS.DefFPS  := StrToFloatDef(cboDefaultFrameRate.Text, DefFPS, FormatSettings);
  Workspace.DefEncoding := cboDefaultFileEncoding.ItemIndex;
  Workspace.DefFormat   := TUWSubtitleFormats(cboDefaultFileFormat.ItemIndex+1);
  if SubtitleInfo.Text.FileName.IsEmpty then
    frmMain.cboFormat.ItemIndex := Integer(Workspace.DefFormat)-1;

  if not IsWorkAreaEnabled then // Only change if work area is disabled
  begin
    frmMain.cboInputFPS.ItemIndex := cboDefaultFrameRate.ItemIndex;
    frmMain.cboInputFPSSelect(NIL);
  end;
  VSTOptions.DrawMode := TVSTDrawMode(cboListMode.ItemIndex);

  ColorThemeInstance.ColorMode := TColorMode(cboTheme.ItemIndex);
  CheckColorTheme;

  SetLength(FShortCutListIndex, 0);
  AppOptions.ShortCutPreset := cboShortCutPreset.Text + '.key';

  UpdateMPVOptions;

  {$IFDEF WINDOWS}
  UpdateFileTypeAssociations;
  {$ENDIF}

  FShortCutListCategory.Free;
  FProfiles.Free;
  FLngList.Free;
  CloseAction := caFree;
  frmSettings := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
  tlbFileTypeIco.Images := imlFileTypes;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.SetLayoutPage(const APage: TUWLayout);
var
  C: TComponent;
begin
  for C in Self do
    if (C is TUWLayout) then
      if TUWLayout(C) = APage then
      begin
        TUWLayout(C).Left := 196;
        TUWLayout(C).Top  := 8;
        TUWLayout(C).Show;
      end
      else
        TUWLayout(C).Hide;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.lstTreeSelectionChange(Sender: TObject; User: boolean);
begin
  case lstTree.ItemIndex of
    0: SetLayoutPage(lyoGeneral);
    1: SetLayoutPage(lyoConventions);
    2: SetLayoutPage(lyoAppearance);
    3: SetLayoutPage(lyoShortcuts);
    4: SetLayoutPage(lyoMPV);
    5: SetLayoutPage(lyoTools);
    {$IFDEF WINDOWS}
    6: SetLayoutPage(lyoFileTypeAssociations);
    {$ENDIF}
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnFFMPEGClick(Sender: TObject);
begin
  edtFFMPEG.Text := GetFileFromOpenDialog(edtFFMPEG.Text);
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnSceneDetectClick(Sender: TObject);
begin
  edtSceneDetect.Text := GetFileFromOpenDialog(edtSceneDetect.Text);
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnWhisperCPPClick(Sender: TObject);
begin
  edtWhisperCPP.Text := GetFileFromOpenDialog(edtWhisperCPP.Text);
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnFasterWhisperClick(Sender: TObject);
begin
  edtFasterWhisper.Text := GetFileFromOpenDialog(edtFasterWhisper.Text);
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnYTDLPClick(Sender: TObject);
begin
  edtYTDLP.Text := GetFileFromOpenDialog(edtYTDLP.Text);
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnTesseractClick(Sender: TObject);
begin
  edtTesseract.Text := GetFileFromOpenDialog(edtTesseract.Text);
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnProfileClick(Sender: TObject);
var
  i, c: Integer;
  s: String;
begin
  with TfrmConventions.Create(Self) do
  try
    for i := 0 to lyoConvention.ControlCount-1 do
      for c := 0 to lyoConventions.ControlCount-1 do
        if lyoConvention.Controls[i].Name = lyoConventions.Controls[c].Name then
        begin
          lyoConvention.Controls[i].Caption := lyoConventions.Controls[c].Caption;
          Break;
        end;

    cboPauseMode.Items.Assign(Self.cboPauseMode.Items);

    FList := FProfiles;
    FList.FillTStrings(lstTree.Items);
    if cboProfile.ItemIndex > 0 then
      lstTree.ItemIndex := cboProfile.ItemIndex-1;
    s := cboProfile.Text;
    ShowModal;

    FillItemsWithConventions(cboProfile.Items, FProfiles);
    cboProfile.Items.Insert(0, lngasCustom);
    i := cboProfile.Items.IndexOf(s);
    if i >= 0 then
      cboProfile.ItemIndex := i
    else
      cboProfile.ItemIndex := 0;

    cboProfileSelect(Sender);
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.cboShortcutCatSelect(Sender: TObject);
var
  i: Integer;
  s, sc: String;
begin
  lstShortcuts.Clear;
  with frmMain do
    for i := 0 to ActionList.ActionCount-1 do
    begin
      with TAction(ActionList.Actions[i]) do
      begin
        if FShortCutListCategory[cboShortcutCat.ItemIndex] = Category then
        begin
          if ShortCut <> 0 then
            sc := ShortCutToTextEx(ShortCut)
          else
            sc := '';

          s := Caption + ' [' + sc + ']';
          lstShortcuts.AddItem(s, NIL);
          SetLength(FShortCutListIndex, lstShortcuts.Count);
          FShortCutListIndex[lstShortcuts.Count-1] := Index;
        end;
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.cboShortCutPresetSelect(Sender: TObject);
begin
  LoadShortCutsFromFile(ConcatPaths([ShortCutFolder, cboShortCutPreset.Text + '.key']), frmMain.ActionList);
  cboShortcutCatSelect(Sender);
  btnShortCutApply.Enabled := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.FillComboWithShortcuts;
var
  i: Integer;
begin
  with frmMain do
  begin
    for i := 0 to ActionList.ActionCount-1 do
      with TAction(ActionList.Actions[i]) do
      begin
        if FShortCutListCategory.IndexOf(Category) < 0 then
        begin
          cboShortcutCat.AddItem(LocalizeCategory(ActionList.Actions[i].Category), NIL);
          FShortCutListCategory.Add(Category);
        end; //if
      end; //with TAction

    if cboShortcutCat.Items.Count > 0 then cboShortcutCat.ItemIndex := 0;
    cboShortcutCatSelect(NIL);
  end; //with frmMain
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.lstShortcutsSelectionChange(Sender: TObject;
  User: boolean);
begin
  if lstShortcuts.Count > 0 then
  begin
    lstShortcuts.Hint := TAction(frmMain.ActionList.Actions[FShortCutListIndex[lstShortcuts.ItemIndex]]).Name;
    hkShortcut.HotKey := TAction(frmMain.ActionList.Actions[FShortCutListIndex[lstShortcuts.ItemIndex]]).ShortCut;
  end
  else
    lstShortcuts.Hint := '';
end;

// -----------------------------------------------------------------------------

function TfrmSettings.GetActionFromShortCut(const AShortCut: TShortCut): TAction;
var
  i: Integer;
begin
  Result := NIL;
  with frmMain do
    for i := 0 to ActionList.ActionCount-1 do
      if AShortCut = TAction(ActionList.Actions[i]).ShortCut then
        Exit(TAction(ActionList.Actions[i]));
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.hkShortcutChange(Sender: TObject);
var
  AAction: TAction;
begin
  lblShortCutInUse.Caption := '';
  if (lstShortcuts.ItemIndex >= 0) then
    with TAction(frmMain.ActionList.Actions[FShortCutListIndex[lstShortcuts.ItemIndex]]) do
      if (ShortCut <> hkShortcut.HotKey) then
      begin
        AAction := GetActionFromShortCut(hkShortcut.HotKey);
        if (AAction <> NIL) and (hkShortcut.HotKey <> 0) then
        begin
          {if MsgReplaceShortCut(AAction.Caption) = mrNo then
          begin
            hkShortcut.HotKey := 0;
            Exit;
          end
          else
            AAction.ShortCut := 0;}

          lblShortCutInUse.Caption := Format(lngShortCutInUse, [AAction.Caption]);
        end;

        ShortCut := hkShortcut.HotKey;
        lstShortcuts.Items[lstShortcuts.ItemIndex] := Caption + ' [' + ShortCutToTextEx(ShortCut) + ']'; //cboShortcutCatSelect(NIL);
        btnShortCutApply.Enabled := True;
      end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.lblAPI_TTSClick(Sender: TObject);
begin
  OpenURL('https://elevenlabs.io/sign-up');
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.lblFasterWhisperClick(Sender: TObject);
begin
  OpenURL('https://github.com/Purfview/whisper-standalone-win');
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.lblFFMPEGClick(Sender: TObject);
begin
  OpenURL('https://ffmpeg.org');
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.lblSceneDetectClick(Sender: TObject);
begin
  OpenURL('https://www.scenedetect.com');
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.lblWhisperCPPClick(Sender: TObject);
begin
  OpenURL('https://github.com/ggerganov/whisper.cpp');
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.lblYTDLPClick(Sender: TObject);
begin
  OpenURL('https://github.com/yt-dlp/yt-dlp');
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.lblTesseractClick(Sender: TObject);
begin
  OpenURL('https://github.com/tesseract-ocr/tesseract');
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnShortCutApplyClick(Sender: TObject);
begin
  AppOptions.ShortCutPreset := cboShortCutPreset.Text + '.key';
  SaveShortCutsToFile(ShortCutFileName, frmMain.ActionList);
  btnShortCutApply.Enabled := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnShortCutSaveClick(Sender: TObject);
var
  s, f: String;
begin
  s := InputDialog(lngNewShortCutPreset, lngNewShortCutPresetName, '');
  if not s.IsEmpty then
  begin
    f := ConcatPaths([ShortCutFolder, s + '.key']);
    if not FileExists(f) then
    begin
      SaveShortCutsToFile(f, frmMain.ActionList);
      cboShortCutPreset.ItemIndex := cboShortCutPreset.Items.Add(s);
    end
    else
      cboShortCutPreset.ItemIndex := cboShortCutPreset.Items.IndexOf(s);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.cboProfileSelect(Sender: TObject);
var
  Profile: PProfileItem;
begin
  if cboProfile.ItemIndex = 0 then
    Profile := @AppOptions.Conventions
  else if (FProfiles.Items.Count > 0) and (cboProfile.ItemIndex > 0) and (cboProfile.ItemIndex <= FProfiles.Items.Count) then
    Profile := FProfiles.Items[cboProfile.ItemIndex-1];

  with Profile^ do
  begin
    cboProfile.Tag := 1;

    if PauseInFrames then
      cboPauseMode.ItemIndex := 1
    else
      cboPauseMode.ItemIndex := 0;

    edtRepeatableChars.Text     := RepeatableChars;
    edtProhibitedChars.Text     := ProhibitedChars;
    edtCPSStrategy.Text         := CPSLineLenStrategy;
    spnNewSubtitleMs.Value      := NewSubtitleMs;
    spnMinDurationMs.Value      := MinDuration;
    spnMinDurationPerWord.Value := MinDurationPerWord;
    spnMaxDurationMs.Value      := MaxDuration;
    spnMaxLineCount.Value       := MaxLines;
    spnSubtitlePauseMs.Value    := MinPause;
    spnCPS.Value                := MaxCPS;
    spnWPM.Value                := WPM;
    spnCPL.Value                := CPL;
    spnSCSnapArea.Value         := ShotcutSnapArea;
    spnSCSnapThreshold.Value    := ShotcutThreshold;
    spnSCSnapInCues.Value       := ShotcutInCues;
    spnSCSnapOutCues.Value      := ShotcutOutCues;
    spnChaining.Value           := Chaining;
    chkDotsOnSplit.Checked      := DotsOnSplit;

    cboProfile.Tag := 0;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.spnConventionChange(Sender: TObject);
begin
  if (cboProfile.Tag = 0) and (cboProfile.ItemIndex <> 0) then
    cboProfile.ItemIndex := 0;
end;

// -----------------------------------------------------------------------------

{$IFDEF WINDOWS}
procedure TfrmSettings.PrepareToolbarAndImagelistIcos;
var
  Icos: TStringList;
  Picture: TPicture;
  x, i: Integer;
begin
  FUpdateFileTypes := False;
  Icos := TStringList.Create;
  try
    Icos.Sorted := True;
    FindAllFiles(Icos, IconsFolder, '*.ico', False);

    if Icos.Count > 0 then
    begin
      Picture := TPicture.Create;
      try
        for x := 0 to Icos.Count-1 do
        begin
          Picture.LoadFromFile(Icos[x]);
          i := imlFileTypes.AddIcon(Picture.Icon);

          with TToolButton.Create(tlbFileTypeIco) do
          begin
            Parent     := tlbFileTypeIco;
            Caption    := ChangeFileExt(ExtractFileName(Icos[x]), '');
            Style      := tbsCheck;
            AutoSize   := True;
            OnClick    := @FileTypeIconClick;
            ImageIndex := i;
            Down       := IsRegisteredFileType('.' + Caption);
          end;
        end;
      finally
        Picture.Free;
      end;
    end;
  finally
    Icos.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.UpdateFileTypeAssociations;
var
  x: Integer;
begin
  for x := 0 to tlbFileTypeIco.ButtonCount-1 do
    RegisterFileType('.' + tlbFileTypeIco.Buttons[x].Caption, IconsFolder + tlbFileTypeIco.Buttons[x].Caption + '.ico', tlbFileTypeIco.Buttons[x].Down, FUpdateFileTypes and (x = tlbFileTypeIco.ButtonCount-1));
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.FileTypeIconClick(Sender: TObject);
begin
  FUpdateFileTypes := True;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnMinusClick(Sender: TObject);
begin
  if (lstMPVOptions.Count > 0) and (lstMPVOptions.ItemIndex >= 0) then
    lstMPVOptions.Items.Delete(lstMPVOptions.ItemIndex);

  btnMinus.Enabled := lstMPVOptions.Count > 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnPlusClick(Sender: TObject);
var
  s: String;
begin
  s := InputDialog(lngAdditionalOption, '', '');
  if not s.IsEmpty and (Pos('=', s) > 0) then
    lstMPVOptions.Items.Add(s);

  btnMinus.Enabled := lstMPVOptions.Count > 0;
end;

// -----------------------------------------------------------------------------

end.

