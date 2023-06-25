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
  ExtCtrls, UWLayout, UWCheckBox, UWHotKey, UWSystem.XMLLang,
  procConventions, LCLProc, ComCtrls, UWSubtitleAPI.Formats;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    btnClose: TButton;
    btnFFMPEG: TButton;
    btnProfile: TButton;
    btnShortCutApply: TButton;
    btnShortCutSave: TButton;
    cbnWaveColor: TColorButton;
    cbnWaveStart: TColorButton;
    cbnWaveEnd: TColorButton;
    cboDefaultFileFormat: TComboBox;
    cboLanguage: TComboBox;
    cboShortCutPreset: TComboBox;
    cboTheme: TComboBox;
    cboListMode: TComboBox;
    cboTimeCodeMode: TComboBox;
    cboDefaultFrameRate: TComboBox;
    cboDefaultFileEncoding: TComboBox;
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
    chkUseOwnFileDialog: TUWCheckBox;
    cboProfile: TComboBox;
    cboShortcutCat: TComboBox;
    cbnSub: TColorButton;
    cbnSubBorder: TColorButton;
    cbnSubShadow: TColorButton;
    cbnSubBackground: TColorButton;
    cboPauseMode: TComboBox;
    edtCPSStrategy: TEdit;
    imlFileTypes: TImageList;
    lblCPSStrategy: TLabel;
    lblShortCutPreset: TLabel;
    lblWaveStart: TLabel;
    lblWaveColor: TLabel;
    lblWaveEnd: TLabel;
    lblSubSize: TLabel;
    lblSubColor: TLabel;
    lblSubBorder: TLabel;
    lblSubShadow: TLabel;
    lblSubBackground: TLabel;
    lblSubtitles: TLabel;
    lblSeekTime: TLabel;
    lblFrameStep: TLabel;
    lblWaveform: TLabel;
    lblToolBarWave: TLabel;
    lyoFileTypeAssociations: TUWLayout;
    spnSubSize: TSpinEdit;
    spnSeekTime: TSpinEdit;
    spnFrameStep: TSpinEdit;
    tlbMain_: TToolBar;
    tlbEditor_: TToolBar;
    edtFFMPEG: TEdit;
    edtWebReference: TEdit;
    edtRepeatableChars: TEdit;
    edtProhibitedChars: TEdit;
    tlbVideo_: TToolBar;
    lblToolBarVideo: TLabel;
    lblToolBarMain: TLabel;
    lblToolBarEditor: TLabel;
    lblZeroToDisable: TLabel;
    lblDefaultFileFormat: TLabel;
    lblMinDurationPerWordMs: TLabel;
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
    lyoToolbar: TUWLayout;
    tlbWaveform_: TToolBar;
    tlbFileTypeIco: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton3: TToolButton;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    ToolButton33: TToolButton;
    ToolButton34: TToolButton;
    ToolButton35: TToolButton;
    ToolButton36: TToolButton;
    ToolButton37: TToolButton;
    ToolButton38: TToolButton;
    ToolButton39: TToolButton;
    ToolButton4: TToolButton;
    ToolButton40: TToolButton;
    ToolButton41: TToolButton;
    ToolButton42: TToolButton;
    ToolButton43: TToolButton;
    ToolButton44: TToolButton;
    ToolButton45: TToolButton;
    ToolButton46: TToolButton;
    ToolButton47: TToolButton;
    ToolButton48: TToolButton;
    ToolButton49: TToolButton;
    ToolButton5: TToolButton;
    ToolButton50: TToolButton;
    ToolButton51: TToolButton;
    ToolButton52: TToolButton;
    ToolButton53: TToolButton;
    ToolButton54: TToolButton;
    ToolButton55: TToolButton;
    ToolButton56: TToolButton;
    ToolButton57: TToolButton;
    ToolButton58: TToolButton;
    ToolButton59: TToolButton;
    ToolButton6: TToolButton;
    ToolButton60: TToolButton;
    ToolButton61: TToolButton;
    ToolButton62: TToolButton;
    ToolButton63: TToolButton;
    ToolButton64: TToolButton;
    ToolButton65: TToolButton;
    ToolButton66: TToolButton;
    ToolButton67: TToolButton;
    ToolButton68: TToolButton;
    ToolButton69: TToolButton;
    ToolButton7: TToolButton;
    ToolButton70: TToolButton;
    ToolButton71: TToolButton;
    ToolButton72: TToolButton;
    ToolButton73: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure btnCloseClick(Sender: TObject);
    procedure btnFFMPEGClick(Sender: TObject);
    procedure btnProfileClick(Sender: TObject);
    procedure btnShortCutApplyClick(Sender: TObject);
    procedure btnShortCutSaveClick(Sender: TObject);
    procedure cboProfileSelect(Sender: TObject);
    procedure cboShortcutCatSelect(Sender: TObject);
    procedure cboShortCutPresetSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure hkShortcutChange(Sender: TObject);
    procedure lstShortcutsSelectionChange(Sender: TObject; User: boolean);
    procedure lstTreeSelectionChange(Sender: TObject; User: boolean);
    procedure spnConventionChange(Sender: TObject);
  private
    FProfiles: TProfiles;
    FShortCutListIndex: array of Byte;
    {$IFDEF WINDOWS}
    FUpdateFileTypes: Boolean;
    procedure PrepareToolbarAndImagelistIcos;
    procedure UpdateFileTypeAssociations;
    procedure FileTypeIconClick(Sender: TObject);
    {$ENDIF}
    procedure SetLayoutPage(const APage: TUWLayout);
    procedure FillComboWithShortcuts;
  public

  end;

var
  frmSettings: TfrmSettings;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procCommon, UWSystem.SysUtils, ActnList,
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
  FAppStringList: TAppStringList = NIL;
  i: Integer;
begin
  LoadLanguage(Self);

  LanguageManager.GetAppStringList('SettingsStrings', FAppStringList);
  with lstTree.Items do
  begin
    Add(GetString(FAppStringList, 'General'));
    Add(GetString(FAppStringList, 'Conventions'));
    Add(GetString(FAppStringList, 'Appearance'));
    Add(GetString(FAppStringList, 'Toolbar'));
    Add(GetString(FAppStringList, 'Shortcuts'));
    Add(GetString(FAppStringList, 'MPV'));
    Add(GetString(FAppStringList, 'Tools'));
    {$IFDEF WINDOWS}
    Add(GetString(FAppStringList, 'FileTypeAssociations'));
    {$ENDIF}
  end;
  lstTree.ItemIndex := 0;
  with cboPauseMode.Items do
  begin
    BeginUpdate;
    Add(GetString(FAppStringList, 'Milliseconds'));
    Add(GetString(FAppStringList, 'Frames'));
    EndUpdate;
  end;

  FillComboWithLanguageFiles(cboLanguage);
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

  with LanguageManager do
  begin
    cboTheme.AddItem(GetString(FAppStringList, 'AutoMode'), NIL);
    cboTheme.AddItem(GetString(FAppStringList, 'LightMode'), NIL);
    cboTheme.AddItem(GetString(FAppStringList, 'DarkMode'), NIL);
    cboListMode.AddItem(GetString(FAppStringList, 'ListMode'), NIL);
    cboListMode.AddItem(GetString(FAppStringList, 'BlockMode'), NIL);
  end;
  FAppStringList.Free;

  with AppOptions do
  begin
    cboLanguage.ItemIndex := cboLanguage.Items.IndexOf(GetCultureDisplayName(Language));
    edtWebReference.Text  := WebSearchURL;
    with Conventions do
    begin
      if PauseInFrames then
        cboPauseMode.ItemIndex := 1
      else
        cboPauseMode.ItemIndex := 0;

      spnMinDurationMs.Value      := MinDuration;
      spnMinDurationPerWord.Value := MinDurationPerWord;
      spnMaxDurationMs.Value      := MaxDuration;
      spnMaxLineCount.Value       := MaxLines;
      spnSubtitlePauseMs.Value    := MinPause;
      edtProhibitedChars.Text     := ProhibitedChars;
      edtRepeatableChars.Text     := RepeatableChars;
      spnCPS.Value                := MaxCPS;
      spnWPM.Value                := WPM;
      spnCPL.Value                := CPL;
      edtCPSStrategy.Text         := CPSLineLenStrategy;
      spnNewSubtitleMs.Value      := NewSubtitleMs;
      chkDotsOnSplit.Checked      := DotsOnSplit;
    end;
    spnShiftTimeMs.Value        := ShiftTimeMS;
    spnAutoBackupMinutes.Value  := AutoBackupSeconds div 60;
    chkShowSplashWindow.Checked := ShowWelcomeAtStartup;
    chkUseOwnFileDialog.Checked := UseOwnFileDialog;
    chkDrawTags.Checked                 := VSTOptions.DrawTags;
    chkDrawErrors.Checked               := VSTOptions.DrawErrors;
    chkShowCPSBar.Checked               := frmMain.mmoText.CPSBar.Visible;
    chkDrawWaveformGAP.Checked          := frmMain.WAVE.DrawGAP;
    chkPromptForDeleteSubtitles.Checked := AskForDeleteLines;

    cboTheme.ItemIndex := Integer(ColorThemeInstance.ColorMode);
  end;

  with MPVOptions do
  begin
    chkAutoPlay.Checked          := AutoStartPlaying;
    spnSeekTime.Value            := SeekTime;
    spnFrameStep.Value           := FrameStep;

    cbnSub.ButtonColor           := HexStrToColor(TextColor);
    cbnSubBorder.ButtonColor     := HexStrToColor(TextBorderColor);
    cbnSubShadow.ButtonColor     := HexStrToColor(TextShadowColor);
    chkSubShadow.Checked         := UseTextShadowColor;
    cbnSubBackground.ButtonColor := HexStrToColor(TextBackgroundColor);
    chkSubBackground.Checked     := UseTextBackgroundColor;
    spnSubSize.Value             := TextSize;
  end;

  with frmMain.WAVE do
  begin
    cbnWaveColor.ButtonColor := CustomColors.Waveform;
    cbnWaveStart.ButtonColor := CustomColors.ItemIT;
    cbnWaveEnd.ButtonColor   := CustomColors.ItemFT;
  end;
  edtFFMPEG.Text := WAVEOptions.ffmpegFolder;

  cboTimeCodeMode.ItemIndex := Integer(Workspace.WorkMode);
  cboListMode.ItemIndex := Integer(VSTOptions.DrawMode);

  FProfiles := TProfiles.Create(ConventionsFileName);
  FillItemsWithConventions(cboProfile.Items, FProfiles);
  cboProfile.Items.Insert(0, LanguageManager.GetAppString('Custom'));
  i := cboProfile.Items.IndexOf(AppOptions.Conventions.Name);
  if i >= 0 then
    cboProfile.ItemIndex := i
  else
    cboProfile.ItemIndex := 0;

  hkShortcut.HotKey := 0;
  {$IFNDEF WINDOWS}
  cboTheme.Enabled := False;
  {$ENDIF}

  with frmMain do
  begin
    // Main
    for i := 0 to tlbMain_.ButtonCount-1 do
      if tlbMain_.Buttons[i].Visible and (tlbMain_.Buttons[i].Style = tbsCheck) then
        tlbMain_.Buttons[i].Down := ToolBarMain.Buttons[i].Visible;

    // Editor toolbar
    for i := 0 to tlbEditor_.ButtonCount-1 do
      if tlbEditor_.Buttons[i].Visible and (tlbEditor_.Buttons[i].Style = tbsCheck) then
        tlbEditor_.Buttons[i].Down := ToolBarEditor.Buttons[i].Visible;

    // Video toolbar
    for i := 0 to tlbVideo_.ButtonCount-1 do
      if tlbVideo_.Buttons[i].Visible and (tlbVideo_.Buttons[i].Style = tbsCheck) then
        tlbVideo_.Buttons[i].Down := ToolBarVideo.Buttons[i].Visible;

    // Wave toolbar
    for i := 0 to tlbWaveform_.ButtonCount-1 do
      if tlbWaveform_.Buttons[i].Visible and (tlbWaveform_.Buttons[i].Style = tbsCheck) then
        tlbWaveform_.Buttons[i].Down := ToolBarWaveform.Buttons[i].Visible;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
begin
  with AppOptions do
  begin
    Language     := GetCultureName(cboLanguage.Text);
    WebSearchURL := edtWebReference.Text;
    with Conventions do
    begin
      if cboProfile.ItemIndex > 0 then
        Name             := cboProfile.Text
      else
        Name             := '';
      PauseInFrames      := cboPauseMode.ItemIndex = 1;
      MinDuration        := spnMinDurationMs.Value;
      MaxDuration        := spnMaxDurationMs.Value;
      ProhibitedChars    := edtProhibitedChars.Text;
      RepeatableChars    := edtRepeatableChars.Text;
      MaxLines           := spnMaxLineCount.Value;
      MinDurationPerWord := spnMinDurationPerWord.Value;
      MinPause           := spnSubtitlePauseMs.Value;
      MaxCPS             := spnCPS.Value;
      WPM                := spnWPM.Value;
      CPL                := spnCPL.Value;
      CPSLineLenStrategy := edtCPSStrategy.Text;
      NewSubtitleMS      := spnNewSubtitleMs.Value;
      DotsOnSplit        := chkDotsOnSplit.Checked;

      if PauseInFrames then
        frmMain.WAVE.MinimumBlank := FramesToTime(MinPause, Workspace.FPS.OutputFPS)
      else
        frmMain.WAVE.MinimumBlank := MinPause;

      frmMain.mmoText.CPSBar.Max := MaxCPS;
    end;
    ShiftTimeMS           := spnShiftTimeMs.Value;
    AutoBackupSeconds     := spnAutoBackupMinutes.Value*60;
    ShowWelcomeAtStartup  := chkShowSplashWindow.Checked;
    UseOwnFileDialog      := chkUseOwnFileDialog.Checked;
    VSTOptions.DrawTags   := chkDrawTags.Checked;
    VSTOptions.DrawErrors := chkDrawErrors.Checked;
    frmMain.WAVE.DrawGAP  := chkDrawWaveformGAP.Checked;
    frmMain.mmoText.CPSBar.Visible        := chkShowCPSBar.Checked;
    frmMain.mmoTranslation.CPSBar.Visible := chkShowCPSBar.Checked;
    AskForDeleteLines     := chkPromptForDeleteSubtitles.Checked;
  end;

  with MPVOptions do
  begin
    AutoStartPlaying       := chkAutoPlay.Checked;
    SeekTime               := spnSeekTime.Value;
    FrameStep              := spnFrameStep.Value;
    TextColor              := ColorToHexStr(cbnSub.ButtonColor);
    TextBorderColor        := ColorToHexStr(cbnSubBorder.ButtonColor);
    TextShadowColor        := ColorToHexStr(cbnSubShadow.ButtonColor);
    UseTextShadowColor     := chkSubShadow.Checked;
    TextBackgroundColor    := ColorToHexStr(cbnSubBackground.ButtonColor);
    UseTextBackgroundColor := chkSubBackground.Checked;
    TextSize               := spnSubSize.Value;
  end;

  with frmMain.WAVE do
  begin
    CustomColors.Waveform := cbnWaveColor.ButtonColor;
    CustomColors.ItemIT   := cbnWaveStart.ButtonColor;
    CustomColors.ItemFT   := cbnWaveEnd.ButtonColor;
  end;
  WAVEOptions.ffmpegFolder := edtFFMPEG.Text;

  SetWorkMode(TWorkMode(cboTimeCodeMode.ItemIndex));
  Workspace.FPS.DefFPS  := StrToFloatDef(cboDefaultFrameRate.Text, Workspace.FPS.OutputFPS, AppOptions.FormatSettings);
  Workspace.DefEncoding := cboDefaultFileEncoding.ItemIndex;
  Workspace.DefFormat   := TUWSubtitleFormats(cboDefaultFileFormat.ItemIndex+1);
  if SubtitleInfo.Text.FileName.IsEmpty then
    frmMain.cboFormat.ItemIndex := Integer(Workspace.DefFormat)-1;

  with frmMain do
  begin
    // Main
    for i := 0 to tlbMain_.ButtonCount-1 do
      if tlbMain_.Buttons[i].Visible and (tlbMain_.Buttons[i].Style = tbsCheck) then
        ToolBarMain.Buttons[i].Visible := tlbMain_.Buttons[i].Down;

    // Editor toolbar
    for i := 0 to tlbEditor_.ButtonCount-1 do
      if tlbEditor_.Buttons[i].Visible and (tlbEditor_.Buttons[i].Style = tbsCheck) then
        ToolBarEditor.Buttons[i].Visible := tlbEditor_.Buttons[i].Down;

    // Video toolbar
    for i := 0 to tlbVideo_.ButtonCount-1 do
      if tlbVideo_.Buttons[i].Visible and (tlbVideo_.Buttons[i].Style = tbsCheck) then
        ToolBarVideo.Buttons[i].Visible := tlbVideo_.Buttons[i].Down;

    // Wave toolbar
    for i := 0 to tlbWaveform_.ButtonCount-1 do
      if tlbWaveform_.Buttons[i].Visible and (tlbWaveform_.Buttons[i].Style = tbsCheck) then
        ToolBarWaveform.Buttons[i].Visible := tlbWaveform_.Buttons[i].Down;

    // Dividers
    UpdateToolBarButtons(True);
  end;
  UpdateToolBarButtons(False);

  //if not frmMain.Enabled then
  frmMain.cboInputFPS.ItemIndex := cboDefaultFrameRate.ItemIndex;
  frmMain.cboFPS.ItemIndex      := cboDefaultFrameRate.ItemIndex;

  frmMain.VSTDrawInitialize(TVSTDrawMode(cboListMode.ItemIndex));
  frmMain.VSTResize(NIL);

  ColorThemeInstance.ColorMode := TColorMode(cboTheme.ItemIndex);
  CheckColorTheme;

  SetLength(FShortCutListIndex, 0);
  AppOptions.ShortCutPreset := cboShortCutPreset.Text + '.key';

  UpdateMPVOptions;

  {$IFDEF WINDOWS}
  UpdateFileTypeAssociations;
  {$ENDIF}

  FProfiles.Free;
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
        TUWLayout(C).Left := 168;
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
    3: SetLayoutPage(lyoToolbar);
    4: SetLayoutPage(lyoShortcuts);
    5: SetLayoutPage(lyoMPV);
    6: SetLayoutPage(lyoTools);
    {$IFDEF WINDOWS}
    7: SetLayoutPage(lyoFileTypeAssociations);
    {$ENDIF}
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnFFMPEGClick(Sender: TObject);
begin
  with TSelectDirectoryDialog.Create(Self) do
  try
    Options := Options + [ofPathMustExist, ofOldStyleDialog];

    if Execute then
    begin
      edtFFMPEG.Text := FileName;
    end;
  finally
    Free;
  end;
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
    cboProfile.Items.Insert(0, LanguageManager.GetAppString('Custom'));
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
  s, sc, cat: String;
begin
  cat := cboShortcutCat.Text;
  lstShortcuts.Clear;
  with frmMain do
    for i := 0 to ActionList.ActionCount-1 do
    begin
      with TAction(ActionList.Actions[i]) do
      begin
        if cat = Category then
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
        if cboShortcutCat.Items.IndexOf(Category) < 0 then
          cboShortcutCat.AddItem(Category, NIL);
      end;

    if cboShortcutCat.Items.Count > 0 then cboShortcutCat.ItemIndex := 0;
    cboShortcutCatSelect(NIL);
  end;
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

procedure TfrmSettings.hkShortcutChange(Sender: TObject);
begin
  if (lstShortcuts.ItemIndex >= 0) then
    with TAction(frmMain.ActionList.Actions[FShortCutListIndex[lstShortcuts.ItemIndex]]) do
      if (ShortCut <> hkShortcut.HotKey) then
      begin
        ShortCut := hkShortcut.HotKey;
        lstShortcuts.Items[lstShortcuts.ItemIndex] := Caption + ' [' + ShortCutToTextEx(ShortCut) + ']';

        btnShortCutApply.Enabled := True;
      end;
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
  s := InputDialog(GetCommonString('NewShortCutPreset'), GetCommonString('NewShortCutPresetName'), '');
  if not s.IsEmpty then
  begin
    f := ConcatPaths([ShortCutFolder, s + '.key']);
    if not FileExists(f) then
    begin
      SaveShortCutsToFile(f, frmMain.ActionList);
      cboShortCutPreset.ItemIndex := cboShortCutPreset.Items.Add(s);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.cboProfileSelect(Sender: TObject);
var
  Profile: PProfileItem;
begin
  if cboProfile.ItemIndex = 0 then
    Profile := @AppOptions.Conventions
  else if (FProfiles.Items.Count > 0) and (cboProfile.ItemIndex > 0) and (cboProfile.ItemIndex < FProfiles.Items.Count) then
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
    spnNewSubtitleMs.Value      := NewSubtitleMs;
    spnMinDurationMs.Value      := MinDuration;
    spnMinDurationPerWord.Value := MinDurationPerWord;
    spnMaxDurationMs.Value      := MaxDuration;
    spnMaxLineCount.Value       := MaxLines;
    spnSubtitlePauseMs.Value    := MinPause;
    spnCPS.Value                := MaxCPS;
    spnWPM.Value                := WPM;
    spnCPL.Value                := CPL;
    chkDotsOnSplit.Checked      := DotsOnSplit;
    edtCPSStrategy.Text         := CPSLineLenStrategy;

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

end.

