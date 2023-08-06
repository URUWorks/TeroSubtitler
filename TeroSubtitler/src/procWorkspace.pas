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

unit procWorkspace;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, StdCtrls, Controls, ComCtrls, SysUtils, Menus, Forms, procTypes,
  UWTimeEdit, UWMemo, UWSubtitleAPI, procColorTheme, ActnList, procConventions;

procedure CheckColorTheme(const AForm: TForm); overload;
procedure CheckColorTheme; overload;

procedure AddFPSToCombo(const FPS: Single; const Combo: TComboBox);
procedure FillComboWithLanguageFiles(const Combo: TComboBox);
procedure FillComboWithFPS(const Combo: TComboBox; const ADefault: Single = -1);
procedure FillComboWithEncodings(const Combo: TComboBox);
procedure FillComboWithFormats(const Combo: TComboBox);
procedure FillComboWithOCRScripts(const ACombo: TComboBox; const AIndex: Integer = 0);
procedure FillComboWithGoogleLanguages(const Combo: TCombobox; const Index: Integer = 0);
procedure FillItemsWithConventions(const AItems: TStrings; AProfiles: TProfiles = NIL);
procedure FillComboWithShortCutsPreset(const ACombo: TComboBox);
procedure FillComboWithModels(const Combo: TComboBox);
procedure FillComboWithCustomFormat(const Combo: TComboBox; const AExtension: String = '*.cft');
procedure FillWithDictionaries(const AMenu: TMenuItem; const ACombo: TComboBox);
procedure FillMenuWithPlayRate(const AParent: TComponent);
procedure FillMenuWithLoopCount(const AParent: TComponent);
procedure FillMenuWithAudioStreams(const AParent: TComponent);
procedure FillComboWithAudioStreams(const ACombo: TComboBox);
procedure FillMenuWithUnicodeSymbols(const AParent: TComponent);
procedure FillMenuWithSilentZone(const AParent: TComponent);

function IsWorkAreaEnabled: Boolean;
procedure EnableWorkArea(const AValue: Boolean = True);
procedure EnableActionsByTag(const ATags: array of Integer; const AValue: Boolean);
procedure EnableTimerAutoBackup(const AValue: Boolean = True);
procedure SetViewMode(const AViewMode: TViewMode);
procedure SetTranslatorMode(const AValue: Boolean);
procedure SwapWorkspaceLayout;
procedure SetVideoPreview(const AValue: Boolean);
procedure SetWaveformPreview(const AValue: Boolean);
procedure SetDockVideoWindow(const AValue: Boolean);
procedure SetDockWaveformWindow(const AValue: Boolean);
function GetTimeEditMode: TUWTimeEditMode;
procedure SetTimeFPStoTimeEditCtrls;
procedure SetWorkMode(const Mode: TWorkMode; const AUpdate: Boolean = True);

procedure FocusMemo(const SelectText: Boolean = True);
function GetMemoFocused: TUWMemo;
function GetMemoWordUnderCaret(const Memo: TUWMemo; const SelectWord: Boolean = False): String;
function GetFPS: Single;
function GetDefPause: Integer;

procedure SetActor;

procedure UpdateVideoLengthString;

procedure GetTranslationMemoryAtIndex(const Index: Integer);
procedure CheckForTranslationMemory(const AIndex: Integer);
procedure CheckForTerminology(const AIndex: Integer);

procedure RefreshAppTitle;

procedure SetStatusBarText(const AText: String; const APanelIndex: Integer = 0; const AUseTimer: Boolean = True);

procedure DoAutoCheckErrors(const AAll: Boolean = True; const ARefresh: Boolean = False);

procedure UpdateStatusBar;
procedure UpdateCPSAndTexts(const AIndex: Integer);
procedure UpdateValuesForSubtitle(const AIndex: Integer);
procedure UpdateValues(const AInvalidate: Boolean = False);

procedure UpdateToolBarButtons(const AOnlyDivider: Boolean);
procedure UpdateCoolBar(const ABand: Integer; const AVisible: Boolean);

// -----------------------------------------------------------------------------

implementation

uses
  procVST, procCommon, formMain, formVideo, formWaveform, procSubtitle,
  procUnDockVideoControls, UWSystem.Globalization, UWSystem.SysUtils,
  UWSystem.StrUtils, UWSystem.Encoding, UWSubtitleAPI.Formats, MPVPlayer,
  character, LazUTF8, formTranslationMemory, UWSubtitleAPI.Tags,
  UWTranslateAPI.Google, procTranscription, formCustomQuestionDlg,
  UWSystem.TimeUtils, formTBX, procVST_Loops;

// -----------------------------------------------------------------------------

procedure CheckColorTheme(const AForm: TForm);
begin
  with frmMain do
    ColorThemeInstance.Apply(AForm, ColorThemeInstance.ColorMode, ImageList_Dark, ImageList_Default);
end;

// -----------------------------------------------------------------------------

procedure CheckColorTheme;
begin
  with frmMain do
    ColorThemeInstance.Apply(ColorThemeInstance.ColorMode, ImageList_Dark, ImageList_Default);
end;

// -----------------------------------------------------------------------------

procedure AddFPSToCombo(const FPS: Single; const Combo: TComboBox);
var
  FPSStr : String;
  Index  : Integer;
begin
  if FPS > 0 then
  begin
    FPSStr := SingleToStr(FPS, AppOptions.FormatSettings);
    Index := Combo.Items.IndexOf(FPSStr);
    if (Index = -1) and (FPSStr <> '') then
      Combo.ItemIndex := Combo.Items.Add(FPSStr);
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithLanguageFiles(const Combo: TComboBox);
var
  SearchRec: TSearchRec;
begin
  if SysUtils.FindFirst(LanguageFolder + '*.xml', faAnyFile, SearchRec) = 0 then
  try
    repeat
      Combo.Items.Add(GetCultureDisplayName(AnsiString(ChangeFileExt(SearchRec.Name, ''))));
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithFPS(const Combo: TComboBox; const ADefault: Single = -1);
var
  i: Byte;
begin
  with Combo, AppOptions do
  begin
    Items.BeginUpdate;
    Clear;
    for i := 0 to Length(DefFPSList)-1 do Items.Add(SingleToStr(DefFPSList[i], FormatSettings));
    if ADefault > -1 then
      ItemIndex := Items.IndexOf(SingleToStr(ADefault, FormatSettings));
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithEncodings(const Combo: TComboBox);
var
  i: Byte;
begin
  with Combo do
  begin
    Items.BeginUpdate;
    Clear;
    for i := 0 to MaxEncodings-1 do Items.Add(
      Format('%d: %s', [Encodings[i].CPID, Encodings[i].CPName]));

    ItemIndex := Items.Count-1;
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithFormats(const Combo: TComboBox);
var
  Formats: TStrings;
begin
  with Combo do
  begin
    Items.BeginUpdate;
    Clear;
    Formats := NIL;
    AddFormatsToStrings(Formats);
    try
      Items.Assign(Formats);
    finally
      Formats.Free;
    end;
    ItemIndex := Integer(Workspace.DefFormat)-1;
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithOCRScripts(const ACombo: TComboBox; const AIndex: Integer = 0);
var
  SearchRec : TSearchRec;
begin
  ACombo.Items.BeginUpdate;
  try
    if FindFirst(OCRFolder + '*.ocr', faAnyFile, SearchRec) = 0 then
    try
      repeat
          ACombo.Items.Add(ChangeFileExt(SearchRec.Name, ''));
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  finally
    if ACombo.Items.Count > 0 then ACombo.ItemIndex := AIndex;
    ACombo.Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithGoogleLanguages(const Combo: TCombobox; const Index: Integer = 0);
var
  i: Integer;
begin
  with Combo do
  begin
    Items.BeginUpdate;
    Clear;
    for i := 0 to Length(GoogleTranslateName)-1 do
      Items.Add(GoogleTranslateName[i]);
    ItemIndex := Index;
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillItemsWithConventions(const AItems: TStrings; AProfiles: TProfiles = NIL);
var
  AFreeOnExit: Boolean;
begin
  if AProfiles = NIL then
  begin
    AProfiles   := TProfiles.Create(ConventionsFileName);
    AFreeOnExit := True;
  end
  else
    AFreeOnExit := False;

  try
    AProfiles.FillTStrings(AItems);
  finally
    if AFreeOnExit then
      AProfiles.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithShortCutsPreset(const ACombo: TComboBox);
var
  SearchRec : TSearchRec;
  i, x : Integer;
begin
  ACombo.Items.BeginUpdate;
  try
    i := -1;
    if FindFirst(ShortCutFolder + '*.key', faAnyFile, SearchRec) = 0 then
    try
      repeat
          x := ACombo.Items.Add(ChangeFileExt(SearchRec.Name, ''));
          if SearchRec.Name = AppOptions.ShortCutPreset then
            i := x;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  finally
    if i > -1 then ACombo.ItemIndex := i;
    ACombo.Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithModels(const Combo: TComboBox);
var
  SearchRec: TSearchRec;
begin
  if SysUtils.FindFirst(WhisperModelsFolder + '*.bin', faAnyFile, SearchRec) = 0 then
  try
    Combo.Items.BeginUpdate;
    Combo.Items.Clear;
    repeat
      Combo.Items.Add(ChangeFileExt(SearchRec.Name, ''));
    until FindNext(SearchRec) <> 0;
  finally
    if Combo.Items.Count > 0 then
      Combo.ItemIndex := 0;
    Combo.Items.EndUpdate;
    FindClose(SearchRec);
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithCustomFormat(const Combo: TComboBox; const AExtension: String = '*.cft');
var
  SearchRec: TSearchRec;
begin
  if SysUtils.FindFirst(CustomFormatFolder + AExtension, faAnyFile, SearchRec) = 0 then
  try
    Combo.Items.BeginUpdate;
    Combo.Items.Clear;
    repeat
      Combo.Items.Add(ChangeFileExt(SearchRec.Name, ''));
    until FindNext(SearchRec) <> 0;
  finally
    if Combo.Items.Count > 0 then
      Combo.ItemIndex := 0;
    Combo.Items.EndUpdate;
    FindClose(SearchRec);
  end;
end;

// -----------------------------------------------------------------------------

procedure FillWithDictionaries(const AMenu: TMenuItem; const ACombo: TComboBox);
var
  SearchRec : TSearchRec;
  mnu       : TMenuItem;
  x, i, l   : Integer;
  s, s1     : String;
begin
  if FindFirst(DictionariesFolder + '*.dic', faAnyFile, SearchRec) = 0 then
  try
    x := 0;
    repeat
      s := ChangeFileExt(SearchRec.Name, '');
      if AMenu <> NIL then
      begin
        l := Pos(' ', s);
        if l > 0 then
          s1 := Copy(s, 1, l-1)
        else
          s1 := s;

        mnu         := TMenuItem.Create(AMenu);
        mnu.Name    := 'dic_' + IntToStr(x);
        mnu.Caption := Trim(GetCultureDisplayName(ReplaceString(s1, '_', '-')) + ' [' + s + ']');
        mnu.OnClick := @frmMain.DoDictionaryItemClick;
        if s = AppOptions.HunspellLanguage then
          mnu.Checked := True
        else
          mnu.Checked := False;
        AMenu.Insert(x, mnu);
      end
      else if ACombo <> NIL then
      begin
        i := ACombo.Items.Add(Trim(GetCultureDisplayName(ReplaceString(s, '_', '-')) + ' [' + s + ']'));
        if s = AppOptions.HunspellLanguage then ACombo.ItemIndex := i;
      end;
     inc(x);
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

// -----------------------------------------------------------------------------

procedure FillMenuWithPlayRate(const AParent: TComponent);
var
  i: Integer;
  Item: TMenuItem;
begin
  if AParent = NIL then Exit;
  i := 30;
  while i < 260 do
  begin
    Item := TMenuItem.Create(AParent);
    Item.Caption := IntToStr(i) + '%';
    Item.OnClick := @frmMain.PlayRateClick;
    if AParent is TPopupMenu then
      with AParent as TPopupMenu do
        Items.Add(Item)
    else if AParent is TMenuItem then
      with AParent as TMenuItem do
        Add(Item);

    Inc(i, 10);
  end;
end;

// -----------------------------------------------------------------------------

procedure FillMenuWithLoopCount(const AParent: TComponent);
var
  i: Integer;
  Item: TMenuItem;
begin
  if AParent = NIL then Exit;
  i := 1;
  while i < 11 do
  begin
    Item := TMenuItem.Create(AParent);
    Item.Caption := IntToStr(i);
    Item.OnClick := @frmMain.LoopCountClick;
    if AParent is TPopupMenu then
      with AParent as TPopupMenu do
        Items.Add(Item)
    else if AParent is TMenuItem then
      with AParent as TMenuItem do
        Add(Item);

    Inc(i);
  end;
end;

// -----------------------------------------------------------------------------

procedure FillMenuWithAudioStreams(const AParent: TComponent);
var
  i, tl: Integer;
  Item: TMenuItem;
  s: String;
begin
  if AParent = NIL then Exit;

  (AParent as TMenuItem).Clear;

  tl := Length(frmMain.MPV.TrackList);
  if tl > 0 then
  begin
    for i := 0 to tl-1 do
    begin
      if frmMain.MPV.TrackList[i].Kind = ttAudio then
      begin
        Item := TMenuItem.Create(AParent);

        s := IntToStr(frmMain.MPV.TrackList[i].ID) + ': ';
        if frmMain.MPV.TrackList[i].Title <> '' then s := s + frmMain.MPV.TrackList[i].Title + ', ';
        if frmMain.MPV.TrackList[i].Lang <> '' then s := s + frmMain.MPV.TrackList[i].Lang + ', ';
        if frmMain.MPV.TrackList[i].Decoder <> '' then s := s + frmMain.MPV.TrackList[i].Decoder;
        //if frmMain.MPV.TrackList[i].Codec <> '' then s := s + ' [' + frmMain.MPV.TrackList[i].Codec + ']';
        //if frmMain.MPV.TrackList[i].Channels <> '' then s := s + ' [' + frmMain.MPV.TrackList[i].Channels + ']';

        Item.Caption := s;
        Item.Tag := frmMain.MPV.TrackList[i].ID;
        Item.Checked := frmMain.MPV.TrackList[i].Selected;
        Item.OnClick := @frmMain.DoAudioTrackSet;
        if AParent is TPopupMenu then
          with AParent as TPopupMenu do
            Items.Add(Item)
        else if AParent is TMenuItem then
          with AParent as TMenuItem do
            Add(Item);
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithAudioStreams(const ACombo: TComboBox);
var
  i, tl: Integer;
  s: String;
begin
  with ACombo do
  begin
    Items.BeginUpdate;
    Clear;
    tl := Length(frmMain.MPV.TrackList);
    if tl > 0 then
    begin
      for i := 0 to tl-1 do
      begin
        if frmMain.MPV.TrackList[i].Kind = ttAudio then
        begin
          s := IntToStr(frmMain.MPV.TrackList[i].ID) + ': ';
          if frmMain.MPV.TrackList[i].Title <> '' then s := s + frmMain.MPV.TrackList[i].Title + ', ';
          if frmMain.MPV.TrackList[i].Lang <> '' then s := s + frmMain.MPV.TrackList[i].Lang + ', ';
          if frmMain.MPV.TrackList[i].Decoder <> '' then s := s + frmMain.MPV.TrackList[i].Decoder;
          Items.Add(s);
        end;
      end;
    end;
    if Items.Count > 0 then ItemIndex := 0;
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillMenuWithUnicodeSymbols(const AParent: TComponent);
var
  i: Integer;
  Item: TMenuItem;
begin
  if AParent = NIL then Exit;
  for i := 0 to Length(TUnicodeSymbols)-1 do
  begin
    Item := TMenuItem.Create(AParent);
    Item.Caption := TUnicodeSymbols[i];
    Item.OnClick := @frmMain.DoUnicodeSymbolClick;
    if AParent is TPopupMenu then
      with AParent as TPopupMenu do
        Items.Add(Item)
    else if AParent is TMenuItem then
      with AParent as TMenuItem do
        Add(Item);
  end;
end;

// -----------------------------------------------------------------------------

procedure FillMenuWithSilentZone(const AParent: TComponent);
var
  i: Integer;
  Item: TMenuItem;
begin
  if AParent = NIL then Exit;
  with frmMain do
    if WAVE.DetectSilentZone then
    begin
      for i := 0 to Length(WAVE.SilentZones)-1 do
      begin
        Item := TMenuItem.Create(AParent);
        with WAVE.SilentZones[i]^ do
          Item.Caption := Format('%s-%s', [GetTimeStr(Start), GetTimeStr(Stop)]);
          //Item.Caption := Format('%s -> %s (%.1f)', [GetTimeStr(Start),
          //  GetTimeStr(Stop), (RmsSum / RmsCount)]);

        Item.Tag := i;
        Item.OnClick := @frmMain.DoSilentZoneClick;
        if AParent is TPopupMenu then
          with AParent as TPopupMenu do
            Items.Add(Item)
        else if AParent is TMenuItem then
          with AParent as TMenuItem do
            Add(Item);
      end;
    end;
end;

// -----------------------------------------------------------------------------

function IsWorkAreaEnabled: Boolean;
begin
  with frmMain do
    Result := VST.Enabled; // and LayoutVST.Visible;
end;

// -----------------------------------------------------------------------------

procedure EnableWorkArea(const AValue: Boolean = True);
var
  i: Integer;
begin
  with frmMain do
  begin
    // Components
    VST.Enabled            := AValue;
    mmoSourceView.Enabled  := AValue;
    mmoText.Enabled        := AValue;
    mmoTranslation.Enabled := AValue;
    btnInitialTime.Enabled := AValue;
    btnFinalTime.Enabled   := AValue;
    btnDuration.Enabled    := AValue;
    btnPause.Enabled       := AValue;
    tedInitial.Enabled     := AValue;
    tedFinal.Enabled       := AValue;
    tedDuration.Enabled    := AValue;
    tedPause.Enabled       := AValue;
    sbrSeek.Enabled        := AValue;
    cboActor.Enabled       := AValue;

    if frmMain.Visible and VST.Enabled and mmoText.Enabled and LayoutVST.Visible then
    begin
      VSTSelectNode(VST, 0, True);
      mmoText.SetFocus;
    end;
    UpdateCPSAndTexts(VSTFocusedNode(VST));
    UpdateStatusBar;

    // Enable/Disable actions by tag
    for i := 0 to ActionList.ActionCount-1 do
       with (ActionList[i] as TAction) do
       begin
         if (Tag and TAG_ACTION_BYVAL) = TAG_ACTION_BYVAL then
           Enabled := AValue
         else if Tag <> TAG_ACTION_ALWAYSENABLED then
         begin
           //if (Tag and TAG_ACTION_VIDEO) = TAG_ACTION_VIDEO then
           //  Enabled := actCloseVideo.Enabled
           //else if (Tag and TAG_ACTION_AUDIO) = TAG_ACTION_AUDIO then
           //  Enabled := WAVE.IsEnabled
           //else
             Enabled := False;
         end;
       end;
    // Actions
    actUndo.Enabled := False;
    actRedo.Enabled := False;
    actCloseVideo.Enabled := MPV.GetMediaLenInMs > 0;

    // Find
    if AValue then
    begin
      actFindNext.Enabled     := not AppOptions.TextToFind.IsEmpty;
      actFindPrevious.Enabled := actFindNext.Enabled;
    end;

    tlbValidate.Visible := TMX.Ready;
    mnuEditInsertSymbol.Enabled := AValue;

    // Backup Timer
    EnableTimerAutoBackup(AValue);
  end;
end;

// -----------------------------------------------------------------------------

procedure EnableActionsByTag(const ATags: array of Integer; const AValue: Boolean);
var
  i, c: Integer;
begin
  with frmMain do
    for i := 0 to ActionList.ActionCount-1 do
       with (ActionList[i] as TAction) do
         for c := 0 to Length(ATags)-1 do
           if (Tag and ATags[c]) = ATags[c] then
             Enabled := AValue;
end;

// -----------------------------------------------------------------------------

procedure EnableTimerAutoBackup(const AValue: Boolean = True);
begin
  with frmMain.TimerAutoBackup do
  begin
    Interval := 1000 * AppOptions.AutoBackupSeconds;
    Enabled  := AValue;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetViewMode(const AViewMode: TViewMode);
var
  r: Integer;
begin
  // show requested layout (like a pagecontrol)
  if Workspace.ViewMode = AViewMode then Exit;

  if (Workspace.ViewMode = vmTranscription) and (AViewMode <> vmTranscription) then
  begin
    with Workspace.Transcription do
      if Assigned(Memo) and (Memo.Modified) and
        (CustomQuestionDialog('CommonStrings', 'TranscriptionModeExit', 'ContinueAnyway', [mbYes, mbNo]) = mrNo) then
        begin
          frmMain.actTranscriptionMode.Checked := True;
          Exit;
        end;
  end
  else if (Workspace.ViewMode = vmList) and (AViewMode = vmSource) then
  begin
    with frmMain do
      if Workspace.TranslatorMode and (TUWSubtitleFormats(cboFormat.ItemIndex+1) <> sfTeroSubtitler) then
      begin
        r := CustomQuestionDialog('CommonStrings', 'SourceModeWarning', 'ContinueAnyway', [mbYes, mbNo, mbCancel]);
        case r of
          mrCancel : begin
                       frmMain.actListMode.Checked := True;
                       Exit;
                     end;
          mrYes    : cboFormat.ItemIndex := Integer(sfTeroSubtitler)-1;
        end;
      end;
  end;

  Workspace.ViewMode := AViewMode;
  with frmMain do
    case AViewMode of
      vmList:
      begin
        LayoutVST.Align   := alClient;
        LayoutVST.Visible := True;
        LayoutSource.Hide;
        LayoutTranscription.Hide;
        TranscriptionUnInitializeControls;

        if (Subtitles.Count > 0) and not VST.Enabled then
          EnableWorkArea(True);

        if VST.Enabled and (mmoSourceView.Text <> '') then
        begin
          Subtitles.LoadFromString(mmoSourceView.Text, NIL, GetFPS, TUWSubtitleFormats(cboFormat.ItemIndex+1));
          VST.RootNodeCount := Subtitles.Count;
        end;

        actListMode.Checked := True;
        EnableActionsByTag([TAG_ACTION_TRANSCRIPTION], False);
        EnableActionsByTag([TAG_ACTION_BYVAL], True);
      end;

      vmSource:
      begin
        LayoutSource.Align   := alClient;
        LayoutSource.Visible := True;
        LayoutVST.Hide;
        LayoutTranscription.Hide;
        TranscriptionUnInitializeControls;

        if VST.Enabled and (Subtitles.Count > 0) then
        begin
          mmoSourceView.Tag := TAG_CONTROL_UPDATE;
          mmoSourceView.Text := Subtitles.SaveToString(GetFPS, NIL, TUWSubtitleFormats(cboFormat.ItemIndex+1), smText);
          mmoSourceView.Tag := TAG_CONTROL_NORMAL;
        end;

        actSourceMode.Checked := True;
        EnableActionsByTag([TAG_ACTION_TRANSCRIPTION], False);
        EnableActionsByTag([TAG_ACTION_BYVAL], True);
      end;

      vmTranscription:
      begin
        TranscriptionInitializeControls;
        LayoutTranscription.Align   := alClient;
        LayoutTranscription.Visible := True;
        LayoutVST.Hide;
        LayoutSource.Hide;

        actTranscriptionMode.Checked := True;
        EnableActionsByTag([TAG_ACTION_BYVAL], False);
        EnableActionsByTag([TAG_ACTION_TRANSCRIPTION], True);
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure SetTranslatorMode(const AValue: Boolean);
begin
  if Workspace.TranslatorMode <> AValue then
    Workspace.TranslatorMode := AValue;

  with frmMain do
  begin
    if actTranslatorMode.Checked <> AValue then actTranslatorMode.Checked := AValue;
    VSTShowColumn(VST, 5, AValue);
    mmoTranslation.Visible := AValue;
    if AValue then UpdateCPSAndTexts(VSTFocusedNode(VST));
    LayoutVSTResize(LayoutVST);
    VSTResize(VST);
  end;
  RefreshAppTitle;
end;

// -----------------------------------------------------------------------------

procedure SwapWorkspaceLayout;
begin
  with frmMain do
    if LayoutVideo.Parent = frmMain then
    begin
      if actSwapWorkspace.Checked then
      begin
        LayoutVideo.Align   := alLeft;
        SplitterVideo.Align := alLeft;
        LayoutVideo.Left    := 0;
      end
      else
      begin
        LayoutVideo.Align   := alRight;
        SplitterVideo.Align := alRight;
        SplitterVideo.Left  := 0;
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure SetVideoPreview(const AValue: Boolean);
begin
  with frmMain do
    if not Assigned(frmVideo) then
    begin
      actVideoPreview.Checked := AValue;
      LayoutVideo.Visible     := AValue;
      SplitterVideo.Visible   := AValue;
    end
    else
    begin
      actVideoPreview.Checked := AValue;
      LayoutVideo.Visible     := AValue;
      frmVideo.Visible        := AValue;
    end;
end;

// -----------------------------------------------------------------------------

procedure SetWaveformPreview(const AValue: Boolean);
begin
  with frmMain do
    if not Assigned(frmWaveform) then
    begin
      LayoutWaveform.Top         := 0;
      SplitterWaveform.Top       := 0;
      actWaveformPreview.Checked := AValue;
      LayoutWaveform.Visible     := AValue;
      SplitterWaveform.Visible   := AValue;
    end
    else
    begin
      actWaveformPreview.Checked := AValue;
      LayoutWaveform.Visible     := AValue;
      frmWaveform.Visible        := AValue;
    end;
end;

// -----------------------------------------------------------------------------

procedure SetDockVideoWindow(const AValue: Boolean);
begin
  if not AValue then
    UnDockVideoControls
  else
    DockVideoControls;
end;

// -----------------------------------------------------------------------------

procedure SetDockWaveformWindow(const AValue: Boolean);
begin
  if not AValue then
    UnDockWaveformControls
  else
    DockWaveformControls;
end;

// -----------------------------------------------------------------------------

function GetTimeEditMode: TUWTimeEditMode;
begin
  if Workspace.WorkMode = wmTime then
    Result := temTime
  else
    Result := temFrames;
end;

// -----------------------------------------------------------------------------

procedure SetTimeFPStoTimeEditCtrls;
var
  i: Integer;
begin
  with frmMain do
  begin
    for i := 0 to ComponentCount-1 do
      if Components[i] is TUWTimeEdit then
        with (Components[i] as TUWTimeEdit) do
          SetFPSValueOnly(Workspace.FPS.OutputFPS);

    WAVE.FPS := Workspace.FPS.OutputFPS;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetWorkMode(const Mode: TWorkMode; const AUpdate: Boolean = True);
var
  i: Integer;
begin
  with frmMain do
  begin
    VSTBeginUpdate(VST);
    try
      for i := 0 to ComponentCount-1 do
        if Components[i] is TUWTimeEdit then
          with (Components[i] as TUWTimeEdit) do
          begin
            FPS      := Workspace.FPS.OutputFPS;
            TimeMode := TUWTimeEditMode(Mode);
          end;

      WAVE.FPS := Workspace.FPS.OutputFPS;
      WAVE.FPSTimeMode := (Mode = wmFrames);

      //MPV.SMPTEMode := (Mode = wmFrames);

      if Mode = wmTime then
      begin
        Workspace.WorkMode    := wmTime;
        actTimeMode.Checked   := True;
        actFramesMode.Checked := False;
      end
      else
      begin
        Workspace.WorkMode    := wmFrames;
        actFramesMode.Checked := True;
        actTimeMode.Checked   := False;
      end;

      UpdateVideoLengthString;

      if AUpdate then UpdateValues(True); // update times
    finally
      VSTEndUpdate(VST);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure FocusMemo(const SelectText: Boolean = True);
var
  Memo: TUWMemo;
begin
  with frmMain do
  begin
    if Workspace.TranslatorMode then
      Memo := mmoTranslation
    else
      Memo := mmoText;

    if Memo.Enabled and LayoutVST.Visible then
    begin
      if SelectText then Memo.SelectAll;
      Memo.SetFocus;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function GetMemoFocused: TUWMemo;
begin
  with frmMain do
    if mmoText.Focused then
      Result := mmoText
    else if mmoTranslation.Focused then
      Result := mmoTranslation
    else
      Result := NIL;
end;

// -----------------------------------------------------------------------------

function GetMemoWordUnderCaret(const Memo: TUWMemo; const SelectWord: Boolean = False): String;
var
   Line     : Integer;
   Pos, i   : Integer;
   LineText : String;
   InitPos  : Integer;
   EndPos   : Integer;
begin
  if Memo.SelText <> '' then
    Result := Memo.SelText
  else
  begin
     //Get the caret position
     Line := Memo.CaretPos.y;
     Pos  := Memo.CaretPos.x;
     //Validate the line number
     if Memo.Lines.Count-1 < Line then Exit;
     //Get the text of the line
     LineText := Memo.Lines[Line];
     //Inc(Pos);
     InitPos := Pos;
     //search the initial position using the space symbol as separator
     while (InitPos > 0) and TCharacter.IsLetter(LineText, InitPos) do Dec(InitPos);
     Inc(Pos);
     EndPos := Pos;
     //search the final position using the space symbol as separator
     while (EndPos <= UTF8Length(LineText)) and TCharacter.IsLetter(LineText, EndPos) do Inc(EndPos);
     //Get the text
     Inc(InitPos);
     Result := UTF8Copy(LineText, InitPos, EndPos - InitPos);
     Dec(InitPos);
     //Finally select the text in the Memo if needed
     if SelectWord then
     begin
       if Line > 0 then
         for i := 0 to Line-1 do InitPos := InitPos + UTF8Length(Memo.Lines[i]) + UTF8Length(sLineBreak);

       Memo.SelStart  := InitPos;
       Memo.SelLength := UTF8Length(Result);
     end;
  end;
end;

// -----------------------------------------------------------------------------

function GetFPS: Single;
begin
  with frmMain.cboFPS, AppOptions do
    Result := StrToFloatDef(Text, Workspace.FPS.OutputFPS, FormatSettings);
end;

// -----------------------------------------------------------------------------

function GetDefPause: Integer;
begin
  with AppOptions.Conventions do
    if not PauseInFrames then
      Result := MinPause
    else
      Result := FramesToTime(MinPause, Workspace.FPS.OutputFPS);
end;

// -----------------------------------------------------------------------------

procedure SetActor;
begin
  with frmMain do
    if GetMemoFocused = NIL then
      VSTDoLoop(VST, @ApplyActor)
    else
    begin
      Subtitles.ItemPointer[VSTFocusedNode(VST)]^.Actor := cboActor.Text;
      UpdateValues(True);
    end;
end;

// -----------------------------------------------------------------------------

procedure UpdateVideoLengthString;
var
  s: String;
  l: Integer;
begin
  with frmMain, AppOptions do
    if MPV.Initialized then
    begin
      l := MPV.GetMediaLenInMs;

      if l > 0 then
      begin
        if not actSMPTE.Checked then
          s := GetLangString('VideoLen')
        else
          s := GetLangString('VideoLenSMPTE');

        lblMediaLength.Caption := Format(s, [GetTimeStr(MPV.GetMediaLenInMs), SingleToStr(MPV.GetVideoFPS, FormatSettings)]);
      end
      else
        lblMediaLength.Caption := '';
    end
    else
      lblMediaLength.Caption := '';
end;

// -----------------------------------------------------------------------------

procedure GetTranslationMemoryAtIndex(const Index: Integer);
var
  s: String;
begin
  if (frmTranslationMemory = NIL) or (frmMain.VST.SelectedCount <> 1) or (not Workspace.TranslatorMode) then Exit;
  s := frmTranslationMemory.GetItemAtIndex(Index);
  if not s.IsEmpty then frmMain.mmoTranslation.Text := s;
end;

// -----------------------------------------------------------------------------

procedure CheckForTranslationMemory(const AIndex: Integer);
begin
  with frmMain do
  begin
    if not Workspace.TranslatorMode and (TMX.Items.Count = 0) or
      (frmTranslationMemory = NIL) or (VST.SelectedCount <> 1) then Exit;

    if Subtitles[AIndex].Data > 0 then
      frmTranslationMemory.ShowSimilary(Subtitles[AIndex].Data-1)
    else
      frmTranslationMemory.FindSimilary(RemoveTSTags(Subtitles[AIndex].Text));
  end;
end;

// -----------------------------------------------------------------------------

procedure CheckForTerminology(const AIndex: Integer);
begin
  if (frmTBX <> NIL) then
  begin
    TBX.FindAllTerms(Subtitles.Text[AIndex]);
    frmTBX.UpdateTermList;
  end;
end;

// -----------------------------------------------------------------------------

procedure RefreshAppTitle;
var
  s: String;
begin
  s := ExtractFileName(SubtitleInfo.Text.FileName);
  if not s.IsEmpty then
  begin
    if Workspace.TranslatorMode then
      s := s + ' / ' + iff(SubtitleInfo.Translation.FileName.IsEmpty, '?', ExtractFileName(SubtitleInfo.Translation.FileName));

    s := s + ' - ' + ProgramName;
  end
  else if not frmMain.MPV.FileName.IsEmpty then
    s := ExtractFileName(frmMain.MPV.FileName) + ' - ' + ProgramName
  else
    s := ProgramName;

  frmMain.Caption := s;
  {$IFDEF WINDOWS}
  Application.Title := s;
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure SetStatusBarText(const AText: String; const APanelIndex: Integer = 0; const AUseTimer: Boolean = True);
begin
  with frmMain do
  begin
    TimerStatus.Enabled := False;
    StatusBar.Panels[APanelIndex].Text := AText;
    if not AText.IsEmpty and AUseTimer then
      TimerStatus.Enabled := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure DoAutoCheckErrors(const AAll: Boolean = True; const ARefresh: Boolean = False);
begin
  if AppOptions.AutoCheckErrors then
  begin
    if AAll then
      VSTDoLoop(frmMain.VST, @ApplyCheckErrors, dlAll, ARefresh)
    else
      VSTDoLoop(frmMain.VST, @ApplyCheckErrorsTimesOnly, dlAll, ARefresh);
  end;
end;

// -----------------------------------------------------------------------------

procedure UpdateStatusBar;
var
  s: String;
begin
  s := '';
  with frmMain do
    if VST.Enabled then
    begin
      if VST.SelectedCount > 1 then
        s := Format(GetLangString('LineSelected'), [VST.SelectedCount, VST.TotalCount])
      else if (VST.SelectedCount = 1) and (VSTFocusedNode(VST) >= 0) then
        s := Format('%d / %d', [VSTFocusedNode(VST)+1, VST.TotalCount])
      else
        s := IntToStr(VST.TotalCount);
    end;

  SetStatusBarText(s + '     ', 1);
end;

// -----------------------------------------------------------------------------

procedure UpdateCPSAndTexts(const AIndex: Integer);
var
  sCPS, tCPS: Double;
begin
  with frmMain, AppOptions do
    if (AIndex > -1) and (VST.SelectedCount = 1) then
    begin
      sCPS := Subtitles.TextCPS[AIndex, Conventions.CPSLineLenStrategy];
      mmoText.LabelMemo.Caption := Format(GetLangString('TextChars'), [GetLengthForEachLine(Subtitles[AIndex].Text, '/', '='), sCPS], FormatSettings);
      mmoText.CPSBar.SetCPS(sCPS);

      if Workspace.TranslatorMode then
      begin
        tCPS := Subtitles.TranslationCPS[AIndex, Conventions.CPSLineLenStrategy];
        mmoTranslation.LabelMemo.Caption := Format(GetLangString('TranslationChars'), [GetLengthForEachLine(Subtitles[AIndex].Translation, '/', '='), tCPS], FormatSettings);
        mmoTranslation.CPSBar.SetCPS(tCPS);
      end;
    end
    else
    begin
      mmoText.LabelMemo.Caption := GetLangString('Text');
      mmoText.CPSBar.SetCPS(0);

      if Workspace.TranslatorMode then
      begin
        mmoTranslation.LabelMemo.Caption := GetLangString('Translation');
        mmoTranslation.CPSBar.SetCPS(0);
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure UpdateValuesForSubtitle(const AIndex: Integer);
begin
  if not Subtitles.ValidIndex(AIndex) then Exit;

  with frmMain do
  begin
    tedInitial.SetValueOnly(Subtitles[AIndex].InitialTime);
    tedFinal.SetValueOnly(Subtitles[AIndex].FinalTime);
    tedDuration.SetValueOnly(Subtitles.Duration[AIndex]);
    tedPause.SetValueOnly(Subtitles.Pause[AIndex]);
  end;
  //UpdateCPSAndTexts(AIndex);
end;

// -----------------------------------------------------------------------------

procedure UpdateValues(const AInvalidate: Boolean = False);
var
  NodeIndex: Integer;
  LastIndex: Integer;
begin
  with frmMain do
  begin
    VSTBeginUpdate(VST);
    try
      mmoText.Enabled        := VST.SelectedCount > 0;
      mmoTranslation.Enabled := mmoText.Enabled;
      tedInitial.Enabled     := mmoText.Enabled;
      tedFinal.Enabled       := mmoText.Enabled;
      tedPause.Enabled       := mmoText.Enabled;
      tedDuration.Enabled    := mmoText.Enabled;
      actExtendLengthToPrevious.Enabled := mmoText.Enabled;
      actExtendLengthToNext.Enabled     := mmoText.Enabled;
      actSetAutomaticDuration.Enabled   := mmoText.Enabled;
      actSetDefaultPause.Enabled        := mmoText.Enabled;

      if VST.SelectedCount = 0 then // zero selected
      begin
        tedInitial.SetValueOnly(0);
        tedFinal.SetValueOnly(0);
        tedPause.SetValueOnly(0);
        tedDuration.SetValueOnly(0);
        mmoText.Text        := '';
        mmoTranslation.Text := '';
        cboActor.Tag  := 1;
        cboActor.Text := '';
        cboActor.Tag  := 0;
      end
      else if Assigned(VST.GetFirstSelected) then // one or more selected
      begin
        LastIndex := -1;
        NodeIndex := VST.GetFirstSelected^.Index;
        if VST.SelectedCount > 1 then LastIndex := VSTLastSelectedNodeIndex(VST);
        if not Subtitles.ValidIndex(LastIndex) then LastIndex := NodeIndex;

        tedInitial.SetValueOnly(Subtitles[NodeIndex].InitialTime);
        tedFinal.SetValueOnly(Subtitles[LastIndex].FinalTime);
        if VST.SelectedCount = 1 then
        begin
          tedDuration.SetValueOnly(Subtitles.Duration[NodeIndex]);
          tedPause.SetValueOnly(Subtitles.Pause[NodeIndex]);
          mmoText.Text        := Subtitles[NodeIndex].Text;
          mmoTranslation.Text := Subtitles[NodeIndex].Translation;
          case Subtitles[NodeIndex].Align of
            0: actAlignToNone.Checked   := True;
            1: actAlignToLeft.Checked   := True;
            2: actAlignToCenter.Checked := True;
            3: actAlignToRight.Checked  := True;
          end;
          case Subtitles[NodeIndex].VAlign of
            0: actVAlignToBottom.Checked := True;
            1: actVAlignToMiddle.Checked := True;
            2: actVAlignToTop.Checked    := True;
          end;

          actExtendLengthToPrevious.Enabled := (NodeIndex > 0);
          actExtendLengthToNext.Enabled     := NodeIndex < Subtitles.Count-1;
          actSetDefaultPause.Enabled        := actExtendLengthToNext.Enabled;
          tedPause.Enabled                  := actSetDefaultPause.Enabled;

          cboActor.Tag  := 1;
          cboActor.Text := Subtitles[NodeIndex].Actor;
          cboActor.Tag  := 0;

          CheckForTranslationMemory(NodeIndex);
          CheckForTerminology(NodeIndex);
        end
        else
        begin
          tedDuration.SetValueOnly(Range(tedFinal.Value - tedInitial.Value, 0, tedFinal.Value));
          tedPause.SetValueOnly(0);
          mmoText.Text              := '';
          mmoTranslation.Text       := '';
          actAlignToNone.Checked    := False;
          actAlignToLeft.Checked    := False;
          actAlignToCenter.Checked  := False;
          actAlignToRight.Checked   := False;
          actVAlignToBottom.Checked := False;
          actVAlignToMiddle.Checked := False;
          actVAlignToTop.Checked    := False;

          actExtendLengthToPrevious.Enabled := True;
          actExtendLengthToNext.Enabled     := True;
          actSetDefaultPause.Enabled        := NodeIndex < Subtitles.Count-1;
          tedPause.Enabled                  := actSetDefaultPause.Enabled;
        end;
        actAutomaticDuration.Enabled := True;
      end;
      UpdateCPSAndTexts(NodeIndex);

      if AInvalidate then
      begin
        VST.Invalidate;
        WAVE.DoUpdate;
      end;

      UpdateStatusBar;
    finally
      VSTEndUpdate(VST);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure UpdateToolBarButtons(const AOnlyDivider: Boolean);

  procedure SetToolButtonPos(const AToolBar: TToolBar);
  var
    i: Integer;
  begin
    with AToolBar do
    begin
      BeginUpdate;
      AutoSize := False;
      for i := ButtonCount-1 downto 0 do
        if Buttons[i].Visible then
          Buttons[i].Left := -1;
      AutoSize := True;
      EndUpdate;
    end;
  end;

begin
  with frmMain do
    if AOnlyDivider then
    begin
      tlbdiv0.Visible  := tlb3.Visible or tlb4.Visible;
      tlbdiv1.Visible  := tlb5.Visible or tlb6.Visible or tlb7.Visible;
      tlbdiv2.Visible  := tlb8.Visible or tlb9.Visible or tlb10.Visible;
      tlbdiv3.Visible  := tlb11.Visible or tlb12.Visible or tlb13.Visible;
      tlbdiv4.Visible  := tlb14.Visible or tlb15.Visible or tlb16.Visible;
      tlbdiv5.Visible  := tlb17.Visible or tlb18.Visible;

      etlbdiv0.Visible := etlb2.Visible or etlb3.Visible or etlb4.Visible or etlb5.Visible or etlb6.Visible;
      etlbdiv1.Visible := etlb7.Visible or etlb8.Visible or etlb9.Visible or etlb10.Visible;
      etlbdiv2.Visible := etlb11.Visible or etlb12.Visible or etlb13.Visible;
      etlbdiv3.Visible := etlb14.Visible or tlbValidate.Visible;

      vtlbdiv0.Visible := vtlb2.Visible or vtlb3.Visible;
      vtlbdiv1.Visible := vtlb4.Visible or vtlb5.Visible or vtlb6.Visible or vtlb7.Visible;
      vtlbdiv2.Visible := vtlb8.Visible or vtlb9.Visible or vtlb10.Visible or vtlb11.Visible;

      wtlbdiv0.Visible := wtlb4.Visible;
      wtlbdiv1.Visible := wtlb5.Visible or wtlb6.Visible or wtlb7.Visible;
      wtlbdiv2.Visible := wtlb8.Visible or wtlb9.Visible or wtlb10.Visible;
    end
    else
    begin
      SetToolButtonPos(ToolBarMain);
      SetToolButtonPos(ToolBarEditor);
      SetToolButtonPos(ToolBarVideo);
      SetToolButtonPos(ToolBarWaveform);
    end;
end;

// -----------------------------------------------------------------------------

procedure UpdateCoolBar(const ABand: Integer; const AVisible: Boolean);
var
  i, x: Integer;
begin
  with frmMain do
  begin
    if ABand > -1 then
      CoolBarMain.Bands[ABand].Visible := AVisible;

    x := 0;
    for i := 0 to CoolBarMain.Bands.Count-1 do
      if CoolBarMain.Bands[i].Visible then
        Inc(x);

    CoolBarMain.Visible := x > 0;
  end;
end;

// -----------------------------------------------------------------------------

end.

