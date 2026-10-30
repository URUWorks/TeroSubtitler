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

unit formFixSubtitles;

{$mode delphi}
//{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CheckLst, laz.VirtualTrees, UWSubtitleAPI, procFixSubtitles,
  generics.collections, LCLIntf, LCLType, Menus, procConventions, UWLayout,
  LCLTranslator, procLocalize;

type

  { TfrmFixSubtitles }

  TVSTSelectionMode = (vsmSelectAll, vsmDeSelectAll, vsmInvertSelection);

  TCustomErrorOption = record
    Error       : TSubtitleErrorType;
    Enabled     : Boolean;
    Description : String;
    Count       : Integer;
    Estimate    : Integer; // count with every fix enabled (GetTotalErrorsCount)
  end;

  TfrmFixSubtitles = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    cboOCR: TComboBox;
    cboConvention: TComboBox;
    clbOptions: TCheckListBox;
    cboSpacingHyphen: TComboBox;
    cboSource: TComboBox;
    lblSource: TLabel;
    lblSpacingHyphen: TLabel;
    lblOCR: TLabel;
    lblConvention: TLabel;
    lblTotalErrors: TLabel;
    mnuInvertSelection: TMenuItem;
    mnuDeSelectAll: TMenuItem;
    mnuSelectAll: TMenuItem;
    PopupMenuSelect: TPopupMenu;
    Splitter1: TSplitter;
    lyoBottom: TUWLayout;
    lyoLeft: TUWLayout;
    lyoClient: TUWLayout;
    VST: TLazVirtualStringTree;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure cboConventionSelect(Sender: TObject);
    procedure cboSourceSelect(Sender: TObject);
    procedure clbOptionsClickCheck(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuInvertSelectionClick(Sender: TObject);
    procedure mnuSelectAllClick(Sender: TObject);
    procedure mnuDeSelectAllClick(Sender: TObject);
    procedure VSTAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTHeaderDrawQueryElements(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VSTResize(Sender: TObject);
  private
    FOptions: array of TCustomErrorOption;
    FList: TSubtitleInfoList;
    FProfiles: TProfiles;
    FProfile: PProfileItem;
    FTotalErrorCount: Integer;
    FCheckingSelected: Boolean;
    procedure UpdateItemList(const AUpdateTotalCount: Boolean = True);
    function GetErrorStr(const AError: TSubtitleErrorTypeSet): String;
    function IsTimeFixed(const Item: TSubtitleInfoItem): Boolean;
    function GetFixedText(const Item: TSubtitleInfoItem): String;
    function GetText(const Index: Integer; const Item: TSubtitleInfoItem): String;
    procedure VSTSelect(const ASelectMode: TVSTSelectionMode);
    procedure CheckListSelect(const ASelectMode: TVSTSelectionMode);
    procedure GetTotalErrorsCount;
    procedure RefreshTotalErrorsCount(const AInfoList: TSubtitleInfoList; const AOnlyEnabled: Boolean = False);
  public

  end;

var
  frmFixSubtitles: TfrmFixSubtitles;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procVST, procWorkspace, procColorTheme, procSubtitle, procUndo,
  procConfig, UWSystem.StrUtils, XMLConf, formMain;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmFixSubtitles }

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.FormCreate(Sender: TObject);

  procedure AddOptionItem(const AError: TSubtitleErrorType; const ADescription: String; const AEnabled: Boolean = True);
  var
    AErrorItem: TCustomErrorOption;
    l: Integer;
  begin
    with AErrorItem do
    begin
      Error       := AError;
      Enabled     := AEnabled;
      Description := ADescription;
      Count       := 0;
      Estimate    := 0;
    end;
    l := Length(FOptions);
    clbOptions.AddItem(ADescription, NIL);
    clbOptions.Checked[l] := AEnabled;
    SetLength(FOptions, l+1);
    FOptions[l] := AErrorItem;
  end;

var
  s: String;
  x: Integer;
begin
  FProfiles := TProfiles.Create(ConventionsFileName);
  FillItemsWithConventions(cboConvention.Items, FProfiles);
  FillComboWithOCRScripts(cboOCR);
  FTotalErrorCount := 0;
  cboConvention.Items.Insert(0, lngasCustom);
  if FileExists(SettingsFileName) then
  begin
    with TXMLConfig.Create(NIL) do
    try
      FileName := SettingsFileName;
      OpenKey(Self.Name);

      AddOptionItem(etUnnecessarySpaces, lngfsetUnnecessarySpaces, GetValue('Option0', True));
      AddOptionItem(etUnnecessaryDots, lngfsetUnnecessaryDots, GetValue('Option1', True));
      AddOptionItem(etFixTags, lngfsetFixTags, GetValue('Option2', True));
      AddOptionItem(etTimeTooShort, lngfsetTimeTooShort, GetValue('Option3', True));
      AddOptionItem(etTimeTooLong, lngfsetTimeTooLong, GetValue('Option4', True));
      AddOptionItem(etOverlapping, lngfsetOverlapping, GetValue('Option5', True));
      AddOptionItem(etBadValues, lngfsetBadValues, GetValue('Option6', True));
      AddOptionItem(etUnbreak, Format(lngfsetUnbreak, [GetCPL(AppOptions.Conventions, '')]), GetValue('Option7', True));
      AddOptionItem(etBreakLongLines, lngfsetBreakLongLines, GetValue('Option8', True));
      AddOptionItem(etEmpty, lngfsetEmpty, GetValue('Option9', True));
      AddOptionItem(etEllipsesSingleSmartCharacter, lngfsetEllipsesSingleSmartCharacter, GetValue('Option10', True));
      AddOptionItem(etProhibitedChars, Format(lngfsetProhibitedChars, [AppOptions.Conventions.ProhibitedChars]), GetValue('Option11', True));
      AddOptionItem(etHearingImpaired, lngfsetHearingImpaired, GetValue('Option12', True));
      AddOptionItem(etRepeatedSubtitle, lngfsetRepeatedEntry, GetValue('Option13', True));
      AddOptionItem(etRepeatedChars, lngfsetRepeatedChars, GetValue('Option14', True));
      AddOptionItem(etIncompleteHyphenText, lngfsetIncompleteHyphenText, GetValue('Option15', True));
      AddOptionItem(etSpaceOfOpeningHyphen, lngfsetSpaceOfOpeningHyphen, GetValue('Option16', False));
      AddOptionItem(etRemoveSpacesWithinBrackets, lngfsetRemoveSpacesWithinBrackets, GetValue('Option17', False));
      AddOptionItem(etFixInterrobang, lngfsetFixInterrobang, GetValue('Option18', False));
      AddOptionItem(etOCR, lngfsetOCR, GetValue('Option19', False));
      AddOptionItem(etSnapToShotChanges, lngfsetSnapToShotChanges, GetValue('Option20', False));
      AddOptionItem(etChaining, lngfsetChaining, GetValue('Option21', False));
      AddOptionItem(etCleanupTags, lngfsetCleanupTags, GetValue('Option22', False));
      AddOptionItem(etStraightToCurlyQuotes, lngfsetStraightToCurlyQuotes, GetValue('Option23', False));
      AddOptionItem(etCurlyToStraightQuotes, lngfsetCurlyToStraightQuotes, GetValue('Option24', False));
      AddOptionItem(etFixBrackets, lngfsetFixBrackets, GetValue('Option25', False));
      s := GetValue('OCRScript', '');
      x := cboOCR.Items.IndexOf(s);
      if x >= 0 then
        cboOCR.ItemIndex := x;
      x := GetValue('SpaceOfOpeningHyphen', 0);

      CloseKey;
    finally
      Free;
    end;
  end
  else
  begin
    AddOptionItem(etUnnecessarySpaces, lngfsetUnnecessarySpaces);
    AddOptionItem(etUnnecessaryDots, lngfsetUnnecessaryDots);
    AddOptionItem(etFixTags, lngfsetFixTags);
    AddOptionItem(etTimeTooShort, lngfsetTimeTooShort);
    AddOptionItem(etTimeTooLong, lngfsetTimeTooLong);
    AddOptionItem(etOverlapping, lngfsetOverlapping);
    AddOptionItem(etBadValues, lngfsetBadValues);
    AddOptionItem(etUnbreak, Format(lngfsetUnbreak, [GetCPL(AppOptions.Conventions, '')]));
    AddOptionItem(etBreakLongLines, lngfsetBreakLongLines);
    AddOptionItem(etEmpty, lngfsetEmpty);
    AddOptionItem(etEllipsesSingleSmartCharacter, lngfsetEllipsesSingleSmartCharacter);
    AddOptionItem(etProhibitedChars, Format(lngfsetProhibitedChars, [AppOptions.Conventions.ProhibitedChars]));
    AddOptionItem(etHearingImpaired, lngfsetHearingImpaired);
    AddOptionItem(etRepeatedSubtitle, lngfsetRepeatedEntry);
    AddOptionItem(etRepeatedChars, lngfsetRepeatedChars);
    AddOptionItem(etIncompleteHyphenText, lngfsetIncompleteHyphenText);
    AddOptionItem(etSpaceOfOpeningHyphen, lngfsetSpaceOfOpeningHyphen);
    AddOptionItem(etRemoveSpacesWithinBrackets, lngfsetRemoveSpacesWithinBrackets);
    AddOptionItem(etFixInterrobang, lngfsetFixInterrobang);
    AddOptionItem(etOCR, lngfsetOCR, False);
    AddOptionItem(etSnapToShotChanges, lngfsetSnapToShotChanges, False);
    AddOptionItem(etChaining, lngfsetChaining, False);
    AddOptionItem(etCleanupTags, lngfsetCleanupTags, False);
    AddOptionItem(etStraightToCurlyQuotes, lngfsetStraightToCurlyQuotes, False);
    AddOptionItem(etCurlyToStraightQuotes, lngfsetCurlyToStraightQuotes, False);
    AddOptionItem(etFixBrackets, lngfsetFixBrackets, False);
  end;

  cboSpacingHyphen.AddItem(lngfsAddSpacing, NIL);
  cboSpacingHyphen.AddItem(lngfsRemoveSpacing, NIL);
  cboSpacingHyphen.ItemIndex := x;

  // fix the text or the translation
  cboSource.AddItem(lngasText, NIL);
  cboSource.AddItem(lngasTranslation, NIL);
  cboSource.ItemIndex := 0;
  cboSource.Enabled   := Workspace.TranslatorMode;

  // select several entries (Shift/Cmd) and check or uncheck them at once
  VST.TreeOptions.SelectionOptions := VST.TreeOptions.SelectionOptions + [toMultiSelect];
  VSTAddColumn(VST, lngfsIndex, 75);
  VSTAddColumn(VST, lngfsAction, 50, taLeftJustify);
  VSTAddColumn(VST, lngfsCurrent, 50, taLeftJustify);
  VSTAddColumn(VST, lngfsAfter, 50, taLeftJustify);

  FList := TSubtitleInfoList.Create(Subtitles);
  FList.Source := smText;

  x := cboConvention.Items.IndexOf(AppOptions.Conventions.Name);
  if x >= 0 then
    cboConvention.ItemIndex := x
  else
    cboConvention.ItemIndex := 0;

  cboConventionSelect(NIL);
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  i: Integer;
begin
  with TXMLConfig.Create(NIL) do
  try
    FileName := SettingsFileName;
    RootName := RootCfg;
    OpenKey(Self.Name);
    for i := 0 to clbOptions.Count-1 do
      SetValue('Option'+IntToStr(i), clbOptions.Checked[i]);

    SetValue('OCRScript', cboOCR.Text);
    SetValue('SpaceOfOpeningHyphen', cboSpacingHyphen.ItemIndex);
    CloseKey;
    SafeFlush;
  finally
    Free;
  end;

  SaveFormSettings(Self);
  FProfiles.Free;
  FList.Free;
  SetLength(FOptions, 0);

  CloseAction := caFree;
  frmFixSubtitles := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.FormShow(Sender: TObject);
begin
  LoadFormSettings(Self);
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.VSTAdvancedHeaderDraw(Sender: TVTHeader;
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

procedure TfrmFixSubtitles.VSTHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if ColorThemeInstance.GetRealColorMode = cmDark then
    Elements := [hpeBackground];
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node.CheckType  := ctTriStateCheckBox;
  Node.CheckState := csCheckedNormal;
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  case Column of
    0: CellText := IntToStr(FList[Node^.Index].Index+1);
    1: CellText := GetErrorStr(FList[Node^.Index].ErrorsFixed);
    2: CellText := GetText(FList[Node^.Index].Index, FList[Node^.Index]^);
    3: CellText := GetFixedText(FList[Node^.Index]^);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.VSTChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Run: PVirtualNode;
begin
  if not Assigned(Node) or (FList.Count = 0) then Exit;

  FList[Node^.Index]^.Apply := Node^.CheckState = csCheckedNormal;

  // checking one of several selected entries checks all of them
  if not FCheckingSelected and (vsSelected in Node^.States) and (VST.SelectedCount > 1) then
  begin
    FCheckingSelected := True;
    try
      Run := VST.GetFirstSelected;
      while Assigned(Run) do
      begin
        if Run <> Node then
        begin
          Run^.CheckState := Node^.CheckState;
          FList[Run^.Index]^.Apply := Node^.CheckState = csCheckedNormal;
        end;
        Run := VST.GetNextSelected(Run);
      end;
    finally
      FCheckingSelected := False;
    end;
    VST.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.VSTResize(Sender: TObject);
var
  c, wCols, ClientW: Integer;
begin
  if VST.Header.Columns.Count > 0 then
  begin
    ClientW := VST.Width - VST.Header.Columns[0].Width - (GetSystemMetrics(SM_CXVSCROLL)+VST.Header.Columns.Count-1);
    wCols := ClientW div (VST.Header.Columns.Count-1);
    for c := 1 to VST.Header.Columns.Count-1 do
      VST.Header.Columns[c].Width := wCols;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.cboConventionSelect(Sender: TObject);
begin
  FProfile := FProfiles.FindProfile(cboConvention.Text);
  if FProfile = NIL then
    FProfile := @AppOptions.Conventions;

  clbOptions.Items[11] := Format(lngfsetProhibitedChars, [FProfile^.ProhibitedChars]);
  UpdateItemList;
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.cboSourceSelect(Sender: TObject);
begin
  if cboSource.ItemIndex = 1 then
    FList.Source := smTranslation
  else
    FList.Source := smText;

  UpdateItemList;
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.clbOptionsClickCheck(Sender: TObject);
var
  i: Integer;
begin
  // straight to curly quotes and curly to straight are opposite
  i := clbOptions.ItemIndex;
  if (i >= 0) and (i < Length(FOptions)) and clbOptions.Checked[i] then
  begin
    if FOptions[i].Error = etStraightToCurlyQuotes then
      clbOptions.Checked[i+1] := False
    else if FOptions[i].Error = etCurlyToStraightQuotes then
      clbOptions.Checked[i-1] := False;
  end;

  for i := 0 to clbOptions.Items.Count-1 do
    FOptions[i].Enabled := clbOptions.Checked[i];

  UpdateItemList(False);
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.btnApplyClick(Sender: TObject);
var
  i: Integer;
  numList: TList<Cardinal>;
begin
  if FList.Count > 0 then
  begin
    numList := TList<Cardinal>.Create;
    try
      numList.Clear;
      // Apply all text/times changes
      for i := 0 to FList.Count-1 do
        with FList[i]^ do
          if Apply then
          begin
            if (etEmpty in ErrorsFixed) or (etProhibitedChars in ErrorsFixed) or (etRepeatedSubtitle in ErrorsFixed) then
              numList.Add(Index)
            else
              SetSubtitleValues(Index, InitialTime, FinalTime, Text, False, False, FList.Source);
          end;

      // Remove empty/prohibited lines...
      if numList.Count > 0 then
      begin
        numList.Sort;
        for i := numList.Count-1 downto 0 do DeleteSubtitle(numList[i], False, False);
      end;

      UndoInstance.IncrementUndoGroup;
      SubtitleChanged(FList.Source = smText, FList.Source = smTranslation);
      DoAutoCheckErrors;
      UpdateValues(True);
    finally
      numList.Free;
    end;
  end;
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.UpdateItemList(const AUpdateTotalCount: Boolean = True);
var
  i: Integer;
begin
  FList.Errors := [];
  Screen.Cursor := crHourGlass;
  VST.BeginUpdate;
  try
    VST.RootNodeCount := 0;
    FList.ClearItems;

    if AUpdateTotalCount then
      GetTotalErrorsCount;

    for i := 0 to Length(FOptions)-1 do
      if FOptions[i].Enabled then FList.Errors := FList.Errors + [FOptions[i].Error];

    if FProfile = NIL then
      FProfile := @AppOptions.Conventions;

    FProfile^.AddHyphenSpace := (cboSpacingHyphen.ItemIndex = 0);

    if cboOCR.Items.Count > 0 then
      FList.FixErrors(OCRFolder + cboOCR.Items[cboOCR.ItemIndex] + '.ocr', FProfile, frmMain.WAVE.GetSceneChangeList)
    else
      FList.FixErrors('', FProfile, frmMain.WAVE.GetSceneChangeList);

    // The totals are estimated with every fix enabled, and fixes interact
    // (e.g. a duration fix can move an end time next to a shot change), so
    // an enabled option could show [1] while the list showed nothing. Take
    // the count of the enabled options from the list itself.
    RefreshTotalErrorsCount(FList, True);

    lblTotalErrors.Caption := Format(lngfsTotalErrors, [FTotalErrorCount, FList.Count]);
  finally
    VST.RootNodeCount := FList.Count;
    VST.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

// -----------------------------------------------------------------------------

function TfrmFixSubtitles.GetErrorStr(const AError: TSubtitleErrorTypeSet): String;
begin
  if etUnnecessarySpaces in AError then
    Result := FOptions[0].Description
  else if etUnnecessaryDots in AError then
    Result := FOptions[1].Description
  else if etFixTags in AError then
    Result := FOptions[2].Description
  else if etTimeTooShort in AError then
    Result := FOptions[3].Description
  else if etTimeTooLong in AError then
    Result := FOptions[4].Description
  else if etOverlapping in AError then
    Result := FOptions[5].Description
  else if etBadValues in AError then
    Result := FOptions[6].Description
  else if etUnbreak in AError then
    Result := FOptions[7].Description
  else if etBreakLongLines in AError then
    Result := FOptions[8].Description
  else if etEmpty in AError then
    Result := FOptions[9].Description
  else if etEllipsesSingleSmartCharacter in AError then
    Result := FOptions[10].Description
  else if etProhibitedChars in AError then
    Result := FOptions[11].Description
  else if etHearingImpaired in AError then
    Result := FOptions[12].Description
  else if etRepeatedSubtitle in AError then
    Result := FOptions[13].Description
  else if etRepeatedChars in AError then
    Result := FOptions[14].Description
  else if etIncompleteHyphenText in AError then
    Result := FOptions[15].Description
  else if etSpaceOfOpeningHyphen in AError then
    Result := FOptions[16].Description
  else if etRemoveSpacesWithinBrackets in AError then
    Result := FOptions[17].Description
  else if etFixInterrobang in AError then
    Result := FOptions[18].Description
  else if etOCR in AError then
    Result := FOptions[19].Description
  else if etSnapToShotChanges in AError then
    Result := FOptions[20].Description
  else if etSnapToShotChangesInCue in AError then
    Result := lngfsetSnapToShotChangesInCue
  else if etSnapToShotChangesInCueAway in AError then
    Result := lngfsetSnapToShotChangesInCueAway
  else if etSnapToShotChangesOutCue in AError then
    Result := lngfsetSnapToShotChangesOutCue
  else if etChaining in AError then
    Result := FOptions[21].Description
  else if etCleanupTags in AError then
    Result := FOptions[22].Description
  else if etStraightToCurlyQuotes in AError then
    Result := FOptions[23].Description
  else if etCurlyToStraightQuotes in AError then
    Result := FOptions[24].Description
  else if etFixBrackets in AError then
    Result := FOptions[25].Description
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

function TfrmFixSubtitles.IsTimeFixed(const Item: TSubtitleInfoItem): Boolean;
begin
  with Item do
    if (etOverlapping in ErrorsFixed) or (etBadValues in ErrorsFixed) or
       (etTimeTooShort in ErrorsFixed) or (etTimeTooLong in ErrorsFixed) or
       (etSnapToShotChanges in ErrorsFixed) or
       (etSnapToShotChangesInCue in ErrorsFixed) or
       (etSnapToShotChangesInCueAway in ErrorsFixed) or
       (etSnapToShotChangesOutCue in ErrorsFixed) or
       (etChaining in ErrorsFixed) then
      Result := True
    else
      Result := False;
end;

// -----------------------------------------------------------------------------

function TfrmFixSubtitles.GetFixedText(const Item: TSubtitleInfoItem): String;
begin
  with Item do
    if IsTimeFixed(Item) then
      Result := GetTimeStr(InitialTime) + ' --> ' +
                GetTimeStr(FinalTime) // + ' ' + Text
    else
      Result := ReplaceEnters(Text);
end;

// -----------------------------------------------------------------------------

function TfrmFixSubtitles.GetText(const Index: Integer; const Item: TSubtitleInfoItem): String;
begin
  with Subtitles[Index] do
    if IsTimeFixed(Item) then
      Result := GetTimeStr(InitialTime) + ' --> ' +
                GetTimeStr(FinalTime) // + ' ' + Text
    else if FList.Source = smTranslation then
      Result := ReplaceEnters(Translation)
    else
      Result := ReplaceEnters(Text);
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.mnuSelectAllClick(Sender: TObject);
begin
  if PopupMenuSelect.PopupComponent = VST then
    VSTSelect(vsmSelectAll)
  else
    CheckListSelect(vsmSelectAll);
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.mnuDeSelectAllClick(Sender: TObject);
begin
  if PopupMenuSelect.PopupComponent = VST then
    VSTSelect(vsmDeSelectAll)
  else
    CheckListSelect(vsmDeSelectAll);
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.mnuInvertSelectionClick(Sender: TObject);
begin
  if PopupMenuSelect.PopupComponent = VST then
    VSTSelect(vsmInvertSelection)
  else
    CheckListSelect(vsmInvertSelection);
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.VSTSelect(const ASelectMode: TVSTSelectionMode);
var
  Run: PVirtualNode;
begin
  if VST.RootNodeCount > 0 then
  begin
    Run := VST.GetFirst;
    if Assigned(Run) then
      while Assigned(Run) do
      begin
        case ASelectMode of
          vsmSelectAll   : Run^.CheckState := csCheckedNormal;
          vsmDeSelectAll : Run^.CheckState := csUncheckedNormal;
        else
          if Run^.CheckState = csCheckedNormal then
            Run^.CheckState := csUncheckedNormal
          else
            Run^.CheckState := csCheckedNormal;
        end;

        if (FList.Count > 0) then
          FList[Run^.Index]^.Apply := Run^.CheckState = csCheckedNormal;

        Run := VST.GetNext(Run);
      end;
    VST.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.CheckListSelect(const ASelectMode: TVSTSelectionMode);
var
  i: Integer;
begin
  for i := 0 to clbOptions.Items.Count-1 do
  begin
    case ASelectMode of
      vsmSelectAll   : clbOptions.Checked[i] := True;
      vsmDeSelectAll : clbOptions.Checked[i] := False;
    else
      clbOptions.Checked[i] := not clbOptions.Checked[i];
    end;
    FOptions[i].Enabled := clbOptions.Checked[i];
  end;

  UpdateItemList(False);
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.GetTotalErrorsCount;
var
  InfoList : TSubtitleInfoList;
begin
  InfoList := TSubtitleInfoList.Create(Subtitles);
  InfoList.Source := FList.Source;
  try
    InfoList.Errors := [etBadValues, etTimeTooLong,etTimeTooShort,
      etPauseTooShort, etMaxCPS, etOverlapping, etFixTags, etEmpty, etUnbreak,
      etUnnecessarySpaces, etUnnecessaryDots, etRepeatedChars, etProhibitedChars,
      etHearingImpaired, etBreakLongLines, etRepeatedSubtitle, etEllipsesSingleSmartCharacter,
      etOCR, etMaxLines, etIncompleteHyphenText, etSpaceOfOpeningHyphen, etRemoveSpacesWithinBrackets,
      etFixInterrobang, etOverlappingWithPrev,  etOverlappingWithNext, etSnapToShotChanges,
      etSnapToShotChangesInCue, etSnapToShotChangesOutCue, etSnapToShotChangesInCueAway,
      etChaining, etCleanupTags, etStraightToCurlyQuotes, etCurlyToStraightQuotes,
      etFixBrackets];

    if FProfile = NIL then
      FProfile := @AppOptions.Conventions;

    FProfile^.AddHyphenSpace := (cboSpacingHyphen.ItemIndex = 0);

    if cboOCR.Items.Count > 0 then
      InfoList.FixErrors(OCRFolder + cboOCR.Items[cboOCR.ItemIndex] + '.ocr', FProfile, frmMain.WAVE.GetSceneChangeList)
    else
      InfoList.FixErrors('', FProfile, frmMain.WAVE.GetSceneChangeList);

    RefreshTotalErrorsCount(InfoList);
  finally
    InfoList.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.RefreshTotalErrorsCount(const AInfoList: TSubtitleInfoList; const AOnlyEnabled: Boolean = False);

  procedure IncCount(const AId: Byte);
  begin
    if not AOnlyEnabled or FOptions[AId].Enabled then
      FOptions[AId].Count := FOptions[AId].Count + 1;
  end;

  procedure UpdatedOption(const AId: Byte; const ADesc: String);
  begin
    with clbOptions, FOptions[AId] do
      Items[AId] := Format('%s [%d]', [ADesc, Count]);
  end;

var
  i: Integer;
begin
  for i := 0 to Length(FOptions)-1 do
    if not AOnlyEnabled or FOptions[i].Enabled then
      FOptions[i].Count := 0
    else
      // a disabled option shows the estimate again, not the count it had
      // the last time it was enabled (it depends on the other options, e.g.
      // empty entries only appear after removing the hearing impaired text)
      FOptions[i].Count := FOptions[i].Estimate;

  if AInfoList.Count > 0 then
    for i := 0 to AInfoList.Count-1 do
      with AInfoList[i]^ do
      begin
        if etUnnecessarySpaces in ErrorsFixed then
          IncCount(0)
        else if etUnnecessaryDots in ErrorsFixed then
          IncCount(1)
        else if etFixTags in ErrorsFixed then
          IncCount(2)
        else if etTimeTooShort in ErrorsFixed then
          IncCount(3)
        else if etTimeTooLong in ErrorsFixed then
          IncCount(4)
        else if etOverlapping in ErrorsFixed then
          IncCount(5)
        else if etBadValues in ErrorsFixed then
          IncCount(6)
        else if etUnbreak in ErrorsFixed then
          IncCount(7)
        else if etBreakLongLines in ErrorsFixed then
          IncCount(8)
        else if etEmpty in ErrorsFixed then
          IncCount(9)
        else if etEllipsesSingleSmartCharacter in ErrorsFixed then
          IncCount(10)
        else if etProhibitedChars in ErrorsFixed then
          IncCount(11)
        else if etHearingImpaired in ErrorsFixed then
          IncCount(12)
        else if etRepeatedSubtitle in ErrorsFixed then
          IncCount(13)
        else if etRepeatedChars in ErrorsFixed then
          IncCount(14)
        else if etIncompleteHyphenText in ErrorsFixed then
          IncCount(15)
        else if etSpaceOfOpeningHyphen in ErrorsFixed then
          IncCount(16)
        else if etRemoveSpacesWithinBrackets in ErrorsFixed then
          IncCount(17)
        else if etFixInterrobang in ErrorsFixed then
          IncCount(18)
        else if etOCR in ErrorsFixed then
          IncCount(19)
        else if (etSnapToShotChanges in ErrorsFixed) or (etSnapToShotChangesInCue in ErrorsFixed) or (etSnapToShotChangesInCueAway in ErrorsFixed) or (etSnapToShotChangesOutCue in ErrorsFixed) then
          IncCount(20)
        else if etChaining in ErrorsFixed then
          IncCount(21)
        else if etCleanupTags in ErrorsFixed then
          IncCount(22)
        else if etStraightToCurlyQuotes in ErrorsFixed then
          IncCount(23)
        else if etCurlyToStraightQuotes in ErrorsFixed then
          IncCount(24)
        else if etFixBrackets in ErrorsFixed then
          IncCount(25);
      end;

  if not AOnlyEnabled then
    for i := 0 to Length(FOptions)-1 do
      FOptions[i].Estimate := FOptions[i].Count;

  UpdatedOption(0, lngfsetUnnecessarySpaces);
  UpdatedOption(1, lngfsetUnnecessaryDots);
  UpdatedOption(2, lngfsetFixTags);
  UpdatedOption(3, lngfsetTimeTooShort);
  UpdatedOption(4, lngfsetTimeTooLong);
  UpdatedOption(5, lngfsetOverlapping);
  UpdatedOption(6, lngfsetBadValues);
  UpdatedOption(7, Format(lngfsetUnbreak, [GetCPL(AppOptions.Conventions, '')]));
  UpdatedOption(8, lngfsetBreakLongLines);
  UpdatedOption(9, lngfsetEmpty);
  UpdatedOption(10, lngfsetEllipsesSingleSmartCharacter);
  UpdatedOption(11, Format(lngfsetProhibitedChars, [AppOptions.Conventions.ProhibitedChars]));
  UpdatedOption(12, lngfsetHearingImpaired);
  UpdatedOption(13, lngfsetRepeatedEntry);
  UpdatedOption(14, lngfsetRepeatedChars);
  UpdatedOption(15, lngfsetIncompleteHyphenText);
  UpdatedOption(16, lngfsetSpaceOfOpeningHyphen);
  UpdatedOption(17, lngfsetRemoveSpacesWithinBrackets);
  UpdatedOption(18, lngfsetFixInterrobang);
  UpdatedOption(19, lngfsetOCR);
  UpdatedOption(20, lngfsetSnapToShotChanges);
  UpdatedOption(21, lngfsetChaining);
  UpdatedOption(22, lngfsetCleanupTags);
  UpdatedOption(23, lngfsetStraightToCurlyQuotes);
  UpdatedOption(24, lngfsetCurlyToStraightQuotes);
  UpdatedOption(25, lngfsetFixBrackets);

  FTotalErrorCount := 0;
  for i := 0 to Length(FOptions)-1 do
    FTotalErrorCount += FOptions[i].Count;
end;

// -----------------------------------------------------------------------------

end.

