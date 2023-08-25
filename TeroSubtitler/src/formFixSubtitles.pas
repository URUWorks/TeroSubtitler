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

unit formFixSubtitles;

{$mode delphi}
//{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CheckLst, laz.VirtualTrees, UWSubtitleAPI, procFixSubtitles, UWSystem.XMLLang,
  generics.collections, LCLIntf, LCLType, Menus, procConventions, UWLayout;

type

  { TfrmFixSubtitles }

  TVSTSelectionMode = (vsmSelectAll, vsmDeSelectAll, vsmInvertSelection);

  TCustomErrorOption = record
    Error       : TSubtitleErrorType;
    Enabled     : Boolean;
    Description : String;
  end;

  TfrmFixSubtitles = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    cboOCR: TComboBox;
    cboConvention: TComboBox;
    clbOptions: TCheckListBox;
    cboSpacingHyphen: TComboBox;
    lblSpacingHyphen: TLabel;
    lblOCR: TLabel;
    lblConvention: TLabel;
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
    FAppStringList: TAppStringList;
    FProfiles: TProfiles;
    FProfile: PProfileItem;
    procedure UpdateItemList;
    function GetErrorStr(const AError: TSubtitleErrorTypeSet): String;
    function IsTimeFixed(const Item: TSubtitleInfoItem): Boolean;
    function GetFixedText(const Item: TSubtitleInfoItem): String;
    function GetText(const Index: Integer; const Item: TSubtitleInfoItem): String;
    procedure VSTSelect(const ASelectMode: TVSTSelectionMode);
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
  LoadLanguage(Self);
  FProfiles := TProfiles.Create(ConventionsFileName);
  FillItemsWithConventions(cboConvention.Items, FProfiles);
  FillComboWithOCRScripts(cboOCR);
  cboConvention.Items.Insert(0, LanguageManager.GetAppString('Custom'));
  FAppStringList := NIL;
  LanguageManager.GetAppStringList('FixSubtitlesStrings', FAppStringList);

  if FileExists(SettingsFileName) then
  begin
    with TXMLConfig.Create(NIL) do
    try
      FileName := SettingsFileName;
      OpenKey(Self.Name);

      AddOptionItem(etUnnecessarySpaces, GetString(FAppStringList, 'etUnnecessarySpaces'), GetValue('Option0', True));
      AddOptionItem(etUnnecessaryDots, GetString(FAppStringList, 'etUnnecessaryDots'), GetValue('Option1', True));
      AddOptionItem(etFixTags, GetString(FAppStringList, 'etFixTags'), GetValue('Option2', True));
      AddOptionItem(etTimeTooShort, GetString(FAppStringList, 'etTimeTooShort'), GetValue('Option3', True));
      AddOptionItem(etTimeTooLong, GetString(FAppStringList, 'etTimeTooLong'), GetValue('Option4', True));
      AddOptionItem(etOverlapping, GetString(FAppStringList, 'etOverlapping'), GetValue('Option5', True));
      AddOptionItem(etBadValues, GetString(FAppStringList, 'etBadValues'), GetValue('Option6', True));
      AddOptionItem(etUnbreak, Format(GetString(FAppStringList, 'etUnbreak'), [AppOptions.Conventions.CPL]), GetValue('Option7', True));
      AddOptionItem(etBreakLongLines, GetString(FAppStringList, 'etBreakLongLines'), GetValue('Option8', True));
      AddOptionItem(etEmpty, GetString(FAppStringList, 'etEmpty'), GetValue('Option9', True));
      AddOptionItem(etEllipsesSingleSmartCharacter, GetString(FAppStringList, 'etEllipsesSingleSmartCharacter'), GetValue('Option10', True));
      AddOptionItem(etProhibitedChars, Format(GetString(FAppStringList, 'etProhibitedChars'), [AppOptions.Conventions.ProhibitedChars]), GetValue('Option11', True));
      AddOptionItem(etHearingImpaired, GetString(FAppStringList, 'etHearingImpaired'), GetValue('Option12', True));
      AddOptionItem(etRepeatedSubtitle, GetString(FAppStringList, 'etRepeatedSubtitle'), GetValue('Option13', True));
      AddOptionItem(etRepeatedChars, GetString(FAppStringList, 'etRepeatedChars'), GetValue('Option14', True));
      AddOptionItem(etIncompleteHyphenText, GetString(FAppStringList, 'etIncompleteHyphenText'), GetValue('Option15', True));
      AddOptionItem(etSpaceOfOpeningHyphen, GetString(FAppStringList, 'etSpaceOfOpeningHyphen'), GetValue('Option16', False));
      AddOptionItem(etRemoveSpacesWithinBrackets, GetString(FAppStringList, 'etRemoveSpacesWithinBrackets'), GetValue('Option17', False));
      AddOptionItem(etFixInterrobang, GetString(FAppStringList, 'etFixInterrobang'), GetValue('Option18', False));
      AddOptionItem(etOCR, GetString(FAppStringList, 'etOCR'), GetValue('Option19', False));
      AddOptionItem(etSnapToShotChanges, GetString(FAppStringList, 'etSnapToShotChanges'), GetValue('Option20', False));
      AddOptionItem(etChaining, GetString(FAppStringList, 'etChaining'), GetValue('Option21', False));
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
    AddOptionItem(etUnnecessarySpaces, GetString(FAppStringList, 'etUnnecessarySpaces'));
    AddOptionItem(etUnnecessaryDots, GetString(FAppStringList, 'etUnnecessaryDots'));
    AddOptionItem(etFixTags, GetString(FAppStringList, 'etFixTags'));
    AddOptionItem(etTimeTooShort, GetString(FAppStringList, 'etTimeTooShort'));
    AddOptionItem(etTimeTooLong, GetString(FAppStringList, 'etTimeTooLong'));
    AddOptionItem(etOverlapping, GetString(FAppStringList, 'etOverlapping'));
    AddOptionItem(etBadValues, GetString(FAppStringList, 'etBadValues'));
    AddOptionItem(etUnbreak, Format(GetString(FAppStringList, 'etUnbreak'), [AppOptions.Conventions.CPL]));
    AddOptionItem(etBreakLongLines, GetString(FAppStringList, 'etBreakLongLines'));
    AddOptionItem(etEmpty, GetString(FAppStringList, 'etEmpty'));
    AddOptionItem(etEllipsesSingleSmartCharacter, GetString(FAppStringList, 'etEllipsesSingleSmartCharacter'));
    AddOptionItem(etProhibitedChars, Format(GetString(FAppStringList, 'etProhibitedChars'), [AppOptions.Conventions.ProhibitedChars]));
    AddOptionItem(etHearingImpaired, GetString(FAppStringList, 'etHearingImpaired'));
    AddOptionItem(etRepeatedSubtitle, GetString(FAppStringList, 'etRepeatedSubtitle'));
    AddOptionItem(etRepeatedChars, GetString(FAppStringList, 'etRepeatedChars'));
    AddOptionItem(etIncompleteHyphenText, GetString(FAppStringList, 'etIncompleteHyphenText'));
    AddOptionItem(etSpaceOfOpeningHyphen, GetString(FAppStringList, 'etSpaceOfOpeningHyphen'));
    AddOptionItem(etRemoveSpacesWithinBrackets, GetString(FAppStringList, 'etRemoveSpacesWithinBrackets'));
    AddOptionItem(etFixInterrobang, GetString(FAppStringList, 'etFixInterrobang'));
    AddOptionItem(etOCR, GetString(FAppStringList, 'etOCR'), False);
    AddOptionItem(etSnapToShotChanges, GetString(FAppStringList, 'etSnapToShotChanges'), False);
    AddOptionItem(etChaining, GetString(FAppStringList, 'etChaining'), False);
  end;

  cboSpacingHyphen.AddItem(GetString(FAppStringList, 'AddSpacing'), NIL);
  cboSpacingHyphen.AddItem(GetString(FAppStringList, 'RemoveSpacing'), NIL);
  cboSpacingHyphen.ItemIndex := x;

  VSTAddColumn(VST, GetString(FAppStringList, 'Index'), 75);
  VSTAddColumn(VST, GetString(FAppStringList, 'Action'), 50, taLeftJustify);
  VSTAddColumn(VST, GetString(FAppStringList, 'Current'), 50, taLeftJustify);
  VSTAddColumn(VST, GetString(FAppStringList, 'After'), 50, taLeftJustify);

  FList := TSubtitleInfoList.Create(Subtitles);

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
    Flush;
  finally
    Free;
  end;

  SaveFormSettings(Self);
  FProfiles.Free;
  FAppStringList.Free;
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
begin
  if Assigned(Node) and (FList.Count > 0) then
    FList[Node^.Index]^.Apply := Node^.CheckState = csCheckedNormal;
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

  clbOptions.Items[11] := Format(GetString(FAppStringList, 'etProhibitedChars'), [FProfile^.ProhibitedChars]);
  UpdateItemList;
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.clbOptionsClickCheck(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to clbOptions.Items.Count-1 do
    FOptions[i].Enabled := clbOptions.Checked[i];

  UpdateItemList;
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
              SetSubtitleValues(Index, InitialTime, FinalTime, Text, False, False);
          end;

      // Remove empty/prohibited lines...
      if numList.Count > 0 then
      begin
        numList.Sort;
        for i := numList.Count-1 downto 0 do DeleteSubtitle(numList[i], False, False);
      end;

      UndoInstance.IncrementUndoGroup;
      SubtitleChanged(True, False);
      DoAutoCheckErrors;
      UpdateValues(True);
    finally
      numList.Free;
    end;
  end;
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.UpdateItemList;
var
  i: Integer;
begin
  FList.Errors := [];
  Screen.Cursor := crHourGlass;
  VST.BeginUpdate;
  try
    VST.RootNodeCount := 0;
    FList.ClearItems;

    for i := 0 to Length(FOptions)-1 do
      if FOptions[i].Enabled then FList.Errors := FList.Errors + [FOptions[i].Error];

    if FProfile = NIL then
      FProfile := @AppOptions.Conventions;

    FProfile^.AddHyphenSpace := (cboSpacingHyphen.ItemIndex = 0);

    if cboOCR.Items.Count > 0 then
      FList.FixErrors(OCRFolder + cboOCR.Items[cboOCR.ItemIndex] + '.ocr', FProfile, frmMain.WAVE.GetSceneChangeList)
    else
      FList.FixErrors('', FProfile, frmMain.WAVE.GetSceneChangeList);
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
    Result := GetString(FAppStringList, 'etSnapToShotChangesInCue')
  else if etSnapToShotChangesInCueAway in AError then
    Result := GetString(FAppStringList, 'etSnapToShotChangesInCueAway')
  else if etSnapToShotChangesOutCue in AError then
    Result := GetString(FAppStringList, 'etSnapToShotChangesOutCue')
  else if etChaining in AError then
    Result := FOptions[21].Description
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
    else
      Result := ReplaceEnters(Text);
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.mnuSelectAllClick(Sender: TObject);
begin
  VSTSelect(vsmSelectAll);
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.mnuDeSelectAllClick(Sender: TObject);
begin
  VSTSelect(vsmDeSelectAll);
end;

// -----------------------------------------------------------------------------

procedure TfrmFixSubtitles.mnuInvertSelectionClick(Sender: TObject);
begin
  VSTSelect(vsmInvertSelection);
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

end.

