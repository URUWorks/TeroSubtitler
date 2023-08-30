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

unit formQualityCheck;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  laz.VirtualTrees, UWSystem.XMLLang, LCLIntf, LCLType, procQualityCheck,
  procConventions;

type

  { TfrmQualityCheck }

  TfrmQualityCheck = class(TForm)
    btnClose: TButton;
    cboRules: TComboBox;
    lblRules: TLabel;
    VST: TLazVirtualStringTree;
    procedure btnCloseClick(Sender: TObject);
    procedure cboRulesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VSTAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure VSTDblClick(Sender: TObject);
    procedure VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTHeaderDrawQueryElements(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure VSTResize(Sender: TObject);
  private
    FAppStringList: TAppStringList;
    FProfiles: TProfiles;
    FList: TSubtitleCheckItemList;
    function GetRuleDescription(const ARules: TQualityCheckType): String;
  public

  end;

var
  frmQualityCheck: TfrmQualityCheck;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procVST, procWorkspace, procColorTheme, procConfig, formMain;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmQualityCheck }

// -----------------------------------------------------------------------------

procedure TfrmQualityCheck.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  LoadLanguage(Self);

  FProfiles := TProfiles.Create(ConventionsFileName);
  FillItemsWithConventions(cboRules.Items, FProfiles);
  cboRules.Items.Insert(0, LanguageManager.GetAppString('Custom'));

  FList := NIL;

  LanguageManager.GetAppStringList('QualityCheckStrings', FAppStringList);
  VSTAddColumn(VST, GetString(FAppStringList, 'Index'), 50);
  VSTAddColumn(VST, GetString(FAppStringList, 'Rule'), 50, taLeftJustify);

  i := cboRules.Items.IndexOf(AppOptions.Conventions.Name);
  if i >= 0 then
    cboRules.ItemIndex := i
  else
    cboRules.ItemIndex := 0;

  cboRulesChange(NIL);
end;

// -----------------------------------------------------------------------------

procedure TfrmQualityCheck.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FProfiles.Free;
  FAppStringList.Free;
  if Assigned(FList) then
    FList.Free;

  CloseAction := caFree;
  frmQualityCheck := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmQualityCheck.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmQualityCheck.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmQualityCheck.cboRulesChange(Sender: TObject);
begin
  if DoQualityCheck(cboRules.Text, FProfiles, FList, GetFPS) then
    VST.RootNodeCount := FList.Count
  else
    VST.RootNodeCount := 0;

  VST.Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TfrmQualityCheck.VSTAdvancedHeaderDraw(Sender: TVTHeader;
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

procedure TfrmQualityCheck.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmQualityCheck.VSTHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if ColorThemeInstance.GetRealColorMode = cmDark then
    Elements := [hpeBackground];
end;

// -----------------------------------------------------------------------------

procedure TfrmQualityCheck.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  case Column of
    0: CellText := IntToStr(FList[Node^.Index].Index+1);
    1: CellText := GetRuleDescription(FList[Node^.Index].QC);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmQualityCheck.VSTResize(Sender: TObject);
var
  c, wCols, ClientW: Integer;
begin
  if VST.Header.Columns.Count > 0 then
  begin
    ClientW := VST.Width - VST.Header.Columns[0].Width - (GetSystemMetrics(SM_CXVSCROLL)+(VST.Header.Columns.Count*2));
    wCols := ClientW div (VST.Header.Columns.Count-1);
    for c := 1 to VST.Header.Columns.Count-1 do
      VST.Header.Columns[c].Width := wCols;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmQualityCheck.VSTDblClick(Sender: TObject);
begin
  if FList.Count > 0 then
    VSTSelectNode(frmMain.VST, FList[VSTFocusedNode(VST)].Index, True);
end;

// -----------------------------------------------------------------------------

function TfrmQualityCheck.GetRuleDescription(const ARules: TQualityCheckType): String;

  procedure RuleDesc(ADesc: String);
  begin
    Result := GetString(FAppStringList, ADesc);
  end;

begin
  Result := '';
  case ARules of
    qcCPS: RuleDesc('qcCPS');
    qcWPM: RuleDesc('qcWPM');
    qcCPL: RuleDesc('qcCPL');
    qcMaximumLines: RuleDesc('qcMaximumLines');
    qcMinimumDuration: RuleDesc('qcMinimumDuration');
    qcMaximumDuration: RuleDesc('qcMaximumDuration');
    qcGAP: RuleDesc('qcGAP');
  end;
end;

// -----------------------------------------------------------------------------

end.

