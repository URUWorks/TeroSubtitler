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

unit formTBXList;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  laz.VirtualTrees, LCLIntf, LCLType, LCLTranslator;

type

  { TfrmTBXList }

  TfrmTBXList = class(TForm)
    btnClose: TButton;
    btnCopy: TButton;
    edtFind: TEdit;
    lblFind: TLabel;
    lblCount: TLabel;
    lblTotal: TLabel;
    VST: TLazVirtualStringTree;
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure edtFindChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VSTAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTHeaderDrawQueryElements(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure VSTResize(Sender: TObject);
  private

  public

  end;

var
  frmTBXList: TfrmTBXList;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procVST, RegExpr, procWorkspace, procColorTheme, procConfig,
  Clipbrd;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmTBXList }

// -----------------------------------------------------------------------------

procedure TfrmTBXList.FormCreate(Sender: TObject);
begin
  VSTAddColumn(VST, TBX.Langs^.SrcLang, 150, taLeftJustify);
  VSTAddColumn(VST, TBX.Langs^.DstLang, 150, taLeftJustify);

  VST.RootNodeCount := TBX.Items.Count;
  lblCount.Caption := IntToStr(VST.RootNodeCount);
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXList.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveFormSettings(Self);
  CloseAction := caFree;
  frmTBXList := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXList.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
  LoadFormSettings(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXList.VSTAdvancedHeaderDraw(Sender: TVTHeader;
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

procedure TfrmTBXList.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXList.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  with TBX.Items[Node^.Index]^ do
    case Column of
      0: CellText := Original;
      1: CellText := Translated;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXList.VSTHeaderDrawQueryElements(
  Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
  var Elements: THeaderPaintElements);
begin
  if ColorThemeInstance.GetRealColorMode = cmDark then
    Elements := [hpeBackground];
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXList.VSTResize(Sender: TObject);
var
  c, wCols: Integer;
begin
  wCols := (VST.Width - (GetSystemMetrics(SM_CXVSCROLL)+(VST.Header.Columns.Count*2))) div (VST.Header.Columns.Count);
  for c := 0 to VST.Header.Columns.Count-1 do
    VST.Header.Columns[c].Width := wCols;
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXList.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXList.btnCopyClick(Sender: TObject);
begin
  if VST.FocusedNode <> NIL then
    Clipboard.AsText := TBX.Items[VST.FocusedNode^.Index]^.Translated;
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXList.edtFindChange(Sender: TObject);
begin
  if VST.RootNodeCount > 0 then
    VSTSelectNode(VST, TBX.Find(edtFind.Text), True, True);
end;

// -----------------------------------------------------------------------------

end.

