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

unit formTranslationMemoryList;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  laz.VirtualTrees, LCLIntf, LCLType;

type

  { TfrmTranslationMemoryList }

  TfrmTranslationMemoryList = class(TForm)
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
  frmTranslationMemoryList: TfrmTranslationMemoryList;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procVST, RegExpr, procWorkspace, procColorTheme, procConfig,
  Clipbrd, UWSystem.XMLLang, UWSystem.StrUtils;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmTranslationMemoryList }

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemoryList.FormCreate(Sender: TObject);
begin
  LoadLanguage(Self);

  VSTAddColumn(VST, TMX.Langs^.SrcLang, 150, taLeftJustify);
  VSTAddColumn(VST, TMX.Langs^.DstLang, 150, taLeftJustify);

  VST.RootNodeCount := TMX.Items.Count;
  lblCount.Caption := IntToStr(VST.RootNodeCount);
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemoryList.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveFormSettings(Self);
  CloseAction := caFree;
  frmTranslationMemoryList := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemoryList.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
  LoadFormSettings(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemoryList.VSTAdvancedHeaderDraw(Sender: TVTHeader;
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

procedure TfrmTranslationMemoryList.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemoryList.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  with TMX.Items[Node^.Index]^ do
    case Column of
      0: CellText := ReplaceEnters(Original);
      1: CellText := ReplaceEnters(Translated);
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemoryList.VSTHeaderDrawQueryElements(
  Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
  var Elements: THeaderPaintElements);
begin
  if ColorThemeInstance.GetRealColorMode = cmDark then
    Elements := [hpeBackground];
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemoryList.VSTResize(Sender: TObject);
var
  c, wCols: Integer;
begin
  wCols := (VST.Width - (GetSystemMetrics(SM_CXVSCROLL)+(VST.Header.Columns.Count*2))) div (VST.Header.Columns.Count);
  for c := 0 to VST.Header.Columns.Count-1 do
    VST.Header.Columns[c].Width := wCols;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemoryList.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemoryList.btnCopyClick(Sender: TObject);
begin
  if VST.FocusedNode <> NIL then
    Clipboard.AsText := TMX.Items[VST.FocusedNode^.Index]^.Translated;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemoryList.edtFindChange(Sender: TObject);
begin
  if VST.RootNodeCount > 0 then
    VSTSelectNode(VST, TMX.Find(edtFind.Text), True);
end;

// -----------------------------------------------------------------------------

end.

