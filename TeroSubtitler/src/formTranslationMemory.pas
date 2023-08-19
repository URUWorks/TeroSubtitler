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

unit formTranslationMemory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, laz.VirtualTrees,
  LCLIntf, LCLType, StdCtrls;

type

  { TfrmTranslationMemory }

  TfrmTranslationMemory = class(TForm)
    btnClose: TButton;
    btnCopy: TButton;
    btnValidate: TButton;
    btnUse: TButton;
    VST: TLazVirtualStringTree;
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnUseClick(Sender: TObject);
    procedure btnValidateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VSTAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
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
    function GetItemAtIndex(const Index: Integer): String;
    procedure FindSimilary(const AText: String);
    procedure ShowSimilary(const AIndex: Integer);
  end;

var
  frmTranslationMemory: TfrmTranslationMemory;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

uses
  procTypes, procVST, RegExpr, procWorkspace, procColorTheme, procConfig,
  Clipbrd, formMain, UWSystem.XMLLang;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmTranslationMemory }

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemory.FormCreate(Sender: TObject);
var
  FAppStringList: TAppStringList = NIL;
begin
  LoadLanguage(Self);

  LanguageManager.GetAppStringList('TranslationMemoryStrings', FAppStringList);
  VSTAddColumn(VST, GetString(FAppStringList, 'Original'), 150, taLeftJustify);
  VSTAddColumn(VST, GetString(FAppStringList, 'Translated'), 150, taLeftJustify);
  VSTAddColumn(VST, GetString(FAppStringList, 'Percent'), 80, taLeftJustify);
  FAppStringList.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemory.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveFormSettings(Self);
  CloseAction := caFree;
  frmTranslationMemory := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemory.FormShow(Sender: TObject);
begin
  LoadFormSettings(Self);
  CheckColorTheme(Self);
  CheckForTranslationMemory(VSTFocusedNode(frmMain.VST));
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemory.VSTAdvancedHeaderDraw(Sender: TVTHeader;
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

procedure TfrmTranslationMemory.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if (Column = 2) and (TMX.Map.Keys[Node^.Index] = 1) then
    VSTPaintCell(TargetCanvas, CellRect, clLime);
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemory.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
  if (Column = 2) and (TMX.Map.Keys[Node^.Index] = 1) then
    TargetCanvas.Font.Color := clBlack;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemory.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  case Column of
    0: CellText := TMX.ItemFromMap(Node^.Index).Original;
    1: CellText := TMX.ItemFromMap(Node^.Index).Translated;
    2: CellText := FloatToStrF(TMX.Map.Keys[Node^.Index]*100, ffFixed, 0, 0);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemory.VSTHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if ColorThemeInstance.GetRealColorMode = cmDark then
    Elements := [hpeBackground];
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemory.VSTResize(Sender: TObject);
var
  c, wCols: Integer;
begin
  if VST.Header.Columns.Count > 2 then
  begin
    wCols := ((VST.Width - VST.Header.Columns[2].Width) - (GetSystemMetrics(SM_CXVSCROLL)+(VST.Header.Columns.Count)*2)) div (VST.Header.Columns.Count-1);
    for c := 1 to VST.Header.Columns.Count-1 do
      VST.Header.Columns[c].Width := wCols;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemory.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemory.btnCopyClick(Sender: TObject);
begin
  if Assigned(VST.FocusedNode) then
    Clipboard.AsText := GetItemAtIndex(VST.FocusedNode^.Index);
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemory.btnUseClick(Sender: TObject);
begin
  if Assigned(VST.FocusedNode) then
    with VST.FocusedNode^ do
      frmMain.mmoTranslation.Text := GetItemAtIndex(Index);
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemory.btnValidateClick(Sender: TObject);
begin
  if Assigned(VST.FocusedNode) then
    with VST.FocusedNode^ do
    begin
      Subtitles.ItemPointer[VSTFocusedNode(frmMain.VST)]^.Data := TMX.IndexFromMap(Index)+1;
      frmMain.mmoTranslation.Text := GetItemAtIndex(Index);
      frmMain.actNextSubtitle.Execute;
    end;
end;

// -----------------------------------------------------------------------------

function TfrmTranslationMemory.GetItemAtIndex(const Index: Integer): String;
begin
  Result := TMX.ItemFromMap(Index).Translated;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemory.FindSimilary(const AText: String);
begin
  VST.RootNodeCount := TMX.FindSimilary(AText);
  VST.Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemory.ShowSimilary(const AIndex: Integer);
begin
  VST.RootNodeCount := TMX.FillMapWithIndex(AIndex);
  VST.Invalidate;
end;

// -----------------------------------------------------------------------------

end.

