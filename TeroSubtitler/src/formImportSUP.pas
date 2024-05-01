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
 *  Copyright (C) 2023-2024 URUWorks, uruworks@gmail.com.
 *}

unit formImportSUP;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf,
  LCLType, laz.VirtualTrees, LCLTranslator, ExtCtrls, procLocalize,
  BlurayPGSParser;

type

  { TfrmImportSUP }

  TfrmImportSUP = class(TForm)
    btnImport: TButton;
    btnClose: TButton;
    imgSUP: TImage;
    pnlSUP: TPanel;
    VST: TLazVirtualStringTree;
    procedure btnCloseClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VSTAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure VSTDblClick(Sender: TObject);
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
    FList: TBlurayPGSParser;
  public

  end;

var
  frmImportSUP: TfrmImportSUP;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procVST, procColorTheme, FileUtil, procSubtitle,
  UWSubtitleAPI.Formats, UWSubtitles.Utils, BGRABitmap;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmImportSUP }

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.FormCreate(Sender: TObject);
//var
//  TextHeight: Integer;
begin
  VSTAddColumn(VST, lngIndex, 55);
  VSTAddColumn(VST, lnghTimes, 90);
  //VSTAddColumn(VST, lnghDuration, 60);
  VSTAddColumn(VST, lnghText, 200, taLeftJustify);

  //TextHeight := VST.Canvas.Font.GetTextHeight('W');
  //VST.DefaultNodeHeight := (TextHeight*2);
  //VST.Header.Height     := TextHeight+4;

  FList := TBlurayPGSParser.Create;

  with TOpenDialog.Create(Self) do
  try
    if Execute then
      FList.Parse(FileName);
  finally
    Free;
  end;

  VST.RootNodeCount := FList.DisplaySets.Count;
  btnImport.Enabled := VST.RootNodeCount > 0;

  if btnImport.Enabled then
    VSTSelectNode(VST, 0, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FList.Free;
  CloseAction := caFree;
  frmImportSUP := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.btnImportClick(Sender: TObject);
begin
  if (VST.RootNodeCount > 0) and (VST.SelectedCount = 1) then
  begin
    Close;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.VSTAdvancedHeaderDraw(Sender: TVTHeader;
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

procedure TfrmImportSUP.VSTHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if ColorThemeInstance.GetRealColorMode = cmDark then
    Elements := [hpeBackground];
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  bmp: TBGRABitmap;
begin
  if VST.FocusedNode = NIL then Exit;

  bmp := FList.GetBitmap(VSTFocusedNode(VST));
  if bmp = NIL then
    Exit;

  //Label2.Caption := inttostr(bmp.Width) + 'x' + inttostr(bmp.Height);
  imgSUP.Picture.Assign(bmp.Bitmap);
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  it, ft: Integer;
begin
  if FList.DisplaySets.Count > 0 then
  begin
    case Column of
      0: CellText := IntToStr(Node^.Index+1);
      1: CellText := GetTimeStr(FList.DisplaySets[Node^.Index]^.InCue) + '->' + GetTimeStr(FList.DisplaySets[Node^.Index]^.OutCue);
//      2: begin
//           RoundFramesValue(FList.DisplaySets[Node^.Index]^.InCue, FList.DisplaySets[Node^.Index]^.OutCue, Workspace.FPS.OutputFPS, it, ft);
//           CellText := GetTimeStr(ft-it, True);
//         end;
      2: CellText := '';
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.VSTResize(Sender: TObject);
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

procedure TfrmImportSUP.VSTDblClick(Sender: TObject);
begin
  btnImport.Click;
end;

// -----------------------------------------------------------------------------

end.

