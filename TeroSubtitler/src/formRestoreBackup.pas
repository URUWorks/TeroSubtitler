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

unit formRestoreBackup;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf,
  LCLType, laz.VirtualTrees, LCLTranslator,procLocalize;

type

  { TfrmRestoreBackup }

  TfrmRestoreBackup = class(TForm)
    btnRestore: TButton;
    btnClose: TButton;
    VST: TLazVirtualStringTree;
    procedure btnCloseClick(Sender: TObject);
    procedure btnRestoreClick(Sender: TObject);
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
    FList: TStringList;
  public

  end;

var
  frmRestoreBackup: TfrmRestoreBackup;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procConfig, procVST, procFiles, procColorTheme,
  FileUtil, UWSubtitleAPI.Formats;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmRestoreBackup }

// -----------------------------------------------------------------------------

function UTF8Compare(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := -WideCompareStr(UTF8Decode(List[Index1]), UTF8Decode(List[Index2]));
end;

// -----------------------------------------------------------------------------

procedure TfrmRestoreBackup.FormCreate(Sender: TObject);
begin
  VSTAddColumn(VST, lngrbDateTime, 150);
  VSTAddColumn(VST, lngrbFilename, 150);

  FList := FindAllFiles(BackupFolder, '*.tero', False);
  FList.CustomSort(@UTF8Compare);

  //VST.DefaultNodeHeight := VST.Font.GetTextHeight('Aj')+4;
  VST.RootNodeCount     := FList.Count;
  btnRestore.Enabled    := (FList.Count > 0);
end;

// -----------------------------------------------------------------------------

procedure TfrmRestoreBackup.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FList.Free;
  CloseAction := caFree;
  frmRestoreBackup := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmRestoreBackup.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmRestoreBackup.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmRestoreBackup.btnRestoreClick(Sender: TObject);
begin
  if (VST.RootNodeCount > 0) and (VST.SelectedCount = 1) then
  begin
    LoadSubtitle(FList[VSTFocusedNode(VST)], sfTeroSubtitler);
    Close;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmRestoreBackup.VSTAdvancedHeaderDraw(Sender: TVTHeader;
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

procedure TfrmRestoreBackup.VSTHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if ColorThemeInstance.GetRealColorMode = cmDark then
    Elements := [hpeBackground];
end;

// -----------------------------------------------------------------------------

procedure TfrmRestoreBackup.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmRestoreBackup.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  s: String;
begin
  if FList.Count > 0 then
  begin
    s := ExtractFileName(FList[Node^.Index]);
    case Column of
      0: CellText := Copy(s, 1, 10) + ' ' + Copy(s, 12, 8).Replace('-', ':');
      1: CellText := ChangeFileExt(Copy(s, 21, s.Length-19), '');
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmRestoreBackup.VSTResize(Sender: TObject);
//var
//  c, wCols: Integer;
begin
//  wCols := VST.Width div VST.Header.Columns.Count;
//  for c := 0 to VST.Header.Columns.Count-1 do
//    VST.Header.Columns[c].Width := wCols;

  VST.Header.Columns[1].Width := (VST.Width - VST.Header.Columns[0].Width) - (GetSystemMetrics(SM_CXVSCROLL)+VST.Header.Columns.Count);
end;

// -----------------------------------------------------------------------------

procedure TfrmRestoreBackup.VSTDblClick(Sender: TObject);
begin
  btnRestore.Click;
end;

// -----------------------------------------------------------------------------

end.

