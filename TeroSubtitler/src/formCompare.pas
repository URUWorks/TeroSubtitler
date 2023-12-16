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

unit formCompare;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf,
  LCLType, laz.VirtualTrees, UWSubtitleAPI, UWLayout, LCLTranslator, procLocalize;

type

  { TfrmCompare }

  TfrmCompare = class(TForm)
    btnClose: TButton;
    btnFile1: TButton;
    btnFile2: TButton;
    edtFile1: TEdit;
    edtFile2: TEdit;
    lyoFile1: TUWLayout;
    lyoFile2: TUWLayout;
    lyoBottom: TUWLayout;
    VST1: TLazVirtualStringTree;
    VST2: TLazVirtualStringTree;
    procedure btnCloseClick(Sender: TObject);
    procedure btnFile1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VST1AdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure VST1DrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VST1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VST1HeaderDrawQueryElements(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure VST1Resize(Sender: TObject);
    procedure VST1Scroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
  private
    SAPI1: TUWSubtitles;
    SAPI2: TUWSubtitles;
    SAPI: TUWSubtitles;
    procedure CompareSubtitles;
  public

  end;

var
  frmCompare: TfrmCompare;

// -----------------------------------------------------------------------------

implementation

uses
  procVST, procTypes, RegExpr, procWorkspace, procColorTheme, UWSystem.XMLLang,
  procSubtitle;

const
  DIFF_INITIAL = 1;
  DIFF_FINAL   = 2;
  DIFF_TEXT    = 4;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmCompare }

// -----------------------------------------------------------------------------

procedure TfrmCompare.FormCreate(Sender: TObject);
begin
  VSTAddColumn(VST1, lngteIndex, 70);
  VSTAddColumn(VST1, lngteInitialTime, 100);
  VSTAddColumn(VST1, lngteFinalTime, 100);
  VSTAddColumn(VST1, lngteText, 150);
  VST2.Header.Columns.Assign(VST1.Header.Columns);

  SAPI := NIL;
  SAPI1 := TUWSubtitles.Create;
  SAPI2 := TUWSubtitles.Create;
end;

// -----------------------------------------------------------------------------

procedure TfrmCompare.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SAPI := NIL;
  SAPI1.Free;
  SAPI2.Free;

  CloseAction := caFree;
  frmCompare := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmCompare.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmCompare.FormResize(Sender: TObject);
begin
  lyoFile1.Width := ClientWidth div 2;
end;

// -----------------------------------------------------------------------------

procedure TfrmCompare.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmCompare.VST1AdvancedHeaderDraw(Sender: TVTHeader;
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

procedure TfrmCompare.VST1HeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if ColorThemeInstance.GetRealColorMode = cmDark then
    Elements := [hpeBackground];
end;

// -----------------------------------------------------------------------------

procedure TfrmCompare.VST1DrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  case Column of
    0: if (SAPI[Node^.Index].Data > 0) then
       begin
         VSTPaintCell(TargetCanvas, CellRect, clRed);
         TargetCanvas.Brush.Style := bsClear;
       end;
    1: if (SAPI[Node^.Index].Data and DIFF_INITIAL) = DIFF_INITIAL then TargetCanvas.Font.Color := clRed;
    2: if (SAPI[Node^.Index].Data and DIFF_FINAL)   = DIFF_FINAL   then TargetCanvas.Font.Color := clRed;
    3: if (SAPI[Node^.Index].Data and DIFF_TEXT)    = DIFF_TEXT    then TargetCanvas.Font.Color := clRed;
  end;

  DefaultDraw := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmCompare.VST1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  SUB: TUWSubtitles;
begin
  CellText := '';
  if SAPI <> NIL then
  begin
    if Column = 0 then
      CellText := IntToStr(Node^.Index+1)
    else
    begin
      if Sender = VST1 then
        SUB := SAPI1
      else
        SUB := SAPI2;

      if SUB.ValidIndex(Node^.Index) then
        case Column of
          1: CellText := GetTimeStr(SUB[Node^.Index].InitialTime);
          2: CellText := GetTimeStr(SUB[Node^.Index].FinalTime);
          3: CellText := SUB[Node^.Index].Text;
        end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmCompare.VST1Resize(Sender: TObject);
var
  c, wCols: Integer;
  VST: TLazVirtualStringTree;
begin
  VST := TLazVirtualStringTree(Sender);
  wCols := ((VST.Width-VST.Header.Columns[0].Width) - (GetSystemMetrics(SM_CXVSCROLL)+VST.Header.Columns.Count)) div (VST.Header.Columns.Count-1);
  for c := 1 to VST.Header.Columns.Count-1 do
    VST.Header.Columns[c].Width := wCols;
end;

// -----------------------------------------------------------------------------

procedure TfrmCompare.VST1Scroll(Sender: TBaseVirtualTree; DeltaX,
  DeltaY: Integer);
begin
  if Sender = VST1 then
    VST2.OffsetY := VST1.OffsetY
  else if Sender = VST2 then
    VST1.OffsetY := VST2.OffsetY;
end;

// -----------------------------------------------------------------------------

procedure TfrmCompare.btnFile1Click(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  try
    Title   := lngOpenFile;
    Filter  := Subtitles.FillDialogFilter(lngAllSupportedFiles);
    Options := Options + [ofFileMustExist];

    if Execute then
    begin
      if TButton(Sender).Name = 'btnFile1' then
      begin
        edtFile1.Text := FileName;
        SAPI1.LoadFromFile(FileName, NIL, GetFPS);
      end
      else
      begin
        edtFile2.Text := FileName;
        SAPI2.LoadFromFile(FileName, NIL, GetFPS);
      end;

      if (SAPI1.Count > 0) and (SAPI2.Count > 0) then
        CompareSubtitles;
    end;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmCompare.CompareSubtitles;
var
  i: Integer;
begin
  if SAPI1.Count > SAPI2.Count then
    SAPI := SAPI1
  else
    SAPI := SAPI2;

  for i := 0 to SAPI.Count-1 do
  begin
    SAPI.ItemPointer[i]^.Data := 0;

    if SAPI1[i].InitialTime <> SAPI2[i].InitialTime then
      SAPI.ItemPointer[i]^.Data := SAPI[i].Data or DIFF_INITIAL;

    if SAPI1[i].FinalTime <> SAPI2[i].FinalTime then
      SAPI.ItemPointer[i]^.Data := SAPI[i].Data or DIFF_FINAL;

    if SAPI1[i].Text <> SAPI2[i].Text then
      SAPI.ItemPointer[i]^.Data := SAPI[i].Data or DIFF_TEXT;
  end;

  VST1.RootNodeCount := SAPI.Count;
  VST2.RootNodeCount := SAPI.Count;
end;

// -----------------------------------------------------------------------------

end.

