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

unit formAdjustSubtitle;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  laz.VirtualTrees, UWTimeEdit, UWLayout, UWRadioButton, procTypes, LCLIntf,
  LCLType, LCLTranslator, procLocalize;

type

  { TfrmAdjustSubtitle }

  TfrmAdjustSubtitle = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    btnPlus: TButton;
    btnMinus: TButton;
    lblIndex: TLabel;
    lblFirstSpoken: TLabel;
    lblLastSpoken: TLabel;
    lyoSimple: TUWLayout;
    rdoAdvanced: TUWRadioButton;
    rdoSimple: TUWRadioButton;
    spnIndex: TSpinEdit;
    tedFirst: TUWTimeEdit;
    tedLast: TUWTimeEdit;
    lyoAdvanced: TUWLayout;
    tedTime: TUWTimeEdit;
    VST: TLazVirtualStringTree;
    procedure btnPlusClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnMinusClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rdoSimpleChange(Sender: TObject);
    procedure spnIndexChange(Sender: TObject);
    procedure tedFirstTimeChange(Sender: TObject; const NewTime: Cardinal);
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
    AdjustList: TAdjustSubtitles;
  public

  end;

var
  frmAdjustSubtitle: TfrmAdjustSubtitle;

// -----------------------------------------------------------------------------

implementation

uses
  procVST, procWorkspace, procSubtitle, procColorTheme, formMain;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmAdjustSubtitle }

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitle.FormCreate(Sender: TObject);
begin
  tedFirst.FPS      := GetFPS;
  tedLast.FPS       := tedFirst.FPS;
  tedFirst.TimeMode := GetTimeEditMode;
  tedLast.TimeMode  := tedFirst.TimeMode;

  if Subtitles.Count > 1 then
  begin
    tedFirst.Value := Subtitles[0].InitialTime;
    tedLast.Value  := Subtitles[Subtitles.Count-1].InitialTime;
  end;

  spnIndex.MaxValue := Subtitles.Count;
  spnIndex.MinValue := 1;

  //Lng AdjustSubtitleStrings
  VSTAddColumn(VST, lngIndex, 75);
  VSTAddColumn(VST, lngOldTime, 150);
  VSTAddColumn(VST, lngNewTime, 100);

  spnIndexChange(Sender);
  rdoSimple.Checked := True;

  {$IFNDEF WINDOWS}
  PrepareCustomControls(Self);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitle.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SetLength(AdjustList, 0);

  CloseAction := caFree;
  frmAdjustSubtitle := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitle.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitle.VSTAdvancedHeaderDraw(Sender: TVTHeader;
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

procedure TfrmAdjustSubtitle.VSTHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if ColorThemeInstance.GetRealColorMode = cmDark then
    Elements := [hpeBackground];
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitle.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitle.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  if Length(AdjustList) > 0 then
    case Column of
      0: CellText := (AdjustList[Node^.Index].SubtitleIndex+1).ToString;
      1: CellText := GetInitialTimeStr(AdjustList[Node^.Index].SubtitleIndex);
      2: CellText := GetTimeStr(AdjustList[Node^.Index].NewInitialTime);
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitle.VSTResize(Sender: TObject);
var
  c, wCols: Integer;
begin
  wCols := (VST.Width - (GetSystemMetrics(SM_CXVSCROLL)+VST.Header.Columns.Count)) div VST.Header.Columns.Count;
  for c := 0 to VST.Header.Columns.Count-1 do
    VST.Header.Columns[c].Width := wCols;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitle.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitle.btnMinusClick(Sender: TObject);
var
  Index, len, rest: Integer;
begin
  Index := VSTFocusedNode(VST);
  len   := Length(AdjustList);
  rest  := len - Index;

  if (Index > -1) and (len > 0) and (rest > 0) then
  begin
    Move(AdjustList[Index+1], AdjustList[Index], SizeOf(TAdjustItem)*rest);
    SetLength(AdjustList, len-1);
    VST.RootNodeCount := Length(AdjustList);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitle.rdoSimpleChange(Sender: TObject);
begin
  lyoSimple.Enabled   := rdoSimple.Checked;
  lyoAdvanced.Enabled := not lyoSimple.Enabled;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitle.spnIndexChange(Sender: TObject);
begin
  tedTime.Value := Subtitles[spnIndex.Value-1].InitialTime;
  VSTSelectNode(frmMain.VST, spnIndex.Value-1, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitle.tedFirstTimeChange(Sender: TObject;
  const NewTime: Cardinal);
begin
  frmMain.MPV.SetMediaPosInMs(TUWTimeEdit(Sender).Value);
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitle.btnApplyClick(Sender: TObject);
var
  AdjSub : TAdjustSubtitles;
begin
  // Simple mode
  if rdoSimple.Checked then
  begin
    SetLength(AdjSub, 2);
    try
      AdjSub[0].SubtitleIndex  := 0;
      AdjSub[0].NewInitialTime := tedFirst.Value;
      AdjSub[1].SubtitleIndex  := Subtitles.Count-1;
      AdjSub[1].NewInitialTime := tedLast.Value;

      VSTAdjustSubtitles(AdjSub);
    finally
      SetLength(AdjSub, 0);
    end;
  end
  // Advanced mode
  else if (VST.RootNodeCount > 0) and (Length(AdjustList) > 0) then
  begin
    VSTAdjustSubtitles(AdjustList);
  end;

  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitle.btnPlusClick(Sender: TObject);
var
  Item: TAdjustItem;
  i   : Integer;
  dup : Boolean;
begin
  dup := False;

  with Item do
  begin
    SubtitleIndex  := spnIndex.Value-1;
    NewInitialTime := tedTime.Value;
  end;

  for i := 0 to Length(AdjustList)-1 do
    if (AdjustList[i].SubtitleIndex) = Item.SubtitleIndex then
    begin
      dup := True;
      Break;
    end;

  if not dup then
  begin
    SetLength(AdjustList, Length(AdjustList)+1);
    AdjustList[Length(AdjustList)-1] := Item;

    VST.RootNodeCount := Length(AdjustList);
  end;
end;

// -----------------------------------------------------------------------------

end.

