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

unit formMultipleReplace;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  laz.VirtualTrees, fgl, StrUtils, LCLIntf, LCLType, UWCheckBox, LCLTranslator;

type

  { TListFind }

  PFindItem = ^TFindItem;
  TFindItem = record
    FindText    : String;
    ReplaceText : String;
    UseRE       : Boolean;
  end;

  TListFind = specialize TFPGList<PFindItem>;

  { TfrmMultipleReplace }

  TfrmMultipleReplace = class(TForm)
    btnMinus: TButton;
    btnClose: TButton;
    btnApply: TButton;
    btnPlus: TButton;
    chkRE: TUWCheckBox;
    edtFind: TEdit;
    edtReplace: TEdit;
    lblFind: TLabel;
    lblReplace: TLabel;
    VST: TLazVirtualStringTree;
    procedure btnPlusClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnMinusClick(Sender: TObject);
    procedure edtFindChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VSTAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTHeaderDrawQueryElements(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure VSTResize(Sender: TObject);
  private
    FList: TListFind;
  public

  end;

var
  frmMultipleReplace: TfrmMultipleReplace;

// -----------------------------------------------------------------------------

implementation

uses
  UWSystem.SysUtils, UWSubtitleAPI, procVST, procSubtitle, procTypes, RegExpr,
  procWorkspace, procColorTheme, procLocalize;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmMultipleReplace }

// -----------------------------------------------------------------------------

procedure TfrmMultipleReplace.FormCreate(Sender: TObject);
begin
  //LoadLanguage(Self);
  //TODO: Localize = Seems there is nothing to do, the strings are commented

  FList := TListFind.Create;

  VSTAddColumn(VST, lngmrFind, 150);
  VSTAddColumn(VST, lngmrReplaceWith, 150);
  VSTAddColumn(VST, lngmrRegExpr, 100);

{  LanguageManager.GetAppStringList('MultipleReplaceStrings', FAppStringList);
  VSTAddColumn(VST, GetString(FAppStringList, 'Find'), 150);
  VSTAddColumn(VST, GetString(FAppStringList, 'ReplaceWith'), 150);
  VSTAddColumn(VST, GetString(FAppStringList, 'RegExpr'), 100);}
end;

// -----------------------------------------------------------------------------

procedure TfrmMultipleReplace.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  i: Integer;
begin
  if FList.Count > 0 then
  begin
    for i := FList.Count-1 downto 0 do
    begin
      Dispose(PFindItem(FList.Items[i]));
      FList.Items[i] := NIL;
      FList.Delete(i);
    end;
  end;

  FList.Free;
  CloseAction := caFree;
  frmMultipleReplace := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmMultipleReplace.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmMultipleReplace.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmMultipleReplace.btnPlusClick(Sender: TObject);
var
  Item: PFindItem;
begin
  if edtFind.Text = '' then
  begin
    edtFind.SetFocus;
    Exit;
  end;

  if edtReplace.Text = '' then
  begin
    edtReplace.SetFocus;
    Exit;
  end;

  New(Item);
  with Item^ do
  begin
    FindText    := edtFind.Text;
    ReplaceText := edtReplace.Text;
    UseRE       := chkRE.Checked;
  end;
  FList.Add(Item);

  edtFind.Clear;
  edtReplace.Clear;
  edtFind.SetFocus;
  VST.RootNodeCount := FList.Count;
end;

// -----------------------------------------------------------------------------

procedure TfrmMultipleReplace.btnMinusClick(Sender: TObject);
var
  i: Integer;
begin
  if (VST.SelectedCount > 0) and (FList.Count > 0) then
  begin
    i := VST.FocusedNode^.Index;
    Dispose(PFindItem(FList.Items[i]));
    FList.Items[i] := NIL;
    FList.Delete(i);
    VST.RootNodeCount := FList.Count;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMultipleReplace.edtFindChange(Sender: TObject);
begin
  btnPlus.Enabled := (edtFind.Text <> '') and (edtReplace.Text <> '');
end;

// -----------------------------------------------------------------------------

procedure TfrmMultipleReplace.VSTAdvancedHeaderDraw(Sender: TVTHeader;
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

procedure TfrmMultipleReplace.VSTHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if ColorThemeInstance.GetRealColorMode = cmDark then
    Elements := [hpeBackground];
end;

// -----------------------------------------------------------------------------

procedure TfrmMultipleReplace.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmMultipleReplace.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  case Column of
    0: CellText := FList.Items[Node^.Index]^.FindText;
    1: CellText := FList.Items[Node^.Index]^.ReplaceText;
    2: CellText := iff(FList.Items[Node^.Index]^.UseRE, #$2713, ''); // unicode checkmark
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMultipleReplace.VSTResize(Sender: TObject);
var
  c, wCols: Integer;
begin
  wCols := (VST.Width - (GetSystemMetrics(SM_CXVSCROLL)+VST.Header.Columns.Count)) div VST.Header.Columns.Count;
  for c := 0 to VST.Header.Columns.Count-1 do
    VST.Header.Columns[c].Width := wCols;
end;

// -----------------------------------------------------------------------------

procedure TfrmMultipleReplace.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  btnMinus.Enabled := VST.RootNodeCount > 0;
end;

// -----------------------------------------------------------------------------

procedure ApplyMultipleReplace(const Item: PUWSubtitleItem; const Index: Integer);
var
  i, c: Integer;
  sFind, sReplace: array of String;
  s: String;
begin
  with frmMultipleReplace do
    if FList.Count > 0 then
    begin
      c := 0;
      s := '';
      for i := 0 to FList.Count-1 do
        if not FList[i]^.UseRE then
        begin
          Inc(c);
          SetLength(sFind, c);
          SetLength(sReplace, c);
          sFind[c-1] := FList[i]^.FindText;
          sReplace[c-1] := FList[i]^.ReplaceText;
        end
        else
        begin
          s := ReplaceRegExpr(FList[i]^.FindText, Subtitles[Index].Text, FList[i]^.ReplaceText);
        end;

        if Length(sFind) > 0 then
        begin
          s := StringsReplace(Subtitles[Index].Text, sFind, sReplace, [rfReplaceAll, rfIgnoreCase]);
          SetLength(sFind, 0);
          SetLength(sReplace, 0);
        end;

        if not s.Equals(Subtitles[Index].Text) then
          SetSubtitleText(Index, s, smText, False, False, False);
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMultipleReplace.btnApplyClick(Sender: TObject);
begin
  VSTDoLoop(VST, @ApplyMultipleReplace, dlAll, True, True);
  Close;
end;

// -----------------------------------------------------------------------------

end.

