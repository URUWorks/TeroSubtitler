{*
 *  URUWorks Tero Subtitler
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
 *  Copyright (C) 2023-2026 URUWorks, uruworks@gmail.com.
 *}

unit formStyles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, Spin,
  Dialogs, LCLTranslator, UWSubtitleAPI.Styles;

type

  { TfrmStyles }

  TfrmStyles = class(TForm)
    btnApplyToSelection: TButton;
    btnClose: TButton;
    btnCopy: TButton;
    btnDelete: TButton;
    btnNew: TButton;
    btnOK: TButton;
    cbnBack: TColorButton;
    cbnOutlineColor: TColorButton;
    cbnPrimary: TColorButton;
    cbnSecondary: TColorButton;
    cboFont: TComboBox;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    chkOpaqueBox: TCheckBox;
    chkStrikeOut: TCheckBox;
    chkUnderline: TCheckBox;
    edtName: TEdit;
    lblAlignment: TLabel;
    lblAngle: TLabel;
    lblBack: TLabel;
    lblColors: TLabel;
    lblFont: TLabel;
    lblMarginL: TLabel;
    lblMarginR: TLabel;
    lblMarginV: TLabel;
    lblName: TLabel;
    lblOutline: TLabel;
    lblOutlineColor: TLabel;
    lblPrimary: TLabel;
    lblScaleX: TLabel;
    lblScaleY: TLabel;
    lblSecondary: TLabel;
    lblShadow: TLabel;
    lblSpacing: TLabel;
    lstStyles: TListBox;
    pbPreview: TPaintBox;
    rgAlignment: TRadioGroup;
    spnAngle: TFloatSpinEdit;
    spnMarginL: TSpinEdit;
    spnMarginR: TSpinEdit;
    spnMarginV: TSpinEdit;
    spnOutline: TFloatSpinEdit;
    spnScaleX: TFloatSpinEdit;
    spnScaleY: TFloatSpinEdit;
    spnShadow: TFloatSpinEdit;
    spnSize: TFloatSpinEdit;
    spnSpacing: TFloatSpinEdit;
    procedure btnApplyToSelectionClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure ControlChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstStylesSelectionChange(Sender: TObject; User: boolean);
    procedure pbPreviewPaint(Sender: TObject);
  private
    FStyles    : TASSStyles;
    FOrigNames : array of String; // the name of each style when it was read ('' new)
    FLoading   : Boolean;
    FChanged   : Boolean;
    FPlayResY  : Integer; // of the file, for the size of the preview
    procedure FillList;
    procedure LoadStyle(const AIndex: Integer);
    procedure SaveStyle(const AIndex: Integer);
    function UniqueName(const ABase: String): String;
    function ValidNames: Boolean;
    function Commit: Boolean;
    procedure AddStyle(const AStyle: TASSStyle);
  public

  end;

var
  frmStyles: TfrmStyles;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procColorTheme, procDialogs, procLocalize,
  procSubtitle, procVST, procVST_Loops, procUndo, formMain, UWSubtitleAPI, Math;

{$R *.lfm}

// The styles of the subtitle (ASS/SSA): they are kept in the sections of the
// file (Subtitles.ASSHeader), only the Style lines are written again (one
// undo step with the renamed entries). The
// entries have the name of their style (Item.Style, "" = Default).

const
  // rgAlignment (rows from the top, as the numpad) -> ASS alignment
  AlignOfItem: array[0..8] of Integer = (7, 8, 9, 4, 5, 6, 1, 2, 3);

// -----------------------------------------------------------------------------

{ TfrmStyles }

// -----------------------------------------------------------------------------

procedure TfrmStyles.FormCreate(Sender: TObject);
var
  i: Integer;
  s: String;
begin
  FLoading := True;
  FChanged := False;
  cboFont.Items.Assign(Screen.Fonts);

  // the font sizes are lines of PlayResY (288 without it, as libass)
  FPlayResY := 288;
  for s in Subtitles.ASSHeader.Split([LineEnding]) do
    if SameText(Copy(s.Trim, 1, 9), 'PlayResY:') then
      FPlayResY := StrToIntDef(Trim(Copy(s.Trim, 10, MaxInt)), 288);
  if FPlayResY <= 0 then FPlayResY := 288;

  if not ASSReadStyles(Subtitles.ASSHeader, FStyles) or (Length(FStyles) = 0) then
  begin
    // the style that is written when the file has none (as before)
    FStyles := NIL;
    SetLength(FStyles, 1);
    FStyles[0] := ASSNewStyle('Default', Subtitles.FormatProperties^.SSA.DefaultStyleSettings);
    // not written until it's changed: OK without changes doesn't mark
    // the subtitle as modified (saving as ASS writes this same style)
  end;

  SetLength(FOrigNames, Length(FStyles));
  for i := 0 to High(FStyles) do
    FOrigNames[i] := FStyles[i].Name;

  // SSA v4 files and others: only the columns of the file are written
  if Pos('[v4 styles]', LowerCase(Subtitles.ASSHeader)) > 0 then
    Caption := Caption + ' - ' + lngstSSAv4;

  FillList;
  FLoading := False;
  if lstStyles.Count > 0 then
    lstStyles.ItemIndex := 0;
  LoadStyle(lstStyles.ItemIndex);
end;

// -----------------------------------------------------------------------------

procedure TfrmStyles.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmStyles := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmStyles.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmStyles.FillList;
var
  i: Integer;
begin
  lstStyles.Items.BeginUpdate;
  try
    lstStyles.Items.Clear;
    for i := 0 to High(FStyles) do
      lstStyles.Items.Add(FStyles[i].Name);
  finally
    lstStyles.Items.EndUpdate;
  end;
  btnDelete.Enabled := lstStyles.Count > 1;
end;

// -----------------------------------------------------------------------------

procedure TfrmStyles.LoadStyle(const AIndex: Integer);
var
  i: Integer;
begin
  if (AIndex < 0) or (AIndex > High(FStyles)) then Exit;

  FLoading := True;
  try
    with FStyles[AIndex] do
    begin
      edtName.Text             := Name;
      cboFont.Text             := FontName;
      spnSize.Value            := FontSize;
      chkBold.Checked          := Bold;
      chkItalic.Checked        := Italic;
      chkUnderline.Checked     := Underline;
      chkStrikeOut.Checked     := StrikeOut;
      cbnPrimary.ButtonColor      := TColor(ASSColorToRGB(Colors[0]));
      cbnSecondary.ButtonColor    := TColor(ASSColorToRGB(Colors[1]));
      cbnOutlineColor.ButtonColor := TColor(ASSColorToRGB(Colors[2]));
      cbnBack.ButtonColor         := TColor(ASSColorToRGB(Colors[3]));
      spnOutline.Value         := Outline;
      spnShadow.Value          := Shadow;
      chkOpaqueBox.Checked     := BorderStyle = 3;
      rgAlignment.ItemIndex    := 7;
      for i := 0 to High(AlignOfItem) do
        if AlignOfItem[i] = Alignment then
          rgAlignment.ItemIndex := i;
      spnMarginL.Value         := MarginL;
      spnMarginR.Value         := MarginR;
      spnMarginV.Value         := MarginV;
      spnScaleX.Value          := ScaleX;
      spnScaleY.Value          := ScaleY;
      spnSpacing.Value         := Spacing;
      spnAngle.Value           := Angle;
    end;
  finally
    FLoading := False;
  end;
  pbPreview.Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TfrmStyles.SaveStyle(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex > High(FStyles)) then Exit;

  with FStyles[AIndex] do
  begin
    // a comma would break the Style and Dialogue lines
    Name      := StringReplace(Trim(edtName.Text), ',', ';', [rfReplaceAll]);
    FontName  := Trim(cboFont.Text);
    FontSize  := spnSize.Value;
    Bold      := chkBold.Checked;
    Italic    := chkItalic.Checked;
    Underline := chkUnderline.Checked;
    StrikeOut := chkStrikeOut.Checked;
    // the alpha of the file is kept (the color buttons have none)
    Colors[0] := ASSMakeColor(ColorToRGB(cbnPrimary.ButtonColor), ASSColorAlpha(Colors[0]));
    Colors[1] := ASSMakeColor(ColorToRGB(cbnSecondary.ButtonColor), ASSColorAlpha(Colors[1]));
    Colors[2] := ASSMakeColor(ColorToRGB(cbnOutlineColor.ButtonColor), ASSColorAlpha(Colors[2]));
    Colors[3] := ASSMakeColor(ColorToRGB(cbnBack.ButtonColor), ASSColorAlpha(Colors[3]));
    Outline   := spnOutline.Value;
    Shadow    := spnShadow.Value;
    if chkOpaqueBox.Checked then
      BorderStyle := 3
    else if BorderStyle = 3 then
      BorderStyle := 1; // other values of the file are kept
    if rgAlignment.ItemIndex >= 0 then
      Alignment := AlignOfItem[rgAlignment.ItemIndex];
    MarginL   := spnMarginL.Value;
    MarginR   := spnMarginR.Value;
    MarginV   := spnMarginV.Value;
    ScaleX    := spnScaleX.Value;
    ScaleY    := spnScaleY.Value;
    Spacing   := spnSpacing.Value;
    Angle     := spnAngle.Value;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmStyles.ControlChange(Sender: TObject);
var
  i: Integer;
begin
  if FLoading then Exit;
  i := lstStyles.ItemIndex;
  if i < 0 then Exit;

  SaveStyle(i);
  FChanged := True;
  if (Sender = edtName) and (lstStyles.Items[i] <> FStyles[i].Name) then
    lstStyles.Items[i] := FStyles[i].Name;
  pbPreview.Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TfrmStyles.lstStylesSelectionChange(Sender: TObject; User: boolean);
begin
  if not FLoading then
    LoadStyle(lstStyles.ItemIndex);
end;

// -----------------------------------------------------------------------------

function TfrmStyles.UniqueName(const ABase: String): String;
var
  n: Integer;
begin
  Result := ABase;
  n := 1;
  while ASSIndexOfStyle(FStyles, Result) >= 0 do
  begin
    Inc(n);
    Result := ABase + ' ' + IntToStr(n);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmStyles.AddStyle(const AStyle: TASSStyle);
begin
  SetLength(FStyles, Length(FStyles)+1);
  FStyles[High(FStyles)] := AStyle;
  SetLength(FOrigNames, Length(FStyles));
  FOrigNames[High(FOrigNames)] := ''; // new, no entries to rename
  FChanged := True;

  FLoading := True;
  try
    FillList;
    lstStyles.ItemIndex := High(FStyles);
  finally
    FLoading := False;
  end;
  LoadStyle(lstStyles.ItemIndex);
  edtName.SetFocus;
  edtName.SelectAll;
end;

// -----------------------------------------------------------------------------

procedure TfrmStyles.btnNewClick(Sender: TObject);
begin
  AddStyle(ASSNewStyle(UniqueName(lngstNewStyle), Subtitles.FormatProperties^.SSA.DefaultStyleSettings));
end;

// -----------------------------------------------------------------------------

procedure TfrmStyles.btnCopyClick(Sender: TObject);
var
  s: TASSStyle;
begin
  if lstStyles.ItemIndex < 0 then Exit;
  s := FStyles[lstStyles.ItemIndex];
  s.Fields := Copy(s.Fields); // its own array
  s.Name := UniqueName(s.Name);
  AddStyle(s);
end;

// -----------------------------------------------------------------------------

procedure TfrmStyles.btnDeleteClick(Sender: TObject);
var
  i, k: Integer;
begin
  i := lstStyles.ItemIndex;
  if (i < 0) or (Length(FStyles) <= 1) then Exit;

  // the entries with it keep the name, the player uses the default style
  for k := i to High(FStyles)-1 do
  begin
    FStyles[k] := FStyles[k+1];
    FOrigNames[k] := FOrigNames[k+1];
  end;
  SetLength(FStyles, Length(FStyles)-1);
  SetLength(FOrigNames, Length(FStyles));
  FChanged := True;

  FLoading := True;
  try
    FillList;
    if i > High(FStyles) then i := High(FStyles);
    lstStyles.ItemIndex := i;
  finally
    FLoading := False;
  end;
  LoadStyle(i);
end;

// -----------------------------------------------------------------------------

function TfrmStyles.ValidNames: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FStyles) do
    if (FStyles[i].Name = '') or (ASSIndexOfStyle(FStyles, FStyles[i].Name) <> i) then
    begin
      lstStyles.ItemIndex := i;
      ShowErrorMessageDialog(lngstInvalidStyleName);
      edtName.SetFocus;
      Exit;
    end;
  Result := True;
end;

// -----------------------------------------------------------------------------

// the styles to the subtitle: the renamed ones also in the entries
function TfrmStyles.Commit: Boolean;
var
  i, k: Integer;
  Old, NewName: String;
  p: PUWSubtitleItem;
  Renamed: Boolean;
begin
  Result := ValidNames;
  if not Result then Exit;
  if not FChanged then Exit;

  // the styles and the renamed entries are one undo step
  UndoInstance.AddUndoHeader(Subtitles.ASSHeader, False);
  // one pass over the entries, each one by the name it had when the editor
  // was opened: before, a style per pass, and with chained renames (A -> B
  // and B -> C, or a swap A <-> B) an entry was renamed twice
  Renamed := False;
  for i := 0 to Subtitles.Count-1 do
  begin
    p := Subtitles.ItemPointer[i];
    if not Assigned(p) then Continue;
    Old := Trim(p^.Style);
    if Old = '' then Old := 'Default';
    for k := 0 to High(FStyles) do
      if (FOrigNames[k] <> '') and SameText(FOrigNames[k], Old) then
      begin
        NewName := FStyles[k].Name;
        if NewName <> FOrigNames[k] then
        begin
          UndoInstance.AddUndo(utSubtitleChange, i, p^, False);
          p^.Style := NewName;
          Renamed := True;
        end;
        Break;
      end;
  end;

  Subtitles.ASSHeader := ASSWriteStyles(Subtitles.ASSHeader, FStyles);
  UndoInstance.IncrementUndoGroup;
  for k := 0 to High(FStyles) do
    FOrigNames[k] := FStyles[k].Name;
  FChanged := False;

  SubtitleChanged(True, True); // also the track of mpv
  if Renamed then
    UpdateValues(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmStyles.btnApplyToSelectionClick(Sender: TObject);
begin
  if (lstStyles.ItemIndex < 0) or not Commit then Exit;
  if frmMain.VST.SelectedCount = 0 then Exit;

  StyleToApply := FStyles[lstStyles.ItemIndex].Name;
  VSTDoLoop(frmMain.VST, @ApplyStyle, dlSelected, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmStyles.btnOKClick(Sender: TObject);
begin
  if Commit then Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmStyles.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmStyles.pbPreviewPaint(Sender: TObject);
const
  Sample = 'AaBbCc 123';
var
  c: TCanvas;
  x, y, o, dx, dy, sz: Integer;
  w, h: Integer;
begin
  c := pbPreview.Canvas;
  c.Brush.Style := bsSolid;
  c.Brush.Color := $00404040;
  c.FillRect(0, 0, pbPreview.Width, pbPreview.Height);

  // the size in a video of 300 lines, limited to the box: the same for a
  // file of 1080 lines and one without PlayRes (288)
  sz := Round(spnSize.Value * 300 / FPlayResY);
  if sz < 6 then sz := 6;
  if sz > pbPreview.Height - 8 then sz := pbPreview.Height - 8;
  c.Font.Name := cboFont.Text;
  c.Font.Height := -sz;
  c.Font.Style := [];
  if chkBold.Checked then c.Font.Style := c.Font.Style + [fsBold];
  if chkItalic.Checked then c.Font.Style := c.Font.Style + [fsItalic];
  if chkUnderline.Checked then c.Font.Style := c.Font.Style + [fsUnderline];
  if chkStrikeOut.Checked then c.Font.Style := c.Font.Style + [fsStrikeOut];

  w := c.TextWidth(Sample);
  h := c.TextHeight(Sample);
  case AlignOfItem[Max(0, rgAlignment.ItemIndex)] mod 3 of
    1: x := 8;                               // left
    0: x := pbPreview.Width - w - 8;         // right
  else
    x := (pbPreview.Width - w) div 2;        // center
  end;
  y := (pbPreview.Height - h) div 2;
  o := Round(spnOutline.Value);
  if o > 4 then o := 4;
  c.Brush.Style := bsClear;

  if chkOpaqueBox.Checked then
  begin
    c.Brush.Style := bsSolid;
    c.Brush.Color := cbnOutlineColor.ButtonColor;
    c.FillRect(x - o - 2, y - o, x + w + o + 2, y + h + o);
    c.Brush.Style := bsClear;
  end
  else
  begin
    // shadow and outline
    if spnShadow.Value > 0 then
    begin
      c.Font.Color := cbnBack.ButtonColor;
      dx := Round(spnShadow.Value);
      if dx > 4 then dx := 4;
      c.TextOut(x + o + dx, y + o + dx, Sample);
    end;
    if o > 0 then
    begin
      c.Font.Color := cbnOutlineColor.ButtonColor;
      for dx := -o to o do
        for dy := -o to o do
          if (dx <> 0) or (dy <> 0) then
            c.TextOut(x + dx, y + dy, Sample);
    end;
  end;
  c.Font.Color := cbnPrimary.ButtonColor;
  c.TextOut(x, y, Sample);
end;

// -----------------------------------------------------------------------------

end.
