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

unit formCharacterMap;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, StdCtrls,
  ComCtrls, Grids, LCLIntf, LCLType, LCLUnicodeData, LazUTF8, UWLayout;

type

  TInsertCharacterEvent = procedure(const C: TUTF8Char) of object;

  { TfrmCharacterMap }

  TfrmCharacterMap = class(TForm)
    btnInsert: TButton;
    btnClose: TButton;
    btnCopy: TButton;
    btnMinus: TButton;
    btnPlus: TButton;
    cboUnicodeRange: TComboBox;
    cboFavorites: TComboBox;
    lblFavorites: TLabel;
    pnlUnicodeChar: TPanel;
    lblUnicodeRange: TLabel;
    lblUnicodeCharInfo: TLabel;
    GridUnicode: TStringGrid;
    lyoUnicode: TUWLayout;
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnMinusClick(Sender: TObject);
    procedure btnPlusClick(Sender: TObject);
    procedure cboUnicodeRangeSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridUnicodeKeyPress(Sender: TObject; var Key: char);
    procedure GridUnicodePrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure GridUnicodeSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  private
    FInsertCharacter: TInsertCharacterEvent;
    FUnicodeBlockIndex: Integer;
    FFavoritesChanged: Boolean;
    procedure FillUnicodeRangeList(const ASorted: Boolean = False);
    procedure FillUnicodeGrid;
    procedure FillFavoriteChars;
    procedure UpdateFavoriteChars;
    procedure DoStatusUnicodeGrid(const ACol, ARow: Integer);
    function UnicodeBlockSelected: Boolean;
    function UnicodeBlockIndexByName(AName: String): Integer;
  public
    property OnInsertCharacter: TInsertCharacterEvent read FInsertCharacter write FInsertCharacter;
  end;

var
  frmCharacterMap: TfrmCharacterMap;

  // -----------------------------------------------------------------------------

implementation

uses
  Clipbrd, procWorkspace, procTypes, formMain;

{$R *.lfm}

const
  NOT_SELECTED = Low(UnicodeBlocks)-1;

// -----------------------------------------------------------------------------

{ TfrmCharacterMap }

// -----------------------------------------------------------------------------

procedure TfrmCharacterMap.FormCreate(Sender: TObject);
begin
  FInsertCharacter := NIL;
  FFavoritesChanged := False;
  cboUnicodeRange.DropDownCount := 25;
  GridUnicode.Font.Size := 14;
  FUnicodeBlockIndex := NOT_SELECTED;
  FillUnicodeRangeList;
  FillUnicodeGrid;
  FillFavoriteChars;
end;

// -----------------------------------------------------------------------------

procedure TfrmCharacterMap.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  UpdateFavoriteChars;
  FillMenuWithUnicodeChars(frmMain.mnuEditInsertChar);

  CloseAction := caFree;
  frmCharacterMap := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmCharacterMap.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmCharacterMap.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmCharacterMap.btnCopyClick(Sender: TObject);
begin
  Clipboard.AsText := GridUnicode.Cells[GridUnicode.Col, GridUnicode.Row];
end;

// -----------------------------------------------------------------------------

procedure TfrmCharacterMap.btnInsertClick(Sender: TObject);
var
  s: String;
begin
  s := GridUnicode.Cells[GridUnicode.Col, GridUnicode.Row];
  if (s <> '') and (Assigned(FInsertCharacter)) then
    FInsertCharacter(s);
end;

// -----------------------------------------------------------------------------

procedure TfrmCharacterMap.btnMinusClick(Sender: TObject);
var
  i: Integer;
begin
  FFavoritesChanged := True;
  if cboFavorites.ItemIndex >= 0 then
  begin
    i := cboFavorites.ItemIndex;
    cboFavorites.Items.Delete(cboFavorites.ItemIndex);
    if i > 0 then
      Dec(i)
    else
      i := 0;
    cboFavorites.ItemIndex := i;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmCharacterMap.btnPlusClick(Sender: TObject);
var
  s: String;
begin
  FFavoritesChanged := True;
  s := GridUnicode.Cells[GridUnicode.Col, GridUnicode.Row];
  if not s.IsEmpty then
    cboFavorites.ItemIndex := cboFavorites.Items.Add(s);
end;

// -----------------------------------------------------------------------------

procedure TfrmCharacterMap.GridUnicodeKeyPress(Sender: TObject; var Key: char);
var
  sg: TStringGrid;
  s: String;
begin
  if Key = #13 then
  begin
    sg := Sender as TStringGrid;
    s := sg.Cells[sg.Col, sg.Row];
    if (s <> '') and (Assigned(FInsertCharacter)) then
      FInsertCharacter(s);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmCharacterMap.cboUnicodeRangeSelect(Sender: TObject);
begin
  FUnicodeBlockIndex := UnicodeBlockIndexByName(cboUnicodeRange.Text);
  FillUnicodeGrid;
end;

// -----------------------------------------------------------------------------

procedure TfrmCharacterMap.GridUnicodePrepareCanvas(Sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
var
  ts: TTextStyle;
begin
  with (Sender as TStringGrid) do
  begin
    ts := Canvas.TextStyle;
    ts.Alignment := taCenter;
    Canvas.TextStyle := ts;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmCharacterMap.GridUnicodeSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  DoStatusUnicodeGrid(aCol, aRow);
end;

// -----------------------------------------------------------------------------

procedure TfrmCharacterMap.FillUnicodeRangeList(const ASorted: Boolean = False);
var
  BlockIdx: Integer;
begin
  cboUnicodeRange.Items.Clear;
  cboUnicodeRange.Sorted := ASorted;

  for BlockIdx := Low(UnicodeBlocks) to High(UnicodeBlocks) do
    cboUnicodeRange.Items.Append(UnicodeBlocks[BlockIdx].PG);

  if not UnicodeBlockSelected then
    FUnicodeBlockIndex := Low(UnicodeBlocks);

  cboUnicodeRange.Text := UnicodeBlocks[FUnicodeBlockIndex].PG;
end;

// -----------------------------------------------------------------------------

procedure TfrmCharacterMap.FillUnicodeGrid;

  function RoundUp(const Value, Divi: Integer): Integer;
  begin
    if Value mod Divi = 0 then
      Result := Value div Divi
    else
      Result := (Value div Divi)+1;
  end;

var
  cnt, x, y: integer;
  S, E: integer;
begin
  GridUnicode.Clear;
  if not UnicodeBlockSelected then
    Exit;

  S := UnicodeBlocks[FUnicodeBlockIndex].S;
  E := UnicodeBlocks[FUnicodeBlockIndex].E;
  GridUnicode.ColCount := 16;
  GridUnicode.RowCount := RoundUp(E-S, 16);
  cnt := 0;
  for y := 0 to GridUnicode.RowCount-1 do
    for x := 0 to GridUnicode.ColCount-1 do
    begin
      if S + Cnt <= E then
        GridUnicode.Cells[x, y] := UnicodeToUTF8(S + Cnt);

      Inc(cnt);
    end;

  GridUnicode.AutoSizeColumns;
end;

// -----------------------------------------------------------------------------

procedure TfrmCharacterMap.FillFavoriteChars;
var
  i: Integer;
begin
  cboFavorites.Clear;
  with AppOptions do
    for i := 0 to Length(UnicodeChars)-1 do
      cboFavorites.Items.Add(UnicodeChars[i]);

  if cboFavorites.Items.Count > 0 then cboFavorites.ItemIndex := 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmCharacterMap.UpdateFavoriteChars;
var
  i: Integer;
begin
  with AppOptions do
  begin
    SetLength(UnicodeChars, cboFavorites.Items.Count);
    for i := 0 to cboFavorites.Items.Count-1 do
      UnicodeChars[i] := cboFavorites.Items[i];
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmCharacterMap.DoStatusUnicodeGrid(const ACol, ARow: Integer);
var
  S: Cardinal;
  tmp, tmp2: String;
  i: Integer;
begin
  if not UnicodeBlockSelected then Exit;
  S := UnicodeBlocks[FUnicodeBlockIndex].S + (ACol) + (ARow*16);
  tmp := UnicodeToUTF8(S);
  tmp2 := '';
  for i := 1 to Length(tmp) do
    tmp2 := tmp2 + '$' + IntToHex(Ord(tmp[i]), 2);

  lblUnicodeCharInfo.Caption := 'U+' + IntToHex(S, 4) + ', UTF-8: ' + tmp2;
  pnlUnicodeChar.Caption := tmp;
end;

// -----------------------------------------------------------------------------

function TfrmCharacterMap.UnicodeBlockSelected: Boolean;
begin
  Result := (FUnicodeBlockIndex >= Low(UnicodeBlocks)) and (FUnicodeBlockIndex <= High(UnicodeBlocks));
end;

// -----------------------------------------------------------------------------

function TfrmCharacterMap.UnicodeBlockIndexByName(AName: String): Integer;
var
  BlockIdx: Integer;
begin
  for BlockIdx := Low(UnicodeBlocks) to High(UnicodeBlocks) do
    if UnicodeBlocks[BlockIdx].PG = AName then
      Exit(BlockIdx);

  Result := NOT_SELECTED;
end;

// -----------------------------------------------------------------------------

end.

