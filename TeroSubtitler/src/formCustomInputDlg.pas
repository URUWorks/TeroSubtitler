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

unit formCustomInputDlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LCLType, LCLIntf, LCLTranslator;

type

  { TfrmCustomInputDlg }

  TfrmCustomInputDlg = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    lblHelp: TLabel;
    lblMessage: TLabel;
    mmoInput: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblHelpClick(Sender: TObject);
    procedure mmoInputChange(Sender: TObject);
    procedure mmoInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
  private
    FShown: Boolean;
    procedure AdjustInputHeight;
  public
    FURL: String;
    function Execute(const ACaption, APrompt: String; const ADefault: String = ''; const AHeight: Integer = 93; const AOnlyNumbers: Boolean = False): String;
  end;

var
  frmCustomInputDlg: TfrmCustomInputDlg;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, Math;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmCustomInputDlg }

// -----------------------------------------------------------------------------

procedure TfrmCustomInputDlg.FormCreate(Sender: TObject);
begin
  lblMessage.Caption := '';
  FURL := '';
  FShown := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomInputDlg.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
  FShown := True;
  AdjustInputHeight;
end;

// -----------------------------------------------------------------------------

// with several lines (pasted, like a multi-line mpv command) the box shows
// them all (up to 8, then a scroll bar), before only the first one was seen

procedure TfrmCustomInputDlg.AdjustInputHeight;
var
  n, lh, h: Integer;
begin
  if not FShown or mmoInput.NumbersOnly then Exit;

  n := Min(Max(mmoInput.Lines.Count, 1), 8);
  if n < 2 then Exit;

  Canvas.Font.Assign(mmoInput.Font);
  // the line spacing of the memo is a bit more than the text height
  lh := Canvas.TextHeight('Aj') + Scale96ToForm(4);
  h  := (n * lh) + Scale96ToForm(12);
  if h > mmoInput.Height then
    Height := Min(Height + (h - mmoInput.Height), Screen.WorkAreaHeight - 100);
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomInputDlg.mmoInputChange(Sender: TObject);
begin
  AdjustInputHeight;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomInputDlg.lblHelpClick(Sender: TObject);
begin
  OpenURL(FURL);
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomInputDlg.mmoInputKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) then
  begin
    Key := 0;
    btnOk.Click;
  end;
end;

// -----------------------------------------------------------------------------

function TfrmCustomInputDlg.Execute(const ACaption, APrompt: String; const ADefault: String = ''; const AHeight: Integer = 93; const AOnlyNumbers: Boolean = False): String;
begin
  Height               := AHeight;
  Caption              := ACaption;
  lblMessage.Caption   := APrompt;
  mmoInput.Text        := ADefault;
  mmoInput.NumbersOnly := AOnlyNumbers;
  mmoInput.SelectAll;

  if (ShowModal = mrOK) then
    Result := mmoInput.Text
  else
    Result := ADefault;
end;

// -----------------------------------------------------------------------------

end.

