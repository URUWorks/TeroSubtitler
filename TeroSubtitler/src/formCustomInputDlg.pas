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

unit formCustomInputDlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LCLType;

type

  { TfrmCustomInputDlg }

  TfrmCustomInputDlg = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    lblMessage: TLabel;
    mmoInput: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mmoInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
  private

  public
    function Execute(const ACaption, APrompt: String; const ADefault: String = ''; const AHeight: Integer = 93): String;
  end;

var
  frmCustomInputDlg: TfrmCustomInputDlg;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procCommon, procWorkspace, UWSystem.XMLLang;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmCustomInputDlg }

// -----------------------------------------------------------------------------

procedure TfrmCustomInputDlg.FormCreate(Sender: TObject);
begin
  LoadLanguage(Self);
  lblMessage.Caption := '';
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomInputDlg.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
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

function TfrmCustomInputDlg.Execute(const ACaption, APrompt: String; const ADefault: String = ''; const AHeight: Integer = 93): String;
begin
  Height             := AHeight;
  Caption            := ACaption;
  lblMessage.Caption := APrompt;
  mmoInput.Text      := ADefault;
  mmoInput.SelectAll;

  if (ShowModal = mrOK) then
    Result := mmoInput.Text
  else
    Result := ADefault;
end;

// -----------------------------------------------------------------------------

end.

