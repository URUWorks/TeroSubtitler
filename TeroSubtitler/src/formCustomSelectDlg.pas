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

unit formCustomSelectDlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LCLType, LCLIntf, LCLTranslator;

type

  { TfrmCustomSelectDlg }

  TfrmCustomSelectDlg = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    cboSelect: TComboBox;
    lblMessage: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

function ExecuteDialog(const ACaption, APrompt: String; const AItems: TStrings; const ADefault: Integer = 0; const AOnlyOkButton: Boolean = False): Integer;

var
  frmCustomSelectDlg: TfrmCustomSelectDlg;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmCustomSelectDlg }

// -----------------------------------------------------------------------------

procedure TfrmCustomSelectDlg.FormCreate(Sender: TObject);
begin
  lblMessage.Caption := '';
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomSelectDlg.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

function ExecuteDialog(const ACaption, APrompt: String; const AItems: TStrings; const ADefault: Integer = 0; const AOnlyOkButton: Boolean = False): Integer;
begin
  with TfrmCustomSelectDlg.Create(NIL) do
  try
    Caption := ACaption;
    lblMessage.Caption := APrompt;
    cboSelect.Items.Assign(AItems);
    cboSelect.ItemIndex := ADefault;

    if AOnlyOkButton then
    begin
      btnCancel.Hide;
      btnOk.Left := btnCancel.Left
    end;

    if ShowModal = mrOK then
      Result := cboSelect.ItemIndex
    else
      Result := ADefault;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

end.

