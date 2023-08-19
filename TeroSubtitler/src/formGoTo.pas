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

unit formGoTo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmGoTo }

  TfrmGoTo = class(TForm)
    btnClose: TButton;
    btnGo: TButton;
    edtNumber: TEdit;
    lblNumber: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure edtNumberChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmGoTo: TfrmGoTo;

// -----------------------------------------------------------------------------

implementation

uses
  procVST, formMain, procWorkspace, procConfig;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmGoTo }

// -----------------------------------------------------------------------------

procedure TfrmGoTo.FormCreate(Sender: TObject);
begin
  LoadLanguage(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmGoTo.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmGoTo := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmGoTo.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmGoTo.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmGoTo.btnGoClick(Sender: TObject);
begin
  VSTSelectNode(frmMain.VST, StrToInt(edtNumber.Text)-1, True);
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmGoTo.edtNumberChange(Sender: TObject);
begin
  btnGo.Enabled := edtNumber.Text <> '';
end;

// -----------------------------------------------------------------------------

end.

