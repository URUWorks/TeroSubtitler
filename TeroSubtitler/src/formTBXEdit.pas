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

unit formTBXEdit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLIntf, LCLType,
  StdCtrls;

type

  { TfrmTBXEdit }

  TfrmTBXEdit = class(TForm)
    btnClose: TButton;
    btnAdd: TButton;
    edtOriginal: TEdit;
    edtTranslated: TEdit;
    lblOriginal: TLabel;
    lblTranslated: TLabel;
    lblNotes: TLabel;
    mmoNotes: TMemo;
    procedure btnCloseClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmTBXEdit: TfrmTBXEdit;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

uses
  procTypes, RegExpr, procWorkspace, procColorTheme, procCommon,
  Clipbrd, UWSystem.XMLLang, formMain;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmTBXEdit }

// -----------------------------------------------------------------------------

procedure TfrmTBXEdit.FormCreate(Sender: TObject);
begin
  LoadLanguage(Self);
  edtOriginal.Text := frmMain.mmoText.SelText;
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXEdit.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveFormSettings(Self);
  CloseAction := caFree;
  frmTBXEdit := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXEdit.FormShow(Sender: TObject);
begin
  LoadFormSettings(Self);
  CheckColorTheme(Self);

  if edtOriginal.Text <> '' then
    edtTranslated.SetFocus;
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXEdit.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXEdit.btnAddClick(Sender: TObject);
begin
  if edtOriginal.Text = '' then
  begin
    edtOriginal.SetFocus;
    Exit;
  end;

  if edtTranslated.Text = '' then
  begin
    edtTranslated.SetFocus;
    Exit;
  end;

  if TBX.AddItem(edtOriginal.Text, edtTranslated.Text, mmoNotes.Text) > 0 then
  begin
    edtOriginal.Clear;
    edtTranslated.Clear;
    edtOriginal.SetFocus;
  end;
end;

// -----------------------------------------------------------------------------

end.

