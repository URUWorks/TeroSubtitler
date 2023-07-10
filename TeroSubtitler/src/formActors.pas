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

unit formActors;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LCLType;

type

  { TfrmActors }

  TfrmActors = class(TForm)
    btnActorAdd: TButton;
    btnActorRemove: TButton;
    btnApply: TButton;
    btnClose: TButton;
    edtActor: TEdit;
    lstActors: TListBox;
    procedure btnActorAddClick(Sender: TObject);
    procedure btnActorRemoveClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure edtActorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmActors: TfrmActors;

// -----------------------------------------------------------------------------

implementation

uses
  procWorkspace, procCommon, procColorTheme, formMain;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmActors }

// -----------------------------------------------------------------------------

procedure TfrmActors.FormCreate(Sender: TObject);
begin
  LoadLanguage(Self);
  lstActors.Items.Assign(frmMain.cboActor.Items);
end;

// -----------------------------------------------------------------------------

procedure TfrmActors.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmActors := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmActors.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmActors.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmActors.btnApplyClick(Sender: TObject);
begin
  frmMain.cboActor.Items.Assign(lstActors.Items);
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmActors.btnActorAddClick(Sender: TObject);
begin
  if (edtActor.Text <> '') and (lstActors.Items.IndexOf(edtActor.Text) < 0) then
  begin
    lstActors.Items.Add(edtActor.Text);
    edtActor.Text := '';
    edtActor.SetFocus;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmActors.btnActorRemoveClick(Sender: TObject);
begin
  if lstActors.ItemIndex >= 0 then
    lstActors.Items.Delete(lstActors.ItemIndex);
end;

// -----------------------------------------------------------------------------

procedure TfrmActors.edtActorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    btnActorAdd.Click;
    Key := 0;
  end;
end;

// -----------------------------------------------------------------------------

end.

