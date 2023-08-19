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

unit formModifySelection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  UWRadioButton;

type

  { TfrmModifySelection }

  TfrmModifySelection = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    cboRule: TComboBox;
    edtText: TEdit;
    rbnNewSelection: TUWRadioButton;
    rbnAddToCurrentSelection: TUWRadioButton;
    rbnSubtractFromCurrentSelection: TUWRadioButton;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmModifySelection: TfrmModifySelection;

// -----------------------------------------------------------------------------

implementation

uses
  formMain, UWSubtitleAPI, procVST, procTypes, procWorkspace, procConfig,
  UWSystem.XMLLang;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmModifySelection }

// -----------------------------------------------------------------------------

procedure TfrmModifySelection.FormCreate(Sender: TObject);
var
  FAppStringList: TAppStringList = NIL;
begin
  LoadLanguage(Self);

  LanguageManager.GetAppStringList('ModifySelectionStrings', FAppStringList);
  cboRule.Items.Add(GetString(FAppStringList, 'Contains'));
  cboRule.Items.Add(GetString(FAppStringList, 'StartsWith'));
  cboRule.Items.Add(GetString(FAppStringList, 'EndsWith'));
  FAppStringList.Free;

  cboRule.ItemIndex := 0;

  if frmMain.VST.SelectedCount > 0 then
    rbnAddToCurrentSelection.Checked := True
  else
    rbnNewSelection.Checked := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmModifySelection.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmModifySelection := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmModifySelection.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmModifySelection.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure ApplyModifySelection(const Item: PUWSubtitleItem; const Index: Integer);
var
  found: Boolean;
begin
  found := False;

  with frmModifySelection, Item^ do
  begin
    case cboRule.ItemIndex of
      0: if Text.Contains(edtText.Text)   then found := True;
      1: if Text.StartsWith(edtText.Text) then found := True;
      2: if Text.EndsWith(edtText.Text)   then found := True;
    end;

    if found then
      frmMain.VST.Selected[VSTGetNodeAtIndex(frmMain.VST, Index)] := (rbnSubtractFromCurrentSelection.Checked <> True);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmModifySelection.btnApplyClick(Sender: TObject);
begin
  with frmMain do
    if (edtText.Text <> '') and (VST.RootNodeCount > 0) then
    begin
      if rbnNewSelection.Checked then VST.ClearSelection;
      VSTDoLoop(VST, @ApplyModifySelection, dlAll, True, True);
      Self.Close;
    end;
end;

// -----------------------------------------------------------------------------

end.

