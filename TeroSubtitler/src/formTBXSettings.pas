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

unit formTBXSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLTranslator, procLocalize ;

type

  { TfrmTBXSettings }

  TfrmTBXSettings = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    btnOpenFile: TButton;
    cboSourceLang: TComboBox;
    cboTransLang: TComboBox;
    edtOpenFile: TEdit;
    lblSourceLang: TLabel;
    lblFileName: TLabel;
    lblTransLang: TLabel;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmTBXSettings: TfrmTBXSettings;

// -----------------------------------------------------------------------------

implementation

uses
  procWorkspace, UWSystem.XMLLang, UWSystem.Globalization, procTypes;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmTBXSettings }

// -----------------------------------------------------------------------------

procedure TfrmTBXSettings.FormCreate(Sender: TObject);
begin
  FillCultureTStrings(cboSourceLang.Items);
  cboTransLang.Items.Assign(cboSourceLang.Items);

  //TODO: Localize = Maybe nothing to do
  cboSourceLang.ItemIndex := 49;  // eng
  cboTransLang.ItemIndex  := 120; // spa
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXSettings.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmTBXSettings := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXSettings.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXSettings.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXSettings.btnOpenFileClick(Sender: TObject);
var
  OD: TOpenDialog;
begin
  OD := TOpenDialog.Create(NIL);
  try
    OD.Title   := lngOpenFile;
    OD.Filter  := lngAllSupportedFiles + '(*.tbx)|*.tbx';
    OD.Options := [ofCreatePrompt, ofOverwritePrompt];

    if OD.Execute then
    begin
      if not FileExists(OD.FileName) and (ExtractFileExt(OD.FileName) = '') then
        OD.FileName := ChangeFileExt(OD.FileName, '.tbx');

      edtOpenFile.Text := OD.FileName;
    end;
  finally
    OD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTBXSettings.btnApplyClick(Sender: TObject);
begin
  TBX.Header^.lang   := GetCultureName(cboSourceLang.ItemIndex);
  TBX.Langs^.SrcLang := GetCultureShortName(cboSourceLang.ItemIndex);
  TBX.Langs^.DstLang := GetCultureShortName(cboTransLang.ItemIndex);
  TBX.LoadFromFile(edtOpenFile.Text);
  Close;
end;

// -----------------------------------------------------------------------------

end.

