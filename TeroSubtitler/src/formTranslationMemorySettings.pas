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

unit formTranslationMemorySettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLTranslator, procLocalize;

type

  { TfrmTranslationMemorySettings }

  TfrmTranslationMemorySettings = class(TForm)
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
  frmTranslationMemorySettings: TfrmTranslationMemorySettings;

// -----------------------------------------------------------------------------

implementation

uses
  procWorkspace, UWSystem.XMLLang, UWSystem.Globalization, procTypes;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmTranslationMemorySettings }

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemorySettings.FormCreate(Sender: TObject);
begin
  FillCultureTStrings(cboSourceLang.Items);
  cboTransLang.Items.Assign(cboSourceLang.Items);

  //TODO: Localize = Maybe nothing to do
  cboSourceLang.ItemIndex := 49;  // eng
  cboTransLang.ItemIndex  := 120; // spa
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemorySettings.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmTranslationMemorySettings := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemorySettings.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemorySettings.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemorySettings.btnOpenFileClick(Sender: TObject);
var
  OD: TOpenDialog;
begin
  OD := TOpenDialog.Create(NIL);
  try
    OD.Title   := lngOpenFile;
    OD.Filter  := lngAllSupportedFiles + '(*.tmx)|*.tmx';
    OD.Options := [ofCreatePrompt, ofOverwritePrompt];

    if OD.Execute then
    begin
      if not FileExists(OD.FileName) and (ExtractFileExt(OD.FileName) = '') then
        OD.FileName := ChangeFileExt(OD.FileName, '.tmx');

      edtOpenFile.Text := OD.FileName;
    end;
  finally
    OD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslationMemorySettings.btnApplyClick(Sender: TObject);
begin
  TMX.Langs^.SrcLang := GetCultureName(cboSourceLang.ItemIndex);
  TMX.Langs^.DstLang := GetCultureName(cboTransLang.ItemIndex);
  TMX.LoadFromFile(edtOpenFile.Text);

  //ShowTranslationMemory;
  Close;
end;

// -----------------------------------------------------------------------------

end.

