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

unit formImportSUPLanguages;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLTranslator;

type

  { TfrmImportSUPLanguages }

  TfrmImportSUPLanguages = class(TForm)
    btnClose: TButton;
    btnDownload: TButton;
    cboLanguages: TComboBox;
    procedure btnCloseClick(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure SetControlsEnabled(const AValue: Boolean);
  public

  end;

var
  frmImportSUPLanguages: TfrmImportSUPLanguages;

// -----------------------------------------------------------------------------

implementation

uses
  procWorkspace, procConfig, procTypes, formDownload, UWSystem.InetUtils,
  procTesseract;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmImportSUPLanguages }

// -----------------------------------------------------------------------------

procedure TfrmImportSUPLanguages.FormCreate(Sender: TObject);
begin
  FillTStringsWithLanguages(cboLanguages.Items);
  if cboLanguages.Items.Count > 0 then cboLanguages.ItemIndex := 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUPLanguages.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmImportSUPLanguages := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUPLanguages.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUPLanguages.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUPLanguages.btnDownloadClick(Sender: TObject);
const
  URL = 'https://github.com/tesseract-ocr/tessdata/raw/main/';

var
  filename, s: String;
begin
  SetControlsEnabled(False);
  try
    s := CultureNameFromIndex(cboLanguages.ItemIndex)+'.traineddata';
    filename := ConcatPaths([TessDataFolder, s]);
    ShowDownloadDialog(URL + s, filename);
  finally
    SetControlsEnabled(True);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUPLanguages.SetControlsEnabled(const AValue: Boolean);
begin
  cboLanguages.Enabled := AValue;
  btnDownload.Enabled := AValue;
  btnClose.Enabled := AValue;
end;

// -----------------------------------------------------------------------------

end.

