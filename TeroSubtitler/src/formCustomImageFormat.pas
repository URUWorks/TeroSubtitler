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

unit formCustomImageFormat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  UWSubtitleAPI.CustomFormat, UWTextBox;

type

  { TfrmCustomImageFormat }

  TfrmCustomImageFormat = class(TForm)
    btnSave: TButton;
    btnClose: TButton;
    btnFolder: TButton;
    cboScript: TComboBox;
    cboFPS: TComboBox;
    cboFont: TComboBox;
    edtFileName: TEdit;
    edtPrefix: TEdit;
    edtFolder: TEdit;
    lblFont: TLabel;
    lblSafeArea: TLabel;
    lblFontSize: TLabel;
    lblX: TLabel;
    lblScript: TLabel;
    lblFPS: TLabel;
    lblFileName: TLabel;
    lblPrefix: TLabel;
    lblFolder: TLabel;
    lblImageSize: TLabel;
    spnFontSize: TSpinEdit;
    spnTop: TSpinEdit;
    spnBottom: TSpinEdit;
    spnWidth: TSpinEdit;
    spnHeight: TSpinEdit;
    spnLeft: TSpinEdit;
    spnRight: TSpinEdit;
    ttbPreview: TUWTextBox;
    procedure btnCloseClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cboFontSelect(Sender: TObject);
    procedure cboScriptSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CustomFormat: TUWSubtitleCustomTextFormat;
  public

  end;

var
  frmCustomImageFormat: TfrmCustomImageFormat;

// -----------------------------------------------------------------------------

implementation

uses
  procWorkspace, procTypes, procCommon, UWSystem.XMLLang, UWSystem.Encoding,
  UWSystem.SysUtils;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmCustomImageFormat }

// -----------------------------------------------------------------------------

procedure TfrmCustomImageFormat.FormCreate(Sender: TObject);
var
  FAppStringList: TAppStringList = NIL;
  i: Integer;
begin
  LoadLanguage(Self);
  CustomFormat := TUWSubtitleCustomTextFormat.Create('');

  FillComboWithFPS(cboFPS, GetFPS);
  FillComboWithCustomFormat(cboScript, '*.cfi');

  cboFont.Items.Assign(Screen.Fonts);
  i := cboFont.Items.IndexOf('Verdana');
  if i >= 0 then
    cboFont.ItemIndex := i
  else
    cboFont.ItemIndex := 0;

  LanguageManager.GetAppStringList('CustomFormatStrings', FAppStringList);
  FAppStringList.Free;

  cboScriptSelect(NIL);

  btnSave.Enabled := cboScript.Items.Count > 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomImageFormat.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CustomFormat.Free;

  CloseAction := caFree;
  frmCustomImageFormat := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomImageFormat.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
  ttbPreview.BackColor := Color;
  cboFontSelect(NIL);
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomImageFormat.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomImageFormat.btnSaveClick(Sender: TObject);
var
  SD : TSaveDialog;
begin
  SD := TSaveDialog.Create(NIL);
  try
    SD.Title  := GetCommonString('SaveFile');
    //SD.Filter := Format('%s (%s)|%s', []);
    SD.FilterIndex := 0;
    SD.FileName := ChangeFileExt(ExtractFileName(SubtitleInfo.Text.FileName), '');
    SD.Options := [ofOverwritePrompt, ofEnableSizing];
    if SD.Execute then
    begin
    end;
  finally
    SD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomImageFormat.cboScriptSelect(Sender: TObject);
var
  FS: TFormatSettings;
begin
  with CustomFormat do
  begin
    LoadFromFile(CustomFormatFolder + cboScript.Items[cboScript.ItemIndex] + '.cfi');
    if Success then
    begin
      FS := AppOptions.FormatSettings;
      FS.DecimalSeparator := DecSeparator;
      cboFPS.Text := SingleToStr(FPS, FS);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomImageFormat.cboFontSelect(Sender: TObject);
begin
  ttbPreview.Font.Name := cboFont.Text;
  ttbPreview.Font.Size := spnFontSize.Value;
  ttbPreview.ReDraw;
end;

// -----------------------------------------------------------------------------

end.

