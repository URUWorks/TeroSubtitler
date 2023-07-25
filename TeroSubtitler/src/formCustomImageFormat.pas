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
  UWTextBox, UWSubtitleAPI.CustomFormat, UWSubtitleAPI, UWSubtitleAPI.Tags;

type

  { TfrmCustomImageFormat }

  TfrmCustomImageFormat = class(TForm)
    btnSave: TButton;
    btnClose: TButton;
    btnFolder: TButton;
    cboScript: TComboBox;
    cboFPS: TComboBox;
    cboFont: TComboBox;
    edtPrefix: TEdit;
    edtFolder: TEdit;
    lblStatus: TLabel;
    lblFont: TLabel;
    lblSafeArea: TLabel;
    lblFontSize: TLabel;
    lblX: TLabel;
    lblScript: TLabel;
    lblFPS: TLabel;
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
    procedure btnFolderClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cboFontSelect(Sender: TObject);
    procedure cboScriptSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CustomFormat: TUWSubtitleCustomImageFormat;
    StatusString: String;
    procedure SaveImageFile(const AFileName: String; const AIndex: Integer);
  public

  end;

var
  frmCustomImageFormat: TfrmCustomImageFormat;

// -----------------------------------------------------------------------------

implementation

uses
  procWorkspace, procTypes, procCommon, UWSystem.XMLLang, UWSystem.Encoding,
  UWSystem.SysUtils, UWSystem.StrUtils, UWSystem.TimeUtils, procCustomFormat;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmCustomImageFormat }

// -----------------------------------------------------------------------------

procedure TfrmCustomImageFormat.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  LoadLanguage(Self);
  CustomFormat := TUWSubtitleCustomImageFormat.Create('');

  FillComboWithFPS(cboFPS, GetFPS);
  FillComboWithCustomFormat(cboScript, '*.cfi');

  cboFont.Items.Assign(Screen.Fonts);
  i := cboFont.Items.IndexOf('Verdana');
  if i >= 0 then
    cboFont.ItemIndex := i
  else
    cboFont.ItemIndex := 0;

  StatusString := GetCommonString('WriteStatus');

  cboScriptSelect(NIL);

  btnSave.Enabled := cboScript.Items.Count > 0;

  lblStatus.Hide;
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

procedure TfrmCustomImageFormat.btnFolderClick(Sender: TObject);
begin
  with TSelectDirectoryDialog.Create(Self) do
  try
    Options := Options + [ofOldStyleDialog, ofCreatePrompt];

    if Execute then
      edtFolder.Text := FileName;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomImageFormat.btnSaveClick(Sender: TObject);
var
  SD  : TSaveDialog;
  TXT : TStringList;
  i   : Integer;
  img : String;
begin
  if edtPrefix.Text = '' then
    edtPrefix.SetFocus
  else if edtFolder.Text = '' then
    edtFolder.SetFocus
  else
  begin
    SD := TSaveDialog.Create(NIL);
    try
      SD.Title  := GetCommonString('SaveFile');
      with CustomFormat do
        SD.Filter := Format('%s (%s)|%s', [CustomFormat.Name, CustomFormat.Extension, CustomFormat.Extension]);
      SD.FilterIndex := 0;
      SD.FileName := ChangeFileExt(ExtractFileName(SubtitleInfo.Text.FileName), '');
      SD.Options := [ofOverwritePrompt, ofEnableSizing];
      if SD.Execute then
      begin
        with CustomFormat do
        begin
          TXT := TStringList.Create;
          with TXT do
          try
            lblStatus.Show;
            ttbPreview.Hide;
            BeginUpdate;
            try
              if Header <> '' then
                Add(CFReplaceHeaderTags(Header, TimeFormat, cboFPS.Text, spnWidth.Value, spnHeight.Value));

              for i := 0 to Subtitles.Count-1 do
              begin
                lblStatus.Caption := Format(StatusString, [i, Subtitles.Count]);
                Application.ProcessMessages;
                img := Format('%s%.4d.png', [edtPrefix.Text, i+1]);
                Add(CFReplaceBodyTags(Body, TimeFormat, cboFPS.Text, Subtitles[i], not Time, sLineBreak, i+1, img));
                SaveImageFile(ConcatPaths([edtFolder.Text, img]), i);
              end;

              if Footer <> '' then
                Add(CFReplaceHeaderTags(Footer, TimeFormat, cboFPS.Text, spnWidth.Value, spnHeight.Value));
            finally
              EndUpdate;
            end;
            SaveToFile(ChangeFileExt(SD.FileName, Extension));
          finally
            Free;
          end;
        end;
        Close;
      end;
    finally
      SD.Free;
    end;
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


procedure TfrmCustomImageFormat.SaveImageFile(const AFileName: String; const AIndex: Integer);
begin
  ttbPreview.SaveImageToFile(AFileName, RemoveTSTags(Subtitles[AIndex].Text), spnWidth.Value, spnHeight.Value, True);
end;

// -----------------------------------------------------------------------------

end.

