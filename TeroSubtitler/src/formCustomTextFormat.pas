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

unit formCustomTextFormat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UWLayout,
  UWSubtitleAPI.CustomFormat, UWSubtitleAPI, LCLTranslator, procLocalize;

type

  { TfrmCustomTextFormat }

  TfrmCustomTextFormat = class(TForm)
    btnSave: TButton;
    btnClose: TButton;
    cboEncoding: TComboBox;
    cboTimeCode: TComboBox;
    cboScript: TComboBox;
    cboFPS: TComboBox;
    edtExtension: TEdit;
    edtTitle: TEdit;
    edtNewLineChar: TEdit;
    edtTimeFormat: TEdit;
    edtSeparatorChar: TEdit;
    lblEncoding: TLabel;
    lblTimeCode: TLabel;
    lblExtension: TLabel;
    lblNewLineChar: TLabel;
    lblTitle: TLabel;
    lblTimeFormat: TLabel;
    lblSeparatorChar: TLabel;
    lblScript: TLabel;
    lblFPS: TLabel;
    mmoSource: TMemo;
    lyoEditor: TUWLayout;
    lyoSettings: TUWLayout;
    procedure btnCloseClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cboScriptSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CustomFormat: TUWSubtitleCustomTextFormat;
  public

  end;

var
  frmCustomTextFormat: TfrmCustomTextFormat;

// -----------------------------------------------------------------------------

implementation

uses
  procWorkspace, procTypes, procConfig, UWSystem.XMLLang, UWSystem.Encoding,
  UWSystem.SysUtils, procCustomFormat;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmCustomTextFormat }

// -----------------------------------------------------------------------------

procedure TfrmCustomTextFormat.FormCreate(Sender: TObject);
begin
  CustomFormat := TUWSubtitleCustomTextFormat.Create('');

  FillComboWithFPS(cboFPS, GetFPS);
  FillComboWithEncodings(cboEncoding);
  FillComboWithCustomFormat(cboScript);

  with cboTimeCode.Items do
  begin
    Add(lngcfsTime);
    Add(lngcfsFrames);
  end;
  cboTimeCode.ItemIndex := 0;

  cboScriptSelect(NIL);

  btnSave.Enabled := cboScript.Items.Count > 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomTextFormat.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CustomFormat.Free;

  CloseAction := caFree;
  frmCustomTextFormat := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomTextFormat.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomTextFormat.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomTextFormat.btnSaveClick(Sender: TObject);
var
  SD  : TSaveDialog;
  TXT : TStringList;
  i   : Integer;
  s   : String;
begin
  SD := TSaveDialog.Create(NIL);
  try
    SD.Title  := lngSaveFile;
    SD.Filter := Format('%s (%s)|%s', [edtTitle.Text, edtExtension.Text, edtExtension.Text]);
    SD.FilterIndex := 0;
    SD.FileName := ChangeFileExt(ExtractFileName(SubtitleInfo.Text.FileName), '');
    SD.Options := [ofOverwritePrompt, ofEnableSizing];
    if SD.Execute then
    begin
      s := SD.FileName;
      CustomFormat.Clear;
      with CustomFormat do
      begin
        Name         := edtTitle.Text;
        Extension    := Copy(edtExtension.Text, 2);
        NewLineChar  := edtNewLineChar.Text;
        TimeFormat   := edtTimeFormat.Text;
        DecSeparator := iff(edtSeparatorChar.Text <> '', edtSeparatorChar.Text[1], ',');
        FPS          := StrToSingle(cboFPS.Text, Workspace.FPS.InputFPS, AppOptions.FormatSettings);
        Time         := cboTimeCode.ItemIndex = 0;
        ReadFormattingFromStrings(mmoSource.Lines);

        TXT := TStringList.Create;
        with TXT do
        try
          BeginUpdate;
          try
            Clear;
            if Header <> '' then
              Add(CFReplaceHeaderTags(Header, TimeFormat, cboFPS.Text, 0, 0));

            for i := 0 to Subtitles.Count-1 do
              Add(CFReplaceBodyTags(Body, TimeFormat, cboFPS.Text, Subtitles[i], not Time, NewLineChar, i+1));

            if Footer <> '' then
              Add(CFReplaceHeaderTags(Footer, TimeFormat, cboFPS.Text, 0, 0));
          finally
            EndUpdate;
          end;
          SaveToFile(CFFixExtension(s, Extension), TEncoding.GetEncoding(Encodings[cboEncoding.ItemIndex].CPID));
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

// -----------------------------------------------------------------------------

procedure TfrmCustomTextFormat.cboScriptSelect(Sender: TObject);
var
  FS: TFormatSettings;
begin
  with CustomFormat do
  begin
    LoadFromFile(CustomFormatFolder + cboScript.Items[cboScript.ItemIndex] + '.cft');
    if Success then
    begin
      FS := AppOptions.FormatSettings;
      FS.DecimalSeparator := DecSeparator;

      edtTitle.Text           := Name;
      edtExtension.Text       := Extension;
      edtNewLineChar.Text     := NewLineChar;
      edtTimeFormat.Text      := TimeFormat;
      edtSeparatorChar.Text   := DecSeparator;
      cboFPS.Text             := SingleToStr(FPS, FS);
      if Time then
        cboTimeCode.ItemIndex := 0
      else
        cboTimeCode.ItemIndex := 1;
      mmoSource.Text          := Text;
    end;
  end;
end;

// -----------------------------------------------------------------------------

end.

