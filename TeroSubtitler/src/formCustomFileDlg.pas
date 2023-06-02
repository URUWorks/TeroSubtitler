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

unit formCustomFileDlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UWSubtitleAPI.Formats;

type

  { TfrmCustomFileDlg }

  TfrmCustomFileDlgMode = (dmOpen, dmSave);

  TfrmCustomFileDlg = class(TForm)
    btnApply: TButton;
    btnCancel: TButton;
    btnOpenFile: TButton;
    cboFormat: TComboBox;
    cboEncoding: TComboBox;
    cboFPS: TComboBox;
    edtFileName: TEdit;
    lblEncoding: TLabel;
    lblFormat: TLabel;
    lblFileName: TLabel;
    lblFPS: TLabel;
    procedure btnOpenFileClick(Sender: TObject);
    procedure cboFormatSelect(Sender: TObject);
    procedure edtFileNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDlgMode : TfrmCustomFileDlgMode;
  public
    FileName : String;
    Format   : TUWSubtitleFormats;
    Encoding : TEncoding;
    FPS      : Integer;
    function Execute(const ADlgMode: TfrmCustomFileDlgMode = dmOpen): Boolean; // our showmodal function
  end;

var
  frmCustomFileDlg: TfrmCustomFileDlg;

// -----------------------------------------------------------------------------

implementation

uses
  procWorkspace, procTypes, procCommon, UWSystem.XMLLang, UWSystem.Encoding;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmCustomFileDlg }

// -----------------------------------------------------------------------------

procedure TfrmCustomFileDlg.FormCreate(Sender: TObject);
begin
  LoadLanguage(Self);

  FileName := '';
  Format   := sfInvalid;
  Encoding := NIL;
  FPS      := -1;

  FillComboWithFPS(cboFPS);
  FillComboWithEncodings(cboEncoding);
  FillComboWithFormats(cboFormat);
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomFileDlg.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

function TfrmCustomFileDlg.Execute(const ADlgMode: TfrmCustomFileDlgMode = dmOpen): Boolean;
begin
  FDlgMode := ADlgMode;
  edtFileName.Text := FileName;

  if (ADlgMode = dmOpen) then
  begin
    Caption := GetCommonString('OpenFile');
    btnApply.Caption := GetCommonString('btnLoad', 'CommonControls');
    cboEncoding.Items.Insert(0, GetCommonString('Detect'));
    cboFormat.Items.Insert(0, cboEncoding.Items[0]);
    cboEncoding.ItemIndex := 0;
    cboFormat.ItemIndex   := 0;
  end
  else
  begin
    Caption := GetCommonString('SaveFile');
    btnApply.Caption := GetCommonString('btnSave', 'CommonControls');
    cboFormat.ItemIndex   := Integer(Workspace.DefFormat)-1; //frmMain.cboFormat.ItemIndex;
    cboEncoding.ItemIndex := Workspace.DefEncoding; //frmMain.cboEncoding.ItemIndex;
  end;

  if (ShowModal = mrOK) and (edtFileName.Text <> '') then
  begin
    FileName := edtFileName.Text;

    if (ADlgMode = dmOpen) then
    begin
      Format := TUWSubtitleFormats(cboFormat.ItemIndex);

      if cboEncoding.ItemIndex = 0 then
        Encoding := NIL
      else
        Encoding := TEncoding.GetEncoding(Encodings[cboEncoding.ItemIndex-1].CPID);
    end
    else
    begin
      Format   := TUWSubtitleFormats(cboFormat.ItemIndex+1);
      Encoding := TEncoding.GetEncoding(Encodings[cboEncoding.ItemIndex].CPID);
    end;

    FPS    := cboFPS.ItemIndex;
    Result := True;
  end
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomFileDlg.btnOpenFileClick(Sender: TObject);
var
  OD : TOpenDialog;
  SD : TSaveDialog;
begin
  if (FDlgMode = dmOpen) then
  begin
    OD := TOpenDialog.Create(Self);
    try
      OD.Title  := GetCommonString('OpenFile');
      OD.Filter := Subtitles.FillDialogFilter(GetCommonString('AllSupportedFiles'));
      OD.FilterIndex := cboFormat.ItemIndex+1;
      OD.FileName := ExtractFileName(edtFileName.Text);
      if OD.Execute then
        edtFileName.Text := OD.FileName;
    finally
      OD.Free;
    end;
  end
  else
  begin
    SD := TSaveDialog.Create(Self);
    try
      SD.Title  := GetCommonString('SaveFile');
      SD.Filter := Subtitles.FillDialogFilter('');
      SD.FilterIndex := cboFormat.ItemIndex+1;
      SD.FileName := ExtractFileName(edtFileName.Text);
      SD.Options := [ofOverwritePrompt, ofEnableSizing];
      if SD.Execute then
        edtFileName.Text := SD.FileName;
    finally
      SD.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomFileDlg.cboFormatSelect(Sender: TObject);
begin
  if (FDlgMode = dmSave) and (edtFileName.Text <> '') then
    edtFileName.Text := Subtitles.FixFileNameExtension(edtFileName.Text, TUWSubtitleFormats(cboFormat.ItemIndex+1));
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomFileDlg.edtFileNameChange(Sender: TObject);
begin
  btnApply.Enabled := edtFileName.Text <> '';
end;

// -----------------------------------------------------------------------------

end.

