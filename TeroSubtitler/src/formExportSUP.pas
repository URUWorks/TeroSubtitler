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

unit formExportSUP;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, Menus,
  UWTextBox, UWSubtitleAPI, LCLTranslator, procLocalize;

type

  { TfrmExportSUP }

  TfrmExportSUP = class(TForm)
    btnRes: TButton;
    btnSave: TButton;
    btnClose: TButton;
    cboFont: TComboBox;
    lblStatus: TLabel;
    lblFont: TLabel;
    lblSafeArea: TLabel;
    lblFontSize: TLabel;
    lblX: TLabel;
    lblVideoRes: TLabel;
    popRes: TPopupMenu;
    spnFontSize: TSpinEdit;
    spnTop: TSpinEdit;
    spnBottom: TSpinEdit;
    spnWidth: TSpinEdit;
    spnHeight: TSpinEdit;
    spnLeft: TSpinEdit;
    spnRight: TSpinEdit;
    ttbPreview: TUWTextBox;
    procedure btnCloseClick(Sender: TObject);
    procedure btnResClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cboFontSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    StatusString: String;
    procedure ResItemClick(Sender: TObject);
    procedure SetControlsEnabled(const AValue: Boolean);
    procedure WriteSUPFile(const AFileName: String);
  public

  end;

var
  frmExportSUP: TfrmExportSUP;

// -----------------------------------------------------------------------------

implementation

uses
  procWorkspace, procTypes, UWSystem.Encoding, procCustomFormat, procSUP;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmExportSUP }

// -----------------------------------------------------------------------------

procedure TfrmExportSUP.FormCreate(Sender: TObject);
var
  i: Integer;
  m: TMenuItem;
begin
  cboFont.Items.Assign(Screen.Fonts);
  i := cboFont.Items.IndexOf('Verdana');
  if i >= 0 then
    cboFont.ItemIndex := i
  else
    cboFont.ItemIndex := 0;

  popRes.Items.Clear;
  for i := 0 to High(TResolutionList) do
  begin
    m := TMenuItem.Create(popRes);
    m.Caption := TResolutionList[i].Name;
    m.Tag := i;
    m.OnClick := @ResItemClick;
    popRes.Items.Add(m);
  end;

  StatusString := lngWriteStatus;

  lblStatus.Hide;
end;

// -----------------------------------------------------------------------------

procedure TfrmExportSUP.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmExportSUP := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmExportSUP.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
  ttbPreview.BackColor := Color;
  cboFontSelect(NIL);
end;

// -----------------------------------------------------------------------------

procedure TfrmExportSUP.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmExportSUP.btnResClick(Sender: TObject);
begin
  popRes.PopUp;
end;

// -----------------------------------------------------------------------------

procedure TfrmExportSUP.btnSaveClick(Sender: TObject);
var
  SD : TSaveDialog;
begin
  SD := TSaveDialog.Create(NIL);
  try
    SD.Title  := lngSaveFile;
    SD.Filter := Format('%s (*%s)|*%s', ['Blu-Ray', TBluRayExt, TBluRayExt]);
    SD.FilterIndex := 0;
    SD.FileName := ChangeFileExt(ExtractFileName(SubtitleInfo.Text.FileName), TBluRayExt);
    SD.Options := [ofOverwritePrompt, ofEnableSizing];
    if SD.Execute then
    begin
      SetControlsEnabled(False);
      lblStatus.Show;
      ttbPreview.Hide;

      WriteSUPFile(SD.FileName);

      SetControlsEnabled(True);
      Close;
    end;
  finally
    SD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmExportSUP.cboFontSelect(Sender: TObject);
begin
  ttbPreview.Font.Name := cboFont.Text;
  ttbPreview.Font.Size := spnFontSize.Value;
  ttbPreview.ReDraw;
end;

// -----------------------------------------------------------------------------

procedure TfrmExportSUP.ResItemClick(Sender: TObject);
begin
  with TResolutionList[TMenuItem(Sender).Tag] do
  begin
    spnWidth.Value  := Width;
    spnHeight.Value := Height;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmExportSUP.SetControlsEnabled(const AValue: Boolean);
begin
  spnWidth.Enabled := AValue;
  spnHeight.Enabled := AValue;
  btnRes.Enabled := AValue;
  spnLeft.Enabled := AValue;
  spnTop.Enabled := AValue;
  spnRight.Enabled := AValue;
  spnBottom.Enabled := AValue;
  cboFont.Enabled := AValue;
  spnFontSize.Enabled := AValue;
  btnSave.Enabled := AValue;
  btnClose.Enabled := AValue;

  if not AValue then
    Cursor := crHourGlass
  else
    Cursor := crDefault;

  lblStatus.Cursor := Cursor;
end;

// -----------------------------------------------------------------------------

procedure TfrmExportSUP.WriteSUPFile(const AFileName: String);
var
  fs : TFileStream;
  i, n : Integer;
  buf : TUWBGRAText;
begin
  fs := TFileStream.Create(AFileName, fmCreate or fmOpenReadWrite);
  try
    buf := TUWBGRAText.Create('', 0, 0, ttbPreview.Font, True);
    try
      n := 0;
      for i := 0 to Subtitles.Count-1 do
      begin
        buf.DrawBuffer(Subtitles[i].Text);
        WriteSUPFrame(fs, Subtitles, i, n, buf.Bitmap, spnWidth.Value, spnHeight.Value, Rect(spnLeft.Value, spnTop.Value, spnRight.Value, spnBottom.Value));
        Inc(n, 2);

        lblStatus.Caption := Format(StatusString, [i, Subtitles.Count]);
        Application.ProcessMessages;
      end;
    finally
      buf.Free;
    end;
  finally
    fs.Free;
  end;
end;

// -----------------------------------------------------------------------------


end.

