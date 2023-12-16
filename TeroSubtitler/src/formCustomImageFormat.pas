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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, Menus,
  UWTextBox, UWSubtitleAPI.CustomFormat, UWSubtitleAPI, LCLTranslator, procLocalize;

type

  { TfrmCustomImageFormat }

  TfrmCustomImageFormat = class(TForm)
    btnRes: TButton;
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
    procedure btnFolderClick(Sender: TObject);
    procedure btnResClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cboFontSelect(Sender: TObject);
    procedure cboScriptSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CustomFormat: TUWSubtitleCustomImageFormat;
    StatusString: String;
    procedure ResItemClick(Sender: TObject);
    procedure SetControlsEnabled(const AValue: Boolean);
    procedure SaveImageFile(const AFileName: String; const AIndex: Integer);
  public

  end;

var
  frmCustomImageFormat: TfrmCustomImageFormat;

// -----------------------------------------------------------------------------

implementation

uses
  procWorkspace, procTypes, procConfig, UWSystem.XMLLang, UWSystem.Encoding,
  UWSystem.SysUtils, procCustomFormat;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmCustomImageFormat }

// -----------------------------------------------------------------------------

procedure TfrmCustomImageFormat.FormCreate(Sender: TObject);
var
  i: Integer;
  m: TMenuItem;
begin
  CustomFormat := TUWSubtitleCustomImageFormat.Create('');

  FillComboWithFPS(cboFPS, GetFPS);
  FillComboWithCustomFormat(cboScript, '*.cfi');

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

procedure TfrmCustomImageFormat.btnResClick(Sender: TObject);
begin
  popRes.PopUp;
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
      SD.Title  := lngSaveFile;
      SD.Filter := Format('%s (%s)|%s', [CustomFormat.Name, CustomFormat.Extension, CustomFormat.Extension]);
      SD.FilterIndex := 0;
      SD.FileName := ChangeFileExt(ExtractFileName(SubtitleInfo.Text.FileName), '');
      SD.Options := [ofOverwritePrompt, ofEnableSizing];
      if SD.Execute then
      begin
        SetControlsEnabled(False);
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
                Add(CFReplaceBodyTags(Body, TimeFormat, cboFPS.Text, Subtitles[i], not Time, sLineBreak, i+1, img, spnWidth.Value, spnHeight.Value));
                SaveImageFile(ConcatPaths([edtFolder.Text, img]), i);
              end;

              if Footer <> '' then
                Add(CFReplaceHeaderTags(Footer, TimeFormat, cboFPS.Text, spnWidth.Value, spnHeight.Value));
            finally
              EndUpdate;
            end;
            SaveToFile(CFFixExtension(SD.FileName, Extension));
          finally
            Free;
          end;
        end;
        SetControlsEnabled(True);
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
      spnWidth.Value := Width;
      spnHeight.Value := Height;
      spnLeft.Value := Margin.Left;
      spnTop.Value := Margin.Top;
      spnRight.Value := Margin.Right;
      spnBottom.Value := Margin.Bottom;
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

procedure TfrmCustomImageFormat.ResItemClick(Sender: TObject);
begin
  with TResolutionList[TMenuItem(Sender).Tag] do
  begin
    spnWidth.Value  := Width;
    spnHeight.Value := Height;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomImageFormat.SetControlsEnabled(const AValue: Boolean);
begin
  cboScript.Enabled := AValue;
  cboFPS.Enabled := AValue;
  edtPrefix.Enabled := AValue;
  edtFolder.Enabled := AValue;
  btnFolder.Enabled := AValue;
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

procedure TfrmCustomImageFormat.SaveImageFile(const AFileName: String; const AIndex: Integer);
var
  h : TAlignment;
  v : TTextLayout;
  buf : TUWBGRAText;
begin
  case Subtitles[AIndex].Align of
    shaLeft  : h := taLeftJustify;
    shaRight : h := taRightJustify;
  else
    h := taCenter;
  end;

  case Subtitles[AIndex].VAlign of
    svaCenter : v := tlCenter;
    svaTop    : v := tlTop;
  else
    v := tlBottom;
  end;

  buf := TUWBGRAText.Create(Subtitles[AIndex].Text, ttbPreview.Width, ttbPreview.Height, ttbPreview.Font, True);
  try
    buf.DrawBuffer(Subtitles[AIndex].Text, spnWidth.Value, spnHeight.Value, h, v, spnLeft.Value, spnTop.Value, spnRight.Value, spnBottom.Value);
    buf.Bitmap.SaveToFile(AFileName);
  finally
    buf.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.

