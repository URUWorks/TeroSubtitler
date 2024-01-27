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

unit formGenerateBlankVideo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, Menus,
  ComCtrls, UWTimeEdit, UWRadioButton, UWCheckBox, procLocalize;

type

  { TfrmGenerateBlankVideo }

  TfrmGenerateBlankVideo = class(TForm)
    btnClose: TButton;
    btnRes: TButton;
    btnImage: TButton;
    btnGenerate: TButton;
    cbnSolidColor: TColorButton;
    cboFPS: TComboBox;
    chkGenerateTone: TUWCheckBox;
    lblDuration: TLabel;
    lblFPS: TLabel;
    lblTimeElapsed: TLabel;
    lblVideoRes: TLabel;
    lblBackground: TLabel;
    lblX: TLabel;
    prbProgress: TProgressBar;
    rdoImage: TUWRadioButton;
    popRes: TPopupMenu;
    rdoSMPTEColorBars: TUWRadioButton;
    spnHeight: TSpinEdit;
    spnWidth: TSpinEdit;
    tedDuration: TUWTimeEdit;
    rdoSolidColor: TUWRadioButton;
    chkGenerateTC: TUWCheckBox;
    procedure btnCloseClick(Sender: TObject);
    procedure btnImageClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnResClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FBackImage: String;
    procedure ResItemClick(Sender: TObject);
    procedure SetControlsEnabled(const AValue: Boolean);
  public

  end;

var
  frmGenerateBlankVideo: TfrmGenerateBlankVideo;
  CancelGeneration: Boolean;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procColorTheme, procConfig, procGenerateVideo,
  procDialogs, procCustomFormat, UWSystem.TimeUtils;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmGenerateBlankVideo }

// -----------------------------------------------------------------------------

procedure TfrmGenerateBlankVideo.FormCreate(Sender: TObject);
var
  i: Integer;
  m: TMenuItem;
begin
  tedDuration.FPS := GetFPS;
  tedDuration.TimeMode := GetTimeEditMode;

  FillComboWithFPS(cboFPS, GetFPS);

  popRes.Items.Clear;
  for i := 0 to High(TResolutionList) do
  begin
    m := TMenuItem.Create(popRes);
    m.Caption := TResolutionList[i].Name;
    m.Tag := i;
    m.OnClick := @ResItemClick;
    popRes.Items.Add(m);
  end;

  rdoSMPTEColorBars.Checked := True;
  FBackImage := '';

  CancelGeneration := False;

  {$IFNDEF WINDOWS}
  PrepareCustomControls(Self);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateBlankVideo.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveFormSettings(Self, Format('%d,%d,%d', [chkGenerateTC.Checked.ToInteger, chkGenerateTone.Checked.ToInteger, cbnSolidColor.ButtonColor]));

  CloseAction := caFree;
  frmGenerateBlankVideo := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateBlankVideo.FormShow(Sender: TObject);
var
  s: String;
  AParamArray: TStringArray;
begin
  CheckColorTheme(Self);
  s := LoadFormSettings(Self);
  if not s.IsEmpty then
  begin
    AParamArray := s.Split(',');
    if Length(AParamArray) = 3 then
    begin
      chkGenerateTC.Checked := AParamArray[0].ToBoolean;
      chkGenerateTone.Checked := AParamArray[1].ToBoolean;
      cbnSolidColor.ButtonColor := AParamArray[2].ToInteger;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateBlankVideo.btnCloseClick(Sender: TObject);
begin
  if btnGenerate.Enabled then
    Close
  else
    CancelGeneration := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateBlankVideo.btnImageClick(Sender: TObject);
var
  OD : TOpenDialog;
  i : Integer;
  s : String = '';
begin
  OD := TOpenDialog.Create(Self);
  try
    OD.Title  := lngOpenFile;
    for i := 0 to Length(TImageExts)-1 do
      s += '*' + TImageExts[i] + ';';

    OD.Filter := lngAllSupportedFiles + ' (' + s + ')|' + s;
    OD.FilterIndex := 0;
    OD.FileName := '';
    if OD.Execute then
      FBackImage := OD.FileName;
  finally
    OD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure ProcessCB(Output: String; var ATerminate: Boolean);
var
  x, time : Integer;
begin
  //WriteLn(output);
  ATerminate := CancelGeneration;
  Application.ProcessMessages;
  // process output received
  x := Pos('time=', Output);
  if x > 0 then
  begin
    time := StringToTime(Copy(Output, x+5, 8));
    if (time >= 0) and (frmGenerateBlankVideo.tedDuration.Value > 0) then
    begin
      x := Round((time / frmGenerateBlankVideo.tedDuration.Value) * 100);
      frmGenerateBlankVideo.prbProgress.Position := x;
      frmGenerateBlankVideo.lblTimeElapsed.Caption := x.ToString + '%';
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateBlankVideo.btnGenerateClick(Sender: TObject);
var
  SD : TSaveDialog;
begin
  CancelGeneration := False;
  prbProgress.Position := 0;
  lblTimeElapsed.Caption := '';

  if tedDuration.Value = 0 then
  begin
    tedDuration.SetFocus;
    Exit;
  end
  else if rdoImage.Checked and FBackImage.IsEmpty then
  begin
    btnImage.SetFocus;
    Exit;
  end;

  SD := TSaveDialog.Create(NIL);
  try
    SD.Title  := lngSaveFile;
    SD.Filter := lngAllSupportedFiles + ' (*.mp4)|*.mp4';
    SD.FilterIndex := 0;
    SD.Options := [ofOverwritePrompt, ofEnableSizing];
    if SD.Execute then
    begin
      SetControlsEnabled(False);

      SD.FileName := ChangeFileExt(SD.FileName, '.mp4');

      if GenerateBlankVideo(SD.FileName, spnWidth.Value, spnHeight.Value,
         DefFPSList[cboFPS.ItemIndex], tedDuration.Value,
         cbnSolidColor.ButtonColor, FBackImage,
         rdoSMPTEColorBars.Checked, rdoSolidColor.Checked, rdoImage.Checked,
         chkGenerateTC.Checked, chkGenerateTone.Checked,
         @ProcessCB) then
          ShowMessageDialog(lngFileSavedSuccessfully)
        else
          ShowErrorMessageDialog(lngVideoGenerationFailed);

      SetControlsEnabled(True);
      Close;
    end;
  finally
    SD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateBlankVideo.btnResClick(Sender: TObject);
begin
  popRes.PopUp;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateBlankVideo.ResItemClick(Sender: TObject);
begin
  with TResolutionList[TMenuItem(Sender).Tag] do
  begin
    spnWidth.Value  := Width;
    spnHeight.Value := Height;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateBlankVideo.SetControlsEnabled(const AValue: Boolean);
begin
  lblDuration.Enabled := AValue;
  lblFPS.Enabled := AValue;
  lblVideoRes.Enabled := AValue;
  lblBackground.Enabled := AValue;
  lblTimeElapsed.Caption := '';
  lblTimeElapsed.Visible := not AValue;
  prbProgress.Visible := not AValue;
  tedDuration.Enabled := AValue;
  cboFPS.Enabled := AValue;
  spnWidth.Enabled := AValue;
  spnHeight.Enabled := AValue;
  btnRes.Enabled := AValue;
  rdoSMPTEColorBars.Enabled := AValue;
  rdoSolidColor.Enabled := AValue;
  cbnSolidColor.Enabled := AValue;
  rdoImage.Enabled := AValue;
  btnImage.Enabled := AValue;
  chkGenerateTC.Enabled := AValue;
  chkGenerateTone.Enabled := AValue;
  btnGenerate.Enabled := AValue;

  if AValue then
    btnClose.Caption := lngbtnClose
  else
    btnClose.Caption := lngbtnCancel;

  if not AValue then
    Cursor := crHourGlass
  else
    Cursor := crDefault;
end;

// -----------------------------------------------------------------------------

end.

