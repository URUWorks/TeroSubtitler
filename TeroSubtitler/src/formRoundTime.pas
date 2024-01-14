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

unit formRoundTime;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, UWRadioButton, UWLayout;

type

  { TfrmRoundTime }

  TfrmRoundTime = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    cboFPS: TComboBox;
    lblScope: TLabel;
    rbnAllTheSubtitles: TUWRadioButton;
    rbnFromTheSelectedSubtitle: TUWRadioButton;
    rbnOnlySelectedSubtitles: TUWRadioButton;
    spnValue: TSpinEdit;
    lyoModes: TUWLayout;
    rdoFPS: TUWRadioButton;
    rdoMilliseconds: TUWRadioButton;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rdoFPSChange(Sender: TObject);
  private

  public

  end;

var
  frmRoundTime: TfrmRoundTime;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

uses procTypes, procVST, procSubtitle, procWorkspace, UWSubtitleAPI,
  UWSubtitles.Utils, procConfig, formMain, LCLTranslator, procVST_Loops;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmRoundTime }

// -----------------------------------------------------------------------------

procedure TfrmRoundTime.FormCreate(Sender: TObject);
begin
  if (frmMain.VST.SelectedCount = 1) then
    rbnFromTheSelectedSubtitle.Checked := True
  else if (frmMain.VST.SelectedCount > 1) then
    rbnOnlySelectedSubtitles.Checked := True
  else
    rbnAllTheSubtitles.Checked := True;

  FillComboWithFPS(cboFPS, Workspace.FPS.InputFPS);

  {$IFNDEF WINDOWS}
  PrepareCustomControls(Self);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TfrmRoundTime.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveFormSettings(Self, Format('%d,%d,%d', [rdoFPS.Checked.ToInteger, cboFPS.ItemIndex, spnValue.Value]));

  CloseAction := caFree;
  frmRoundTime := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmRoundTime.FormShow(Sender: TObject);
var
  s: String;
  b: Boolean;
  AParamArray: TStringArray;
begin
  CheckColorTheme(Self);

  s := LoadFormSettings(Self);
  if not s.IsEmpty then
  begin
    AParamArray := s.Split(',');
    if Length(AParamArray) = 3 then
    begin
      b := AParamArray[0].ToBoolean;
      if b then
        rdoFPS.Checked := True
      else
        rdoMilliseconds.Checked := True;

      cboFPS.ItemIndex := AParamArray[1].ToInteger;
      spnValue.Value := AParamArray[2].ToInteger;
    end;
  end;
  rdoFPSChange(NIL);
end;

// -----------------------------------------------------------------------------

procedure TfrmRoundTime.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmRoundTime.rdoFPSChange(Sender: TObject);
begin
  cboFPS.Enabled := rdoFPS.Checked;
  spnValue.Enabled := rdoMilliseconds.Checked;
end;

// -----------------------------------------------------------------------------

procedure ApplyRoundTimeMs(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^, frmRoundTime do
    SetSubtitleTimes(Index, RoundTimeValue(InitialTime, spnValue.Value),
      RoundTimeValue(FinalTime, spnValue.Value), False, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmRoundTime.btnApplyClick(Sender: TObject);
var
  SelLoop: TVSTDoLoopSelection;
begin
  if rbnAllTheSubtitles.Checked then
    SelLoop := dlAll
  else if rbnFromTheSelectedSubtitle.Checked then
    SelLoop := dlCurrentToLast
  else
    SelLoop := dlSelected;

  if rdoFPS.Checked then
    VSTDoLoop(frmMain.VST, @ApplyRoundTimesFPS, SelLoop, True, True)
  else if spnValue.Value > 0 then
    VSTDoLoop(frmMain.VST, @ApplyRoundTimeMs, SelLoop, True, True);

  Close;
end;

// -----------------------------------------------------------------------------

end.

