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

unit formDurationLimits;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  UWCheckBox, UWRadioButton;

type

  { TfrmDurationLimits }

  TfrmDurationLimits = class(TForm)
    btnClose: TButton;
    btnApply: TButton;
    chkSetMaxDuration: TUWCheckBox;
    chkSetMinDuration: TUWCheckBox;
    edtSetMaxDuration: TEdit;
    edtSetMinDuration: TEdit;
    lblScope: TLabel;
    rbnAllTheSubtitles: TUWRadioButton;
    rbnFromTheSelectedSubtitle: TUWRadioButton;
    rbnOnlySelectedSubtitles: TUWRadioButton;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure chkSetMaxDurationChange(Sender: TObject);
    procedure chkSetMinDurationChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmDurationLimits: TfrmDurationLimits;

// -----------------------------------------------------------------------------

implementation

uses procTypes, procVST, procSubtitle, UWSubtitleAPI,
  UWSubtitles.Utils, formMain, procWorkspace, procConfig;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmDurationLimits }

// -----------------------------------------------------------------------------

procedure TfrmDurationLimits.FormCreate(Sender: TObject);
begin
  LoadLanguage(Self);

  edtSetMaxDuration.Text := AppOptions.Conventions.MaxDuration.ToString;
  edtSetMinDuration.Text := AppOptions.Conventions.MinDuration.ToString;

  if (frmMain.VST.SelectedCount = 1) then
    rbnFromTheSelectedSubtitle.Checked := True
  else if (frmMain.VST.SelectedCount > 1) then
    rbnOnlySelectedSubtitles.Checked := True
  else
    rbnAllTheSubtitles.Checked := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmDurationLimits.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  AppOptions.Conventions.MaxDuration := StrToIntDef(edtSetMaxDuration.Text, 1);
  AppOptions.Conventions.MinDuration := StrToIntDef(edtSetMinDuration.Text, 1);

  CloseAction := caFree;
  frmDurationLimits := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmDurationLimits.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmDurationLimits.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmDurationLimits.chkSetMaxDurationChange(Sender: TObject);
begin
  edtSetMaxDuration.Enabled := chkSetMaxDuration.Checked;
  btnApply.Enabled := edtSetMaxDuration.Enabled or edtSetMinDuration.Enabled;
end;

// -----------------------------------------------------------------------------

procedure TfrmDurationLimits.chkSetMinDurationChange(Sender: TObject);
begin
  edtSetMinDuration.Enabled := chkSetMinDuration.Checked;
  btnApply.Enabled := edtSetMaxDuration.Enabled or edtSetMinDuration.Enabled;
end;

// -----------------------------------------------------------------------------

procedure ApplyDurationLimits(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^, frmDurationLimits do
    SetSubtitleTime(Index, InitialTime + SetDurationLimits(Subtitles.Duration[Index], StrToInt(edtSetMinDuration.Text), StrToInt(edtSetMaxDuration.Text), chkSetMaxDuration.Checked, chkSetMinDuration.Checked), 1, False, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmDurationLimits.btnApplyClick(Sender: TObject);
var
  SelLoop: TVSTDoLoopSelection;
begin
  if (edtSetMaxDuration.Text <> '') and (edtSetMinDuration.Text <> '') then
  begin
    if rbnAllTheSubtitles.Checked then
      SelLoop := dlAll
    else if rbnFromTheSelectedSubtitle.Checked then
      SelLoop := dlCurrentToLast
    else
      SelLoop := dlSelected;

    VSTDoLoop(frmMain.VST, @ApplyDurationLimits, SelLoop, True, True);
    Close;
  end;
end;

// -----------------------------------------------------------------------------

end.

