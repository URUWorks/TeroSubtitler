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
  Spin, UWRadioButton;

type

  { TfrmRoundTime }

  TfrmRoundTime = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    lblMilliseconds: TLabel;
    lblScope: TLabel;
    rbnAllTheSubtitles: TUWRadioButton;
    rbnFromTheSelectedSubtitle: TUWRadioButton;
    rbnOnlySelectedSubtitles: TUWRadioButton;
    spnValue: TSpinEdit;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmRoundTime: TfrmRoundTime;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

uses procTypes, procVST, procSubtitle, procWorkspace, UWSubtitleAPI,
  UWSubtitles.Utils, procConfig, formMain, LCLTranslator;

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

  {$IFNDEF WINDOWS}
  PrepareCustomControls(Self);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TfrmRoundTime.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveFormSettings(Self, spnValue.Value.ToString);

  CloseAction := caFree;
  frmRoundTime := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmRoundTime.FormShow(Sender: TObject);
begin
  spnValue.Value := StrToIntDef(LoadFormSettings(Self), 500);
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmRoundTime.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure ApplyRoundTime(const Item: PUWSubtitleItem; const Index: Integer);
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
  if spnValue.Value > 0 then
  begin
    if rbnAllTheSubtitles.Checked then
      SelLoop := dlAll
    else if rbnFromTheSelectedSubtitle.Checked then
      SelLoop := dlCurrentToLast
    else
      SelLoop := dlSelected;

    VSTDoLoop(frmMain.VST, @ApplyRoundTime, SelLoop, True, True);
  end;
  Close;
end;

// -----------------------------------------------------------------------------

end.

