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

unit formPauses;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  UWRadioButton, UWLayout, LCLTranslator, UWTimeEdit;

type

  { TfrmPauses }

  TfrmPauses = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    lblGap: TLabel;
    lblDuration: TLabel;
    lblScope: TLabel;
    rbnAllTheSubtitles: TUWRadioButton;
    rbnFromTheSelectedSubtitle: TUWRadioButton;
    rbnOnlySelectedSubtitles: TUWRadioButton;
    rdoSecondEntry: TUWRadioButton;
    rdoFirstEntry: TUWRadioButton;
    rdoEntryWithLessCPS: TUWRadioButton;
    rdoBothEntries: TUWRadioButton;
    lyoScope: TUWLayout;
    tedGap: TUWTimeEdit;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmPauses: TfrmPauses;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procVST, formMain, LazUTF8, procWorkspace, procSubtitle;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmPauses }

// -----------------------------------------------------------------------------

procedure TfrmPauses.FormCreate(Sender: TObject);
begin
  if (frmMain.VST.SelectedCount = 1) then
    rbnFromTheSelectedSubtitle.Checked := True
  else if (frmMain.VST.SelectedCount > 1) then
    rbnOnlySelectedSubtitles.Checked := True
  else
    rbnAllTheSubtitles.Checked := True;

  tedGap.FPS      := GetFPS;
  tedGap.TimeMode := GetTimeEditMode;
  tedGap.Value    := GetCorrectTime(AppOptions.Conventions.MinPause, AppOptions.Conventions.PauseInFrames);

  {$IFNDEF WINDOWS}
  PrepareCustomControls(Self);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TfrmPauses.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmPauses := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmPauses.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmPauses.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmPauses.btnApplyClick(Sender: TObject);
var
  SelLoop: TVSTDoLoopSelection;
  GapMode: TVSTSetPausesMode;
begin
  if rbnAllTheSubtitles.Checked then
    SelLoop := dlAll
  else if rbnFromTheSelectedSubtitle.Checked then
    SelLoop := dlCurrentToLast
  else
    SelLoop := dlSelected;

  if rdoFirstEntry.Checked then
    GapMode := spmFirst
  else if rdoSecondEntry.Checked then
    GapMode := spmSecond
  else if rdoBothEntries.Checked then
    GapMode := spmBoth
  else
    GapMode := spmCPS;

  VSTSetPauses(frmMain.VST, tedGap.Value, GapMode, SelLoop);
  Close;
end;

// -----------------------------------------------------------------------------

end.

