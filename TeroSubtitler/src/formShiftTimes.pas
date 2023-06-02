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

unit formShiftTimes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UWRadioButton, UWLayout, UWTimeEdit;

type

  { TfrmShiftTimes }

  TfrmShiftTimes = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    cboOffset: TComboBox;
    lblOffset: TLabel;
    lblScope: TLabel;
    lyoSimple1: TUWLayout;
    rbnAllTheSubtitles: TUWRadioButton;
    rbnFromTheSelectedSubtitle: TUWRadioButton;
    rbnOnlySelectedSubtitles: TUWRadioButton;
    tedOffset: TUWTimeEdit;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmShiftTimes: TfrmShiftTimes;

// -----------------------------------------------------------------------------

implementation

uses
  procVST, procWorkspace, procSubtitle, procColorTheme, procCommon, procTypes,
  UWSystem.XMLLang, formMain, UWSubtitles.Utils, UWSubtitleAPI;

{$R *.lfm}

// -----------------------------------------------------------------------------

procedure ApplyTimeOffset(const Item: PUWSubtitleItem; const Index: Integer);
var
  it, ft, v: Integer;
begin
  with frmShiftTimes do
  begin
    if cboOffset.ItemIndex = 0 then
      v := tedOffset.Value
    else
      v := -tedOffset.Value;
  end;
  ShiftTime(Item^.InitialTime, Item^.FinalTime, v, it, ft);
  SetSubtitleTimes(Index, it, ft, False, False);
end;

// -----------------------------------------------------------------------------

{ TfrmShiftTimes }

// -----------------------------------------------------------------------------

procedure TfrmShiftTimes.FormCreate(Sender: TObject);
begin
  LoadLanguage(Self);

  tedOffset.FPS      := GetFPS;
  tedOffset.TimeMode := GetTimeEditMode;

  if (frmMain.VST.SelectedCount = 1) then
    rbnFromTheSelectedSubtitle.Checked := True
  else if (frmMain.VST.SelectedCount > 1) then
    rbnOnlySelectedSubtitles.Checked := True
  else
    rbnAllTheSubtitles.Checked := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmShiftTimes.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmShiftTimes := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmShiftTimes.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmShiftTimes.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmShiftTimes.btnApplyClick(Sender: TObject);
var
  SelLoop: TVSTDoLoopSelection;
begin
  if rbnAllTheSubtitles.Checked then
    SelLoop := dlAll
  else if rbnFromTheSelectedSubtitle.Checked then
    SelLoop := dlCurrentToLast
  else
    SelLoop := dlSelected;

  VSTDoLoop(frmMain.VST, @ApplyTimeOffset, SelLoop, True, True);
  Close;
end;

// -----------------------------------------------------------------------------

end.

