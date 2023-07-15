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

unit formDelay;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  UWTimeEdit, UWRadioButton;

type

  { TfrmDelay }

  TfrmDelay = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    cboMode: TComboBox;
    lblScope: TLabel;
    rbnAllTheSubtitles: TUWRadioButton;
    rbnFromTheSelectedSubtitle: TUWRadioButton;
    rbnOnlySelectedSubtitles: TUWRadioButton;
    tedTime: TUWTimeEdit;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmDelay: TfrmDelay;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

uses procTypes, procVST, procSubtitle, procWorkspace, UWSubtitleAPI,
  UWSubtitles.Utils, UWSystem.SysUtils, formMain, procCommon;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmDelay }

// -----------------------------------------------------------------------------

procedure TfrmDelay.FormCreate(Sender: TObject);
begin
  LoadLanguage(Self);

  tedTime.FPS := GetFPS;
  tedTime.TimeMode := GetTimeEditMode;

  if (frmMain.VST.SelectedCount = 1) then
    rbnFromTheSelectedSubtitle.Checked := True
  else if (frmMain.VST.SelectedCount > 1) then
    rbnOnlySelectedSubtitles.Checked := True
  else
    rbnAllTheSubtitles.Checked := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmDelay.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmDelay := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmDelay.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmDelay.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure ApplyDelay(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^, frmDelay do
  begin
    SetSubtitleTime(Index, SetDelay(InitialTime, iff(cboMode.ItemIndex = 0, tedTime.Value, -tedTime.Value)), frmMain.tedInitial.Tag, False, False);
    SetSubtitleTime(Index, SetDelay(FinalTime, iff(cboMode.ItemIndex = 0, tedTime.Value, -tedTime.Value)), frmMain.tedFinal.Tag, False, False);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmDelay.btnApplyClick(Sender: TObject);
var
  SelLoop: TVSTDoLoopSelection;
begin
  if tedTime.Value > 0 then
  begin
    if rbnAllTheSubtitles.Checked then
      SelLoop := dlAll
    else if rbnFromTheSelectedSubtitle.Checked then
      SelLoop := dlCurrentToLast
    else
      SelLoop := dlSelected;

    VSTDoLoop(frmMain.VST, @ApplyDelay, SelLoop, True, True);
  end;
  Close;
end;

// -----------------------------------------------------------------------------

end.

