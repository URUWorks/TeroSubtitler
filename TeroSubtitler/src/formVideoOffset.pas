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

unit formVideoOffset;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  UWTimeEdit, UWRadioButton;

type

  { TfrmVideoOffset }

  TfrmVideoOffset = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
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
  frmVideoOffset: TfrmVideoOffset;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

uses procTypes, procVST, procSubtitle, procWorkspace, UWSubtitleAPI,
  UWSubtitles.Utils, UWSystem.SysUtils, formMain, LCLTranslator;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmVideoOffset }

// -----------------------------------------------------------------------------

procedure TfrmVideoOffset.FormCreate(Sender: TObject);
begin
  tedTime.FPS := GetFPS;
  tedTime.TimeMode := GetTimeEditMode;
  tedTime.Value := Workspace.VideoOffset;

  {$IFNDEF WINDOWS}
  PrepareCustomControls(Self);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoOffset.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmVideoOffset := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoOffset.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoOffset.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmVideoOffset.btnApplyClick(Sender: TObject);
begin
  if tedTime.Value <> Workspace.VideoOffset then
  begin
    Workspace.VideoOffset := tedTime.Value;
    frmMain.lblMediaTime.Caption := GetTimeStr(frmMain.MPV.GetMediaPosInMs + Workspace.VideoOffset);
    frmMain.WAVE.OffsetMs := Workspace.VideoOffset;
    UpdateVideoLengthString;
  end;

  Close;
end;

// -----------------------------------------------------------------------------

end.

