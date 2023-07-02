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

unit formGoToTime;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UWTimeEdit;

type

  { TfrmGoToTime }

  TfrmGoToTime = class(TForm)
    btnClose: TButton;
    btnGo: TButton;
    lblTime: TLabel;
    tedPosition: TUWTimeEdit;
    procedure btnCloseClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmGoToTime: TfrmGoToTime;

// -----------------------------------------------------------------------------

implementation

uses
  formMain, procWorkspace, procCommon, procMPV;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmGoToTime }

// -----------------------------------------------------------------------------

procedure TfrmGoToTime.FormCreate(Sender: TObject);
begin
  LoadLanguage(Self);
  tedPosition.TimeMode := GetTimeEditMode;
  tedPosition.FPS := GetFPS;
  tedPosition.Value := frmMain.MPV.GetMediaPosInMs;
end;

// -----------------------------------------------------------------------------

procedure TfrmGoToTime.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CloseAction := caFree;
  frmGoToTime := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmGoToTime.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmGoToTime.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmGoToTime.btnGoClick(Sender: TObject);
begin
  MPVSeekTo(tedPosition.Value);
  Close;
end;

// -----------------------------------------------------------------------------

end.

