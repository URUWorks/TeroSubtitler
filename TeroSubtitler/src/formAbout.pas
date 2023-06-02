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

unit formAbout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LCLIntf, fileinfo,
  winpeimagereader, {need this for reading exe info}
  elfreader, {needed for reading ELF executables}
  machoreader; {needed for reading MACH-O executables}


type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnClose: TButton;
    imgLogo: TImage;
    lblDonate: TLabel;
    lblInfo: TLabel;
    lblUW: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblDonateClick(Sender: TObject);
    procedure lblUWClick(Sender: TObject);
  private

  public

  end;

var
  frmAbout: TfrmAbout;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procCommon, procColorTheme, UWSystem.StrUtils;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmAbout }

// -----------------------------------------------------------------------------

procedure TfrmAbout.FormCreate(Sender: TObject);
var
  FileVerInfo: TFileVersionInfo;
begin
  LoadLanguage(Self);

  FileVerInfo := TFileVersionInfo.Create(NIL);
  try
    FileVerInfo.ReadFileInfo;

    lblInfo.Caption := Format(ReplaceEnters(lblInfo.Caption, '|', sLineBreak),
      [FileVerInfo.VersionStrings.Values['FileVersion'], {$I %TIME%}, {$I %DATE%}, {$I %FPCVERSION%}, {$I %FPCTARGET%}]);

    lblUW.Caption := 'uruworks.net' + sLineBreak + FileVerInfo.VersionStrings.Values['LegalCopyright'];
    lblDonate.Caption := ReplaceEnters(lblDonate.Caption, '|', sLineBreak);
  finally
    FileVerInfo.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmAbout := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.FormShow(Sender: TObject);
var
  png: TPortableNetworkGraphic;
begin
  CheckColorTheme(Self);
  png := TPortableNetworkGraphic.Create;
  try
    if ColorThemeInstance.GetRealColorMode = cmDark then
      png.LoadFromLazarusResource('terosubtitler_dark')
    else
      png.LoadFromLazarusResource('terosubtitler');

    imgLogo.Picture.Graphic := png;
  finally
    png.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.lblUWClick(Sender: TObject);
begin
  OpenURL(ProgramWebsite);
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.lblDonateClick(Sender: TObject);
begin
  OpenURL(UWDonateURL);
end;

// -----------------------------------------------------------------------------

end.

