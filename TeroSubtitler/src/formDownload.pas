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

unit formDownload;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  UWSystem.InetUtils, LCLTranslator, procLocalize;

type

  { TfrmDownload }

  TfrmDownload = class(TForm)
    lblInfo: TLabel;
    lblDownload: TLabel;
    prbDownload: TProgressBar;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDownloader: TDownloader;
    FURL, FFolder: String;
    FDownloaded: Boolean;
    procedure DoOnWriteStream(Sender: TObject; APosition: Int64);
  public
    function Execute(const AURL, AFolder: String): Boolean;
  end;

function ShowDownloadDialog(const AURL, AFolder: String): Boolean;

var
  frmDownload: TfrmDownload;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procDialogs, procColorTheme, Zipper;

{$R *.lfm}

// -----------------------------------------------------------------------------

procedure UnZipFile(const AFileName, AFolder: String);
var
  UnZipper: TUnZipper;
begin
  if not FileExists(AFileName) then Exit;

  UnZipper := TUnZipper.Create;
  try
    UnZipper.FileName := AFileName;
    UnZipper.OutputPath := AFolder;
    try
      UnZipper.Examine;
      UnZipper.UnZipAllFiles;
    except
    end;
  finally
    UnZipper.Free;
  end;
end;

// -----------------------------------------------------------------------------

{ TfrmDownload }

// -----------------------------------------------------------------------------

procedure TfrmDownload.FormCreate(Sender: TObject);
begin
  FDownloader := TDownloader.Create;
end;

// -----------------------------------------------------------------------------

procedure TfrmDownload.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FDownloader.Free;
  CloseAction := caFree;
  frmDownload := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmDownload.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmDownload.FormActivate(Sender: TObject);
begin
  Application.ProcessMessages;
  if FDownloader.DownloadToFile(FURL, FFolder, @DoOnWriteStream) then
  begin
    FDownloaded := True;
    if FFolder.EndsWith('.zip') then
    begin
      UnZipFile(FFolder, ExtractFileDir(FFolder));
      DeleteFile(FFolder);
    end;
  end;
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmDownload.DoOnWriteStream(Sender: TObject; APosition: Int64);
begin
  lblInfo.Caption := FDownloader.SizeToString(APosition);
  Application.ProcessMessages;
end;

// -----------------------------------------------------------------------------

function TfrmDownload.Execute(const AURL, AFolder: String): Boolean;
begin
  FURL        := AURL;
  FFolder     := AFolder;
  FDownloaded := False;
  ShowModal;
  Result := FDownloaded;
end;

// -----------------------------------------------------------------------------

function ShowDownloadDialog(const AURL, AFolder: String): Boolean;
begin
  with TfrmDownload.Create(NIL) do
  try
    Result := Execute(AURL, AFolder);
    if not Result then
      ShowErrorMessageDialog(lngFailedToDownload);
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

end.

