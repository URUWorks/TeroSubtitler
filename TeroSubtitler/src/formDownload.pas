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

  TDownloadResult = (dfCompleted, dfCanceled, dfError);

  TfrmDownload = class(TForm)
    btnCancel: TButton;
    lblFile: TLabel;
    lblFileData: TLabel;
    lblReceived: TLabel;
    lblReceivedData: TLabel;
    lblSpeedData: TLabel;
    lblElapsedData: TLabel;
    lblRemainingData: TLabel;
    lblSpeed: TLabel;
    lblElapsed: TLabel;
    lblRemaining: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDownloader: TDownload;
    FURL, FLocalFile: String;
    FDownloaded: TDownloadResult;
    procedure DoOnDownloadProgress(Sender: TObject; const AFrom, ATo: String; const APos, ASize, AElapsed, ARemaining, ASpeed: LongInt);
    procedure DoOnDownloadCompleted(Sender: TObject);
  public
    function Execute(const AURL, ALocalFile: String): TDownloadResult;
  end;

  function ShowDownloadDialog(const AURL, ALocalFile: String; const ADeleteFileOnCancel: Boolean = True): TDownloadResult;

var
  frmDownload: TfrmDownload;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procDialogs, procColorTheme, Zipper,
  UWSystem.TimeUtils;

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
  FDownloader := TDownload.Create;
  FDownloader.OnDownloadProgress := @DoOnDownloadProgress;
  FDownloader.OnDownloadCompleted := @DoOnDownloadCompleted;
end;

// -----------------------------------------------------------------------------

procedure TfrmDownload.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FDownloader) then
    FDownloader.CancelDownload;

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
  FDownloader.DownloadToFile(FURL, FLocalFile);
end;

// -----------------------------------------------------------------------------

procedure TfrmDownload.btnCancelClick(Sender: TObject);
begin
  FDownloaded := dfCanceled;
  Close;
end;

// -----------------------------------------------------------------------------

function TfrmDownload.Execute(const AURL, ALocalFile: String): TDownloadResult;
begin
  FURL        := AURL;
  FLocalFile  := ALocalFile;
  FDownloaded := dfError;
  ShowModal;
  Result := FDownloaded;
end;

// -----------------------------------------------------------------------------

procedure TfrmDownload.DoOnDownloadProgress(Sender: TObject; const AFrom, ATo: String; const APos, ASize, AElapsed, ARemaining, ASpeed: LongInt);
begin
  lblFileData.Caption := ExtractFileName(ATo);

  lblSpeedData.Caption := FormatSpeed(ASpeed);
  lblSpeedData.Update;

  lblElapsedData.Caption := TimeToString(AElapsed);
  lblElapsedData.Update;

  if ASize > 0 then
    lblRemainingData.Caption := TimeToString(ARemaining)
  else
    lblRemainingData.Caption := '';

  lblRemainingData.Update;

  if ASize > 0 then
    lblReceivedData.Caption := FormatSize(APos) + ' / ' + FormatSize(ASize)
  else
    lblReceivedData.Caption := FormatSize(APos);

  lblReceived.Update;

  Application.ProcessMessages;
end;

// -----------------------------------------------------------------------------

procedure TfrmDownload.DoOnDownloadCompleted(Sender: TObject);
begin
  FDownloaded := dfCompleted;
  btnCancel.Enabled := False;

  if FLocalFile.EndsWith('.zip') then
  begin
    UnZipFile(FLocalFile, ExtractFileDir(FLocalFile));
    DeleteFile(FLocalFile);
  end;

  Close;
end;

// -----------------------------------------------------------------------------

function ShowDownloadDialog(const AURL, ALocalFile: String; const ADeleteFileOnCancel: Boolean = True): TDownloadResult;
begin
  with TfrmDownload.Create(NIL) do
  try
    Result := Execute(AURL, ALocalFile);
    if Result = dfError then
      ShowErrorMessageDialog(lngFailedToDownload)
    else if Result = dfCanceled then
    begin
      if ADeleteFileOnCancel and FileExists(ALocalFile) then
        DeleteFile(ALocalFile);

      ShowErrorMessageDialog(lngDownloadCanceled);
    end;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

end.

