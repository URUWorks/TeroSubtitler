{*
 *  URUWorks InetUtils
 *
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
 *  Copyright (C) 2001-2023 URUWorks, uruworks@gmail.com.
 *}

unit UWSystem.InetUtils;

{$mode objfpc}{$H+}

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, FileUtil, fphttpclient, opensslsockets;
type

  { TCheckInternet }

  TCheckInternet = class(TFPHTTPClient)
  public
    class function Available(const AServer: String = 'www.google.com'): Boolean;
  end;

  { TDownloadStream }

  TOnWriteStream = procedure(Sender: TObject; APos: Int64) of object;

  TDownloadStream = class(TStream)
  private
    FOnWriteStream: TOnWriteStream;
    FStream: TStream;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
    procedure DoProgress;
  published
    property OnWriteStream: TOnWriteStream read FOnWriteStream write FOnWriteStream;
  end;

  { TDownload }

  TOnDownloadProgress = procedure(Sender: TObject; const AFrom, ATo: String; const APos, ASize, AElapsed, ARemaining, ASpeed: LongInt) of object;
  TOnDownloadError = procedure(Sender: TObject; const AErrMsg: String = '') of object;
  TOnDownloadCompleted = TNotifyEvent;

  TDownload = class(TThread)
  private
    FFPHTTPClient: TFPHTTPClient;
    FURL: String;
    FLocalFile: String;
    FRemaining: Integer;
    FSpeed: Integer;
    FStartTime: QWord;
    FElapsed: QWord;
    FTick: Qword;
    FPos: Int64;
    FSize: Int64;
    FErrMsg: String;
    FOnDownloadProgress: TOnDownloadProgress;
    FOnDownloadError: TOnDownloadError;
    FOnDownloadCompleted: TOnDownloadCompleted;
    function GetContentLength: Boolean;
    procedure DoOnDataReceived(Sender: TObject; const ContentLength, {%H-}CurrentPos: Int64);
    procedure DoOnWriteStream(Sender: TObject; APos: Int64);
    procedure DoOnDownloadProgress;
    procedure DoOnDownloadError;
    procedure DoOnDownloadCompleted;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CancelDownload;
    procedure DownloadToFile(const AURL, ALocalFile: String);
  public
    property OnDownloadProgress: TOnDownloadProgress read FOnDownloadProgress write FOnDownloadProgress;
    property OnDownloadError: TOnDownloadError read FOnDownloadError write FOnDownloadError;
    property OnDownloadCompleted: TOnDownloadCompleted read FOnDownloadCompleted write FOnDownloadCompleted;
  end;

function FileExistsInServer(AURL: String): Boolean;
function DownloadToString(AURL: String; out AString: String): Boolean;
function IsInternetAlive: Boolean;

// Helpers
function FormatSize(const Size: Int64): String;
function FormatSpeed(const Speed: LongInt): String;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function FormatSize(const Size: Int64): String;
const
  KB = 1024;
  MB = KB * KB;
  GB = KB * MB;
begin
  if Size < KB then
    Result := FormatFloat('#,##0 Bytes', Size)
  else if Size < MB then
    Result := FormatFloat('#,##0.0 KB', Size / KB)
  else if Size < GB then
    Result := FormatFloat('#,##0.0 MB', Size / MB)
  else
    Result := FormatFloat('#,##0.0 GB', Size / GB);
end;

// -----------------------------------------------------------------------------

function FormatSpeed(const Speed: LongInt): String;
const
  KB = 1024;
  MB = KB * KB;
  GB = KB * MB;
begin
  if Speed < KB then
    Result := FormatFloat('#,##0 bits/s', Speed)
  else if Speed < MB then
    Result := FormatFloat('#,##0.0 kB/s', Speed / KB)
  else if Speed < GB then
    Result := FormatFloat('#,##0.0 MB/s', Speed / MB)
  else
    Result := FormatFloat('#,##0.0 GB/s', Speed / GB);
end;

// -----------------------------------------------------------------------------

function FixProtocol(const AURL: String): String;
begin
  Result := AURL;
  if (Pos('http://', Result) = 0) and (Pos('https://', Result) = 0) then
    Result := 'https://' + Result;
end;

// -----------------------------------------------------------------------------

function FileExistsInServer(AURL: String): Boolean;
var
  HTTPClient: TFPHTTPClient;
begin
  Result := False;
  AURL := FixProtocol(AURL);
  HTTPClient := TFPHTTPClient.Create(NIL);
  try
    HTTPClient.AllowRedirect := True;

    try
      HTTPClient.HTTPMethod('HEAD', AURL, NIL, []);
    except
      Exit;
    end;

    if HTTPClient.ResponseStatusCode = 404 then // file not exists
      Exit;

    Result := True;
  finally
    HTTPClient.Free;
  end;
end;

// -----------------------------------------------------------------------------

function DownloadToString(AURL: String; out AString: String): Boolean;
var
  HTTPClient: TFPHTTPClient;
begin
  Result := False;
  AURL := FixProtocol(AURL);

  HTTPClient := TFPHTTPClient.Create(NIL);
  try
    HTTPClient.AllowRedirect := True;
    HTTPClient.AddHeader('User-Agent', 'Mozilla/5.0(compatible; fpweb)');
    try
      AString := HTTPClient.Get(AURL);
      Result := True;
    except
      AString := '';
    end;
  finally
    HTTPClient.Free;
  end;
end;

// -----------------------------------------------------------------------------

{ TCheckInternet }

// -----------------------------------------------------------------------------

class function TCheckInternet.Available(const AServer: String = 'www.google.com'): Boolean;
begin
  Result := False;
  with TCheckInternet.Create(NIL) do
  try
    try
      ConnectToServer(AServer, 80);
      Result := True;
    except
    end;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

function IsInternetAlive: Boolean;
begin
  Result := TCheckInternet.Available;
end;

// -----------------------------------------------------------------------------

{ TDownloadStream }

// -----------------------------------------------------------------------------

constructor TDownloadStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  FStream.Position := 0;
end;

// -----------------------------------------------------------------------------

destructor TDownloadStream.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TDownloadStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result := FStream.Read(Buffer, Count);
end;

// -----------------------------------------------------------------------------

function TDownloadStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result := FStream.Write(Buffer, Count);
  DoProgress;
end;

// -----------------------------------------------------------------------------

function TDownloadStream.Seek(Offset: LongInt; Origin: Word): LongInt;
begin
  Result := FStream.Seek(Offset, Origin);
end;

// -----------------------------------------------------------------------------

procedure TDownloadStream.DoProgress;
begin
  if Assigned(FOnWriteStream) then
    FOnWriteStream(Self, Position);
end;

// -----------------------------------------------------------------------------

{ TDownload }

// -----------------------------------------------------------------------------

constructor TDownload.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FFPHTTPClient := TFPHTTPClient.Create(NIL);
end;

// -----------------------------------------------------------------------------

destructor TDownload.Destroy;
begin
  FFPHTTPClient.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TDownload.DownloadToFile(const AURL, ALocalFile: String);
begin
  FURL := FixProtocol(AURL);
  FLocalFile := ALocalFile;
  Start;
end;

// -----------------------------------------------------------------------------

procedure TDownload.CancelDownload;
begin
  if Assigned(FFPHTTPClient) then
    FFPHTTPClient.Terminate;
end;

// -----------------------------------------------------------------------------

procedure TDownload.DoOnDataReceived(Sender: TObject; const ContentLength, CurrentPos: Int64);
begin
  if ContentLength > 0 then
    Abort;
end;

// -----------------------------------------------------------------------------

procedure TDownload.DoOnWriteStream(Sender: TObject; APos: Int64);
begin
  FElapsed := GetTickCount64 - FStartTime;
  if FElapsed < 1000 then
    Exit;

  FPos := APos;
  FSpeed := Round(FPos/FElapsed);

  if FSpeed > 0 then
    FRemaining := Round((FSize - FPos)/FSpeed);

  if FElapsed >= FTick + 1 then
  begin
    FTick := FElapsed;
    Synchronize(@DoOnDownloadProgress);
  end;
end;

// -----------------------------------------------------------------------------

procedure TDownload.DoOnDownloadProgress;
begin
  if Assigned(FOnDownloadProgress) then
    FOnDownloadProgress(Self, FURL, FLocalFile, FPos, FSize, FElapsed, FRemaining, FSpeed);
end;

// -----------------------------------------------------------------------------

procedure TDownload.DoOnDownloadError;
begin
  if Assigned(FOnDownloadError) then
    FOnDownloadError(Self, FErrMsg);
end;

// -----------------------------------------------------------------------------

procedure TDownload.DoOnDownloadCompleted;
begin
  if Assigned(FOnDownloadCompleted) then
    FOnDownloadCompleted(Self);
end;

// -----------------------------------------------------------------------------

function TDownload.GetContentLength: Boolean;
var
  SS: TStringStream;
  HttpClient: TFPHTTPClient;
  URL: String;
begin
  Result := False;
  FSize := 0;
  SS := TStringStream.Create('');
  try
    URL := FixProtocol(FURL);
    HttpClient := TFPHTTPClient.Create(NIL);
    try
      HttpClient.AllowRedirect := True;

      HttpClient.HTTPMethod('HEAD', URL, NIL, []);
      if HttpClient.ResponseStatusCode = 404 then // file not exists
        Exit;

      HttpClient.OnDataReceived := @DoOnDataReceived;
      HttpClient.ResponseHeaders.NameValueSeparator := ':';
      try
        HttpClient.HTTPMethod('GET', URL, SS, []);
      except
      end;
      if HttpClient.ResponseStatusCode = 200 then
        FSize := StrToInt64Def(HttpClient.ResponseHeaders.Values['CONTENT-LENGTH'], 0);

      Result := True;
    finally
      HttpClient.Free;
    end;
  finally
    SS.Free
  end;
end;

// -----------------------------------------------------------------------------

procedure TDownload.Execute;
var
  DS: TDownloadStream;
  Flags: Word;
  Success: Boolean;
begin
  FStartTime := GetTickCount64;
  if GetContentLength then
  begin
    Flags := fmOpenWrite;
    Success := False;

    if not FileExists(FLocalFile) then
    begin
      FPos := 0;
      Flags := Flags or fmCreate;
    end
    else
      FPos := FileUtil.FileSize(FLocalFile);

    DS := TDownloadStream.Create(TFileStream.Create(FLocalFile, Flags));
    try
      DS.FOnWriteStream := @DoOnWriteStream;
      try
        if (FPos > 0) and (FPos < FSize) then
        begin
          DS.Position := FPos;
          FFPHTTPClient.AddHeader('Range', 'bytes=' + IntToStr(FPos) + '-' + IntToStr(FSize));
        end;

        FFPHTTPClient.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
        FFPHTTPClient.AllowRedirect := True;
        FFPHTTPClient.HTTPMethod('GET', FURL, DS, [200, 206]);
        if not FFPHTTPClient.Terminated then
          Success := True;
      except
        on E: Exception do
        begin
          FErrMsg := E.Message;
          Synchronize(@DoOnDownloadError);
        end;
      end;
    finally
      DS.Free
    end;

    if Success then
    begin
      Synchronize(@DoOnDownloadProgress);
      Synchronize(@DoOnDownloadCompleted);
    end;
  end
  else
  begin
    FErrMsg := '404';
    Synchronize(@DoOnDownloadError);
  end;
end;

// -----------------------------------------------------------------------------

end.
