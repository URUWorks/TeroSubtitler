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

{$IFDEF DARWIN}
  {$modeswitch ObjectiveC1}
{$ENDIF}

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets
  {$IFDEF DARWIN}, CocoaAll{$ENDIF};
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

  { TDownloader }

  TDownloader = class
  private
    {$IFDEF DARWIN}
    FTimeOut: Integer;
    {$ELSE}
    FHTTPClient: TFPHTTPClient;
    {$ENDIF}
    FMethod: String;
    function DoRequest(const AURL: String; AStream: TStream; AOnWrite: TOnWriteStream = NIL): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function DownloadToFile(const AURL, AOutFileName: String; AOnWrite: TOnWriteStream = NIL): Boolean;
    function DownloadToString(const AURL: String; out AString: String): Boolean;
    function SizeToString(const Size: Int64): String;
  end;

function IsInternetAlive: Boolean;

// -----------------------------------------------------------------------------

implementation

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
  FStream := NIL;
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
    FOnWriteStream(Self, Self.Position);
end;

// -----------------------------------------------------------------------------

{ TDownloader }

// -----------------------------------------------------------------------------

constructor TDownloader.Create;
begin
  {$IFDEF DARWIN}
  FTimeOut := 30;
  {$ELSE}
  FHTTPClient := TFPHTTPClient.Create(NIL);
  {$ENDIF}
  FMethod := 'GET';
end;

// -----------------------------------------------------------------------------

destructor TDownloader.Destroy;
begin
  {$IFNDEF DARWIN}
  FHTTPClient.Free;
  {$ENDIF}
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TDownloader.DoRequest(const AURL: String; AStream: TStream; AOnWrite: TOnWriteStream = NIL): Boolean;
var
  {$IFDEF DARWIN}
  urlRequest  : NSMutableURLRequest;
  requestData : NSMutableData;
  HdrNum      : Integer;
  urlResponse : NSURLResponse;
  error       : NSError;
  urlData     : NSData;
  {$ENDIF}
  FDS: TDownloadStream;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  FDS := TDownloadStream.Create(AStream);
  try
    FDS.FOnWriteStream := AOnWrite;
    try
      {$IFDEF DARWIN}
      urlRequest := NSMutableURLRequest.requestWithURL_cachePolicy_timeoutInterval(
                      NSURL.URLWithString(NSSTR(AURL)),
                      NSURLRequestUseProtocolCachePolicy, FTimeOut);

      if FMethod <> '' then
        urlRequest.setHTTPMethod(NSSTR(FMethod));

      urlData := NSURLConnection.sendSynchronousRequest_returningResponse_error(
                   urlRequest, @urlResponse, @error);

      if not Assigned(urlData) then
        Exit;

      FDS.Position := 0;
      FDS.WriteBuffer(urlData.bytes^, urlData.length);
      FDS.Position := 0;
      {$ELSE}
      FDS.Position := 0;
      FHTTPClient.AllowRedirect := True;
      FHTTPClient.HTTPMethod(FMethod, AURL, FDS, [200]);
      {$ENDIF}

      Result := FDS.Size > 0;
    except
      on E: Exception do
      begin
        //WriteLn('Exception occurred: ' +  E.ClassName +  sLineBreak +  E.Message);
      end;
    end;
  finally
    FDS.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TDownloader.DownloadToFile(const AURL, AOutFileName: String; AOnWrite: TOnWriteStream = NIL): Boolean;
var
  Dir: String;
  AStream: TFileStream;
begin
  Dir := ExtractFileDir(AOutFileName);
  if not Dir.IsEmpty and not DirectoryExists(Dir) then
    if not ForceDirectories(Dir) then
      Exit(False);

  AStream := TFileStream.Create(AOutFileName, fmCreate);
  try
    Result := DoRequest(AURL, AStream, AOnWrite);
  finally
    AStream.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TDownloader.DownloadToString(const AURL: String; out AString: String): Boolean;
var
  AStream : TStringStream;
begin
  AStream := TStringStream.Create;
  try
    Result := DoRequest(AURL, AStream);
    if Result then
      AString := AStream.DataString
    else
      AString := '';
  finally
    AStream.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TDownloader.SizeToString(const Size: Int64): String;
const
  KB = 1024;
  MB = 1024 * KB;
  GB = 1024 * MB;
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

end.
