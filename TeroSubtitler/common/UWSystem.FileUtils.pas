{*
 *  URUWorks Lazarus File Utils
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

unit UWSystem.FileUtils;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, Classes;

type

  { TUWCustomFile }

  TUWCustomFile = class
  private
    { Private declarations }
    FFileName  : String;
    FFileMode  : Byte;
    FReady     : Boolean;
    FOverwrite : Boolean;
    FErrorCode : Integer;
    procedure GetErrorCode;
    procedure Open; virtual; abstract;
    procedure Close; virtual; abstract;
  public
    { Public declarations }
    constructor Create(const FileName: String; const Mode: Byte = fmOpenRead; const Overwrite: Boolean = False);
    destructor Destroy; override;
    function EOF: Boolean; virtual; abstract;
    property Ready     : Boolean read FReady;
    property ErrorCode : Integer read FErrorCode;
  end;

  { TUWFile }

  TUWFile = class(TUWCustomFile)
  private
    { Private declarations }
    FFile        : File;
    FTransferred : Integer;
    procedure Open; override;
    procedure Close; override;
  public
    { Public declarations }
    function Read(var Buf; Count: Integer): Boolean;
    function Write(const Buf; Count: Integer): Boolean;
    function Seek(N: LongInt): Boolean;
    function Position: LongInt;
    function Truncate: Boolean;
    function Size: Integer;
    function EOF: Boolean; override;
    property Transferred: Integer read FTransferred;
  end;

  { TUWTextFile }

  TUWTextFile = class(TUWCustomFile)
  private
    { Private declarations }
    FFile: TextFile;
    procedure Open; override;
    procedure Close; override;
  public
    { Public declarations }
    function ReadLn(var Buf: String): Boolean;
    function WriteLn(const Buf: String): Boolean;
    function EOF: Boolean; override;
  end;

{ Helpers }

function GetFileSize(AFileName: String): Int64;
function IsEmptyFolder(const AFolder: String): Boolean;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

{ TUWCustomFile }

// -----------------------------------------------------------------------------

constructor TUWCustomFile.Create(const FileName: String; const Mode: Byte = fmOpenRead; const Overwrite: Boolean = False);
begin
  FFileName  := FileName;
  FFileMode  := FileMode;
  FReady     := False;
  FOverwrite := Overwrite;
  FErrorCode := 0;
  FileMode   := Mode;
  Open;
end;

// -----------------------------------------------------------------------------

destructor TUWCustomFile.Destroy;
begin
  Close;
  FileMode := FFileMode;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWCustomFile.GetErrorCode;
begin
  FErrorCode := IOResult;
end;

// -----------------------------------------------------------------------------

{ TUWFile }

// -----------------------------------------------------------------------------

procedure TUWFile.Open;
begin
  if (FFileName <> '') and FileExists(FFileName) then
  begin
    AssignFile(FFile, FFileName);

    if FileExists(FFileName) then
      {$I-}Reset(FFile, 1){$I+}
    else if FileMode = fmOpenWrite then
      {$I-}ReWrite(FFile, 1);{$I+}

    GetErrorCode;
    if FErrorCode = 0 then FReady := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWFile.Close;
begin
  if FReady then {$I-}CloseFile(FFile);{$I+}
end;

// -----------------------------------------------------------------------------

function TUWFile.Read(var Buf; Count: Integer): Boolean;
begin
  if FReady then
  begin
    {$I-}BlockRead(FFile, Buf, Count, FTransferred);{$I+}
    GetErrorCode;
    Result := (FErrorCode = 0);
  end
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWFile.Write(const Buf; Count: Integer): Boolean;
begin
  if FReady then
  begin
    {$I-}BlockWrite(FFile, Buf, Count, FTransferred);{$I+}
    GetErrorCode;
    Result := (FErrorCode = 0);
  end
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWFile.Seek(N: LongInt): Boolean;
begin
  if FReady then
  begin
    {$I-}System.Seek(FFile, N);{$I+}
    GetErrorCode;
    Result := (FErrorCode = 0);
  end
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWFile.Position: LongInt;
begin
  if FReady then
  begin
    {$I-}Result := FilePos(FFile);{$I+}
    GetErrorCode;
  end
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function TUWFile.Truncate: Boolean;
begin
  if FReady then
  begin
    {$I-}System.Truncate(FFile);{$I+}
    GetErrorCode;
    Result := (FErrorCode = 0);
  end
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWFile.Size: Integer;
begin
  if FReady then
  begin
    {$I-}Result := FileSize(FFile);{$I+}
    GetErrorCode;
  end
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function TUWFile.EOF: Boolean;
begin
  if FReady then
  begin
    {$I-}Result := System.EOF(FFile);{$I+}
    GetErrorCode;
  end
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

{ TUWTextFile }

// -----------------------------------------------------------------------------

procedure TUWTextFile.Open;
begin
  if (FFileName <> '') then
  begin
    AssignFile(FFile, FFileName);

    if FileExists(FFileName) then
    begin
      if FileMode = fmOpenRead then
        {$I-}Reset(FFile){$I+}
      else
      begin
        if not FOverwrite then
          {$I-}Append(FFile){$I+}
        else
          {$I-}ReWrite(FFile){$I+}
      end;
    end
    else
      {$I-}ReWrite(FFile);{$I+}

    GetErrorCode;
    if FErrorCode = 0 then FReady := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWTextFile.Close;
begin
  if FReady then {$I-}CloseFile(FFile);{$I+}
end;

// -----------------------------------------------------------------------------

function TUWTextFile.ReadLn(var Buf: String): Boolean;
begin
  if FReady then
  begin
    {$I-}System.ReadLn(FFile, Buf);{$I+}
    GetErrorCode;
    Result := (FErrorCode = 0);
  end
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWTextFile.WriteLn(const Buf: String): Boolean;
begin
  if FReady then
  begin
    {$I-}System.WriteLn(FFile, Buf);{$I+}
    GetErrorCode;
    Result := (FErrorCode = 0);
  end
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWTextFile.EOF: Boolean;
begin
  if FReady then
  begin
    {$I-}Result := System.EOF(FFile);{$I+}
    GetErrorCode;
  end
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function GetFileSize(AFileName: String): Int64;
var
  Info: TSearchRec;
begin
  if FindFirst(AFileName, 0, Info) = 0 then
    Result := Info.Size
  else
    Result := 0;

  FindClose(Info);
end;

// -----------------------------------------------------------------------------

function IsEmptyFolder(const AFolder: String): Boolean;
var
  Info: TSearchRec;
  i: Integer;
begin
  Result := False;
  if FindFirst(IncludeTrailingPathDelimiter(AFolder) + '*', faAnyFile, Info) = 0 then
    try
      for i := 1 to 2 do
        if (Info.Name = '.') or (Info.Name = '..') then
          Result := FindNext(Info) <> 0;
    finally
      FindClose(Info);
    end;
end;

// -----------------------------------------------------------------------------

end.
