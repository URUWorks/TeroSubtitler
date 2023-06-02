{*
 *  URUWorks MRU
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

unit UWFiles.MRU;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, Classes, Menus;

type

  { TUWMRU }

  TUWMRU = class
  private
    { Private declarations }
    FStrings        : TStrings;
    FMax            : Byte;
    FMenu           : TPopupMenu;
    FOnMRUItemClick : TNotifyEvent;
    FOnChange       : TNotifyEvent;
    procedure RefreshMenu;
  public
    { Public declarations }
    constructor Create(const Menu: TPopupMenu);
    destructor Destroy; override;
    procedure Add(const FileName: String);
    procedure SaveToJSON(const FileName: String);
    procedure LoadFromJSON(const FileName: String);
    procedure UpdateMenu(const AMenu: TMenuItem);
    property Items          : TStrings     read FStrings;
    property Max            : Byte         read FMax            write FMax;
    property OnMRUItemClick : TNotifyEvent read FOnMRUItemClick write FOnMRUItemClick;
    property OnChange       : TNotifyEvent read FOnChange       write FOnChange;
  end;

// -----------------------------------------------------------------------------

implementation

uses
    fpjson, jsonparser;

// -----------------------------------------------------------------------------

{ TUWMRU }

// -----------------------------------------------------------------------------

constructor TUWMRU.Create(const Menu: TPopupMenu);
begin
  FStrings  := TStringList.Create;
  FMax      := 8;
  FOnChange := NIL;
  FMenu     := Menu;
end;

// -----------------------------------------------------------------------------

destructor TUWMRU.Destroy;
begin
  FOnChange := NIL;
  FStrings.Free;

  inherited;
end;

// -----------------------------------------------------------------------------

procedure TUWMRU.UpdateMenu(const AMenu: TMenuItem);
var
  Idx : Integer;
  mnu : TMenuItem;
begin
  if AMenu <> NIL then
  begin
    // Add files (if exists)
    AMenu.Clear;
    AMenu.Enabled := (FStrings.Count > 0);
    for Idx := 0 to FStrings.Count-1 do
    begin
      mnu         := TMenuItem.Create(AMenu);
      mnu.Name    := 'mru_' + IntToStr(Idx);
      mnu.Caption := FStrings[Idx];
      mnu.OnClick := FOnMRUItemClick;
      if not FileExists(FStrings[Idx]) then mnu.Enabled := False;
      AMenu.Add(mnu);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWMRU.RefreshMenu;
begin
  UpdateMenu(FMenu.Items);
  if Assigned(FOnChange) then FOnChange(Self);
end;

// -----------------------------------------------------------------------------

procedure TUWMRU.Add(const FileName: String);
var
  Idx : Integer;
begin
  // Search if file is already present
  Idx := FStrings.IndexOf(FileName);
  if (Idx = -1) then
    FStrings.Insert(0, FileName)
  else
    FStrings.Move(Idx, 0);

  while (FStrings.Count > FMax) do FStrings.Delete(FMax);

  RefreshMenu;
end;

// -----------------------------------------------------------------------------

procedure TUWMRU.SaveToJSON(const FileName: String);
var
  FileStream : TFileStream;
  joData     : TJSONObject;
  jaData     : TJSONArray;
  s          : TJSONStringType;
  i          : Integer;
begin
  if FStrings.Count = 0 then Exit;
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    joData := TJSONObject.Create;
    jaData := TJSONArray.Create;
    try
      for i := 0 to FStrings.Count-1 do
        jaData.Add(FStrings[i]);

      joData.Add('MRU', jaData);
      s := joData.AsJSON;
      FileStream.WriteBuffer(s[1], Length(s));
    finally
      joData.Free;
    end;
  finally
    FileStream.Destroy;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWMRU.LoadFromJSON(const FileName: String);
var
  FileStream : TFileStream;
  Parser     : TJSONParser;
  Data       : TJSONData;
  joData     : TJSONObject;
  jaData     : TJSONArray;
  i          : Integer;
  ZeroArray  : TJSONArray;
begin
  if not FileExists(FileName) then Exit;
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Parser := TJSONParser.Create(FileStream);
    ZeroArray := TJSONArray.Create;
    try
      try
        Data := Parser.Parse;
      except
        Exit;
      end;
      FStrings.Clear;
      ZeroArray.Clear;
      joData := TJSONObject(Data);
      jaData := joData.Get('MRU', ZeroArray);
      for i := 0 to jaData.Count-1 do
        FStrings.Add(jaData.Strings[i]);
    finally
      ZeroArray.Free;
      FreeAndNil(Parser);
    end;
  finally
    FileStream.Destroy;
  end;
  RefreshMenu;
end;

// -----------------------------------------------------------------------------

end.
