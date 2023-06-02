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

unit procMRU;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, Menus;

type

  { TMRU }

  TMRUInfoObject = class
    VideoFile,
    WaveformFile: String;
    SelectedLine,
    MPVPosition,
    WAVEPosition: Integer;
    SMPTE: Boolean;
  end;

  TMRU = class
  private
    { Private declarations }
    FStrings        : TStringList;
    FMax            : Byte;
    FMenu           : TPopupMenu;
    FOnMRUItemClick : TNotifyEvent;
    FOnChange       : TNotifyEvent;
    procedure RefreshMenu;
  public
    { Public declarations }
    constructor Create(const Menu: TPopupMenu);
    destructor Destroy; override;
    procedure Add(const FileName, AVideoFile, AWaveFile: String; const ASelectedLine, AMPVPosition, AWAVEPosition: Integer; const ASMPTE: Boolean);
    procedure Update(const FileName, AVideoFile, AWaveFile: String; const ASelectedLine, AMPVPosition, AWAVEPosition: Integer; const ASMPTE: Boolean);
    function GetValues(const FileName: String): TMRUInfoObject;
    procedure SaveToXML(const AFileName: String);
    procedure LoadFromXML(const AFileName: String);
    procedure UpdateMenu(const AMenu: TMenuItem);
    property Items          : TStringList  read FStrings;
    property Max            : Byte         read FMax            write FMax;
    property OnMRUItemClick : TNotifyEvent read FOnMRUItemClick write FOnMRUItemClick;
    property OnChange       : TNotifyEvent read FOnChange       write FOnChange;
  end;

// -----------------------------------------------------------------------------

implementation

uses
  Laz2_DOM, laz2_XMLRead, laz2_XMLWrite;

// -----------------------------------------------------------------------------

{ TMRU }

// -----------------------------------------------------------------------------

constructor TMRU.Create(const Menu: TPopupMenu);
begin
  FStrings  := TStringList.Create;
  FMax      := 8;
  FOnChange := NIL;
  FMenu     := Menu;
end;

// -----------------------------------------------------------------------------

destructor TMRU.Destroy;
var
  i: Integer;
begin
  FOnChange := NIL;
  for i := 0 to FStrings.Count-1 do
    if FStrings.Objects[i] <> NIL then FStrings.Objects[i].Free;

  FStrings.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TMRU.UpdateMenu(const AMenu: TMenuItem);
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
      mnu.Tag     := Idx;
      if not FileExists(FStrings[Idx]) then mnu.Enabled := False;
      AMenu.Add(mnu);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMRU.RefreshMenu;
begin
  UpdateMenu(FMenu.Items);
  if Assigned(FOnChange) then FOnChange(Self);
end;

// -----------------------------------------------------------------------------

procedure TMRU.Add(const FileName, AVideoFile, AWaveFile: String; const ASelectedLine, AMPVPosition, AWAVEPosition: Integer; const ASMPTE: Boolean);
var
  Idx : Integer;
  Obj : TMRUInfoObject;
begin
  // Search if file is already present
  Idx := FStrings.IndexOf(FileName);
  if (Idx = -1) then
  begin
    Obj := TMRUInfoObject.Create;
    with Obj do
    begin
      VideoFile    := AVideoFile;
      WaveformFile := AWaveFile;
      SelectedLine := ASelectedLine;
      MPVPosition  := AMPVPosition;
      WAVEPosition := AWAVEPosition;
      SMPTE        := ASMPTE;
    end;
    FStrings.InsertObject(0, FileName, Obj);

    while (FStrings.Count > FMax) do
    begin
      if FStrings.Objects[FMax] <> NIL then
        FStrings.Objects[FMax].Free;
      FStrings.Delete(FMax);
    end;
  end
  else if (Idx > 0) then
  begin
    FStrings.Move(Idx, 0);
  end;

  RefreshMenu;
end;

// -----------------------------------------------------------------------------

procedure TMRU.Update(const FileName, AVideoFile, AWaveFile: String; const ASelectedLine, AMPVPosition, AWAVEPosition: Integer; const ASMPTE: Boolean);
var
  Idx : Integer;
begin
  Idx := FStrings.IndexOf(FileName);
  if (Idx <> -1) and (FStrings.Objects[Idx] <> NIL) then
  begin
    with TMRUInfoObject(FStrings.Objects[Idx]) do
    begin
      VideoFile    := AVideoFile;
      WaveformFile := AWaveFile;
      SelectedLine := ASelectedLine;
      MPVPosition  := AMPVPosition;
      WAVEPosition := AWAVEPosition;
      SMPTE        := ASMPTE;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TMRU.GetValues(const FileName: String): TMRUInfoObject;
var
  Idx : Integer;
begin
  Result := NIL;
  Idx := FStrings.IndexOf(FileName);
  if (Idx <> -1) and (FStrings.Objects[Idx] <> NIL) then
    Result := TMRUInfoObject(FStrings.Objects[Idx]);
end;

// -----------------------------------------------------------------------------

procedure TMRU.SaveToXML(const AFileName: String);
var
  XmlDoc : TXMLDocument;
  Root, Element, Node: TDOMNode;
  i : Integer;
begin
  if FStrings.Count = 0 then Exit;

  XmlDoc := TXMLDocument.Create;
  try
    Root := XmlDoc.CreateElement('MRU');
      XmlDoc.Appendchild(Root);
    Root := XmlDoc.DocumentElement;

    Element := XmlDoc.CreateElement('RecentFiles');
    Root.AppendChild(Element);

    for i := 0 to FStrings.Count-1 do
    begin
      Node := XmlDoc.CreateElement('Recent');
      with TMRUInfoObject(FStrings.Objects[i]), TDOMElement(Node) do
      begin
        TextContent := FStrings[i];
        if not VideoFile.IsEmpty then
          SetAttribute('VideoFile', VideoFile);

        if not WaveformFile.IsEmpty then
          SetAttribute('WaveformFile', WaveformFile);

        SetAttribute('SelectedLine', SelectedLine.ToString);
        SetAttribute('MPVPosition', MPVPosition.ToString);
        SetAttribute('WAVEPosition', WAVEPosition.ToString);
        SetAttribute('SMPTE', SMPTE.ToString);
      end;
      Element.AppendChild(Node);
    end;

    WriteXMLFile(XmlDoc, AFileName);
  finally
    XmlDoc.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMRU.LoadFromXML(const AFileName: String);
var
  Doc     : TXMLDocument;
  Members : TDOMNodeList;
  Member  : TDOMNode;
  i       : Integer;

  function GetDefStr(const AItemName: String): String;
  var
    Attr: TDOMNode;
  begin
    Attr := Member.Attributes.GetNamedItem(AItemName);
    if Assigned(Attr) then
      Result := Attr.TextContent
    else
      Result := '';
  end;

  function GetDefInt(const AItemName: String): Integer;
  var
    Attr: TDOMNode;
  begin
    Attr := Member.Attributes.GetNamedItem(AItemName);
    if Assigned(Attr) then
      Result := Attr.TextContent.ToInteger
    else
      Result := 0;
  end;

begin
  if not FileExists(AFileName) then Exit;

  // Read the XML file into an XML Document
  try
    ReadXMLFile(Doc, AFileName);
  except
  end;

  if Assigned(Doc) then
  begin
    FStrings.Clear;

    // Get all nodes with name "Recent"
    Members := Doc.GetElementsByTagName('Recent');
    if Assigned(Members) then
      // For all Member nodes
      for i := Members.Count-1 downto 0 do //for i := 0 to Members.Count-1 do
      begin
        Member := Members[i];
        // Get the attribute
        if Member.HasAttributes then
        begin
          Add(
            Member.TextContent,
            GetDefStr('VideoFile'),
            GetDefStr('WaveformFile'),
            GetDefInt('SelectedLine'),
            GetDefInt('MPVPosition'),
            GetDefInt('WAVEPosition'),
            Boolean(GetDefInt('SMPTE'))
            );
        end;
      end;
  end;

  RefreshMenu;
end;

// -----------------------------------------------------------------------------

end.

