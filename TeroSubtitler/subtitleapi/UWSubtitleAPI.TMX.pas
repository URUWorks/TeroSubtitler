{*
 *  URUWorks Translation Memory Exchange (TMX)
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

unit UWSubtitleAPI.TMX;

// -----------------------------------------------------------------------------

interface

uses Classes, SysUtils, FGL, laz2_XMLRead, laz2_DOM, laz2_XMLWrite, Math,
  UWSystem.StrUtils, lgStrHelpers;

type

  { TUWTMXHeader }

  PUWTMXHeader = ^TUWTMXHeader;
  TUWTMXHeader = record
    creationtool,        // "tero subtitler"
    creationtoolversion, // "1.0"
    datatype,            // "tbx"
    segtype,             // "block", "sentence"
    adminlang,           // "en-US"
    srclang,             // "en-US"
    otmf: String;        // ""
  end;

  { TUWTMXItem }

  PUWTMXItem = ^TUWTMXItem;
  TUWTMXItem = record
    Original,
    Translated,
    Notes: String;
  end;

  { TUWTMXItemInfo }

  PUWTMXItemInfo = ^TUWTMXItemInfo;
  TUWTMXItemInfo = record
    SrcLang,         // "en-US"
    DstLang: String; // "es-UY"
  end;

  { TUWTMXList }

  TUWTMXList = specialize TFPGList<PUWTMXItem>;

  { TUWTMXMap }

  TUWTMXMap = specialize TFPGMap<Double, Integer>; // Ratio, Index

  { TUWTMX }

  TUWTMX = class
  private
    FFileName : String;
    FChanged  : Boolean;
    FHeader   : TUWTMXHeader;
    FLangs    : TUWTMXItemInfo;
    FList     : TUWTMXList;
    FMap      : TUWTMXMap;
    FPercent  : Single;
    function GetLangPointer: PUWTMXItemInfo;
  public
    constructor Create(const AFileName: String; ASrcLang: String = ''; ADstLang: String = '');
    destructor Destroy; override;
    procedure Clear;
    procedure Close;
    procedure LoadFromFile(const AFileName: String);
    function SaveToFile(const AFileName: String): Boolean;
    function AddItem(const Original, Translated, Notes: String; const AllowDuplicate: Boolean = False): Integer;
    function FindSimilary(const AText: String): Integer; // fill Map with results
    function ItemFromMap(const AIndex: Integer): TUWTMXItem;
    function IndexFromMap(const AIndex: Integer): Integer;
    function Ready: Boolean;
    function FillMapWithIndex(const AIndex: Integer): Integer;
    function Find(AText: String): Integer;
    property FileName     : String         read FFileName write FFileName;
    property Items        : TUWTMXList     read FList;
    property Map          : TUWTMXMap      read FMap;
    property Langs        : PUWTMXItemInfo read GetLangPointer;
    property Header       : TUWTMXHeader   read FHeader write FHeader;
    property SimilPercent : Single         read FPercent;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Utils;

// -----------------------------------------------------------------------------

{ Helpers}

// -----------------------------------------------------------------------------

function ListCompare(const Item1, Item2: PUWTMXItem): Integer;
begin
  if Item1^.Original < Item2^.Original then
    Result := -1
  else if Item1^.Original > Item2^.Original then
    Result := 1
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function KeyMapCompare(const Key1, Key2: Double): Integer;
begin
  if Key1 < Key2 then
    Result := 1
  else if Key1 > Key2 then
    Result := -1
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function StrSimilarityRatio(const Str1, Str2: String): Double;
var
  MaxLen: Integer;
  Distance: Integer;
begin
  Result := 1.0;
  if Length(Str1) > Length(Str2) then
    MaxLen := Length(Str1)
  else
    MaxLen := Length(Str2);

  if MaxLen <> 0 then
  begin
    Distance := LevDistanceMyers(Str1, Str2);
    Result := Result - (Distance / MaxLen);
  end;
end;

// -----------------------------------------------------------------------------

{ TUWTMX }

// -----------------------------------------------------------------------------

constructor TUWTMX.Create(const AFileName: String; ASrcLang: String = ''; ADstLang: String = '');
begin
  FillByte(FHeader, SizeOf(TUWTMXHeader), 0);
  if ASrcLang.IsEmpty then ASrcLang := 'en-US';
  if ADstLang.IsEmpty then ADstLang := 'es-ES';
  FLangs.SrcLang := ASrcLang;
  FLangs.DstLang := ADstLang;

  FList := TUWTMXList.Create;

  FMap := TUWTMXMap.Create;
  FMap.OnKeyCompare := @KeyMapCompare;
  FMap.Sorted := True;

  FFileName := '';
  FChanged  := False;
  FPercent  := 0.65;

  if AFileName <> '' then LoadFromFile(AFileName);
end;

// -----------------------------------------------------------------------------

destructor TUWTMX.Destroy;
begin
  Close;
  FList.Free;
  FMap.Clear;
  FMap.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWTMX.Clear;
var
  i: Integer;
begin
  if FList.Count > 0 then
  begin
    for i := FList.Count-1 downto 0 do
    begin
      Dispose(PUWTMXItem(FList.Items[i]));
      FList.Items[i] := NIL;
      FList.Delete(i);
    end;
  end;

  FLangs.SrcLang := '';
  FLangs.DstLang := '';
end;

// -----------------------------------------------------------------------------

procedure TUWTMX.Close;
begin
  if FChanged then
    SaveToFile(FFileName);

  Clear;
end;

// -----------------------------------------------------------------------------

procedure TUWTMX.LoadFromFile(const AFileName: String);
var
  XmlDoc: TXMLDocument;
  NodeTU, NodeTUV,
  NodeID, NodeText: TDOMNode;
  Item: PUWTMXItem;
begin
  FFileName := AFileName;
  if not FileExists(AFileName) then Exit;

  XmlDoc := NIL;
  ReadXMLFile(XmlDoc, AFileName);
  if Assigned(XmlDoc) then
  try
    // Header
    NodeID := XMLFindNodeByName(XmlDoc, 'header');
    if NodeID <> NIL then
      with FHeader, NodeID.Attributes do
      begin
        creationtool        := GetNamedItem('creationtool').NodeValue;
        creationtoolversion := GetNamedItem('creationtoolversion').NodeValue;
        datatype            := GetNamedItem('datatype').NodeValue;
        segtype             := GetNamedItem('segtype').NodeValue;
        adminlang           := GetNamedItem('adminlang').NodeValue;
        srclang             := GetNamedItem('srclang').NodeValue;
        otmf                := GetNamedItem('o-tmf').NodeValue;
      end;

    if FHeader.srclang <> '' then FLangs.SrcLang := FHeader.srclang;

    // TMs
    Item := NIL;
    NodeTU := XMLFindNodeByName(XmlDoc, 'tu');
    while NodeTU <> NIL do
    begin
      New(Item);
      FillByte(Item[0], SizeOf(PUWTMXItem), 0);

      NodeTUV := NodeTU.FirstChild;
      while NodeTUV <> NIL do
      begin
        NodeID := NodeTUV.Attributes.GetNamedItem('xml:lang');
        if NodeID <> NIL then
        begin
          if (NodeID.NodeValue = FLangs.SrcLang) then
          begin // src
            NodeText := NodeTUV.FirstChild;
            while NodeText <> NIL do
            begin
              if NodeText.NodeName = 'seg' then
                Item^.Original := ReplaceEnters(NodeText.TextContent, '|', sLineBreak);

              NodeText := NodeText.NextSibling;
            end;
          end
          else if (NodeID.NodeValue = FLangs.DstLang) then
          begin // dst
            NodeText := NodeTUV.FirstChild;
            while NodeText <> NIL do
            begin
              if NodeText.NodeName = 'seg' then
                Item^.Translated := ReplaceEnters(NodeText.TextContent, '|', sLineBreak)
              else if NodeText.NodeName = 'note' then
                Item^.Notes := NodeText.TextContent;

              NodeText := NodeText.NextSibling;
            end;
          end;
        end;
        NodeTUV := NodeTUV.NextSibling;
      end;
      NodeTU := NodeTU.NextSibling;

      if (Item^.Original <> '') and (Item^.Translated <> '') then
        FList.Add(Item)
      else
        Dispose(Item);
    end;
  finally
    XmlDoc.Free;
  end;
  // Sort for fast searching
  if FList.Count > 0 then FList.Sort(@ListCompare);
end;

// -----------------------------------------------------------------------------

function TUWTMX.SaveToFile(const AFileName: String): Boolean;
var
  XmlDoc : TXMLDocument;
  Root, Element, Node, SubNode : TDOMNode;
  i : Integer;
begin
  Result := False;
  if FList.Count = 0 then
    Exit
  else if not AFileName.IsEmpty then
    FFileName := AFileName;

  XmlDoc := TXMLDocument.Create;
  try
    Root := XmlDoc.CreateElement('tmx');
      TDOMElement(Root).SetAttribute('version', '1.4');
      XmlDoc.Appendchild(Root);
    Root := XmlDoc.DocumentElement;

    Element := XmlDoc.CreateElement('header');
      with TDOMElement(Element), FHeader do
      begin
        SetAttribute('creationtool', creationtool);
        SetAttribute('creationtoolversion', creationtoolversion);
        SetAttribute('datatype', datatype);
        SetAttribute('segtype', segtype);
        SetAttribute('adminlang', adminlang);
        SetAttribute('srclang', srclang);
        SetAttribute('o-tmf', otmf);

      end;
    Root.AppendChild(Element);

    Element := XmlDoc.CreateElement('body');
    Root.AppendChild(Element);

    Root := Element; // body

    for i := 0 to FList.Count-1 do
    begin
      Node := XmlDoc.CreateElement('tu');
      Root.AppendChild(Node);

      // original
      Element := XmlDoc.CreateElement('tuv');
      TDOMElement(Element).SetAttribute('xml:lang', FLangs.SrcLang);
        SubNode := XmlDoc.CreateElement('seg');
        SubNode.TextContent := ReplaceEnters(FList[i]^.Original);
      Element.AppendChild(SubNode); // tuv
      Node.AppendChild(Element); // tu

      // translated
      Element := XmlDoc.CreateElement('tuv');
      TDOMElement(Element).SetAttribute('xml:lang', FLangs.DstLang);
        SubNode := XmlDoc.CreateElement('seg');
        SubNode.TextContent := ReplaceEnters(FList[i]^.Translated);
        Element.AppendChild(SubNode); // tuv
        if FList[i]^.Notes <> '' then
        begin
          SubNode := XmlDoc.CreateElement('note');
          SubNode.TextContent := FList[i]^.Notes;
          Element.AppendChild(SubNode); // tuv
        end;
      Node.AppendChild(Element); // tu
    end;

    try
      WriteXMLFile(XmlDoc, FFileName);
      Result := True;
    except
    end;
  finally
    XmlDoc.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TUWTMX.AddItem(const Original, Translated, Notes: String; const AllowDuplicate: Boolean = False): Integer;
var
  Item: PUWTMXItem;
  i: Integer;
begin
  Result := 0;
  if (Original = '') or (Translated = '') then Exit;

  if not AllowDuplicate and (FList.Count > 0) then
    for i := 0 to FList.Count-1 do
      if FList.Items[i]^.Original = Original then Exit;

  New(Item);
  Item^.Original   := Original;
  Item^.Translated := Translated;
  Item^.Notes      := Notes;
  Result   := FList.Add(Item)+1;
  FChanged := True;
end;

// -----------------------------------------------------------------------------

function TUWTMX.FindSimilary(const AText: String): Integer;
var
  i: Integer;
  Percent: Double;
begin
  Result := 0;
  FMap.Clear;

  if (FList.Count > 0) and (AText <> '') then
  begin
    for i := 0 to FList.Count-1 do
    begin
      Percent := StrSimilarityRatio(AText, FList.Items[i]^.Original);
      if Percent > FPercent then
      begin
        FMap.Add(Percent, i);
        if FMap.Count >= 5 then
          Break;
      end;
    end;

    Result := FMap.Count;
  end;
end;

// -----------------------------------------------------------------------------

function TUWTMX.ItemFromMap(const AIndex: Integer): TUWTMXItem;
begin
  FillByte(Result, SizeOf(TUWTMXItem), 0);
  if (FMap.Count > 0) and InRange(AIndex, 0, FMap.Count-1) then
    Result := FList[ FMap.Data[AIndex] ]^;
end;

// -----------------------------------------------------------------------------

function TUWTMX.IndexFromMap(const AIndex: Integer): Integer;
begin
  if (FMap.Count > 0) and InRange(AIndex, 0, FMap.Count-1) then
    Result := FMap.Data[AIndex];
end;

// -----------------------------------------------------------------------------

function TUWTMX.GetLangPointer: PUWTMXItemInfo;
begin
  Result := PUWTMXItemInfo(@FLangs);
end;

// -----------------------------------------------------------------------------

function TUWTMX.Ready: Boolean;
begin
  Result := (FFileName <> '') and (FLangs.SrcLang <> '') and (FLangs.DstLang <> '');
end;

// -----------------------------------------------------------------------------

function TUWTMX.FillMapWithIndex(const AIndex: Integer): Integer;
begin
  Result := 0;
  FMap.Clear;

  if (FList.Count > 0) and InRange(AIndex, 0, FList.Count-1) then
  begin
    FMap.Add(1.0, AIndex);
    Result := FMap.Count;
  end;
end;

// -----------------------------------------------------------------------------

function TUWTMX.Find(AText: String): Integer;
var
  i: Integer;
begin
  Result := 0;

  if (FList.Count > 0) and (AText <> '') then
  begin
    AText := AText.ToLower;
    for i := 0 to FList.Count-1 do
    begin
      Result := Pos(AText, FList.Items[i]^.Original.ToLower);
      if Result > 0 then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

end.
