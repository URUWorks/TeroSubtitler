{*
 *  URUWorks TermBase eXchange (TBX)
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

unit UWSubtitleAPI.TBX;

// -----------------------------------------------------------------------------

interface

uses Classes, SysUtils, FGL, laz2_XMLRead, laz2_DOM, laz2_XMLWrite, Types,
  UWSystem.StrUtils;

type

  { TUWTBXHeader }

  PUWTBXHeader = ^TUWTBXHeader;
  TUWTBXHeader = record
    lang     : String;
    fileDesc : String;
  end;

  { TUWTBXItem }

  PUWTBXItem = ^TUWTBXItem;
  TUWTBXItem = record
    Original,
    Translated,
    Notes: String;
  end;

  { TUWTBXItemInfo }

  PUWTBXItemInfo = ^TUWTBXItemInfo;
  TUWTBXItemInfo = record
    SrcLang,         // "en"
    DstLang: String; // "es"
  end;

  { TUWTBXList }

  TUWTBXList = specialize TFPGList<PUWTBXItem>;

  { TUWTBX }

  TUWTBX = class
  private
    FFileName  : String;
    FChanged   : Boolean;
    FHeader    : TUWTBXHeader;
    FLangs     : TUWTBXItemInfo;
    FList      : TUWTBXList;
    FFoundList :  TIntegerDynArray;
    function GetLangPointer: PUWTBXItemInfo;
    function GetHeaderPointer: PUWTBXHeader;
  public
    constructor Create(const AFileName: String; ASrcLang: String = ''; ADstLang: String = '');
    destructor Destroy; override;
    procedure Clear;
    procedure Close;
    procedure LoadFromFile(const AFileName: String);
    function SaveToFile(const AFileName: String): Boolean;
    function AddItem(const Original, Translated, Notes: String; const AllowDuplicate: Boolean = False): Integer;
    function Ready: Boolean;
    function Find(AText: String): Integer;
    function FindAllTerms(const AText: String): Boolean;
    property FileName  : String           read FFileName write FFileName;
    property Items     : TUWTBXList       read FList;
    property Langs     : PUWTBXItemInfo   read GetLangPointer;
    property Header    : PUWTBXHeader     read GetHeaderPointer;
    property FoundList : TIntegerDynArray read FFoundList;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Utils;

// -----------------------------------------------------------------------------

{ Helpers}

// -----------------------------------------------------------------------------

function ListCompare(const Item1, Item2: PUWTBXItem): Integer;
begin
  if Item1^.Original < Item2^.Original then
    Result := -1
  else if Item1^.Original > Item2^.Original then
    Result := 1
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

{ TUWTBX }

// -----------------------------------------------------------------------------

constructor TUWTBX.Create(const AFileName: String; ASrcLang: String = ''; ADstLang: String = '');
begin
  FillByte(FHeader, SizeOf(TUWTBXHeader), 0);
  if ASrcLang.IsEmpty then ASrcLang := 'en';
  if ADstLang.IsEmpty then ADstLang := 'es';
  FLangs.SrcLang := ASrcLang;
  FLangs.DstLang := ADstLang;

  FList := TUWTBXList.Create;

  FFileName := '';
  FChanged  := False;
  SetLength(FFoundList, 0);

  if AFileName <> '' then LoadFromFile(AFileName);
end;

// -----------------------------------------------------------------------------

destructor TUWTBX.Destroy;
begin
  Close;
  FList.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWTBX.Clear;
var
  i: Integer;
begin
  if FList.Count > 0 then
  begin
    for i := FList.Count-1 downto 0 do
    begin
      Dispose(PUWTBXItem(FList.Items[i]));
      FList.Items[i] := NIL;
      FList.Delete(i);
    end;
  end;

  FLangs.SrcLang := '';
  FLangs.DstLang := '';
  SetLength(FFoundList, 0);
end;

// -----------------------------------------------------------------------------

procedure TUWTBX.Close;
begin
  if FChanged then
    SaveToFile(FFileName);

  Clear;
end;

// -----------------------------------------------------------------------------

procedure TUWTBX.LoadFromFile(const AFileName: String);
var
  XmlDoc: TXMLDocument;
  NodeTU, NodeTUV,
  NodeID, NodeText: TDOMNode;
  Item: PUWTBXItem;
begin
  FFileName := AFileName;
  if not FileExists(AFileName) then Exit;

  XmlDoc := NIL;
  ReadXMLFile(XmlDoc, AFileName);
  if Assigned(XmlDoc) then
  try
    // Header
    NodeID := XMLFindNodeByName(XmlDoc, 'martif');
    if NodeID <> NIL then
      with FHeader, NodeID.Attributes do
      begin
        if (GetNamedItem('xml:lang') <> NIL) and (FHeader.lang = '')  then
        begin
          FHeader.lang := GetNamedItem('xml:lang').NodeValue;
        end;
      end;

    NodeID := XMLFindNodeByName(XmlDoc, 'fileDesc');
    if NodeID <> NIL then
      with FHeader, NodeID.Attributes do
      begin
        if GetNamedItem('fileDesc') <> NIL then
          FHeader.fileDesc := GetNamedItem('fileDesc').NodeValue;

        if FHeader.lang <> '' then FLangs.SrcLang := FHeader.lang;
      end;

    // Terms
    Item := NIL;
    NodeTU := XMLFindNodeByName(XmlDoc, 'termEntry');
    while NodeTU <> NIL do
    begin
      New(Item);
      FillByte(Item[0], SizeOf(PUWTBXItem), 0);

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
              if NodeText.NodeName = 'tig' then
                if NodeText.FirstChild <> NIL then
                  with NodeText.FirstChild do
                    if (NodeName = 'term') then
                      Item^.Original := ReplaceEnters(TextContent, '|', sLineBreak);

              NodeText := NodeText.NextSibling;
            end;
          end
          else if (NodeID.NodeValue = FLangs.DstLang) then
          begin // dst
            NodeText := NodeTUV.FirstChild;
            while NodeText <> NIL do
            begin
              if NodeText.NodeName = 'tig' then
                if NodeText.FirstChild  <> NIL then
                  with NodeText.FirstChild do
                    if (NodeName = 'term') then
                      Item^.Translated := ReplaceEnters(TextContent, '|', sLineBreak)
                    else if (NodeName = 'termNote') then
                      Item^.Notes := TextContent;

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

function TUWTBX.SaveToFile(const AFileName: String): Boolean;
var
  XmlDoc : TXMLDocument;
  Root, Element, Node, SubNode, NodeTerm : TDOMNode;
  i : Integer;
begin
  Result := False;
  if FList.Count = 0 then
    Exit
  else if not AFileName.IsEmpty then
    FFileName := AFileName;

  XmlDoc := TXMLDocument.Create;
  try
    Root := XmlDoc.CreateElement('martif');
      TDOMElement(Root).SetAttribute('type', 'TBX');
      TDOMElement(Root).SetAttribute('xml:lang', FHeader.lang);
      XmlDoc.Appendchild(Root);
    Root := XmlDoc.DocumentElement;

    Element := XmlDoc.CreateElement('martifHeader');
      Node  := XmlDoc.CreateElement('fileDesc');
      Node.TextContent := FHeader.fileDesc;
    Element.AppendChild(Node);
      Node := XmlDoc.CreateElement('encodingDesc');
        SubNode := XmlDoc.CreateElement('p');
        TDOMElement(SubNode).SetAttribute('type', 'DCSName');
        SubNode.TextContent := 'tbxdefault.xcs';
      Node.AppendChild(Node);
    Element.AppendChild(Node);

    Element := XmlDoc.CreateElement('text');
    Root.AppendChild(Element);
    Root := Element; // text

    Element := XmlDoc.CreateElement('body');
    Root.AppendChild(Element);
    Root := Element; // body

    for i := 0 to FList.Count-1 do
    begin
      Node := XmlDoc.CreateElement('termEntry');
      Root.AppendChild(Node);

      // original
      Element := XmlDoc.CreateElement('LangSet');
      TDOMElement(Element).SetAttribute('xml:lang', FLangs.SrcLang);
        SubNode := XmlDoc.CreateElement('tig');
          NodeTerm := XmlDoc.CreateElement('term');
          NodeTerm.TextContent := ReplaceEnters(FList[i]^.Original);
        SubNode.AppendChild(NodeTerm); // tig
      Element.AppendChild(SubNode); // LangSet
      Node.AppendChild(Element); // termEntry

      // translated
      Element := XmlDoc.CreateElement('LangSet');
      TDOMElement(Element).SetAttribute('xml:lang', FLangs.DstLang);
        SubNode := XmlDoc.CreateElement('tig');
          NodeTerm := XmlDoc.CreateElement('term');
          NodeTerm.TextContent := ReplaceEnters(FList[i]^.Translated);
        SubNode.AppendChild(NodeTerm); // tig
      Element.AppendChild(SubNode); // LangSet
      if FList[i]^.Notes <> '' then
      begin
        SubNode := XmlDoc.CreateElement('note');
        SubNode.TextContent := FList[i]^.Notes;
        Element.AppendChild(SubNode); // LangSet
      end;
      Node.AppendChild(Element); // termEntry
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

function TUWTBX.AddItem(const Original, Translated, Notes: String; const AllowDuplicate: Boolean = False): Integer;
var
  Item: PUWTBXItem;
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

function TUWTBX.GetLangPointer: PUWTBXItemInfo;
begin
  Result := PUWTBXItemInfo(@FLangs);
end;

// -----------------------------------------------------------------------------

function TUWTBX.GetHeaderPointer: PUWTBXHeader;
begin
  Result := PUWTBXHeader(@FHeader);
end;

// -----------------------------------------------------------------------------

function TUWTBX.Ready: Boolean;
begin
  Result := (FFileName <> '') and (FLangs.SrcLang <> '') and (FLangs.DstLang <> '');
end;

// -----------------------------------------------------------------------------

function TUWTBX.Find(AText: String): Integer;
var
  i: Integer;
begin
  Result := -1;

  if (FList.Count > 0) and (AText <> '') then
  begin
    AText := AText.ToLower;
    for i := 0 to FList.Count-1 do
      if (AText = FList.Items[i]^.Original.ToLower) then
      begin
        Result := i;
        Break;
      end;
  end;
end;

// -----------------------------------------------------------------------------

function TUWTBX.FindAllTerms(const AText: String): Boolean;
var
  i, c : Integer;
  w : TStringArray;
begin
  Result := False;
  SetLength(FFoundList, 0);

  if not AText.IsEmpty and Ready and (FList.Count > 0) then
  begin
    w := AText.Split([' ',',','.',';','/','\',':','''','"','`','(',')','[',']','{','}']);
    for i := 0 to High(w) do
    begin
      c := Find(w[i]);
      if (c > -1) then
      begin
        SetLength(FFoundList, Length(FFoundList)+1);
        FFoundList[High(FFoundList)] := c;
      end;
    end;
    Result := (Length(FFoundList) > 0);
  end;
end;

// -----------------------------------------------------------------------------

end.
