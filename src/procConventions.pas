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

unit procConventions;

// -----------------------------------------------------------------------------

interface

uses Classes, SysUtils, FGL, laz2_XMLRead, laz2_DOM, laz2_XMLWrite, LCLTranslator;

type

  { TProfileItem }

  PProfileItem = ^TProfileItem;
  TProfileItem = record
    Name               : String;
    NewSubtitleMs      : Cardinal;
    MaxLines           : Byte;
    MaxDuration        : Cardinal;
    MinDuration        : Cardinal;
    MinDurationPerWord : Cardinal;
    MinPause           : Cardinal;
    PauseInFrames      : Boolean;
    MaxCPS             : Cardinal;
    RepeatedTolerance  : Cardinal;
    WPM                : Cardinal;
    CPL                : Cardinal;
    RepeatableChars    : String;
    ProhibitedChars    : String;
    DotsOnSplit        : Boolean;
    AddHyphenSpace     : Boolean;
    CPSLineLenStrategy : String;
    ShotcutSnapArea    : Cardinal;
    ShotcutThreshold   : Cardinal;
    ShotcutInCues      : Cardinal;
    ShotcutOutCues     : Cardinal;
    Chaining           : Cardinal;
  end;

  { TProfileList }

  TProfileList = specialize TFPGList<PProfileItem>;

  { TProfiles }

  TProfiles = class
  private
    FFileName : String;
    FChanged  : Boolean;
    FList     : TProfileList;
  public
    constructor Create(const AFileName: String);
    destructor Destroy; override;
    procedure Clear;
    procedure Close;
    procedure LoadFromFile(const AFileName: String);
    function SaveToFile(const AFileName: String): Boolean;
    function AddItem(const AName: String; const ANewSubtitleMs, AMinDuration, AMinDurationPerWord, AMaxDuration, AMaxLines, AMinPause, AMaxCPS, AWPM, ACPL, AShotcutSnapArea, AShotcutThreshold, AShotcutInCues, AShotcutOutCues, AChaining: Cardinal; const APauseInFrames: Boolean; const ARepeatableChars, AProhibitedChars: String; const ADotsOnSplit: Boolean; const ACPSLineLenStrategy: String; const AUpdate: Boolean = False; const AUpdateIndex: Integer = -1): Integer;
    procedure UpdateItem(const AIndex: Integer; const AName: String; const ANewSubtitleMs, AMinDuration, AMinDurationPerWord, AMaxDuration, AMaxLines, AMinPause, AMaxCPS, AWPM, ACPL, AShotcutSnapArea, AShotcutThreshold, AShotcutInCues, AShotcutOutCues, AChaining: Cardinal; const APauseInFrames: Boolean; const ARepeatableChars, AProhibitedChars: String; const ADotsOnSplit: Boolean; const ACPSLineLenStrategy: String);
    function Ready: Boolean;
    procedure FillTStrings(const AStrings: TStrings; const AClear: Boolean = True);
    function FindItemIndex(const AName: String): Integer;
    function FindProfile(const AName: String): PProfileItem;
    procedure Sort;
    property FileName : String       read FFileName write FFileName;
    property Items    : TProfileList read FList;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Utils, LazUTF8;

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function CompareItems(const Item1, Item2: PProfileItem): Integer;
begin
  Result := CompareText(Item1^.Name, Item2^.Name);
end;

// -----------------------------------------------------------------------------

{ TProfiles }

// -----------------------------------------------------------------------------

constructor TProfiles.Create(const AFileName: String);
begin
  FList     := TProfileList.Create;
  FFileName := AFileName;
  FChanged  := False;
  if AFileName <> '' then LoadFromFile(AFileName);
end;

// -----------------------------------------------------------------------------

destructor TProfiles.Destroy;
begin
  Close;
  FList.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TProfiles.Clear;
var
  i: Integer;
begin
  if FList.Count > 0 then
  begin
    for i := FList.Count-1 downto 0 do
    begin
      Dispose(PProfileItem(FList.Items[i]));
      FList.Items[i] := NIL;
      FList.Delete(i);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TProfiles.Close;
begin
  if FChanged then
    SaveToFile(FFileName);

  Clear;
end;

// -----------------------------------------------------------------------------

procedure TProfiles.LoadFromFile(const AFileName: String);

  function TryGetNamedItem(const ANode: TDOMNode; const AName, ADefault: DOMString): DOMString;
  var
    ANodeItem: TDOMNode;
  begin
    Result := ADefault;
    if (ANode <> NIL) then
    begin
      ANodeItem := ANode.Attributes.GetNamedItem(AName);
      if ANodeItem <> NIL then
        Result := ANodeItem.NodeValue;
    end;
  end;

var
  XmlDoc: TXMLDocument;
  NodeParent,
  NodeProfile: TDOMNode;
  Item: PProfileItem;
begin
  FFileName := AFileName;
  if not FileExists(AFileName) then Exit;

  XmlDoc := NIL;
  ReadXMLFile(XmlDoc, AFileName);
  if Assigned(XmlDoc) then
  try
    Item := NIL;
    NodeParent := XMLFindNodeByName(XmlDoc, 'conventions');
    if NodeParent <> NIL then
    begin
      NodeProfile := NodeParent.FirstChild;
      while NodeProfile <> NIL do
      begin
        New(Item);
        FillByte(Item[0], SizeOf(PProfileItem), 0);
        try
          with Item^ do
          begin
            Name               := TryGetNamedItem(NodeProfile, 'name', '');
            NewSubtitleMs      := TryGetNamedItem(NodeProfile, 'NewSubtitleMs', '1000').ToInteger;
            MinDuration        := TryGetNamedItem(NodeProfile, 'MinDuration', '1000').ToInteger;
            MinDurationPerWord := TryGetNamedItem(NodeProfile, 'MinDurationPerWord', '300').ToInteger;
            MaxDuration        := TryGetNamedItem(NodeProfile, 'MaxDuration', '8000').ToInteger;
            MaxLines           := TryGetNamedItem(NodeProfile, 'MaxLines', '2').ToInteger;
            MinPause           := TryGetNamedItem(NodeProfile, 'MinPause', '200').ToInteger;
            PauseInFrames      := TryGetNamedItem(NodeProfile, 'PauseInFrames', '0').ToBoolean;
            MaxCPS             := TryGetNamedItem(NodeProfile, 'MaxCPS', '15').ToInteger;
            WPM                := TryGetNamedItem(NodeProfile, 'WPM', '180').ToInteger;
            CPL                := TryGetNamedItem(NodeProfile, 'CPL', '37').ToInteger;
            RepeatableChars    := TryGetNamedItem(NodeProfile, 'RepeatableChars', '');
            ProhibitedChars    := TryGetNamedItem(NodeProfile, 'ProhibitedChars', '');
            DotsOnSplit        := TryGetNamedItem(NodeProfile, 'DotsOnSplit', '1').ToBoolean;
            CPSLineLenStrategy := TryGetNamedItem(NodeProfile, 'CPSLineLenStrategy', '');
            ShotcutSnapArea    := TryGetNamedItem(NodeProfile, 'ShotcutSnapArea', '7').ToInteger;
            ShotcutThreshold   := TryGetNamedItem(NodeProfile, 'ShotcutThreshold', '12').ToInteger;
            ShotcutInCues      := TryGetNamedItem(NodeProfile, 'ShotcutInCues', '0').ToInteger;
            ShotcutOutCues     := TryGetNamedItem(NodeProfile, 'ShotcutOutCues', '2').ToInteger;
            Chaining           := TryGetNamedItem(NodeProfile, 'Chaining', '12').ToInteger;
          end;
          FList.Add(Item);
        except
        end;
        NodeProfile := NodeProfile.NextSibling;
      end;
      Sort;
    end;
  finally
    XmlDoc.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TProfiles.SaveToFile(const AFileName: String): Boolean;
var
  XmlDoc: TXMLDocument;
  Root, Node: TDOMNode;
  i: Integer;
begin
  Result := False;
  if FList.Count = 0 then
    Exit
  else if not AFileName.IsEmpty then
    FFileName := AFileName;

  XmlDoc := TXMLDocument.Create;
  try
    Root := XmlDoc.CreateElement('conventions');
    XmlDoc.Appendchild(Root);
    Root := XmlDoc.DocumentElement;

    for i := 0 to FList.Count-1 do
    begin
      Node := XmlDoc.CreateElement('profile');
      with FList[i]^ do
      begin
        TDOMElement(Node).SetAttribute('name', Name);
        TDOMElement(Node).SetAttribute('NewSubtitleMs', NewSubtitleMs.ToString);
        TDOMElement(Node).SetAttribute('MinDuration', MinDuration.ToString);
        TDOMElement(Node).SetAttribute('MinDurationPerWord', MinDurationPerWord.ToString);
        TDOMElement(Node).SetAttribute('MaxDuration', MaxDuration.ToString);
        TDOMElement(Node).SetAttribute('MaxLines', MaxLines.ToString);
        TDOMElement(Node).SetAttribute('MinPause', MinPause.ToString);
        TDOMElement(Node).SetAttribute('PauseInFrames', PauseInFrames.ToString);
        TDOMElement(Node).SetAttribute('MaxCPS', MaxCPS.ToString);
        TDOMElement(Node).SetAttribute('WPM', WPM.ToString);
        TDOMElement(Node).SetAttribute('CPL', CPL.ToString);
        TDOMElement(Node).SetAttribute('RepeatableChars', RepeatableChars);
        TDOMElement(Node).SetAttribute('ProhibitedChars', ProhibitedChars);
        TDOMElement(Node).SetAttribute('DotsOnSplit', DotsOnSplit.ToString);
        TDOMElement(Node).SetAttribute('CPSLineLenStrategy', CPSLineLenStrategy);
        TDOMElement(Node).SetAttribute('ShotcutSnapArea', ShotcutSnapArea.ToString);
        TDOMElement(Node).SetAttribute('ShotcutThreshold', ShotcutThreshold.ToString);
        TDOMElement(Node).SetAttribute('ShotcutInCues', ShotcutInCues.ToString);
        TDOMElement(Node).SetAttribute('ShotcutOutCues', ShotcutOutCues.ToString);
        TDOMElement(Node).SetAttribute('Chaining', Chaining.ToString);
      end;
      Root.AppendChild(Node);
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

function TProfiles.AddItem(const AName: String; const ANewSubtitleMs, AMinDuration, AMinDurationPerWord, AMaxDuration, AMaxLines, AMinPause, AMaxCPS, AWPM, ACPL, AShotcutSnapArea, AShotcutThreshold, AShotcutInCues, AShotcutOutCues, AChaining: Cardinal; const APauseInFrames: Boolean; const ARepeatableChars, AProhibitedChars: String; const ADotsOnSplit: Boolean; const ACPSLineLenStrategy: String; const AUpdate: Boolean = False; const AUpdateIndex: Integer = -1): Integer;
var
  s : String;
  Item : PProfileItem;
  i : Integer;
begin
  Result := -1;

  s := UTF8LowerCase(AName);

  for i := 0 to FList.Count-1 do
    if UTF8LowerCase(FList.Items[i]^.Name) = s then
      if not AUpdate or (i <> AUpdateIndex) then
        Exit;

  if not AUpdate then
    New(Item)
  else
    Item := FList.Items[AUpdateIndex];

  with Item^ do
  begin
    Name               := AName;
    RepeatableChars    := ARepeatableChars;
    ProhibitedChars    := AProhibitedChars;
    NewSubtitleMs      := ANewSubtitleMs;
    MinDuration        := AMinDuration;
    MinDurationPerWord := AMinDurationPerWord;
    MaxDuration        := AMaxDuration;
    MaxLines           := AMaxLines;
    MinPause           := AMinPause;
    PauseInFrames      := APauseInFrames;
    MaxCPS             := AMaxCPS;
    WPM                := AWPM;
    CPL                := ACPL;
    DotsOnSplit        := ADotsOnSplit;
    CPSLineLenStrategy := ACPSLineLenStrategy;
    ShotcutSnapArea    := AShotcutSnapArea;
    ShotcutThreshold   := AShotcutThreshold;
    ShotcutInCues      := AShotcutInCues;
    ShotcutOutCues     := AShotcutOutCues;
    Chaining           := AChaining;
  end;

  if not AUpdate then
  begin
    FList.Add(Item);
    Sort;
    Result := FList.IndexOf(Item);
  end;

  FChanged := True;
end;

// -----------------------------------------------------------------------------

procedure TProfiles.UpdateItem(const AIndex: Integer; const AName: String; const ANewSubtitleMs, AMinDuration, AMinDurationPerWord, AMaxDuration, AMaxLines, AMinPause, AMaxCPS, AWPM, ACPL, AShotcutSnapArea, AShotcutThreshold, AShotcutInCues, AShotcutOutCues, AChaining: Cardinal; const APauseInFrames: Boolean; const ARepeatableChars, AProhibitedChars: String; const ADotsOnSplit: Boolean; const ACPSLineLenStrategy: String);
begin
  if FList.Count = 0 then Exit;

  AddItem(AName, ANewSubtitleMs, AMinDuration, AMinDurationPerWord, AMaxDuration,
    AMaxLines, AMinPause, AMaxCPS, AWPM, ACPL, AShotcutSnapArea, AShotcutThreshold, AShotcutInCues, AShotcutOutCues, AChaining,
    APauseInFrames, ARepeatableChars, AProhibitedChars, ADotsOnSplit, ACPSLineLenStrategy, True, AIndex);
end;

// -----------------------------------------------------------------------------

function TProfiles.Ready: Boolean;
begin
  Result := (FFileName <> '');
end;

// -----------------------------------------------------------------------------

procedure TProfiles.FillTStrings(const AStrings: TStrings; const AClear: Boolean = True);
var
  i: Integer;
begin
  if not Assigned(AStrings) or (FList.Count = 0) then Exit;

  if AClear then
    AStrings.Clear;

  for i := 0 to FList.Count-1 do
    AStrings.Add(FList.Items[i]^.Name);
end;

// -----------------------------------------------------------------------------

function TProfiles.FindItemIndex(const AName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  if (FList.Count = 0) then Exit;

  for i := 0 to FList.Count-1 do
    if AName = FList.Items[i]^.Name then
    begin
      Exit(i);
    end;
end;

// -----------------------------------------------------------------------------

function TProfiles.FindProfile(const AName: String): PProfileItem;
var
  i: Integer;
begin
  Result := NIL;
  if (FList.Count = 0) then Exit;

  for i := 0 to FList.Count-1 do
    if AName = FList.Items[i]^.Name then
    begin
      Result := FList.Items[i];
      Break;
    end;
end;

// -----------------------------------------------------------------------------

procedure TProfiles.Sort;
begin
  FList.Sort(@CompareItems);
end;

// -----------------------------------------------------------------------------

end.
