{*
 *  URUWorks Edit Decision Lists (EDL)
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

unit UWSubtitleAPI.EDL;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, FGL;

type

  { THeader }

  TFCM = (fcmNONDROPFRAME, fcmDROPFRAME);

  PHeader = ^THeader;
  THeader = record
    Title : String; // project title
    FCM   : TFCM;   // NON-DROP FRAME
  end;

  { TItem }

  TTypeOfTrack = (totAAV, totAA, totV);
  TTypeOfCut   = (tocC);

  PItem = ^TItem;
  TItem = record
    Index       : Cardinal;     // line event number
    ReelName    : String;       // reel name 7 digit abbreviation
    TypeOfTrack : TTypeOfTrack; // AA/V, AA, V
    TypeOfCut   : TTypeOfCut;   // C
    ClipIn,                     // clip in
    ClipOut,                    // clip out
    TapeIn,                     // tape/timeline in
    TapeOut     : Cardinal;     // tape/timeline out
    ClipName,                   // clip name
    Comment     : String;       // comments
  end;

  TEDLList = specialize TFPGList<PItem>;

  TEDL = class
  private
    FFileName : String;
    FHeader   : THeader;
    FList     : TEDLList;
    FChanged  : Boolean;
    FFPS      : Single;
  public
    constructor Create(const AFileName: String; const ATitle: String = ''; const AFPS: Single = 25; const AFCM: TFCM = fcmNONDROPFRAME);
    destructor Destroy; override;
    procedure Clear;
    procedure Close;
    function Ready: Boolean;
    procedure LoadFromFile(const AFileName: String);
    function SaveToFile(const AFileName: String): Boolean;
    function SaveToFileXML(const AFileName: String): Boolean;
    function AddItem(const AItem: TItem): Integer; overload;
    function AddItem(const AIndex: Cardinal; const AReelName: String; const ATypeOfTrack: TTypeOfTrack; const ATypeOfCut: TTypeOfCut; const AClipIn, AClipOut, ATapeIn, ATapeOut: Cardinal; const AClipName, AComment: String): Integer; overload;
    property FileName : String   read FFileName write FFileName;
    property Items    : TEDLList read FList;
    property Header   : THeader  read FHeader write FHeader;
  end;

// -----------------------------------------------------------------------------

implementation

uses
  UWSystem.TimeUtils, UWSystem.StrUtils, laz2_XMLRead, laz2_DOM, laz2_XMLWrite;

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function totToStr(const ATOT: TTypeOfTrack): String;
begin
  if ATOT = totAAV then
    Result := 'AAV'
  else if ATOT = totAA then
    Result := 'AA'
  else
    Result := 'V'
end;

// -----------------------------------------------------------------------------

function tocToStr(const ATOC: TTypeOfCut): String;
begin
  Result := 'C';
end;

// -----------------------------------------------------------------------------

constructor TEDL.Create(const AFileName: String; const ATitle: String = ''; const AFPS: Single = 25; const AFCM: TFCM = fcmNONDROPFRAME);
begin
  FillByte(FHeader, SizeOf(THeader), 0);
  FList := TEDLList.Create;

  FHeader.Title := ATitle;
  FHeader.FCM   := AFCM;

  FFPS      := AFPS;
  FChanged  := False;
  FFileName := AFileName;
  if AFileName <> '' then LoadFromFile(AFileName);
end;

// -----------------------------------------------------------------------------

destructor TEDL.Destroy;
begin
  Close;
  FList.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TEDL.Clear;
var
  i: Integer;
begin
  if FList.Count > 0 then
  begin
    for i := FList.Count-1 downto 0 do
    begin
      Dispose(PItem(FList.Items[i]));
      FList.Items[i] := NIL;
      FList.Delete(i);
    end;
  end;

  FHeader.Title := '';
  FHeader.FCM   := fcmNONDROPFRAME;
end;

// -----------------------------------------------------------------------------

procedure TEDL.Close;
begin
  if FChanged then
    SaveToFile(FFileName);

  Clear;
end;

// -----------------------------------------------------------------------------

function TEDL.Ready: Boolean;
begin
  Result := (FFileName <> '');
end;

// -----------------------------------------------------------------------------

procedure TEDL.LoadFromFile(const AFileName: String);

  function CopyAndDel(var AText: String; const ASpace: String): String;
  var
    z: Integer;
  begin
    z := Pos(ASpace, AText);
    Result := Copy(AText, 1, z-1);
    Delete(AText, 1, z);
  end;

var
  SL: TStrings;
  s, sx: String;
  x, i: Integer;
  Item: TItem;

  XmlDoc: TXMLDocument;
  Node, SubNode: TDOMNode;
  NodeList: TDOMNodeList;
begin
  if not FileExists(AFileName) then Exit;
  Clear;

  if AFileName.ToLower.EndsWith('.xml') then // XML
  begin
    XmlDoc := NIL;
    try
      ReadXMLFile(XmlDoc, AFileName);
    except
      Exit;
    end;

    if Assigned(XmlDoc) then
    try
      i := 0;
      NodeList := XmlDoc.GetElementsByTagName('clipitem');
      if Assigned(NodeList) and (NodeList.Count > 0) then
      begin
        with Item do
        begin
          while i < NodeList.Count do
          begin
            Node := NodeList.Item[i];
            SubNode := Node.FindNode('start');
            if Assigned(SubNode) then
            begin
              Index       := i+1;
              ReelName    := '';
              TypeOfTrack := totV;
              TypeOfCut   := tocC;
              ClipName    := '';
              Comment     := '';
              ClipIn      := 0;
              ClipOut     := FramesToTime(StrToIntDef(SubNode.TextContent, 0), FFPS);
              TapeIn      := 0;
              TapeOut     := 0;
              AddItem(Item);
            end;
            Inc(i);
          end;
        end;
      end;
    finally
      XmlDoc.Free;
    end;
  end
  else // TEXT
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(AFileName);
      if SL.Count < 2 then Exit;

      if SL[0].StartsWith('TITLE') then
      begin
        x := Pos(':', SL[0]);
        FHeader.Title := Trim(Copy(SL[0], x+1, SL[0].Length-x));
        SL.Delete(0);
      end;

      if SL[0].StartsWith('FCM') then
      begin
        x := Pos(' ', SL[0]);
        s := Copy(SL[0], x+1, SL[0].Length-x);
        if s = 'NON-DROP FRAME' then
          FHeader.FCM := fcmNONDROPFRAME
        else
          FHeader.FCM := fcmDROPFRAME;
        SL.Delete(0);
      end;

      for i := 0 to SL.Count-1 do
        with Item do
        begin
          s  := SL[i];
          sx := CopyAndDel(s, ' ');
          s  := s.Trim;

          Index := StrToIntDef(sx, 0);
          if Index = 0 then Continue;

          if sx.Length = 6 then // 000001
          begin
            ReelName := CopyAndDel(s, '|');
            ClipName := CopyAndDel(s, ' ');

            sx := CopyAndDel(s, ' ');
            if sx = 'V' then
              TypeOfTrack := totV
            else if sx.StartsWith('A') then
              TypeOfTrack := totAA
            else
              TypeOfTrack := totAAV;

            s := Trim(s);

            sx := CopyAndDel(s, ' ');
            if sx = 'C' then
              TypeOfCut := tocC;

            s := s.Trim;

            ClipIn  := StringToTime(CopyAndDel(s, ' '), False, FFPS);
            ClipOut := StringToTime(CopyAndDel(s, ' '), False, FFPS);
            TapeIn  := StringToTime(CopyAndDel(s, ' '), False, FFPS);
            TapeOut := StringToTime(s, False, FFPS);

            AddItem(Item);
          end
          else
          begin // 001
            sx := CopyAndDel(s, ' '); // AX
            s := s.Trim;

            sx := CopyAndDel(s, ' '); // AA...
            s := s.Trim;
            if sx = 'V' then
              TypeOfTrack := totV
            else if sx = 'AA' then
              TypeOfTrack := totAA
            else
              TypeOfTrack := totAAV;

            s := s.Trim;

            sx := CopyAndDel(s, ' '); // C
            if sx = 'C' then
              TypeOfCut := tocC;

            s := s.Trim;

            ClipIn  := StringToTime(CopyAndDel(s, ' '), False, FFPS);
            ClipOut := StringToTime(CopyAndDel(s, ' '), False, FFPS);
            TapeIn  := StringToTime(CopyAndDel(s, ' '), False, FFPS);
            TapeOut := StringToTime(s, False, FFPS);

            AddItem(Item);
          end;
        end;
    finally
      SL.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TEDL.SaveToFile(const AFileName: String): Boolean;
var
  SL: TStrings;
  i: Integer;
  s, f: String;
begin
  Result := False;
  if FList.Count = 0 then Exit;

  if AFileName.ToLower.EndsWith('.xml') then // XML
    SaveToFileXML(AFileName)
  else
  begin
    SL := TStringList.Create;
    try
      f := ExtractFileName(FHeader.Title);
      FHeader.Title := ChangeFileExt(f, '');

      SL.Add('TITLE : ' + FHeader.Title);
      for i := 0 to FList.Count-1 do
        with FList.Items[i]^ do
        begin
          s := Format('%s  %s|%s %s     %s        %s %s %s %s',
            [AddChar('0', Index.ToString, 6),
            ReplaceString(FHeader.Title, ' ', '_'),
            ReplaceString(f, ' ', '_'),
            totToStr(TypeOfTrack),
            tocToStr(TypeOfCut),
            TimeToString(ClipIn, 'hh:mm:ss:ff', FFPS),
            TimeToString(ClipOut, 'hh:mm:ss:ff', FFPS),
            TimeToString(ClipIn, 'hh:mm:ss:ff', FFPS),
            TimeToString(ClipOut, 'hh:mm:ss:ff', FFPS)]);

          SL.Add(s);
        end;
      SL.SaveToFile(AFileName);
      Result := True;
    finally
      SL.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TEDL.SaveToFileXML(const AFileName: String): Boolean;
var
  XmlDoc : TXMLDocument;
  Root, Element, Node, SubNode, XNode : TDOMNode;
  i : Integer;
begin
  Result := False;
  if FList.Count = 0 then Exit;

  XmlDoc := TXMLDocument.Create;
  try
//    XmlDoc.AppendChild(XmlDoc.CreateComment('!DOCTYPE xmeml'));

    Root := XmlDoc.CreateElement('xmeml');
      TDOMElement(Root).SetAttribute('version', '5');
      XmlDoc.Appendchild(Root);
    Root := XmlDoc.DocumentElement;

    Element := XmlDoc.CreateElement('sequence');
      TDOMElement(Element).SetAttribute('id', 'Sequence 1 ');
//      Node := XmlDoc.CreateElement('uuid');
//      Node.TextContent := '';
//      Element.AppendChild(Node);
      Node := XmlDoc.CreateElement('updatebehavior');
      Node.TextContent := 'add';
      Element.AppendChild(Node);
      Node := XmlDoc.CreateElement('name');
      Node.TextContent := 'Sequence 1';
      Element.AppendChild(Node);
//      Node := XmlDoc.CreateElement('duration');
//      Node.TextContent := '';
//      Element.AppendChild(Node);
      Node := XmlDoc.CreateElement('in');
      Node.TextContent := '-1';
      Element.AppendChild(Node);
      Node := XmlDoc.CreateElement('out');
      Node.TextContent := '-1';
      Element.AppendChild(Node);
      Node := XmlDoc.CreateElement('ismasterclip');
      Node.TextContent := 'FALSE';
      Element.AppendChild(Node);
      Node := XmlDoc.CreateElement('rate');
        SubNode := XmlDoc.CreateElement('ntsc');
        SubNode.TextContent := 'TRUE';
      Node.AppendChild(SubNode);
//      SubNode := XmlDoc.CreateElement('timebase');
//        SubNode.TextContent := '';
//      Node.AppendChild(SubNode);
      Element.AppendChild(Node);
      Node := XmlDoc.CreateElement('media');
      Element.AppendChild(Node);
      SubNode := XmlDoc.CreateElement('video');
      Node.AppendChild(SubNode);
      Node := XmlDoc.CreateElement('track'); // Node = TRACK
      SubNode.AppendChild(Node);
      //
    Root.AppendChild(Element);

    for i := 0 to FList.Count-1 do
      with FList.Items[i]^ do
      begin
        SubNode := XmlDoc.CreateElement('clipitem');
          XNode := XmlDoc.CreateElement('start');
          XNode.TextContent := TimeToFrames(ClipOut, FFPS).ToString;
        SubNode.AppendChild(XNode);
        Node.AppendChild(SubNode);
      end;
    try
      WriteXML(XmlDoc, AFileName);
      Result := True;
    except
    end;
  finally
    XmlDoc.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TEDL.AddItem(const AItem: TItem): Integer;
var
  Item: PItem;
begin
  New(Item);
  with Item^ do
  begin
    Index       := AItem.Index;
    ReelName    := AItem.ReelName;
    TypeOfTrack := AItem.TypeOfTrack;
    TypeOfCut   := AItem.TypeOfCut;
    ClipIn      := AItem.ClipIn;
    ClipOut     := AItem.ClipOut;
    TapeIn      := AItem.TapeIn;
    TapeOut     := AItem.TapeOut;
    ClipName    := AItem.ClipName;
    Comment     := AItem.Comment;
  end;
  Result := FList.Add(Item)+1;
end;

// -----------------------------------------------------------------------------

function TEDL.AddItem(const AIndex: Cardinal; const AReelName: String; const ATypeOfTrack: TTypeOfTrack; const ATypeOfCut: TTypeOfCut; const AClipIn, AClipOut, ATapeIn, ATapeOut: Cardinal; const AClipName, AComment: String): Integer;
var
  Item: TItem;
begin
  with Item do
  begin
    Index       := AIndex;
    ReelName    := AReelName;
    TypeOfTrack := ATypeOfTrack;
    TypeOfCut   := ATypeOfCut;
    ClipIn      := AClipIn;
    ClipOut     := AClipOut;
    TapeIn      := ATapeIn;
    TapeOut     := ATapeOut;
    ClipName    := AClipName;
    Comment     := AComment;
  end;
  Result := AddItem(Item);
end;

// -----------------------------------------------------------------------------

end.

