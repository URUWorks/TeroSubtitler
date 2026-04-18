{*
 * URUWorks Subtitle API
 *
 * The contents of this file are used with permission, subject to
 * the Mozilla Public License Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/2.0.html
 *
 * Software distributed under the License is distributed on an
 * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * Copyright (C) 2001-2023 URUWorks, uruworks@gmail.com.
 *}

unit UWSubtitleAPI.Formats.ITunesTimedText;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, StrUtils,
  UWSubtitleAPI.Formats, laz2_XMLRead, laz2_DOM; // laz2_XMLWrite;

type

  { TUWITunesTimedText }

  TUWITunesTimedText = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function IsTimeBased: Boolean; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Tags, UWSubtitleAPI.Utils, UWSystem.StrUtils,
  UWSystem.SysUtils;

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

procedure AppendStyledTextDirect(XmlDoc: TXMLDocument; Parent: TDOMNode; const Txt: string);
var
  i: Integer;
  Buffer: String;
  Stack: array of TDOMElement;
  CurrentNode: TDOMNode;

  procedure FlushText;
  begin
    if Buffer <> '' then
    begin
      CurrentNode.AppendChild(XmlDoc.CreateTextNode(Buffer));
      Buffer := '';
    end;
  end;

  procedure PushSpan(const StyleName: string);
  var
    Span: TDOMElement;
  begin
    Span := XmlDoc.CreateElement('span');
    Span.SetAttribute('style', StyleName);
    CurrentNode.AppendChild(Span);

    SetLength(Stack, Length(Stack) + 1);
    Stack[High(Stack)] := Span;
    CurrentNode := Span;
  end;

  procedure PushColor(const ColorHex: string);
  var
    Span: TDOMElement;
  begin
    Span := XmlDoc.CreateElement('span');
    Span.SetAttribute('tts:color', ColorHex);
    CurrentNode.AppendChild(Span);

    SetLength(Stack, Length(Stack) + 1);
    Stack[High(Stack)] := Span;
    CurrentNode := Span;
  end;

  procedure PopSpan;
  begin
    if Length(Stack) > 0 then
    begin
      SetLength(Stack, Length(Stack) - 1);
      if Length(Stack) > 0 then
        CurrentNode := Stack[High(Stack)]
      else
        CurrentNode := Parent;
    end;
  end;

  function MatchTag(const S: string; Pos: Integer; const Tag: string): Boolean;
  begin
    Result := Copy(S, Pos, Length(Tag)) = Tag;
  end;

  function ParseASSColor(const S: string; var Advance: Integer): string;
  // \c&HBBGGRR&
  var
    tmp: string;
    b, g, r: string;
  begin
    Result := '';
    Advance := 0;

    if (Length(S) < 10) then Exit;

    // &H......&
    if (S[1] = '\') and ((S[2] = 'c') or (S[2] = '1')) then
    begin
      if Pos('&H', S) > 0 then
      begin
        tmp := Copy(S, Pos('&H', S) + 2, 6); // BBGGRR

        if Length(tmp) = 6 then
        begin
          b := Copy(tmp, 1, 2);
          g := Copy(tmp, 3, 2);
          r := Copy(tmp, 5, 2);

          Result := '#' + r + g + b;
        end;

        Advance := Pos('&', Copy(S, Pos('&H', S) + 2, 10)) + Pos('&H', S) + 1;
      end;
    end;
  end;

var
  TagContent, ColorHex: String;
  j, Adv: Integer;
begin
  Buffer := '';
  CurrentNode := Parent;
  SetLength(Stack, 0);

  i := 1;
  while i <= Length(Txt) do
  begin
    // TAG BLOCK { ... }
    if Txt[i] = '{' then
    begin
      FlushText;

      // buscamos cierre
      j := i + 1;
      while (j <= Length(Txt)) and (Txt[j] <> '}') do Inc(j);

      if j > Length(Txt) then Break;

      TagContent := Copy(Txt, i + 1, j - i - 1);

      // estilos
      if TagContent = '\b1' then PushSpan('bold')
      else if TagContent = '\b0' then PopSpan
      else if TagContent = '\i1' then PushSpan('italic')
      else if TagContent = '\i0' then PopSpan
      else if TagContent = '\u1' then PushSpan('underline')
      else if TagContent = '\u0' then PopSpan
      else if TagContent = '\s1' then PushSpan('strikeout')
      else if TagContent = '\s0' then PopSpan
      else
      begin
        // color
        ColorHex := ParseASSColor(TagContent, Adv);
        if ColorHex <> '' then
          PushColor(ColorHex);
      end;

      i := j + 1;
      Continue;
    end;

    // SALTO
    if Txt[i] = #10 then
    begin
      FlushText;
      CurrentNode.AppendChild(XmlDoc.CreateElement('br'));
      Inc(i);
      Continue;
    end;

    // TEXTO
    Buffer += Txt[i];
    Inc(i);
  end;

  FlushText;
end;

// -----------------------------------------------------------------------------

function TUWITunesTimedText.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWITunesTimedText.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfITunesTimedText;
end;

// -----------------------------------------------------------------------------

function TUWITunesTimedText.Extension: String;
begin
  Result := '*.itt';
end;

// -----------------------------------------------------------------------------

function TUWITunesTimedText.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWITunesTimedText.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWITunesTimedText.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (LowerCase(ExtractFileExt(SubtitleFile.FileName)) = '.itt') and
  SubtitleFile[Row].Contains('<tt xml') then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWITunesTimedText.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  XmlDoc : TXMLDocument;
  Node, LayoutNode, RegionNode : TDOMNode;
  Item   : TUWSubtitleItem;
  fr     : Double;
  RegionMap: TStringList;
  RegionID, AlignVal: String;
begin
  Result := False;
  XmlDoc := NIL;
  RegionMap := TStringList.Create;

  StringsToXML(XmlDoc, SubtitleFile);
  if Assigned(XmlDoc) then
    try
      Node := XMLFindNodeByName(XmlDoc, 'tt');
      Subtitles.FrameRate := FPS;

      if Assigned(Node) then
      begin
        if XMLHasAttribute(Node, 'ttp:frameRate') then
        begin
          fr := StrToFloatDef(XMLGetAttrValue(Node, 'ttp:frameRate'), FPS);
          Subtitles.FrameRate := fr;
        end;

        if XMLHasAttribute(Node, 'ttp:timeBase') and XMLHasAttribute(Node, 'ttp:dropMode') then
        begin
          if (XMLGetAttrValue(Node, 'ttp:dropMode') <> 'nonDrop') then
            Subtitles.TimecodeMode := tcDF
          else
            Subtitles.TimecodeMode := tcNDF;

          if (XMLGetAttrValue(Node, 'ttp:timeBase') = 'smpte') then
            Subtitles.TimeBase := stbSMPTE
          else
            Subtitles.TimeBase := stbMedia;
        end;

        if XMLHasAttribute(Node, 'ttp:frameRateMultiplier') then
          Subtitles.FrameRate := XMLGetCorrectFPS(fr, XMLGetAttrValue(Node, 'ttp:frameRateMultiplier'), Subtitles.TimecodeMode = tcDF);
      end;

      LayoutNode := XMLFindNodeByName(XmlDoc, 'layout');
      if Assigned(LayoutNode) then
      begin
        RegionNode := LayoutNode.FirstChild;
        while Assigned(RegionNode) do
        begin
          if RegionNode.NodeName = 'region' then
          begin
            if XMLHasAttribute(RegionNode, 'xml:id') then
            begin
              RegionID := XMLGetAttrValue(RegionNode, 'xml:id');

              if XMLHasAttribute(RegionNode, 'tts:displayAlign') then
                AlignVal := XMLGetAttrValue(RegionNode, 'tts:displayAlign')
              else
                AlignVal := '';

              RegionMap.Values[RegionID] := AlignVal;
            end;
          end;
          RegionNode := RegionNode.NextSibling;
        end;
      end;

      Node := XMLFindNodeByName(XmlDoc, 'p');
      if Assigned(Node) then
        repeat
          if Node.HasAttributes then
          begin
            ClearSubtitleItem(Item);
            with Item do
            begin
              if Node.Attributes.GetNamedItem('region') <> NIL then
              begin
                RegionID := Node.Attributes.GetNamedItem('region').NodeValue;
                AlignVal := RegionMap.Values[RegionID];

                // tts:displayAlign
                if SameText(AlignVal, 'before') then
                  VAlign := svaTop
                else if SameText(AlignVal, 'center') then
                  VAlign := svaCenter
                else if SameText(AlignVal, 'after') then
                  VAlign := svaBottom
                // no existe tts:displayAlign
                else if LowerCase(RegionID).Contains('top') or (RegionID = 'sh0') then
                  VAlign := svaTop
                else if LowerCase(RegionID).Contains('middle') or LowerCase(RegionID).Contains('center') then
                  VAlign := svaCenter
                else
                  VAlign := svaBottom;
              end
              else
                VAlign := svaBottom;

              if Node.Attributes.GetNamedItem('begin') <> NIL then
                InitialTime := SMPTEStringToMS(Node.Attributes.GetNamedItem('begin').NodeValue, Subtitles.FrameRate);
              if Node.Attributes.GetNamedItem('end') <> NIL then
                FinalTime   := SMPTEStringToMS(Node.Attributes.GetNamedItem('end').NodeValue, Subtitles.FrameRate);

              Text := XMLExtractTextContent(Node.ChildNodes);
              Text := HTMLTagsToTS(ReplaceEnters(Text, '<br/>', LineEnding));
            end;
            Subtitles.Add(Item, NIL);
          end;

          Node := Node.NextSibling;
        until (Node = NIL);
    finally
       RegionMap.Free;
       XmlDoc.Free;
       Result := Subtitles.Count > 0;
    end;
end;

// -----------------------------------------------------------------------------

function TUWITunesTimedText.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  XmlDoc : TXMLDocument;
  Root, Element, Node, SubNode : TDOMNode;
  i, it, ft : Integer;
  NewFPS: Double;
  txt, RegionStr: String;
begin
  Result := False;
  XmlDoc := TXMLDocument.Create;
  try
    Root := XmlDoc.CreateElement('tt');
      TDOMElement(Root).SetAttribute('xmlns', 'http://www.w3.org/ns/ttml');
      TDOMElement(Root).SetAttribute('xmlns:ttp', 'http://www.w3.org/ns/ttml#parameter');
      TDOMElement(Root).SetAttribute('xmlns:tts', 'http://www.w3.org/ns/ttml#style');
      TDOMElement(Root).SetAttribute('xmlns:ttm', 'http://www.w3.org/ns/ttml#metadata');
      TDOMElement(Root).SetAttribute('xml:lang', 'en');

      if Subtitles.TimeBase = stbSMPTE then
        TDOMElement(Root).SetAttribute('ttp:timeBase', 'smpte')
      else
        TDOMElement(Root).SetAttribute('ttp:timeBase', 'media');

      if IsInteger(FPS) then
      begin
        NewFPS := FPS;
        TDOMElement(Root).SetAttribute('ttp:frameRate', Trunc(NewFPS).ToString);
        TDOMElement(Root).SetAttribute('ttp:frameRateMultiplier', '1 1');
      end
      else
      begin
        NewFPS := Round(FPS) * (1000 /  1001);
        TDOMElement(Root).SetAttribute('ttp:frameRate', Round(NewFPS).ToString);
        TDOMElement(Root).SetAttribute('ttp:frameRateMultiplier', '1000 1001');
      end;

      if Subtitles.TimecodeMode = tcDF then
        TDOMElement(Root).SetAttribute('ttp:dropMode', 'dropNTSC')
      else
        TDOMElement(Root).SetAttribute('ttp:dropMode', 'nonDrop');

      XmlDoc.Appendchild(Root);
    Root := XmlDoc.DocumentElement;

    Element := XmlDoc.CreateElement('head');
      Node := XmlDoc.CreateElement('metadata');
      Element.AppendChild(Node);
      SubNode := XmlDoc.CreateElement('ttm:title');
      Node.AppendChild(SubNode);
    Root.AppendChild(Element);
      Node := XmlDoc.CreateElement('styling');
      Element.AppendChild(Node);
      SubNode := XmlDoc.CreateElement('style');
      TDOMElement(SubNode).SetAttribute('xml:id', 'normal');
      TDOMElement(SubNode).SetAttribute('tts:fontStyle', 'normal');
      TDOMElement(SubNode).SetAttribute('tts:fontSize', '100%');
      TDOMElement(SubNode).SetAttribute('tts:fontWeight', 'normal');
      TDOMElement(SubNode).SetAttribute('tts:fontFamily', 'sansSerif');
      TDOMElement(SubNode).SetAttribute('tts:color', 'white');
      Node.AppendChild(SubNode);
      SubNode := XmlDoc.CreateElement('style');
      TDOMElement(SubNode).SetAttribute('xml:id', 'italic');
      TDOMElement(SubNode).SetAttribute('tts:fontStyle', 'italic');
      TDOMElement(SubNode).SetAttribute('tts:fontSize', '100%');
      TDOMElement(SubNode).SetAttribute('tts:fontWeight', 'normal');
      TDOMElement(SubNode).SetAttribute('tts:fontFamily', 'sansSerif');
      TDOMElement(SubNode).SetAttribute('tts:color', 'white');
      Node.AppendChild(SubNode);
      SubNode := XmlDoc.CreateElement('style');
      TDOMElement(SubNode).SetAttribute('xml:id', 'bold');
      TDOMElement(SubNode).SetAttribute('tts:fontStyle', 'bold');
      TDOMElement(SubNode).SetAttribute('tts:fontSize', '100%');
      TDOMElement(SubNode).SetAttribute('tts:fontWeight', 'normal');
      TDOMElement(SubNode).SetAttribute('tts:fontFamily', 'sansSerif');
      TDOMElement(SubNode).SetAttribute('tts:color', 'white');
      Node.AppendChild(SubNode);
    Root.AppendChild(Element);

    Node := XmlDoc.CreateElement('layout');
    Element.AppendChild(Node);
      SubNode := XmlDoc.CreateElement('region');
      TDOMElement(SubNode).SetAttribute('xml:id', 'top');
      TDOMElement(SubNode).SetAttribute('tts:origin', '0% 0%');
      TDOMElement(SubNode).SetAttribute('tts:extent', '100% 15%');
      TDOMElement(SubNode).SetAttribute('tts:textAlign', 'center');
      TDOMElement(SubNode).SetAttribute('tts:displayAlign', 'before');
      Node.AppendChild(SubNode);

      SubNode := XmlDoc.CreateElement('region');
      TDOMElement(SubNode).SetAttribute('xml:id', 'middle');
      TDOMElement(SubNode).SetAttribute('tts:origin', '0% 40%');
      TDOMElement(SubNode).SetAttribute('tts:extent', '100% 20%');
      TDOMElement(SubNode).SetAttribute('tts:textAlign', 'center');
      TDOMElement(SubNode).SetAttribute('tts:displayAlign', 'center');
      Node.AppendChild(SubNode);

      SubNode := XmlDoc.CreateElement('region');
      TDOMElement(SubNode).SetAttribute('xml:id', 'bottom');
      TDOMElement(SubNode).SetAttribute('tts:origin', '0% 85%');
      TDOMElement(SubNode).SetAttribute('tts:extent', '100% 15%');
      TDOMElement(SubNode).SetAttribute('tts:textAlign', 'center');
      TDOMElement(SubNode).SetAttribute('tts:displayAlign', 'after');
      Node.AppendChild(SubNode);
    Root.AppendChild(Element);

    Element := XmlDoc.CreateElement('body');
      TDOMElement(Element).SetAttribute('style', 'normal');
    Root.AppendChild(Element);

    Node := XmlDoc.CreateElement('div');
    Element.AppendChild(Node);

    for i := FromItem to ToItem do
    begin
      it := Subtitles[i].InitialTime;
      ft := Subtitles[i].FinalTime;

      Element := XmlDoc.CreateElement('p');

      if Subtitles[i].VAlign = svaTop then
        RegionStr := 'top'
      else if Subtitles[i].VAlign = svaCenter then
        RegionStr := 'middle'
      else
        RegionStr := 'bottom';

      TDOMElement(Element).SetAttribute('xml:id', 'p' + IntToStr(i + 1)); // XML Estándar ID
      TDOMElement(Element).SetAttribute('region', RegionStr);
      TDOMElement(Element).SetAttribute('begin', MSToHHMMSSFFTime(it, NewFPS, Subtitles.TimecodeMode));
      TDOMElement(Element).SetAttribute('end', MSToHHMMSSFFTime(ft, NewFPS, Subtitles.TimecodeMode));

      Txt := iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]);
      AppendStyledTextDirect(XmlDoc, Element, Txt);

      Node.AppendChild(Element);
    end;

    try
      StringList.Clear;
      XMLToStrings(XmlDoc, StringList, Subtitles.ReplaceEntities);

      if not FileName.IsEmpty then
        StringList.SaveToFile(FileName, TEncoding.UTF8); // must be encoded using UTF-8

      Result := True;
    except
    end;
  finally
    XmlDoc.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
