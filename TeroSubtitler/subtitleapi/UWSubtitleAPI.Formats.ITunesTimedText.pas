{*
 *  URUWorks Subtitle API
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

unit UWSubtitleAPI.Formats.ITunesTimedText;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, UWSubtitleAPI, UWSystem.TimeUtils,
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
  Node   : TDOMNode;
  Item   : TUWSubtitleItem;
begin
  Result := False; // TODO: read styles,...
  XmlDoc := NIL;

  StringsToXML(XmlDoc, SubtitleFile);
  //ReadXMLFile(XmlDoc, SubtitleFile.FileName);
  if Assigned(XmlDoc) then
    try
      Node := XMLFindNodeByName(XmlDoc, 'tt');
      Subtitles.FrameRate := FPS;

      if Assigned(Node) then
      begin
        if XMLHasAttribute(Node, 'ttp:frameRate') then
        begin
          Subtitles.FrameRate := StrToFloatDef(XMLGetAttrValue(Node, 'ttp:frameRate'), FPS);

          if XMLHasAttribute(Node, 'ttp:frameRateMultiplier') then
            Subtitles.FrameRate := XMLGetCorrectFPS(Subtitles.FrameRate, XMLGetAttrValue(Node, 'ttp:frameRateMultiplier'));
        end;

        if XMLHasAttribute(Node, 'ttp:timeBase') then
        begin
          if XMLGetAttrValue(Node, 'ttp:timeBase') = 'smpte' then
            Subtitles.TimeBase := stbSMPTE
          else
            Subtitles.TimeBase := stbMedia;
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
                with Node.Attributes.GetNamedItem('region') do
                if NodeValue.Contains('top') or (NodeValue = 'sh0') then
                    VAlign := svaTop
                  else
                    VAlign := svaBottom;
              end;

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

      //if Subtitles.TimeBase = stbSMPTE then
        TDOMElement(Root).SetAttribute('ttp:timeBase', 'smpte');
      //else
      //  TDOMElement(Root).SetAttribute('ttp:timeBase', 'media');

      if IsInteger(FPS) then
      begin
        NewFPS := FPS;
        TDOMElement(Root).SetAttribute('ttp:frameRate', Trunc(NewFPS).ToString);
        TDOMElement(Root).SetAttribute('ttp:frameRateMultiplier', '1 1');
      end
      else
      begin
        NewFPS := FPS + (FPS * 0.001);
        TDOMElement(Root).SetAttribute('ttp:frameRate', Round(NewFPS).ToString);
        TDOMElement(Root).SetAttribute('ttp:frameRateMultiplier', '999 1000');
      end;

      TDOMElement(Root).SetAttribute('ttp:dropMode', 'nonDrop');

      XmlDoc.Appendchild(Root);
    Root := XmlDoc.DocumentElement;

    Element := XmlDoc.CreateElement('head');
      //TDOMElement(Element).SetAttribute('xmlns', '');
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
      if not IsInteger(FPS) and (Subtitles.TimeBase = stbMedia) then
      begin
        it := Round(it / 1.001);
        ft := Round(ft / 1.001);
      end;

      Element := XmlDoc.CreateElement('p');
      TDOMElement(Element).SetAttribute('region', iff(Subtitles[i].VAlign = svaTop, 'top', 'bottom'));
      TDOMElement(Element).SetAttribute('begin', TimeToString(it, 'hh:mm:ss:ff', NewFPS));
      //TDOMElement(Element).SetAttribute('id', TimeToString(Subtitles.InitialTime[i], 'p' + IntToStr(i)));
      TDOMElement(Element).SetAttribute('end', TimeToString(ft, 'hh:mm:ss:ff', NewFPS));
      SubNode := XmlDoc.CreateTextNode(TSTagsToXML(ReplaceEnters(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]), sLineBreak, '<br/>')));
      Element.AppendChild(SubNode);
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
