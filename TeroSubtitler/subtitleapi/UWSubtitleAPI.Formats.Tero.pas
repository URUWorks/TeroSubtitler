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

unit UWSubtitleAPI.Formats.Tero;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, UWSubtitleAPI, UWSubtitleAPI.Formats,
  laz2_XMLRead, laz2_DOM; //, laz2_XMLWrite;

type

  { TUWTero }

  TUWTero = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function IsTimeBased: Boolean; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Utils, UWSystem.StrUtils, UWSystem.SysUtils;

// -----------------------------------------------------------------------------

function TUWTero.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWTero.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfTero;
end;

// -----------------------------------------------------------------------------

function TUWTero.Extension: String;
begin
  Result := '*.tero';
end;

// -----------------------------------------------------------------------------

function TUWTero.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWTero.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWTero.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (LowerCase(ExtractFileExt(SubtitleFile.FileName)) = '.tero') and
    SubtitleFile[Row].Contains('<tt xml') then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWTero.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  XmlDoc      : TXMLDocument;
  Node        : TDOMNode;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text,
  Translation : String;
  i           : Integer;
begin
  Result := False;
  XmlDoc := NIL;

  StringsToXML(XmlDoc, SubtitleFile);
  //ReadXMLFile(XmlDoc, SubtitleFile.FileName);
  if Assigned(XmlDoc) then
    try
      Node := XMLFindNodeByName(XmlDoc, 'tt');
      if Assigned(Node) and XMLHasAttribute(Node, 'ttp:frameRate') then
        Subtitles.FrameRate := StrToFloatDef(XMLGetAttrValue(Node, 'ttp:frameRate'), -1);

      Node := XMLFindNodeByName(XmlDoc, 'p');
      if Assigned(Node) then
        repeat
          if Node.HasAttributes then
          begin
            if Node.Attributes.GetNamedItem('begin') <> NIL then
              InitialTime := Node.Attributes.GetNamedItem('begin').NodeValue.ToInteger;
            if Node.Attributes.GetNamedItem('end') <> NIL then
              FinalTime   := Node.Attributes.GetNamedItem('end').NodeValue.ToInteger;

            if Node.GetChildCount > 0 then
              Text := ReplaceEnters(Node.ChildNodes.Item[0].TextContent, '|', sLineBreak)
            else
              Text := '';

            if Node.GetChildCount > 1 then
              Translation := ReplaceEnters(Node.ChildNodes.Item[1].TextContent, '|', sLineBreak)
            else
              Translation := '';

            i := Subtitles.Add(InitialTime, FinalTime, Text, Translation, NIL, False);
            with Subtitles.ItemPointer[i]^ do
            begin
              if Node.Attributes.GetNamedItem('align') <> NIL then
                Align  := Node.Attributes.GetNamedItem('align').NodeValue.ToInteger;
              if Node.Attributes.GetNamedItem('valign') <> NIL then
                VAlign := Node.Attributes.GetNamedItem('valign').NodeValue.ToInteger;
              if Node.Attributes.GetNamedItem('marked') <> NIL then
                Marked := Node.Attributes.GetNamedItem('marked').NodeValue.ToBoolean;
              if Node.Attributes.GetNamedItem('notes') <> NIL then
                Notes  := Node.Attributes.GetNamedItem('notes').NodeValue;
            end;
          end;
          Node := Node.NextSibling;
        until (Node = NIL);
    finally
       XmlDoc.Free;
       Result := Subtitles.Count > 0;
    end;
end;

// -----------------------------------------------------------------------------

function TUWTero.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  XmlDoc : TXMLDocument;
  Root, Element, SubElement, Node, SubNode : TDOMNode;
  i : Integer;
begin
  Result := False;
  XmlDoc := TXMLDocument.Create;
  try
    Root := XmlDoc.CreateElement('tt');
      TDOMElement(Root).SetAttribute('xmlns', 'http://www.w3.org/ns/ttml');
//      TDOMElement(Root).SetAttribute('xmlns:ttp', 'http://www.w3.org/ns/ttml#parameter');
//      TDOMElement(Root).SetAttribute('xmlns:tts', 'http://www.w3.org/ns/ttml#style');
//      TDOMElement(Root).SetAttribute('xml:lang', 'en');
//      TDOMElement(Root).SetAttribute('xmlns:ttm', 'http://www.w3.org/ns/ttml#metadata');
      TDOMElement(Root).SetAttribute('ttp:timeBase', 'media');
//      TDOMElement(Root).SetAttribute('ttp:timeBase', 'smpte');

      TDOMElement(Root).SetAttribute('ttp:frameRate', SingleToStr(FPS, FormatSettings));
      if IsInteger(FPS) then
        TDOMElement(Root).SetAttribute('ttp:frameRateMultiplier', '1 1')
      else
        TDOMElement(Root).SetAttribute('ttp:frameRateMultiplier', '999 1000');

      TDOMElement(Root).SetAttribute('ttp:dropMode', 'nonDrop');

      XmlDoc.Appendchild(Root);
    Root := XmlDoc.DocumentElement;

    Element := XmlDoc.CreateElement('head');
      TDOMElement(Element).SetAttribute('xmlns', '');
      Node := XmlDoc.CreateElement('metadata');
      Element.AppendChild(Node);
      SubNode := XmlDoc.CreateElement('ttm:title');
      Node.AppendChild(SubNode);
    Root.AppendChild(Element);
{      Node := XmlDoc.CreateElement('styling');
      Element.AppendChild(Node);
      SubNode := XmlDoc.CreateElement('style');
      TDOMElement(SubNode).SetAttribute('id', 'normal');
      TDOMElement(SubNode).SetAttribute('tts:fontStyle', 'normal');
      TDOMElement(SubNode).SetAttribute('tts:fontSize', '100%');
      TDOMElement(SubNode).SetAttribute('tts:fontWeight', 'normal');
      TDOMElement(SubNode).SetAttribute('tts:fontFamily', 'sansSerif');
      TDOMElement(SubNode).SetAttribute('tts:color', 'white');
      Node.AppendChild(SubNode);
    Root.AppendChild(Element);}

{    Node := XmlDoc.CreateElement('layout');
    Element.AppendChild(Node);
      SubNode := XmlDoc.CreateElement('region');
      TDOMElement(SubNode).SetAttribute('id', 'top');
      TDOMElement(SubNode).SetAttribute('tts:origin', '0% 0%');
      TDOMElement(SubNode).SetAttribute('tts:extent', '100% 15%');
      TDOMElement(SubNode).SetAttribute('tts:textAlign', 'center');
      TDOMElement(SubNode).SetAttribute('tts:displayAlign', 'before');
      Node.AppendChild(SubNode);
      SubNode := XmlDoc.CreateElement('region');
      TDOMElement(SubNode).SetAttribute('id', 'bottom');
      TDOMElement(SubNode).SetAttribute('tts:origin', '0% 85%');
      TDOMElement(SubNode).SetAttribute('tts:extent', '100% 15%');
      TDOMElement(SubNode).SetAttribute('tts:textAlign', 'center');
      TDOMElement(SubNode).SetAttribute('tts:displayAlign', 'after');
      Node.AppendChild(SubNode);
    Root.AppendChild(Element);}

    Element := XmlDoc.CreateElement('body');
      //TDOMElement(Element).SetAttribute('style', 'normal');
    Root.AppendChild(Element);

    Node := XmlDoc.CreateElement('div');
    Element.AppendChild(Node);

    for i := FromItem to ToItem do
    begin
      Element := XmlDoc.CreateElement('p');
      TDOMElement(Element).SetAttribute('begin', Subtitles.InitialTime[i].ToString);
      TDOMElement(Element).SetAttribute('end', Subtitles.FinalTime[i].ToString);

      if Subtitles[i].Align > 0 then
        TDOMElement(Element).SetAttribute('align', Subtitles[i].Align.ToString);
      if Subtitles[i].VAlign > 0 then
        TDOMElement(Element).SetAttribute('valign', Subtitles[i].VAlign.ToString);
      if Subtitles[i].Marked then
        TDOMElement(Element).SetAttribute('marked', Subtitles[i].Marked.ToString);
      if not Subtitles[i].Notes.IsEmpty then
        TDOMElement(Element).SetAttribute('notes', Subtitles[i].Notes);

      SubElement := XmlDoc.CreateElement('text');
      SubNode := XmlDoc.CreateTextNode(ReplaceEnters(Subtitles.Text[i]));
      SubElement.AppendChild(SubNode);
      Element.AppendChild(SubElement);
      if not Subtitles.Translation[i].IsEmpty then
      begin
        SubElement := XmlDoc.CreateElement('translation');
        SubNode := XmlDoc.CreateTextNode(Subtitles.Translation[i]);
        SubElement.AppendChild(SubNode);
        Element.AppendChild(SubElement);
      end;

      Node.AppendChild(Element);
    end;

    try
      StringList.Clear;
      XMLToStrings(XmlDoc, StringList, Subtitles.ReplaceEntities);

      if not FileName.IsEmpty then
        StringList.SaveToFile(FileName, Encoding);

      Result := True;
    except
    end;
  finally
    XmlDoc.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
