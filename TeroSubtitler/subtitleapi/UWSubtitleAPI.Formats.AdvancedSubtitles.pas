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

unit UWSubtitleAPI.Formats.AdvancedSubtitles;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, UWSubtitleAPI, UWSystem.TimeUtils,
  UWSubtitleAPI.Formats, laz2_XMLRead, laz2_DOM; // laz2_XMLWrite;

type

  { TUWAdvancedSubtitles }

  TUWAdvancedSubtitles = class(TUWSubtitleCustomFormat)
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

uses LCLIntf, UWSubtitleAPI.ExtraInfo, UWSubtitleAPI.Tags, UWSubtitleAPI.Utils,
  UWSystem.StrUtils, UWSystem.SysUtils;

// -----------------------------------------------------------------------------

function TUWAdvancedSubtitles.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubtitles.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfAdvancedSubtitles;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubtitles.Extension: String;
begin
  Result := '*.xas';
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubtitles.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubtitles.HasStyleSupport: Boolean;
begin
  Result := True; // Has tags
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubtitles.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if Contains('dvddisc/ADV_OBJ/', SubtitleFile[Row]) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubtitles.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  XmlDoc      : TXMLDocument;
  Node        : TDOMNode;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
  Style       : String;
begin
  Result := False;
  XmlDoc := NIL;

  StringsToXML(XmlDoc, SubtitleFile);
  //ReadXMLFile(XmlDoc, SubtitleFile.FileName);
  if Assigned(XmlDoc) then
    try
      // Styles
      Node := XMLFindNodeByName(XmlDoc, 'style');
      if Assigned(Node) then
      begin
        Style := Node.Attributes.GetNamedItem('id').NodeValue;
        // Subtitles
        Node := XMLFindNodeByName(XmlDoc, 'p');
        if Assigned(Node) then
          repeat
            if Node.HasAttributes then
            begin
              if Style = Node.Attributes.GetNamedItem('style').NodeValue then
              begin
                if Node.Attributes.GetNamedItem('begin') <> NIL then
                  InitialTime := StringToTime(Node.Attributes.GetNamedItem('begin').NodeValue);
                if Node.Attributes.GetNamedItem('end') <> NIL then
                  FinalTime   := StringToTime(Node.Attributes.GetNamedItem('end').NodeValue);

                Text := XMLExtractTextContent(Node.ChildNodes);
                Text := ReplaceEnters(Text, '<br/>', LineEnding);
                Subtitles.Add(InitialTime, FinalTime, HTMLTagsToTS(Text), '', NIL, False);
              end;
            end;
            Node := Node.NextSibling;
          until (Node = NIL);
      end;
    finally
      XmlDoc.Free;
      Result := Subtitles.Count > 0;
    end;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubtitles.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;

  function AlphaColorToRGBAString(const Color: Cardinal): String;
  begin
    Result := SysUtils.Format('rgba(%d, %d, %d, %d)', [GetRValue(Color), GetGValue(Color), GetBValue(Color), $FF]);
  end;

  function ByteToTextAlignString(const Alignment: Byte): String;
  begin
    case Alignment of
      1: Result := 'Left';
      2: Result := 'Right';
    else
      Result := 'Center'
    end;
  end;

var
  XmlDoc : TXMLDocument;
  Root, Element, Node, SubNode : TDOMNode;
  i : Integer;
begin
  Result := False;
  XmlDoc := TXMLDocument.Create;
  try
    with Subtitles.FormatProperties^.AdvancedSubtitles do
    begin
      Root := XmlDoc.CreateElement('root');
        TDOMElement(Root).SetAttribute('xml:lang', Language);
        TDOMElement(Root).SetAttribute('xmlns', 'http://www.dvdforum.org/2005/ihd');
        TDOMElement(Root).SetAttribute('xmlns:style', 'http://www.dvdforum.org/2005/ihd#style');
        TDOMElement(Root).SetAttribute('xmlns:state', 'http://www.dvdforum.org/2005/ihd#state');
        XmlDoc.Appendchild(Root);
      Root := XmlDoc.DocumentElement;

      Element := XmlDoc.CreateElement('head');
      Root.AppendChild(Element);

      Element := XmlDoc.CreateElement('styling');
      Root.AppendChild(Element);

      Element := XmlDoc.CreateElement('style');
        TDOMElement(Element).SetAttribute('id', 'swText');
        TDOMElement(Element).SetAttribute('style:font', 'file:///dvddisc/ADV_OBJ/' + FontName);
        TDOMElement(Element).SetAttribute('style:fontSize', IntToStr(FontSize) + 'px');
        TDOMElement(Element).SetAttribute('style:color', AlphaColorToRGBAString(FontColor));
        TDOMElement(Element).SetAttribute('style:textAlign', ByteToTextAlignString(Alignment));
      Root.AppendChild(Element);

      Element := XmlDoc.CreateElement('body');
        TDOMElement(Element).SetAttribute('xml:base', 'file:///dvddisc/ADV_OBJ/');
      Root.AppendChild(Element);

      Element := XmlDoc.CreateElement('div');
        TDOMElement(Element).SetAttribute('style:position', 'absolute');
        TDOMElement(Element).SetAttribute('style:x', IntToStr(X) + '%');
        TDOMElement(Element).SetAttribute('style:y', IntToStr(Y) + '%');
        TDOMElement(Element).SetAttribute('style:width', IntToStr(W) + '%');
        TDOMElement(Element).SetAttribute('style:height', IntToStr(H) + '%');
      Root.AppendChild(Element);

      Node := Element;
    end;

    for i := FromItem to ToItem do
    begin
      Element := XmlDoc.CreateElement('p');
      TDOMElement(Element).SetAttribute('style', 'swText');
      TDOMElement(Element).SetAttribute('begin', TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:zz'));
      TDOMElement(Element).SetAttribute('end', TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:zz'));
      SubNode := XmlDoc.CreateTextNode(TSTagsToXML(ReplaceEnters(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]), sLineBreak, '<br/>')));
      Element.AppendChild(SubNode);
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
