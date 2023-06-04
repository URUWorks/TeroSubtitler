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

unit UWSubtitleAPI.Formats.NetflixTimedText;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, StrUtils, UWSubtitleAPI, UWSystem.TimeUtils,
  UWSubtitleAPI.Formats, laz2_XMLRead, laz2_DOM; // laz2_XMLWrite;

type

  { TUWNetflixTimedText }

  TUWNetflixTimedText = class(TUWSubtitleCustomFormat)
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

uses UWSubtitleAPI.Tags, UWSubtitleAPI.Utils, UWSystem.StrUtils,
  UWSystem.SysUtils;

// -----------------------------------------------------------------------------

function TUWNetflixTimedText.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWNetflixTimedText.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfNetflixTimedText;
end;

// -----------------------------------------------------------------------------

function TUWNetflixTimedText.Extension: String;
begin
  Result := '*.dfxp';
end;

// -----------------------------------------------------------------------------

function TUWNetflixTimedText.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWNetflixTimedText.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWNetflixTimedText.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (LowerCase(ExtractFileExt(SubtitleFile.FileName)) = '.dfxp')
    and Contains('<tt xmlns=', SubtitleFile[Row]) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWNetflixTimedText.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;

  function GetTime(const S: String): Integer;
  begin
    if AnsiEndsStr('t', S) then // ticks
      Result := TicksToMSecs(StrToInt64(Copy(S, 0, Length(S)-1)))
    else
      Result := StringToTime(S);
  end;

var
  XmlDoc      : TXMLDocument;
  Node        : TDOMNode;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  XmlDoc := NIL;

  StringsToXML(XmlDoc, SubtitleFile);
  //ReadXMLFile(XmlDoc, SubtitleFile.FileName);
  if Assigned(XmlDoc) then
    try
      Node := XMLFindNodeByName(XmlDoc, 'p');
      if Assigned(Node) then
        repeat
          if Node.HasAttributes then
          begin
            if Node.Attributes.GetNamedItem('begin') <> NIL then
              InitialTime := GetTime(Node.Attributes.GetNamedItem('begin').NodeValue);
            if Node.Attributes.GetNamedItem('end') <> NIL then
              FinalTime   := GetTime(Node.Attributes.GetNamedItem('end').NodeValue);

            Text := XMLExtractTextContent(Node.ChildNodes);
            Text := ReplaceEnters(Text, '<br/>', LineEnding);
            Subtitles.Add(InitialTime, FinalTime, HTMLTagsToTS(Text), '', NIL, False);
          end;
          Node := Node.NextSibling;
        until (Node = NIL);
    finally
       XmlDoc.Free;
       Result := Subtitles.Count > 0;
    end;
end;

// -----------------------------------------------------------------------------

function TUWNetflixTimedText.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  XmlDoc : TXMLDocument;
  Root, Element, Node, SubNode : TDOMNode;
  i : Integer;
begin
  Result := False;
  XmlDoc := TXMLDocument.Create;
  try
    Root := XmlDoc.CreateElement('tt');
      TDOMElement(Root).SetAttribute('xmlns', 'http://www.w3.org/ns/ttml');
      TDOMElement(Root).SetAttribute('xmlns:ttm', 'http://www.w3.org/ns/ttml#metadata');
      TDOMElement(Root).SetAttribute('xmlns:tts', 'http://www.w3.org/ns/ttml#styling');
      TDOMElement(Root).SetAttribute('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
      TDOMElement(Root).SetAttribute('xml:lang', 'en');
      XmlDoc.Appendchild(Root);
    Root := XmlDoc.DocumentElement;

    Element := XmlDoc.CreateElement('head');
      //TDOMElement(Element).SetAttribute('xmlns', '');
      Node := XmlDoc.CreateElement('metadata');
      Element.AppendChild(Node);
      SubNode := XmlDoc.CreateElement('ttm:title');
      SubNode.AppendChild(XmlDoc.CreateTextNode('Netflix Subtitle'));
      Node.AppendChild(SubNode);
    Root.AppendChild(Element);
    Node := XmlDoc.CreateElement('styling');
      Element.AppendChild(Node);
      SubNode := XmlDoc.CreateElement('style');
      TDOMElement(SubNode).SetAttribute('xml:id', 's1');
      TDOMElement(SubNode).SetAttribute('tts:fontStyle', 'normal');
      TDOMElement(SubNode).SetAttribute('tts:fontWeight', 'normal');
      TDOMElement(SubNode).SetAttribute('tts:fontFamily', 'Arial');
      TDOMElement(SubNode).SetAttribute('tts:fontSize', '100%');
      TDOMElement(SubNode).SetAttribute('tts:color', 'white');
    Node.AppendChild(SubNode);
    Root.AppendChild(Element);
    Node := XmlDoc.CreateElement('layout');
      Element.AppendChild(Node);
      SubNode := XmlDoc.CreateElement('region');
      TDOMElement(SubNode).SetAttribute('xml:id', 'bottomCenter');
      TDOMElement(SubNode).SetAttribute('tts:extent', '80% 40%');
      TDOMElement(SubNode).SetAttribute('tts:origin', '10% 50%');
      TDOMElement(SubNode).SetAttribute('tts:displayAlign', 'after');
      TDOMElement(SubNode).SetAttribute('tts:textAlign', 'center');
      Node.AppendChild(SubNode);

      SubNode := XmlDoc.CreateElement('region');
      TDOMElement(SubNode).SetAttribute('xml:id', 'topCenter');
      TDOMElement(SubNode).SetAttribute('tts:extent', '80% 40%');
      TDOMElement(SubNode).SetAttribute('tts:origin', '10% 10%');
      TDOMElement(SubNode).SetAttribute('tts:displayAlign', 'before');
      TDOMElement(SubNode).SetAttribute('tts:textAlign', 'center');
      Node.AppendChild(SubNode);

    Root.AppendChild(Element);

    Element := XmlDoc.CreateElement('body');
    Root.AppendChild(Element);

    Node := XmlDoc.CreateElement('div');
    TDOMElement(Node).SetAttribute('style', 's1');
    TDOMElement(Node).SetAttribute('xml:id', 'd1');
    Element.AppendChild(Node);

    for i := FromItem to ToItem do
    begin
      Element := XmlDoc.CreateElement('p');
      TDOMElement(Element).SetAttribute('xml:id', TimeToString(Subtitles.InitialTime[i], 'p' + IntToStr(i)));
      TDOMElement(Element).SetAttribute('begin', TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss.zzz'));
      TDOMElement(Element).SetAttribute('end', TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss.zzz'));
      TDOMElement(Element).SetAttribute('region', 'bottomCenter');
      SubNode := XmlDoc.CreateTextNode(TSTagsToHTML(ReplaceEnters(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]), sLineBreak, '<br/>')));
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