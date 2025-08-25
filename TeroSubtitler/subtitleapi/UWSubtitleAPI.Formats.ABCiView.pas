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

unit UWSubtitleAPI.Formats.ABCiView;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats,
  laz2_XMLRead, laz2_DOM; // laz2_XMLWrite;

type

  { TUWABCiView }

  TUWABCiView = class(TUWSubtitleCustomFormat)
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

function TUWABCiView.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWABCiView.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfABCiView;
end;

// -----------------------------------------------------------------------------

function TUWABCiView.Extension: String;
begin
  Result := '*.xml';
end;

// -----------------------------------------------------------------------------

function TUWABCiView.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWABCiView.HasStyleSupport: Boolean;
begin
  Result := True; // Has tags
end;

// -----------------------------------------------------------------------------

function TUWABCiView.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if Contains('<reel ', SubtitleFile[Row]) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWABCiView.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
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
      Node := XMLFindNodeByName(XmlDoc, 'title');
      if Assigned(Node) then
        repeat
          if Node.HasAttributes then
          begin
            if Node.Attributes.GetNamedItem('start') <> NIL then
              InitialTime := StringToTime(Node.Attributes.GetNamedItem('start').NodeValue);
            if Node.Attributes.GetNamedItem('end') <> NIL then
              FinalTime   := StringToTime(Node.Attributes.GetNamedItem('end').NodeValue);

            Text := XMLExtractTextContent(Node.ChildNodes);
            Text := ReplaceEnters(Text, '<br/>', LineEnding);

            if (InitialTime >= 0) and (FinalTime > 0) and not Text.IsEmpty then
              Subtitles.Add(InitialTime, FinalTime, HTMLTagsToTS(Text), '', NIL);
          end;
          Node := Node.NextSibling;
        until (Node = NIL);
    finally
       XmlDoc.Free;
       Result := Subtitles.Count > 0;
    end;
end;

// -----------------------------------------------------------------------------

function TUWABCiView.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  XmlDoc : TXMLDocument;
  Root, Element, Node, SubNode : TDOMNode;
  i : Integer;
begin
  Result := False;
  XmlDoc := TXMLDocument.Create;
  try
    Root := XmlDoc.CreateElement('root');
      TDOMElement(Root).SetAttribute('fps', '25');
      TDOMElement(Root).SetAttribute('movie', 'program title');
      TDOMElement(Root).SetAttribute('language', 'GBR:English (UK)');
      TDOMElement(Root).SetAttribute('font', 'Arial');
      TDOMElement(Root).SetAttribute('style', 'normal');
      TDOMElement(Root).SetAttribute('size', '48');
      XmlDoc.Appendchild(Root);
    Root := XmlDoc.DocumentElement;

    Element := XmlDoc.CreateElement('reel');
      TDOMElement(Element).SetAttribute('start', '');
      TDOMElement(Element).SetAttribute('first', '');
      TDOMElement(Element).SetAttribute('last', '');
    Root.AppendChild(Element);
    Node := Element;

    for i := FromItem to ToItem do
    begin
      Element := XmlDoc.CreateElement('title');
      TDOMElement(Element).SetAttribute('start', TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:zzz'));
      TDOMElement(Element).SetAttribute('end', TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:zzz'));
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
