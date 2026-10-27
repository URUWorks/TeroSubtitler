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
 *  Copyright (C) 2001-2026 URUWorks, uruworks@gmail.com.
 *}

unit TeroSubtitler;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, UWSubtitleAPI, UWSubtitleAPI.Formats,
  laz2_XMLRead, laz2_DOM; //, laz2_XMLWrite;

type

  { TUWTeroSubtitler }

  TUWTeroSubtitler = class(TUWSubtitleCustomFormat)
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

uses UWSubtitleAPI.Utils, UWSystem.StrUtils, UWSystem.SysUtils;

// -----------------------------------------------------------------------------

function TUWTeroSubtitler.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWTeroSubtitler.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfTeroSubtitler;
end;

// -----------------------------------------------------------------------------

function TUWTeroSubtitler.Extension: String;
begin
  Result := '*.tero';
end;

// -----------------------------------------------------------------------------

function TUWTeroSubtitler.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWTeroSubtitler.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWTeroSubtitler.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (LowerCase(ExtractFileExt(SubtitleFile.FileName)) = '.tero') and
    SubtitleFile[Row].Contains('<tt xml') then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWTeroSubtitler.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  XmlDoc      : TXMLDocument;
  Node        : TDOMNode;
  NodeList    : TDOMNodeList;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text,
  Translation : String;
  i, AlignVal : Integer;
  FS          : TFormatSettings;
begin
  Result := False;
  XmlDoc := NIL;

  StringsToXML(XmlDoc, SubtitleFile);
  //ReadXMLFile(XmlDoc, SubtitleFile.FileName);
  if Assigned(XmlDoc) then
    try
      // Antes StrToFloatDef(..., -1) dejaba un framerate INVÁLIDO (-1) si el
      // atributo no era numérico (p. ej. escrito con coma decimal en otro
      // equipo); ahora acepta '.' o ',' y si no, el FPS actual.
      Node := XMLFindNodeByName(XmlDoc, 'tt');
      if Assigned(Node) and XMLHasAttribute(Node, 'ttp:frameRate') then
      begin
        FS := FormatSettings;
        FS.DecimalSeparator := '.';
        Subtitles.FrameRate := StrToFloatDef(ReplaceString(XMLGetAttrValue(Node, 'ttp:frameRate'), ',', '.'), FPS, FS);
        if Subtitles.FrameRate <= 0 then Subtitles.FrameRate := FPS;
      end;

      Node := XMLFindNodeByName(XmlDoc, 'p');
      if Assigned(Node) then
        repeat
          if Node.HasAttributes then
          begin
            // Antes los tiempos no se reseteaban entre <p> (uno sin begin/end
            // heredaba los del anterior), .ToInteger lanzaba EConvertError ante
            // un valor no numérico y no había ningún chequeo antes de Add.
            InitialTime := -1;
            FinalTime   := -1;
            if Node.Attributes.GetNamedItem('begin') <> NIL then
              InitialTime := StrToIntDef(Node.Attributes.GetNamedItem('begin').NodeValue, -1);
            if Node.Attributes.GetNamedItem('end') <> NIL then
              FinalTime   := StrToIntDef(Node.Attributes.GetNamedItem('end').NodeValue, -1);

            Text := '';
            Translation := '';
            NodeList := Node.GetChildNodes;
            for i := 0 to NodeList.Count-1 do
            begin
              if NodeList.Item[i].NodeName = 'text' then
                Text := ReplaceEnters(Node.ChildNodes.Item[i].TextContent, '|', sLineBreak)
              else if NodeList.Item[i].NodeName = 'translation' then
                Translation := ReplaceEnters(Node.ChildNodes.Item[i].TextContent, '|', sLineBreak);
            end;

            if (InitialTime >= 0) and (FinalTime > InitialTime) then
            begin
              i := Subtitles.Add(InitialTime, FinalTime, Text, Translation, NIL);

              if i >= 0 then
              with Subtitles.ItemPointer[i]^ do
              begin
                // los casts a enum se validan antes: un valor fuera de rango
                // era un enum inválido
                if Node.Attributes.GetNamedItem('align') <> NIL then
                  AlignVal := StrToIntDef(Node.Attributes.GetNamedItem('align').NodeValue, -1)
                else
                  AlignVal := -1;
                if (AlignVal >= Ord(Low(TSubtitleHAlign))) and (AlignVal <= Ord(High(TSubtitleHAlign))) then
                  Align := TSubtitleHAlign(AlignVal);

                if Node.Attributes.GetNamedItem('valign') <> NIL then
                  AlignVal := StrToIntDef(Node.Attributes.GetNamedItem('valign').NodeValue, -1)
                else
                  AlignVal := -1;
                if (AlignVal >= Ord(Low(TSubtitleVAlign))) and (AlignVal <= Ord(High(TSubtitleVAlign))) then
                  VAlign := TSubtitleVAlign(AlignVal);

                if Node.Attributes.GetNamedItem('marked') <> NIL then
                  Marked := StrToBoolDef(Node.Attributes.GetNamedItem('marked').NodeValue, False);
                if Node.Attributes.GetNamedItem('notes') <> NIL then
                  Notes  := Node.Attributes.GetNamedItem('notes').NodeValue;
                if Node.Attributes.GetNamedItem('actor') <> NIL then
                  Actor  := Node.Attributes.GetNamedItem('actor').NodeValue;
              end;
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

function TUWTeroSubtitler.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
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

      TDOMElement(Root).SetAttribute('ttp:frameRate', SingleToStr(FPS, '.')); // independiente del locale
//      if IsInteger(FPS) then
//        TDOMElement(Root).SetAttribute('ttp:frameRateMultiplier', '1 1')
//      else
//        TDOMElement(Root).SetAttribute('ttp:frameRateMultiplier', '999 1000');

//      TDOMElement(Root).SetAttribute('ttp:dropMode', 'nonDrop');

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

      if Subtitles[i].Align <> shaNone then
        TDOMElement(Element).SetAttribute('align', Integer(Subtitles[i].Align).ToString);
      if Subtitles[i].VAlign <> svaBottom then
        TDOMElement(Element).SetAttribute('valign', Integer(Subtitles[i].VAlign).ToString);
      if Subtitles[i].Marked then
        TDOMElement(Element).SetAttribute('marked', Subtitles[i].Marked.ToString);
      if not Subtitles[i].Notes.IsEmpty then
        TDOMElement(Element).SetAttribute('notes', Subtitles[i].Notes);
      if not Subtitles[i].Actor.IsEmpty then
        TDOMElement(Element).SetAttribute('actor', Subtitles[i].Actor); // antes se escribía Notes

      SubElement := XmlDoc.CreateElement('text');
      SubNode := XmlDoc.CreateTextNode(ReplaceEnters(Subtitles.Text[i]));
      SubElement.AppendChild(SubNode);
      Element.AppendChild(SubElement);
      if not Subtitles.Translation[i].IsEmpty then
      begin
        SubElement := XmlDoc.CreateElement('translation');
        SubNode := XmlDoc.CreateTextNode(ReplaceEnters(Subtitles.Translation[i])); // '|' como el texto
        SubElement.AppendChild(SubNode);
        Element.AppendChild(SubElement);
      end;

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
