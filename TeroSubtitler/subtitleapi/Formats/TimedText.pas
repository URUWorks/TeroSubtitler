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

unit TimedText;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils,
  UWSubtitleAPI.Formats, Classes, laz2_XMLRead, laz2_DOM; // laz2_XMLWrite;

type

  { TUWTimedText }

  TUWTimedText = class(TUWSubtitleCustomFormat)
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
  UWSystem.SysUtils, UWSubtitleAPI.Styles;

const
  // the styles of the subtitle (ASS) go in the <style> elements as normal
  // TTML attributes, and also as the "Style:" line in an attribute of this
  // namespace, so that Tero can read them back as they were
  TeroNS = 'urn:uruworks:terosubtitler';

// -----------------------------------------------------------------------------

function TTMLColor(const AColor: Cardinal): String;
begin
  // #RRGGBB, or #RRGGBBAA when it isn't opaque
  Result := ASSColorToHTML(AColor);
  if ASSColorAlpha(AColor) <> 0 then
    Result := Result + IntToHex(255 - ASSColorAlpha(AColor), 2);
end;

// -----------------------------------------------------------------------------

function TUWTimedText.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWTimedText.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfTimedText;
end;

// -----------------------------------------------------------------------------

function TUWTimedText.Extension: String;
begin
  Result := '*.xml;*.ttml';
end;

// -----------------------------------------------------------------------------

function TUWTimedText.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWTimedText.HasStyleSupport: Boolean;
begin
  Result := True; // Has tags
end;

// -----------------------------------------------------------------------------

function TUWTimedText.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
var
  sExt: String;
begin
  sExt := LowerCase(ExtractFileExt(SubtitleFile.FileName));
  if ((sExt = '.xml') or (sExt = '.ttml') or (sExt = '.tt') or (sExt = '.dfxp')) and
    (Contains('<tt xml:', SubtitleFile[Row]) or Contains('<tt:tt', SubtitleFile[Row]) or Contains('</tt>', SubtitleFile[Row])) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWTimedText.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  XmlDoc : TXMLDocument;
  Node, LayoutNode, RegionNode : TDOMNode;
  Item   : TUWSubtitleItem;
  prefix : String;
  RegionMap: TStringList;
  RegionID, AlignVal, TextAlignVal, NewText: String;
  CurBegin, CurEnd: Integer;
  IsContinuation: Boolean;
  TickRate: Double;
  Styles: TASSStyles;
  StyleIds: TStringList;
  StyleNode: TDOMNode;
  ResX, ResY, si: Integer;
  St: TASSStyle;

  procedure ReadTeroStyles(const ANode: TDOMNode);
  var
    Child: TDOMNode;
  begin
    // the <style> elements with the line of a Tero style (tero:style)
    Child := ANode.FirstChild;
    while Assigned(Child) do
    begin
      if ((Child.NodeName = prefix+'style') or (Child.NodeName = 'style')) and
        XMLHasAttribute(Child, 'xml:id') and XMLHasAttribute(Child, 'tero:style') and
        ASSStyleFromLine(XMLGetAttrValue(Child, 'tero:style'), St) and
        (ASSIndexOfStyle(Styles, St.Name) < 0) then
      begin
        SetLength(Styles, Length(Styles) + 1);
        Styles[High(Styles)] := St;
        StyleIds.Values[XMLGetAttrValue(Child, 'xml:id')] := IntToStr(High(Styles));
      end;
      Child := Child.NextSibling;
    end;
  end;

begin
  Result := False;
  XmlDoc := NIL;
  Styles := NIL;

  StringsToXML(XmlDoc, SubtitleFile);

  // Antes: RegionMap se creaba siempre y no se liberaba si el XML era inválido
  if not Assigned(XmlDoc) then Exit;

  RegionMap := TStringList.Create;
  StyleIds := TStringList.Create;
  try
      if Contains('<tt:tt', SubtitleFile.Text) then
        prefix := 'tt:'
      else
        prefix := '';

      Node := XMLFindNodeByName(XmlDoc, prefix+'tt');
      Subtitles.FrameRate := FPS;
      TickRate := 10000000;

      if Assigned(Node) and XMLHasAttribute(Node, 'xml:lang') and
        (Trim(XMLGetAttrValue(Node, 'xml:lang')) <> '') then
        Subtitles.FormatProperties^.TimedText.Language := Trim(XMLGetAttrValue(Node, 'xml:lang'));


      if Assigned(Node) then
      begin
        if XMLHasAttribute(Node, 'ttp:tickRate') then
          TickRate := StrToFloatDef(XMLGetAttrValue(Node, 'ttp:tickRate'), TickRate);

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

      // the styles written by Tero
      ResX := 0;
      ResY := 0;
      if Assigned(Node) and XMLHasAttribute(Node, 'tero:playRes') then
      begin
        ResX := StrToIntDef(Copy(XMLGetAttrValue(Node, 'tero:playRes'), 1, Pos('x', XMLGetAttrValue(Node, 'tero:playRes')) - 1), 0);
        ResY := StrToIntDef(Copy(XMLGetAttrValue(Node, 'tero:playRes'), Pos('x', XMLGetAttrValue(Node, 'tero:playRes')) + 1, MaxInt), 0);
      end;
      StyleNode := XMLFindNodeByName(XmlDoc, prefix+'styling');
      if Assigned(StyleNode) then ReadTeroStyles(StyleNode);

      // VAlign
      LayoutNode := XMLFindNodeByName(XmlDoc, prefix+'layout');
      if Assigned(LayoutNode) then
      begin
        RegionNode := LayoutNode.FirstChild;
        while Assigned(RegionNode) do
        begin
          if (RegionNode.NodeName = prefix+'region') or (RegionNode.NodeName = 'region') then
          begin
            if XMLHasAttribute(RegionNode, 'xml:id') then
            begin
              RegionID := XMLGetAttrValue(RegionNode, 'xml:id');
              if XMLHasAttribute(RegionNode, 'tts:displayAlign') then
                AlignVal := XMLGetAttrValue(RegionNode, 'tts:displayAlign')
              else
                AlignVal := '';

              // Antes: tts:textAlign nunca se leía
              RegionMap.Values[RegionID] := AlignVal + '|' + XMLGetAttrValue(RegionNode, 'tts:textAlign');
            end;
          end;
          RegionNode := RegionNode.NextSibling;
        end;
      end;

      // <p> con UNIFICACIÓN de tiempos
      Node := XMLFindNodeByName(XmlDoc, prefix+'p');
      if Assigned(Node) then
        repeat
          if Node.HasAttributes then
          begin
            // Extraer tiempos para comparar
            CurBegin := 0;
            CurEnd := 0;
            if XMLHasAttribute(Node, 'begin') then
              CurBegin := TTMLTimeToMS(XMLGetAttrValue(Node, 'begin'), Subtitles.FrameRate, TickRate);
            if XMLHasAttribute(Node, 'end') then
              CurEnd := TTMLTimeToMS(XMLGetAttrValue(Node, 'end'), Subtitles.FrameRate, TickRate)
            else if XMLHasAttribute(Node, 'dur') then // Antes: 'dur' se ignoraba
              CurEnd := CurBegin + TTMLTimeToMS(XMLGetAttrValue(Node, 'dur'), Subtitles.FrameRate, TickRate);

            // Extraer y limpiar el texto del nodo actual
            // Antes: el XML del <p> se volvía a serializar y se pasaba por regex,
            // que perdía los spans anidados, los tts:* en línea y los <tt:br/>
            NewText := HTMLTagsToTS(ExtractStyledTTMLText(Node));

            // Lógica de UNIFICACIÓN
            IsContinuation := (Subtitles.Count > 0) and
                             (Subtitles[Subtitles.Count-1].InitialTime = CurBegin) and
                             (Subtitles[Subtitles.Count-1].FinalTime = CurEnd);

            if IsContinuation then
            begin
              // Si el tiempo es el mismo, concatenamos el texto al ítem anterior
              Subtitles.Text[Subtitles.Count-1] := Subtitles[Subtitles.Count-1].Text + LineEnding + NewText;
            end
            else
            begin
              // Si es un tiempo nuevo, creamos un nuevo TUWSubtitleItem
              ClearSubtitleItem(Item);
              Item.InitialTime := CurBegin;
              Item.FinalTime := CurEnd;
              Item.Text := NewText;

              // Determinar alineación vertical
              if XMLHasAttribute(Node, 'region') then
              begin
                RegionID := XMLGetAttrValue(Node, 'region');
                AlignVal := RegionMap.Values[RegionID];
                TextAlignVal := '';
                if Pos('|', AlignVal) > 0 then
                begin
                  TextAlignVal := Copy(AlignVal, Pos('|', AlignVal) + 1, MaxInt);
                  AlignVal := Copy(AlignVal, 1, Pos('|', AlignVal) - 1);
                end;

                // tts:textAlign
                if SameText(TextAlignVal, 'left') or SameText(TextAlignVal, 'start') then
                  Item.Align := shaLeft
                else if SameText(TextAlignVal, 'right') or SameText(TextAlignVal, 'end') then
                  Item.Align := shaRight;

                // tts:displayAlign
                if SameText(AlignVal, 'before') then
                  Item.VAlign := svaTop
                else if SameText(AlignVal, 'center') then
                  Item.VAlign := svaCenter
                else if SameText(AlignVal, 'after') then
                  Item.VAlign := svaBottom
                // no existe tts:displayAlign
                else if SameText(RegionID, 'top') or SameText(RegionID, 'sh0') then
                  Item.VAlign := svaTop
                else if SameText(RegionID, 'middle') or SameText(RegionID, 'center') then
                  Item.VAlign := svaCenter
                else
                  Item.VAlign := svaBottom;
              end
              else
                Item.VAlign := svaBottom;

              // the style of the entry; the alignment of the region is the one
              // of the style when it was saved without its own
              si := StrToIntDef(StyleIds.Values[XMLGetAttrValue(Node, 'style')], -1);
              if si >= 0 then
              begin
                Item.Style := Styles[si].Name;
                if ItemNumpadAlign(Item.Align, Item.VAlign) = Styles[si].Alignment then
                begin
                  Item.Align  := shaNone;
                  Item.VAlign := svaBottom;
                end;
              end;

              Subtitles.Add(Item, NIL);
            end;
          end;

          Node := Node.NextSibling;
        until (Node = NIL);

      // a header with the styles, for ASS and the style editor (when
      // appending to a subtitle that has its own, those are kept)
      if (Length(Styles) > 0) and (Subtitles.ASSHeader = '') then
        Subtitles.ASSHeader := ASSHeaderFromStyles(ResX, ResY, Styles);

    finally
      StyleIds.Free;
      RegionMap.Free;
      XmlDoc.Free;
      Result := Subtitles.Count > 0;
    end;
end;

// -----------------------------------------------------------------------------

function TUWTimedText.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  XmlDoc : TXMLDocument;
  Root, Element, Node, SubNode, StylingNode, LayoutNode : TDOMNode;
  i, si : Integer;
  RegionStr: String;
  WrittenRegions: TStringList;
  STab: TASSStyleTable;
  HasStyles: Boolean;
  HA: TSubtitleHAlign;
  VA: TSubtitleVAlign;

  procedure AddASSStyle(const AIndex: Integer);
  var
    St: TASSStyle;
    E: TDOMElement;
    s: String;
  begin
    // the style of the subtitle as TTML (the size in cells of the height,
    // 15 rows) and as its ASS line, to read it back
    St := STab.Styles[AIndex];
    E := XmlDoc.CreateElement('style');
    E.SetAttribute('xml:id', STab.Ids[AIndex]);
    E.SetAttribute('tts:fontFamily', ASSFontFamily(St.FontName) + ', sansSerif');
    if St.FontSize > 0 then
      E.SetAttribute('tts:fontSize', ASSFloatToStr(St.FontSize * 15 / STab.ResY) + 'c');
    E.SetAttribute('tts:color', TTMLColor(St.Colors[0]));
    E.SetAttribute('tts:fontWeight', iff(St.Bold, 'bold', 'normal'));
    E.SetAttribute('tts:fontStyle', iff(St.Italic, 'italic', 'normal'));
    s := '';
    if St.Underline then s := 'underline';
    if St.StrikeOut then s := Trim(s + ' lineThrough');
    if s = '' then s := 'none';
    E.SetAttribute('tts:textDecoration', s);
    if St.BorderStyle = 3 then // opaque box, of the outline color
      E.SetAttribute('tts:backgroundColor', TTMLColor(St.Colors[2]))
    else if (St.Outline > 0) and (St.FontSize > 0) then
      E.SetAttribute('tts:textOutline', TTMLColor(St.Colors[2]) + ' ' + ASSFloatToStr(St.Outline / St.FontSize) + 'em');
    E.SetAttribute('tero:style', ASSStyleToLine(St));
    StylingNode.AppendChild(E);
  end;

  procedure AddDecorationStyle(const AId, ADecoration: String);
  begin
    // Antes: <span style="underline/strikeout"> apuntaba a un estilo inexistente
    if DOMHasStyleRef(Root, AId) then
    begin
      SubNode := XmlDoc.CreateElement('style');
      TDOMElement(SubNode).SetAttribute('xml:id', AId);
      TDOMElement(SubNode).SetAttribute('tts:fontStyle', 'normal');
      TDOMElement(SubNode).SetAttribute('tts:fontSize', '100%');
      TDOMElement(SubNode).SetAttribute('tts:fontWeight', 'normal');
      TDOMElement(SubNode).SetAttribute('tts:fontFamily', 'sansSerif');
      TDOMElement(SubNode).SetAttribute('tts:color', 'white');
      TDOMElement(SubNode).SetAttribute('tts:textDecoration', ADecoration);
      StylingNode.AppendChild(SubNode);
    end;
  end;

begin
  Result := False;
  XmlDoc := TXMLDocument.Create;
  WrittenRegions := TStringList.Create;
  try
    Root := XmlDoc.CreateElement('tt');
      TDOMElement(Root).SetAttribute('xmlns', 'http://www.w3.org/ns/ttml');
      TDOMElement(Root).SetAttribute('xmlns:ttp', 'http://www.w3.org/ns/ttml#parameter');
      TDOMElement(Root).SetAttribute('ttp:timeBase', 'media');
      // Antes: '#style', la URI del namespace de estilos TTML es '#styling'
      TDOMElement(Root).SetAttribute('xmlns:tts', 'http://www.w3.org/ns/ttml#styling');
      TDOMElement(Root).SetAttribute('xml:lang', TimedTextLanguage(Subtitles));
      TDOMElement(Root).SetAttribute('xmlns:ttm', 'http://www.w3.org/ns/ttml#metadata');
      // the styles of the subtitle, only when it has them (ASS)
      ASSStyleTableInit(STab, Subtitles.ASSHeader);
      HasStyles := Length(STab.Styles) > 0;
      if HasStyles then
      begin
        TDOMElement(Root).SetAttribute('ttp:cellResolution', '32 15');
        TDOMElement(Root).SetAttribute('xmlns:tero', TeroNS);
        TDOMElement(Root).SetAttribute('tero:playRes', IntToStr(STab.ResX) + 'x' + IntToStr(STab.ResY));
      end;
      XmlDoc.Appendchild(Root);
    Root := XmlDoc.DocumentElement;

    // Antes: xmlns='' en <head> lo dejaba fuera del namespace TTML
    Element := XmlDoc.CreateElement('head');
      Node := XmlDoc.CreateElement('metadata');
      Element.AppendChild(Node);
      SubNode := XmlDoc.CreateElement('ttm:title');
      Node.AppendChild(SubNode);
    Root.AppendChild(Element);
      Node := XmlDoc.CreateElement('styling');
      StylingNode := Node;
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
      // Antes: tts:fontStyle="bold" (valor inválido) y fontWeight normal
      TDOMElement(SubNode).SetAttribute('tts:fontStyle', 'normal');
      TDOMElement(SubNode).SetAttribute('tts:fontSize', '100%');
      TDOMElement(SubNode).SetAttribute('tts:fontWeight', 'bold');
      TDOMElement(SubNode).SetAttribute('tts:fontFamily', 'sansSerif');
      TDOMElement(SubNode).SetAttribute('tts:color', 'white');
      Node.AppendChild(SubNode);
      // all of them, also the ones not used, so they come back on load
      if HasStyles then
        for si := 0 to High(STab.Styles) do AddASSStyle(si);
    Root.AppendChild(Element);

    // Antes las 3 regiones se escribían siempre; ahora solo las que se usan
    LayoutNode := XmlDoc.CreateElement('layout');
    Element.AppendChild(LayoutNode);
    Root.AppendChild(Element);

    Element := XmlDoc.CreateElement('body');
      TDOMElement(Element).SetAttribute('style', 'normal');
    Root.AppendChild(Element);

    Node := XmlDoc.CreateElement('div');
    Element.AppendChild(Node);

    for i := FromItem to ToItem do
    begin
      Element := XmlDoc.CreateElement('p');

      // the alignment of the entry, or the one of its style without it
      HA := Subtitles[i].Align;
      VA := Subtitles[i].VAlign;
      si := -1;
      if HasStyles then
      begin
        si := ASSStyleTableFind(STab, Subtitles[i].Style);
        if (si >= 0) and ItemHasNoAlign(HA, VA) then
          NumpadToItemAlign(STab.Styles[si].Alignment, HA, VA);
      end;

      // Antes: la alineación horizontal se ignoraba (siempre centrado)
      if VA = svaTop then
        RegionStr := 'top'
      else if VA = svaCenter then
        RegionStr := 'middle'
      else
        RegionStr := 'bottom';
      if HA = shaLeft then
        RegionStr := RegionStr + 'Left'
      else if HA = shaRight then
        RegionStr := RegionStr + 'Right';

      if WrittenRegions.IndexOf(RegionStr) < 0 then
      begin
        WrittenRegions.Add(RegionStr);
        SubNode := XmlDoc.CreateElement('region');
        TDOMElement(SubNode).SetAttribute('xml:id', RegionStr);
        if VA = svaTop then
        begin
          TDOMElement(SubNode).SetAttribute('tts:origin', '0% 0%');
          TDOMElement(SubNode).SetAttribute('tts:extent', '100% 15%');
          TDOMElement(SubNode).SetAttribute('tts:displayAlign', 'before');
        end
        else if VA = svaCenter then
        begin
          TDOMElement(SubNode).SetAttribute('tts:origin', '0% 40%');
          TDOMElement(SubNode).SetAttribute('tts:extent', '100% 20%');
          TDOMElement(SubNode).SetAttribute('tts:displayAlign', 'center');
        end
        else
        begin
          TDOMElement(SubNode).SetAttribute('tts:origin', '0% 85%');
          TDOMElement(SubNode).SetAttribute('tts:extent', '100% 15%');
          TDOMElement(SubNode).SetAttribute('tts:displayAlign', 'after');
        end;
        if HA = shaLeft then
          TDOMElement(SubNode).SetAttribute('tts:textAlign', 'left')
        else if HA = shaRight then
          TDOMElement(SubNode).SetAttribute('tts:textAlign', 'right')
        else
          TDOMElement(SubNode).SetAttribute('tts:textAlign', 'center');
        LayoutNode.AppendChild(SubNode);
      end;

      TDOMElement(Element).SetAttribute('region', RegionStr);
      if si >= 0 then
        TDOMElement(Element).SetAttribute('style', STab.Ids[si]);

      // Antes: 'hh:mm:ss:zz' (centésimas con ':'), que se leía como cuadros
      TDOMElement(Element).SetAttribute('begin', TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss.zzz'));
      TDOMElement(Element).SetAttribute('xml:id', 'p' + IntToStr(i + 1));
      TDOMElement(Element).SetAttribute('end', TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss.zzz'));
      // Antes: los tags y <br/> se escribían como texto escapado (&lt;br/&gt;)
      // with styles, italic and bold in line: the "italic" and "bold" styles
      // would change the font and color of the style of the <p> to white
      AppendStyledTextDirect(XmlDoc, Element, iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]), HasStyles);
      Node.AppendChild(Element);
    end;

    AddDecorationStyle('underline', 'underline');
    AddDecorationStyle('strikeout', 'lineThrough');

    try
      StringList.Clear;
      XMLToStrings(XmlDoc, StringList, Subtitles.ReplaceEntities);
      JoinXMLElementLines(StringList, 'p');

      if not FileName.IsEmpty then
        StringList.SaveToFile(FileName, TEncoding.UTF8); // must be encoded using UTF-8

      Result := True;
    except
    end;
  finally
    XmlDoc.Free;
    WrittenRegions.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
