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

unit ITunesTimedText;

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

uses UWSubtitleAPI.Tags, UWSubtitleAPI.Utils, UWSystem.SysUtils;

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
  RegionID, AlignVal, TextAlignVal, NewText: String;
  CurBegin, CurEnd: Integer;
  IsContinuation: Boolean;

  function GetTime(const S: String): Integer;
  begin
    if Length(Trim(S).Replace(';', ':').Split(':')) = 4 then // hh:mm:ss:ff
      Result := SMPTEStringToMS(Trim(S), Subtitles.FrameRate, Subtitles.TimecodeMode = tcDF)
    else
      Result := TTMLTimeToMS(S, Subtitles.FrameRate);
  end;

begin
  Result := False;
  XmlDoc := NIL;

  StringsToXML(XmlDoc, SubtitleFile);
  // Antes: RegionMap se creaba siempre y no se liberaba si el XML era inválido
  if not Assigned(XmlDoc) then Exit;

  RegionMap := TStringList.Create;
  try
      Node := XMLFindNodeByName(XmlDoc, 'tt');
      Subtitles.FrameRate := FPS;
      fr := FPS; // Antes: sin ttp:frameRate, fr quedaba sin inicializar

      if Assigned(Node) and XMLHasAttribute(Node, 'xml:lang') and
        (Trim(XMLGetAttrValue(Node, 'xml:lang')) <> '') then
        Subtitles.FormatProperties^.TimedText.Language := Trim(XMLGetAttrValue(Node, 'xml:lang'));


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

      // Regiones
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

              // Antes: tts:textAlign nunca se leía
              RegionMap.Values[RegionID] := AlignVal + '|' + XMLGetAttrValue(RegionNode, 'tts:textAlign');
            end;
          end;
          RegionNode := RegionNode.NextSibling;
        end;
      end;

      // <p> con UNIFICACIÓN de tiempos
      Node := XMLFindNodeByName(XmlDoc, 'p');
      if Assigned(Node) then
        repeat
          if Node.HasAttributes then
          begin
            // Antes: siempre como non-drop (un archivo dropNTSC se corría ~3.6 s
            // por hora) y sin aceptar tiempos de reloj ni de desplazamiento
            CurBegin := 0;
            CurEnd := 0;
            if XMLHasAttribute(Node, 'begin') then
              CurBegin := GetTime(XMLGetAttrValue(Node, 'begin'));
            if XMLHasAttribute(Node, 'end') then
              CurEnd := GetTime(XMLGetAttrValue(Node, 'end'))
            else if XMLHasAttribute(Node, 'dur') then
              CurEnd := CurBegin + GetTime(XMLGetAttrValue(Node, 'dur'));

            // Extraer y limpiar el texto
            // Antes: el XML del <p> se volvía a serializar y se pasaba por regex,
            // que perdía los spans anidados, los tts:* en línea y los <tt:br/>
            NewText := HTMLTagsToTS(ExtractStyledTTMLText(Node));
            // Cursiva en toda la entrada: tts:fontStyle="italic" en el <p> (como FCP)
            if (NewText <> '') and SameText(XMLGetAttrValue(Node, 'tts:fontStyle'), 'italic') then
              NewText := '{\i1}' + NewText + '{\i0}';

            // Lógica de UNIFICACIÓN
            IsContinuation := (Subtitles.Count > 0) and
                             (Subtitles[Subtitles.Count-1].InitialTime = CurBegin) and
                             (Subtitles[Subtitles.Count-1].FinalTime = CurEnd);

            if IsContinuation then
            begin
              // Concatenar texto si el tiempo es idéntico
              Subtitles.Text[Subtitles.Count-1] := Subtitles[Subtitles.Count-1].Text + LineEnding + NewText;
            end
            else
            begin
              // Nuevo subtítulo
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
                else if LowerCase(RegionID).Contains('top') or (RegionID = 'sh0') then
                  Item.VAlign := svaTop
                // Antes: 'bottomCenter' contiene 'center' y quedaba centrado
                else if LowerCase(RegionID).Contains('bottom') then
                  Item.VAlign := svaBottom
                else if LowerCase(RegionID).Contains('middle') or LowerCase(RegionID).Contains('center') then
                  Item.VAlign := svaCenter
                else
                  Item.VAlign := svaBottom;
              end
              else
                Item.VAlign := svaBottom;

              Subtitles.Add(Item, NIL);
            end;
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
  Root, Element, Node, StylingNode, LayoutNode, DivNode : TDOMNode;
  i, it, ft : Integer;
  NewFPS: Double;
  txt, StripTxt: String;
  NeedTop: Boolean;

  // Preset de <style>: solo se escriben los que el texto realmente referencia
  procedure AddPresetStyle(const AName, AFontStyle, AFontWeight, ADecoration: String);
  var
    S: TDOMNode;
  begin
    S := XmlDoc.CreateElement('style');
    TDOMElement(S).SetAttribute('xml:id', AName);
    TDOMElement(S).SetAttribute('tts:color', 'white');
    TDOMElement(S).SetAttribute('tts:fontFamily', 'sansSerif');
    TDOMElement(S).SetAttribute('tts:fontSize', '100%');
    // Antes el estilo bold tenía tts:fontStyle="bold" (valor inválido)
    TDOMElement(S).SetAttribute('tts:fontStyle', AFontStyle);
    TDOMElement(S).SetAttribute('tts:fontWeight', AFontWeight);
    if ADecoration <> '' then
      TDOMElement(S).SetAttribute('tts:textDecoration', ADecoration);
    StylingNode.AppendChild(S);
  end;

  // Región fija de iTT/FCP (iTT centra todo el texto)
  procedure AddRegion(const AId, ADisplayAlign, AExtent, AOrigin: String);
  var
    R: TDOMNode;
  begin
    R := XmlDoc.CreateElement('region');
    TDOMElement(R).SetAttribute('xml:id', AId);
    TDOMElement(R).SetAttribute('tts:displayAlign', ADisplayAlign);
    TDOMElement(R).SetAttribute('tts:extent', AExtent);
    TDOMElement(R).SetAttribute('tts:origin', AOrigin);
    LayoutNode.AppendChild(R);
  end;

begin
  Result := False;
  NeedTop := False;
  XmlDoc := TXMLDocument.Create;
  try
    Root := XmlDoc.CreateElement('tt');
      // Mismos namespaces y orden de atributos que escribe Final Cut Pro
      TDOMElement(Root).SetAttribute('xmlns:vt', 'http://namespace.itunes.apple.com/itt/ttml-extension#vertical');
      TDOMElement(Root).SetAttribute('xmlns:ttp', 'http://www.w3.org/ns/ttml#parameter');
      TDOMElement(Root).SetAttribute('xmlns:ittp', 'http://www.w3.org/ns/ttml/profile/imsc1#parameter');
      TDOMElement(Root).SetAttribute('xmlns:tt_feature', 'http://www.w3.org/ns/ttml/feature/');
      TDOMElement(Root).SetAttribute('xmlns:ebutts', 'urn:ebu:tt:style');
      // Antes: '#style', la URI del namespace de estilos TTML es '#styling'
      TDOMElement(Root).SetAttribute('xmlns:tts', 'http://www.w3.org/ns/ttml#styling');
      TDOMElement(Root).SetAttribute('xmlns:tt_extension', 'http://www.w3.org/ns/ttml/extension/');
      TDOMElement(Root).SetAttribute('xmlns:tt_profile', 'http://www.w3.org/ns/ttml/profile/');
      TDOMElement(Root).SetAttribute('xmlns:ttm', 'http://www.w3.org/ns/ttml#metadata');
      TDOMElement(Root).SetAttribute('xmlns:ry', 'http://namespace.itunes.apple.com/itt/ttml-extension#ruby');
      TDOMElement(Root).SetAttribute('xmlns:itts', 'http://www.w3.org/ns/ttml/profile/imsc1#styling');
      TDOMElement(Root).SetAttribute('xmlns', 'http://www.w3.org/ns/ttml');
      TDOMElement(Root).SetAttribute('xml:lang', TimedTextLanguage(Subtitles));

      if Subtitles.TimecodeMode = tcDF then
        TDOMElement(Root).SetAttribute('ttp:dropMode', 'dropNTSC')
      else
        TDOMElement(Root).SetAttribute('ttp:dropMode', 'nonDrop');

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

      // En TS ttp:timeBase también indica el modo SMPTE del proyecto
      if Subtitles.TimeBase = stbSMPTE then
        TDOMElement(Root).SetAttribute('ttp:timeBase', 'smpte')
      else
        TDOMElement(Root).SetAttribute('ttp:timeBase', 'media');
      TDOMElement(Root).SetAttribute('xmlns:tt', 'http://www.w3.org/ns/ttml');
      TDOMElement(Root).SetAttribute('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');

      XmlDoc.Appendchild(Root);
    Root := XmlDoc.DocumentElement;

    // Sin <metadata>: FCP no lo escribe y un <ttm:title/> vacío no aporta nada
    Element := XmlDoc.CreateElement('head');
    Root.AppendChild(Element);
      StylingNode := XmlDoc.CreateElement('styling');
      Element.AppendChild(StylingNode);
      AddPresetStyle('normal', 'normal', 'normal', '');

      LayoutNode := XmlDoc.CreateElement('layout');
      Element.AppendChild(LayoutNode);

    // <body> como en FCP: color, región y estilo por defecto
    Element := XmlDoc.CreateElement('body');
      TDOMElement(Element).SetAttribute('tts:color', 'white');
      TDOMElement(Element).SetAttribute('region', 'bottom');
      TDOMElement(Element).SetAttribute('style', 'normal');
    Root.AppendChild(Element);

    DivNode := XmlDoc.CreateElement('div');
    Element.AppendChild(DivNode);

    for i := FromItem to ToItem do
    begin
      it := Subtitles[i].InitialTime;
      ft := Subtitles[i].FinalTime;

      Element := XmlDoc.CreateElement('p');

      // Orden como FCP: begin/end, después fontStyle y region
      TDOMElement(Element).SetAttribute('begin', MSToHHMMSSFFTime(it, NewFPS, Subtitles.TimecodeMode));
      TDOMElement(Element).SetAttribute('end', MSToHHMMSSFFTime(ft, NewFPS, Subtitles.TimecodeMode));

      // Cursiva en toda la entrada: tts:fontStyle="italic" en el <p> (como FCP)
      txt := iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]);
      if IsEntireTextItalic(txt, StripTxt) then
      begin
        TDOMElement(Element).SetAttribute('tts:fontStyle', 'italic');
        txt := StripTxt;
      end;

      // Apple/FCP solo admite top y bottom: lo que no es arriba va abajo
      // (antes se escribía también 'middle', que iTT no admite)
      if Subtitles[i].VAlign = svaTop then
      begin
        NeedTop := True;
        TDOMElement(Element).SetAttribute('region', 'top');
      end
      else
        TDOMElement(Element).SetAttribute('region', 'bottom');

      // Antes: <span style="italic"> apuntando al <head>; FCP usa
      // <span tts:fontStyle="italic"> en línea
      AppendStyledTextDirect(XmlDoc, Element, txt, True);

      DivNode.AppendChild(Element);
    end;

    // Regiones: top antes que bottom, solo las usadas ('bottom' siempre,
    // porque el <body> la referencia)
    if NeedTop then
      AddRegion('top', 'before', '100% 15%', '0% 0%');
    AddRegion('bottom', 'after', '100% 15%', '0% 85%');

    try
      StringList.Clear;
      XMLToStrings(XmlDoc, StringList, Subtitles.ReplaceEntities);
      // Declaración como la de FCP (sin encoding; UTF-8 por defecto)
      if (StringList.Count > 0) and (Pos('<?xml', StringList[0]) = 1) then
        StringList[0] := '<?xml version="1.0"?>';
      // Antes: un <p> que empezaba con <span> se partía en 3 líneas y
      // esos saltos se volvían parte del texto al releer
      JoinXMLElementLines(StringList, 'p');
      IndentXMLLines(StringList, 2); // 2 espacios por nivel, como FCP

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
