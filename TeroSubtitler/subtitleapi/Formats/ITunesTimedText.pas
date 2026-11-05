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

type

  TITTLanguage = record
    Name, Code: String;
  end;

const

  ITTLanguages: array[0..68] of TITTLanguage = (
    (Name: 'Afrikaans'; Code: 'af'),
    (Name: 'Arabic'; Code: 'ar'),
    (Name: 'Bangla'; Code: 'bn'),
    (Name: 'Bulgarian'; Code: 'bg'),
    (Name: 'Catalan'; Code: 'ca'),
    (Name: 'Chinese (Cantonese)'; Code: 'yue-Hant'),
    (Name: 'Chinese (Simplified)'; Code: 'zh-Hans'),
    (Name: 'Chinese (Traditional)'; Code: 'zh-Hant'),
    (Name: 'Croatian'; Code: 'hr'),
    (Name: 'Czech'; Code: 'cs'),
    (Name: 'Danish'; Code: 'da'),
    (Name: 'Dutch'; Code: 'nl'),
    (Name: 'English'; Code: 'en'),
    (Name: 'English (Australia)'; Code: 'en-AU'),
    (Name: 'English (Canada)'; Code: 'en-CA'),
    (Name: 'English (United Kingdom)'; Code: 'en-GB'),
    (Name: 'Estonian'; Code: 'et'),
    (Name: 'Finnish'; Code: 'fi'),
    (Name: 'French (Belgium)'; Code: 'fr-BE'),
    (Name: 'French (Canada)'; Code: 'fr-CA'),
    (Name: 'French (France)'; Code: 'fr-FR'),
    (Name: 'French (Switzerland)'; Code: 'fr-CH'),
    (Name: 'German'; Code: 'de'),
    (Name: 'German (Austria)'; Code: 'de-AT'),
    (Name: 'German (Germany)'; Code: 'de-DE'),
    (Name: 'German (Switzerland)'; Code: 'de-CH'),
    (Name: 'Greek'; Code: 'el'),
    (Name: 'Greek (Cyprus)'; Code: 'el-CY'),
    (Name: 'Hebrew'; Code: 'he'),
    (Name: 'Hindi'; Code: 'hi'),
    (Name: 'Hungarian'; Code: 'hu'),
    (Name: 'Icelandic'; Code: 'is'),
    (Name: 'Indonesian'; Code: 'id'),
    (Name: 'Italian'; Code: 'it'),
    (Name: 'Japanese'; Code: 'ja'),
    (Name: 'Kannada'; Code: 'kn'),
    (Name: 'Kazakh'; Code: 'kk'),
    (Name: 'Korean'; Code: 'ko'),
    (Name: 'Lao'; Code: 'lo'),
    (Name: 'Latvian'; Code: 'lv'),
    (Name: 'Lithuanian'; Code: 'lt'),
    (Name: 'Luxembourgish'; Code: 'lb'),
    (Name: 'Malay'; Code: 'ms'),
    (Name: 'Malayalam'; Code: 'ml'),
    (Name: 'Maltese'; Code: 'mt'),
    (Name: 'Marathi'; Code: 'mr'),
    (Name: 'Norwegian'; Code: 'no'),
    (Name: 'Portuguese (Brazil)'; Code: 'pt-BR'),
    (Name: 'Portuguese (Portugal)'; Code: 'pt-PT'),
    (Name: 'Punjabi'; Code: 'pa'),
    (Name: 'Romanian'; Code: 'ro'),
    (Name: 'Russian'; Code: 'ru'),
    (Name: 'Scottish Gaelic'; Code: 'gd'),
    (Name: 'Slovak'; Code: 'sk'),
    (Name: 'Slovenian'; Code: 'sl'),
    (Name: 'Spanish (Latin America)'; Code: 'es-419'),
    (Name: 'Spanish (Mexico)'; Code: 'es-MX'),
    (Name: 'Spanish (Spain)'; Code: 'es-ES'),
    (Name: 'Swedish'; Code: 'sv'),
    (Name: 'Tagalog'; Code: 'tl'),
    (Name: 'Tamil'; Code: 'ta'),
    (Name: 'Telugu'; Code: 'te'),
    (Name: 'Thai'; Code: 'th'),
    (Name: 'Turkish'; Code: 'tr'),
    (Name: 'Ukrainian'; Code: 'uk'),
    (Name: 'Urdu'; Code: 'ur'),
    (Name: 'Vietnamese'; Code: 'vi'),
    (Name: 'Welsh'; Code: 'cy'),
    (Name: 'Zulu'; Code: 'zu'));

// xml:lang for the iTT files ('en' if none)
function ITTLanguage(const ASubtitles: TUWSubtitles): String;
function ITTLanguageIndex(const ACode: String): Integer;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Tags, UWSubtitleAPI.Utils, UWSystem.SysUtils;

type

  TITTEntryStyle = record
    Visible   : Boolean; // tiene texto visible
    Italic,
    Bold,
    Underline : Boolean; // en toda la entrada
    Color     : String;  // 'rgba(R,G,B,255)' en toda la entrada, o ''
    Text      : String;  // el texto con solo los tags que van en <span>
  end;

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function ITTLanguage(const ASubtitles: TUWSubtitles): String;
begin
  Result := '';
  if Assigned(ASubtitles) and Assigned(ASubtitles.FormatProperties) then
    Result := Trim(ASubtitles.FormatProperties^.ITunesTimedText.Language);
  if Result.IsEmpty then
    Result := 'en';
end;

// -----------------------------------------------------------------------------

function ITTLanguageIndex(const ACode: String): Integer;
var
  i: Integer;
begin
  for i := 0 to High(ITTLanguages) do
    if SameText(ITTLanguages[i].Code, Trim(ACode)) then
      Exit(i);
  Result := -1;
end;

// -----------------------------------------------------------------------------

// Lee los tags TS de una entrada ({\i1}, {\b1}, {\u1}, {\s1}, {\c&BBGGRR&},
// también varios en un bloque como {\b1\c&H0000FF&}) y dice qué formato cubre
// todo el texto visible. Text queda sin ese formato (va en el <p>), sin la
// negrita ni el color parciales (Apple no los admite) y con un tag por bloque.

function AnalyzeITTEntry(const Txt: String): TITTEntryStyle;
type
  TTok = record
    IsTag : Boolean;
    S     : String;  // texto, o el nombre del tag: i, b, u, s, c
    TagOn : Boolean;
    Color : String;  // 'rgba(R,G,B,255)' al abrir un color
  end;
var
  Toks: array of TTok;
  i, j, n, Count: Integer;
  Block, Tag, Num, Hex: String;
  Parts: TStringArray;
  It, Bo, Un: Boolean;
  Col, FirstCol: String;
  AllIt, AllBo, AllUn, SameCol, HaveFirst: Boolean;

  procedure AddText(const C: String);
  begin
    if (Length(Toks) > 0) and not Toks[High(Toks)].IsTag then
      Toks[High(Toks)].S := Toks[High(Toks)].S + C
    else
    begin
      SetLength(Toks, Length(Toks)+1);
      Toks[High(Toks)].IsTag := False;
      Toks[High(Toks)].S := C;
    end;
  end;

  procedure AddTag(const AName: String; const AOn: Boolean; const AColor: String = '');
  begin
    SetLength(Toks, Length(Toks)+1);
    Toks[High(Toks)].IsTag := True;
    Toks[High(Toks)].S := AName;
    Toks[High(Toks)].TagOn := AOn;
    Toks[High(Toks)].Color := AColor;
  end;

  function AllDigits(const S: String): Boolean;
  var
    k: Integer;
  begin
    Result := S <> '';
    for k := 1 to Length(S) do
      if not (S[k] in ['0'..'9']) then Exit(False);
  end;

begin
  Result.Visible := False;
  Result.Italic := False;
  Result.Bold := False;
  Result.Underline := False;
  Result.Color := '';
  Result.Text := '';
  Toks := NIL;

  // tokens
  n := Length(Txt);
  i := 1;
  while i <= n do
  begin
    j := 0;
    if (Txt[i] = '{') and (i < n) and (Txt[i+1] = '\') then
      j := Pos('}', Txt, i+1);
    if j = 0 then
    begin
      AddText(Txt[i]);
      Inc(i);
      Continue;
    end;

    Block := Copy(Txt, i+1, j-i-1);
    Parts := Block.Split(['\']);
    for Tag in Parts do
    begin
      if Tag = '' then Continue;
      if (Tag[1] in ['i', 'u', 's', 'b']) and AllDigits(Copy(Tag, 2, MaxInt)) then
        AddTag(Tag[1], StrToIntDef(Copy(Tag, 2, MaxInt), 0) <> 0)
      else if (Tag = 'c') or (Tag = '1c') then
        AddTag('c', False)
      else if (Tag[1] = 'c') or (Copy(Tag, 1, 2) = '1c') then
      begin
        // c&BBGGRR&, c&HBBGGRR&, 1c&H...& -> #RRGGBB
        Hex := Copy(Tag, Pos('c', Tag)+1, MaxInt);
        Hex := StringReplace(Hex, '&', '', [rfReplaceAll]);
        if (Hex <> '') and (UpCase(Hex[1]) = 'H') then Delete(Hex, 1, 1);
        if (Length(Hex) >= 1) and (Length(Hex) <= 6) then
        begin
          Hex := UpperCase(StringOfChar('0', 6-Length(Hex)) + Hex);
          // como FCP: rgba(R,G,B,255)
          Num := Format('rgba(%d,%d,%d,255)', [StrToInt('$' + Copy(Hex, 5, 2)),
            StrToInt('$' + Copy(Hex, 3, 2)), StrToInt('$' + Copy(Hex, 1, 2))]);
          AddTag('c', True, Num);
        end;
      end;
      // los demás tags ({\an8}, {\pos()}...) no existen en iTT
    end;
    i := j + 1;
  end;

  // ¿qué formato cubre todo el texto visible?
  It := False; Bo := False; Un := False; Col := '';
  AllIt := True; AllBo := True; AllUn := True; SameCol := True;
  HaveFirst := False; FirstCol := '';
  Count := 0;
  for i := 0 to High(Toks) do
    if Toks[i].IsTag then
    begin
      case Toks[i].S of
        'i': It := Toks[i].TagOn;
        'b': Bo := Toks[i].TagOn;
        'u': Un := Toks[i].TagOn;
        'c': if Toks[i].TagOn then Col := Toks[i].Color else Col := '';
      end;
    end
    else
      for j := 1 to Length(Toks[i].S) do
        if not (Toks[i].S[j] in [' ', #9, #10, #13]) then
        begin
          Inc(Count);
          if not It then AllIt := False;
          if not Bo then AllBo := False;
          if not Un then AllUn := False;
          if not HaveFirst then
          begin
            FirstCol := Col;
            HaveFirst := True;
          end
          else if Col <> FirstCol then
            SameCol := False;
        end;

  Result.Visible := Count > 0;
  if Result.Visible then
  begin
    Result.Italic    := AllIt;
    Result.Bold      := AllBo;
    Result.Underline := AllUn;
    if SameCol then Result.Color := FirstCol;
  end;

  // el texto: cursiva y subrayado parciales en <span>, tachado como antes,
  // negrita y color fuera (en el <p> si cubren todo, si no se pierden)
  for i := 0 to High(Toks) do
    if not Toks[i].IsTag then
      Result.Text := Result.Text + Toks[i].S
    else
      case Toks[i].S of
        'i': if not Result.Italic then Result.Text := Result.Text + iff(Toks[i].TagOn, '{\i1}', '{\i0}');
        'u': if not Result.Underline then Result.Text := Result.Text + iff(Toks[i].TagOn, '{\u1}', '{\u0}');
        's': Result.Text := Result.Text + iff(Toks[i].TagOn, '{\s1}', '{\s0}');
      end;
end;

// -----------------------------------------------------------------------------

// Formato de un <p> o del <body> al importar: el del <p> manda sobre el del
// <body>; el blanco del <body> es el color por defecto, no un formato

procedure ReadITTStyles(const ANode: TDOMNode; var AItalic, ABold, AUnderline: Boolean; var AColor: String);
var
  s, t: String;
begin
  if not Assigned(ANode) then Exit;
  if XMLHasAttribute(ANode, 'tts:fontStyle') then
    AItalic := SameText(XMLGetAttrValue(ANode, 'tts:fontStyle'), 'italic');
  if XMLHasAttribute(ANode, 'tts:fontWeight') then
    ABold := SameText(XMLGetAttrValue(ANode, 'tts:fontWeight'), 'bold');
  if XMLHasAttribute(ANode, 'tts:textDecoration') then
  begin
    AUnderline := False;
    s := LowerCase(XMLGetAttrValue(ANode, 'tts:textDecoration'));
    for t in s.Split([' ']) do
      if t = 'underline' then AUnderline := True;
  end;
  if XMLHasAttribute(ANode, 'tts:color') then
  begin
    AColor := TTMLColorToBGR(XMLGetAttrValue(ANode, 'tts:color'));
    if AColor = 'FFFFFF' then AColor := '';
  end;
end;

// -----------------------------------------------------------------------------

{ TUWITunesTimedText }

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
  Node, LayoutNode, RegionNode, BodyNode : TDOMNode;
  Item   : TUWSubtitleItem;
  fr     : Double;
  RegionMap: TStringList;
  RegionID, AlignVal, TextAlignVal, NewText: String;
  CurBegin, CurEnd: Integer;
  IsContinuation: Boolean;
  BodyRegion, PColor: String;
  BodyItalic, BodyBold, BodyUnderline, PItalic, PBold, PUnderline: Boolean;
  BodyColor: String;

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
        Subtitles.FormatProperties^.ITunesTimedText.Language := Trim(XMLGetAttrValue(Node, 'xml:lang'));


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

      // Formato y región del <body>, para todas las entradas (como FCP)
      BodyItalic := False; BodyBold := False; BodyUnderline := False;
      BodyColor := ''; BodyRegion := '';
      BodyNode := XMLFindNodeByName(XmlDoc, 'body');
      if Assigned(BodyNode) then
      begin
        ReadITTStyles(BodyNode, BodyItalic, BodyBold, BodyUnderline, BodyColor);
        BodyRegion := XMLGetAttrValue(BodyNode, 'region');
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
            // Formato de toda la entrada: en el <p> o en el <body> (como FCP).
            // Antes solo se leía la cursiva del <p>
            PItalic := BodyItalic; PBold := BodyBold; PUnderline := BodyUnderline;
            PColor := BodyColor;
            ReadITTStyles(Node, PItalic, PBold, PUnderline, PColor);
            if NewText <> '' then
            begin
              if PItalic then NewText := '{\i1}' + NewText + '{\i0}';
              if PBold then NewText := '{\b1}' + NewText + '{\b0}';
              if PUnderline then NewText := '{\u1}' + NewText + '{\u0}';
              if PColor <> '' then NewText := '{\c&' + PColor + '&}' + NewText + '{\c}';
            end;

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

              // Determinar alineación vertical (sin region en el <p>, la del <body>)
              if XMLHasAttribute(Node, 'region') or (BodyRegion <> '') then
              begin
                RegionID := XMLGetAttrValue(Node, 'region');
                if RegionID = '' then RegionID := BodyRegion;
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
  NeedTop, AllTop, AnyVisible, AllItalic, AllBold, AllUnderline, AllColor: Boolean;
  Styles: array of TITTEntryStyle;
  St: TITTEntryStyle;
  RefColor: String;

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
      TDOMElement(Root).SetAttribute('xml:lang', ITTLanguage(Subtitles));

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

    // El formato de cada entrada, y lo que comparten todas (va en el <body>)
    Styles := NIL;
    SetLength(Styles, ToItem - FromItem + 1);
    AnyVisible := False; AllItalic := True; AllBold := True; AllUnderline := True;
    AllColor := True; AllTop := ToItem >= FromItem; RefColor := '';
    for i := FromItem to ToItem do
    begin
      St := AnalyzeITTEntry(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));
      Styles[i-FromItem] := St;
      if Subtitles[i].VAlign <> svaTop then AllTop := False;
      if not St.Visible then Continue; // una entrada vacía no cuenta
      if not St.Italic then AllItalic := False;
      if not St.Bold then AllBold := False;
      if not St.Underline then AllUnderline := False;
      if not AnyVisible then
      begin
        AnyVisible := True;
        RefColor := St.Color; // el de la primera entrada visible
      end;
      if (St.Color = '') or (St.Color <> RefColor) then AllColor := False;
    end;
    if not AnyVisible then
    begin
      AllItalic := False; AllBold := False; AllUnderline := False; AllColor := False;
    end;

    // <body> como en FCP: color, región y estilo por defecto, y el formato
    // que tienen todas las entradas (una sola vez, no en cada <p>)
    Element := XmlDoc.CreateElement('body');
      if AllColor then
        TDOMElement(Element).SetAttribute('tts:color', RefColor)
      else
        TDOMElement(Element).SetAttribute('tts:color', 'white');
      if AllItalic then TDOMElement(Element).SetAttribute('tts:fontStyle', 'italic');
      if AllBold then TDOMElement(Element).SetAttribute('tts:fontWeight', 'bold');
      if AllUnderline then TDOMElement(Element).SetAttribute('tts:textDecoration', 'underline');
      // FCP deja region="bottom" aunque todas vayan arriba (y no haya 'bottom')
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

      // Formato de toda la entrada en el <p> (como FCP), salvo el que ya va
      // en el <body>. Antes solo la cursiva; la negrita y el color iban en
      // <span>, que Apple no admite
      St := Styles[i-FromItem];
      if St.Visible then
      begin
        // mismo orden que FCP: color, fontStyle, fontWeight, textDecoration
        if (St.Color <> '') and not AllColor then
          TDOMElement(Element).SetAttribute('tts:color', St.Color);
        if St.Italic and not AllItalic then
          TDOMElement(Element).SetAttribute('tts:fontStyle', 'italic');
        if St.Bold and not AllBold then
          TDOMElement(Element).SetAttribute('tts:fontWeight', 'bold');
        if St.Underline and not AllUnderline then
          TDOMElement(Element).SetAttribute('tts:textDecoration', 'underline');
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
      AppendStyledTextDirect(XmlDoc, Element, St.Text, True);

      DivNode.AppendChild(Element);
    end;

    // Regiones: top antes que bottom, solo las usadas (si todas van arriba no
    // hay 'bottom', como FCP)
    if NeedTop then
      AddRegion('top', 'before', '100% 15%', '0% 0%');
    if not AllTop then
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
