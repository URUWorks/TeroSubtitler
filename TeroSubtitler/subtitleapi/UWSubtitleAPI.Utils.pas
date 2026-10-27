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

unit UWSubtitleAPI.Utils;

// -----------------------------------------------------------------------------

interface

uses
  Classes, UWSubtitleAPI, laz2_DOM, laz2_XMLRead, laz2_XMLWrite, SysUtils,
  StrUtils;

procedure StringsToXML(var XmlDoc: TXMLDocument; const StringList: TUWStringList);
procedure XMLToStrings(const XmlDoc: TXMLDocument; const StringList: TUWStringList; const ReplaceEntities: Boolean = True);

function XMLFindNodeByName(const XmlDoc: TXMLDocument; const NodeName: String): TDOMNode;
function XMLGetAttrValue(const ANode: TDOMNode; const AAttrName: String): String;
function XMLHasAttribute(const ANode: TDOMNode; const AAttrName: String): Boolean;
function TimedTextLanguage(const ASubtitles: TUWSubtitles): String;
function XMLExtractTextContent(const A: TDOMNodeList): String;
function XMLGetCorrectFPS(const AFPS: Double; const AMultiplier: String; const AForceBroadcastNTSC: Boolean  = False): Double;
// Texto con tags TS -> nodos DOM (texto, <br/>, <span>) hijos de Parent
procedure AppendStyledTextDirect(XmlDoc: TXMLDocument; Parent: TDOMNode; const Txt: string; const AInlineStyles: Boolean = False);
// Une en una sola línea cada <ATagName>...</ATagName> partido por el escritor XML
procedure JoinXMLElementLines(const AList: TUWStringList; const ATagName: String);
// ¿Algún elemento del subárbol referencia el estilo AStyle (atributo style)?
function DOMHasStyleRef(const ANode: TDOMNode; const AStyle: String): Boolean;
// Tiempo TTML (reloj o desplazamiento: h, m, s, ms, f, t) -> ms
function TTMLTimeToMS(const S: String; const AFPS: Double; const ATickRate: Double = 10000000): Integer;
// Recorre el DOM de un <p> TTML: <span> (style o tts:* en línea, anidados) y <br> -> texto con tags TS
function ExtractStyledTTMLText(const ANode: TDOMNode): String;
// ¿Todo el texto visible está en cursiva? Stripped es el texto sin esos tags
function IsEntireTextItalic(const Txt: String; out Stripped: String): Boolean;
// Indenta un XML que ya tiene un elemento por línea
procedure IndentXMLLines(const AList: TUWStringList; const AIndentSize: Integer = 2);
// Netflix solo admite cursiva: quita negrita, subrayado, tachado y color
function StripNonItalicTSFormatting(const Txt: String): String;

function HTMLReplaceEntities(const Input: String): String;
function HTMLDecode(const AStr: String): String;

function IsBinaryFormat(const AFileName: String): Boolean;

// -----------------------------------------------------------------------------

implementation

uses
  UWSubtitleAPI.Tags, UWSystem.StrUtils, UWSystem.TimeUtils, RegExpr;

// -----------------------------------------------------------------------------

procedure StringsToXML(var XmlDoc: TXMLDocument; const StringList: TUWStringList);
var
  Stream : TStringStream;
begin
  if not Assigned(StringList) then Exit;

  with TRegExpr.Create('<\?xml version="(.*?)"') do
  try
    if Exec(StringList[0]) then
    begin
      if Match[1].Length > 3 then
        StringList[0] := Replace(InputString, '<?xml version="1.0"', True);
    end;
  finally
    Free;
  end;

  Stream := TStringStream.Create(StringList.Text.Replace('& ', '&amp; '));
  try
    Stream.Position := 0;
    ReadXMLFile(XmlDoc, Stream, [xrfAllowSpecialCharsInAttributeValue, xrfPreserveWhiteSpace]);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure XMLToStrings(const XmlDoc: TXMLDocument; const StringList: TUWStringList; const ReplaceEntities: Boolean = True);
var
  Stream : TStringStream;
begin
  if not Assigned(XmlDoc) or not Assigned(StringList) then Exit;

  Stream := TStringStream.Create;
  try
    WriteXMLFile(XmlDoc, Stream); //[xwfPreserveWhiteSpace]
    Stream.Position := 0;

    if ReplaceEntities then
      StringList.Text := StrToXMLValue(Stream.DataString) //HTMLDecode(Stream.DataString) //HTMLReplaceEntities(Stream.DataString)
    else
      StringList.Text := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------

function XMLFindNodeByName(const XmlDoc: TXMLDocument; const NodeName: String): TDOMNode;

  function FindNode(ANode: TDOMNode): TDOMNode;
  var
    i: Integer;
  begin
    Result := NIL;
    if Assigned(ANode) then
    begin
      if ANode.NodeName = NodeName then
        Result := ANode
      else
        for i := 0 to ANode.ChildNodes.Count-1 do
        begin
          Result := FindNode(ANode.ChildNodes[i]);
          if Assigned(Result) then Break;
        end;
    end;
  end;

var
  i: Integer;
begin
  Result := NIL;
  if Assigned(XmlDoc) and (XmlDoc.ChildNodes.Count > 0) then
  begin
    for i := 0 to XmlDoc.ChildNodes.Count-1 do
    begin
      Result := FindNode(XmlDoc.ChildNodes[i]);
      if Assigned(Result) then Break;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function XMLGetAttrValue(const ANode: TDOMNode; const AAttrName: String): String;
var
  i: LongWord;
  Found: Boolean;
begin
  Result := '';
  if (ANode = NIL) or (ANode.Attributes = NIL) then Exit;

  Found := False;
  i := 0;
  while not Found and (i < ANode.Attributes.Length) do
  begin
    if ANode.Attributes.Item[i].NodeName = AAttrName then
    begin
      Found := True;
      Result := ANode.Attributes.Item[i].NodeValue;
    end;
    Inc(i);
  end;
end;

// -----------------------------------------------------------------------------

function XMLHasAttribute(const ANode: TDOMNode; const AAttrName: String): Boolean;
begin
  Result := XMLGetAttrValue(ANode, AAttrName) <> '';
end;

// -----------------------------------------------------------------------------

// xml:lang of the TTML formats (iTT, Timed Text, Netflix)
function TimedTextLanguage(const ASubtitles: TUWSubtitles): String;
begin
  Result := '';
  if Assigned(ASubtitles) and Assigned(ASubtitles.FormatProperties) then
    Result := Trim(ASubtitles.FormatProperties^.TimedText.Language);

  if Result.IsEmpty then
    Result := 'en';
end;

// -----------------------------------------------------------------------------

function DOMNodeToString(const A: TDOMNode): String;
var
  LStream : TStringStream;
begin
  LStream := TStringStream.Create;
  try
    WriteXML(A, LStream); //[xwfPreserveWhiteSpace]
    Result := ReplaceEnters(LStream.DataString, sLineBreak, '');
  finally
    LStream.Free;
  end;
end;

// -----------------------------------------------------------------------------

function XMLExtractTextContent(const A: TDOMNodeList): String;
var
  I : Integer;
begin
  Result := '';
  for I := 0 to A.Count-1 do
    Result := Result + DOMNodeToString(A[I]);

  Result := XMLTagsToTS(XMLValueToStr(Result));
end;

// -----------------------------------------------------------------------------

function XMLGetCorrectFPS(const AFPS: Double; const AMultiplier: String; const AForceBroadcastNTSC: Boolean = False): Double;
var
  Parts: TStringArray;
  Num, Den: Integer;
begin
  Result := AFPS;

  Parts := AMultiplier.Split(' ');
  if Length(Parts) <> 2 then
    Exit;

  Num := StrToIntDef(Parts[0], 1);
  Den := StrToIntDef(Parts[1], 1);

  if Den = 0 then
    Exit;

  if AForceBroadcastNTSC and (Num = 999) and (Den = 1000) and ((Abs(AFPS - 30.0) < 0.0001) or
    (Abs(AFPS - 60.0) < 0.0001) or (Abs(AFPS - 24.0) < 0.0001)) then
  begin
    Num := 1000;
    Den := 1001;
  end;

  Result := AFPS * (Num / Den);
end;

// -----------------------------------------------------------------------------
// Simple HTML decoding
// Author: Colin J.D. Stewart
// -----------------------------------------------------------------------------

function html_entity2char(const code: string): Char;
const
  html_code: array of string = (
    'nbsp', 'quot', 'amp', 'apos', 'lt', 'gt', 'iexcl', 'cent',
    'pound', 'curren', 'yen', 'brvbar', 'sect', 'uml', 'copy', 'ordf', 'laquo',
    'not', 'shy', 'reg', 'macr', 'deg', 'plusmn', 'sup2', 'sup3', 'acute',
    'micro', 'para', 'middot', 'cedil', 'sup1', 'ordm', 'raquo', 'frac14', 'frac12',
    'frac34', 'iquest', 'Agrave', 'Aacute', 'Acirc', 'Atilde', 'Auml', 'Aring', 'AElig',
    'Ccedil', 'Egrave', 'Eacute', 'Ecirc', 'Euml', 'Igrave', 'Iacute', 'Icirc', 'Iuml',
    'ETH', 'Ntilde', 'Ograve', 'Oacute', 'Ocirc', 'Otilde', 'Ouml', 'times', 'Oslash',
    'Ugrave', 'Uacute', 'Ucirc', 'Uuml', 'Yacute', 'THORN', 'szlig', 'agrave', 'aacute',
    'acirc', 'atilde', 'auml', 'aring', 'aelig', 'ccedil', 'egrave', 'eacute', 'ecirc',
    'euml', 'igrave', 'iacute', 'icirc', 'iuml', 'eth', 'ntilde', 'ograve', 'oacute',
    'ocirc', 'otilde', 'ouml', 'divide', 'oslash', 'ugrave', 'uacute', 'ucirc', 'uuml',
    'yacute', 'thorn', 'yuml', 'amp', 'bull', 'deg', 'infin', 'permil', 'sdot',
    'plusmn', 'dagger', 'mdash', 'not', 'micro', 'perp', 'par', 'euro', 'pound',
    'yen', 'cent', 'copy', 'reg', 'trade', 'alpha', 'beta', 'gamma', 'delta',
    'epsilon', 'zeta', 'eta', 'theta', 'iota', 'kappa', 'lambda', 'mu', 'nu',
    'xi', 'omicron', 'pi', 'rho', 'sigma', 'tau', 'upsilon', 'phi', 'chi',
    'psi', 'omega', 'Alpha', 'Beta', 'Gamma', 'Delta', 'Epsilon', 'Zeta', 'Eta',
    'Theta', 'Iota', 'Kappa', 'Lambda', 'Mu', 'Nu', 'Xi', 'Omicron', 'Pi',
    'Rho', 'Sigma', 'Tau', 'Upsilon', 'Phi', 'Chi', 'Psi', 'Omega'
  );

  chars: array of Cardinal = (
    32, 34, 38, 39, 60, 62, 161, 162,
    163, 164, 165, 166, 167, 168, 169, 170, 171,
    172, 173, 174, 175, 176, 177, 178, 179, 180,
    181, 182, 183, 184, 185, 186, 187, 188, 189,
    190, 191, 192, 193, 194, 195, 196, 197, 198,
    199, 200, 201, 202, 203, 204, 205, 206, 207,
    208, 209, 210, 211, 212, 213, 214, 215, 216,
    217, 218, 219, 220, 221, 222, 223, 224, 225,
    226, 227, 228, 229, 230, 231, 232, 233, 234,
    235, 236, 237, 238, 239, 240, 241, 242, 243,
    244, 245, 246, 247, 248, 249, 250, 251, 252,
    253, 254, 255, 38, 8226, 176, 8734, 8240, 8901,
    177, 8224, 8212, 172, 181, 8869, 8741, 8364, 163,
    165, 162, 169, 174, 8482, 945, 946, 947, 948,
    949, 950, 951, 952, 953, 954, 955, 956, 957,
    958, 959, 960, 961, 963, 964, 965, 966, 967,
    968, 969, 913, 914, 915, 916, 917, 918, 919,
    920, 921, 922, 923, 924, 925, 926, 927, 928,
    929, 931, 932, 933, 934, 935, 936, 937
  );

var
  i: Integer;


  function UCS4ToString(uch: UCS4Char): string; inline;
  var
    s: UCS4String;
  begin
    SetLength(s, 2);
    s[0] := uch;
    s[1] := 0;
    Result := UCS4StringToUnicodeString(s);
  end;

begin
  Result := Char(' ');
  if Length(code) < 2 then Exit;

  if code[1] = '#' then
  begin
    Result := UCS4ToString(StrToUInt(code.Substring(1)))[1];
  end
  else
  begin
    for i := Low(html_code) to High(html_code) do
    begin
      if html_code[i] = code then
        Exit(UCS4ToString(chars[i])[1]);
    end;
  end;
end;

// -----------------------------------------------------------------------------
// run through the input and replace html codes
// -----------------------------------------------------------------------------

function HTMLReplaceEntities(const Input: String): String;
var
  startPos, endPos: Integer;
begin
  startPos := 0;
  endPos   := 0;

  Result := '';
  repeat
    endPos := Input.IndexOf('&', startPos);
    if endPos = -1 then Break;

    Result := Result + Input.Substring(startPos, endPos-startPos);

    startPos := Input.IndexOf(';', endPos)+1;
    if startPos = 0 then Break;

    Result := Result + html_entity2char(Input.Substring(endPos+1, startPos-endPos-2));
  until False;

  if startPos = 0 then
    Result := Input
  else
    Result := Result + Input.Substring(startPos+1);
end;

// -----------------------------------------------------------------------------

function HTMLDecode(const AStr: String): String;
begin
  Result := StringsReplace(AStr, [#13#13, #10#10, #10#13#10#13, '&amp;',
    '&quot;', '&lt;', '&gt;', '&nbsp;', '&auml;', '&ouml;', '&uuml;', '&Auml;',
    '&Ouml;', '&Uuml;', '&szlig;'],
    [#13, #10, #10#13, '&', '"', '<', '>', ' ', 'ä', 'ö', 'ü', 'Ä',
    'Ö', 'Ü', 'ß'], [rfReplaceAll]);
end;

// -----------------------------------------------------------------------------

function IsBinaryFormat(const AFileName: String): Boolean;
var
  Stream    : TFileStream;
  BOM       : array[0..39] of Byte;
  BytesRead : Integer;
  Ext       : String;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;

  Ext := LowerCase(ExtractFileExt(AFileName));

  // 1. Verificación rápida por extensión
  if (Ext = '.xls') or (Ext = '.xlsx') or (Ext = '.ods') or (Ext = '.890') or (Ext = '.cap') then
    Exit(True);

  // 2. Verificación profunda por Firma Binaria (Magic Numbers)
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    FillChar(BOM, SizeOf(BOM), 0);
    BytesRead := Stream.Read(BOM, SizeOf(BOM));

    // Si el archivo es extremadamente pequeño descartarlo inmediatamente
    if BytesRead < 8 then Exit;

    // EBU STL ("STL" en bytes 3,4,5)
    if (BytesRead >= 9) and (BOM[3] = $53) and (BOM[4] = $54) and (BOM[5] = $4C) and (BOM[8] = $2E) then
      Exit(True)
    // OLE2 (XLS Antiguo)
    else if (BOM[0] = $D0) and (BOM[1] = $CF) and (BOM[2] = $11) and (BOM[3] = $E0) and
            (BOM[4] = $A1) and (BOM[5] = $B1) and (BOM[6] = $1A) and (BOM[7] = $E1) then
      Exit(True)
    // Archivos ZIP (ODS, XLSX, DOCX, etc.) "PK" ($50, $4B)
    else if (BOM[0] = $50) and (BOM[1] = $4B) and (BOM[2] = $03) and (BOM[3] = $04) then
      Exit(True)
    // Cavena 890
    else if (BytesRead >= 40) and (BOM[0] = $00) and (BOM[1] = $00) and
            (BOM[2] = $03) and (BOM[3] = $04) and (BOM[38] = $00) and (BOM[39] = $00) then
      Exit(True);
  finally
    Stream.Free;
  end;
end;

// -----------------------------------------------------------------------------

function DOMHasStyleRef(const ANode: TDOMNode; const AStyle: String): Boolean;
var
  Child: TDOMNode;
begin
  Result := False;
  if not Assigned(ANode) then Exit;
  if (ANode.NodeType = ELEMENT_NODE) and (XMLGetAttrValue(ANode, 'style') = AStyle) then
    Exit(True);

  Child := ANode.FirstChild;
  while Assigned(Child) do
  begin
    if DOMHasStyleRef(Child, AStyle) then Exit(True);
    Child := Child.NextSibling;
  end;
end;

// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------

procedure AppendStyledTextDirect(XmlDoc: TXMLDocument; Parent: TDOMNode; const Txt: string; const AInlineStyles: Boolean);
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
    if AInlineStyles then
    begin
      // TTML1 en línea (Netflix): el formato va como atributo tts:* del propio
      // span, sin depender de un <style> del <head> al que apuntar con 'style'.
      if StyleName = 'italic' then
        Span.SetAttribute('tts:fontStyle', 'italic')
      else if StyleName = 'bold' then
        Span.SetAttribute('tts:fontWeight', 'bold')
      else if StyleName = 'underline' then
        Span.SetAttribute('tts:textDecoration', 'underline')
      else if StyleName = 'strikeout' then
        Span.SetAttribute('tts:textDecoration', 'lineThrough')
      else
        Span.SetAttribute('style', StyleName);
    end
    else
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

  function ParseASSColor(const S: string; var Advance: Integer): string;
  // Antes: exigía el prefijo '&H' (sintaxis ASS cruda \c&HBBGGRR&), pero
  // el tag TS canónico interno que usa esta biblioteca (el que EBU,
  // WebVTT, ASS, etc. realmente generan) es \c&BGR& — SIN la 'H'. Nunca
  // matcheaba, así que el color nunca llegaba a convertirse en
  // <span tts:color>: se perdía en silencio en cada guardado.
  var
    tmp: string;
    b, g, r: string;
  begin
    Result := '';
    Advance := 0;

    if (Length(S) < 2) or (S[1] <> '\') or not ((S[2] = 'c') or (S[2] = '1')) then Exit;

    tmp := Copy(S, 3, MaxInt); // todo lo que sigue a '\c' o '\1'
    if (Length(tmp) > 0) and (tmp[1] = '&') then Delete(tmp, 1, 1);
    if (Length(tmp) > 0) and (UpperCase(Copy(tmp, 1, 1)) = 'H') then Delete(tmp, 1, 1); // tolera '&H' también
    if (Length(tmp) > 0) and (tmp[Length(tmp)] = '&') then Delete(tmp, Length(tmp), 1);

    if Length(tmp) = 6 then
    begin
      b := Copy(tmp, 1, 2);
      g := Copy(tmp, 3, 2);
      r := Copy(tmp, 5, 2);
      Result := '#' + r + g + b;
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

      // Sin cierre: es una '{' literal del texto. Antes se hacia Break y se
      // perdia el resto de la entrada.
      if j > Length(Txt) then
      begin
        Buffer += Txt[i];
        Inc(i);
        Continue;
      end;

      TagContent := Copy(Txt, i + 1, j - i - 1);

      // Solo los bloques de override de ASS ({\b1}, {\an8}, {\c&H...&}...) son
      // tags. Un '{...}' sin barra inicial (p. ej. '{blank}') es TEXTO del
      // usuario y se escribe tal cual; antes se descartaba en silencio.
      if (TagContent = '') or (TagContent[1] <> '\') then
      begin
        Buffer += Copy(Txt, i, j - i + 1);
        i := j + 1;
        Continue;
      end;

      // estilos
      if TagContent = '\b1' then PushSpan('bold')
      else if TagContent = '\b0' then PopSpan
      else if TagContent = '\i1' then PushSpan('italic')
      else if TagContent = '\i0' then PopSpan
      else if TagContent = '\u1' then PushSpan('underline')
      else if TagContent = '\u0' then PopSpan
      else if TagContent = '\s1' then PushSpan('strikeout')
      else if TagContent = '\s0' then PopSpan
      else if TagContent = '\c' then
      begin
        // Cierre de color. Antes ParseASSColor('\c') devolvía '' y el tag se
        // ignoraba: el span de color NUNCA se cerraba y todo el texto que
        // seguía a el cierre quedaba dentro del color. Solo se cierra si el
        // tope de la pila ES un span de color (no se pisa uno de cursiva/negrita).
        if (Length(Stack) > 0) and (Stack[High(Stack)].GetAttribute('tts:color') <> '') then
          PopSpan;
      end
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

    // IGNORAR CARRIAGE RETURN — esta rama faltaba acá (TimedText ya la
    // tenía). En Windows el texto interno trae CRLF: sin esto, el #13 se
    // colaba dentro del nodo de texto ('DIALISCOPIO'#13) y el #10 generaba
    // el <br/>. Al volcar el XML a TStringList, un CR suelto cuenta como
    // fin de línea, así que cada subtítulo multilínea salía partido en
    // dos líneas del archivo — y como el CR quedaba DENTRO del texto, al
    // releerlo el parser XML lo normaliza a LF y se sumaba un salto de
    // más (línea en blanco extra entre las dos líneas del subtítulo).
    if Txt[i] = #13 then
    begin
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

procedure JoinXMLElementLines(const AList: TUWStringList; const ATagName: String);
var
  i, j, k, P: Integer;
  Acc, OpenA, OpenB, CloseTag: String;
begin
  OpenA    := '<' + ATagName + ' ';
  OpenB    := '<' + ATagName + '>';
  CloseTag := '</' + ATagName + '>';

  i := 0;
  while i < AList.Count do
  begin
    if (Pos(OpenA, AList[i]) = 1) or (Pos(OpenB, AList[i]) = 1) then
    begin
      // <p .../> autocerrado (sin hijos): no hay nada que unir.
      P := Pos('>', AList[i]);
      if (not ((P > 1) and (AList[i][P-1] = '/'))) and (Pos(CloseTag, AList[i]) = 0) then
      begin
        j := i + 1;
        while (j < AList.Count) and (Pos(CloseTag, AList[j]) = 0) do Inc(j);
        if j < AList.Count then // solo se une si realmente se encontró la línea de cierre
        begin
          Acc := AList[i];
          for k := i + 1 to j do Acc := Acc + AList[k];
          AList[i] := Acc;
          for k := j downto i + 1 do AList.Delete(k);
        end;
      end;
    end;
    Inc(i);
  end;
end;

// -----------------------------------------------------------------------------


function TTMLTimeToMS(const S: String; const AFPS: Double; const ATickRate: Double = 10000000): Integer;
var
  V   : String;
  FS  : TFormatSettings;
  P   : TStringArray;
  N   : Double;
begin
  // Antes: '1500ms' terminaba en 's' y se leía como segundos, 'h'/'m'/'f'
  // no se reconocían, 'hh:mm:ss.zz' se leía como cuadros y los ticks
  // ignoraban ttp:tickRate
  Result := 0;
  V := Trim(S);
  if V = '' then Exit;

  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';

  if AnsiEndsStr('ms', V) then
    Result := Round(StrToFloatDef(Copy(V, 1, Length(V)-2).Replace(',', '.'), 0, FS))
  else if AnsiEndsStr('t', V) then
  begin
    N := StrToFloatDef(Copy(V, 1, Length(V)-1), 0, FS);
    if ATickRate > 0 then
      Result := Round(N / ATickRate * 1000);
  end
  else if AnsiEndsStr('s', V) then
    Result := Round(StrToFloatDef(Copy(V, 1, Length(V)-1).Replace(',', '.'), 0, FS) * 1000)
  else if AnsiEndsStr('m', V) then
    Result := Round(StrToFloatDef(Copy(V, 1, Length(V)-1).Replace(',', '.'), 0, FS) * 60000)
  else if AnsiEndsStr('h', V) then
    Result := Round(StrToFloatDef(Copy(V, 1, Length(V)-1).Replace(',', '.'), 0, FS) * 3600000)
  else if AnsiEndsStr('f', V) then
  begin
    if AFPS > 0 then
      Result := Round(StrToFloatDef(Copy(V, 1, Length(V)-1), 0, FS) * 1000 / AFPS);
  end
  else
  begin
    P := V.Replace(';', ':').Split(':');
    if Length(P) = 4 then // hh:mm:ss:ff
      Result := SMPTEStringToMS(V, AFPS)
    else if Length(P) = 3 then // hh:mm:ss(.fracción)
      Result := Round(((StrToIntDef(P[0], 0) * 60 + StrToIntDef(P[1], 0)) * 60 +
        StrToFloatDef(P[2].Replace(',', '.'), 0, FS)) * 1000);
  end;

  if Result < 0 then Result := 0;
end;

// -----------------------------------------------------------------------------


function StripFormattingLineBreaks(const S: String): String;
var
  a, b: Integer;
  HasBreak: Boolean;
begin
  Result := S;

  // Borde izquierdo
  a := 1;
  HasBreak := False;
  while (a <= Length(Result)) and (Result[a] in [#9, #10, #13, ' ']) do
  begin
    if Result[a] in [#10, #13] then HasBreak := True;
    Inc(a);
  end;
  if HasBreak then Delete(Result, 1, a - 1);

  // Borde derecho
  b := Length(Result);
  HasBreak := False;
  while (b >= 1) and (Result[b] in [#9, #10, #13, ' ']) do
  begin
    if Result[b] in [#10, #13] then HasBreak := True;
    Dec(b);
  end;
  if HasBreak then Delete(Result, b + 1, Length(Result) - b);
end;

// -----------------------------------------------------------------------------

function ExtractStyledTTMLText(const ANode: TDOMNode): String;
var
  Child: TDOMNode;
  LocalName, StyleAttr, ColorAttr, Inner, s2, DecToken: String;
  IsBold, IsItalic, IsUnderline, IsStrike: Boolean;
  ColorBGR: String;
begin
  Result := '';
  if not Assigned(ANode) then Exit;

  Child := ANode.FirstChild;
  while Assigned(Child) do
  begin
    if Child.NodeType = ELEMENT_NODE then
    begin
      // Eliminar prefijos de namespace (ej. "tt:span" -> "span")
      LocalName := LowerCase(Child.NodeName);
      if Pos(':', LocalName) > 0 then
        LocalName := Copy(LocalName, Pos(':', LocalName) + 1, Length(LocalName));

      if LocalName = 'br' then
        Result := Result + LineEnding
      else if LocalName = 'span' then
      begin
        StyleAttr := LowerCase(XMLGetAttrValue(Child, 'style'));
        IsBold      := (Pos('bold', StyleAttr) > 0) or (LowerCase(XMLGetAttrValue(Child, 'tts:fontWeight')) = 'bold');
        IsItalic    := (Pos('italic', StyleAttr) > 0) or (LowerCase(XMLGetAttrValue(Child, 'tts:fontStyle')) = 'italic');

        // tts:textDecoration por TOKEN exacto, no por substring: TTML
        // permite negación explícita ("noUnderline"/"noLineThrough"), y
        // esos strings CONTIENEN "underline"/"linethrough" como
        // substring — Pos() los detectaría al revés. 'lineThrough'
        // (camelCase, sin guión) es el valor de la spec; 'line-through'
        // es la convención CSS que algunos archivos usan de todas
        // formas — se aceptan las dos.
        s2 := LowerCase(XMLGetAttrValue(Child, 'tts:textDecoration'));
        IsUnderline := Pos('underline', StyleAttr) > 0;
        IsStrike    := (Pos('line-through', StyleAttr) > 0) or (Pos('strikeout', StyleAttr) > 0);
        for DecToken in s2.Split([' ']) do
        begin
          if DecToken = 'underline' then IsUnderline := True;
          if (DecToken = 'linethrough') or (DecToken = 'line-through') then IsStrike := True;
        end;

        // tts:color="#RRGGBB" -> BGR sin '#' (el tag TS interno usa BGR,
        // ver ReplaceHTMLTagColor en SubtitleLib.Tags.pas).
        ColorBGR := '';
        ColorAttr := XMLGetAttrValue(Child, 'tts:color');
        if (Length(ColorAttr) = 7) and (ColorAttr[1] = '#') then
          ColorBGR := Copy(ColorAttr, 6, 2) + Copy(ColorAttr, 4, 2) + Copy(ColorAttr, 2, 2);

        Inner := ExtractStyledTTMLText(Child); // recursivo: los spans anidados se resuelven bien, a diferencia del regex de XMLTagsToTS

        if IsBold      then Inner := '{\b1}' + Inner + '{\b0}';
        if IsItalic    then Inner := '{\i1}' + Inner + '{\i0}';
        if IsUnderline then Inner := '{\u1}' + Inner + '{\u0}';
        if IsStrike    then Inner := '{\s1}' + Inner + '{\s0}';
        if ColorBGR <> '' then Inner := '{\c&' + ColorBGR + '&}' + Inner + '{\c}';

        Result := Result + Inner;
      end
      else
        Result := Result + ExtractStyledTTMLText(Child); // div u otro contenedor: recursar sin envolver
    end
    else if (Child.NodeType = TEXT_NODE) or (Child.NodeType = CDATA_SECTION_NODE) then
    begin
      // Saltos de línea de formato en los bordes del nodo de texto: no son
      // contenido (ver StripFormattingLineBreaks). CDATA se deja intacto.
      if Child.NodeType = TEXT_NODE then
        Result := Result + StripFormattingLineBreaks(Child.NodeValue)
      else
        Result := Result + Child.NodeValue;
    end;

    Child := Child.NextSibling;
  end;
end;


// -----------------------------------------------------------------------------

function IsEntireTextItalic(const Txt: String; out Stripped: String): Boolean;
var
  i, j, n: Integer;
  Italic, HasItalicText: Boolean;
  Tag: String;
begin
  Result := False;
  Stripped := Txt;
  n := Length(Txt);
  Italic := False;
  HasItalicText := False;
  Stripped := '';
  i := 1;
  while i <= n do
  begin
    if (Txt[i] = '{') and (i < n) and (Txt[i + 1] = '\') then
    begin
      j := i + 2;
      while (j <= n) and (Txt[j] <> '}') do Inc(j);
      if j > n then
      begin
        // Tag sin cerrar: no se analiza (se deja el texto como está).
        Stripped := Txt;
        Exit(False);
      end;
      Tag := LowerCase(Copy(Txt, i + 2, j - i - 2));
      if Tag = 'i1' then
        Italic := True
      else if Tag = 'i0' then
        Italic := False
      else
        Stripped := Stripped + Copy(Txt, i, j - i + 1); // otros tags se conservan
      i := j + 1;
    end
    else
    begin
      if not (Txt[i] in [' ', #9, #10, #13]) then
      begin
        if not Italic then
        begin
          Stripped := Txt;
          Exit(False);
        end;
        HasItalicText := True;
      end;
      Stripped := Stripped + Txt[i];
      Inc(i);
    end;
  end;

  Result := HasItalicText;
  if not Result then Stripped := Txt;
end;

// -----------------------------------------------------------------------------

procedure IndentXMLLines(const AList: TUWStringList; const AIndentSize: Integer);
var
  i, Depth: Integer;
  L: String;
begin
  Depth := 0;
  for i := 0 to AList.Count - 1 do
  begin
    L := Trim(AList[i]);
    if L = '' then Continue;

    if (Pos('<?', L) = 1) or (Pos('<!', L) = 1) then
      AList[i] := L
    else if Pos('</', L) = 1 then
    begin
      Dec(Depth);
      if Depth < 0 then Depth := 0;
      AList[i] := StringOfChar(' ', Depth * AIndentSize) + L;
    end
    else if Pos('<', L) = 1 then
    begin
      AList[i] := StringOfChar(' ', Depth * AIndentSize) + L;
      // Elemento que se abre y no se cierra en la misma línea: sube un nivel.
      // (<x/> autocerrado y <p>...</p> completo no cambian la profundidad.)
      if (Pos('</', L) = 0) and (Copy(L, Length(L) - 1, 2) <> '/>') then
        Inc(Depth);
    end;
  end;
end;


// -----------------------------------------------------------------------------

function StripNonItalicTSFormatting(const Txt: String): String;
var
  i, j, n: Integer;
  Content: String;

  // Mismos criterios que ParseASSColor: '\c' o '\1' + [&][H]RRGGBB[&].
  function IsColorTag(const S: String): Boolean;
  var
    Tmp: String;
    k: Integer;
  begin
    Result := False;
    if (Length(S) < 2) or (S[1] <> '\') or not ((S[2] = 'c') or (S[2] = '1')) then Exit;
    if S = '\c' then Exit(True); // cierre de color
    Tmp := Copy(S, 3, MaxInt);
    if (Length(Tmp) > 0) and (Tmp[1] = '&') then Delete(Tmp, 1, 1);
    if (Length(Tmp) > 0) and (UpCase(Tmp[1]) = 'H') then Delete(Tmp, 1, 1);
    if (Length(Tmp) > 0) and (Tmp[Length(Tmp)] = '&') then Delete(Tmp, Length(Tmp), 1);
    if Length(Tmp) <> 6 then Exit;
    for k := 1 to 6 do
      if not (Tmp[k] in ['0'..'9', 'A'..'F', 'a'..'f']) then Exit;
    Result := True;
  end;

begin
  Result := '';
  n := Length(Txt);
  i := 1;
  while i <= n do
  begin
    if (Txt[i] = '{') and (i < n) and (Txt[i + 1] = '\') then
    begin
      j := i + 2;
      while (j <= n) and (Txt[j] <> '}') do Inc(j);
      if j > n then
      begin
        Result := Result + Copy(Txt, i, n - i + 1); // tag sin cerrar: tal cual
        Break;
      end;
      Content := Copy(Txt, i + 1, j - i - 1);       // con la barra: '\b1'
      if not ((Content = '\b1') or (Content = '\b0') or (Content = '\u1') or
              (Content = '\u0') or (Content = '\s1') or (Content = '\s0') or
              IsColorTag(Content)) then
        Result := Result + Copy(Txt, i, j - i + 1);
      i := j + 1;
    end
    else
    begin
      Result := Result + Txt[i];
      Inc(i);
    end;
  end;
end;

// -----------------------------------------------------------------------------

end.

