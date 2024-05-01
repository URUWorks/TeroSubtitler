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
function XMLExtractTextContent(const A: TDOMNodeList): String;
function XMLGetCorrectFPS(const AFPS: Single; AMultiplier: String): Single;

function HTMLReplaceEntities(const Input: String): String;
function HTMLDecode(const AStr: String): String;

function SMPTEStringToMS(const ASMPTE: String; const AFPS: Single; const ADropFrame: Boolean = False): Integer;

function IsBinaryFormat(const AFileName: String): Boolean;

// -----------------------------------------------------------------------------

implementation

uses
  UWSubtitleAPI.Tags, UWSystem.StrUtils, RegExpr;

// -----------------------------------------------------------------------------

procedure StringsToXML(var XmlDoc: TXMLDocument; const StringList: TUWStringList);
var
  Stream : TStringStream;
begin
  if not Assigned(StringList) then Exit;

  {with TRegExpr.Create('<\?xml version="(.*?)"') do
  try
    if Exec(StringList.Text) then
    begin
      if Match[1].Length > 3 then
        StringList.Text := Replace(InputString, '<?xml version="1.0"', True);
    end;
  finally
    Free;
  end;}

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

function XMLGetCorrectFPS(const AFPS: Single; AMultiplier: String): Single;
var
  multiplier: TStringArray;
begin
  Result := AFPS;
  multiplier := AMultiplier.Split(' ');
  if Length(multiplier) = 2 then
    Result := AFPS * (multiplier[0].ToInteger /  multiplier[1].ToInteger);
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

function SMPTEStringToMS(const ASMPTE: String; const AFPS: Single; const ADropFrame: Boolean = False): Integer;
var
  DropFrame : Boolean;
  ArraySMPTE : TStringArray;
  DropFrames : Integer;
  TotalFrames : Integer;
  h, m, s, f : Integer;
  TotalMinutes : Integer;
begin
  Result := 0;
  DropFrame := ADropFrame;

  ArraySMPTE := ASMPTE.Split(';');
  if Length(ArraySMPTE) >= 2 then
  begin
    DropFrame := True;
    f := ArraySMPTE[1].ToInteger;
    ArraySMPTE := ASMPTE.Split(':');
    h := ArraySMPTE[0].ToInteger;
    m := ArraySMPTE[1].ToInteger;
    s := Copy(ArraySMPTE[2], 1, Pos(';', ArraySMPTE[2])).ToInteger - 1;
  end
  else
  begin
    ArraySMPTE := ASMPTE.Split(':');
    h := ArraySMPTE[0].ToInteger;
    m := ArraySMPTE[1].ToInteger;
    s := ArraySMPTE[2].ToInteger;
    f := ArraySMPTE[3].ToInteger;
  end;

  // Drop frames is the 6% of the framerate rounded to the nearest number.
  // I get the 0.06666 * framerate to calculate the drop frames from here https://www.davidheidelberger.com/2010/06/10/drop-frame-timecode/
  if DropFrame then
    DropFrames := Round(AFPS * 0.0666666)
  else
    DropFrames := 0;

  if m > 0 then
    TotalMinutes := (60 * h) div m
  else
    TotalMinutes := 0;

  TotalFrames := (((h * 3600) + (m * 60) + s) * Round(AFPS)) + f - (DropFrames * (TotalMinutes - (Trunc(TotalMinutes / 10))));
  Result := Round((TotalFrames / AFPS ) * 1000);
end;

// -----------------------------------------------------------------------------

function IsBinaryFormat(const AFileName: String): Boolean;
var
  Stream : TStream;
  BOM    : TBytes;
  Ext    : String;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;

  Ext := LowerCase(ExtractFileExt(AFileName));
  if (Ext = '.xls') or (Ext = '.xlsx') or (Ext = '.xls') or (Ext = '.890') or (Ext = '.cap') then
    Exit(True);

  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(BOM, 39);
    Stream.Position := 0;
    Stream.Read(BOM[0], Length(BOM));

    if (BOM[3] = 53) and (BOM[4] = 54) and (BOM[5] = $4C) and (BOM[8] = $2E) then // STL
      Exit(True)
    else if (BOM[0] = $D0) and (BOM[1] = $CF) and (BOM[2] = 11) and (BOM[3] = $E0) and (BOM[4] = $A1) and (BOM[5] = $B1) and (BOM[6] = $1A) and (BOM[7] = $E1) then // XLS
      Exit(True)
    else if (BOM[0] = 50) and (BOM[1] = $4B) and (BOM[2] = 03) and (BOM[3] = 04) then // ODS
      Exit(True)
    else if (BOM[0] = 0) and (BOM[1] = 0) and (BOM[2] = 03) and (BOM[3] = 04) and (BOM[38] = 0) and (BOM[39] = 0) then // 890
      Exit(True)
  finally
    Stream.Free;
    SetLength(BOM, 0);
  end;
end;

// -----------------------------------------------------------------------------

end.

