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

unit DCinema.Utils;

interface

uses
  Classes, SysUtils, UWSubtitleAPI, UWSubtitleAPI.ExtraInfo, laz2_DOM;

type

  TDCinemaFlavor = (dcfInterop, dcfSMPTE);

const
  DCinemaSMPTENamespace = 'http://www.smpte-ra.org/schemas/428-7/2010/DCST';

function DCinemaLoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles; const AFlavor: TDCinemaFlavor): Boolean;
function DCinemaSaveSubtitle(const StringList: TUWStringList; const FPS: Double; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem, ToItem: Integer; const AFlavor: TDCinemaFlavor): Boolean;

// -----------------------------------------------------------------------------

implementation

uses
  Math, StrUtils, UWSubtitleAPI.Utils;

const
  DCEffectNames : array[0..2] of String = ('none', 'border', 'shadow');


// -----------------------------------------------------------------------------

// Shared code of the D-Cinema XML subtitles:
//   Interop (CineCanvas Rev C): <DCSubtitle>, times in ticks of 4 ms
//   SMPTE ST 428-7: <SubtitleReel>, times in frames of TimeCodeRate
// Both have the same structure: Font > Subtitle > Text (> Font for italics...)

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function LocalName(const ANode: TDOMNode): String;
var
  x: Integer;
begin
  // without namespace prefix (dcst:Subtitle), lower case
  Result := ANode.NodeName;
  x := Pos(':', Result);
  if x > 0 then System.Delete(Result, 1, x);
  Result := LowerCase(Result);
end;

// -----------------------------------------------------------------------------

function GetAttr(const ANode: TDOMNode; const AName: String; const ADefault: String = ''): String;
var
  i: Integer;
begin
  // case insensitive: Interop uses VAlign, SMPTE uses Valign
  Result := ADefault;
  if (ANode = NIL) or (ANode.Attributes = NIL) then Exit;
  for i := 0 to ANode.Attributes.Length-1 do
    if SameText(ANode.Attributes[i].NodeName, AName) then
      Exit(Trim(ANode.Attributes[i].NodeValue));
end;

// -----------------------------------------------------------------------------

function StrToFloatDot(const S: String; const ADefault: Double): Double;
var
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  Result := StrToFloatDef(Trim(S), ADefault, FS);
end;

// -----------------------------------------------------------------------------

function NewUUID: String;
var
  G: TGUID;
begin
  CreateGUID(G);
  Result := LowerCase(Copy(GUIDToString(G), 2, 36));
end;

// -----------------------------------------------------------------------------

function TimeCodeRate(const FPS: Double): Integer;
begin
  Result := Round(FPS);
  if Result <= 0 then Result := 24;
end;

// -----------------------------------------------------------------------------

function DCTimeToMS(const S: String; const AFlavor: TDCinemaFlavor; const ARate: Integer): Integer;
var
  Parts: TStringArray;
  h, m, sec, f: Integer;
  Last: String;
begin
  // Interop: HH:MM:SS:ttt (ticks of 4 ms) or HH:MM:SS.mmm
  // SMPTE  : HH:MM:SS:FF (frames)
  Result := -1;
  Parts := Trim(S).Replace('.', ':#').Split([':']);
  if Length(Parts) <> 4 then Exit;

  h   := StrToIntDef(Parts[0], -1);
  m   := StrToIntDef(Parts[1], -1);
  sec := StrToIntDef(Parts[2], -1);
  Last := Parts[3];
  if (h < 0) or (m < 0) or (sec < 0) then Exit;

  Result := ((h * 60 + m) * 60 + sec) * 1000;
  if Last.StartsWith('#') then // decimal point: milliseconds
  begin
    System.Delete(Last, 1, 1);
    Last := Copy(Last + '000', 1, 3);
    Inc(Result, StrToIntDef(Last, 0));
  end
  else
  begin
    f := StrToIntDef(Last, 0);
    if AFlavor = dcfInterop then
      Inc(Result, f * 4)
    else
      Inc(Result, Round(f * 1000 / ARate));
  end;
end;

// -----------------------------------------------------------------------------

function MSToDCTime(const AMS: Integer; const AFlavor: TDCinemaFlavor; const ARate: Integer): String;
var
  ms, h, m, s, f: Integer;
begin
  ms := Max(0, AMS);
  if AFlavor = dcfInterop then
  begin
    f  := Round((ms mod 1000) / 4);
    ms := ms div 1000;
    if f > 249 then // rounding up to the next second
    begin
      f := 0;
      Inc(ms);
    end;
  end
  else
  begin
    f  := Round(Int64(ms) * ARate / 1000);
    ms := f div ARate;
    f  := f mod ARate;
  end;
  h := ms div 3600;
  m := (ms div 60) mod 60;
  s := ms mod 60;

  if AFlavor = dcfInterop then
    Result := SysUtils.Format('%.2d:%.2d:%.2d:%.3d', [h, m, s, f])
  else
    Result := SysUtils.Format('%.2d:%.2d:%.2d:%.2d', [h, m, s, f]);
end;

// -----------------------------------------------------------------------------

function DCFadeToMS(const S: String; const AFlavor: TDCinemaFlavor; const ARate: Integer): Integer;
begin
  // Interop: number of ticks, SMPTE: time code (or ticks in old files)
  if Pos(':', S) > 0 then
    Result := Max(0, DCTimeToMS(S, AFlavor, ARate))
  else if AFlavor = dcfInterop then
    Result := StrToIntDef(S, 0) * 4
  else
    Result := Round(StrToIntDef(S, 0) * 1000 / ARate);
end;

// -----------------------------------------------------------------------------

function MSToDCFade(const AMS: Integer; const AFlavor: TDCinemaFlavor; const ARate: Integer): String;
begin
  if AFlavor = dcfInterop then
    Result := IntToStr(Round(Max(0, AMS) / 4))
  else
    Result := MSToDCTime(AMS, AFlavor, ARate);
end;

// -----------------------------------------------------------------------------

function ColorToDC(const AColor: Cardinal): String;
begin
  Result := SysUtils.Format('%.8X', [AColor]); // AARRGGBB
end;

// -----------------------------------------------------------------------------

function DCToColor(const S: String; const ADefault: Cardinal): Cardinal;
var
  v: Int64;
begin
  if TryStrToInt64('$' + Trim(S), v) then
    Result := Cardinal(v)
  else
    Result := ADefault;
end;

// -----------------------------------------------------------------------------

function EffectIndex(const S: String): Byte;
var
  i: Integer;
begin
  Result := 1; // border
  for i := Low(DCEffectNames) to High(DCEffectNames) do
    if SameText(S, DCEffectNames[i]) then Exit(i);
end;

// -----------------------------------------------------------------------------

{ Load }

// -----------------------------------------------------------------------------

type
  TDCStyle = record
    Italic, Bold, Underline: Boolean;
  end;

  TDCLine = record
    Text : String;
    Key  : Double; // to sort the lines from top to bottom
    VAlign : String;
    HAlign : String;
  end;

// -----------------------------------------------------------------------------

function FontStyle(const ANode: TDOMNode; const AStyle: TDCStyle): TDCStyle;
var
  s: String;
begin
  Result := AStyle;
  s := GetAttr(ANode, 'Italic');
  if s <> '' then Result.Italic := SameText(s, 'yes');
  s := GetAttr(ANode, 'Weight');
  if s <> '' then Result.Bold := SameText(s, 'bold');
  s := GetAttr(ANode, 'Underlined', GetAttr(ANode, 'Underline'));
  if s <> '' then Result.Underline := SameText(s, 'yes');
end;

// -----------------------------------------------------------------------------

function StyleTags(const AFrom, ATo: TDCStyle): String;
begin
  // Tero tags to go from one style to the other
  Result := '';
  if AFrom.Bold <> ATo.Bold then Result := Result + IfThen(ATo.Bold, '{\b1}', '{\b0}');
  if AFrom.Italic <> ATo.Italic then Result := Result + IfThen(ATo.Italic, '{\i1}', '{\i0}');
  if AFrom.Underline <> ATo.Underline then Result := Result + IfThen(ATo.Underline, '{\u1}', '{\u0}');
end;

// -----------------------------------------------------------------------------

function CollectText(const ANode: TDOMNode; const AStyle: TDCStyle): String;
var
  Child: TDOMNode;
  NewStyle: TDCStyle;
  s: String;
begin
  Result := '';
  Child := ANode.FirstChild;
  while Assigned(Child) do
  begin
    case Child.NodeType of
      TEXT_NODE, CDATA_SECTION_NODE:
      begin
        s := Child.NodeValue.Replace(#13, '').Replace(#10, ' ').Replace(#9, ' ');
        Result := Result + s;
      end;
      ELEMENT_NODE:
        if LocalName(Child) = 'font' then
        begin
          NewStyle := FontStyle(Child, AStyle);
          Result := Result + StyleTags(AStyle, NewStyle) + CollectText(Child, NewStyle) + StyleTags(NewStyle, AStyle);
        end
        else if LocalName(Child) = 'space' then
          Result := Result + ' '
        else
          Result := Result + CollectText(Child, AStyle);
    end;
    Child := Child.NextSibling;
  end;
end;

// -----------------------------------------------------------------------------

function DCinemaLoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles; const AFlavor: TDCinemaFlavor): Boolean;
var
  XmlDoc : TXMLDocument;
  Rate   : Integer;
  FontRead, PosRead, SpacingRead : Boolean;
  Lines  : array of TDCLine;

  procedure AddLine(const ANode: TDOMNode; const AStyle: TDCStyle);
  var
    L: TDCLine;
    NoStyle: TDCStyle;
  begin
    FillChar(NoStyle, SizeOf(NoStyle), 0);
    L.Text := Trim(StyleTags(NoStyle, AStyle) + CollectText(ANode, AStyle) + StyleTags(AStyle, NoStyle));
    L.Text := L.Text.Replace('{\i1}{\i0}', '').Replace('{\b1}{\b0}', '').Replace('{\u1}{\u0}', '');
    if L.Text = '' then Exit;

    L.VAlign := LowerCase(GetAttr(ANode, 'VAlign', 'bottom'));
    L.HAlign := LowerCase(GetAttr(ANode, 'HAlign', 'center'));
    // VPosition is from the edge of VAlign
    if L.VAlign = 'bottom' then
      L.Key := 100 - StrToFloatDot(GetAttr(ANode, 'VPosition'), 0)
    else if L.VAlign = 'center' then
      L.Key := 50 + StrToFloatDot(GetAttr(ANode, 'VPosition'), 0)
    else
      L.Key := StrToFloatDot(GetAttr(ANode, 'VPosition'), 0);

    SetLength(Lines, Length(Lines)+1);
    Lines[High(Lines)] := L;
  end;

  procedure ReadLines(const ANode: TDOMNode; const AStyle: TDCStyle);
  var
    Child: TDOMNode;
  begin
    Child := ANode.FirstChild;
    while Assigned(Child) do
    begin
      if Child.NodeType = ELEMENT_NODE then
      begin
        if LocalName(Child) = 'text' then
          AddLine(Child, AStyle)
        else if LocalName(Child) = 'font' then
          ReadLines(Child, FontStyle(Child, AStyle));
        // <Image> subtitles are not supported
      end;
      Child := Child.NextSibling;
    end;
  end;

  procedure AddSubtitle(const ANode: TDOMNode; const AStyle: TDCStyle);
  var
    InitialTime, FinalTime, i, j, x: Integer;
    L: TDCLine;
    Text: String;
  begin
    InitialTime := DCTimeToMS(GetAttr(ANode, 'TimeIn'), AFlavor, Rate);
    FinalTime   := DCTimeToMS(GetAttr(ANode, 'TimeOut'), AFlavor, Rate);
    if (InitialTime < 0) or (FinalTime <= 0) then Exit;

    SetLength(Lines, 0);
    ReadLines(ANode, AStyle);
    if Length(Lines) = 0 then Exit;

    // from top to bottom (stable)
    for i := 1 to High(Lines) do
    begin
      L := Lines[i];
      j := i - 1;
      while (j >= 0) and (Lines[j].Key > L.Key) do
      begin
        Lines[j+1] := Lines[j];
        Dec(j);
      end;
      Lines[j+1] := L;
    end;

    Text := '';
    for i := 0 to High(Lines) do
    begin
      if i > 0 then Text := Text + LineEnding;
      Text := Text + Lines[i].Text;
    end;

    x := Subtitles.Add(InitialTime, FinalTime, Text, '', NIL);
    if x >= 0 then
      with Subtitles.ItemPointer[x]^ do
      begin
        if Lines[0].VAlign = 'top' then
          VAlign := svaTop
        else if Lines[0].VAlign = 'center' then
          VAlign := svaCenter;

        if Lines[0].HAlign = 'left' then
          Align := shaLeft
        else if Lines[0].HAlign = 'right' then
          Align := shaRight;
      end;

    // the position and the line spacing of the file, so they are kept
    with Subtitles.FormatProperties^.DCinema do
    begin
      if not PosRead and (Lines[High(Lines)].VAlign = 'bottom') then
      begin
        PosRead := True;
        VPosition := Round(100 - Lines[High(Lines)].Key);
      end;
      if not SpacingRead and (Length(Lines) > 1) and (Lines[High(Lines)-1].VAlign = 'bottom') and
        (Lines[High(Lines)].VAlign = 'bottom') then
      begin
        SpacingRead := True;
        LineSpacing := Max(1, Round(Lines[High(Lines)].Key - Lines[High(Lines)-1].Key));
      end;
    end;

    if Subtitles.Count = 1 then
      with Subtitles.FormatProperties^.DCinema do
      begin
        FadeUpTime   := DCFadeToMS(GetAttr(ANode, 'FadeUpTime', '0'), AFlavor, Rate);
        FadeDownTime := DCFadeToMS(GetAttr(ANode, 'FadeDownTime', '0'), AFlavor, Rate);
      end;
  end;

  procedure ReadFont(const ANode: TDOMNode);
  begin
    // the properties of the first font, so they are kept when saving
    if FontRead then Exit;
    FontRead := True;
    with Subtitles.FormatProperties^.DCinema do
    begin
      FontId      := GetAttr(ANode, IfThen(AFlavor = dcfInterop, 'Id', 'ID'), FontId);
      FontSize    := StrToIntDef(GetAttr(ANode, 'Size'), FontSize);
      FontColor   := DCToColor(GetAttr(ANode, 'Color'), FontColor);
      EffectColor := DCToColor(GetAttr(ANode, 'EffectColor'), EffectColor);
      if GetAttr(ANode, 'Effect') <> '' then
        Effect := EffectIndex(GetAttr(ANode, 'Effect'));
    end;
  end;

  procedure Walk(const ANode: TDOMNode; const AStyle: TDCStyle);
  var
    Child: TDOMNode;
    Name, s: String;
  begin
    Child := ANode.FirstChild;
    while Assigned(Child) do
    begin
      if Child.NodeType = ELEMENT_NODE then
      begin
        Name := LocalName(Child);
        s := Trim(Child.TextContent);
        with Subtitles.FormatProperties^.DCinema do
          if (Name = 'movietitle') or (Name = 'contenttitletext') then
            Title := s
          else if Name = 'reelnumber' then
            ReelNumber := StrToIntDef(s, 1)
          else if Name = 'language' then
            Language := s
          else if Name = 'loadfont' then
          begin
            if AFlavor = dcfInterop then
              FontURI := GetAttr(Child, 'URI')
            else
              FontURI := s;
          end
          else if Name = 'font' then
          begin
            ReadFont(Child);
            Walk(Child, FontStyle(Child, AStyle));
          end
          else if Name = 'subtitle' then
            AddSubtitle(Child, AStyle)
          else if Name = 'subtitlelist' then
            Walk(Child, AStyle);
      end;
      Child := Child.NextSibling;
    end;
  end;

var
  Node, TCNode: TDOMNode;
  NoStyle: TDCStyle;
begin
  Result := False;
  XmlDoc := NIL;
  FontRead := False;
  PosRead := False;
  SpacingRead := False;
  FillChar(NoStyle, SizeOf(NoStyle), 0);

  StringsToXML(XmlDoc, SubtitleFile);
  if Assigned(XmlDoc) then
    try
      Node := XmlDoc.DocumentElement;
      if Assigned(Node) then
      begin
        Rate := TimeCodeRate(FPS);
        if AFlavor = dcfSMPTE then
        begin
          TCNode := XMLFindNodeByName(XmlDoc, 'TimeCodeRate');
          if Assigned(TCNode) then
            Rate := StrToIntDef(Trim(TCNode.TextContent), Rate);
          if Rate <= 0 then Rate := TimeCodeRate(FPS);
        end;

        Walk(Node, NoStyle);
      end;
    finally
      XmlDoc.Free;
      Result := Subtitles.Count > 0;
    end;
end;

// -----------------------------------------------------------------------------

{ Save }

// -----------------------------------------------------------------------------

procedure AppendStyledText(const XmlDoc: TXMLDocument; const AParent: TDOMElement; const AText: String; const AFlavor: TDCinemaFlavor);
var
  Style: TDCStyle;
  Run: String;
  i, x: Integer;
  Tag: String;

  procedure Flush;
  var
    F: TDOMElement;
  begin
    if Run = '' then Exit;
    if Style.Italic or Style.Bold or Style.Underline then
    begin
      F := XmlDoc.CreateElement('Font');
      if Style.Italic then F.SetAttribute('Italic', 'yes');
      if Style.Underline then F.SetAttribute(IfThen(AFlavor = dcfInterop, 'Underlined', 'Underline'), 'yes');
      if Style.Bold then F.SetAttribute('Weight', 'bold');
      F.AppendChild(XmlDoc.CreateTextNode(Run));
      AParent.AppendChild(F);
    end
    else
      AParent.AppendChild(XmlDoc.CreateTextNode(Run));
    Run := '';
  end;

begin
  // italic, bold and underline of Tero tags as <Font> elements, other tags out
  FillChar(Style, SizeOf(Style), 0);
  Run := '';
  i := 1;
  while i <= Length(AText) do
  begin
    if (AText[i] = '{') and (i < Length(AText)) and (AText[i+1] = '\') then
    begin
      x := PosEx('}', AText, i);
      if x > 0 then
      begin
        Tag := Copy(AText, i, x - i + 1);
        if (Tag = '{\i1}') or (Tag = '{\i0}') or (Tag = '{\b1}') or (Tag = '{\b0}') or
           (Tag = '{\u1}') or (Tag = '{\u0}') then
        begin
          Flush;
          case Tag[3] of
            'i': Style.Italic    := Tag[4] = '1';
            'b': Style.Bold      := Tag[4] = '1';
            'u': Style.Underline := Tag[4] = '1';
          end;
        end;
        i := x + 1;
        Continue;
      end;
    end;
    Run := Run + AText[i];
    Inc(i);
  end;
  Flush;
end;

// -----------------------------------------------------------------------------

function DCinemaSaveSubtitle(const StringList: TUWStringList; const FPS: Double; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem, ToItem: Integer; const AFlavor: TDCinemaFlavor): Boolean;
var
  XmlDoc : TXMLDocument;
  Root, Parent, FontNode, Sub, TextNode : TDOMElement;
  Rate, i, j, n, Spot : Integer;
  Lines : TStringArray;
  Text, VAlign, HAlign, FId : String;
  VPos, HPos : Double;
  FS : TFormatSettings;

  procedure AddChild(const AParent: TDOMElement; const AName, AValue: String);
  var
    E: TDOMElement;
  begin
    E := XmlDoc.CreateElement(AName);
    E.AppendChild(XmlDoc.CreateTextNode(AValue));
    AParent.AppendChild(E);
  end;

begin
  Result := False;
  Rate := TimeCodeRate(FPS);
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';

  XmlDoc := TXMLDocument.Create;
  try
    with Subtitles.FormatProperties^.DCinema do
    begin
      FId := IfThen(FontId = '', 'Font1', FontId);

      if AFlavor = dcfInterop then
      begin
        Root := XmlDoc.CreateElement('DCSubtitle');
        Root.SetAttribute('Version', '1.0');
        XmlDoc.AppendChild(Root);
        AddChild(Root, 'SubtitleID', NewUUID);
        AddChild(Root, 'MovieTitle', Title);
        AddChild(Root, 'ReelNumber', IntToStr(Max(1, ReelNumber)));
        AddChild(Root, 'Language', Language);
        Parent := XmlDoc.CreateElement('LoadFont');
        Parent.SetAttribute('Id', FId);
        Parent.SetAttribute('URI', IfThen(FontURI = '', 'arial.ttf', FontURI));
        Root.AppendChild(Parent);
        Parent := Root;
      end
      else
      begin
        Root := XmlDoc.CreateElement('SubtitleReel');
        Root.SetAttribute('xmlns', DCinemaSMPTENamespace);
        Root.SetAttribute('xmlns:xs', 'http://www.w3.org/2001/XMLSchema');
        XmlDoc.AppendChild(Root);
        AddChild(Root, 'Id', 'urn:uuid:' + NewUUID);
        AddChild(Root, 'ContentTitleText', Title);
        AddChild(Root, 'IssueDate', FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss".000-00:00"', Now));
        AddChild(Root, 'ReelNumber', IntToStr(Max(1, ReelNumber)));
        AddChild(Root, 'Language', Language);
        AddChild(Root, 'EditRate', IntToStr(Rate) + ' 1');
        AddChild(Root, 'TimeCodeRate', IntToStr(Rate));
        AddChild(Root, 'StartTime', '00:00:00:00');
        Parent := XmlDoc.CreateElement('LoadFont');
        Parent.SetAttribute('ID', FId);
        Parent.AppendChild(XmlDoc.CreateTextNode(IfThen(FontURI = '', 'urn:uuid:' + NewUUID, FontURI)));
        Root.AppendChild(Parent);
        Parent := XmlDoc.CreateElement('SubtitleList');
        Root.AppendChild(Parent);
      end;

      FontNode := XmlDoc.CreateElement('Font');
      FontNode.SetAttribute(IfThen(AFlavor = dcfInterop, 'Id', 'ID'), FId);
      FontNode.SetAttribute('Color', ColorToDC(FontColor));
      FontNode.SetAttribute('Effect', DCEffectNames[Min(Effect, High(DCEffectNames))]);
      FontNode.SetAttribute('EffectColor', ColorToDC(EffectColor));
      FontNode.SetAttribute('Italic', 'no');
      FontNode.SetAttribute(IfThen(AFlavor = dcfInterop, 'Underlined', 'Underline'), 'no');
      FontNode.SetAttribute('Script', 'normal');
      FontNode.SetAttribute('Size', IntToStr(FontSize));
      if AFlavor = dcfSMPTE then
        FontNode.SetAttribute('Weight', 'normal');
      Parent.AppendChild(FontNode);

      Spot := 0;
      for i := FromItem to ToItem do
      begin
        if SubtitleMode = smText then
          Text := Subtitles.Text[i]
        else
          Text := Subtitles.Translation[i];
        Text := Trim(Text.Replace(#13#10, #10).Replace(#13, #10));
        if Text = '' then Continue;
        Lines := Text.Split([#10]);
        n := Length(Lines);

        Inc(Spot);
        Sub := XmlDoc.CreateElement('Subtitle');
        Sub.SetAttribute('SpotNumber', IntToStr(Spot));
        Sub.SetAttribute('TimeIn', MSToDCTime(Subtitles.InitialTime[i], AFlavor, Rate));
        Sub.SetAttribute('TimeOut', MSToDCTime(Subtitles.FinalTime[i], AFlavor, Rate));
        Sub.SetAttribute('FadeUpTime', MSToDCFade(FadeUpTime, AFlavor, Rate));
        Sub.SetAttribute('FadeDownTime', MSToDCFade(FadeDownTime, AFlavor, Rate));
        FontNode.AppendChild(Sub);

        case Subtitles[i].Align of
          shaLeft  : begin HAlign := 'left';  HPos := 10; end;
          shaRight : begin HAlign := 'right'; HPos := 10; end;
        else
          HAlign := 'center';
          HPos   := 0;
        end;

        for j := 0 to n-1 do
        begin
          // one <Text> per line, the position is from the edge of VAlign
          case Subtitles[i].VAlign of
            svaTop :
              begin
                VAlign := 'top';
                VPos := VPosition + j * LineSpacing;
              end;
            svaCenter : // centered block, as top to avoid the sign of center offsets
              begin
                VAlign := 'top';
                VPos := 50 - (n * LineSpacing) / 2 + j * LineSpacing;
              end;
          else
            VAlign := 'bottom';
            VPos := VPosition + (n - 1 - j) * LineSpacing;
          end;

          TextNode := XmlDoc.CreateElement('Text');
          if AFlavor = dcfInterop then
          begin
            TextNode.SetAttribute('HAlign', HAlign);
            TextNode.SetAttribute('HPosition', FormatFloat('0.0', HPos, FS));
            TextNode.SetAttribute('VAlign', VAlign);
            TextNode.SetAttribute('VPosition', FormatFloat('0.0', VPos, FS));
          end
          else
          begin
            TextNode.SetAttribute('Halign', HAlign);
            TextNode.SetAttribute('Hposition', FormatFloat('0.0', HPos, FS));
            TextNode.SetAttribute('Valign', VAlign);
            TextNode.SetAttribute('Vposition', FormatFloat('0.0', VPos, FS));
            TextNode.SetAttribute('Direction', 'ltr');
          end;
          AppendStyledText(XmlDoc, TextNode, Trim(Lines[j]), AFlavor);
          Sub.AppendChild(TextNode);
        end;
      end;
    end;

    try
      StringList.Clear;
      XMLToStrings(XmlDoc, StringList, Subtitles.ReplaceEntities);
      // each <Text> in one line, the writer breaks the lines around <Font> and
      // that whitespace would be part of the text
      JoinXMLElementLines(StringList, 'Text');
      IndentXMLLines(StringList, 2);
      Result := True;
    except
    end;
  finally
    XmlDoc.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
