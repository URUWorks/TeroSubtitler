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

unit UWSubtitleAPI.Styles;

// The styles of an ASS/SSA file, read from and written to the sections kept
// as text in TUWSubtitles.ASSHeader. Only the "Style:" lines of the first
// styles section are rewritten, with the columns of its Format line: the
// other sections and the columns that aren't known here are kept as they are.

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils;

type

  { TASSStyle }

  TASSStyle = record
    Name        : String;
    FontName    : String;
    FontSize    : Double;
    Colors      : array[0..3] of Cardinal; // primary, secondary, outline, back: $AABBGGRR, AA 0 = opaque
    Bold        : Boolean;
    Italic      : Boolean;
    Underline   : Boolean;
    StrikeOut   : Boolean;
    ScaleX      : Double;
    ScaleY      : Double;
    Spacing     : Double;
    Angle       : Double;
    BorderStyle : Integer; // 1 = outline and shadow, 3 = opaque box
    Outline     : Double;
    Shadow      : Double;
    Alignment   : Integer; // numpad (1-3 bottom, 4-6 middle, 7-9 top), also for SSA v4
    MarginL     : Integer;
    MarginR     : Integer;
    MarginV     : Integer;
    Encoding    : Integer;
    Fields      : TStringArray; // the line as it was read, by the columns of Format
  end;

  TASSStyles = array of TASSStyle;

const
  ASSStyleFormatV4Plus = 'Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding';
  ASSDefaultStyle = 'Default,Arial,18,&H00FFFFFF,&H0000FFFF,&H00000000,&H00000000,0,0,0,0,100,100,0,0,1,1,1,2,10,10,10,1';

{ the styles of the first styles section of AHeader; False without it }
function ASSReadStyles(const AHeader: String; out AStyles: TASSStyles): Boolean;
{ AHeader with the Style lines of AStyles; a header with a styles section
  (and script info) is created when AHeader doesn't have it }
function ASSWriteStyles(const AHeader: String; const AStyles: TASSStyles): String;
{ a style with the values of ADefaultLine (V4+ columns), named AName }
function ASSNewStyle(const AName: String; const ADefaultLine: String = ASSDefaultStyle): TASSStyle;
{ the index of a style by name, -1 if none: not case sensitive, so there are
  no names that only differ in case }
function ASSIndexOfStyle(const AStyles: TASSStyles; const AName: String): Integer;

{ a style as a V4+ "Style:" line, and back (False if it isn't one) }
function ASSStyleToLine(const AStyle: TASSStyle): String;
function ASSStyleFromLine(const ALine: String; out AStyle: TASSStyle): Boolean;
{ PlayResX/Y of the header, 384x288 (libass) without them }
procedure ASSPlayRes(const AHeader: String; out AResX, AResY: Integer);
{ sections with these styles, for a file of another format that had them }
function ASSHeaderFromStyles(const AResX, AResY: Integer; const AStyles: TASSStyles): String;

type

  { TASSStyleTable: the styles of a subtitle for another format (TTML,
    WebVTT, DCinema): the entries name them, an id for each one (a valid
    XML id and CSS class) and which ones are used }

  TASSStyleTable = record
    Styles   : TASSStyles;
    Ids      : array of String;
    Used     : array of Boolean;
    ResX     : Integer;
    ResY     : Integer;
  end;

{ the table of the styles of AHeader, empty without styles }
procedure ASSStyleTableInit(out ATable: TASSStyleTable; const AHeader: String; const AIdPrefix: String = 'tero-');
{ the index of the style of an entry ('' is Default), -1 if it doesn't exist;
  marks it as used }
function ASSStyleTableFind(var ATable: TASSStyleTable; const AStyleName: String): Integer;
{ "#RRGGBB" and the opacity (0..1) of an ASS color }
function ASSColorToHTML(const AColor: Cardinal): String;
function ASSColorOpacity(const AColor: Cardinal): Double;
{ a font name for a list of TTML or CSS families: as it is when it's only
  words (letters, digits, "-", "_"), otherwise in double quotes }
function ASSFontFamily(const AName: String): String;
{ a number with "." and up to 3 decimals }
function ASSFloatToStr(const V: Double): String;

{ SSA v4 alignment (1-3 bottom, 5-7 top, 9-11 middle) <-> numpad }
function ASSV4ToNumpad(const A: Integer): Integer;
function ASSNumpadToV4(const A: Integer): Integer;

{ $AABBGGRR <-> RGB color (TColor $00BBGGRR) and alpha (0 = opaque) }
function ASSColorToRGB(const AColor: Cardinal): Cardinal;
function ASSColorAlpha(const AColor: Cardinal): Byte;
function ASSMakeColor(const ARGB: Cardinal; const AAlpha: Byte): Cardinal;

// -----------------------------------------------------------------------------

implementation

uses
  Math;

type
  TStyleColumn = (scNone, scName, scFontName, scFontSize, scPrimary, scSecondary,
    scOutlineColor, scBack, scBold, scItalic, scUnderline, scStrikeOut,
    scScaleX, scScaleY, scSpacing, scAngle, scBorderStyle, scOutline, scShadow,
    scAlignment, scMarginL, scMarginR, scMarginV, scEncoding, scTertiary);

  TStyleColumns = array of TStyleColumn;

// -----------------------------------------------------------------------------

function ColumnOf(const AName: String): TStyleColumn;
var
  s: String;
begin
  s := LowerCase(Trim(AName));
  if s = 'name' then Result := scName
  else if s = 'fontname' then Result := scFontName
  else if s = 'fontsize' then Result := scFontSize
  else if s = 'primarycolour' then Result := scPrimary
  else if s = 'secondarycolour' then Result := scSecondary
  else if s = 'outlinecolour' then Result := scOutlineColor
  else if s = 'tertiarycolour' then Result := scTertiary // SSA v4, the outline
  else if s = 'backcolour' then Result := scBack
  else if s = 'bold' then Result := scBold
  else if s = 'italic' then Result := scItalic
  else if s = 'underline' then Result := scUnderline
  else if (s = 'strikeout') or (s = 'strikethrough') then Result := scStrikeOut
  else if s = 'scalex' then Result := scScaleX
  else if s = 'scaley' then Result := scScaleY
  else if s = 'spacing' then Result := scSpacing
  else if s = 'angle' then Result := scAngle
  else if s = 'borderstyle' then Result := scBorderStyle
  else if s = 'outline' then Result := scOutline
  else if s = 'shadow' then Result := scShadow
  else if s = 'alignment' then Result := scAlignment
  else if s = 'marginl' then Result := scMarginL
  else if s = 'marginr' then Result := scMarginR
  else if s = 'marginv' then Result := scMarginV
  else if s = 'encoding' then Result := scEncoding
  else Result := scNone;
end;

// -----------------------------------------------------------------------------

function ParseColumns(const AFormat: String): TStyleColumns;
var
  Names: TStringArray;
  i: Integer;
begin
  Names := AFormat.Split([',']);
  Result := NIL;
  SetLength(Result, Length(Names));
  for i := 0 to High(Names) do
    Result[i] := ColumnOf(Names[i]);
end;

// -----------------------------------------------------------------------------

function IsV4(const ACols: TStyleColumns): Boolean;
var
  c: TStyleColumn;
begin
  // SSA v4: TertiaryColour and the old alignment, no OutlineColour
  Result := False;
  for c in ACols do
    if c = scOutlineColor then Exit(False)
    else if c = scTertiary then Result := True;
end;

// -----------------------------------------------------------------------------

{ SSA v4 alignment: 1-3 bottom, +4 top (5-7), +8 middle (9-11) }

function V4ToNumpad(const A: Integer): Integer;
begin
  if A in [1..3] then Result := A
  else if A in [5..7] then Result := A + 2
  else if A in [9..11] then Result := A - 5
  else Result := 2;
end;

function NumpadToV4(const A: Integer): Integer;
begin
  if A in [1..3] then Result := A
  else if A in [7..9] then Result := A - 2
  else if A in [4..6] then Result := A + 5
  else Result := 2;
end;

function ASSV4ToNumpad(const A: Integer): Integer;
begin
  Result := V4ToNumpad(A);
end;

function ASSNumpadToV4(const A: Integer): Integer;
begin
  Result := NumpadToV4(A);
end;

// -----------------------------------------------------------------------------

function ASSColorToRGB(const AColor: Cardinal): Cardinal;
begin
  Result := AColor and $00FFFFFF; // BBGGRR, as TColor
end;

function ASSColorAlpha(const AColor: Cardinal): Byte;
begin
  Result := (AColor shr 24) and $FF;
end;

function ASSMakeColor(const ARGB: Cardinal; const AAlpha: Byte): Cardinal;
begin
  Result := (ARGB and $00FFFFFF) or (Cardinal(AAlpha) shl 24);
end;

// -----------------------------------------------------------------------------

function ParseColor(const S: String): Cardinal;
var
  t: String;
  v: Int64;
begin
  t := UpperCase(Trim(S));
  if t.StartsWith('&H') then
  begin
    t := Copy(t, 3, MaxInt);
    if t.EndsWith('&') then SetLength(t, Length(t)-1);
    Result := StrToInt64Def('$' + t, 0) and $FFFFFFFF;
  end
  else
  begin
    // SSA v4: a decimal BGR value (can be negative)
    v := StrToInt64Def(t, 0);
    Result := Cardinal(v and $FFFFFFFF);
  end;
end;

// -----------------------------------------------------------------------------

function ColorToStr(const AColor: Cardinal; const AV4: Boolean): String;
begin
  if AV4 then
    Result := IntToStr(LongInt(AColor)) // as it was read, signed
  else
    Result := '&H' + IntToHex(AColor, 8);
end;

// -----------------------------------------------------------------------------

function ParseFloat(const S: String; const ADefault: Double): Double;
begin
  Result := StrToFloatDef(Trim(S), ADefault, DefaultFormatSettings);
end;

function FloatToStrASS(const V: Double): String;
var
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  Result := FormatFloat('0.##', V, fs);
end;

// -----------------------------------------------------------------------------

function ParseBool(const S: String): Boolean;
begin
  // -1 in the files of Aegisub, 1 in others
  Result := StrToIntDef(Trim(S), 0) <> 0;
end;

function BoolToStrASS(const B: Boolean): String;
begin
  if B then Result := '-1' else Result := '0';
end;

// -----------------------------------------------------------------------------

{ the values of the default style of libass, for the missing columns }
function BaseStyle(const AName: String): TASSStyle;
begin
  Result := Default(TASSStyle);
  Result.Name        := AName;
  Result.FontName    := 'Arial';
  Result.FontSize    := 18;
  Result.Colors[0]   := $00FFFFFF;
  Result.Colors[1]   := $0000FFFF;
  Result.ScaleX      := 100;
  Result.ScaleY      := 100;
  Result.BorderStyle := 1;
  Result.Outline     := 1;
  Result.Shadow      := 1;
  Result.Alignment   := 2;
  Result.MarginL     := 10;
  Result.MarginR     := 10;
  Result.MarginV     := 10;
  Result.Encoding    := 1;
end;

// -----------------------------------------------------------------------------

{ the fields of a Style line: the font name could have commas only in the
  last column, the same as libass (split by comma) }
function StyleFromFields(const AFields: TStringArray; const ACols: TStyleColumns): TASSStyle;
var
  i: Integer;
  v4: Boolean;
  s: String;
begin
  Result := BaseStyle('Default');
  Result.Fields := Copy(AFields);
  v4 := IsV4(ACols);
  for i := 0 to Min(High(AFields), High(ACols)) do
  begin
    s := Trim(AFields[i]);
    case ACols[i] of
      scName         : Result.Name        := s;
      scFontName     : Result.FontName    := s;
      scFontSize     : Result.FontSize    := ParseFloat(s, Result.FontSize);
      scPrimary      : Result.Colors[0]   := ParseColor(s);
      scSecondary    : Result.Colors[1]   := ParseColor(s);
      scOutlineColor,
      scTertiary     : Result.Colors[2]   := ParseColor(s);
      scBack         : Result.Colors[3]   := ParseColor(s);
      scBold         : Result.Bold        := ParseBool(s);
      scItalic       : Result.Italic      := ParseBool(s);
      scUnderline    : Result.Underline   := ParseBool(s);
      scStrikeOut    : Result.StrikeOut   := ParseBool(s);
      scScaleX       : Result.ScaleX      := ParseFloat(s, 100);
      scScaleY       : Result.ScaleY      := ParseFloat(s, 100);
      scSpacing      : Result.Spacing     := ParseFloat(s, 0);
      scAngle        : Result.Angle       := ParseFloat(s, 0);
      scBorderStyle  : Result.BorderStyle := StrToIntDef(s, 1);
      scOutline      : Result.Outline     := ParseFloat(s, 0);
      scShadow       : Result.Shadow      := ParseFloat(s, 0);
      scAlignment    : if v4 then
                         Result.Alignment := V4ToNumpad(StrToIntDef(s, 2))
                       else
                         Result.Alignment := EnsureRange(StrToIntDef(s, 2), 1, 9);
      scMarginL      : Result.MarginL     := StrToIntDef(s, 0);
      scMarginR      : Result.MarginR     := StrToIntDef(s, 0);
      scMarginV      : Result.MarginV     := StrToIntDef(s, 0);
      scEncoding     : Result.Encoding    := StrToIntDef(s, 1);
    else
      ; // scNone: an unknown column, kept in Fields
    end;
  end;
end;

// -----------------------------------------------------------------------------

function StyleToLine(const AStyle: TASSStyle; const ACols: TStyleColumns): String;
var
  F: TStringArray;
  i: Integer;
  v4: Boolean;
begin
  v4 := IsV4(ACols);
  // the read fields (the unknown columns as they were)
  F := Copy(AStyle.Fields);
  SetLength(F, Length(ACols));
  for i := 0 to High(ACols) do
    case ACols[i] of
      scName         : F[i] := StringReplace(AStyle.Name, ',', ';', [rfReplaceAll]);
      scFontName     : F[i] := StringReplace(AStyle.FontName, ',', ';', [rfReplaceAll]);
      scFontSize     : F[i] := FloatToStrASS(AStyle.FontSize);
      scPrimary      : F[i] := ColorToStr(AStyle.Colors[0], v4);
      scSecondary    : F[i] := ColorToStr(AStyle.Colors[1], v4);
      scOutlineColor,
      scTertiary     : F[i] := ColorToStr(AStyle.Colors[2], v4);
      scBack         : F[i] := ColorToStr(AStyle.Colors[3], v4);
      scBold         : F[i] := BoolToStrASS(AStyle.Bold);
      scItalic       : F[i] := BoolToStrASS(AStyle.Italic);
      scUnderline    : F[i] := BoolToStrASS(AStyle.Underline);
      scStrikeOut    : F[i] := BoolToStrASS(AStyle.StrikeOut);
      scScaleX       : F[i] := FloatToStrASS(AStyle.ScaleX);
      scScaleY       : F[i] := FloatToStrASS(AStyle.ScaleY);
      scSpacing      : F[i] := FloatToStrASS(AStyle.Spacing);
      scAngle        : F[i] := FloatToStrASS(AStyle.Angle);
      scBorderStyle  : F[i] := IntToStr(AStyle.BorderStyle);
      scOutline      : F[i] := FloatToStrASS(AStyle.Outline);
      scShadow       : F[i] := FloatToStrASS(AStyle.Shadow);
      scAlignment    : if v4 then
                         F[i] := IntToStr(NumpadToV4(AStyle.Alignment))
                       else
                         F[i] := IntToStr(AStyle.Alignment);
      scMarginL      : F[i] := IntToStr(AStyle.MarginL);
      scMarginR      : F[i] := IntToStr(AStyle.MarginR);
      scMarginV      : F[i] := IntToStr(AStyle.MarginV);
      scEncoding     : F[i] := IntToStr(AStyle.Encoding);
    else
      F[i] := Trim(F[i]); // unknown column: as it was (empty for a new style)
    end;
  Result := 'Style: ' + String.Join(',', F);
end;

// -----------------------------------------------------------------------------

function ASSNewStyle(const AName: String; const ADefaultLine: String = ASSDefaultStyle): TASSStyle;
var
  F: TStringArray;
begin
  Result := BaseStyle(AName);
  if ADefaultLine <> '' then
  begin
    F := ADefaultLine.Split([',']);
    if Length(F) = 23 then
    begin
      Result := StyleFromFields(F, ParseColumns(ASSStyleFormatV4Plus));
      Result.Name := AName;
    end;
  end;
  // a new style has no fields read from a file
  Result.Fields := NIL;
end;

// -----------------------------------------------------------------------------

function ASSIndexOfStyle(const AStyles: TASSStyles; const AName: String): Integer;
var
  i: Integer;
begin
  for i := 0 to High(AStyles) do
    if SameText(Trim(AStyles[i].Name), Trim(AName)) then
      Exit(i);

  Result := -1;
end;

// -----------------------------------------------------------------------------

function ASSStyleToLine(const AStyle: TASSStyle): String;
var
  s: TASSStyle;
begin
  s := AStyle;
  s.Fields := NIL; // the columns of V4+, not those of the file
  Result := StyleToLine(s, ParseColumns(ASSStyleFormatV4Plus));
end;

// -----------------------------------------------------------------------------

function ASSStyleFromLine(const ALine: String; out AStyle: TASSStyle): Boolean;
var
  t: String;
  F: TStringArray;
begin
  AStyle := BaseStyle('Default');
  t := Trim(ALine);
  if SameText(Copy(t, 1, 6), 'Style:') then t := Trim(Copy(t, 7, MaxInt));
  F := t.Split([',']);
  Result := (Length(F) = 23) and (Trim(F[0]) <> '');
  if Result then
  begin
    AStyle := StyleFromFields(F, ParseColumns(ASSStyleFormatV4Plus));
    AStyle.Fields := NIL;
  end;
end;

// -----------------------------------------------------------------------------

procedure ASSPlayRes(const AHeader: String; out AResX, AResY: Integer);
var
  Line, t: String;
begin
  AResX := 0;
  AResY := 0;
  for Line in AHeader.Split([LineEnding]) do
  begin
    t := Trim(Line);
    if SameText(Copy(t, 1, 9), 'PlayResX:') then
      AResX := StrToIntDef(Trim(Copy(t, 10, MaxInt)), 0)
    else if SameText(Copy(t, 1, 9), 'PlayResY:') then
      AResY := StrToIntDef(Trim(Copy(t, 10, MaxInt)), 0);
  end;
  // as libass: one from the other (4:3), or 384x288
  if (AResX <= 0) and (AResY <= 0) then
  begin
    AResX := 384;
    AResY := 288;
  end
  else if AResY <= 0 then
    AResY := Round(AResX * 3 / 4)
  else if AResX <= 0 then
    AResX := Round(AResY * 4 / 3);
end;

// -----------------------------------------------------------------------------

function ASSHeaderFromStyles(const AResX, AResY: Integer; const AStyles: TASSStyles): String;
begin
  Result := '[Script Info]' + LineEnding + 'ScriptType: v4.00+';
  if (AResX > 0) and (AResY > 0) then
    Result := Result + LineEnding + 'PlayResX: ' + IntToStr(AResX) +
      LineEnding + 'PlayResY: ' + IntToStr(AResY);
  Result := ASSWriteStyles(Result, AStyles);
end;

// -----------------------------------------------------------------------------

procedure ASSStyleTableInit(out ATable: TASSStyleTable; const AHeader: String; const AIdPrefix: String = 'tero-');
var
  i, k, n: Integer;
  Id, Base: String;
  c: Char;
begin
  ATable := Default(TASSStyleTable);
  ASSPlayRes(AHeader, ATable.ResX, ATable.ResY);
  if (AHeader = '') or not ASSReadStyles(AHeader, ATable.Styles) then
    ATable.Styles := NIL;

  SetLength(ATable.Ids, Length(ATable.Styles));
  SetLength(ATable.Used, Length(ATable.Styles));
  for i := 0 to High(ATable.Styles) do
  begin
    // letters, digits, "-" and "_": an XML id and a CSS class
    Base := '';
    for c in ATable.Styles[i].Name do
      if c in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_'] then
        Base := Base + c
      else
        Base := Base + '_';
    Base := AIdPrefix + Base;
    // two names could give the same id ("a b" and "a_b")
    Id := Base;
    n := 1;
    repeat
      k := 0;
      while (k < i) and not SameText(ATable.Ids[k], Id) do Inc(k);
      if k < i then
      begin
        Inc(n);
        Id := Base + '-' + IntToStr(n);
      end;
    until k >= i;
    ATable.Ids[i] := Id;
  end;
end;

// -----------------------------------------------------------------------------

function ASSStyleTableFind(var ATable: TASSStyleTable; const AStyleName: String): Integer;
begin
  if Trim(AStyleName) = '' then
    Result := ASSIndexOfStyle(ATable.Styles, 'Default')
  else
    Result := ASSIndexOfStyle(ATable.Styles, AStyleName);
  if Result >= 0 then ATable.Used[Result] := True;
end;

// -----------------------------------------------------------------------------

function ASSColorToHTML(const AColor: Cardinal): String;
begin
  // $AABBGGRR -> #RRGGBB
  Result := '#' + IntToHex(AColor and $FF, 2) + IntToHex((AColor shr 8) and $FF, 2) +
    IntToHex((AColor shr 16) and $FF, 2);
end;

// -----------------------------------------------------------------------------

function ASSColorOpacity(const AColor: Cardinal): Double;
begin
  Result := (255 - ASSColorAlpha(AColor)) / 255;
end;

// -----------------------------------------------------------------------------

function ASSFontFamily(const AName: String): String;
var
  w: String;
  Plain: Boolean;
  c: Char;
begin
  Result := Trim(StringReplace(AName, '"', '', [rfReplaceAll]));
  Plain := Result <> '';
  for w in Result.Split([' '], TStringSplitOptions.ExcludeEmpty) do
  begin
    if not (w[1] in ['A'..'Z', 'a'..'z', '_']) then Plain := False;
    for c in w do
      if not (c in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_']) then Plain := False;
  end;
  if not Plain then Result := '"' + Result + '"';
end;

// -----------------------------------------------------------------------------

function ASSFloatToStr(const V: Double): String;
var
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  Result := FormatFloat('0.###', V, fs);
end;

// -----------------------------------------------------------------------------

function IsStylesSection(const ALine: String): Boolean;
var
  s: String;
begin
  s := LowerCase(Trim(ALine));
  Result := s.StartsWith('[') and (Pos('styles]', s) > 0);
end;

// -----------------------------------------------------------------------------

function ASSReadStyles(const AHeader: String; out AStyles: TASSStyles): Boolean;
var
  Lines: TStringArray;
  Cols : TStyleColumns;
  i    : Integer;
  t    : String;
  InSec, Done: Boolean;
begin
  AStyles := NIL;
  Result := False;
  Cols := ParseColumns(ASSStyleFormatV4Plus);
  Lines := AHeader.Split([LineEnding]);
  InSec := False;
  Done  := False;
  for i := 0 to High(Lines) do
  begin
    t := Trim(Lines[i]);
    if t.StartsWith('[') then
    begin
      if InSec then Done := True; // only the first styles section
      InSec := (not Done) and IsStylesSection(t);
      if InSec then Result := True;
    end
    else if InSec then
    begin
      if SameText(Copy(t, 1, 7), 'Format:') then
        Cols := ParseColumns(Copy(t, 8, MaxInt))
      else if SameText(Copy(t, 1, 6), 'Style:') then
      begin
        SetLength(AStyles, Length(AStyles)+1);
        AStyles[High(AStyles)] := StyleFromFields(Trim(Copy(t, 7, MaxInt)).Split([',']), Cols);
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function ASSWriteStyles(const AHeader: String; const AStyles: TASSStyles): String;
var
  Lines   : TStringArray;
  Output  : TStringList;
  Cols    : TStyleColumns;
  i, j    : Integer;
  t       : String;
  InSec,
  Done,
  Written : Boolean;

  procedure WriteStyles;
  var
    k: Integer;
  begin
    for k := 0 to High(AStyles) do
      Output.Add(StyleToLine(AStyles[k], Cols));
    Written := True;
  end;

begin
  Output := TStringList.Create;
  try
    Output.LineBreak := LineEnding;
    Cols    := ParseColumns(ASSStyleFormatV4Plus);
    InSec   := False;
    Done    := False;
    Written := False;
    Lines   := AHeader.Split([LineEnding]);
    if Trim(AHeader) = '' then Lines := NIL;

    for i := 0 to High(Lines) do
    begin
      t := Trim(Lines[i]);
      if t.StartsWith('[') then
      begin
        // the end of the styles section: ours, if it had no Style lines
        if InSec and not Written then WriteStyles;
        if InSec then Done := True;
        InSec := (not Done) and IsStylesSection(t);
        Output.Add(Lines[i]);
      end
      else if InSec then
      begin
        if SameText(Copy(t, 1, 7), 'Format:') then
        begin
          Cols := ParseColumns(Copy(t, 8, MaxInt));
          Output.Add(Lines[i]);
        end
        else if SameText(Copy(t, 1, 6), 'Style:') then
        begin
          // all of them where the first one was
          if not Written then WriteStyles;
        end
        else
          Output.Add(Lines[i]); // a comment...
      end
      else
        Output.Add(Lines[i]);
    end;
    if InSec and not Written then WriteStyles;

    // without a styles section: ours (and the script info, if none)
    if not Done and not InSec then
    begin
      j := -1;
      for i := 0 to Output.Count-1 do
        if SameText(Trim(Output[i]), '[Script Info]') then j := i;
      if j < 0 then
      begin
        Output.Insert(0, '[Script Info]');
        Output.Insert(1, 'ScriptType: v4.00+');
      end;
      Output.Add('[V4+ Styles]');
      Output.Add('Format: ' + ASSStyleFormatV4Plus);
      Cols := ParseColumns(ASSStyleFormatV4Plus);
      WriteStyles;
    end;

    Result := TrimRight(Output.Text);
  finally
    Output.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
