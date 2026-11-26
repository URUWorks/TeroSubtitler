{*
 * URUWorks Subtitle API
 *
 * The contents of this file are used with permission, subject to
 * the Mozilla Public License Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/2.0.html
 *
 * Software distributed under the License is distributed on an
 * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * Copyright (C) 2001-2026 URUWorks, uruworks@gmail.com.
 *}

unit AdvancedSubstationAlpha;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWAdvancedSubstationAlpha }

  TUWAdvancedSubstationAlpha = class(TUWSubtitleCustomFormat)
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

uses strUtils, Math, UWSystem.StrUtils, UWSystem.SysUtils, UWSubtitleAPI.Tags,
  UWSubtitleAPI.ExtraInfo, UWSubtitleAPI.Styles;

const
  SSA_EventFormat = 'Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text';
  // SSA v4: Marked instead of Layer ("Dialogue: Marked=0,...")
  SSA_V4EventFormat = 'Format: Marked, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text';
  SSA_StyleFormat = 'Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding';

type

  // columns of the [Events] Format line, -1 when the file doesn't have it
  TASSEventFields = record
    Count, Layer, Start, EndT, Style, Name, MarginL, MarginR, MarginV, Effect: Integer;
    IsMarked: Boolean; // the Layer column is Marked (SSA v4)
  end;

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

procedure ASSDefaultEventFields(out E: TASSEventFields);
begin
  // Layer, Start, End, Style, Actor, MarginL, MarginR, MarginV, Effect, Text
  E.Count   := 10;
  E.Layer   := 0;
  E.Start   := 1;
  E.EndT    := 2;
  E.Style   := 3;
  E.Name    := 4;
  E.MarginL := 5;
  E.MarginR := 6;
  E.MarginV := 7;
  E.Effect  := 8;
  E.IsMarked := False;
end;

// -----------------------------------------------------------------------------

// the Format line of [Events] ("Layer, Start, End, ..."), the files don't
// always have the standard columns or order. The text is always the last one.
procedure ASSParseEventFormat(const AFormat: String; out E: TASSEventFields);
var
  Names: TStringArray;
  k: Integer;
  N: String;
begin
  Names := AFormat.Split([',']);
  E.Count   := Max(1, Length(Names));
  E.Layer   := -1;
  E.Start   := -1;
  E.EndT    := -1;
  E.Style   := -1;
  E.Name    := -1;
  E.MarginL := -1;
  E.MarginR := -1;
  E.MarginV := -1;
  E.Effect  := -1;
  E.IsMarked := False;
  for k := 0 to High(Names) - 1 do // the last one is the text
  begin
    N := LowerCase(Trim(Names[k]));
    if N = 'layer' then E.Layer := k
    else if N = 'marked' then // SSA v4
    begin
      E.Layer := k;
      E.IsMarked := True;
    end
    else if N = 'start' then E.Start := k
    else if N = 'end' then E.EndT := k
    else if N = 'style' then E.Style := k
    else if (N = 'name') or (N = 'actor') then E.Name := k
    else if N = 'marginl' then E.MarginL := k
    else if N = 'marginr' then E.MarginR := k
    else if N = 'marginv' then E.MarginV := k
    else if N = 'effect' then E.Effect := k;
  end;
end;

// -----------------------------------------------------------------------------

// our style (made for 384x288, the resolution of libass without PlayRes, or
// for 720 in the temporary track of mpv) for a file with PlayResY and no
// styles: the sizes scaled to its resolution, the text was tiny in a 1080 file
function ASSStyleForHeader(const AStyle, AHeader: String; const ABaseY: Integer): String;
var
  Fields : TStringArray;
  Line   : String;
  ResY, k: Integer;
  f      : Double;
begin
  Result := AStyle;
  ResY := 0;
  for Line in AHeader.Split([LineEnding]) do
    if SameText(Copy(Line.Trim, 1, 9), 'PlayResY:') then
      ResY := StrToIntDef(Trim(Copy(Line.Trim, 10, MaxInt)), 0);
  if (ResY <= 0) or (ResY = ABaseY) or (ABaseY <= 0) then Exit;

  // Name, Fontname, Fontsize, ... Outline (16), Shadow (17), Alignment,
  // MarginL (19), MarginR (20), MarginV (21), Encoding
  Fields := AStyle.Split([',']);
  if Length(Fields) < 22 then Exit;
  f := ResY / ABaseY;
  for k in [2, 16, 17, 19, 20, 21] do
    Fields[k] := IntToStr(Round(StrToFloatDef(Trim(Fields[k]), 0, DefaultFormatSettings) * f));
  Result := String.Join(',', Fields);
end;

// -----------------------------------------------------------------------------

// splits the fields of a Dialogue line: the first N-1 by comma and the rest,
// as it is, is the text (it can have commas)
procedure ASSSplitFields(const S: String; const N: Integer; out AFields: TStringArray);
var
  k, p, Start: Integer;
begin
  SetLength(AFields, N);
  Start := 1;
  for k := 0 to N-2 do
  begin
    p := Start;
    while (p <= Length(S)) and (S[p] <> ',') do Inc(p);
    AFields[k] := Trim(Copy(S, Start, p - Start));
    Start := p + 1;
  end;
  AFields[N-1] := Copy(S, Start, MaxInt);
end;

// -----------------------------------------------------------------------------

// removes \anN from every {...} block (the first one is returned, 0 if none)
// and the blocks left empty. The alignment is written when saving ({\anN} at
// the start and {\an0} at the end), without reading it back it stayed in the
// text and was added again on every save.
function ASSExtractAlign(var AText: String): Integer;
var
  i, j, p, q, v: Integer;
  Block, Res: String;
begin
  Result := 0;
  // only with \anN or \aN (before, a regular expression was compiled for
  // each Dialogue line of the file)
  p := Pos('\a', AText);
  while (p > 0) and not ((p + 2 <= Length(AText)) and (AText[p + 2] in ['0'..'9', 'n'])) do
    p := PosEx('\a', AText, p + 2);
  if p = 0 then Exit;
  Res := '';
  i := 1;
  while i <= Length(AText) do
  begin
    if AText[i] <> '{' then
    begin
      Res := Res + AText[i];
      Inc(i);
      Continue;
    end;

    j := i + 1;
    while (j <= Length(AText)) and (AText[j] <> '}') do Inc(j);
    if j > Length(AText) then
    begin
      Res := Res + Copy(AText, i, MaxInt); // not closed, it's text
      Break;
    end;
    Block := Copy(AText, i + 1, j - i - 1);

    p := Pos('\an', Block);
    while (p > 0) and (p + 3 <= Length(Block)) and (Block[p + 3] in ['0'..'9']) and
          ((p + 4 > Length(Block)) or not (Block[p + 4] in ['0'..'9'])) do
    begin
      if (Result = 0) and (Block[p + 3] in ['1'..'9']) then
        Result := Ord(Block[p + 3]) - Ord('0');
      Delete(Block, p, 4);
      p := Pos('\an', Block);
    end;

    // \aN of SSA v4 (1-3 bottom, 5-7 top, 9-11 middle), not \alpha
    p := Pos('\a', Block);
    while p > 0 do
    begin
      q := p + 2;
      while (q <= Length(Block)) and (Block[q] in ['0'..'9']) do Inc(q);
      if (q > p + 2) and (q - p - 2 <= 2) then
      begin
        v := StrToIntDef(Copy(Block, p + 2, q - p - 2), 0);
        if (Result = 0) and (v in [1..3, 5..7, 9..11]) then
          Result := ASSV4ToNumpad(v);
        Delete(Block, p, q - p);
        p := PosEx('\a', Block, p);
      end
      else
        p := PosEx('\a', Block, p + 2);
    end;

    if Block <> '' then Res := Res + '{' + Block + '}';
    i := j + 1;
  end;
  AText := Res;
end;

// -----------------------------------------------------------------------------

// ASS inline colors (\c&HBBGGRR&, \1c&H..&, short forms like \c&HFF&) -> the
// internal format (\c&BBGGRR&, as the color dialog writes them)
function ASSColorsToInternal(const S: String): String;
var
  T: String;
  p, q, n: Integer;
begin
  if Pos('c&H', S) = 0 then Exit(S);
  T := ReplaceString(S, '\1c&H', '\c&H', True, True);
  Result := '';
  while True do
  begin
    p := Pos('\c&H', T);
    if p = 0 then
    begin
      Result := Result + T;
      Break;
    end;
    q := p + 4;
    n := 0;
    while (q + n <= Length(T)) and (n < 6) and (T[q + n] in ['0'..'9', 'A'..'F', 'a'..'f']) do Inc(n);
    if (n > 0) and (q + n <= Length(T)) and (T[q + n] = '&') then
    begin
      Result := Result + Copy(T, 1, p - 1) + '\c&' + UpperCase(StringOfChar('0', 6 - n) + Copy(T, q, n)) + '&';
      T := Copy(T, q + n + 1, MaxInt);
    end
    else
    begin
      Result := Result + Copy(T, 1, q - 1);
      T := Copy(T, q, MaxInt);
    end;
  end;
end;

// -----------------------------------------------------------------------------

// when saving: \c&BBGGRR& (internal) -> \c&HBBGGRR& (ASS, other programs
// don't read the colors without the H)
function ASSColorsFromInternal(const S: String): String;
var
  T: String;
  p, q, k: Integer;
  IsHex: Boolean;
begin
  if Pos('\c&', S) = 0 then Exit(S);
  T := S;
  Result := '';
  while True do
  begin
    p := Pos('\c&', T);
    if p = 0 then
    begin
      Result := Result + T;
      Break;
    end;
    q := p + 3;
    IsHex := Length(T) >= q + 6;
    if IsHex then
      for k := q to q + 5 do
        if not (T[k] in ['0'..'9', 'A'..'F', 'a'..'f']) then IsHex := False;
    if IsHex and (T[q + 6] = '&') then
    begin
      Result := Result + Copy(T, 1, p - 1) + '\c&H' + UpperCase(Copy(T, q, 6)) + '&';
      T := Copy(T, q + 7, MaxInt);
    end
    else
    begin
      Result := Result + Copy(T, 1, q - 1);
      T := Copy(T, q, MaxInt);
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfAdvancedSubstationAlpha;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.Extension: String;
begin
  Result := '*.ass;*.ssa';
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.HasStyleSupport: Boolean;
begin
  Result := True; // Has tags
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
var
  Line, StartTime, EndTime: String;
  P1, P2, P3: Integer;
begin
  Line := TrimLeft(SubtitleFile[Row]);

  // only at the start of the line (a Comment or a text could have it)
  // Antes: las líneas de SSA v4 ("Dialogue: Marked=0,...") no se reconocían
  if not SameText(Copy(Line, 1, 9), 'Dialogue:') then
    Exit(False);

  P1 := Pos(',', Line);
  if P1 = 0 then Exit(False);

  P2 := PosEx(',', Line, P1 + 1);
  if P2 = 0 then Exit(False);

  P3 := PosEx(',', Line, P2 + 1);
  if P3 = 0 then Exit(False);

  // Extraemos los tiempos basándonos en la posición de las comas
  StartTime := Trim(Copy(Line, P1 + 1, P2 - P1 - 1));
  EndTime := Trim(Copy(Line, P2 + 1, P3 - P2 - 1));

  // Aceptamos máscaras estándar (1 dígito de hora) y variantes de broadcast (2 dígitos)
  Result := (TimeInFormat(StartTime, 'h:mm:ss.zz') or TimeInFormat(StartTime, 'hh:mm:ss.zz')) and
            (TimeInFormat(EndTime, 'h:mm:ss.zz') or TimeInFormat(EndTime, 'hh:mm:ss.zz'));
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i, a, al    : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Ev          : TASSEventFields;
  Fields      : TStringArray;
  Line, Text  : String;
  Header,
  Footer      : String;
  InEvents    : Boolean;
  SeenEvents  : Boolean;
  UseExtra    : Boolean;
  ExtraInfo   : PSSA_ExtraInfo;
  Pending     : String; // other event lines (Comment:...) until the next entry

  function Field(const AIndex: Integer): String;
  begin
    if (AIndex >= 0) and (AIndex < High(Fields)) then
      Result := Fields[AIndex]
    else
      Result := '';
  end;

begin
  Result := False;
  Header := '';
  Footer := '';
  InEvents := False;
  SeenEvents := False;
  ASSDefaultEventFields(Ev);
  Pending := '';
  // when appending to a subtitle with other extra info, it can't be mixed
  UseExtra := Subtitles.ExtraInfoType in [eiNone, eiSSA];
  try
    i := 0;
    while i < SubtitleFile.Count do
    begin
      Line := SubtitleFile[i];
      // the other sections are kept as they are (styles, script info, fonts...)
      if Line.Trim.StartsWith('[') then
      begin
        InEvents := SameText(Line.Trim, '[Events]');
        if InEvents then SeenEvents := True;
      end;

      if not InEvents then
      begin
        if not SeenEvents then
          Header := Header + Line + LineEnding
        else
          Footer := Footer + Line + LineEnding;
      end
      else if SameText(Copy(Line.TrimLeft, 1, 7), 'Format:') then
        // the columns of the events, in the order of the file
        ASSParseEventFormat(Copy(Line.TrimLeft, 8, MaxInt), Ev)
      else if SameText(Copy(Line.TrimLeft, 1, 9), 'Dialogue:') and (Ev.Start >= 0) and (Ev.EndT >= 0) then
      begin
        // the fields of the Format line, the text is the rest (it can have commas)
        ASSSplitFields(TrimLeft(Copy(Line.TrimLeft, 10, MaxInt)), Ev.Count, Fields);

        InitialTime := StringToTime(Field(Ev.Start));
        FinalTime   := StringToTime(Field(Ev.EndT));

        if not ((InitialTime >= 0) and (FinalTime > 0)) then
          Pending := Pending + Line.Trim + LineEnding
        else
        begin
          Text := ReplaceString(Trim(Fields[High(Fields)]), '\N', LineEnding, True, True);
          // {\anN} -> Align/VAlign, ASS colors -> internal
          al   := ASSExtractAlign(Text);
          Text := ASSColorsToInternal(Text);

          // layer, margins and effect of the file
          ExtraInfo := NIL;
          if UseExtra then
          begin
            New(ExtraInfo);
            FillChar(ExtraInfo^, SizeOf(TSSA_ExtraInfo), 0);
            with ExtraInfo^ do
            begin
              EventType := etDialogue;
              // SSA v4: "Marked=1" in place of the layer
              Marked    := Ev.IsMarked and SameText(Field(Ev.Layer), 'Marked=1');
              Style     := Field(Ev.Style);
              Name      := Field(Ev.Name);
              if Ev.IsMarked then
                Layer   := 0
              else
                Layer   := StrToIntDef(Field(Ev.Layer), 0);
              MarginL   := StrToIntDef(Field(Ev.MarginL), 0);
              MarginR   := StrToIntDef(Field(Ev.MarginR), 0);
              MarginV   := StrToIntDef(Field(Ev.MarginV), 0);
              Effect    := Field(Ev.Effect);
              Before    := TrimRight(Pending);
            end;
            Pending := '';
          end;

          a := Subtitles.Add(InitialTime, FinalTime, Text, '', ExtraInfo);
          with Subtitles.ItemPointer[a]^ do
          begin
            Actor := Field(Ev.Name);
            Style := Field(Ev.Style);
            if Ev.IsMarked then
              Marked := SameText(Field(Ev.Layer), 'Marked=1');
            // numpad layout: 1-3 bottom, 4-6 middle, 7-9 top
            if al > 0 then
            begin
              case (al - 1) mod 3 of
                0: Align := shaLeft;
                1: Align := shaCenter;
                2: Align := shaRight;
              end;
              case (al - 1) div 3 of
                0: VAlign := svaBottom;
                1: VAlign := svaCenter;
                2: VAlign := svaTop;
              end;
            end;
          end;
        end;
      end
      else if (Line.Trim <> '') and not Line.TrimLeft.StartsWith(';') and (Pos(':', Line) > 0) then
        // Comment:, Picture:, Sound:, Movie:, Command: (and a Dialogue: that
        // couldn't be read), kept as they are to write them back in place
        Pending := Pending + Line.Trim + LineEnding;
      Inc(i);
    end;
  finally
    if Subtitles.Count > 0 then
    begin
      Result := True;
      if UseExtra then
      begin
        Subtitles.ExtraInfoType := eiSSA;
        // event lines after the last entry
        if (Pending <> '') and (Subtitles.ExtraInfo[Subtitles.Count-1] <> NIL) then
          PSSA_ExtraInfo(Subtitles.ExtraInfo[Subtitles.Count-1])^.After := TrimRight(Pending);
      end;
      // with styles or script info (PlayResX/Y, WrapStyle...: without them
      // the \pos of the file are for another resolution), otherwise ours are
      // used as before. Without styles, ours are added when saving
      if (Subtitles.ASSHeader = '') and ((Pos('styles]', LowerCase(Header)) > 0) or
        (Pos('[script info]', LowerCase(Header)) > 0)) then
      begin
        Subtitles.ASSHeader := TrimRight(Header);
        Subtitles.ASSFooter := TrimRight(Footer);
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i : Integer;
  Text, s : String;
  sLayer, sMarginL, sMarginR, sMarginV, sEffect : String;
  HasStyles, HasPlayRes : Boolean;
  IsV4 : Boolean;
  al : Integer;
begin
  Result  := False;

  StringList.Clear;
  // a file of SSA v4 ([V4 Styles]) is saved as SSA v4: the events with
  // Marked and the alignment with \aN. Before they were written as ASS
  // events under the v4 styles
  IsV4 := (Pos('[v4 styles]', LowerCase(Subtitles.ASSHeader)) > 0) and
    (Pos('[v4+ styles]', LowerCase(Subtitles.ASSHeader)) = 0);

  // the original sections of the file (script info and styles)
  if Subtitles.ASSHeader <> '' then
  begin
    HasStyles  := Pos('styles]', LowerCase(Subtitles.ASSHeader)) > 0;
    HasPlayRes := Pos('playresy', LowerCase(Subtitles.ASSHeader)) > 0;
    for s in Subtitles.ASSHeader.Split([LineEnding]) do
    begin
      // the loaded lines have no blank ones, a blank line between sections
      if s.StartsWith('[') and (StringList.Count > 0) then
        StringList.Add('');
      StringList.Add(s);
      // temporary track for mpv, the same as without the sections of the
      // file (below): only when our style is used and there is no PlayRes
      if (Subtitles.Tag = 1) and not HasStyles and not HasPlayRes and
        SameText(s.Trim, '[Script Info]') then
      begin
        StringList.Add('PlayResX: 1280');
        StringList.Add('PlayResY: 720');
      end;
    end;
    // a file with script info but without styles, ours
    if not HasStyles then
    begin
      StringList.Add('');
      StringList.Add('[V4+ Styles]');
      StringList.Add(SSA_StyleFormat);
      StringList.Add('Style: ' + ASSStyleForHeader(Subtitles.FormatProperties^.SSA.DefaultStyleSettings,
        Subtitles.ASSHeader, iff(Subtitles.Tag = 1, 720, 288)));
    end;
  end
  else
  begin
    StringList.Add('[Script Info]');
    StringList.Add('ScriptType: v4.00+');
    // temporary track for mpv (Tag = 1): without PlayRes libass uses 384x288,
    // so the style font size would be huge if mpv doesn't override it. 720 is
    // the same scale mpv uses for sub-font-size.
    if Subtitles.Tag = 1 then
    begin
      StringList.Add('PlayResX: 1280');
      StringList.Add('PlayResY: 720');
    end;

  //  StringList.Add('Collisions: ' + ASSAttributes.Collisions);
  //  StringList.Add('PlayResX: ' + IntToStr(ASSAttributes.PlayResX));
  //  StringList.Add('PlayResY: ' + IntToStr(ASSAttributes.PlayResY));
  //  StringList.Add('Timer: ' + ASSAttributes.Timer);
    StringList.Add('');
    StringList.Add('[V4+ Styles]');
    StringList.Add(SSA_StyleFormat);
    StringList.Add('Style: ' + Subtitles.FormatProperties^.SSA.DefaultStyleSettings);
  end;
  StringList.Add('');
  StringList.Add('[Events]');
  StringList.Add(iff(IsV4, SSA_V4EventFormat, SSA_EventFormat));

  for i := FromItem to ToItem do
  begin
    Text := iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]);
    Text := ASSColorsFromInternal(ReplaceString(Trim(Text), LineEnding, '\N'));
    // temporary track for mpv (Tag = 1): show the text between braces that
    // is not an ASS tag, libass would hide it as a comment
    if Subtitles.Tag = 1 then
      Text := EscapeNonTagBraces(Text);

    s := '';
    if Subtitles[i].Align <> shaNone then
    begin
      case Subtitles[i].Align of
        shaLeft : case Subtitles[i].VAlign of
                    svaCenter : s := '{\an4}';
                    svaTop    : s := '{\an7}';
                  else
                    s := '{\an1}';
           end;
        shaCenter : case Subtitles[i].VAlign of
                      svaCenter : s := '{\an5}';
                      svaTop    : s := '{\an8}';
                    else
                      s := '{\an2}';
                    end;
        shaRight : case Subtitles[i].VAlign of
                     svaCenter : s := '{\an6}';
                     svaTop    : s := '{\an9}';
                   else
                     s := '{\an3}';
                   end;
      end;
    end
    else if Subtitles[i].VAlign <> svaBottom then
    begin
      case Subtitles[i].VAlign of
        svaCenter : s := '{\an5}';
        svaTop    : s := '{\an8}';
      else
        s := '{\an2}';
      end;
    end;

    if not s.IsEmpty then
    begin
      if IsV4 then
      begin
        al := StrToIntDef(Copy(s, 5, 1), 2);
        Text := '{\a' + IntToStr(ASSNumpadToV4(al)) + '}' + Text;
      end
      else
        Text := s + Text + '{\an0}';
    end;

    // layer, margins and effect of the file
    if (Subtitles.ExtraInfoType = eiSSA) and (Subtitles.ExtraInfo[i] <> NIL) then
      with PSSA_ExtraInfo(Subtitles.ExtraInfo[i])^ do
      begin
        sLayer   := IntToStr(Layer);
        if IsV4 then sLayer := 'Marked=' + iff(Subtitles[i].Marked, '1', '0');
        sMarginL := SysUtils.Format('%.4d', [MarginL]);
        sMarginR := SysUtils.Format('%.4d', [MarginR]);
        sMarginV := SysUtils.Format('%.4d', [MarginV]);
        sEffect  := Effect;
      end
    else
    begin
      sLayer   := iff(IsV4, 'Marked=' + iff(Subtitles[i].Marked, '1', '0'), '0');
      sMarginL := '0000';
      sMarginR := '0000';
      sMarginV := '0000';
      sEffect  := '';
    end;

    // the other event lines of the file (Comment:...) in their place
    if (Subtitles.Tag = 0) and (Subtitles.ExtraInfoType = eiSSA) and (Subtitles.ExtraInfo[i] <> NIL) and
       (PSSA_ExtraInfo(Subtitles.ExtraInfo[i])^.Before <> '') then
      for s in PSSA_ExtraInfo(Subtitles.ExtraInfo[i])^.Before.Split([LineEnding]) do
        StringList.Add(s);

    StringList.Add('Dialogue: ' + sLayer + ',' + TimeToString(iff(Subtitles.Tag = 0, Subtitles.InitialTime[i], Subtitles.InitialTime[i] - 1), 'h:mm:ss.zz') + ',' + TimeToString(iff(Subtitles.Tag = 0, Subtitles.FinalTime[i], Subtitles.FinalTime[i] - 1), 'h:mm:ss.zz') + ',' +
      iff(Subtitles[i].Style <> '', Subtitles[i].Style, 'Default') + ',' + ReplaceString(Subtitles[i].Actor, ',', ' ') + ',' + sMarginL + ',' + sMarginR + ',' + sMarginV + ',' + sEffect + ',' + Text);

    if (Subtitles.Tag = 0) and (Subtitles.ExtraInfoType = eiSSA) and (Subtitles.ExtraInfo[i] <> NIL) and
       (PSSA_ExtraInfo(Subtitles.ExtraInfo[i])^.After <> '') then
      for s in PSSA_ExtraInfo(Subtitles.ExtraInfo[i])^.After.Split([LineEnding]) do
        StringList.Add(s);
  end;

  if Subtitles.ASSFooter <> '' then
  begin
    for s in Subtitles.ASSFooter.Split([LineEnding]) do
    begin
      if s.StartsWith('[') and (StringList[StringList.Count-1] <> '') then
        StringList.Add('');
      StringList.Add(s);
    end;
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
