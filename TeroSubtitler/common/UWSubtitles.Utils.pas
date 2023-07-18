{*
 *  URUWorks Subtitles Utils
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

unit UWSubtitles.Utils;

// -----------------------------------------------------------------------------

interface

uses SysUtils, StrUtils, Classes, Graphics, Math;

type

  TAutomaticDurationMode = (dmAlwaysNew, dmIfGreater, dmIfSmaller);

{ Timings }

function SetDurationLimits(const Duration, Min, Max: Cardinal; const UseMax: Boolean = True; const UseMin: Boolean = True): Cardinal; // maximum duration and minimum duration
function SetDelay(const Time, Delay: Integer): Cardinal;                        // positive or negative, time or frames
function TimeExpander(const Text: String; const Duration, MSecsValue, CharsValue, MinMSecsDuration: Cardinal; const Expand: Boolean): Cardinal; // expand/reduce the final time of certain subtitles under certain conditions
function ExtendLength(const NextInitialTime: Cardinal; const AGapMs: Cardinal): Cardinal;
function AutomaticDurations(const Text: String; const Duration, msPerChar, msPerWord, msPerLine: Cardinal; const Mode: TAutomaticDurationMode): Cardinal; // calculate the duration of subtitles using a simple formula
procedure ShiftTime(const InitialTime, FinalTime, Value: Integer; out NewInitialTime, NewFinalTime: Integer); // Time to shift subtitle forwards/backwards

{ Texts }

function FixTags(const Text: String; const StartTag, EndTag: Char): String;
function RemoveUnnecessaryDots(Text: String): String;
function RemoveUnnecessarySpaces(const Text: String; const BreakChar: String = sLineBreak): String;
function HasProhibitedChars(Text, Chars: String): Boolean;
function HasTooLongLine(Text: String; const MaxChars: Integer = 42): Boolean;
function SmartLineAdjust(Text: String; const ChrsPerLine: Integer; const BreakChar: String = sLineBreak): String; // constrain subtitles bigger than three lines into two and adjust length of lines
function AutoBreakSubtitle(Text: String; const ChrsPerLine: Integer; const BreakChar: String = sLineBreak; const UnbreakBefore: Boolean = True): String;
function UnbreakSubtitles(const Text: String; const BreakChar: String = sLineBreak): String; // make subtitles be in one line
function UnbreakSubtitlesIfLessThanChars(const Text: String; const MaxChars: Integer; const BreakChar: String = sLineBreak): String;
function DivideLines(Text: String; const InitialTime, FinalTime: Cardinal; const AddDots: Boolean = False; const ChrsPerLine: Integer = 43; const BreakChar: String = sLineBreak): String; // easily divide a subtitle with more than one line (or one big line) into two subtitles with proper time recalculation
function DivideLinesAtPosition(Text: String; const InitialTime, FinalTime, Position: Cardinal): String;
function SetMaximumLineLength(const Text: String; const MaxChrs: Integer; const BreakChar: String = sLineBreak): String; // splits the subtitle in N number of lines so that each of the lines is shorter than a maximum specified length
function ReverseText(Text: String; const Enter: String = sLineBreak; const KeepLinesOrder: Boolean = True): String;
function FixRTLPunctuation(const Text: String; const Delimiter: String = sLineBreak): String;
function IsHearingImpaired(const Text: String): Boolean;
function FixHearingImpaired(const Text: String; const Enter: String = sLineBreak): String;
function HasRepeatedChar(const Text, RepeatableChars: String): Boolean;
function FixRepeatedChar(Text: String; const RepeatableChars: String): String;
function FixIncompleteOpenedHyphen(const Text: String): String;
function SpacingOfOpeningHyphen(const Text: String; const AddSpace: Boolean): String;
function FixInterrobang(Text: String): String;
function RemoveSpacesWithinBrackets(const Text: String): String;

{ Draw }

procedure DrawASSText(const ACanvas: TCanvas; const ARect: TRect; Text: String; const RTL: Boolean = False);

// -----------------------------------------------------------------------------

implementation

uses UWSystem.SysUtils, UWSystem.StrUtils, RegExpr, LazUTF8, LCLIntf;

// -----------------------------------------------------------------------------

{ Timings }

// -----------------------------------------------------------------------------

function SetDurationLimits(const Duration, Min, Max: Cardinal; const UseMax: Boolean = True; const UseMin: Boolean = True): Cardinal;
begin
  if not UseMax and not UseMin then
    Result := Duration
  else if UseMax and UseMin then
    Result := Range(Duration, Min, Max)
  else if UseMax then
    Result := Math.Max(Duration, Max)
  else if UseMin then
    Result := Math.Max(Duration, Min);
end;

// -----------------------------------------------------------------------------

function SetDelay(const Time, Delay: Integer): Cardinal;
begin
  if (Time + Delay) > 0 then
    Result := Time + Delay
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function TimeExpander(const Text: String; const Duration, MSecsValue, CharsValue,
  MinMSecsDuration: Cardinal; const Expand: Boolean): Cardinal;
var
  Apply : Boolean;
  pc    : Cardinal;
begin
  Result := Duration;
  Apply  := False;
  pc     := UTF8Length(Text);

  if (CharsValue > 0) and (pc > CharsValue) then Apply := True;

  if Expand then
  begin
    if (MinMSecsDuration > 0) and (Duration < MinMSecsDuration) then Apply := True;
    if Apply then Result := Duration + MSecsValue;
  end
  else
  begin
    if (MinMSecsDuration > 0) and (Duration > MinMSecsDuration) then Apply := True;
    if Apply then Result := Duration - MSecsValue;
  end;
end;

// -----------------------------------------------------------------------------

function ExtendLength(const NextInitialTime: Cardinal; const AGapMs: Cardinal): Cardinal;
begin
  Result := NextInitialTime - AGapMs;
end;

// -----------------------------------------------------------------------------

function AutomaticDurations(const Text: String; const Duration, msPerChar,
  msPerWord, msPerLine: Cardinal; const Mode: TAutomaticDurationMode): Cardinal;
var
  pc, pw,
  pl, nd: Cardinal;
begin
  Result := Duration;

  pc := UTF8Length(Text);
  pw := WordCount(Text);
  pl := LineCount(Text);
  nd := (pc * msPerChar) + (pw * msPerWord) + (pl * msPerLine);

  case Mode of
    dmAlwaysNew: Result := nd;
    dmIfGreater: if nd > Duration then Result := nd;
    dmIfSmaller: if nd < Duration then Result := nd;
  end;
end;

// -----------------------------------------------------------------------------

procedure ShiftTime(const InitialTime, FinalTime, Value: Integer;
  out NewInitialTime, NewFinalTime: Integer);
begin
  NewInitialTime := InitialTime + Value;
  NewFinalTime   := FinalTime   + Value;
end;

// -----------------------------------------------------------------------------

{ Texts }

// -----------------------------------------------------------------------------

function FixTags(const Text: String; const StartTag, EndTag: Char): String;

  function FixTag(const S: String; const Tag: Char): String;
  var
    sTag: String;
  begin
    Result := S;
    sTag   := Format('%s\%s1%s', [StartTag, Tag, EndTag]);
    Result := ReplaceString(Result, Format('%s \%s1 %s', [StartTag, Tag, EndTag]), sTag);
    Result := ReplaceString(Result, Format('%s \%s1%s', [StartTag, Tag, EndTag]), sTag);
    Result := ReplaceString(Result, Format('%s\%s1 %s', [StartTag, Tag, EndTag]), sTag);
    sTag   := Format('%s\%s0%s', [StartTag, Tag, EndTag]);
    Result := ReplaceString(Result, Format('%s \ %s0 %s', [StartTag, Tag, EndTag]), sTag);
    Result := ReplaceString(Result, Format('%s \%s0%s', [StartTag, Tag, EndTag]), sTag);
    Result := ReplaceString(Result, Format('%s\%s0 %s', [StartTag, Tag, EndTag]), sTag);
    Result := ReplaceString(Result, Format('%s \%s0 %s', [StartTag, Tag, EndTag]), sTag);

    Result := ReplaceString(Result, Format('%s\%s1%s %s\%s0%s', [StartTag, Tag, EndTag, StartTag, Tag, EndTag]), '');
    Result := ReplaceString(Result, Format('%s\%s1%s%s\%s0%s', [StartTag, Tag, EndTag, StartTag, Tag, EndTag]), '');
  end;

begin
  Result := Text;
  Result := FixTag(Result, 'b');
  Result := FixTag(Result, 'i');
  Result := FixTag(Result, 'u');
  Result := FixTag(Result, 's');
end;

// -----------------------------------------------------------------------------

function RemoveUnnecessaryDots(Text: String): String;
begin
  while UTF8Pos('....', Text) > 0 do UTF8Delete(Text, UTF8Pos('....', Text), 1);
  Result := Text;
end;

// -----------------------------------------------------------------------------

function RemoveUnnecessarySpaces(const Text: String; const BreakChar: String = sLineBreak): String;
var
  v1, v2: String;
begin
  Result := Trim(Text);
  v1 := ' ' + BreakChar;
  v2 := BreakChar + ' ';

  while AnsiContainsText(Result, '  ') do Result := ReplaceString(Result, '  ', ' ');
  while AnsiContainsText(Result, v1) do Result := ReplaceString(Result, v1, BreakChar);
  while AnsiContainsText(Result, v2) do Result := ReplaceString(Result, v2, BreakChar);
  if AnsiEndsText(BreakChar, Result) then Result := Copy(Result, 1);
  if AnsiStartsText(BreakChar, Result) then Delete(Result, 1, 1);
end;

// -----------------------------------------------------------------------------

function HasProhibitedChars(Text, Chars: String): Boolean;
var
  split: TStringList;
  i: Integer;
begin
  Result := False;
  if (Text <> '') and (Chars <> '') then
  begin
    Text  := LowerCase(Text);
    Chars := LowerCase(Chars);
    split := TStringList.Create;
    try
      SplitRegExpr('\,', Chars, split);
      for i := 0 to split.Count-1 do
      begin
        if AnsiContainsText(Text, split[i]) then
        begin
          Result := True;
          Break;
        end;
      end;
    finally
      split.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function HasTooLongLine(Text: String; const MaxChars: Integer = 42): Boolean;
var
  PosEnter : Integer;
  EnterLen : Integer;
begin
  Result   := False;
  EnterLen := UTF8Length(sLineBreak);
  PosEnter := UTF8Pos(sLineBreak, Text);
  while PosEnter > 0 do
  begin
    if PosEnter-1 > MaxChars then
    begin
      Result := True;
      Exit;
    end;
    Text     := UTF8Copy(Text, PosEnter + EnterLen, UTF8Length(Text)-PosEnter);
    PosEnter := UTF8Pos(sLineBreak, Text);
  end;
  Result := UTF8Length(Text) > MaxChars;
end;

// -----------------------------------------------------------------------------

function SmartLineAdjust(Text: String; const ChrsPerLine: Integer; const BreakChar: String = sLineBreak): String;
var
  l, l2, i : Integer;
  s        : TStringList;
begin
  Result := Text;
  l := UTF8Length(Text);
  if (Text = '') or ((l <= ChrsPerLine) and not AnsiContainsText(Text, '-')) then Exit;

  Text := RemoveUnnecessarySpaces(Text, BreakChar);

  // try to break dialogs
  if AnsiContainsText(Text, '-') then
  begin
    s := TStringList.Create;
    try
      SplitRegExpr('\-', Text, s);
      if s.Count > 0 then
      begin
        if UTF8Pos('-', Text) > 1 then
          Result := UTF8Copy(Text, 1, UTF8Pos('-', Text)-1) + BreakChar
        else
          Result := '';

        for i := 1 to s.Count - 1 do
        begin
          Result := Format('%s-%s%s', [Result, s[i], BreakChar]);
        end;
        Result := RemoveUnnecessarySpaces(Result, BreakChar);
      end;
    finally
      s.Free;
    end;
  end
  else
  begin
    l2 := l div 2;
    if l2 > (ChrsPerLine div 2) then l2 := ChrsPerLine;

    while (Result[l2] <> ' ') and (l2 < l) do Inc(l2);

    for i := l2-1 downto 1 do
      if Result[i] = ' ' then
      begin
        Insert(BreakChar, Result, i);
        Result := RemoveUnnecessarySpaces(Result, BreakChar);
        Break;
      end;
  end;
end;

// -----------------------------------------------------------------------------

function AutoBreakSubtitle(Text: String; const ChrsPerLine: Integer; const BreakChar: String = sLineBreak; const UnbreakBefore: Boolean = True): String;
begin
  if UnbreakBefore then Text := UnbreakSubtitles(Text, BreakChar);
  Result := SmartLineAdjust(Text, ChrsPerLine, BreakChar);
end;

// -----------------------------------------------------------------------------

function UnbreakSubtitles(const Text: String; const BreakChar: String = sLineBreak): String;
begin
  Result := RemoveUnnecessarySpaces(ReplaceString(Text, BreakChar, ' '), BreakChar);
end;

// -----------------------------------------------------------------------------

function UnbreakSubtitlesIfLessThanChars(const Text: String; const MaxChars: Integer; const BreakChar: String = sLineBreak): String;
var
  x: Integer;
begin
  x := UTF8Pos(BreakChar, Text);
  if (x > 0) and (not Text.StartsWith('-')) and (UTF8Length(UnbreakSubtitles(Text, BreakChar)) <= MaxChars) then
    Result := UnbreakSubtitles(Text, BreakChar)
  else
    Result := Text;
end;

// -----------------------------------------------------------------------------

function DivideLines(Text: String; const InitialTime, FinalTime: Cardinal; const AddDots: Boolean = False; const ChrsPerLine: Integer = 43; const BreakChar: String = sLineBreak): String;
var
  s               : TStringList;
  i, ft, duration : Cardinal;
  dots            : Boolean;
  str             : String;
begin
  Result := '';
  dots   := False;
  s := TStringList.Create;
  try
    SplitRegExpr('\'+BreakChar, Text, s);
    if s.Count = 0 then
    begin
      Text := AutoBreakSubtitle(Text, ChrsPerLine, BreakChar, False);
      SplitRegExpr('\'+BreakChar, Text, s);
      if s.Count = 0 then Exit;
    end;

    duration := (FinalTime - InitialTime) div s.Count;
    ft       := InitialTime + duration;

    if AddDots and not AnsiEndsText(',', s[0]) and not AnsiEndsText('.', s[0]) then
    begin
      dots := True;
      str  := s[0]+'...';
    end
    else
      str := s[0];

    Result := Format('%d||%d||%s||', [InitialTime, ft, str]);

    for i := 1 to s.Count - 1 do
    begin
      if (i = 1) and dots and not AnsiStartsText('-', s[1]) then
        str := '...'+s[i]
      else
        str := s[i];

      Result := Format('%s%d||%d||%s||', [Result, ft + 1, ft + duration, str]);
      ft := ft + duration;
    end;
  finally
    s.Free;
  end;
end;

// -----------------------------------------------------------------------------

function DivideLinesAtPosition(Text: String; const InitialTime, FinalTime, Position: Cardinal): String;
var
  sl : TStringList;
  i, ft, duration : Cardinal;
begin
  Result := '';
  sl := TStringList.Create;
  try
    if (Position = 0) or (Position >= UTF8Length(Text)) then Exit;

    sl.Add(UTF8Copy(Text, 1, Position).Trim);
    sl.Add(UTF8Copy(Text, Position+1, UTF8Length(Text)-Position).Trim);

    duration := (FinalTime - InitialTime) div sl.Count;
    ft       := InitialTime + duration;

    Result := Format('%d||%d||%s||', [InitialTime, ft, sl[0]]);

    for i := 1 to sl.Count - 1 do
    begin
      Result := Format('%s%d||%d||%s||', [Result, ft + 1, ft + duration, sl[i]]);
      ft := ft + duration;
    end;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SetMaximumLineLength(const Text: String; const MaxChrs: Integer; const BreakChar: String = sLineBreak): String;
begin
  Result := WrapText(UnbreakSubtitles(Text, BreakChar), BreakChar, [',', '.', ';', ':', '-', #9], MaxChrs);
end;

// -----------------------------------------------------------------------------

function ReverseText(Text: String; const Enter: String = sLineBreak; const KeepLinesOrder: Boolean = True): String;
var
  x, TotalLines, c   : Integer;
  PosEnter, NewEnter : Integer;
begin
  try
    if KeepLinesOrder then
    begin
      c := iff(Enter = #13#10, 1, 0);

      SetLength(Result, UTF8Length(Text));
      TotalLines := 0;
      repeat
        NewEnter := 0;
        PosEnter := UTF8Pos(Enter, Text);

        if PosEnter = 0 then
          PosEnter := UTF8Length(Text)+1
        else
          NewEnter := PosEnter;

        for x := 1 to PosEnter-1 do
          Result[TotalLines+PosEnter-x] := Text[x];

        if (NewEnter <> 0) then
        begin
          if (Enter = #13#10) then
          begin
            Result[TotalLines+NewEnter]   := #13;
            Result[TotalLines+NewEnter+1] := #10;
          end
          else
            Result[TotalLines+NewEnter]   := Enter[1];
        end;

        UTF8Delete(Text, 1, PosEnter+c);
        Inc(TotalLines, PosEnter+c);
      until Text = '';
    end
    else
    begin
      Result := '';
      for x := 1 to UTF8Length(Text) do Result := Text[x] + Result;

      if (Enter = #13#10) then
        Result := ReplaceString(Result, #10#13, #13#10);
    end;
  except
    Result := '';
  end;
end;

// -----------------------------------------------------------------------------

function FixRTLPunctuation(const Text: String; const Delimiter: String = sLineBreak): String;
const
  SpecialChars = '.,:;''()-?!+=*&$^%#@~`" /';

var
  Posit : Integer;
  A,B   : String;

  function FixSubString(const Sub: String): String;
  var
    Prefix : String;
    Suffix : String;
    Temp   : String;
    P,I    : Integer;
  begin
    Temp   := Sub;
    Prefix := '';
    Suffix := '';
    I      := 1;
    if Temp = '' then
    begin
      Result := '';
      exit;
    end;

    P := UTF8Pos(Temp[i], SpecialChars);
    while P <> 0 do
    begin
      Prefix := Prefix + Temp[i];
      Temp   := UTF8Copy(Temp, 2, UTF8Length(Temp)-2);
      if Temp = '' then
        P := 0
      else
        P := UTF8Pos(Temp[i], SpecialChars);
    end;
    if Suffix = ' -' then Suffix := '- ';

    I := UTF8Length(Temp);
    if Temp = '' then
      P := 0
    else
      P := UTF8Pos(Temp[i], SpecialChars);
    while P <> 0 do
    begin
      Suffix := Suffix + Temp[I];
      Temp   := UTF8Copy(Temp, 1, UTF8Length(Temp));
      i      := UTF8Length(Temp);
      if Temp = '' then
        P := 0
      else
        P := UTF8Pos(Temp[i], SpecialChars);
      end;
    if Prefix = '- ' then Prefix := ' -';

    Result := Suffix + Temp + Prefix;
  end;
begin
  A := Text;
  B := '';
  Posit := UTF8Pos(Delimiter, A);
  while Posit > 0 do
  begin
    B     := B + FixSubString(UTF8Copy(A, 1, Posit-1)) + Delimiter;
    A     := Copy(A, Posit + UTF8Length(Delimiter));
    Posit := UTF8Pos(Delimiter, A);
  end;
  B := B + FixSubString(A);
  Result := B;
end;

// -----------------------------------------------------------------------------

function IsHearingImpaired(const Text: String): Boolean;
begin
  if ((Pos('(', Text) > 0) and (Pos(')', Text) > Pos('(', Text))) or
    ((Pos('[', Text) > 0) and (Pos(']', Text) > Pos('[', Text))) or
    ((Pos('<', Text) > 0) and (Pos('>', Text) > Pos('<', Text))) or
    (StringCount('♪', Text) > 1) or (StringCount('♫', Text) > 1) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function FixHearingImpaired(const Text: String; const Enter: String = sLineBreak): String;

  function RHearingImpairedBetweenChar(Line, AChar: String): String;
  var
    a, b: Integer;
  begin
    Result := Line;
    repeat
      a := UTF8Pos(AChar, Result);
      b := UTF8Pos(AChar, Result, a+1);
      if (a > 0) and (b > 0) then UTF8Delete(Result, a, b);
    until (a = 0) or (b = 0);
  end;

  function RHearingImpairedBetweenChars(Line, AChar, BChar: String): String;
  begin
    Result := Line;
    while (UTF8Pos(AChar, Result) > 0) and (UTF8Pos(BChar, Result) > UTF8Pos(AChar, Result)) do
    begin
      if UTF8Copy(Result, UTF8Pos(BChar, Result) + 1, 1) = ':' then UTF8Delete(Result, UTF8Pos(BChar, Result) + 1, 1);
      UTF8Delete(Result, UTF8Pos(AChar, Result), UTF8Pos(BChar, Result) - UTF8Pos(AChar, Result) + 1);
    end;
  end;

  function RHearingImpaired(Line: String): String;
  begin
    Result := RHearingImpairedBetweenChars(Line, '(', ')');
    Result := RHearingImpairedBetweenChars(Result, '[', ']');
    //Result := RHearingImpairedBetweenChar(Result, '♪');
    //Result := RHearingImpairedBetweenChar(Result, '♫');
    Result := StringsReplace(Result, ['♪', '♫'], ['', ''], [rfReplaceAll]);
  end;

var
  PosEnter : Integer;
  A, B     : String;
  sl       : TStringList;
  i        : Integer;
begin
  Result := '';
  if Text <> '' then
  begin
    A := Text;
    B := '';
    PosEnter := UTF8Pos(Enter, A);
    while PosEnter > 0 do
    begin
      B        := B + RHearingImpaired(UTF8Copy(A, 1, PosEnter-1)) + Enter;
      A        := UTF8Copy(A, PosEnter + UTF8Length(Enter), UTF8Length(A));
      PosEnter := UTF8Pos(Enter, A);
    end;
    B := RemoveUnnecessarySpaces(RHearingImpaired(B + RHearingImpaired(A)));

    PosEnter := UTF8Pos(Enter, B);
    if (PosEnter > 0) and (UTF8Copy(B, 1, PosEnter-1).Trim = '-') then
    begin
      UTF8Delete(B, 1, PosEnter+UTF8Length(Enter));
    end;

    if (UTF8Pos(Enter, B) = 0) and (UTF8Copy(B, 1, 1) = '-') then
    begin
      UTF8Delete(B, 1, 1);
      Result := RemoveUnnecessarySpaces(B);
    end
    else
      Result := B;

    // remnants after Remove Text for Hearing Impaired
    if Result <> Text then
    begin
      sl := TStringList.Create;
      try
        sl.Text := Result;
        for i := sl.Count-1 downto 0 do
        begin
          A := sl[i].Trim;
          if (A.IsEmpty) or (A.StartsWith('-') and (UTF8Length(A) = 1)) then
            sl.Delete(i);
        end;
        Result := sl.Text.Trim;
      finally
        sl.Free;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function HasRepeatedChar(const Text, RepeatableChars: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to UTF8Length(Text)-1 do
  begin
    if (UTF8Pos(Text[i], RepeatableChars) > 0) and (Text[i+1] = Text[i]) then
      if (Text[i] <> '/') or (UTF8Copy(Text, i-1, 3) <> '://') then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

// -----------------------------------------------------------------------------

function FixRepeatedChar(Text: String; const RepeatableChars: String): String;
var
  i: Integer;
begin
  for i := UTF8Length(Text) downto 2 do
  begin
    if (UTF8Pos(Text[i], RepeatableChars) > 0) and (Text[i-1] = Text[i]) then
      if (Text[i] <> '/') or (UTF8Copy(Text, i-2, 3) <> '://') then
        UTF8Delete(Text, i, 1);
  end;
  Result := Text;
end;

// -----------------------------------------------------------------------------

function FixIncompleteOpenedHyphen(const Text: String): String;
var
  sl: TStringList;
  i, c: Integer;
begin
  Result := Text;
  if Result.IsEmpty or (Pos('-', Text) = 0) then Exit;

  sl := TStringList.Create;
  try
    sl.Text := Text;
    c := sl.Count;

    for i := sl.Count-1 downto 0 do
      if sl[i].Trim = '-' then
        sl.Delete(i);

    if c <> sl.Count then
      Result := sl.Text.TrimRight;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SpacingOfOpeningHyphen(const Text: String; const AddSpace: Boolean): String;
var
  sl: TStringList;
  i: Integer;
  s: String;
begin
  Result := Text;
  if Result.IsEmpty or not Text.StartsWith('-') then Exit;

  sl := TStringList.Create;
  try
    sl.Text := Text;

    for i := 0 to sl.Count-1 do
      if sl[i].StartsWith('-') then
      begin
        s := sl[i];
        if AddSpace then
        begin
          if Pos(' ', s) <> 2 then
            Insert(' ', s, 2);
        end
        else
        begin
          if Pos(' ', s) = 2 then
            Delete(s, 2, 1);
        end;
        sl[i] := s;
      end;

      Result := sl.Text.TrimRight;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

function FixInterrobang(Text: String): String;
begin
  Result := Text.Replace('!?', '?!', [rfReplaceAll]);
end;

// -----------------------------------------------------------------------------

function RemoveSpacesWithinCustomBrackets(const Text: String; const OpenBracket, CloseBracket: String): String;
var
  x, b1, b2, l: Integer;
  s : String;
begin
  Result := Text;

  x := 1;
  while x > 0 do
  begin
    b1 := UTF8Pos(OpenBracket, Result, x);
    b2 := UTF8Pos(CloseBracket, Result, b1);

    if (b1 = 0) or (b2 = 0) then Exit;

    x := b2+1;
    b1 += 1;

    l := b2-b1;
    s := UTF8Copy(Result, b1, l);
    UTF8Delete(Result, b1, l);
    UTF8Insert(s.Trim, Result, b1);
  end;
end;

// -----------------------------------------------------------------------------

function RemoveSpacesWithinBrackets(const Text: String): String;
begin
  Result := RemoveSpacesWithinCustomBrackets(Text, '[', ']');
  Result := RemoveSpacesWithinCustomBrackets(Result, '(', ')');
end;

// -----------------------------------------------------------------------------

{ DrawASSText }

// -----------------------------------------------------------------------------

function BGRHex2Color(Value: String): TColor;
begin
  Result := 0;

  if Value <> '' then
    Result := LCLIntf.RGB(StrToInt('$'+Copy(Value, 5, 2)),
                  StrToInt('$'+Copy(Value, 3, 2)),
                  StrToInt('$'+Copy(Value, 1, 2)));
end;

// -----------------------------------------------------------------------------

procedure DrawASSText(const ACanvas: TCanvas; const ARect: TRect; Text: String; const RTL: Boolean = False);

  function CloseTag(const ATag: String): String;
  begin
    Result := Concat('0', ATag);
  end;

  function GetTagValue(const ATag: String; out TagID: String): String;
  var
    p, px: Integer;
  begin
    Result := '';
    TagID  := '';

    // color
    p := UTF8Pos('&', ATag);
    px := UTF8Pos('&', ATag, p+1);
    if (p > 0) and (px > 0) then
    begin
      TagID := 'c';
      Result := UTF8Copy(ATag, p + 1, px - 1);
      Exit;
    end;
    // font name
    p := UTF8Pos('fn', ATag);
    if (p > 0) then
    begin
      TagID := 'fn';
      Result := UTF8Copy(ATag, p + 2, MaxInt);
      Exit;
    end;
    // font size
    p := UTF8Pos('fs', ATag);
    if (p > 0) then
    begin
      TagID := 'fs';
      Result := UTF8Copy(ATag, p + 2, MaxInt);
      Exit;
    end;
    // font encoding
    p := UTF8Pos('fe', ATag);
    if (p > 0) then
    begin
      TagID := 'fe';
      Result := UTF8Copy(ATag, p + 2, MaxInt);
      Exit;
    end;
  end;

const
  startTag: String = '{'; endTag: String = '}';
  boldTag: String = 'b'; italicTag: String = 'i';
  underlineTag: String = 'u'; strikeoutTag: String = 's';
  colorTag: String = 'c';

var
  x, y, idx,
  CharWidth,
  MaxCharHeight   : Integer;
  CurrChar        : String;
  Tag, TagValue   : String;
  TagTmp, TagTmpX : String;
  TagID           : String;
  PrevFontColour  : TColor;
  NeedBreak       : Boolean;
begin
    if RTL then Text := ReverseText(Text);

    NeedBreak        := False;
    PrevFontColour   := ACanvas.Font.Color;
    x                := ARect.Left;
    y                := ARect.Top;
    idx              := 1;

    MaxCharHeight := ACanvas.TextHeight('Aj');

    while idx <= UTF8Length(Text) do
    begin
      CurrChar := UTF8Copy(Text, idx, 1);
      // Is this a start tag?
      if UTF8CompareStr(CurrChar, startTag) = 0 then
      begin
        Tag := '';
        Inc(idx);

        // Find the end of the tag
        while (UTF8CompareStr(UTF8Copy(Text, idx, 1), endTag) <> 0) and (idx <= UTF8Length(Text)) do
        begin
          Tag := Concat(Tag, (UTF8Copy(Text, idx, 1)));
          Inc(idx);
        end;
        // Simple tags
        if UTF8Copy(Tag, 1, 1) = '\' then
        begin
          TagTmp  := UTF8Copy(Tag, 2, 1);
          TagTmpX := UTF8RightStr(Tag, 1);
          if Length(Tag) = 3 then
          begin
            // Starting tags
            if TagTmpX = '1' then
            begin
              if TagTmp = boldTag then
                ACanvas.Font.Style := ACanvas.Font.Style + [TFontStyle.fsBold]
              else if TagTmp = italicTag then
                ACanvas.Font.Style := ACanvas.Font.Style + [TFontStyle.fsItalic]
              else if TagTmp = underlineTag then
                ACanvas.Font.Style := ACanvas.Font.Style + [TFontStyle.fsUnderline]
              else if TagTmp = strikeoutTag then
                ACanvas.Font.Style := ACanvas.Font.Style + [TFontStyle.fsStrikeout]
            end;
            // Closing tags
            if TagTmpX = '0' then
            begin
              if TagTmp = boldTag then
                ACanvas.Font.Style := ACanvas.Font.Style - [TFontStyle.fsBold]
              else if TagTmp = italicTag then
                ACanvas.Font.Style := ACanvas.Font.Style - [TFontStyle.fsItalic]
              else if TagTmp = UnderlineTag then
                ACanvas.Font.Style := ACanvas.Font.Style - [TFontStyle.fsUnderline]
              else if TagTmp = strikeoutTag then
                ACanvas.Font.Style := ACanvas.Font.Style - [TFontStyle.fsStrikeout]
            end;
          end
          else
          // Tags with values
          begin
            if TagTmp = colorTag then
              ACanvas.Font.Color := PrevFontColour;

            // Get the tag value (everything after ':')
            TagValue := GetTagValue(Tag, TagID);
            if TagValue <> '' then
            begin
              if TagID = colorTag then
              begin
                PrevFontColour := ACanvas.Font.Color;
                try
                  ACanvas.Font.Color := BGRHex2Color(TagValue);
                except
                end;
              end;
            end;
          end;
        end;
      end
      else
      // Enter char?
      if (CurrChar = sLineBreak) or (CurrChar = #13) then
      begin
        Inc(y, MaxCharHeight);
        x := ARect.Left;
      end
      // Draw the character if it's not a ctrl char
      else if (CurrChar >= #32) then
      begin
        CharWidth := ACanvas.TextWidth(CurrChar);

        if (x + CharWidth > ARect.Right) then // too long line!!
          NeedBreak := True;

        if not NeedBreak and (y < ARect.Bottom) then
        begin
          ACanvas.Brush.Style := bsClear;
          ACanvas.TextOut(x, y, CurrChar);
        end;

        x := x + CharWidth;
        NeedBreak := False;
      end;
      Inc(idx);
    end;
end;

// -----------------------------------------------------------------------------

end.
