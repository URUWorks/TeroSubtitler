{*
 *  URUWorks Lazarus StrUtils
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

unit UWSystem.StrUtils;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, StrUtils, RegExpr, lazUTF8, Math;

// -----------------------------------------------------------------------------

function Contains(const AFind, ASource: String): Boolean;

function ReplaceString(const S, OldPattern, NewPattern: String; ReplaceAll: Boolean = True; IgnoreCase: Boolean = True): String;
function ReplaceEnters(const S: String; const OldPattern: String = sLineBreak; const NewPattern: String = '|'): String;
function StringCount(const AFindString, ASourceString: String): Integer;
function StringsCount(const ASourceString: String; const AFindString: array of String): Integer;
function FindWord(const S: String; const AWord: String; const ACaseSensitive: Boolean = True): String;
function IsDelimeter(const C: Char): Boolean;
function WordCount(const Text: String): LongInt;
function LineCount(const Text: String; const Pattern: String = '|'): Cardinal;
function IsUpperCase(const Text: String): Boolean;
function PreserveCase(const Text, NewText: String): String;
function SentenceCase(const Text: String): String;
function InvertCase(const Text: String): String;
function TitleCase(const Text: String): String;
procedure RE_ExtractTags(const AText: String; const AList: TStrings; const AOpenChar: Char = '{'; const ACloseChar: Char = '}');
function RE_StringCount(const ARExpr, ASource: String): Integer;
function RE_Pos(const ARExpr, ASource: String): Integer;
function PosCS(const SubStr, Str: String): Integer;
function LastPos(const SubStr, S: String): Integer;
function RemoveTagsFromText(const Text: String; const T0: Char = '<'; const T1: Char = '>'): String;
function ReplaceWordString(const Text, Word, NewWord: String; const ReplaceAll: Boolean = False): String;
procedure SplitStr(const Delimiter, Text: String; var Pieces: TStringList);
procedure AnsiStringToAnsiChar(var Dest: array of AnsiChar; const Source: AnsiString);
function WrapText(const AText: String; const AMaxChrsPerLine: Integer; const ABreakChar: String = sLineBreak): String;

function FixRTLwithUnicodeControlChars(const AText: String): String;
function RemoveUnicodeControlChars(const AText: String): String;

const
  UCC_NBS  = WideChar(#$00A0); // NBS No-Break Space
  UCC_LRM  = WideChar(#$200E); // LRM Left-to-Right Mark
  UCC_RLM  = WideChar(#$200F); // RLM Right-to-Left Mark
  UCC_ZWJ  = WideChar(#$200D); // ZWJ Zero Width Joiner
  UCC_ZWNJ = WideChar(#$200C); // ZWNJ Zero Width Non-Joiner
  UCC_LRE  = WideChar(#$202A); // LRE Start of Left-to-Right Embedding
  UCC_RLE  = WideChar(#$202B); // RLE Start of Right-to-Left Embedding
  UCC_LRO  = WideChar(#$202D); // LRO Start of Left-to-Right Override
  UCC_RLO  = WideChar(#$202E); // RLO Start of Right-to-Left Override
  UCC_PDF  = WideChar(#$202C); // PDF Pop Directorional Formatting

// -----------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

function Contains(const AFind, ASource: String): Boolean;
begin
  Result := UTF8Pos(AFind, ASource) > 0;
end;

// -----------------------------------------------------------------------------

function ReplaceString(const S, OldPattern, NewPattern: String; ReplaceAll: Boolean = True; IgnoreCase: Boolean = True): String;
var
  Flags : TReplaceFlags;
begin
  Flags := [];
  if ReplaceAll then Flags := Flags + [rfReplaceAll];
  if IgnoreCase then Flags := Flags + [rfIgnoreCase];
  Result := UTF8StringReplace(S, OldPattern, NewPattern, Flags);
end;

// -----------------------------------------------------------------------------

function ReplaceEnters(const S: String; const OldPattern: String = LineEnding; const NewPattern: String = '|'): String;
begin
  Result := ReplaceString(S, OldPattern, NewPattern);
end;

// -----------------------------------------------------------------------------

function StringCount(const AFindString, ASourceString: String): Integer;
var
  i: Integer;
begin
  Result := 0;
  i      := 1;
  repeat
    i := PosEx(AFindString, ASourceString, i);
    if i > 0 then
    begin
      Inc(i);
      Inc(Result);
    end;
  until i = 0;
end;

// -----------------------------------------------------------------------------

function StringsCount(const ASourceString: String; const AFindString: array of String): Integer;
var
  i, c: Integer;
begin
  Result := 0;
  c      := 0;

  for i := Low(AFindString) to High(AFindString) do
    c := c + StringCount(AFindString[i], ASourceString);

  Result := c;
end;

// -----------------------------------------------------------------------------

function FindWord(const S: String; const AWord: String; const ACaseSensitive: Boolean = True): String;
var
  Buffer  : PChar;
  BuffRes : PChar;
  BuffLen : Integer;
  Options : TStringSearchOptions;
begin
  Result := '';
  if S.IsEmpty then Exit;

  Options := [soWholeWord];
  if ACaseSensitive then
    Options += [soMatchCase];

  Buffer  := PChar(S);
  BuffLen := Length(Buffer);
  BuffRes := SearchBuf(Buffer, BuffLen, 0, BuffLen, AWord, Options);

  if BuffRes <> NIL then
    Result := String(BuffRes);
end;

// -----------------------------------------------------------------------------

function IsDelimeter(const C: Char): Boolean;
begin
  Result := CharInSet(C, [#0..#$1F, ' ', '.', ',', '¿', '?', ':', ';', '(', ')',
    '|', '/', '\', '¡', '!', '"', '-', '_', '<', '>']);
end;

// -----------------------------------------------------------------------------

function WordCount(const Text: String): LongInt;
var
  Ix        : Word;
  WorkCount : LongInt;
begin
  WorkCount := 0;
  Ix        := 1;
  while Ix <= UTF8Length(Text) do
  begin
    while (Ix <= UTF8Length(Text)) and IsDelimeter(Text[Ix]) do Inc(Ix);
    if Ix <= UTF8Length(Text) then
    begin
      Inc(WorkCount);
      while (Ix <= UTF8Length(Text)) and (not IsDelimeter(Text[Ix])) do Inc(Ix);
    end;
  end;
  Result := WorkCount;
end;

// -----------------------------------------------------------------------------

function LineCount(const Text: String; const Pattern: String = '|'): Cardinal;
begin
  Result := StringCount(Pattern, Text) + 1;
end;

// -----------------------------------------------------------------------------

function IsUpperCase(const Text: String): Boolean;
begin
  Result := Text = UTF8UpperCase(Text);
end;

// -----------------------------------------------------------------------------

function PreserveCase(const Text, NewText: String): String;
var
  i,
  l1,
  l2: Integer;
begin
  Result := NewText;
  l1     := UTF8Length(Text);
  l2     := UTF8Length(NewText);
  if l1 = l2 then
  begin
    for i := 1 to l1 do
      if IsUpperCase(Text[i]) then
        Result[i] := UTF8UpperCase(NewText)[i]
      else
        Result[i] := LowerCase(NewText[i]);
  end
  else if IsUpperCase(Text) then
    Result := UTF8UpperCase(NewText);
end;

// -----------------------------------------------------------------------------

function SentenceCase(const Text: String): String;
var
  s: String;
begin
  Result := UTF8LowerCase(Text);
  if Result = '' then Exit;
  s := UTF8UpperCase(Text);
  Result[1] := s[1];
end;

// -----------------------------------------------------------------------------

function InvertCase(const Text: String): String;
var
  sl,
  su : String;
  i  : Integer;
begin
  Result := Text;
  if Result = '' then Exit;
  sl := UTF8LowerCase(Text);
  su := UTF8UpperCase(Text);
  for i := 0 to UTF8Length(Text) - 1 do
    if Result[i] = sl[i] then
      Result[i] := su[i]
    else
      Result[i] := sl[i];
end;

// -----------------------------------------------------------------------------

function TitleCase(const Text: String): String;
var
  su : String;
  i  : Integer;
  up : Boolean;
begin
  Result := UTF8LowerCase(Text);
  if Result = '' then Exit;
  su := UTF8UpperCase(Text);
  up := True;
  i  := 1;
  while i <= UTF8Length(Text) do
  begin
    while (i <= UTF8Length(Text)) and IsDelimeter(Text[i]) do
    begin
      Inc(i);
      up := True;
    end;
    if up and not IsDelimeter(Text[i]) then
    begin
      Result[i] := su[i];
      up := False;
    end;
    Inc(i);
  end;
end;

// -----------------------------------------------------------------------------

function Strings_FindPairIndexFromValue(const AItem: String; const AList: TStrings): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to AList.Count-1 do
    if AItem = Copy(AList[i], 1, Pos(AList.NameValueSeparator, AList[i])-1) then
      Exit(i);
end;

// -----------------------------------------------------------------------------

procedure RE_ExtractTags(const AText: String; const AList: TStrings; const AOpenChar: Char = '{'; const ACloseChar: Char = '}');
var
  RE : TRegExpr;
  x  : Integer;
begin
  RE := TRegExpr.Create('\'+AOpenChar+'\\(.*?)\'+ACloseChar);
  try
    if RE.Exec(AText) then
    repeat
      if RE.Match[1].EndsWith('0') then
      begin
        x := Strings_FindPairIndexFromValue(Copy(RE.Match[1], 1, RE.MatchLen[1]-1) + '1', AList);
        if x > -1 then
          AList.Delete(x)
        else
          AList.AddPair(RE.Match[1], RE.MatchPos[0].ToString);
      end
      else
        AList.AddPair(RE.Match[1], RE.MatchPos[0].ToString);
    until not RE.ExecNext;
  finally
    RE.Free;
  end;
end;

// -----------------------------------------------------------------------------

function RE_StringCount(const ARExpr, ASource: String): Integer;
begin
  Result := 0;
  with TRegExpr.Create do
  try
    ModifierG  := False;
    //ModifierM  := True;
    //ModifierI  := True;
    //ModifierR  := True;
    Expression := ARExpr;
    if Exec(ASource) then
      repeat
        Result += 1;
      until not ExecNext;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

function RE_Pos(const ARExpr, ASource: String): Integer;
begin
  with TRegExpr.Create do
  try
    ModifierG  := False;
    Expression := ARExpr;
    Exec(ASource);
    Result := MatchPos[0]; // -1 = not found
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

function PosCS(const SubStr, Str: String): Integer;
begin
  Result := Pos(AnsiLowerCase(SubStr), AnsiLowerCase(Str));
end;

// -----------------------------------------------------------------------------

function LastPos(const SubStr, S: String): Integer;
var
  Found, Len, Pos: Integer;
begin
  Pos   := UTF8Length(S);
  Len   := UTF8Length(SubStr);
  Found := 0;
  while (Pos > 0) and (Found = 0) do
  begin
    if UTF8Copy(S, Pos, Len) = SubStr then Found := Pos;
    Dec(Pos);
  end;
  LastPos := Found;
end;

// -----------------------------------------------------------------------------

function RemoveTagsFromText(const Text: String; const T0: Char = '<'; const T1: Char = '>'): String;
var
  a, b: Integer;
begin
  Result := UTF8Trim(Text);
  repeat
    a := UTF8Pos(T0, Result);
    b := UTF8Pos(T1, Result);
    if (a > 0) and (b > 0) then
    begin
      UTF8Delete(Result, a, b);
    end;
  until (a <= 0) or (b <= 0);
end;

// -----------------------------------------------------------------------------

function ReplaceWordString(const Text, Word, NewWord: String; const ReplaceAll: Boolean = False): String;

  function IsValidToCut(const C: Char): Boolean;
  begin
    Result := not CharInSet(C, ['0'..'9', 'a'..'z', 'A'..'Z']);
  end;

var
  x: Integer;
begin
  Result := Text;

  x := UTF8Pos(Word, Result);
  if x > 0 then
  begin
    if Result = Word then
      Result := NewWord
    else if (x = 1) and ((x+UTF8Length(Word)) <= UTF8Length(Result)) then
    begin
      if IsValidToCut( Result[x+UTF8Length(Word)] ) then
      begin
        UTF8Insert(NewWord, Result, x);
        UTF8Delete(Result, x+UTF8Length(NewWord), UTF8Length(Word));

        if ReplaceAll then Result := ReplaceWordString(Result, Word, NewWord);
      end;
    end
    else if (x > 1) and ((x+UTF8Length(Word))-1 = UTF8Length(Result)) then
    begin
      if IsValidToCut( Result[x-1] ) then
      begin
        UTF8Delete(Result, x, UTF8Length(Word));
        Result := Result + NewWord;

        if ReplaceAll then Result := ReplaceWordString(Result, Word, NewWord);
      end;
    end
    else if (x > 1) and ((x+UTF8Length(Word))-1 < UTF8Length(Result)) then
    begin
      if IsValidToCut( Result[x-1] ) and IsValidToCut( Result[x+UTF8Length(Word)] ) then
      begin
        UTF8Insert(NewWord, Result, x);
        UTF8Delete(Result, x+UTF8Length(NewWord), UTF8Length(Word));

        if ReplaceAll then Result := ReplaceWordString(Result, Word, NewWord);
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure SplitStr(const Delimiter, Text: String; var Pieces: TStringList);
begin
  if Text <> '' then
  begin
    if (Pieces = NIL) then Pieces := TStringList.Create;
    Pieces.Text := UTF8StringReplace(Text, Delimiter, sLineBreak, [rfReplaceAll]);
  end;
end;

// -----------------------------------------------------------------------------

procedure AnsiStringToAnsiChar(var Dest: array of AnsiChar; const Source: AnsiString);
var
  i, MaxLen: Integer;
begin
  //StrLCopy(PChar(@Dest[0]), PChar(Source), High(Dest));
  MaxLen := High(Dest);

  //FillChar(Dest, SizeOf(Dest), #0);
  for i := 0 to MaxLen do
    Dest[i] := #0;

  if MaxLen > Length(Source) then MaxLen := Length(Source)-1;

  for i := 0 to MaxLen do
    Dest[i] := Source[i+1];
end;

// -----------------------------------------------------------------------------

function WrapText(const AText: String; const AMaxChrsPerLine: Integer; const ABreakChar: String = sLineBreak): String;
var
  Words : TStringList;
  CurrentLine : String;
  i : Integer;
begin
  if AText.IsEmpty then Exit(AText);
  Result := '';

  Words := TStringList.Create;
  try
    Words.SkipLastLineBreak := True;
    Words.AddDelimitedText(AText, ' ', True);
    CurrentLine := '';

    for i := 0 to Words.Count-1 do
    begin
      if UTF8Length(CurrentLine + Words[i]) < AMaxChrsPerLine then //CPL breach not flagged in Fix Subtitles #261
      begin
        if not CurrentLine.IsEmpty then
          CurrentLine += ' ';

        CurrentLine += Words[i];
      end
      else
      begin
        Result += CurrentLine + ABreakChar;
        CurrentLine := Words[i];
      end;
    end;

    Result += CurrentLine;
  finally
    Words.Free;
  end;
end;

// -----------------------------------------------------------------------------

function FixRTLwithUnicodeControlChars(const AText: String): String;
begin
  Result := UCC_RLE + AText.Replace(UCC_RLE, '').Replace(sLineBreak, sLineBreak + UCC_RLE);
end;

// -----------------------------------------------------------------------------

function RemoveUnicodeControlChars(const AText: String): String;
begin
  Result := AText.Replace(UCC_NBS, '')
    .Replace(UCC_LRM, '')
    .Replace(UCC_RLM, '')
    .Replace(UCC_ZWJ, '')
    .Replace(UCC_ZWNJ, '')
    .Replace(UCC_LRE, '')
    .Replace(UCC_RLE, '')
    .Replace(UCC_PDF, '')
    .Replace(UCC_LRO, '')
    .Replace(UCC_RLO, '');
end;

// -----------------------------------------------------------------------------

end.
