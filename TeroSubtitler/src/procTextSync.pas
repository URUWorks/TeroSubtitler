{*
 *  URUWorks Tero Subtitler
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
 *  Copyright (C) 2023-2026 URUWorks, uruworks@gmail.com.
 *}

unit procTextSync;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

// Synchronize an untimed transcription (the texts of the entries) with the
// audio: the audio is transcribed with word timestamps (whisper), the words
// recognized are aligned with the words of the entries (like a diff) and
// every entry takes the times of its matched words.

type

  { TSyncWord }

  TSyncWord = record
    Norm       : String;  // lowercase, only letters and digits
    StartTime  : Integer; // ms, recognized words only
    EndTime    : Integer;
    Entry      : Integer; // entry index, text words only
    Match      : Integer; // index of the matched word, -1 none
  end;
  TSyncWords = array of TSyncWord;

  { TSyncTimes }

  TSyncTime = record
    InitialTime,
    FinalTime  : Integer;
    Matched    : Boolean; // false = estimated from its neighbors
  end;
  TSyncTimes = array of TSyncTime;

function NormalizeSyncWord(const AWord: String): String;
procedure AddTextWords(var AWords: TSyncWords; const AText: String; const AEntry: Integer);
procedure AddRecognizedWords(var AWords: TSyncWords; const AText: String; const AStart, AEnd: Integer);
function LoadRecognizedWordsFromJSON(const AFileName: String; var AWords: TSyncWords): Boolean;
procedure AlignSyncWords(var ATextWords: TSyncWords; const ARecognized: TSyncWords);
function BuildSyncTimes(const ATextWords: TSyncWords; const ARecognized: TSyncWords; const AEntryCount: Integer; const AEntryChars: array of Integer; const AMinDuration, AMinPause: Integer; out AMatchedWords: Integer): TSyncTimes;

// -----------------------------------------------------------------------------

implementation

uses
  LazUTF8, Character, Math, fpjson, jsonparser, generics.collections;

// -----------------------------------------------------------------------------

function NormalizeSyncWord(const AWord: String): String;
var
  u, r: UnicodeString;
  i: Integer;
begin
  u := UTF8Decode(UTF8LowerCase(AWord));
  r := '';
  for i := 1 to Length(u) do
    if TCharacter.IsLetterOrDigit(u[i]) then
      r := r + u[i];
  Result := UTF8Encode(r);
end;

// -----------------------------------------------------------------------------

function SplitWords(const AText: String): TStringArray;
var
  s: String;
begin
  s := StringReplace(AText, #13, ' ', [rfReplaceAll]);
  s := StringReplace(s, #10, ' ', [rfReplaceAll]);
  s := StringReplace(s, #9, ' ', [rfReplaceAll]);
  Result := s.Split([' '], TStringSplitOptions.ExcludeEmpty);
end;

// -----------------------------------------------------------------------------

procedure AddTextWords(var AWords: TSyncWords; const AText: String; const AEntry: Integer);
var
  w: TStringArray;
  i, l: Integer;
  n: String;
begin
  w := SplitWords(AText);
  for i := 0 to High(w) do
  begin
    n := NormalizeSyncWord(w[i]);
    if n = '' then Continue; // only punctuation (e.g. "-")
    l := Length(AWords);
    SetLength(AWords, l+1);
    AWords[l].Norm      := n;
    AWords[l].StartTime := 0;
    AWords[l].EndTime   := 0;
    AWords[l].Entry     := AEntry;
    AWords[l].Match     := -1;
  end;
end;

// -----------------------------------------------------------------------------

procedure AddRecognizedWords(var AWords: TSyncWords; const AText: String; const AStart, AEnd: Integer);
var
  w: TStringArray;
  i, l, total, pos, len: Integer;
  n: array of String;
begin
  // a segment can have more than one word, split its time by their lengths
  w := SplitWords(AText);
  SetLength(n, Length(w));
  total := 0;
  for i := 0 to High(w) do
  begin
    n[i] := NormalizeSyncWord(w[i]);
    total += UTF8Length(n[i]);
  end;
  if total = 0 then Exit;

  pos := 0;
  for i := 0 to High(w) do
  begin
    if n[i] = '' then Continue;
    len := UTF8Length(n[i]);
    l := Length(AWords);
    SetLength(AWords, l+1);
    AWords[l].Norm      := n[i];
    AWords[l].StartTime := AStart + Round((AEnd - AStart) * pos / total);
    AWords[l].EndTime   := AStart + Round((AEnd - AStart) * (pos + len) / total);
    AWords[l].Entry     := -1;
    AWords[l].Match     := -1;
    pos += len;
  end;
end;

// -----------------------------------------------------------------------------

function LoadRecognizedWordsFromJSON(const AFileName: String; var AWords: TSyncWords): Boolean;
var
  sl: TStringList;
  Data: TJSONData;
  Segments, Words: TJSONArray;
  Seg, W: TJSONObject;
  i, x: Integer;
begin
  // whisper/faster-whisper json: {"segments": [{"start", "end", "text",
  // "words": [{"start", "end", "word"}]}]}, times in seconds
  Result := False;
  if not FileExists(AFileName) then Exit;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFileName);
    try
      Data := GetJSON(sl.Text);
    except
      Exit;
    end;
    try
      if not (Data is TJSONObject) or (TJSONObject(Data).Find('segments') = NIL) or
        not (TJSONObject(Data).Find('segments') is TJSONArray) then Exit;

      Segments := TJSONArray(TJSONObject(Data).Find('segments'));
      for i := 0 to Segments.Count-1 do
        if Segments[i] is TJSONObject then
        begin
          Seg := TJSONObject(Segments[i]);
          if (Seg.Find('words') <> NIL) and (Seg.Find('words') is TJSONArray) and
            (TJSONArray(Seg.Find('words')).Count > 0) then
          begin
            Words := TJSONArray(Seg.Find('words'));
            for x := 0 to Words.Count-1 do
              if Words[x] is TJSONObject then
              begin
                W := TJSONObject(Words[x]);
                AddRecognizedWords(AWords, W.Get('word', ''),
                  Round(W.Get('start', 0.0) * 1000), Round(W.Get('end', 0.0) * 1000));
              end;
          end
          else
            AddRecognizedWords(AWords, Seg.Get('text', ''),
              Round(Seg.Get('start', 0.0) * 1000), Round(Seg.Get('end', 0.0) * 1000));
        end;

      Result := Length(AWords) > 0;
    finally
      Data.Free;
    end;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

function WordsMatch(const A, B: String): Boolean;
var
  la, lb, i, d: Integer;
begin
  // equal, or one letter different in long words (whisper spelling, typos)
  if A = B then Exit(True);
  Result := False;
  la := Length(A);
  lb := Length(B);
  if (la < 5) or (la <> lb) then Exit;
  d := 0;
  for i := 1 to la do
    if A[i] <> B[i] then
    begin
      Inc(d);
      if d > 1 then Exit;
    end;
  Result := True;
end;

// -----------------------------------------------------------------------------

// Needleman-Wunsch between ATextWords[t0..t1] and ARecognized[r0..r1]
procedure AlignRange(var ATextWords: TSyncWords; const ARecognized: TSyncWords; const t0, t1, r0, r1: Integer);
const
  MaxCells  = 6000000; // bigger gaps are left unmatched (interpolated)
  ScMatch   = 2;
  ScMiss    = -1;
  ScGap     = -1;
var
  n, m, i, j, d, u, l, best: Integer;
  Score: array of array of Integer;
  Dir: array of array of Byte; // 0 diag, 1 up (skip text), 2 left (skip rec)
begin
  n := t1 - t0 + 1;
  m := r1 - r0 + 1;
  if (n <= 0) or (m <= 0) or (Int64(n) * m > MaxCells) then Exit;

  SetLength(Score, n+1, m+1);
  SetLength(Dir, n+1, m+1);
  for i := 0 to n do
  begin
    Score[i][0] := i * ScGap;
    Dir[i][0] := 1;
  end;
  for j := 0 to m do
  begin
    Score[0][j] := j * ScGap;
    Dir[0][j] := 2;
  end;

  for i := 1 to n do
    for j := 1 to m do
    begin
      if WordsMatch(ATextWords[t0+i-1].Norm, ARecognized[r0+j-1].Norm) then
        d := Score[i-1][j-1] + ScMatch
      else
        d := Score[i-1][j-1] + ScMiss;
      u := Score[i-1][j] + ScGap;
      l := Score[i][j-1] + ScGap;

      best := d;
      Dir[i][j] := 0;
      if u > best then
      begin
        best := u;
        Dir[i][j] := 1;
      end;
      if l > best then
      begin
        best := l;
        Dir[i][j] := 2;
      end;
      Score[i][j] := best;
    end;

  i := n;
  j := m;
  while (i > 0) and (j > 0) do
    case Dir[i][j] of
      0: begin
           if WordsMatch(ATextWords[t0+i-1].Norm, ARecognized[r0+j-1].Norm) then
             ATextWords[t0+i-1].Match := r0+j-1;
           Dec(i);
           Dec(j);
         end;
      1: Dec(i);
    else
      Dec(j);
    end;
end;

// -----------------------------------------------------------------------------

procedure AlignSyncWords(var ATextWords: TSyncWords; const ARecognized: TSyncWords);
type
  TAnchor = record
    T, R: Integer;
  end;
var
  TextGrams, RecGrams: specialize TDictionary<String, Integer>;
  Anchors, Chain: array of TAnchor;
  Tails, Prev: array of Integer;
  i, x, lo, hi, mid, len, t0, r0: Integer;
  g: String;

  function Gram(const W: TSyncWords; const AIndex: Integer): String;
  begin
    Result := W[AIndex].Norm + ' ' + W[AIndex+1].Norm + ' ' + W[AIndex+2].Norm;
  end;

begin
  for i := 0 to High(ATextWords) do
    ATextWords[i].Match := -1;
  if (Length(ATextWords) = 0) or (Length(ARecognized) = 0) then Exit;

  // 1. anchors: word trigrams found only once in each side (patience diff),
  //    the longest chain of them in the same order on both sides. This keeps
  //    long texts in sync, the dynamic alignment only runs between anchors
  TextGrams := specialize TDictionary<String, Integer>.Create;
  RecGrams  := specialize TDictionary<String, Integer>.Create;
  try
    for i := 0 to High(ATextWords)-2 do
    begin
      g := Gram(ATextWords, i);
      if TextGrams.ContainsKey(g) then
        TextGrams[g] := -1
      else
        TextGrams.Add(g, i);
    end;
    for i := 0 to High(ARecognized)-2 do
    begin
      g := Gram(ARecognized, i);
      if RecGrams.ContainsKey(g) then
        RecGrams[g] := -1
      else
        RecGrams.Add(g, i);
    end;

    SetLength(Anchors, 0);
    for i := 0 to High(ATextWords)-2 do
    begin
      g := Gram(ATextWords, i);
      if (TextGrams[g] >= 0) and RecGrams.TryGetValue(g, x) and (x >= 0) then
      begin
        SetLength(Anchors, Length(Anchors)+1);
        Anchors[High(Anchors)].T := i;
        Anchors[High(Anchors)].R := x;
      end;
    end;
  finally
    TextGrams.Free;
    RecGrams.Free;
  end;

  // longest increasing subsequence by R (anchors are sorted by T)
  SetLength(Tails, Length(Anchors));
  SetLength(Prev, Length(Anchors));
  len := 0;
  for i := 0 to High(Anchors) do
  begin
    lo := 0;
    hi := len;
    while lo < hi do
    begin
      mid := (lo + hi) div 2;
      if Anchors[Tails[mid]].R < Anchors[i].R then
        lo := mid + 1
      else
        hi := mid;
    end;
    if lo > 0 then Prev[i] := Tails[lo-1] else Prev[i] := -1;
    Tails[lo] := i;
    if lo = len then Inc(len);
  end;

  SetLength(Chain, len);
  if len > 0 then
  begin
    x := Tails[len-1];
    for i := len-1 downto 0 do
    begin
      Chain[i] := Anchors[x];
      x := Prev[x];
    end;
  end;

  // 2. the anchors (3 words each, can overlap) and dynamic alignment of the
  //    gaps between them
  t0 := 0;
  r0 := 0;
  for i := 0 to High(Chain) do
  begin
    if (Chain[i].T < t0) or (Chain[i].R < r0) then Continue; // overlapped

    AlignRange(ATextWords, ARecognized, t0, Chain[i].T-1, r0, Chain[i].R-1);
    for x := 0 to 2 do
      ATextWords[Chain[i].T+x].Match := Chain[i].R+x;

    t0 := Chain[i].T + 3;
    r0 := Chain[i].R + 3;
  end;
  AlignRange(ATextWords, ARecognized, t0, High(ATextWords), r0, High(ARecognized));
end;

// -----------------------------------------------------------------------------

function BuildSyncTimes(const ATextWords: TSyncWords; const ARecognized: TSyncWords; const AEntryCount: Integer; const AEntryChars: array of Integer; const AMinDuration, AMinPause: Integer; out AMatchedWords: Integer): TSyncTimes;
var
  i, j, e, a, b, span, chars, acc, prevEnd: Integer;
begin
  SetLength(Result, AEntryCount);
  for i := 0 to AEntryCount-1 do
  begin
    Result[i].InitialTime := -1;
    Result[i].FinalTime   := -1;
    Result[i].Matched     := False;
  end;

  // entries with matched words: from the first to the last one
  AMatchedWords := 0;
  for i := 0 to High(ATextWords) do
    if ATextWords[i].Match >= 0 then
    begin
      Inc(AMatchedWords);
      e := ATextWords[i].Entry;
      with ARecognized[ATextWords[i].Match] do
      begin
        if (Result[e].InitialTime < 0) or (StartTime < Result[e].InitialTime) then
          Result[e].InitialTime := StartTime;
        if EndTime > Result[e].FinalTime then
          Result[e].FinalTime := EndTime;
      end;
      Result[e].Matched := True;
    end;

  // keep the order (a wrong match far away would cross its neighbors)
  prevEnd := 0;
  for i := 0 to AEntryCount-1 do
    if Result[i].Matched then
    begin
      if Result[i].InitialTime < prevEnd then
      begin
        Result[i].Matched := False;
        Continue;
      end;
      prevEnd := Result[i].FinalTime;
    end;

  // entries without matches: share the time between the matched neighbors
  // by their length
  i := 0;
  while i < AEntryCount do
  begin
    if Result[i].Matched then
    begin
      Inc(i);
      Continue;
    end;

    j := i;
    while (j < AEntryCount) and not Result[j].Matched do Inc(j);
    // entries i..j-1 have no times
    if i > 0 then a := Result[i-1].FinalTime else a := 0;
    if j < AEntryCount then
      b := Result[j].InitialTime
    else if Length(ARecognized) > 0 then
      b := Max(a, ARecognized[High(ARecognized)].EndTime)
    else
      b := a;

    chars := 0;
    for e := i to j-1 do chars += Max(1, AEntryChars[e]);
    span := Max(0, b - a);
    acc := 0;
    for e := i to j-1 do
    begin
      Result[e].InitialTime := a + Round(span * acc / chars);
      acc += Max(1, AEntryChars[e]);
      Result[e].FinalTime := a + Round(span * acc / chars);
    end;
    i := j;
  end;

  // minimum duration (up to the next entry) and gap between entries
  for i := 0 to AEntryCount-1 do
  begin
    if i < AEntryCount-1 then
      b := Result[i+1].InitialTime - AMinPause
    else
      b := MaxInt;

    if Result[i].FinalTime - Result[i].InitialTime < AMinDuration then
      Result[i].FinalTime := Min(Result[i].InitialTime + AMinDuration, b);

    if Result[i].FinalTime > b then
      Result[i].FinalTime := Max(Result[i].InitialTime, b);
  end;
end;

// -----------------------------------------------------------------------------

end.
