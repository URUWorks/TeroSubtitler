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

// -----------------------------------------------------------------------------

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
    Text       : String;  // as written/recognized (with punctuation)
    Norm       : String;  // lowercase, only letters and digits
    StartTime  : Integer; // ms, recognized words only
    EndTime    : Integer;
    Entry      : Integer; // entry index, text words only
    Match      : Integer; // index of the matched word, -1 none
    MatchLast  : Integer; // last one when it was split in two ("can not")
  end;
  TSyncWords = array of TSyncWord;

  { TSyncTimes }

  TSyncTime = record
    InitialTime,
    FinalTime  : Integer;
    Matched    : Boolean; // false = estimated from its neighbors
  end;
  TSyncTimes = array of TSyncTime;


  { Smart segmentation }

  TSegmentOptions = record
    CPL          : Integer; // characters per line
    MaxLines     : Integer;
    MaxDuration  : Integer; // ms
    MinDuration  : Integer; // ms
    MinPause     : Integer; // ms, gap between subtitles
    PauseSplit   : Integer; // ms, a silence this long always starts a new subtitle
    MaxCPS       : Double;
    Language     : String;  // culture name (es-ES...), to know the words that shouldn't end a subtitle
  end;

  TSegment = record
    InitialTime,
    FinalTime  : Integer;
    Text       : String;
  end;
  TSegments = array of TSegment;

function NormalizeSyncWord(const AWord: String): String;
procedure AddTextWords(var AWords: TSyncWords; const AText: String; const AEntry: Integer);
procedure AddRecognizedWords(var AWords: TSyncWords; const AText: String; const AStart, AEnd: Integer);
function LoadRecognizedWordsFromJSON(const AFileName: String; var AWords: TSyncWords): Boolean;
function WhisperDTWPreset(const AModel: String): String;
procedure AlignSyncWords(var ATextWords: TSyncWords; const ARecognized: TSyncWords);
function BuildSyncTimes(const ATextWords: TSyncWords; const ARecognized: TSyncWords; const AEntryCount: Integer; const AEntryChars: array of Integer; const AMinDuration, AMinPause: Integer; out AMatchedWords: Integer): TSyncTimes;

function SegmentWords(const AWords: TSyncWords; const AOptions: TSegmentOptions): TSegments;

// -----------------------------------------------------------------------------

implementation

uses
  LazUTF8, Character, Math, fpjson, jsonparser, generics.collections,
  UWSubtitles.Utils;

const
  // words that shouldn't end a subtitle (articles, prepositions, conjunctions)
  NoEndEN = ' a an the and or but nor of to in on at for with from by as that which who whose if than so my your his her its our their ';
  NoEndES = ' el la los las un una unos unas y e o u ni pero sino que de del al a ante en con contra por para sin sobre tras se lo le les su sus mi mis tu tus nuestro nuestra cuando como donde si porque ';
  NoEndPT = ' o a os as um uma uns umas e ou nem mas que de do da dos das em no na nos nas com por pelo pela para sem se seu sua seus suas meu minha quando como onde porque ';
  NoEndFR = ' le la les un une des et ou ni mais que qui de du au aux à en dans pour par sur sans avec ce cette ces son sa ses mon ma mes ton ta tes si quand comme ';
  NoEndIT = ' il lo la i gli le un uno una e o ma che di del della dei delle a al alla in nel nella con per su da tra fra se quando come ';
  NoEndDE = ' der die das den dem des ein eine einen einem einer und oder aber dass zu von mit auf in im an am für bei aus nach wenn als wie ';

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
    AWords[l].Text      := w[i];
    AWords[l].Norm      := n;
    AWords[l].StartTime := 0;
    AWords[l].EndTime   := 0;
    AWords[l].Entry     := AEntry;
    AWords[l].Match     := -1;
    AWords[l].MatchLast := -1;
  end;
end;

// -----------------------------------------------------------------------------

procedure AddRecognizedWords(var AWords: TSyncWords; const AText: String; const AStart, AEnd: Integer);
var
  w: TStringArray;
  i, l, total, pos, len: Integer;
  n: array of String;
  t: String;
begin
  // not speech: [Music], (applause), ♪
  t := Trim(AText);
  if (t = '') or t.Contains('♪') or
    (t.StartsWith('[') and t.EndsWith(']')) or (t.StartsWith('(') and t.EndsWith(')')) then Exit;

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
    AWords[l].Text      := w[i];
    AWords[l].Norm      := n[i];
    AWords[l].StartTime := AStart + Round((AEnd - AStart) * pos / total);
    AWords[l].EndTime   := AStart + Round((AEnd - AStart) * (pos + len) / total);
    AWords[l].Entry     := -1;
    AWords[l].Match     := -1;
    AWords[l].MatchLast := -1;
    pos += len;
  end;
end;

// -----------------------------------------------------------------------------

function WhisperDTWPreset(const AModel: String): String;
const
  Presets: array[0..11] of String = ('tiny', 'tiny.en', 'base', 'base.en',
    'small', 'small.en', 'medium', 'medium.en', 'large.v1', 'large.v2',
    'large.v3', 'large.v3.turbo');
var
  s: String;
  i: Integer;
begin
  // whisper.cpp -dtw preset of a model file name, e.g. ggml-large-v3-turbo-q5_0
  // -> large.v3.turbo, '' if unknown (a wrong preset gives no dtw times)
  Result := '';
  s := LowerCase(AModel);
  if s.StartsWith('ggml-') then Delete(s, 1, 5);
  i := s.LastIndexOf('-q');
  if (i > 0) and (i+2 < Length(s)) and (s[i+3] in ['0'..'9']) then
    s := Copy(s, 1, i);
  s := StringReplace(s, 'large-v', 'large.v', []);
  s := StringReplace(s, '-turbo', '.turbo', []);
  for i := 0 to High(Presets) do
    if s = Presets[i] then Exit(s);
end;

// -----------------------------------------------------------------------------

procedure LoadWhisperCppTokens(const ASegments: TJSONArray; var AWords: TSyncWords);
type
  TToken = record
    Text: String;
    From, Till, DTW: Integer;
  end;
var
  Tokens: array of TToken;
  Seg, Tok, Ofs: TJSONObject;
  Arr: TJSONArray;
  i, x, n, a, b, p, st, et, srtDur, maxDur: Integer;
  w: String;
  useDTW: Boolean;

  function IsPunct(const S: String): Boolean;
  var
    c: Char;
  begin
    for c in S do
      if not (c in ['.', ',', '!', '?', ';', ':', '"', '''', ')', '-', ' ']) then Exit(False);
    Result := True;
  end;

  function NextDTW(const AIndex: Integer): Integer;
  var
    k: Integer;
  begin
    Result := -1;
    for k := AIndex to High(Tokens) do
      if Tokens[k].DTW >= 0 then Exit(Tokens[k].DTW);
  end;

begin
  // whisper.cpp -ojf json: {"transcription": [{"offsets": {"from", "to"},
  // "tokens": [{"text", "offsets": {"from", "to"}, "t_dtw"}]}]}, offsets in ms,
  // t_dtw in 10 ms (-1 without -dtw). t_dtw is where a token starts in the
  // audio, more precise than the offsets (spread over the segment)
  Tokens := NIL;
  for i := 0 to ASegments.Count-1 do
    if (ASegments[i] is TJSONObject) then
    begin
      Seg := TJSONObject(ASegments[i]);
      if not (Seg.Find('tokens') is TJSONArray) then Continue;
      Arr := TJSONArray(Seg.Find('tokens'));
      for x := 0 to Arr.Count-1 do
        if Arr[x] is TJSONObject then
        begin
          Tok := TJSONObject(Arr[x]);
          w := Tok.Get('text', '');
          if w.StartsWith('[_') then Continue; // [_BEG_], [_TT_150]...
          n := Length(Tokens);
          SetLength(Tokens, n+1);
          Tokens[n].Text := w;
          Tokens[n].DTW  := -1;
          if Tok.Find('t_dtw') <> NIL then
            Tokens[n].DTW := Tok.Get('t_dtw', -1) * 10;
          if Tok.Find('offsets') is TJSONObject then
          begin
            Ofs := TJSONObject(Tok.Find('offsets'));
            Tokens[n].From := Ofs.Get('from', 0);
            Tokens[n].Till := Ofs.Get('to', 0);
          end
          else
          begin
            Tokens[n].From := 0;
            Tokens[n].Till := 0;
          end;
        end;
    end;

  a := 0;
  while a <= High(Tokens) do
  begin
    // a word: tokens until the next one starting with a space
    b := a;
    while (b < High(Tokens)) and not Tokens[b+1].Text.StartsWith(' ') do Inc(b);
    // word tokens without the trailing punctuation
    p := b;
    while (p > a) and IsPunct(Tokens[p].Text) do Dec(p);

    w := '';
    for x := a to b do w += Tokens[x].Text;

    st := Tokens[a].From;
    et := Tokens[p].Till;
    srtDur := et - st;
    if srtDur < 0 then srtDur := 0;

    useDTW := Tokens[a].DTW >= 0;
    if useDTW then
    begin
      // ends where the next token (its punctuation or the next word) starts
      n := NextDTW(p+1);
      // whisper puts a word before a silence until the next speech, limit it
      maxDur := Max(srtDur, 70 * UTF8Length(Trim(w)) + 150);
      if (n <= Tokens[a].DTW) or (n - Tokens[a].DTW > maxDur) then
        n := Tokens[a].DTW + Min(Max(srtDur, 100), maxDur);
      // dtw lost in this word, keep whisper times
      if Abs(Tokens[a].DTW - st) > 2000 then
        useDTW := False
      else
      begin
        st := Tokens[a].DTW;
        et := n;
      end;
    end;

    AddRecognizedWords(AWords, w, st, et);
    a := b+1;
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
      // whisper.cpp full json (-ojf)
      if (Data is TJSONObject) and (TJSONObject(Data).Find('transcription') is TJSONArray) then
      begin
        LoadWhisperCppTokens(TJSONArray(TJSONObject(Data).Find('transcription')), AWords);
        Exit(Length(AWords) > 0);
      end;

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

function WordScore(const A, B: String): Integer;
var
  ua, ub: UnicodeString;
  la, lb, i, j, c: Integer;
  prev, cur: array of Integer;
begin
  // 2 = equal, 1 = similar (whisper spelling, typos, plural...), 0 = no
  if A = B then Exit(2);
  Result := 0;
  ua := UTF8Decode(A);
  ub := UTF8Decode(B);
  la := Length(ua);
  lb := Length(ub);
  if (Min(la, lb) < 4) or (Abs(la - lb) > 2) then Exit;

  // Levenshtein distance
  SetLength(prev, lb+1);
  SetLength(cur, lb+1);
  for j := 0 to lb do prev[j] := j;
  for i := 1 to la do
  begin
    cur[0] := i;
    for j := 1 to lb do
    begin
      if ua[i] = ub[j] then c := 0 else c := 1;
      cur[j] := Min(Min(prev[j] + 1, cur[j-1] + 1), prev[j-1] + c);
    end;
    prev := Copy(cur);
  end;

  if prev[lb] <= Max(1, Max(la, lb) div 4) then
    Result := 1;
end;

// -----------------------------------------------------------------------------

// Needleman-Wunsch between ATextWords[t0..t1] and ARecognized[r0..r1]
procedure AlignRange(var ATextWords: TSyncWords; const ARecognized: TSyncWords; const t0, t1, r0, r1: Integer);
const
  MaxCells  = 6000000; // bigger gaps are left unmatched (interpolated)
  ScMatch   = 2;
  ScSimilar = 1;
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
      case WordScore(ATextWords[t0+i-1].Norm, ARecognized[r0+j-1].Norm) of
        2: d := Score[i-1][j-1] + ScMatch;
        1: d := Score[i-1][j-1] + ScSimilar;
      else
        d := Score[i-1][j-1] + ScMiss;
      end;
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
           if WordScore(ATextWords[t0+i-1].Norm, ARecognized[r0+j-1].Norm) > 0 then
           begin
             ATextWords[t0+i-1].Match     := r0+j-1;
             ATextWords[t0+i-1].MatchLast := r0+j-1;
           end;
           Dec(i);
           Dec(j);
         end;
      1: Dec(i);
    else
      Dec(j);
    end;
end;

// -----------------------------------------------------------------------------

// The words not matched between two matched ones: written together or
// apart ("cannot" / "can not"), or the same number of words on both sides
// (misrecognized words, numbers "10" / "ten"...) paired in order
procedure MatchLeftovers(var ATextWords: TSyncWords; const ARecognized: TSyncWords);
var
  i, k, t0, t1, r0, r1, j, n, tEnd: Integer;
begin
  i := 0;
  while i <= High(ATextWords) do
  begin
    if ATextWords[i].Match >= 0 then
    begin
      Inc(i);
      Continue;
    end;

    // text gap t0..t1, recognized gap r0..r1
    t0 := i;
    t1 := i;
    while (t1 < High(ATextWords)) and (ATextWords[t1+1].Match < 0) do Inc(t1);
    if t0 > 0 then r0 := ATextWords[t0-1].MatchLast + 1 else r0 := 0;
    if t1 < High(ATextWords) then r1 := ATextWords[t1+1].Match - 1 else r1 := High(ARecognized);

    tEnd := t1;
    if (r1 >= r0) and (r1 - r0 <= 12) and (t1 - t0 <= 12) then
    begin
      // together/apart
      k := t0;
      j := r0;
      while (k <= t1) and (j <= r1) do
      begin
        if (j < r1) and (ATextWords[k].Norm = ARecognized[j].Norm + ARecognized[j+1].Norm) then
        begin
          ATextWords[k].Match     := j;
          ATextWords[k].MatchLast := j+1;
          Inc(k);
          Inc(j, 2);
        end
        else if (k < t1) and (ATextWords[k].Norm + ATextWords[k+1].Norm = ARecognized[j].Norm) then
        begin
          ATextWords[k].Match       := j;
          ATextWords[k].MatchLast   := j;
          ATextWords[k+1].Match     := j;
          ATextWords[k+1].MatchLast := j;
          Inc(k, 2);
          Inc(j);
        end
        else if WordScore(ATextWords[k].Norm, ARecognized[j].Norm) > 0 then
        begin
          ATextWords[k].Match     := j;
          ATextWords[k].MatchLast := j;
          Inc(k);
          Inc(j);
        end
        else
          Break;
      end;

      // the same from the end (words not spoken in the middle)
      while (t1 >= k) and (r1 >= j) do
      begin
        if (r1 > j) and (ATextWords[t1].Norm = ARecognized[r1-1].Norm + ARecognized[r1].Norm) then
        begin
          ATextWords[t1].Match     := r1-1;
          ATextWords[t1].MatchLast := r1;
          Dec(t1);
          Dec(r1, 2);
        end
        else if (t1 > k) and (ATextWords[t1-1].Norm + ATextWords[t1].Norm = ARecognized[r1].Norm) then
        begin
          ATextWords[t1].Match       := r1;
          ATextWords[t1].MatchLast   := r1;
          ATextWords[t1-1].Match     := r1;
          ATextWords[t1-1].MatchLast := r1;
          Dec(t1, 2);
          Dec(r1);
        end
        else if WordScore(ATextWords[t1].Norm, ARecognized[r1].Norm) > 0 then
        begin
          ATextWords[t1].Match     := r1;
          ATextWords[t1].MatchLast := r1;
          Dec(t1);
          Dec(r1);
        end
        else
          Break;
      end;

      // the same number of words left on both sides: in order
      n := t1 - k + 1;
      if (n > 0) and (n <= 3) and (r1 - j + 1 = n) then
        for n := 0 to n-1 do
        begin
          ATextWords[k+n].Match     := j+n;
          ATextWords[k+n].MatchLast := j+n;
        end;
    end;

    i := tEnd + 1;
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
  begin
    ATextWords[i].Match     := -1;
    ATextWords[i].MatchLast := -1;
  end;
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
    begin
      ATextWords[Chain[i].T+x].Match     := Chain[i].R+x;
      ATextWords[Chain[i].T+x].MatchLast := Chain[i].R+x;
    end;

    t0 := Chain[i].T + 3;
    r0 := Chain[i].R + 3;
  end;
  AlignRange(ATextWords, ARecognized, t0, High(ATextWords), r0, High(ARecognized));

  // 3. what is left between matched words
  MatchLeftovers(ATextWords, ARecognized);
end;

// -----------------------------------------------------------------------------

function BuildSyncTimes(const ATextWords: TSyncWords; const ARecognized: TSyncWords; const AEntryCount: Integer; const AEntryChars: array of Integer; const AMinDuration, AMinPause: Integer; out AMatchedWords: Integer): TSyncTimes;
const
  LeadIn  = 100; // ms before the voice
  LeadOut = 400; // ms after the voice, if there is room until the next one
var
  i, j, e, a, b, span, chars, acc, need, give, lo, hi: Integer;
  msPerChar: Double;
  First, Last, Before, After: array of Integer;
  total, dur: Int64;
begin
  SetLength(Result, AEntryCount);
  for i := 0 to AEntryCount-1 do
  begin
    Result[i].InitialTime := -1;
    Result[i].FinalTime   := -1;
    Result[i].Matched     := False;
  end;

  // speech rate, to estimate the time of the words not found
  total := 0;
  dur := 0;
  for i := 0 to High(ARecognized) do
  begin
    total += UTF8Length(ARecognized[i].Norm) + 1;
    dur += ARecognized[i].EndTime - ARecognized[i].StartTime;
  end;
  if total > 0 then msPerChar := EnsureRange(dur / total, 35, 150) else msPerChar := 70;

  // first/last matched word of every entry, and the chars of the words not
  // found before/after them
  SetLength(First, AEntryCount);
  SetLength(Last, AEntryCount);
  SetLength(Before, AEntryCount);
  SetLength(After, AEntryCount);
  for e := 0 to AEntryCount-1 do
  begin
    First[e] := -1;
    Last[e] := -1;
    Before[e] := 0;
    After[e] := 0;
  end;

  AMatchedWords := 0;
  for i := 0 to High(ATextWords) do
  begin
    e := ATextWords[i].Entry;
    if ATextWords[i].Match >= 0 then
    begin
      Inc(AMatchedWords);
      if First[e] < 0 then First[e] := i;
      Last[e] := i;
      After[e] := 0;
    end
    else if First[e] < 0 then
      Before[e] += UTF8Length(ATextWords[i].Norm) + 1
    else
      After[e] += UTF8Length(ATextWords[i].Norm) + 1;
  end;

  // entries with matched words, extended by the words not found at the
  // start/end (otherwise they start late or end early)
  for e := 0 to AEntryCount-1 do
    if First[e] >= 0 then
    begin
      Result[e].InitialTime := ARecognized[ATextWords[First[e]].Match].StartTime;
      Result[e].FinalTime   := ARecognized[ATextWords[Last[e]].MatchLast].EndTime;
      if Result[e].FinalTime < Result[e].InitialTime then
        Result[e].FinalTime := Result[e].InitialTime;
      Result[e].InitialTime := Max(0, Result[e].InitialTime - Round(Before[e] * msPerChar));
      Result[e].FinalTime   := Result[e].FinalTime + Round(After[e] * msPerChar);
      Result[e].Matched     := True;
    end;

  // keep the order: a wrong match far away would cross its neighbors, small
  // overlaps (from the estimated words) are trimmed
  b := 0;
  for i := 0 to AEntryCount-1 do
    if Result[i].Matched then
    begin
      if ARecognized[ATextWords[First[i]].Match].StartTime < b then
      begin
        Result[i].Matched := False;
        Continue;
      end;
      if Result[i].InitialTime < b then Result[i].InitialTime := b;
      b := Result[i].FinalTime;
    end;
  for i := AEntryCount-2 downto 0 do
    if Result[i].Matched and Result[i+1].Matched and (Result[i].FinalTime > Result[i+1].InitialTime) then
      Result[i].FinalTime := Max(Result[i].InitialTime, Result[i+1].InitialTime);

  // entries without matches: share the time between the matched neighbors
  // by their length, taking some time from them if there is not enough
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

    need := (j - i) * (AMinDuration + AMinPause);
    if b - a < need then
    begin
      give := need - (b - a);
      if i > 0 then // from the end of the previous one
      begin
        lo := Max(Result[i-1].InitialTime + AMinDuration, a - (give + 1) div 2);
        if lo < a then
        begin
          give -= a - lo;
          a := lo;
          Result[i-1].FinalTime := a;
        end;
      end;
      if (give > 0) and (j < AEntryCount) then // from the start of the next one
      begin
        hi := Min(Result[j].FinalTime - AMinDuration, b + give);
        if hi > b then
        begin
          b := hi;
          Result[j].InitialTime := b;
        end;
      end
      else if (give > 0) and (j = AEntryCount) then
        b := b + give; // after the last one
    end;

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

  // lead in (before the voice) without touching the previous one
  for i := 0 to AEntryCount-1 do
  begin
    if i > 0 then lo := Result[i-1].FinalTime + AMinPause else lo := 0;
    if Result[i].InitialTime - LeadIn >= lo then
      Result[i].InitialTime := Result[i].InitialTime - LeadIn
    else if Result[i].InitialTime < lo then
      Result[i].InitialTime := Min(lo, Max(Result[i].InitialTime, Result[i].FinalTime));
  end;

  // lead out and minimum duration, until the next one (minus the gap)
  for i := 0 to AEntryCount-1 do
  begin
    if i < AEntryCount-1 then
      hi := Result[i+1].InitialTime - AMinPause
    else
      hi := MaxInt;

    b := Max(Result[i].FinalTime + LeadOut, Result[i].InitialTime + AMinDuration);
    Result[i].FinalTime := Max(Result[i].FinalTime, Min(b, hi));
    if Result[i].FinalTime > hi then
      Result[i].FinalTime := Max(Result[i].InitialTime, hi);
  end;
end;

// -----------------------------------------------------------------------------

function NoEndWords(const ALanguage: String): String;
var
  l: String;
begin
  l := LowerCase(Copy(ALanguage, 1, 2));
  if l = 'en' then Result := NoEndEN
  else if l = 'es' then Result := NoEndES
  else if l = 'pt' then Result := NoEndPT
  else if l = 'fr' then Result := NoEndFR
  else if l = 'it' then Result := NoEndIT
  else if l = 'de' then Result := NoEndDE
  else // unknown (detected by whisper), all of them
    Result := NoEndEN + NoEndES + NoEndPT + NoEndFR + NoEndIT + NoEndDE;
end;

// -----------------------------------------------------------------------------

function EndsWithAny(const AText: String; const AChars: array of String): Boolean;
const
  Closing: array[0..6] of String = ('"', '''', ')', ']', '»', '”', '’');
var
  s, c: String;
  Found: Boolean;
begin
  s := TrimRight(AText);
  // closing quotes/brackets after the punctuation: "Yes." or (Yes.)
  repeat
    Found := False;
    for c in Closing do
      if (s <> '') and s.EndsWith(c) then
      begin
        SetLength(s, Length(s) - Length(c));
        Found := True;
      end;
  until not Found;
  for c in AChars do
    if s.EndsWith(c) then Exit(True);
  Result := False;
end;

// -----------------------------------------------------------------------------

function SegmentWords(const AWords: TSyncWords; const AOptions: TSegmentOptions): TSegments;
var
  NoEnd: String;
  MaxChars, cs, i, k: Integer;

  function JoinText(const A, B: Integer): String;
  var
    x: Integer;
  begin
    Result := '';
    for x := A to B do
      if Result = '' then
        Result := Trim(AWords[x].Text)
      else
        Result := Result + ' ' + Trim(AWords[x].Text);
  end;

  function TextLen(const A, B: Integer): Integer;
  begin
    Result := UTF8Length(JoinText(A, B));
  end;

  function Fits(const A, B: Integer): Boolean;
  begin
    Result := (TextLen(A, B) <= MaxChars) and
      ((AOptions.MaxDuration <= 0) or (AWords[B].EndTime - AWords[A].StartTime <= AOptions.MaxDuration));
  end;

  function SentenceEnd(const AIndex: Integer): Boolean;
  begin
    Result := EndsWithAny(AWords[AIndex].Text, ['.', '?', '!', '…', '。', '？', '！']);
  end;

  // the best word to end the subtitle A..B (B is the last candidate)
  function BestBreak(const A, B: Integer): Integer;
  var
    x, sc, best, gap: Integer;
  begin
    Result := B;
    best := Low(Integer);
    for x := A to B do
    begin
      if not Fits(A, x) then Break; // longer won't fit either
      sc := 0;
      if SentenceEnd(x) then
        sc += 30
      else if EndsWithAny(AWords[x].Text, [',', ';', ':', '—', '–']) then
        sc += 15;
      if x < High(AWords) then
      begin
        gap := AWords[x+1].StartTime - AWords[x].EndTime;
        sc += Min(25, Max(0, gap) div 20);
      end;
      if Pos(' ' + AWords[x].Norm + ' ', NoEnd) > 0 then
        sc -= 20;
      // prefer using the room (fewer, fuller subtitles)
      sc += Round(15 * TextLen(A, x) / MaxChars);
      if sc >= best then
      begin
        best := sc;
        Result := x;
      end;
    end;
  end;

  procedure Flush(const A, B: Integer);
  var
    l: Integer;
    t: String;
  begin
    if B < A then Exit;
    t := JoinText(A, B);
    if t = '' then Exit;
    if UTF8Length(t) > AOptions.CPL then
      t := AutoBreakSubtitle(t, AOptions.CPL);
    l := Length(Result);
    SetLength(Result, l+1);
    Result[l].InitialTime := AWords[A].StartTime;
    Result[l].FinalTime   := AWords[B].EndTime;
    Result[l].Text        := t;
  end;

var
  want, limit, chars: Integer;
begin
  SetLength(Result, 0);
  if Length(AWords) = 0 then Exit;

  NoEnd := NoEndWords(AOptions.Language);
  MaxChars := Max(1, AOptions.CPL) * Max(1, AOptions.MaxLines);

  cs := 0;
  for i := 0 to High(AWords) do
  begin
    // a silence always starts a new subtitle
    if (i > cs) and (AOptions.PauseSplit > 0) and
      (AWords[i].StartTime - AWords[i-1].EndTime >= AOptions.PauseSplit) then
    begin
      Flush(cs, i-1);
      cs := i;
    end;

    // too long with this word: end the subtitle at the best place before it
    while (i > cs) and not Fits(cs, i) do
    begin
      k := BestBreak(cs, i-1);
      Flush(cs, k);
      cs := k+1;
    end;

    // the end of a sentence ends the subtitle when it's not too short (short
    // sentences go together: "Yes. Come here.")
    if SentenceEnd(i) and (TextLen(cs, i) >= MaxChars div 4) then
    begin
      Flush(cs, i);
      cs := i+1;
    end;
  end;
  Flush(cs, High(AWords));

  // duration: the minimum and enough for the reading speed, using the room
  // until the next subtitle
  for i := 0 to High(Result) do
  begin
    if i < High(Result) then
      limit := Result[i+1].InitialTime - AOptions.MinPause
    else
      limit := MaxInt;

    chars := UTF8Length(Result[i].Text);
    want := AOptions.MinDuration;
    if AOptions.MaxCPS > 0 then
      want := Max(want, Round(chars / AOptions.MaxCPS * 1000));
    if AOptions.MaxDuration > 0 then
      want := Min(want, AOptions.MaxDuration);

    if Result[i].FinalTime - Result[i].InitialTime < want then
      Result[i].FinalTime := Max(Result[i].FinalTime, Min(Result[i].InitialTime + want, limit));

    if Result[i].FinalTime > limit then
      Result[i].FinalTime := Max(Result[i].InitialTime, limit);
  end;
end;

// -----------------------------------------------------------------------------

end.
