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

unit DolbyVision;

// -----------------------------------------------------------------------------
//
//  Dolby Vision metadata (DolbyLabsMDF, the XML that comes with the ProRes of
//  a Dolby Vision grade, CM v2.9 and v4.0).
//
//  Each <Shot> of the (first) video track is one entry, from its
//  <Record><In> for <Record><Duration> frames. Only the times are edited:
//  the file is kept as it was read (Subtitles.SourceText) and, when saved,
//  only these values are rewritten, the rest is written byte for byte:
//
//    <Record><In>, <Record><Duration>   the times of the entry
//    <Source><In>                       if the head of the shot changed
//    <Frame><EditOffset>                per frame metadata (dissolves, fades)
//    <UniqueID> of the shot             only in a copy of a shot (split)
//
//  Everything is done in whole frames of the <EditRate> of the track, never
//  in ms: the entries have the times of their frames, the frame of the time
//  0 is Subtitles.SourceOrigin (by default 0, so the entries show the record
//  timecode of the XML; the video is aligned with the video offset).
//
//  Changes of the frames inside of a shot (a deleted duplicated frame, an
//  inserted frame, a trimmed head) are kept in its extra info as a list of
//  edits, so the per frame metadata follows its frames and the <Source><In>
//  is moved with the head of the shot. See DoViNormalizeEdits.
//
// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, UWSubtitleAPI, UWSubtitleAPI.Formats, UWSubtitleAPI.ExtraInfo,
  UWSystem.TimeUtils;

type

  { TUWDolbyVision }

  TUWDolbyVision = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function IsTimeBased: Boolean; override;
    function IsFrameBased: Boolean; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

  { Shots of the XML }

  TDoViFrame = record
    StartPos, EndPos : Integer; // text of the <Frame>, [StartPos, EndPos)
    Offset           : Integer; // <EditOffset>
  end;
  TDoViFrameArray = array of TDoViFrame;

  TDoViShot = record
    StartPos, EndPos : Integer; // text of the <Shot>, [StartPos, EndPos)
    RecordIn         : Integer; // <Record><In>
    Duration         : Integer; // <Record><Duration>
    SourceIn         : Integer; // <Source><In>, -1 = without <Source>
    UniqueID         : String;
    Frames           : TDoViFrameArray;
  end;
  TDoViShotArray = array of TDoViShot;

  TDoViDocument = record
    Shots     : TDoViShotArray;
    RateNum   : Integer;        // <EditRate> 24000 1001
    RateDen   : Integer;
    PrefixEnd : Integer;        // the text before the first shot is [1, PrefixEnd)
    SuffixPos : Integer;        // the text after the last shot is [SuffixPos, ...]
  end;

{ Helpers }

function DoViParse(const AXML: String; out ADoc: TDoViDocument): Boolean;
function DoViFramesToMS(const AFrames: Int64; const ANum, ADen: Integer): Integer;
function DoViMSToFrames(const AMS: Int64; const ANum, ADen: Integer): Integer;
function DoViGetRate(const Subtitles: TUWSubtitles; out ANum, ADen: Integer): Boolean;
function DoViFramesToTC(const AFrames: Integer; const ANum, ADen: Integer): String;
function DoViTCToFrames(const ATC: String; const ANum, ADen: Integer; out AFrames: Integer): Boolean;
function DoViEditsLength(const AEdits: String; const ALength: Integer): Integer;
procedure DoViNormalizeEdits(var AInfo: TDolbyVision_ExtraInfo; const ARecordIn, ARecordOut: Integer);
function DoViBuildXML(const Subtitles: TUWSubtitles; const AFromItem, AToItem: Integer; out AXML: String): Boolean;

{ Editing of the shots, in record frames (the frames of the XML) }

type

  TDoViChange = record
    Index    : Integer;                // entry
    RecordIn : Integer;                // new times, record frames
    RecordOut: Integer;
    Info     : TDolbyVision_ExtraInfo; // new extra info
    Delete   : Boolean;                // the shot is left without frames
  end;
  TDoViChangeArray = array of TDoViChange;

  TDoViCutCheckKind = (cckMoved, cckNoShotChange, cckNoCut, cckDone);

  TDoViCutCheck = record
    Kind       : TDoViCutCheckKind;
    Index      : Integer; // entry that starts at the cut (-1 for cckNoCut)
    CutFrame   : Integer; // record frame of the cut (cckNoCut: of the shot change)
    ShotFrame  : Integer; // record frame of the nearest shot change (cckMoved)
  end;
  TDoViCutCheckArray = array of TDoViCutCheck;

  TDoViProblems = record
    Gaps, Overlaps, Empty, NoShot: Integer;
    First: Integer; // first entry with a problem, -1 = none
  end;

function DoViIsEditable(const Subtitles: TUWSubtitles): Boolean;
function DoViRecordFrame(const Subtitles: TUWSubtitles; const AMS: Integer): Integer;
function DoViRecordToMS(const Subtitles: TUWSubtitles; const AFrame: Integer): Integer;
procedure DoViEntryFrames(const Subtitles: TUWSubtitles; const AIndex: Integer; out ARecordIn, ARecordOut: Integer);
function DoViEntryInfo(const Subtitles: TUWSubtitles; const AIndex: Integer; out AInfo: TDolbyVision_ExtraInfo): Boolean;
function DoViPlanDeleteFrames(const Subtitles: TUWSubtitles; const AFrame, ACount: Integer): TDoViChangeArray;
function DoViPlanInsertFrames(const Subtitles: TUWSubtitles; const AFrame, ACount: Integer; const AExtendPrevious: Boolean): TDoViChangeArray;
function DoViCheckCuts(const Subtitles: TUWSubtitles; const AShotChangesMS: array of Integer; const ATolerance: Integer): TDoViCutCheckArray;
function DoViCheckProblems(const Subtitles: TUWSubtitles): TDoViProblems;

// -----------------------------------------------------------------------------

implementation

uses Math;

// -----------------------------------------------------------------------------

{ Text helpers }

// -----------------------------------------------------------------------------

function FindFrom(const ASub, S: String; const AFrom: Integer; const ATo: Integer = MaxInt): Integer;
var
  i, n, Last: Integer;
begin
  // Pos that doesn't look after ATo (the found text starts before ATo)
  Result := 0;
  n := Length(ASub);
  if (n = 0) or (AFrom < 1) then Exit;
  Last := Length(S) - n + 1;
  if ATo - 1 < Last then Last := ATo - 1;
  for i := AFrom to Last do
    if (S[i] = ASub[1]) and (CompareByte(S[i], ASub[1], n) = 0) then
      Exit(i);
end;

// -----------------------------------------------------------------------------

function FindElement(const AName, S: String; const AFrom: Integer; const ATo: Integer = MaxInt): Integer;
var
  p: Integer;
  c: Char;
begin
  // <Name> or <Name attr...>, not <NameX>
  Result := 0;
  p := AFrom;
  repeat
    p := FindFrom('<' + AName, S, p, ATo);
    if p = 0 then Exit;
    if p + Length(AName) + 1 <= Length(S) then
    begin
      c := S[p + Length(AName) + 1];
      if c in ['>', ' ', #9, #10, #13, '/'] then Exit(p);
    end;
    Inc(p);
  until False;
end;

// -----------------------------------------------------------------------------

function LineStart(const S: String; const APos: Integer): Integer;
begin
  // the indentation before an element is part of its block
  Result := APos;
  while (Result > 1) and (S[Result-1] in [' ', #9]) do Dec(Result);
end;

// -----------------------------------------------------------------------------

function LineEndAfter(const S: String; const APos: Integer): Integer;
begin
  // position after the end of line that follows APos (one #13#10, #10 or #13)
  Result := APos;
  if (Result <= Length(S)) and (S[Result] = #13) then Inc(Result);
  if (Result <= Length(S)) and (S[Result] = #10) then Inc(Result);
end;

// -----------------------------------------------------------------------------

function ValuePos(const AName, S: String; const AFrom, ATo: Integer; out AValueStart, AValueEnd: Integer): Boolean;
var
  p, q: Integer;
begin
  // <Name>value</Name> between AFrom and ATo, the value is [AValueStart, AValueEnd)
  Result := False;
  p := FindElement(AName, S, AFrom, ATo);
  if p = 0 then Exit;
  q := FindFrom('>', S, p, ATo);
  if (q = 0) or (S[q-1] = '/') then Exit;
  AValueStart := q + 1;
  AValueEnd := FindFrom('</' + AName + '>', S, AValueStart, ATo);
  Result := AValueEnd > 0;
end;

// -----------------------------------------------------------------------------

function ReadIntValue(const AName, S: String; const AFrom, ATo: Integer; const ADefault: Integer): Integer;
var
  a, b: Integer;
begin
  if ValuePos(AName, S, AFrom, ATo, a, b) then
    Result := StrToIntDef(Trim(Copy(S, a, b-a)), ADefault)
  else
    Result := ADefault;
end;

// -----------------------------------------------------------------------------

function NewUniqueID: String;
var
  G: TGUID;
begin
  CreateGUID(G);
  Result := LowerCase(GUIDToString(G));
  Result := Copy(Result, 2, Length(Result)-2); // without the braces
end;

// -----------------------------------------------------------------------------

{ Parser }

// -----------------------------------------------------------------------------

function DoViParse(const AXML: String; out ADoc: TDoViDocument): Boolean;
var
  p, ShotStart, ShotClose, TrackEnd, RecPos, RecEnd, SrcPos, SrcEnd,
  n, f, FrameStart, FrameClose, a, b: Integer;
  sRate: String;
  sl: TStringList;
begin
  Result := False;
  ADoc := Default(TDoViDocument);
  ADoc.Shots := NIL;
  ADoc.RateNum := 24;
  ADoc.RateDen := 1;

  if Pos('<DolbyLabsMDF', AXML) = 0 then Exit;

  p := FindElement('Shot', AXML, 1);
  if p = 0 then Exit;

  // <EditRate> of the track of the shots: the last one before the first shot
  a := 1;
  b := 0;
  repeat
    n := FindFrom('<EditRate>', AXML, a, p);
    if n > 0 then
    begin
      b := n;
      a := n + 1;
    end;
  until n = 0;
  if b > 0 then
  begin
    n := FindFrom('</EditRate>', AXML, b);
    if n > 0 then
    begin
      sRate := Trim(Copy(AXML, b + 10, n - b - 10));
      sl := TStringList.Create;
      try
        sl.Delimiter := ' ';
        sl.StrictDelimiter := False;
        sl.DelimitedText := sRate;
        if sl.Count >= 1 then ADoc.RateNum := StrToIntDef(sl[0], 24);
        if sl.Count >= 2 then ADoc.RateDen := StrToIntDef(sl[1], 1);
      finally
        sl.Free;
      end;
      if ADoc.RateNum <= 0 then ADoc.RateNum := 24;
      if ADoc.RateDen <= 0 then ADoc.RateDen := 1;
    end;
  end;

  // only the shots of the first track, the others stay as they are
  TrackEnd := FindFrom('</Track>', AXML, p);
  if TrackEnd = 0 then TrackEnd := MaxInt;

  ADoc.PrefixEnd := LineStart(AXML, p);
  n := 0;
  while p > 0 do
  begin
    ShotStart := LineStart(AXML, p);
    ShotClose := FindFrom('</Shot>', AXML, p, TrackEnd);
    if ShotClose = 0 then Exit; // broken

    SetLength(ADoc.Shots, n+1);
    with ADoc.Shots[n] do
    begin
      StartPos := ShotStart;
      EndPos   := LineEndAfter(AXML, ShotClose + 7);

      // the shot's own UniqueID is the first one, before <Source>/<Record>
      if ValuePos('UniqueID', AXML, p, ShotClose, a, b) then
        UniqueID := Trim(Copy(AXML, a, b-a))
      else
        UniqueID := '';

      RecPos := FindElement('Record', AXML, p, ShotClose);
      if RecPos = 0 then Exit;
      RecEnd := FindFrom('</Record>', AXML, RecPos, ShotClose);
      if RecEnd = 0 then Exit;
      RecordIn := ReadIntValue('In', AXML, RecPos, RecEnd, -1);
      Duration := ReadIntValue('Duration', AXML, RecPos, RecEnd, -1);
      if (RecordIn < 0) or (Duration < 0) then Exit;

      SourceIn := -1;
      SrcPos := FindElement('Source', AXML, p, ShotClose);
      if SrcPos > 0 then
      begin
        SrcEnd := FindFrom('</Source>', AXML, SrcPos, ShotClose);
        if SrcEnd > 0 then
          SourceIn := ReadIntValue('In', AXML, SrcPos, SrcEnd, -1);
      end;

      // per frame metadata
      Frames := NIL;
      f := 0;
      FrameStart := FindElement('Frame', AXML, p, ShotClose);
      while FrameStart > 0 do
      begin
        FrameClose := FindFrom('</Frame>', AXML, FrameStart, ShotClose);
        if FrameClose = 0 then Exit;
        SetLength(Frames, f+1);
        Frames[f].StartPos := LineStart(AXML, FrameStart);
        Frames[f].EndPos   := LineEndAfter(AXML, FrameClose + 8);
        Frames[f].Offset   := ReadIntValue('EditOffset', AXML, FrameStart, FrameClose, -1);
        Inc(f);
        FrameStart := FindElement('Frame', AXML, FrameClose, ShotClose);
      end;
    end;

    // a shot ends where the next one begins (what is between them goes with it)
    if n > 0 then ADoc.Shots[n-1].EndPos := ShotStart;
    Inc(n);
    p := FindElement('Shot', AXML, ShotClose, TrackEnd);
  end;

  ADoc.SuffixPos := ADoc.Shots[n-1].EndPos;
  Result := n > 0;
end;

// -----------------------------------------------------------------------------

function DoViFramesToMS(const AFrames: Int64; const ANum, ADen: Integer): Integer;
begin
  // the time of the first ms of the frame, rounded (exact both ways)
  Result := Round(AFrames * 1000.0 * ADen / ANum);
end;

// -----------------------------------------------------------------------------

function DoViMSToFrames(const AMS: Int64; const ANum, ADen: Integer): Integer;
begin
  Result := Round(AMS * ANum / (1000.0 * ADen));
end;

// -----------------------------------------------------------------------------

function DoViGetRate(const Subtitles: TUWSubtitles; out ANum, ADen: Integer): Boolean;
var
  p, q: Integer;
  sl: TStringList;
begin
  ANum := 24;
  ADen := 1;
  Result := False;
  if (Subtitles = NIL) or (Subtitles.SourceText = '') then Exit;
  p := Pos('<EditRate>', Subtitles.SourceText);
  q := Pos('</EditRate>', Subtitles.SourceText);
  if (p = 0) or (q < p) then Exit;
  sl := TStringList.Create;
  try
    sl.Delimiter := ' ';
    sl.DelimitedText := Trim(Copy(Subtitles.SourceText, p + 10, q - p - 10));
    if sl.Count >= 1 then ANum := StrToIntDef(sl[0], 24);
    if sl.Count >= 2 then ADen := StrToIntDef(sl[1], 1);
  finally
    sl.Free;
  end;
  if ANum <= 0 then ANum := 24;
  if ADen <= 0 then ADen := 1;
  Result := True;
end;

// -----------------------------------------------------------------------------

function NominalFPS(const ANum, ADen: Integer): Integer;
begin
  // timecode base: 24000/1001 counts 24 frames per second (non drop frame)
  Result := Max(1, Round(ANum / ADen));
end;

// -----------------------------------------------------------------------------

function DoViFramesToTC(const AFrames: Integer; const ANum, ADen: Integer): String;
var
  fps, f, h, m, s: Integer;
  Neg: Boolean;
begin
  fps := NominalFPS(ANum, ADen);
  Neg := AFrames < 0;
  f := Abs(AFrames);
  h := f div (fps * 3600); f := f mod (fps * 3600);
  m := f div (fps * 60);   f := f mod (fps * 60);
  s := f div fps;          f := f mod fps;
  Result := SysUtils.Format('%.2d:%.2d:%.2d:%.2d', [h, m, s, f]);
  if Neg then Result := '-' + Result;
end;

// -----------------------------------------------------------------------------

function DoViTCToFrames(const ATC: String; const ANum, ADen: Integer; out AFrames: Integer): Boolean;
var
  sl: TStringList;
  fps, i, v: Integer;
  S: String;
begin
  // hh:mm:ss:ff (also ; or . as separators), or a frame number
  Result := False;
  AFrames := 0;
  S := Trim(ATC);
  if S = '' then Exit;

  if TryStrToInt(S, v) then
  begin
    AFrames := v;
    Exit(True);
  end;

  fps := NominalFPS(ANum, ADen);
  sl := TStringList.Create;
  try
    S := StringReplace(S, ';', ':', [rfReplaceAll]);
    S := StringReplace(S, '.', ':', [rfReplaceAll]);
    sl.Delimiter := ':';
    sl.StrictDelimiter := True;
    sl.DelimitedText := S;
    if sl.Count <> 4 then Exit;
    for i := 0 to 3 do
      if not TryStrToInt(Trim(sl[i]), v) or (v < 0) then Exit;
    if StrToInt(sl[3]) >= fps then Exit;
    AFrames := ((StrToInt(sl[0]) * 60 + StrToInt(sl[1])) * 60 + StrToInt(sl[2])) * fps + StrToInt(sl[3]);
    Result := True;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

{ Edits of the frames of a shot }

// -----------------------------------------------------------------------------

type
  // Map[j] = original frame (offset in the original shot) shown at the frame j,
  // a negative value -(o+1) is an inserted copy of the original frame o
  TFrameMap = array of Integer;

// -----------------------------------------------------------------------------

function MapOrig(const V: Integer): Integer; inline;
begin
  if V < 0 then Result := -V - 1 else Result := V;
end;

// -----------------------------------------------------------------------------

procedure ApplyEdits(var Map: TFrameMap; const AEdits: String);
var
  sl: TStringList;
  i, j, p, n, c, src: Integer;
  e: String;
begin
  if AEdits = '' then Exit;
  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    sl.StrictDelimiter := True;
    sl.DelimitedText := AEdits;
    for i := 0 to sl.Count-1 do
    begin
      e := Trim(sl[i]);
      if Length(e) < 4 then Continue;
      c := Pos(',', e);
      if c = 0 then Continue;
      p := StrToIntDef(Copy(e, 2, c-2), -1);
      n := StrToIntDef(Copy(e, c+1, MaxInt), 0);
      if (p < 0) or (n <= 0) then Continue;
      case e[1] of
        'd': begin
               if p >= Length(Map) then Continue;
               n := Min(n, Length(Map) - p);
               for j := p to High(Map) - n do Map[j] := Map[j+n];
               SetLength(Map, Length(Map) - n);
             end;
        'i': begin
               p := Min(p, Length(Map));
               if Length(Map) = 0 then
                 src := 0
               else if p > 0 then
                 src := MapOrig(Map[p-1])
               else
                 src := MapOrig(Map[0]);
               SetLength(Map, Length(Map) + n);
               for j := High(Map) downto p + n do Map[j] := Map[j-n];
               for j := p to p + n - 1 do Map[j] := -(src + 1);
             end;
      end;
    end;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

function DoViEditsLength(const AEdits: String; const ALength: Integer): Integer;
var
  Map: TFrameMap;
  i: Integer;
begin
  // length of a shot of ALength frames after the edits
  Map := NIL;
  SetLength(Map, Max(0, ALength));
  for i := 0 to High(Map) do Map[i] := i;
  ApplyEdits(Map, AEdits);
  Result := Length(Map);
end;

// -----------------------------------------------------------------------------

procedure DoViNormalizeEdits(var AInfo: TDolbyVision_ExtraInfo; const ARecordIn, ARecordOut: Integer);
var
  dS, dE, t, L: Integer;
begin
  // The entry was changed out of the Dolby Vision tools (timeline, shift
  // times, split...), from [BaseIn, BaseOut) to [ARecordIn, ARecordOut):
  //  - both ends moved the same: the shot was moved, its frames are the same
  //  - only the start moved: the head of the shot was trimmed (or extended)
  //  - else: moved by the change of the start and the tail trimmed/extended
  // The change is added to the edits and the base is the new times.
  dS := ARecordIn - AInfo.BaseIn;
  dE := ARecordOut - AInfo.BaseOut;
  L  := AInfo.BaseOut - AInfo.BaseIn;

  if dS <> dE then
  begin
    if dE = 0 then
    begin
      if dS > 0 then
        AInfo.Edits := AInfo.Edits + SysUtils.Format('d0,%d;', [dS])
      else
        AInfo.Edits := AInfo.Edits + SysUtils.Format('i0,%d;', [-dS]);
    end
    else
    begin
      t := dE - dS;
      if t < 0 then
        AInfo.Edits := AInfo.Edits + SysUtils.Format('d%d,%d;', [Max(0, L + t), -t])
      else
        AInfo.Edits := AInfo.Edits + SysUtils.Format('i%d,%d;', [Max(0, L), t]);
    end;
  end;

  AInfo.BaseIn  := ARecordIn;
  AInfo.BaseOut := ARecordOut;
end;

// -----------------------------------------------------------------------------

{ Writer }

// -----------------------------------------------------------------------------

procedure ReplaceValue(var S: String; const AName: String; const AFrom, ATo: Integer; const AValue: String; const AParent: String = '');
var
  a, b, p, q: Integer;
begin
  // value of <AName> (inside of <AParent> if given), only if it's different
  p := AFrom;
  q := ATo;
  if AParent <> '' then
  begin
    p := FindElement(AParent, S, AFrom, ATo);
    if p = 0 then Exit;
    q := FindFrom('</' + AParent + '>', S, p, ATo);
    if q = 0 then Exit;
  end;
  if ValuePos(AName, S, p, q, a, b) and (Trim(Copy(S, a, b-a)) <> AValue) then
  begin
    Delete(S, a, b-a);
    Insert(AValue, S, a);
  end;
end;

// -----------------------------------------------------------------------------

function ShotText(const AXML: String; const AShot: TDoViShot; const ARecordIn, ADuration: Integer;
  const AEdits: String; const ANewID: Boolean; const AKeepFrames: Boolean): String;
var
  Head, Tail, FrameText, Frames: String;
  Map: TFrameMap;
  ByOffset: array of Integer;
  HeadEnd, TailStart, i, k, o, SrcIn: Integer;
begin
  // head: from <Shot> to the first <Frame>, tail: after the last </Frame>
  if Length(AShot.Frames) > 0 then
  begin
    HeadEnd   := AShot.Frames[0].StartPos;
    TailStart := AShot.Frames[High(AShot.Frames)].EndPos;
  end
  else
  begin
    HeadEnd   := AShot.EndPos;
    TailStart := AShot.EndPos;
  end;
  Head := Copy(AXML, AShot.StartPos, HeadEnd - AShot.StartPos);
  Tail := Copy(AXML, TailStart, AShot.EndPos - TailStart);

  // which original frame is at each frame of the shot now
  Map := NIL;
  SetLength(Map, AShot.Duration);
  for i := 0 to High(Map) do Map[i] := i;
  ApplyEdits(Map, AEdits);
  // the times rule: if the edits don't match them, the tail is cut or extended
  if Length(Map) > ADuration then
    SetLength(Map, ADuration)
  else if Length(Map) < ADuration then
  begin
    k := Length(Map);
    SetLength(Map, ADuration);
    for i := k to High(Map) do
      if k > 0 then Map[i] := -(MapOrig(Map[k-1]) + 1) else Map[i] := -1;
  end;

  // <Record>
  ReplaceValue(Head, 'Duration', 1, Length(Head), IntToStr(ADuration), 'Record');
  ReplaceValue(Head, 'In', 1, Length(Head), IntToStr(ARecordIn), 'Record');

  // <Source><In>: the source frame of the first frame of the shot
  if (AShot.SourceIn >= 0) and (Length(Map) > 0) then
  begin
    SrcIn := AShot.SourceIn + MapOrig(Map[0]);
    for i := 0 to High(Map) do
      if Map[i] >= 0 then
      begin
        SrcIn := AShot.SourceIn + Map[i] - i;
        Break;
      end;
    ReplaceValue(Head, 'In', 1, Length(Head), IntToStr(Max(0, SrcIn)), 'Source');
  end;

  // a copy of a shot (split) needs its own id
  if ANewID and (AShot.UniqueID <> '') then
    ReplaceValue(Head, 'UniqueID', 1, Length(Head), NewUniqueID);

  // <Frame>s: each frame of the shot takes the metadata of its original frame
  Frames := '';
  if AKeepFrames and (Length(AShot.Frames) > 0) then
  begin
    ByOffset := NIL;
    SetLength(ByOffset, AShot.Duration);
    for i := 0 to High(ByOffset) do ByOffset[i] := -1;
    for i := 0 to High(AShot.Frames) do
      if (AShot.Frames[i].Offset >= 0) and (AShot.Frames[i].Offset < AShot.Duration) then
        ByOffset[AShot.Frames[i].Offset] := i;

    for i := 0 to High(Map) do
    begin
      o := MapOrig(Map[i]);
      if (o < Length(ByOffset)) and (ByOffset[o] >= 0) then
      begin
        k := ByOffset[o];
        FrameText := Copy(AXML, AShot.Frames[k].StartPos, AShot.Frames[k].EndPos - AShot.Frames[k].StartPos);
        ReplaceValue(FrameText, 'EditOffset', 1, Length(FrameText), IntToStr(i));
        Frames := Frames + FrameText;
      end;
    end;
  end;

  Result := Head + Frames + Tail;
end;

// -----------------------------------------------------------------------------

function DoViBuildXML(const Subtitles: TUWSubtitles; const AFromItem, AToItem: Integer; out AXML: String): Boolean;
var
  Doc: TDoViDocument;
  Src: String;
  Num, Den, i, Shot, LastShot, RecIn, RecOut: Integer;
  Info: TDolbyVision_ExtraInfo;
  P: PDolbyVision_ExtraInfo;
  Used: array of Boolean;
  Body: TStringBuilder;
  IsSMPTE: Boolean;

  function ToFrames(const AMS: Integer): Integer;
  begin
    if IsSMPTE then // the times are of a timecode of the nominal rate
      Result := Round(AMS * NominalFPS(Num, Den) / 1000.0)
    else
      Result := DoViMSToFrames(AMS, Num, Den);
  end;

begin
  Result := False;
  AXML := '';
  Src := Subtitles.SourceText;
  if not DoViParse(Src, Doc) then Exit;
  Num := Doc.RateNum;
  Den := Doc.RateDen;
  IsSMPTE := Subtitles.TimeBase = stbSMPTE;

  Used := NIL;
  SetLength(Used, Length(Doc.Shots));
  LastShot := -1;

  Body := TStringBuilder.Create(Length(Src) + 4096);
  try
    Body.Append(Copy(Src, 1, Doc.PrefixEnd - 1));

    for i := AFromItem to AToItem do
    begin
      RecIn  := ToFrames(Subtitles.InitialTime[i]) + Subtitles.SourceOrigin;
      RecOut := ToFrames(Subtitles.FinalTime[i]) + Subtitles.SourceOrigin;
      if RecOut <= RecIn then Continue; // without frames, it's not a shot

      if Subtitles.ExtraInfoType = eiDolbyVision then
        P := PDolbyVision_ExtraInfo(Subtitles.ExtraInfo[i])
      else
        P := NIL;

      if (P <> NIL) and (P^.Shot >= 0) and (P^.Shot <= High(Doc.Shots)) then
      begin
        Info := P^;
        DoViNormalizeEdits(Info, RecIn, RecOut);
        Shot := Info.Shot;
        Body.Append(ShotText(Src, Doc.Shots[Shot], RecIn, RecOut - RecIn, Info.Edits, Used[Shot], True));
        Used[Shot] := True;
        LastShot := Shot;
      end
      else
      begin
        // a new entry: a copy of the previous shot (the next one for the first),
        // with the metadata of the shot but not the one of its frames
        if LastShot >= 0 then
          Shot := LastShot
        else
          Shot := 0;
        Body.Append(ShotText(Src, Doc.Shots[Shot], RecIn, RecOut - RecIn, '', True, False));
      end;
    end;

    Body.Append(Copy(Src, Doc.SuffixPos, MaxInt));
    AXML := Body.ToString;
    Result := True;
  finally
    Body.Free;
  end;
end;

// -----------------------------------------------------------------------------

{ Editing of the shots }

// -----------------------------------------------------------------------------

function DoViIsEditable(const Subtitles: TUWSubtitles): Boolean;
begin
  Result := (Subtitles <> NIL) and (Subtitles.SourceText <> '') and
    (Subtitles.ExtraInfoType = eiDolbyVision) and (Pos('<DolbyLabsMDF', Subtitles.SourceText) > 0);
end;

// -----------------------------------------------------------------------------

function DoViRecordFrame(const Subtitles: TUWSubtitles; const AMS: Integer): Integer;
var
  Num, Den: Integer;
begin
  // record frame (of the XML) of a time of the entries
  DoViGetRate(Subtitles, Num, Den);
  if Subtitles.TimeBase = stbSMPTE then
    Result := Round(AMS * NominalFPS(Num, Den) / 1000.0)
  else
    Result := DoViMSToFrames(AMS, Num, Den);
  Result := Result + Subtitles.SourceOrigin;
end;

// -----------------------------------------------------------------------------

function DoViRecordToMS(const Subtitles: TUWSubtitles; const AFrame: Integer): Integer;
var
  Num, Den: Integer;
begin
  DoViGetRate(Subtitles, Num, Den);
  if Subtitles.TimeBase = stbSMPTE then
    Result := Round((AFrame - Subtitles.SourceOrigin) * 1000.0 / NominalFPS(Num, Den))
  else
    Result := DoViFramesToMS(AFrame - Subtitles.SourceOrigin, Num, Den);
end;

// -----------------------------------------------------------------------------

procedure DoViEntryFrames(const Subtitles: TUWSubtitles; const AIndex: Integer; out ARecordIn, ARecordOut: Integer);
begin
  ARecordIn  := DoViRecordFrame(Subtitles, Subtitles.InitialTime[AIndex]);
  ARecordOut := DoViRecordFrame(Subtitles, Subtitles.FinalTime[AIndex]);
end;

// -----------------------------------------------------------------------------

function DoViEntryInfo(const Subtitles: TUWSubtitles; const AIndex: Integer; out AInfo: TDolbyVision_ExtraInfo): Boolean;
var
  P: PDolbyVision_ExtraInfo;
  a, b: Integer;
begin
  // its extra info, brought up to date with the times of the entry
  DoViEntryFrames(Subtitles, AIndex, a, b);
  P := NIL;
  if Subtitles.ExtraInfoType = eiDolbyVision then
    P := PDolbyVision_ExtraInfo(Subtitles.ExtraInfo[AIndex]);

  Result := P <> NIL;
  if Result then
    AInfo := P^
  else
  begin
    // an entry made in Tero: it will be a copy of the previous shot
    AInfo := Default(TDolbyVision_ExtraInfo);
    AInfo.Shot    := -1;
    AInfo.BaseIn  := a;
    AInfo.BaseOut := b;
    AInfo.Edits   := '';
  end;
  DoViNormalizeEdits(AInfo, a, b);
end;

// -----------------------------------------------------------------------------

procedure AddChange(var AChanges: TDoViChangeArray; const AIndex, ARecordIn, ARecordOut: Integer; const AInfo: TDolbyVision_ExtraInfo);
var
  n: Integer;
begin
  n := Length(AChanges);
  SetLength(AChanges, n+1);
  AChanges[n].Index     := AIndex;
  AChanges[n].RecordIn  := ARecordIn;
  AChanges[n].RecordOut := ARecordOut;
  AChanges[n].Info      := AInfo;
  AChanges[n].Info.BaseIn  := ARecordIn;
  AChanges[n].Info.BaseOut := ARecordOut;
  AChanges[n].Delete    := ARecordOut <= ARecordIn;
end;

// -----------------------------------------------------------------------------

function DoViPlanDeleteFrames(const Subtitles: TUWSubtitles; const AFrame, ACount: Integer): TDoViChangeArray;
var
  i, S, E, a, b, r, NewS: Integer;
  Info: TDolbyVision_ExtraInfo;
begin
  // the frames [AFrame, AFrame+ACount) are no longer in the video: they are
  // removed from the shots that had them and the next shots come earlier
  Result := NIL;
  if ACount <= 0 then Exit;

  for i := 0 to Subtitles.Count-1 do
  begin
    DoViEntryFrames(Subtitles, i, S, E);
    if E <= AFrame then Continue; // before, the same

    DoViEntryInfo(Subtitles, i, Info);
    if S >= AFrame + ACount then
      AddChange(Result, i, S - ACount, E - ACount, Info) // after, only moved
    else
    begin
      a := Max(S, AFrame);
      b := Min(E, AFrame + ACount);
      r := b - a;
      if S < AFrame then NewS := S else NewS := AFrame;
      Info.Edits := Info.Edits + SysUtils.Format('d%d,%d;', [a - S, r]);
      AddChange(Result, i, NewS, NewS + (E - S) - r, Info);
    end;
  end;
end;

// -----------------------------------------------------------------------------

function DoViPlanInsertFrames(const Subtitles: TUWSubtitles; const AFrame, ACount: Integer; const AExtendPrevious: Boolean): TDoViChangeArray;
var
  i, S, E, Grow: Integer;
  Info: TDolbyVision_ExtraInfo;
begin
  // ACount new frames in the video at AFrame (before the frame that was
  // there), copies of the frame before them: the shot that has them is longer
  // and the next shots come later. At a cut, the frames go to the head of the
  // shot that starts there, or with AExtendPrevious to the tail of the
  // previous one.
  Result := NIL;
  if ACount <= 0 then Exit;

  Grow := -1;
  for i := 0 to Subtitles.Count-1 do
  begin
    DoViEntryFrames(Subtitles, i, S, E);
    if (S < AFrame) and (E > AFrame) then
    begin
      Grow := i;
      Break;
    end
    else if AExtendPrevious and (E = AFrame) and (S < AFrame) then
      Grow := i
    else if not AExtendPrevious and (S = AFrame) and (Grow < 0) then
      Grow := i;
  end;

  // nothing on that side (a gap, the end): the other side
  if Grow < 0 then
    for i := 0 to Subtitles.Count-1 do
    begin
      DoViEntryFrames(Subtitles, i, S, E);
      if (S = AFrame) or (E = AFrame) then
      begin
        Grow := i;
        if S = AFrame then Break;
      end;
    end;

  for i := 0 to Subtitles.Count-1 do
  begin
    DoViEntryFrames(Subtitles, i, S, E);
    if i = Grow then
    begin
      DoViEntryInfo(Subtitles, i, Info);
      Info.Edits := Info.Edits + SysUtils.Format('i%d,%d;', [Min(Max(0, AFrame - S), E - S), ACount]);
      AddChange(Result, i, S, E + ACount, Info);
    end
    else if S >= AFrame then
    begin
      DoViEntryInfo(Subtitles, i, Info);
      AddChange(Result, i, S + ACount, E + ACount, Info);
    end;
  end;
end;

// -----------------------------------------------------------------------------

function DoViCheckCuts(const Subtitles: TUWSubtitles; const AShotChangesMS: array of Integer; const ATolerance: Integer): TDoViCutCheckArray;
var
  SC: array of Integer;
  Matched: array of Boolean;
  i, j, k, S, E, PrevE, Best, BestDiff, n, Num, Den: Integer;

  procedure Add(const AKind: TDoViCutCheckKind; const AIndex, ACut, AShot: Integer);
  begin
    n := Length(Result);
    SetLength(Result, n+1);
    Result[n].Kind      := AKind;
    Result[n].Index     := AIndex;
    Result[n].CutFrame  := ACut;
    Result[n].ShotFrame := AShot;
  end;

begin
  // the cuts of the shots against the shot changes of the video (in ms of the
  // video, from the detection), both in record frames
  Result := NIL;
  DoViGetRate(Subtitles, Num, Den);
  SC := NIL;
  SetLength(SC, Length(AShotChangesMS));
  for i := 0 to High(AShotChangesMS) do
    SC[i] := DoViRecordFrame(Subtitles, AShotChangesMS[i]);
  Matched := NIL;
  SetLength(Matched, Length(SC));

  PrevE := -1;
  j := 0;
  for i := 0 to Subtitles.Count-1 do
  begin
    DoViEntryFrames(Subtitles, i, S, E);
    // the start of the first shot and a start after a gap are not cuts
    if (i > 0) and (S = PrevE) then
    begin
      while (j < High(SC)) and (SC[j] < S - ATolerance) do Inc(j);
      Best := -1;
      BestDiff := MaxInt;
      for k := Max(0, j-1) to High(SC) do
      begin
        if SC[k] > S + ATolerance then Break;
        if (Abs(SC[k] - S) <= ATolerance) and (Abs(SC[k] - S) < BestDiff) then
        begin
          BestDiff := Abs(SC[k] - S);
          Best := k;
        end;
      end;

      if Best < 0 then
        Add(cckNoShotChange, i, S, -1)
      else
      begin
        Matched[Best] := True;
        if SC[Best] <> S then Add(cckMoved, i, S, SC[Best]);
      end;
    end;
    PrevE := E;
  end;

  // shot changes of the video without a cut in the XML (a shot that is not
  // in the metadata, or a cut that moved more than the tolerance)
  for k := 0 to High(SC) do
    if not Matched[k] then
    begin
      Add(cckNoCut, -1, SC[k], SC[k]);
      for i := 0 to Subtitles.Count-1 do
      begin
        DoViEntryFrames(Subtitles, i, S, E);
        if (SC[k] >= S) and (SC[k] < E) then
        begin
          Result[High(Result)].Index := i; // the shot where it is
          Break;
        end;
      end;
    end;
end;

// -----------------------------------------------------------------------------

function DoViCheckProblems(const Subtitles: TUWSubtitles): TDoViProblems;
var
  i, S, E, PrevE: Integer;

  procedure Mark(var ACounter: Integer);
  begin
    Inc(ACounter);
    if Result.First < 0 then Result.First := i;
  end;

begin
  // the shots of a Dolby Vision track are contiguous, without gaps
  Result := Default(TDoViProblems);
  Result.First := -1;
  PrevE := -1;
  for i := 0 to Subtitles.Count-1 do
  begin
    DoViEntryFrames(Subtitles, i, S, E);
    if E <= S then Mark(Result.Empty);
    if (Subtitles.ExtraInfoType <> eiDolbyVision) or (Subtitles.ExtraInfo[i] = NIL) then Mark(Result.NoShot);
    if (i > 0) and (S > PrevE) then Mark(Result.Gaps);
    if (i > 0) and (S < PrevE) then Mark(Result.Overlaps);
    PrevE := E;
  end;
end;

// -----------------------------------------------------------------------------

{ TUWDolbyVision }

// -----------------------------------------------------------------------------

function TUWDolbyVision.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWDolbyVision.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfDolbyVision;
end;

// -----------------------------------------------------------------------------

function TUWDolbyVision.Extension: String;
begin
  Result := '*.xml';
end;

// -----------------------------------------------------------------------------

function TUWDolbyVision.IsTimeBased: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWDolbyVision.IsFrameBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWDolbyVision.HasStyleSupport: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWDolbyVision.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  Result := Pos('<DolbyLabsMDF', SubtitleFile[Row]) > 0;
end;

// -----------------------------------------------------------------------------

function ReadFileAsIs(const AFileName: String; out AText: String): Boolean;
var
  fs: TFileStream;
begin
  Result := False;
  AText := '';
  if (AFileName = '') or not FileExists(AFileName) then Exit;
  try
    fs := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      SetLength(AText, fs.Size);
      if fs.Size > 0 then fs.ReadBuffer(AText[1], fs.Size);
      Result := True;
    finally
      fs.Free;
    end;
  except
    AText := '';
  end;
end;

// -----------------------------------------------------------------------------

function TUWDolbyVision.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  XML, Text: String;
  Doc: TDoViDocument;
  i, Origin, n: Integer;
  P: PDolbyVision_ExtraInfo;
begin
  Result := False;
  // the file as it is (bytes, line breaks, blank lines), it is written the same
  if not ReadFileAsIs(SubtitleFile.FileName, XML) or (Pos('<DolbyLabsMDF', XML) = 0) then
    XML := SubtitleFile.Text;

  if not DoViParse(XML, Doc) or (Length(Doc.Shots) = 0) then Exit;

  // the times of the entries are the record frames of the XML (the first shot
  // of 86352 is at 00:59:58:00), as the timecode of the video; the video is
  // aligned with Video > Video offset. Subtitles.SourceOrigin can move them.
  Origin := 0;

  Subtitles.ExtraInfoType := eiDolbyVision;
  Subtitles.SourceText    := XML;
  Subtitles.SourceOrigin  := Origin;
  Subtitles.FrameRate     := Doc.RateNum / Doc.RateDen;
  Subtitles.TimecodeMode  := tcNDF;

  for i := 0 to High(Doc.Shots) do
    with Doc.Shots[i] do
    begin
      New(P);
      P^.Shot    := i;
      P^.BaseIn  := RecordIn;
      P^.BaseOut := RecordIn + Duration;
      P^.Edits   := '';

      Text := 'Shot ' + IntToStr(i+1);
      n := Length(Frames);
      if n > 0 then Text := Text + SysUtils.Format(' [%d dynamic frames]', [n]);
      if SourceIn >= 0 then Text := Text + ' [source]';

      Subtitles.Add(DoViFramesToMS(RecordIn - Origin, Doc.RateNum, Doc.RateDen),
        DoViFramesToMS(RecordIn + Duration - Origin, Doc.RateNum, Doc.RateDen), Text, '', P);
    end;

  Result := Subtitles.Count > 0;
end;

// -----------------------------------------------------------------------------

function TUWDolbyVision.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  XML: String;
  fs: TFileStream;
begin
  // only from a Dolby Vision XML that was opened, the rest of the metadata
  // can't be made from subtitles
  Result := False;
  if not DoViBuildXML(Subtitles, FromItem, ToItem, XML) then Exit;

  if FileName = '' then // SaveToString
  begin
    StringList.Text := XML;
    Exit(True);
  end;

  try
    // as it was read: same encoding, BOM and line breaks
    fs := TFileStream.Create(FileName, fmCreate);
    try
      if Length(XML) > 0 then fs.WriteBuffer(XML[1], Length(XML));
      Result := True;
    finally
      fs.Free;
    end;
  except
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------

end.
