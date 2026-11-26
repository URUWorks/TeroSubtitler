{*
 *  URUWorks
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

unit procFixSubtitles;

// -----------------------------------------------------------------------------

{$mode delphi}
//{$mode objfpc}{$H+}

interface

uses generics.collections, Types, UWSubtitleAPI, procConventions,
  UWSubtitles.OCR;

type

  { TSubtitleInfoItem }

  PSubtitleInfoItem = ^TSubtitleInfoItem;
  TSubtitleInfoItem = record
    Apply       : Boolean;
    Index       : Integer;
    InitialTime,
    FinalTime   : Integer;
    Text,
    Translation : String;
    ErrorsFixed : TSubtitleErrorTypeSet;
  end;

  { TUWSubtitleInfo }

  TSubtitleInfoCustomList = TList<PSubtitleInfoItem>;

  TSubtitleInfoList = class(TSubtitleInfoCustomList)
  private
    FErrors    : TSubtitleErrorTypeSet;
    FSubtitles : TUWSubtitles;
    FSource    : TSubtitleMode;
    function SourceText(const Index: Integer): String;
  public
    constructor Create(const Subtitles: TUWSubtitles = NIL);
    destructor Destroy; override;
    procedure ClearItems;
    property Errors: TSubtitleErrorTypeSet read FErrors write FErrors;
    property Source: TSubtitleMode read FSource write FSource; // text or translation
    procedure FixErrors(const OCRFile: String = ''; const Profile: PProfileItem = NIL; const AShotChanges: TIntegerDynArray = NIL);
  end;

  TCheckMethodType    = (cmTimes, cmTexts);
  TCheckMethodTypeSet = set of TCheckMethodType;

function CheckErrors(const Subtitles: TUWSubtitles; const Index: Integer; const SubtitleMode: TSubtitleMode; const ErrorsToCheck: TSubtitleErrorTypeSet; const Options: TProfileItem; const ACheckMethod: TCheckMethodTypeSet; const OCR: TUWOCRScript = NIL; const ADefault: TSubtitleErrorTypeSet = []): TSubtitleErrorTypeSet;
function NeedToCheckErrors(const Subtitles: TUWSubtitles): Boolean;
function ClearTimesErrors(const AValue: TSubtitleErrorTypeSet): TSubtitleErrorTypeSet;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, UWSubtitles.Utils, UWSystem.StrUtils, UWSubtitleAPI.Tags, Math,
  UWSystem.TimeUtils, UWSystem.SysUtils, procSubtitle;

const
  // the fixes of the times, the ones that are repeated until nothing changes
  TimeErrors: TSubtitleErrorTypeSet = [etTimeTooShort, etTimeTooLong,
    etBadValues, etOverlapping, etSnapToShotChanges, etChaining];
  SnapErrors: TSubtitleErrorTypeSet = [etSnapToShotChanges,
    etSnapToShotChangesInCue, etSnapToShotChangesOutCue,
    etSnapToShotChangesInCueAway];
  // the fixes of one pass can leave work for another one (a snap moves the
  // start that the chaining of the previous one used, a fixed duration puts
  // an end near a shot change...), so the passes are repeated with the times
  // already fixed until nothing changes. A few are enough, it's a limit only
  // in case two rules keep undoing each other
  MaxFixPasses = 10;

// -----------------------------------------------------------------------------

function FindBestShotCutMs(const AShotChanges: TIntegerDynArray; const ATimeMs: Integer): Integer;
var
  Idx, IdxForward, IdxBackward,
  DistForward, DistBackward : Integer;
begin
  Idx         := BinarySearch_IntArray(AShotChanges, ATimeMs);
  IdxForward  := Idx;
  IdxBackward := Idx - 1;

  if (IdxForward < Length(AShotChanges)) and (IdxBackward >= 0) then
  begin
    DistForward  := AShotChanges[IdxForward] - ATimeMs;
    DistBackward := ATimeMs - AShotChanges[IdxBackward];

    if (DistForward < DistBackward) then
      Result := AShotChanges[IdxForward]
    else
      Result := AShotChanges[IdxBackward];
  end
  else
    Result := -1;
end;

// -----------------------------------------------------------------------------

// the start the entry will have after the "in cue" of the shot change fix of
// Fix Subtitles (the same rules, without the check against the previous one)
function StartAfterShotCutSnap(const AStart: Integer; const AShotChanges: TIntegerDynArray;
  const ASnapMs, AThresholdMs, AInCue: Integer): Integer;
var
  Cut: Integer;
begin
  Result := AStart;
  if Length(AShotChanges) = 0 then Exit;

  Cut := FindBestShotCutMs(AShotChanges, AStart);
  if (Cut >= 0) and (Abs(Cut - AStart) < AThresholdMs) then
  begin
    if Abs(Cut - AStart) <= ASnapMs then
      Result := Cut + AInCue
    else if Sign(Cut - AStart) <= 0 then
      Result := Cut + AThresholdMs
    else
      Result := Cut - AThresholdMs;
  end;
end;

// -----------------------------------------------------------------------------

// the first shot change in [AFrom, ATo], or -1
function FirstShotCutIn(const AShotChanges: TIntegerDynArray; const AFrom, ATo: Integer): Integer;
var
  k: Integer;
begin
  Result := -1;
  for k := 0 to High(AShotChanges) do
    if AShotChanges[k] > ATo then
      Break
    else if AShotChanges[k] >= AFrom then
    begin
      Result := AShotChanges[k];
      Break;
    end;
end;

// -----------------------------------------------------------------------------

function GetItemDuration(const Item: TSubtitleInfoItem): Cardinal;
begin
  Result := Item.FinalTime-Item.InitialTime;
end;

// -----------------------------------------------------------------------------

constructor TSubtitleInfoList.Create(const Subtitles: TUWSubtitles = NIL);
begin
  FSubtitles := Subtitles;
  FErrors    := [];
  FSource    := smText;

  inherited Create;
end;

// -----------------------------------------------------------------------------

destructor TSubtitleInfoList.Destroy;
begin
  if FSubtitles <> NIL then FSubtitles := NIL;
  ClearItems;
  inherited;
end;

// -----------------------------------------------------------------------------

function TSubtitleInfoList.SourceText(const Index: Integer): String;
begin
  if FSource = smTranslation then
    Result := FSubtitles[Index].Translation
  else
    Result := FSubtitles[Index].Text;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleInfoList.ClearItems;
var
  i: Integer;
begin
  if Count > 0 then
  begin
    for i := 0 to Count-1 do
      Dispose(PSubtitleInfoItem(Items[i]));
  end;
  Clear;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleInfoList.FixErrors(const OCRFile: String = ''; const Profile: PProfileItem = NIL; const AShotChanges: TIntegerDynArray = NIL);

var
  FixedItem : TSubtitleInfoItem;
  PrevFixed : TSubtitleInfoItem;
  i, x, z   : Integer;
  MinDurMs  : Integer;
  tmp, s    : String;
  ocr       : TUWOCRScript;
  GapMs     : Integer;

  prevSnapFT,
  SnapMs, ShotCut,
  ThresholdMs,
  InCue, OutCue,
  ChainingMS : Integer;
  NextIT     : Integer;
  SnapAway : Boolean;
  Straightened : Boolean;
  SnapEnabled : Boolean;
  HalfFrameMs : Integer;
  ChainFPS    : Double;
  WIT, WFT    : array of Integer; // times as they are at the start of a pass
  WText       : array of String;
  Removed     : array of Boolean;
  NoChange    : array of Boolean;
  HasPrev     : Boolean;
  Nx          : Integer; // the next entry (that is not removed), or -1
  Errs        : TSubtitleErrorTypeSet;
  Pass, PassStart : Integer;
  TimesChanged : Boolean;
  DurMs       : Integer;
  Seen        : TDictionary<Int64, Boolean>;
  Key         : Int64;


  procedure ClearItem(var AFixedItem: TSubtitleInfoItem; const Index: Integer);
  begin
    AFixedItem.Index       := Index;
    AFixedItem.Apply       := True;
    AFixedItem.InitialTime := WIT[Index];
    AFixedItem.FinalTime   := WFT[Index];
    AFixedItem.Text        := WText[Index];
    AFixedItem.ErrorsFixed := [etNone];
  end;

  // same frame (in frames mode) or same ms: moving it changes nothing that
  // can be seen, and an item showing the same timecode was always in the list
  function SameTime(const A, B: Integer): Boolean;
  begin
    if (Workspace.WorkMode = wmFrames) and (ChainFPS > 0) then
      Result := TimeToFrames(A, ChainFPS) = TimeToFrames(B, ChainFPS)
    else
      Result := A = B;
  end;

  // a shot change other than ACut closer than the threshold to ATime
  function NearOtherShotCut(const ATime, ACut: Integer): Boolean;
  begin
    Result := (FirstShotCutIn(AShotChanges, ATime - ThresholdMs + 1, ACut - 1) >= 0) or
      (FirstShotCutIn(AShotChanges, ACut + 1, ATime + ThresholdMs - 1) >= 0);
  end;

  function TimeKind(const AErrors: TSubtitleErrorTypeSet): Integer;
  var
    e: TSubtitleErrorType;
  begin
    if AErrors * SnapErrors <> [] then
      Result := Ord(etSnapToShotChanges)
    else if etOverlappingWithNext in AErrors then
      Result := Ord(etOverlappingWithNext)
    else
    begin
      Result := -1;
      for e := Low(TSubtitleErrorType) to High(TSubtitleErrorType) do
        if e in AErrors then
        begin
          Result := Ord(e);
          Break;
        end;
    end;
  end;

  function IsTimeItem(const AErrors: TSubtitleErrorTypeSet): Boolean;
  begin
    Result := AErrors * (TimeErrors + SnapErrors + [etOverlappingWithPrev, etOverlappingWithNext]) <> [];
  end;

  function IsRemoveItem(const AErrors: TSubtitleErrorTypeSet): Boolean;
  begin
    Result := (etEmpty in AErrors) or (etProhibitedChars in AErrors) or (etRepeatedSubtitle in AErrors);
  end;

  // time +/- the overlap gap. In frames mode the gap is at least one frame
  // and is added in whole frames, so the fixed time is a visible change
  // (1 ms or a gap under a frame showed the same timecode as before)
  function OverlapGapTime(const ATime: Integer; const ASign: Integer; const AGapMs: Integer): Integer;
  var
    FPS: Double;
  begin
    FPS := Workspace.FPS.OutputFPS.FPS;
    if (Workspace.WorkMode = wmFrames) and (FPS > 0) then
      Result := FramesToTime(TimeToFrames(ATime, FPS) + ASign * Max(1, Ceil(AGapMs * FPS / 1000 - 0.001)), FPS)
    else
      Result := ATime + ASign * Max(1, AGapMs);
  end;

  // overlapping with the previous one, or a pause shorter than the min gap
  function StartTooClose(const AStart, APrevEnd, AGapMs: Integer): Boolean;
  var
    FPS: Double;
  begin
    FPS := Workspace.FPS.OutputFPS.FPS;
    if (Workspace.WorkMode = wmFrames) and (FPS > 0) then
      Result := TimeToFrames(AStart, FPS) < TimeToFrames(OverlapGapTime(APrevEnd, 1, AGapMs), FPS)
    else
      Result := AStart < OverlapGapTime(APrevEnd, 1, AGapMs);
  end;

  function NewItem(AFixedItem: TSubtitleInfoItem): PSubtitleInfoItem;
  begin
    New(Result);
    with Result^ do
    begin
      Index       := AFixedItem.Index;
      Apply       := AFixedItem.Apply;
      InitialTime := AFixedItem.InitialTime;
      FinalTime   := AFixedItem.FinalTime;
      Text        := AFixedItem.Text;
      ErrorsFixed := AFixedItem.ErrorsFixed;
    end;
  end;

begin
  if (FSubtitles = NIL) or (Profile = NIL) or (FSubtitles.Count = 0) then Exit;

  ClearItems;
  GapMs       := GetCorrectTime(Profile^.MinPause, Profile^.PauseInFrames);
  SnapMs      := FramesToTime(Profile^.ShotcutSnapArea, Workspace.FPS.OutputFPS.FPS);
  ThresholdMs := FramesToTime(Profile^.ShotcutThreshold, Workspace.FPS.OutputFPS.FPS);
  InCue       := FramesToTime(Profile^.ShotcutInCues, Workspace.FPS.OutputFPS.FPS);
  OutCue      := FramesToTime(Profile^.ShotcutOutCues, Workspace.FPS.OutputFPS.FPS);
  ChainingMS  := FramesToTime(Profile^.Chaining, Workspace.FPS.OutputFPS.FPS);
  prevSnapFT  := 0;
  SnapEnabled := (etSnapToShotChanges in FErrors) and (Length(AShotChanges) > 0);
  // half a frame: under it the difference is rounding of the ms, not a frame
  ChainFPS    := Workspace.FPS.OutputFPS.FPS;
  HalfFrameMs := Max(1, FramesToTime(1, ChainFPS) div 2);
  SetLength(WIT, FSubtitles.Count);
  SetLength(WFT, FSubtitles.Count);
  SetLength(WText, FSubtitles.Count);
  SetLength(Removed, FSubtitles.Count);
  for i := 0 to FSubtitles.Count-1 do
  begin
    WIT[i]   := FSubtitles[i].InitialTime;
    WFT[i]   := FSubtitles[i].FinalTime;
    WText[i] := SourceText(i);
    Removed[i] := False;
  end;
  ocr := TUWOCRScript.Create(OCRFile);
  try
   Pass := 0;
   repeat
    Inc(Pass);
    PassStart := Count;
    // the texts are fixed in the first pass, the next ones only the times
    if Pass = 1 then
      Errs := FErrors
    else
      Errs := FErrors * TimeErrors;
    prevSnapFT := 0;
    HasPrev    := False;

    for i := 0 to FSubtitles.Count-1 do
    begin
      // an entry that will be removed (empty...) is not there for the times
      // of its neighbours from the second pass
      if Removed[i] then Continue;
      Nx := i + 1;
      while (Nx < FSubtitles.Count) and Removed[Nx] do Inc(Nx);
      if Nx >= FSubtitles.Count then Nx := -1;
      ClearItem(FixedItem, i);

      // -- Texts

      //ClearItem(FixedItem, i);
      // Repeated subtitle
      if etRepeatedSubtitle in Errs then
      begin
        if (i > 0) and (FixedItem.InitialTime = FSubtitles[i-1].InitialTime) and
          (FixedItem.FinalTime = FSubtitles[i-1].FinalTime) and (FixedItem.Text = SourceText(i-1)) then
        begin
          FixedItem.ErrorsFixed := [etRepeatedSubtitle];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      // Unnecessary Dots
      if etUnnecessaryDots in Errs then
      begin
        tmp := RemoveUnnecessaryDots(FixedItem.Text);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etUnnecessaryDots];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      // Unnecessary Spaces
      if etUnnecessarySpaces in Errs then
      begin
        tmp := RemoveUnnecessarySpaces(FixedItem.Text);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etUnnecessarySpaces];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      // Ellipses dots to single smart character
      if etEllipsesSingleSmartCharacter in Errs then
      begin
        tmp := ReplaceString(FixedItem.Text, '...', '…');
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etEllipsesSingleSmartCharacter];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      // Repeated chars
      if etRepeatedChars in Errs then
      begin
        if HasRepeatedChar(FixedItem.Text, Profile^.RepeatableChars) then
        begin
          tmp := FixRepeatedChar(FixedItem.Text, Profile^.RepeatableChars);
          if FixedItem.Text <> tmp then
          begin
            FixedItem.Text        := tmp;
            FixedItem.ErrorsFixed := [etRepeatedChars];
            Add(NewItem(FixedItem));
          end;
        end;
      end;

      //ClearItem(FixedItem, i);
      // Prohibited chars
      if etProhibitedChars in Errs then
      begin
        if HasProhibitedChars(FixedItem.Text, Profile^.ProhibitedChars) then
        begin
          FixedItem.ErrorsFixed := [etProhibitedChars];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      // Incomplete opening hyphen text
      if etIncompleteHyphenText in Errs then
      begin
        tmp := FixIncompleteOpenedHyphen(FixedItem.Text);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etIncompleteHyphenText];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      // Spacing of opening hyphen (Add or Remove)
      if etSpaceOfOpeningHyphen in Errs then
      begin
        tmp := SpacingOfOpeningHyphen(FixedItem.Text, Profile^.AddHyphenSpace);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etSpaceOfOpeningHyphen];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      // Remove spaces within brackets
      if etRemoveSpacesWithinBrackets in Errs then
      begin
        tmp := RemoveSpacesWithinBrackets(FixedItem.Text);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etRemoveSpacesWithinBrackets];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      // Fix interrobang
      if etFixInterrobang in Errs then
      begin
        tmp := FixInterrobang(FixedItem.Text);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etFixInterrobang];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      // Unbreak if chars is less than X
      if etUnbreak in Errs then
      begin
        s   := RemoveTSTags(FixedItem.Text);
        tmp := UnbreakSubtitlesIfLessThanChars(s, GetCPL(Profile^, '')); // result is one line
        if s <> tmp then
        begin
          FixedItem.Text := UnbreakSubtitlesIfLessThanChars(FixedItem.Text, GetCPL(Profile^, ''));
          FixedItem.ErrorsFixed := [etUnbreak];
          Add(NewItem(FixedItem));
        end;
      end;

      // Hearing impaired
      if etHearingImpaired in Errs then
      begin
        if IsHearingImpaired(FixedItem.Text, AppOptions.HIRoundBrackets) then
        begin
          tmp := FixHearingImpaired(FixedItem.Text, sLineBreak, AppOptions.HIRoundBrackets);
          if FixedItem.Text <> tmp then
          begin
            FixedItem.Text        := tmp;
            FixedItem.ErrorsFixed := [etHearingImpaired];
            Add(NewItem(FixedItem));
          end;
        end;
      end;

      //ClearItem(FixedItem, i);
      // Fix Tags
      if etFixTags in Errs then
      begin
        tmp := FixTags(FixedItem.Text, tst_StartTag, tst_EndTag);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etFixTags];
          Add(NewItem(FixedItem));
        end;
        //
        tmp := FixTags(FixedItem.Text, '<', '>');
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etFixTags];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      // Cleanup Tags
      if etCleanupTags in Errs then
      begin
        tmp := CleanupTags(FixedItem.Text, [tst_Bold, tst_Italic, tst_Underline, tst_Strikeout]);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etCleanupTags];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      Straightened := False;
      // Fix brackets, ]switched[, (mismatched], empty ()
      if etFixBrackets in Errs then
      begin
        tmp := FixBrackets(FixedItem.Text);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etFixBrackets];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      // Curly quotes to straight
      if etCurlyToStraightQuotes in Errs then
      begin
        tmp := CurlyToStraightQuotes(FixedItem.Text);
        if FixedItem.Text <> tmp then
        begin
          Straightened := True;
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etCurlyToStraightQuotes];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      // Straight quotes to curly, they are opposite, with both (the estimate of
      // the dialog has all) only the first one changes the entry, so the count
      // of each one is the same as when it's the only one enabled
      if (etStraightToCurlyQuotes in Errs) and not Straightened then
      begin
        tmp := StraightToCurlyQuotes(FixedItem.Text, AppOptions.QuoteStyle);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etStraightToCurlyQuotes];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      // Break long lines
      if etBreakLongLines in Errs then
      begin
        if HasTooLongLine(RemoveTSTags(FixedItem.Text), GetCPL(Profile^, FixedItem.Text)) then //and (Pos(sLineBreak, FixedItem.Text) = 0) then
        begin
          tmp := AutoBreakSubtitle(FixedItem.Text, GetAutoBreakCPL(Profile^, FixedItem.Text));
          if FixedItem.Text <> tmp then
          begin
            FixedItem.Text := tmp;
            FixedItem.ErrorsFixed := [etBreakLongLines];
            Add(NewItem(FixedItem));
          end;
        end;
      end;

      //ClearItem(FixedItem, i);
      // OCR
      if etOCR in Errs then
      begin
        if ocr.HasErrors(FixedItem.Text) then
        begin
          tmp := ocr.Fix(FixedItem.Text);
          if FixedItem.Text <> tmp then
          begin
            FixedItem.Text        := tmp;
            FixedItem.ErrorsFixed := [etOCR];
            Add(NewItem(FixedItem));
          end;
        end;
      end;

      //ClearItem(FixedItem, i);
      // Empty subtitle
      if (etEmpty in Errs) then
      begin
        // the entry is removed, an untranslated one is not empty
        if (FixedItem.Text = '') and (FSubtitles[i].Text = '') then
        begin
          FixedItem.ErrorsFixed := [etEmpty];
          Add(NewItem(FixedItem));
        end;
      end;

      // -- Times

      //ClearItem(FixedItem, i);
      // Time too short
      if etTimeTooShort in Errs then
      begin
        // it fits before the next one with the min gap (before it was
        // compared with next start - pause, that is its own end, so only the
        // last entry was ever fixed)
        if (GetItemDuration(FixedItem) < Profile^.MinDuration) then
          if ((Nx >= 0) and (FixedItem.InitialTime + Profile^.MinDuration <= WIT[Nx] - GapMs)) or
            (Nx < 0) then
          begin
            FixedItem.FinalTime   := FixedItem.InitialTime + Profile^.MinDuration;
            FixedItem.ErrorsFixed := [etTimeTooShort];
            Add(NewItem(FixedItem));
          end;
      end;

      //ClearItem(FixedItem, i);
      // Time too long
      if etTimeTooLong in Errs then
      begin
        if GetItemDuration(FixedItem) > Profile^.MaxDuration then
        begin
          FixedItem.FinalTime   := FixedItem.InitialTime + Profile^.MaxDuration;
          FixedItem.ErrorsFixed := [etTimeTooLong];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      // Bad Values
      if etBadValues in Errs then
      begin
        if FixedItem.InitialTime > FixedItem.FinalTime then
        begin
          x := FixedItem.InitialTime;
          FixedItem.InitialTime := FixedItem.FinalTime;
          FixedItem.FinalTime   := x;
          FixedItem.ErrorsFixed := [etBadValues];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      // Overlapping
      if etOverlapping in Errs then
      begin
        if HasPrev then
        begin
          // the end of the previous one as it is fixed in this same pass (its
          // duration, shot change or chaining fix), not the one in the list:
          // otherwise the overlap only showed up the next time
          if StartTooClose(FixedItem.InitialTime, PrevFixed.FinalTime, GapMs) then
          begin
            x := OverlapGapTime(PrevFixed.FinalTime, 1, GapMs);
            MinDurMs := OverlapGapTime(0, 1, 0); // one frame (1 ms in time mode)
            // moving the start would leave this one without duration (a short
            // subtitle chained to the previous one): end the previous earlier
            if ((FixedItem.FinalTime - x) < MinDurMs) and
              ((OverlapGapTime(FixedItem.InitialTime, -1, GapMs) - PrevFixed.InitialTime) >= MinDurMs) then
            begin
              PrevFixed.FinalTime   := OverlapGapTime(FixedItem.InitialTime, -1, GapMs);
              PrevFixed.ErrorsFixed := [etOverlapping, etOverlappingWithNext];
              Add(NewItem(PrevFixed));
              // the in cue (snap) of this one is checked against the new end
              prevSnapFT := PrevFixed.FinalTime;
            end
            else
            begin
              FixedItem.InitialTime := x;
              FixedItem.ErrorsFixed := [etOverlapping, etOverlappingWithPrev];
              Add(NewItem(FixedItem));
            end;
          end;
        end;
      end;

      //ClearItem(FixedItem, i);
      // Snap to shot changes
      // The checks use where the neighbours will end up with THIS profile
      // (end = cut - OutCue, start = cut + InCue) instead of their current
      // times, so a list already snapped with another profile (e.g. Netflix)
      // is fully checked again with the new one.
      if (etSnapToShotChanges in Errs) and (AShotChanges <> NIL) and (Length(AShotChanges) > 0) then
      begin
        FixedItem.ErrorsFixed := [];
        // the duration it has now (before: the one in the list, so one made
        // longer by "time too short" was only snapped the next time)
        DurMs := FixedItem.FinalTime - FixedItem.InitialTime;
        // In Cue
        ShotCut := FindBestShotCutMs(AShotChanges, FixedItem.InitialTime);
        if (ShotCut >= 0) and (Abs(ShotCut - FixedItem.InitialTime) < ThresholdMs) and
           (DurMs >= Profile^.MinDuration) then
        begin
          SnapAway := True;
          if (Abs(ShotCut - FixedItem.InitialTime) <= SnapMs) then
          begin
            z := ShotCut + InCue;
            SnapAway := False;
          end
          else if Sign(ShotCut - FixedItem.InitialTime) <= 0 then
            z := ShotCut + ThresholdMs
          else
            z := ShotCut - ThresholdMs;

          // a shot shorter than two thresholds: away from this shot change
          // it falls in the threshold of the other one, and the next time it
          // was moved back (it never stopped). It goes to the shot change
          if SnapAway and NearOtherShotCut(z, ShotCut) then
          begin
            z := ShotCut + InCue;
            SnapAway := False;
          end;

          // prevSnapFT is the final end time of the previous subtitle. The
          // gap is checked as the overlap fix does (in frames, in frames
          // mode): in ms it could be a frame short, the overlap fix moved the
          // start and the snap moved it back again
          if (not HasPrev or not StartTooClose(z, prevSnapFT, GapMs)) and not SameTime(FixedItem.InitialTime, z) then
          begin
            FixedItem.InitialTime := z;
            if SnapAway then
              FixedItem.ErrorsFixed := [etSnapToShotChangesInCueAway]
            else
              FixedItem.ErrorsFixed := [etSnapToShotChangesInCue];
          end;
        end;

        // Out Cue
        ShotCut := FindBestShotCutMs(AShotChanges, FixedItem.FinalTime);
        if (ShotCut >= 0) and (Abs(ShotCut - FixedItem.FinalTime) < ThresholdMs) and
           (DurMs >= Profile^.MinDuration) then
        begin
          // start of the next subtitle: if it starts at this same cut it will
          // be moved to cut + InCue, so use that position
          if Nx >= 0 then
          begin
            NextIT := WIT[Nx];
            if Abs(ShotCut - NextIT) <= SnapMs then
              NextIT := ShotCut + InCue;
          end
          else
            NextIT := MaxInt div 2;

          z := FixedItem.FinalTime;
          // away from it would fall in the threshold of another shot change
          // (a short shot): to the shot change, the same as inside the snap
          if (Abs(ShotCut - FixedItem.FinalTime) <= SnapMs) or
             NearOtherShotCut(ShotCut + ThresholdMs, ShotCut) then
          begin
            if (ShotCut - OutCue) <= (NextIT - GapMs) then
              z := ShotCut - OutCue;
          end
          else if (ShotCut + ThresholdMs) <= (NextIT - GapMs) then
            z := ShotCut + ThresholdMs;

          // not shorter than the min duration or longer than the max one:
          // "time too short/long" changed it again and each pass undid the
          // other one
          if not SameTime(FixedItem.FinalTime, z) and (z > FixedItem.InitialTime) and
             ((z > FixedItem.FinalTime) or (z - FixedItem.InitialTime >= Profile^.MinDuration)) and
             ((z < FixedItem.FinalTime) or not (etTimeTooLong in Errs) or (Profile^.MaxDuration <= 0) or
              (z - FixedItem.InitialTime <= Profile^.MaxDuration)) then
          begin
            FixedItem.FinalTime   := z;
            FixedItem.ErrorsFixed := FixedItem.ErrorsFixed + [etSnapToShotChangesOutCue];
          end;
        end;
        prevSnapFT := FixedItem.FinalTime;

        if (etSnapToShotChangesInCue in FixedItem.ErrorsFixed) or (etSnapToShotChangesInCueAway in FixedItem.ErrorsFixed) or (etSnapToShotChangesOutCue in FixedItem.ErrorsFixed) then
        begin
          if (etSnapToShotChangesInCue in FixedItem.ErrorsFixed) and (etSnapToShotChangesOutCue in FixedItem.ErrorsFixed) then
            FixedItem.ErrorsFixed := [etSnapToShotChanges];

          Add(NewItem(FixedItem));
        end;
      end;

      // Chaining
      // - with the start the next one will have after its own shot change
      //   fix in this pass (before: the one in the list, so after snapping it
      //   the gap was wrong again and needed another pass)
      // - a gap of one frame more than the min gap is chained too (before it
      //   had to be more than one frame: with 2 frames of min gap, 3 stayed)
      // - with a shot change in the gap it's chained up to the shot change
      //   (cut - out cue), not across it: the snap moved it back every pass
      if (etChaining in Errs) and (Nx >= 0) then
      begin
        NextIT := WIT[Nx];
        if SnapEnabled and ((WFT[Nx] - WIT[Nx]) >= Profile^.MinDuration) then
          NextIT := StartAfterShotCutSnap(NextIT, AShotChanges, SnapMs, ThresholdMs, InCue);
        if (NextIT - FixedItem.FinalTime) < ChainingMS then
        begin
          // the min gap in whole frames (in frames mode), as the overlap fix
          // and the timecodes count it: NextIT - GapMs in ms could be a frame
          // less than the min gap
          z := OverlapGapTime(NextIT, -1, GapMs);
          if SnapEnabled then
          begin
            ShotCut := FirstShotCutIn(AShotChanges, FixedItem.FinalTime + 1, NextIT);
            if ShotCut >= 0 then
              z := Min(z, ShotCut - OutCue);
          end;

          // at least one frame later (as the timecode shows it); a few ms
          // are rounding of the times, not a gap to close
          if ChainFPS > 0 then
            x := TimeToFrames(z, ChainFPS) - TimeToFrames(FixedItem.FinalTime, ChainFPS)
          else
            x := Ord((z - FixedItem.FinalTime) >= HalfFrameMs);

          // not longer than the max duration: "time too long" cut it again
          // and each pass undid the other one
          if (etTimeTooLong in Errs) and (Profile^.MaxDuration > 0) and
             ((z - FixedItem.InitialTime) > Profile^.MaxDuration) then
            x := 0;

          if x > 0 then
          begin
            FixedItem.FinalTime := z;
            FixedItem.ErrorsFixed := [etChaining];
            Add(NewItem(FixedItem));
          end;
        end;
      end;

      // final state of this one, used to fix it from the next overlap and
      // for the in cue of the next one (chaining can change the end)
      PrevFixed  := FixedItem;
      prevSnapFT := FixedItem.FinalTime;
      HasPrev    := True;
    end;

    // the times (and texts) this pass leaves, the last item of each entry
    // has them all; the next pass starts from there
    TimesChanged := False;
    for x := PassStart to Count-1 do
      with Items[x]^ do
        if IsRemoveItem(ErrorsFixed) then
        begin
          // the neighbours are checked again without it
          if not Removed[Index] then TimesChanged := True;
          Removed[Index] := True;
        end
        else
        begin
          if (WIT[Index] <> InitialTime) or (WFT[Index] <> FinalTime) then
            TimesChanged := True;
          WIT[Index]   := InitialTime;
          WFT[Index]   := FinalTime;
          WText[Index] := Text;
        end;
   until not TimesChanged or (Errs * TimeErrors = []) or (Pass >= MaxFixPasses);
  finally
    ocr.Free;
  end;

  // an entry whose fixes end where it started (one fix undid another one):
  // nothing to apply, it was listed every time
  if Count > 0 then
  begin
    SetLength(NoChange, FSubtitles.Count);
    for i := 0 to FSubtitles.Count-1 do
      NoChange[i] := False;
    for x := 0 to Count-1 do
      with Items[x]^ do
        if not IsRemoveItem(ErrorsFixed) then
          NoChange[Index] := (InitialTime = FSubtitles[Index].InitialTime) and
            (FinalTime = FSubtitles[Index].FinalTime);
    for x := Count-1 downto 0 do
      with Items[x]^ do
        if IsTimeItem(ErrorsFixed) and not IsRemoveItem(ErrorsFixed) and NoChange[Index] and
          (Text = SourceText(Index)) then
        begin
          Dispose(PSubtitleInfoItem(Items[x]));
          Delete(x);
        end;
  end;

  // the same fix of the same entry in more than one pass: only the last one
  // is listed (it has the final times). The items are applied in order and
  // the last one of each entry is never removed, so the result is the same
  if Pass > 1 then
  begin
    Seen := TDictionary<Int64, Boolean>.Create;
    try
      for x := Count-1 downto 0 do
        if IsTimeItem(Items[x]^.ErrorsFixed) then
        begin
          Key := Int64(Items[x]^.Index) * 256 + TimeKind(Items[x]^.ErrorsFixed);
          if Seen.ContainsKey(Key) then
          begin
            Dispose(PSubtitleInfoItem(Items[x]));
            Delete(x);
          end
          else
            Seen.Add(Key, True);
        end;
    finally
      Seen.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

{ Check errors }

// -----------------------------------------------------------------------------

function CheckErrors(const Subtitles: TUWSubtitles; const Index: Integer; const SubtitleMode: TSubtitleMode; const ErrorsToCheck: TSubtitleErrorTypeSet; const Options: TProfileItem; const ACheckMethod: TCheckMethodTypeSet; const OCR: TUWOCRScript = NIL; const ADefault: TSubtitleErrorTypeSet = []): TSubtitleErrorTypeSet;
var
  Text, s, t: String;
begin
  Result := ADefault;

  if not Assigned(Subtitles) or (Index < 0) or (Index >= Subtitles.Count) then Exit;

  if cmTexts in ACheckMethod then
  begin
    if SubtitleMode = smText then
      Text := Subtitles[Index].Text
    else
      Text := Subtitles[Index].Translation;

    // Repeated subtitle
    if etRepeatedSubtitle in ErrorsToCheck then
    begin
      if (Index > 0) and Subtitles.IsEqualItem(Subtitles[Index], Subtitles[Index-1]) and (Text <> '') and (Options.RepeatedTolerance > 0) and (Subtitles.Pause[Index] < Options.RepeatedTolerance) then
      begin
        Result := Result + [etRepeatedSubtitle];
      end;
    end;

    // Unbreak if chars is less than X
    if etUnbreak in ErrorsToCheck then
    begin
      if Options.CPL > 0 then
      begin
        t := RemoveTSTags(Text);
        s := UnbreakSubtitlesIfLessThanChars(t, GetCPL(Options, '')); // result is one line
        if t <> s then
          Result := Result + [etUnbreak];
      end;
    end;

    // Unnecessary Spaces
    if etUnnecessarySpaces in ErrorsToCheck then
    begin
      s := RemoveUnnecessarySpaces(Text);
      if Text <> s then
      begin
        Result := Result + [etUnnecessarySpaces];
      end;
    end;

    // Unnecessary Dots
    if etUnnecessaryDots in ErrorsToCheck then
    begin
      s := RemoveUnnecessaryDots(Text);
      if Text <> s then
      begin
        Result := Result + [etUnnecessaryDots];
      end;
    end;

    // Empty subtitle
    if etEmpty in ErrorsToCheck then
    begin
      if Text = '' then
      begin
        Result := Result + [etEmpty];
      end;
    end;

    // Ellipses dots to single smart character
    if etEllipsesSingleSmartCharacter in ErrorsToCheck then
    begin
      s := ReplaceString(Text, '...', '…');
      if Text <> s then
      begin
        Result := Result + [etEllipsesSingleSmartCharacter];
      end;
    end;

    // Fix Tags
    if etFixTags in ErrorsToCheck then
    begin
      s := FixTags(Text, tst_StartTag, tst_EndTag);
      if Text <> s then
        Result := Result + [etFixTags];
    end;

    // Prohibited chars
    if etProhibitedChars in ErrorsToCheck then
    begin
      if (Options.ProhibitedChars <> '') and HasProhibitedChars(Text, Options.ProhibitedChars) then
        Result := Result + [etProhibitedChars];
    end;

    // Break long lines
    if etBreakLongLines in ErrorsToCheck then
    begin
      if (Options.CPL > 0) and HasTooLongLine(RemoveTSTags(Text), GetCPL(Options, Text)) then
        Result := Result + [etBreakLongLines];
    end;

    // Hearing impaired
    if etHearingImpaired in ErrorsToCheck then
    begin
      if IsHearingImpaired(Text, AppOptions.HIRoundBrackets) then
        Result := Result + [etHearingImpaired];
    end;

    // Repeated chars
    if etRepeatedChars in ErrorsToCheck then
    begin
      if (Options.RepeatableChars <> '') and HasRepeatedChar(Text, Options.RepeatableChars) then
      begin
        s := FixRepeatedChar(Text, Options.RepeatableChars);
        if Text <> s then
          Result := Result + [etRepeatedChars];
      end;
    end;

    // OCR
    if (etOCR in ErrorsToCheck) and (OCR <> NIL) then
    begin
      if OCR.HasErrors(Text) then
      begin
        s := OCR.Fix(Text);
        if Text <> s then
          Result := Result + [etOCR];
      end;
    end;

    // Max Lines *
    if etMaxLines in ErrorsToCheck then
    begin
      if (Options.MaxLines > 0) and (LineCount(Text, sLineBreak) > Options.MaxLines) then
        Result := Result + [etMaxLines];
    end;
  end;

  if cmTimes in ACheckMethod then
  begin
    // Time too short
    if etTimeTooShort in ErrorsToCheck then
    begin
      if (Options.MinDuration > 0) and (Subtitles.Duration[Index] < Options.MinDuration) then
        Result := Result + [etTimeTooShort];
    end;

    // Time too long
    if etTimeTooLong in ErrorsToCheck then
    begin
      if (Options.MaxDuration > 0) and (Subtitles.Duration[Index] > Options.MaxDuration) then
        Result := Result + [etTimeTooLong];
    end;

    // Bad Values
    if etBadValues in ErrorsToCheck then
    begin
      if Subtitles[Index].InitialTime > Subtitles[Index].FinalTime then
        Result := Result + [etBadValues];
    end;

    // Pause too short *
    if etPauseTooShort in ErrorsToCheck then
    begin
      if (Index >= 0) and (Index < Subtitles.Count-1) and (Options.MinPause > 0) and (GetCorrectPause(Index) < GetCorrectTime(Options.MinPause, Options.PauseInFrames)) then
        Result := Result + [etPauseTooShort];
    end;

    // too much CPS *
    if etMaxCPS in ErrorsToCheck then
    begin
      if (Options.MaxCPS > 0) then
        if ((SubtitleMode = smText) and (Subtitles.TextCPS[Index, Options.CPSLineLenStrategy] > Options.MaxCPS)) or
           ((SubtitleMode = smTranslation) and (Subtitles.TranslationCPS[Index, Options.CPSLineLenStrategy] > Options.MaxCPS)) then
        Result := Result + [etMaxCPS];
    end;

    // Overlapping
    if etOverlapping in ErrorsToCheck then
    begin
      if (Index > 0) and (Subtitles[Index].InitialTime <= Subtitles[Index-1].FinalTime) then
        Result := Result + [etOverlapping, etOverlappingWithPrev];

      if (Index < Subtitles.Count-1) and (Subtitles[Index].FinalTime >= Subtitles[Index+1].InitialTime) then
        Result := Result + [etOverlapping, etOverlappingWithNext];
    end;
  end;
end;

// -----------------------------------------------------------------------------

function NeedToCheckErrors(const Subtitles: TUWSubtitles): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Subtitles.Count > 0 then
    for i := 0 to Subtitles.Count-1 do
    begin
      if (Subtitles[i].ErrorType <> []) and (Subtitles[i].ErrorType <> [etHearingImpaired]) then
      begin
        Result := True;
        Break;
      end;
    end;
end;

// -----------------------------------------------------------------------------

function ClearTimesErrors(const AValue: TSubtitleErrorTypeSet): TSubtitleErrorTypeSet;
begin
  Result := AValue - [etTimeTooShort, etTimeTooLong, etBadValues, etPauseTooShort, etMaxCPS, etOverlapping];
end;

// -----------------------------------------------------------------------------

end.
