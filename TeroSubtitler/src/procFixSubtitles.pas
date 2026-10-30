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

  procedure ClearItem(var AFixedItem: TSubtitleInfoItem; const Index: Integer);
  begin
    AFixedItem.Index       := Index;
    AFixedItem.Apply       := True;
    AFixedItem.InitialTime := FSubtitles[Index].InitialTime;
    AFixedItem.FinalTime   := FSubtitles[Index].FinalTime;
    AFixedItem.Text        := SourceText(Index);
    AFixedItem.ErrorsFixed := [etNone];
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
  ocr := TUWOCRScript.Create(OCRFile);
  try
    for i := 0 to FSubtitles.Count-1 do
    begin
      ClearItem(FixedItem, i);

      // -- Texts

      //ClearItem(FixedItem, i);
      // Repeated subtitle
      if etRepeatedSubtitle in FErrors then
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
      if etUnnecessaryDots in FErrors then
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
      if etUnnecessarySpaces in FErrors then
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
      if etEllipsesSingleSmartCharacter in FErrors then
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
      if etRepeatedChars in FErrors then
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
      if etProhibitedChars in FErrors then
      begin
        if HasProhibitedChars(FixedItem.Text, Profile^.ProhibitedChars) then
        begin
          FixedItem.ErrorsFixed := [etProhibitedChars];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      // Incomplete opening hyphen text
      if etIncompleteHyphenText in FErrors then
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
      if etSpaceOfOpeningHyphen in FErrors then
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
      if etRemoveSpacesWithinBrackets in FErrors then
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
      if etFixInterrobang in FErrors then
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
      if etUnbreak in FErrors then
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
      if etHearingImpaired in FErrors then
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
      if etFixTags in FErrors then
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
      if etCleanupTags in FErrors then
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
      if etFixBrackets in FErrors then
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
      if etCurlyToStraightQuotes in FErrors then
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
      if (etStraightToCurlyQuotes in FErrors) and not Straightened then
      begin
        tmp := StraightToCurlyQuotes(FixedItem.Text);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etStraightToCurlyQuotes];
          Add(NewItem(FixedItem));
        end;
      end;

      //ClearItem(FixedItem, i);
      // Break long lines
      if etBreakLongLines in FErrors then
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
      if etOCR in FErrors then
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
      if (etEmpty in FErrors) then
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
      if etTimeTooShort in FErrors then
      begin
        if (GetItemDuration(FixedItem) < Profile^.MinDuration) then
          if ((i < FSubtitles.Count-1) and (FixedItem.InitialTime + Profile^.MinDuration < FSubtitles[i+1].InitialTime - GetCorrectPause(i))) or
            (i = FSubtitles.Count-1) then
          begin
            FixedItem.FinalTime   := FixedItem.InitialTime + Profile^.MinDuration;
            FixedItem.ErrorsFixed := [etTimeTooShort];
            Add(NewItem(FixedItem));
          end;
      end;

      //ClearItem(FixedItem, i);
      // Time too long
      if etTimeTooLong in FErrors then
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
      if etBadValues in FErrors then
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
      if etOverlapping in FErrors then
      begin
        if i > 0 then
        begin
          if StartTooClose(FixedItem.InitialTime, FSubtitles[i-1].FinalTime, GapMs) then
          begin
            x := OverlapGapTime(FSubtitles[i-1].FinalTime, 1, GapMs);
            MinDurMs := OverlapGapTime(0, 1, 0); // one frame (1 ms in time mode)
            // moving the start would leave this one without duration (a short
            // subtitle chained to the previous one): end the previous earlier
            if ((FixedItem.FinalTime - x) < MinDurMs) and
              ((OverlapGapTime(FixedItem.InitialTime, -1, GapMs) - PrevFixed.InitialTime) >= MinDurMs) then
            begin
              PrevFixed.FinalTime   := OverlapGapTime(FixedItem.InitialTime, -1, GapMs);
              PrevFixed.ErrorsFixed := [etOverlapping, etOverlappingWithNext];
              Add(NewItem(PrevFixed));
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
      if (etSnapToShotChanges in FErrors) and (AShotChanges <> NIL) and (Length(AShotChanges) > 0) then
      begin
        FixedItem.ErrorsFixed := [];
        // In Cue
        ShotCut := FindBestShotCutMs(AShotChanges, FixedItem.InitialTime);
        if (ShotCut >= 0) and (Abs(ShotCut - FixedItem.InitialTime) < ThresholdMs) and
           (FSubtitles.Duration[i] >= Profile^.MinDuration) then
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

          // prevSnapFT is the final end time of the previous subtitle
          if ((i = 0) or (z >= prevSnapFT + GapMs)) and (FixedItem.InitialTime <> z) then
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
           (FSubtitles.Duration[i] >= Profile^.MinDuration) then
        begin
          // start of the next subtitle: if it starts at this same cut it will
          // be moved to cut + InCue, so use that position
          if i+1 < FSubtitles.Count then
          begin
            NextIT := FSubtitles[i+1].InitialTime;
            if Abs(ShotCut - NextIT) <= SnapMs then
              NextIT := ShotCut + InCue;
          end
          else
            NextIT := MaxInt div 2;

          z := FixedItem.FinalTime;
          if (Abs(ShotCut - FixedItem.FinalTime) <= SnapMs) then
          begin
            if (ShotCut - OutCue) <= (NextIT - GapMs) then
              z := ShotCut - OutCue;
          end
          else if (ShotCut + ThresholdMs) <= (NextIT - GapMs) then
            z := ShotCut + ThresholdMs;

          if (FixedItem.FinalTime <> z) and (z > FixedItem.InitialTime) then
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
      if (etChaining in FErrors) and (i+1 < FSubtitles.Count) then
      begin
        if ((FSubtitles[i+1].InitialTime - FixedItem.FinalTime) < ChainingMS) and (((FSubtitles[i+1].InitialTime - GapMs) - FixedItem.FinalTime) > FramesToTime(1, Workspace.FPS.OutputFPS.FPS)) then
        begin
          FixedItem.FinalTime := FSubtitles[i+1].InitialTime - GapMs;
          FixedItem.ErrorsFixed := [etChaining];
          Add(NewItem(FixedItem));
        end;
      end;

      // final state of this one, used to fix it from the next overlap
      PrevFixed := FixedItem;
    end;
  finally
    ocr.Free;
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

  if not Assigned(Subtitles) or (Subtitles.Count < 1) or (Index > Subtitles.Count) then Exit;

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
