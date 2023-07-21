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
 *  Copyright (C) 2023 URUWorks, uruworks@gmail.com.
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

  TSubtitleInfoCustomList = TList<TSubtitleInfoItem>;

  TSubtitleInfoList = class(TSubtitleInfoCustomList)
  private
    FErrors    : TSubtitleErrorTypeSet;
    FSubtitles : TUWSubtitles;
  public
    constructor Create(const Subtitles: TUWSubtitles = NIL);
    destructor Destroy; override;
    property Errors: TSubtitleErrorTypeSet read FErrors write FErrors;
    procedure FixErrors(const OCRFile: String = ''; const Profile: PProfileItem = NIL; const AShotChanges: TIntegerDynArray = NIL);
  end;

  TCheckMethodType    = (cmTimes, cmTexts);
  TCheckMethodTypeSet = set of TCheckMethodType;

function CheckErrors(const Subtitles: TUWSubtitles; const Index: Integer; const ErrorsToCheck: TSubtitleErrorTypeSet; const Options: TProfileItem; const ACheckMethod: TCheckMethodTypeSet; const OCR: TUWOCRScript = NIL): TSubtitleErrorTypeSet;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, UWSubtitles.Utils, UWSystem.StrUtils, UWSubtitleAPI.Tags,
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

function IsEqualSubtitle(const Item1: TSubtitleInfoItem; const Item2: TUWSubtitleItem): Boolean;
begin
  if (Item1.InitialTime = Item2.InitialTime) and
     (Item1.FinalTime = Item2.FinalTime) and
     (Item1.Text = Item2.Text) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

constructor TSubtitleInfoList.Create(const Subtitles: TUWSubtitles = NIL);
begin
  FSubtitles := Subtitles;
  FErrors    := [];

  inherited Create;
end;

// -----------------------------------------------------------------------------

destructor TSubtitleInfoList.Destroy;
begin
  if FSubtitles <> NIL then FSubtitles := NIL;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleInfoList.FixErrors(const OCRFile: String = ''; const Profile: PProfileItem = NIL; const AShotChanges: TIntegerDynArray = NIL);

  procedure ClearItem(var AFixedItem: TSubtitleInfoItem; const Index: Integer);
  begin
    AFixedItem.Index       := Index;
    AFixedItem.Apply       := True;
    AFixedItem.InitialTime := FSubtitles[Index].InitialTime;
    AFixedItem.FinalTime   := FSubtitles[Index].FinalTime;
    AFixedItem.Text        := FSubtitles[Index].Text;
    AFixedItem.ErrorsFixed := [etNone];
  end;

var
  FixedItem : TSubtitleInfoItem;
  i, x      : Integer;
  tmp, s    : String;
  ocr       : TUWOCRScript;
  GapMs     : Integer;

  SnapMs, ShotCut,
  InCue, OutCue : Integer;
begin
  if (FSubtitles = NIL) or (Profile = NIL) or (FSubtitles.Count = 0) then Exit;

  Clear;
  GapMs  := GetCorrectTime(Profile^.MinPause, Profile^.PauseInFrames);
  SnapMs := FramesToTime(Profile^.ShotcutSnapArea, Workspace.FPS.OutputFPS);
  InCue  := FramesToTime(Profile^.ShotcutInCues, Workspace.FPS.OutputFPS);
  OutCue := FramesToTime(Profile^.ShotcutOutCues, Workspace.FPS.OutputFPS);
  ocr    := TUWOCRScript.Create(OCRFile);
  try
    for i := 0 to FSubtitles.Count-1 do
    begin
      ClearItem(FixedItem, i);
      // Repeated subtitle
      if etRepeatedSubtitle in FErrors then
      begin
        if (i > 0) and IsEqualSubtitle(FixedItem, FSubtitles[i-1]) then
        begin
          FixedItem.ErrorsFixed := [etRepeatedSubtitle];
          Add(FixedItem);
        end;
      end;

      ClearItem(FixedItem, i);
      // Unbreak if chars is less than X
      if etUnbreak in FErrors then
      begin
        s   := RemoveTSTags(FixedItem.Text);
        tmp := UnbreakSubtitlesIfLessThanChars(s, Profile^.CPL);
        if s <> tmp then
        begin
          FixedItem.Text := UnbreakSubtitlesIfLessThanChars(FixedItem.Text, Profile^.CPL);
          FixedItem.ErrorsFixed := [etUnbreak];
          Add(FixedItem);
        end;
      end;

      ClearItem(FixedItem, i);
      // Unnecessary Spaces
      if etUnnecessarySpaces in FErrors then
      begin
        tmp := RemoveUnnecessarySpaces(FixedItem.Text);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etUnnecessarySpaces];
          Add(FixedItem);
        end;
      end;

      ClearItem(FixedItem, i);
      // Unnecessary Dots
      if etUnnecessaryDots in FErrors then
      begin
        tmp := RemoveUnnecessaryDots(FixedItem.Text);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etUnnecessaryDots];
          Add(FixedItem);
        end;
      end;

      ClearItem(FixedItem, i);
      // Empty subtitle
      if etEmpty in FErrors then
      begin
        if FixedItem.Text = '' then
        begin
          FixedItem.ErrorsFixed := [etEmpty];
          Add(FixedItem);
        end;
      end;

      ClearItem(FixedItem, i);
      // Ellipses dots to single smart character
      if etEllipsesSingleSmartCharacter in FErrors then
      begin
        tmp := ReplaceString(FixedItem.Text, '...', '…');
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etEllipsesSingleSmartCharacter];
          Add(FixedItem);
        end;
      end;

      ClearItem(FixedItem, i);
      // Fix Tags
      if etFixTags in FErrors then
      begin
        tmp := FixTags(FixedItem.Text, swt_StartTag, swt_EndTag);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etFixTags];
          Add(FixedItem);
        end;
        //
        tmp := FixTags(FixedItem.Text, '<', '>');
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etFixTags];
          Add(FixedItem);
        end;
      end;

      ClearItem(FixedItem, i);
      // Prohibited chars
      if etProhibitedChars in FErrors then
      begin
        if HasProhibitedChars(FixedItem.Text, Profile^.ProhibitedChars) then
        begin
          FixedItem.ErrorsFixed := [etProhibitedChars];
          Add(FixedItem);
        end;
      end;

      ClearItem(FixedItem, i);
      // Break long lines
      if etBreakLongLines in FErrors then
      begin
        if HasTooLongLine(RemoveTSTags(FixedItem.Text), Profile^.CPL) and (Pos(sLineBreak, FixedItem.Text) = 0) then
        begin
          tmp := AutoBreakSubtitle(FixedItem.Text, Profile^.CPL, sLineBreak, False);
          if FixedItem.Text <> tmp then
          begin
            FixedItem.Text := tmp;
            FixedItem.ErrorsFixed := [etBreakLongLines];
            Add(FixedItem);
          end;
        end;
      end;

      ClearItem(FixedItem, i);
      // Hearing impaired
      if etHearingImpaired in FErrors then
      begin
        if IsHearingImpaired(FixedItem.Text) then
        begin
          tmp := FixHearingImpaired(FixedItem.Text, sLineBreak);
          if FixedItem.Text <> tmp then
          begin
            FixedItem.Text        := tmp;
            FixedItem.ErrorsFixed := [etHearingImpaired];
            Add(FixedItem);

            if tmp = '' then
            begin
              FixedItem.Text        := '';
              FixedItem.ErrorsFixed := [etEmpty];
              Add(FixedItem);
            end;
          end;
        end;
      end;

      ClearItem(FixedItem, i);
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
            Add(FixedItem);
          end;
        end;
      end;

      ClearItem(FixedItem, i);
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
            Add(FixedItem);
          end;
        end;
      end;

      ClearItem(FixedItem, i);
      // Time too short
      if etTimeTooShort in FErrors then
      begin
        if GetItemDuration(FixedItem) < Profile^.MinDuration then
        begin
          FixedItem.FinalTime   := FixedItem.InitialTime + Profile^.MinDuration;
          FixedItem.ErrorsFixed := [etTimeTooShort];
          Add(FixedItem);
        end;
      end;

      ClearItem(FixedItem, i);
      // Time too long
      if etTimeTooLong in FErrors then
      begin
        if GetItemDuration(FixedItem) > Profile^.MaxDuration then
        begin
          FixedItem.FinalTime   := FixedItem.InitialTime + Profile^.MaxDuration;
          FixedItem.ErrorsFixed := [etTimeTooLong];
          Add(FixedItem);
        end;
      end;

      ClearItem(FixedItem, i);
      // Bad Values
      if etBadValues in FErrors then
      begin
        if FixedItem.InitialTime > FixedItem.FinalTime then
        begin
          x := FixedItem.InitialTime;
          FixedItem.InitialTime := FixedItem.FinalTime;
          FixedItem.FinalTime   := x;
          FixedItem.ErrorsFixed := [etBadValues];
          Add(FixedItem);
        end;
      end;

      ClearItem(FixedItem, i);
      // Overlapping
      if etOverlapping in FErrors then
      begin
        if i > 0 then
        begin
          if FixedItem.InitialTime <= FSubtitles[i-1].FinalTime then
          begin
            FixedItem.InitialTime := FSubtitles[i-1].FinalTime + GapMs;
            FixedItem.ErrorsFixed := [etOverlapping, etOverlappingWithPrev];
            Add(FixedItem);
          end;
        end
        else if (i < (FSubtitles.Count-1)) and (FixedItem.FinalTime >= FSubtitles[i+1].InitialTime) then
        begin
          x := FSubtitles[i+1].InitialTime - 1;
          if x < (FixedItem.InitialTime + Profile^.MinDuration) then
          begin
            FixedItem.FinalTime := FSubtitles[i+1].InitialTime;
            FSubtitles.ItemPointer[i+1].InitialTime := FSubtitles[i+1].InitialTime + 1;
          end
          else
            FixedItem.FinalTime := x;

          FixedItem.ErrorsFixed := [etOverlapping, etOverlappingWithNext];
          Add(FixedItem);
        end;
      end;

      ClearItem(FixedItem, i);
      // Incomplete opening hyphen text
      if etIncompleteHyphenText in FErrors then
      begin
        tmp := FixIncompleteOpenedHyphen(FixedItem.Text);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etIncompleteHyphenText];
          Add(FixedItem);
        end;
      end;

      ClearItem(FixedItem, i);
      // Spacing of opening hyphen (Add or Remove)
      if etSpaceOfOpeningHyphen in FErrors then
      begin
        tmp := SpacingOfOpeningHyphen(FixedItem.Text, Profile^.AddHyphenSpace);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etSpaceOfOpeningHyphen];
          Add(FixedItem);
        end;
      end;

      ClearItem(FixedItem, i);
      // Remove spaces within brackets
      if etRemoveSpacesWithinBrackets in FErrors then
      begin
        tmp := RemoveSpacesWithinBrackets(FixedItem.Text);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etRemoveSpacesWithinBrackets];
          Add(FixedItem);
        end;
      end;

      ClearItem(FixedItem, i);
      // Fix interrobang
      if etFixInterrobang in FErrors then
      begin
        tmp := FixInterrobang(FixedItem.Text);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := [etFixInterrobang];
          Add(FixedItem);
        end;
      end;

      ClearItem(FixedItem, i);
      // Snap to shot changes
      if (etSnapToShotChanges in FErrors) and (AShotChanges <> NIL) and (Length(AShotChanges) > 0) then
      begin
        // In Cue
        ShotCut := FindBestShotCutMs(AShotChanges, FixedItem.InitialTime);
        if ShotCut >= 0 then
        begin
          if (Abs(ShotCut - FixedItem.InitialTime) < SnapMs) and (Subtitles.Duration[i] > Profile^.MinDuration) and
             (i-1 >= 0) and (ShotCut > Subtitles[i-1].FinalTime + Profile^.MinPause) then
          begin
            FixedItem.InitialTime := ShotCut + InCue;
            FixedItem.ErrorsFixed := [etSnapToShotChangesInCue];
          end;
        end;
        // Out Cue
        ShotCut := FindBestShotCutMs(AShotChanges, FixedItem.FinalTime);
        if ShotCut >= 0 then
        begin
          if (Abs(ShotCut - FixedItem.FinalTime) < SnapMs) and (Subtitles.Duration[i] > Profile^.MinDuration) and
             (i+1 < Subtitles.Count) and (ShotCut - OutCue < Subtitles[i+1].InitialTime - Profile^.MinPause) then
          begin
            FixedItem.FinalTime   := ShotCut - OutCue;
            FixedItem.ErrorsFixed := FixedItem.ErrorsFixed + [etSnapToShotChangesOutCue];
          end;
        end;

        if (etSnapToShotChangesInCue in FixedItem.ErrorsFixed) or (etSnapToShotChangesOutCue in FixedItem.ErrorsFixed) then
          Add(FixedItem);
      end;
    end;
  finally
    ocr.Free;
  end;
end;

// -----------------------------------------------------------------------------

{ Check errors }

// -----------------------------------------------------------------------------

function CheckErrors(const Subtitles: TUWSubtitles; const Index: Integer; const ErrorsToCheck: TSubtitleErrorTypeSet; const Options: TProfileItem; const ACheckMethod: TCheckMethodTypeSet; const OCR: TUWOCRScript = NIL): TSubtitleErrorTypeSet;
var
  s, t: String;
begin
  Result := [];
  if not Assigned(Subtitles) or (Subtitles.Count < 1) or (Index > Subtitles.Count) then Exit;

  if cmTexts in ACheckMethod then
  begin
    // Repeated subtitle
    if etRepeatedSubtitle in ErrorsToCheck then
    begin
      if (Index > 0) and Subtitles.IsEqualItem(Subtitles[Index], Subtitles[Index-1]) and (Subtitles[Index].Text <> '') and (Subtitles.Pause[Index] < Options.RepeatedTolerance) then
      begin
        Result := Result + [etRepeatedSubtitle];
      end;
    end;

    // Unbreak if chars is less than X
    if etUnbreak in ErrorsToCheck then
    begin
      t := RemoveTSTags(Subtitles[Index].Text);
      s := UnbreakSubtitlesIfLessThanChars(t, Options.CPL);
      if t <> s then
      begin
        Result := Result + [etUnbreak];
      end;
    end;

    // Unnecessary Spaces
    if etUnnecessarySpaces in ErrorsToCheck then
    begin
      s := RemoveUnnecessarySpaces(Subtitles[Index].Text);
      if Subtitles[Index].Text <> s then
      begin
        Result := Result + [etUnnecessarySpaces];
      end;
    end;

    // Unnecessary Dots
    if etUnnecessaryDots in ErrorsToCheck then
    begin
      s := RemoveUnnecessaryDots(Subtitles[Index].Text);
      if Subtitles[Index].Text <> s then
      begin
        Result := Result + [etUnnecessaryDots];
      end;
    end;

    // Empty subtitle
    if etEmpty in ErrorsToCheck then
    begin
      if Subtitles[Index].Text = '' then
      begin
        Result := Result + [etEmpty];
      end;
    end;

    // Ellipses dots to single smart character
    if etEllipsesSingleSmartCharacter in ErrorsToCheck then
    begin
      s := ReplaceString(Subtitles[Index].Text, '...', '…');
      if Subtitles[Index].Text <> s then
      begin
        Result := Result + [etEllipsesSingleSmartCharacter];
      end;
    end;

    // Fix Tags
    if etFixTags in ErrorsToCheck then
    begin
      s := FixTags(Subtitles[Index].Text, swt_StartTag, swt_EndTag);
      if Subtitles[Index].Text <> s then
      begin
        Result := Result + [etFixTags];
      end;
    end;

    // Prohibited chars
    if etProhibitedChars in ErrorsToCheck then
    begin
      if HasProhibitedChars(Subtitles[Index].Text, Options.ProhibitedChars) then
      begin
        Result := Result + [etProhibitedChars];
      end;
    end;

    // Break long lines
    if etBreakLongLines in ErrorsToCheck then
    begin
      if HasTooLongLine(RemoveTSTags(Subtitles[Index].Text), Options.CPL) then
      begin
        Result := Result + [etBreakLongLines];
      end;
    end;

    // Hearing impaired
    if etHearingImpaired in ErrorsToCheck then
    begin
      if IsHearingImpaired(Subtitles[Index].Text) then
      begin
        s := FixHearingImpaired(Subtitles[Index].Text, sLineBreak);
        if Subtitles[Index].Text <> s then
        begin
          Result := Result + [etHearingImpaired];
        end;
      end;
    end;

    // Repeated chars
    if etRepeatedChars in ErrorsToCheck then
    begin
      if HasRepeatedChar(Subtitles[Index].Text, Options.RepeatableChars) then
      begin
        s := FixRepeatedChar(Subtitles[Index].Text, Options.RepeatableChars);
        if Subtitles[Index].Text <> s then
        begin
          Result := Result + [etRepeatedChars];
        end;
      end;
    end;

    // OCR
    if (etOCR in ErrorsToCheck) and (OCR <> NIL) then
    begin
      if OCR.HasErrors(Subtitles[Index].Text) then
      begin
        s := OCR.Fix(Subtitles[Index].Text);
        if Subtitles[Index].Text <> s then
        begin
          Result := Result + [etOCR];
        end;
      end;
    end;

    // Max Lines *
    if etMaxLines in ErrorsToCheck then
    begin
      if (LineCount(Subtitles.Text[Index], sLineBreak) > Options.MaxLines) then
      begin
        Result := Result + [etMaxLines];
      end;
    end;
  end;

  if cmTimes in ACheckMethod then
  begin
    // Time too short
    if etTimeTooShort in ErrorsToCheck then
    begin
      if Subtitles.Duration[Index] < Options.MinDuration then
      begin
        Result := Result + [etTimeTooShort];
      end;
    end;

    // Time too long
    if etTimeTooLong in ErrorsToCheck then
    begin
      if Subtitles.Duration[Index] > Options.MaxDuration then
      begin
        Result := Result + [etTimeTooLong];
      end;
    end;

    // Bad Values
    if etBadValues in ErrorsToCheck then
    begin
      if Subtitles.InitialTime[Index] > Subtitles.FinalTime[Index] then
      begin
        Result := Result + [etBadValues];
      end;
    end;

    // Pause too short *
    if etPauseTooShort in ErrorsToCheck then
    begin
      if (Index >= 0) and (Index < Subtitles.Count-1) and (Subtitles.Pause[Index] < GetCorrectTime(Options.MinPause, Options.PauseInFrames)) then
      begin
        Result := Result + [etPauseTooShort];
      end;
    end;

    // too much CPS *
    if etMaxCPS in ErrorsToCheck then
    begin
      if (Subtitles.TextCPS[Index, Options.CPSLineLenStrategy] > Options.MaxCPS) then
      begin
        Result := Result + [etMaxCPS];
      end;
    end;

    // Overlapping
    if etOverlapping in ErrorsToCheck then
    begin
      if (Index > 0) and (Subtitles.InitialTime[Index] <= Subtitles.FinalTime[Index-1]) then
      begin
        Result := Result + [etOverlapping, etOverlappingWithPrev];
      end;
      if (Index < Subtitles.Count-1) and (Subtitles.FinalTime[Index] >= Subtitles.InitialTime[Index+1]) then
      begin
        Result := Result + [etOverlapping, etOverlappingWithNext];
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

end.
