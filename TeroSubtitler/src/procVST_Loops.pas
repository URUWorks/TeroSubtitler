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

unit procVST_Loops;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, UWSubtitleAPI, procTypes;

procedure ApplyCheckErrors(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyCheckErrorsTimesOnly(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetTextFromMemo(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetTranslationFromMemo(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyText(const Item: PUWSubtitleItem; const Index: Integer; const NewSubtitleText, NewSubtitleTranslation: String; const Resent: Boolean = True);
procedure ApplyActor(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyFontBold(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyFontItalic(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyFontStrikeOut(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyFontUnderline(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyFontColor(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyFontClear(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyAddQuotationMarks(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyAlign(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyVAlign(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetTimeInitialFromSpin(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetTimeFinalFromSpin(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetTimeDurationFromSpin(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetTimePauseFromSpin(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetTimeInitialFromMPV(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetTimeFinalFromMPV(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyMoveSubtitle(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyUnbreakSubtitles(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyAutoBreakSubtitles(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyChangeFPS(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyExtendLengthToNext(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyExtendLengthToPrevious(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyAutomaticDuration(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyDefaultPause(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyShiftTimeMore(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyShiftTimeLess(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetMaximumLineLength(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyReverseText(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyFixRTLPunctuation(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyEndCueAddOneFrame(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyEndCueSubtractOneFrame(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySwapTexts(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyUnicodeChar(const Item: PUWSubtitleItem; const Index: Integer);

// -----------------------------------------------------------------------------

implementation

uses
  StrUtils, formMain, procUndo, procSubtitle, procWorkspace, procFixSubtitles,
  UWSubtitleAPI.Tags, UWSubtitles.Utils, UWSystem.TimeUtils;

// -----------------------------------------------------------------------------

procedure ApplyCheckErrors(const Item: PUWSubtitleItem; const Index: Integer);
begin
  Item^.ErrorType := CheckErrors(Subtitles, Index, AppOptions.CommonErrors, AppOptions.Conventions, [cmTexts, cmTimes]);
end;

// -----------------------------------------------------------------------------

procedure ApplyCheckErrorsTimesOnly(const Item: PUWSubtitleItem; const Index: Integer);
begin
  Item^.ErrorType := CheckErrors(Subtitles, Index, AppOptions.CommonErrors, AppOptions.Conventions, [cmTimes]);
end;

// -----------------------------------------------------------------------------

procedure ApplySetTextFromMemo(const Item: PUWSubtitleItem; const Index: Integer);
begin
  SetSubtitleText(Index, frmMain.mmoText.Text);
end;

// -----------------------------------------------------------------------------

procedure ApplySetTranslationFromMemo(const Item: PUWSubtitleItem; const Index: Integer);
begin
  SetSubtitleText(Index, frmMain.mmoTranslation.Text, smTranslation);
end;

// -----------------------------------------------------------------------------

procedure ApplyText(const Item: PUWSubtitleItem; const Index: Integer; const NewSubtitleText, NewSubtitleTranslation: String; const Resent: Boolean = True);
begin
  SetSubtitleTexts(Index, NewSubtitleText, NewSubtitleTranslation, False, False, Resent);
end;

// -----------------------------------------------------------------------------

procedure ApplyActor(const Item: PUWSubtitleItem; const Index: Integer);
begin
  UndoInstance.AddUndo(utSubtitleChange, Index, Subtitles[Index]);

  with frmMain do
    Item^.Actor := cboActor.Text;
end;

// -----------------------------------------------------------------------------

function ApplyTag(const Text: String; const Tag: Char): String;
var
  sTag, eTag: String;
begin
  if Text <> '' then
  begin
    sTag := swt_StartTag + '\' + Tag + '1' + swt_EndTag;
    eTag := swt_StartTag + '\' + Tag + '0' + swt_EndTag;

    if AnsiStartsText(sTag, Text) and AnsiEndsText(eTag, Text) then
      Result := Copy(Text, Length(sTag)+1, Length(Text) - (Length(sTag)+Length(eTag)))
    else
      Result := sTag + Text + eTag;
  end
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

function ApplyTagColor(const Text: String; const Color: String): String;
begin
  if Text <> '' then
    Result := Format('%s\%s&%s&%s%s%s\%s%s',
      [swt_StartTag, swt_Color, Color, swt_EndTag, Text, swt_StartTag, swt_Color, swt_EndTag])
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

function ApplyCustomTag(const Text: String; const Tag: String): String;
begin
  if Text <> '' then
    Result := Format('%s%s%s', [Tag, Text, Tag])
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

procedure ApplyFontBold(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, ApplyTag(Text, swt_Bold), ApplyTag(Translation, swt_Bold), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyFontItalic(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, ApplyTag(Text, swt_Italic), ApplyTag(Translation, swt_Italic), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyFontStrikeOut(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, ApplyTag(Text, swt_StrikeOut), ApplyTag(Translation, swt_StrikeOut), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyFontUnderline(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, ApplyTag(Text, swt_Underline), ApplyTag(Translation, swt_Underline), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyFontColor(const Item: PUWSubtitleItem; const Index: Integer);
var
  AColor: String;
begin
  with frmMain, SubtitleInfo do
    AColor := Format('%s%s%s', [IntToHex(LCLIntf.GetBValue(LastSubtitle.Color), 2),
               IntToHex(LCLIntf.GetGValue(LastSubtitle.Color), 2),
               IntToHex(LCLIntf.GetRValue(LastSubtitle.Color), 2)]);

  with Item^ do
    ApplyText(Item, Index, ApplyTagColor(Text, AColor), ApplyTagColor(Translation, AColor), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyFontClear(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, RemoveTSTags(Text), RemoveTSTags(Translation), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyAddQuotationMarks(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, ApplyCustomTag(Text, '"'), ApplyCustomTag(Translation, '"'), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyAlign(const Item: PUWSubtitleItem; const Index: Integer);
begin
  UndoInstance.AddUndo(utSubtitleChange, Index, Subtitles[Index]);

  with frmMain do
    if actAlignToLeft.Checked then
      Item^.Align := 1
    else if actAlignToCenter.Checked then
      Item^.Align := 2
    else if actAlignToRight.Checked then
      Item^.Align := 3
    else
      Item^.Align := 0;
end;

// -----------------------------------------------------------------------------

procedure ApplyVAlign(const Item: PUWSubtitleItem; const Index: Integer);
begin
  UndoInstance.AddUndo(utSubtitleChange, Index, Subtitles[Index]);

  with frmMain do
    if actVAlignToMiddle.Checked then
      Item^.VAlign := 1
    else if actVAlignToTop.Checked then
      Item^.VAlign := 2
    else
      Item^.VAlign := 0;
end;

// -----------------------------------------------------------------------------

procedure ApplySetTimeInitialFromSpin(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with frmMain do
    SetSubtitleTime(Index, tedInitial.Value, tedInitial.Tag);
end;

// -----------------------------------------------------------------------------

procedure ApplySetTimeFinalFromSpin(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with frmMain do
    SetSubtitleTime(Index, tedFinal.Value, tedFinal.Tag);
end;

// -----------------------------------------------------------------------------

procedure ApplySetTimeDurationFromSpin(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with frmMain do
    SetSubtitleTime(Index, tedDuration.Value, tedDuration.Tag);
end;

// -----------------------------------------------------------------------------

procedure ApplySetTimePauseFromSpin(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with frmMain do
    SetSubtitleTime(Index, tedPause.Value, tedPause.Tag);
end;

// -----------------------------------------------------------------------------

procedure ApplySetTimeInitialFromMPV(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with frmMain do
    SetSubtitleTime(Index, MPV.GetMediaPosInMs, tedInitial.Tag);
end;

// -----------------------------------------------------------------------------

procedure ApplySetTimeFinalFromMPV(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with frmMain do
    SetSubtitleTime(Index, MPV.GetMediaPosInMs, tedFinal.Tag);
end;

// -----------------------------------------------------------------------------

procedure ApplyMoveSubtitle(const Item: PUWSubtitleItem; const Index: Integer);
var
  d: Integer;
begin
  with Item^, frmMain do
  begin
    d := MPV.GetMediaPosInMs;
    SetSubtitleTime(Index, SetDelay(InitialTime, d), tedInitial.Tag, False, False);
    SetSubtitleTime(Index, SetDelay(FinalTime, d), tedFinal.Tag, False, False);
  end;
end;

// -----------------------------------------------------------------------------

procedure ApplyUnbreakSubtitles(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, UnbreakSubtitles(Text), UnbreakSubtitles(Translation), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyAutoBreakSubtitles(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^, AppOptions.Conventions do
    ApplyText(Item, Index, AutoBreakSubtitle(Text, CPL), AutoBreakSubtitle(Translation, CPL), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyChangeFPS(const Item: PUWSubtitleItem; const Index: Integer);
var
  it, ft : Integer;
begin
  UndoInstance.AddUndo(utSubtitleChange, Index, Item^, False, False);
  with Item^ do
  begin
    it          := TimeToFrames(InitialTime, Workspace.FPS.OutputFPS);
    ft          := TimeToFrames(FinalTime, Workspace.FPS.OutputFPS);
    InitialTime := FramesToTime(it, GetFPS);
    FinalTime   := FramesToTime(ft, GetFPS);
  end;
end;

// -----------------------------------------------------------------------------

procedure ApplyExtendLengthToNext(const Item: PUWSubtitleItem; const Index: Integer);
var
  NewTime: Cardinal;
begin
  if Index < (Subtitles.Count-1) then
  begin
    NewTime := Subtitles.InitialTime[Index+1];
    SetSubtitleTime(Index, ExtendLength(NewTime), frmMain.tedFinal.Tag, False, False);
  end;
end;

// -----------------------------------------------------------------------------

procedure ApplyExtendLengthToPrevious(const Item: PUWSubtitleItem; const Index: Integer);
var
  NewTime: Integer;
begin
  if Index > 0 then
    NewTime := Subtitles.FinalTime[Index-1] + GetDefPause
  else
    NewTime := Subtitles.InitialTime[Index];

  if NewTime <> Subtitles.InitialTime[Index] then
    SetSubtitleTime(Index, NewTime, frmMain.tedInitial.Tag);
end;

// -----------------------------------------------------------------------------

procedure ApplyAutomaticDuration(const Item: PUWSubtitleItem; const Index: Integer);
var
  NewTime: Integer;
begin
  with Subtitles[Index] do
    NewTime := InitialTime + AutomaticDurations(Text, Subtitles.Duration[Index], 60, 50, 50, TAutomaticDurationMode.dmAlwaysNew);

  if NewTime <> Subtitles.FinalTime[Index] then
    SetSubtitleTime(Index, NewTime, frmMain.tedFinal.Tag);
end;

// -----------------------------------------------------------------------------

procedure ApplyDefaultPause(const Item: PUWSubtitleItem; const Index: Integer);
begin
  if Index > 0 then
  begin
    if Subtitles.Pause[Index] < GetDefPause then
      SetSubtitleTime(Index, GetDefPause, frmMain.tedPause.Tag);
  end;
end;

// -----------------------------------------------------------------------------

procedure ApplyShiftTimeMore(const Item: PUWSubtitleItem; const Index: Integer);
var
  it, ft: Integer;
begin
  with Item^ do
  begin
    ShiftTime(InitialTime, FinalTime, AppOptions.ShiftTimeMS, it, ft);
    SetSubtitleTimes(Index, it, ft, False, False);
  end;
end;

// -----------------------------------------------------------------------------

procedure ApplyShiftTimeLess(const Item: PUWSubtitleItem; const Index: Integer);
var
  it, ft: Integer;
begin
  with Item^ do
  begin
    ShiftTime(InitialTime, FinalTime, -AppOptions.ShiftTimeMS, it, ft);
    SetSubtitleTimes(Index, it, ft, False, False);
  end;
end;

// -----------------------------------------------------------------------------

procedure ApplySetMaximumLineLength(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^, AppOptions.Conventions do
    ApplyText(Item, Index, SetMaximumLineLength(Text, CPL), SetMaximumLineLength(Translation, CPL), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyReverseText(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, ReverseText(Text), ReverseText(Translation), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyFixRTLPunctuation(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, FixRTLPunctuation(Text), FixRTLPunctuation(Translation), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyEndCueAddOneFrame(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    SetSubtitleTime(Index, SetEndCueOneFrame(FinalTime), frmMain.tedFinal.Tag, False, False);
end;

// -----------------------------------------------------------------------------

procedure ApplyEndCueSubtractOneFrame(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    SetSubtitleTime(Index, SetEndCueOneFrame(FinalTime, True), frmMain.tedFinal.Tag, False, False);
end;

// -----------------------------------------------------------------------------

procedure ApplySwapTexts(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, Translation, Text, False);
end;

// -----------------------------------------------------------------------------

procedure ApplyUnicodeChar(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^, AppOptions do
    ApplyText(Item, Index, LastUnicodeChar + ' ' + Text, LastUnicodeChar + ' ' + Translation, False);
end;

// -----------------------------------------------------------------------------

end.

