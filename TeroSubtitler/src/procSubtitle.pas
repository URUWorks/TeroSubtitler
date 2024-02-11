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

unit procSubtitle;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils, Graphics, LazUTF8, procTypes, laz.VirtualTrees,
  UWSubtitleAPI, UWMemo, Types;

type
  TPushWordMode = (pwmUp, pwmDown, pwmToPrevious, pwmToNext);

procedure CopyToClipboard(const ACut: Boolean = False);
procedure PasteFromClipboard;
procedure CopyCurrentVideoPosToClipboard;

function InsertSubtitle(const Index: Integer; const Item: TUWSubtitleItem; const AutoIncrementUndo: Boolean = True; const AUpdate: Boolean = True): Integer; overload;
function InsertSubtitle(const Index: Integer; const InitialTime, FinalTime: Integer; const Text, Translation: String; const AutoIncrementUndo: Boolean = True; const AUpdate: Boolean = True): Integer; overload;
procedure DeleteSubtitle(const Index: Integer; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);
function DeleteLineFromText(const Index, ALine: Integer): String;
procedure PushFirstLineToPreviousEntry(const Index: Integer);
procedure PushLastLineToNextEntry(const Index: Integer);
procedure PullLastLineFromPreviousEntry(const Index: Integer);
procedure PullFirstLineFromNextEntry(const Index: Integer);
procedure PushWord(const Index: Integer; const ALine: Integer; const AMode: TPushWordMode);
procedure MergeWithNext(const Index: Integer; const APrevious: Boolean = False);
procedure ClearSubtitles(const AutoIncrementUndo: Boolean = True);
function SetEndCueOneFrame(const AFinalTime: Integer; const ASubtract: Boolean = False): Integer;

function CalcNewSubtitleFinalTime(const Index: Integer; const AInitialTime: Integer): Integer;

procedure SubtitleChanged(const AText, ATranslation: Boolean);
procedure SubtitleChangedReset(const ASubtitleMode: TSubtitleMode); overload;
procedure SubtitleChangedReset; overload;
procedure SetSubtitleTimes(const Index: Integer; const AInitialTime, AFinalTime: Integer; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);
procedure SetSubtitleTime(const Index: Integer; const Time: Integer; const Tag: Integer; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);
procedure SetSubtitleText(const Index: Integer; const Text: String; const SubtitleMode: TSubtitleMode = smText; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True; const Resent: Boolean = True);
procedure SetSubtitleTexts(const Index: Integer; const Text: String; const Translation: String; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True; const Resent: Boolean = True);
procedure SetSubtitleValues(const Index: Integer; const AInitialTime, AFinalTime: Integer; const Text: String; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);

function GetCorrectTime(const ATime: Integer; AIsFrames: Boolean): Integer;
function GetCorrectPause(const AIndex: Integer): Integer;
function GetTimeStr(const Time: Integer; const Trim: Boolean = False): String;
function GetInitialTimeStr(const Index: Integer; const Trim: Boolean = False): String;
function GetFinalTimeStr(const Index: Integer; const Trim: Boolean = False): String;
function GetDurationTimeStr(const Index: Integer; const Trim: Boolean = False): String;
function GetPauseTimeStr(const Index: Integer; const Trim: Boolean = False): String;
function GetSubtitleText(const Index: Integer; const SubtitleMode: TSubtitleMode = smText): String;

function GetMaxLinesOf(Text: String; const Separator: String = sLineBreak): Integer;
function GetLengthForEachLine(Text: String; const Separator: String = sLineBreak; const LastSeparator: String = sLineBreak): String;
function GetLengthForEachLineIntArray(Text: String; const Separator: String = sLineBreak; const LastSeparator: String = sLineBreak): TIntegerDynArray;
function GetTextLength(const Text: String): Integer;
procedure SelectSubtitleAndFocusMemo(const NextSibiling: Boolean; const WaveToo: Boolean = False);
procedure GoToNextEntryAndPlay(const NextSibiling: Boolean = True);
procedure GoToCurrentEntryTime(const AInitialTime: Boolean = True);
procedure PlayCurrentEntry;
function GetSubtitleIndexAtTime(MSecs: Cardinal): Integer;
function GetSubtitleTextAtTime(const MSecs: Cardinal): String;

procedure InsertMemoText(const AText: String; const AVSTLoop: Boolean = True);

procedure SetTextTag(const Tag: String);
procedure SetTextCustomTag(const Tag: String);
procedure SetTextTagColor(const HexColor: String);

procedure SetAlignTo(const AAlign: TSubtitleHAlign);
procedure SetVAlignTo(const AVAlign: TSubtitleVAlign);

function GetSubtitleMarkedCount: Integer;

// -----------------------------------------------------------------------------

implementation

uses
  formMain, procVST, procUndo, procVST_Loops, procWorkspace, procFixSubtitles,
  UWSystem.Encoding, UWSystem.TimeUtils, UWSystem.StrUtils, procMPV,
  UWSubtitleAPI.Tags, UWSubtitleAPI.Formats, procTranscription,
  UWSubtitles.Utils, Clipbrd, UWSystem.SysUtils;

// -----------------------------------------------------------------------------


procedure CopyToClipboard(const ACut: Boolean = False);
var
  Memo: TUWMemo;
begin
  if frmMain.VST.Focused then
    VSTCopyEntriesToClipboard(frmMain.VST, ACut)
  else
  begin
    Memo := GetMemoFocused;
    if Memo <> NIL then
    begin
      if ACut then
        Memo.CutToClipboard
      else
        Memo.CopyToClipboard;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure PasteFromClipboard;
var
  Memo: TUWMemo;
begin
  if frmMain.VST.Focused then
    VSTPasteEntriesFromClipboard(frmMain.VST)
  else
  begin
    Memo := GetMemoFocused;
    if Memo <> NIL then
      Memo.PasteFromClipboard;
  end;
end;

// -----------------------------------------------------------------------------

procedure CopyCurrentVideoPosToClipboard;
begin
  with frmMain.MPV do
    if IsMediaLoaded then
      Clipboard.AsText := GetTimeStr(GetMediaPosInMs);
end;

// -----------------------------------------------------------------------------

function InsertSubtitle(const Index: Integer; const Item: TUWSubtitleItem; const AutoIncrementUndo: Boolean = True; const AUpdate: Boolean = True): Integer;
begin
  Result := Index;
  if Subtitles.ValidIndex(Result) then
    Subtitles.Insert(Result, Item, NIL)
  else
    Result := Subtitles.Add(Item, NIL);

  UndoInstance.AddUndo(utInsertLine, Result, Item, AutoIncrementUndo);

  frmMain.VST.RootNodeCount := Subtitles.Count;
  if AUpdate then
  begin
    UpdateValues(True);
    SubtitleChanged(True, True);
  end;
end;

// -----------------------------------------------------------------------------

function InsertSubtitle(const Index: Integer; const InitialTime, FinalTime: Integer; const Text, Translation: String; const AutoIncrementUndo: Boolean = True; const AUpdate: Boolean = True): Integer;
var
  Item: TUWSubtitleItem;
  it, ft: Integer;
begin
  ClearSubtitleItem(Item);
  Item.InitialTime := InitialTime;
  Item.FinalTime   := FinalTime;
  Item.Text        := Text;
  Item.Translation := Translation;

  if Workspace.WorkMode = wmFrames then
  begin
    RoundFramesValue(InitialTime, FinalTime, Workspace.FPS.OutputFPS, it, ft);
    Item.InitialTime := it;
    Item.FinalTime   := ft;
  end;

  Result := InsertSubtitle(Index, Item, AutoIncrementUndo, AUpdate);
end;

// -----------------------------------------------------------------------------

procedure DeleteSubtitle(const Index: Integer; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);
begin
  if not Subtitles.ValidIndex(Index) then Exit;
  UndoInstance.AddUndo(utDeleteLine, Index, Subtitles[Index], AutoIncrementUndo);

  Subtitles.Delete(Index);
  frmMain.VST.RootNodeCount := Subtitles.Count;
  if AUpdate then
  begin
    UpdateValues(True);
    SubtitleChanged(True, True);
  end;
end;

// -----------------------------------------------------------------------------

function DeleteLineFromText(const Index, ALine: Integer): String;
var
  sl : TStrings;
begin
  Result := '';
  if not Subtitles.ValidIndex(Index) then Exit;

  sl := TStringList.Create;
  try
    sl.SkipLastLineBreak := True;
    Result := Subtitles.Text[Index];
    sl.Text := Result;
    if (ALine >= 0) and (ALine < sl.Count) then
    begin
      sl.Delete(ALine);
      Result := sl.Text;
    end;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure PushFirstLineToPreviousEntry(const Index: Integer);
var
  sl : TStrings;
begin
  if not Subtitles.ValidIndex(Index) or (Index < 1) then Exit;
  if Subtitles[Index].Text.IsEmpty then Exit;

  sl := TStringList.Create;
  try
    sl.SkipLastLineBreak := True;
    sl.Text := Subtitles[Index].Text;

    SetSubtitleText(Index-1, Subtitles[Index-1].Text + sLineBreak + sl[0], smText, False, False, False);
    sl.Delete(0);
    SetSubtitleText(Index, sl.Text, smText, False, False, False);

    UndoInstance.IncrementUndoGroup;
    SubtitleChanged(True, False);
    UpdateValues(True);
    DoAutoCheckErrors;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure PushLastLineToNextEntry(const Index: Integer);
var
  sl : TStrings;
begin
  if not Subtitles.ValidIndex(Index) or (Index = Subtitles.Count-1) then Exit;
  if Subtitles[Index].Text.IsEmpty then Exit;

  sl := TStringList.Create;
  try
    sl.SkipLastLineBreak := True;
    sl.Text := Subtitles[Index].Text;

    SetSubtitleText(Index+1, sl[sl.Count-1] + sLineBreak + Subtitles[Index+1].Text, smText, False, False, False);
    sl.Delete(sl.Count-1);
    SetSubtitleText(Index, sl.Text, smText, False, False, False);

    UndoInstance.IncrementUndoGroup;
    SubtitleChanged(True, False);
    UpdateValues(True);
    DoAutoCheckErrors;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure PullLastLineFromPreviousEntry(const Index: Integer);
var
  sl : TStrings;
begin
  if not Subtitles.ValidIndex(Index) or (Index < 1) then Exit;
  if Subtitles[Index-1].Text.IsEmpty then Exit;

  sl := TStringList.Create;
  try
    sl.SkipLastLineBreak := True;
    sl.Text := Subtitles[Index-1].Text;

    SetSubtitleText(Index, sl[sl.Count-1] + sLineBreak + Subtitles[Index].Text, smText, False, False, False);
    sl.Delete(sl.Count-1);
    SetSubtitleText(Index-1, sl.Text, smText, False, False, False);

    UndoInstance.IncrementUndoGroup;
    SubtitleChanged(True, False);
    UpdateValues(True);
    DoAutoCheckErrors;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure PullFirstLineFromNextEntry(const Index: Integer);
var
  sl : TStrings;
begin
  if not Subtitles.ValidIndex(Index) or (Index = Subtitles.Count-1) then Exit;
  if Subtitles[Index+1].Text.IsEmpty then Exit;

  sl := TStringList.Create;
  try
    sl.SkipLastLineBreak := True;
    sl.Text := Subtitles[Index+1].Text;

    SetSubtitleText(Index, Subtitles[Index].Text + sLineBreak + sl[0], smText, False, False, False);
    sl.Delete(0);
    SetSubtitleText(Index+1, sl.Text, smText, False, False, False);

    UndoInstance.IncrementUndoGroup;
    SubtitleChanged(True, False);
    UpdateValues(True);
    DoAutoCheckErrors;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure PushWord(const Index: Integer; const ALine: Integer; const AMode: TPushWordMode);
var
  sl : TStrings;
  words : TStringArray;
  i, ti, tf, pause : Integer;
begin
  if not Subtitles.ValidIndex(Index) or (ALine < 0) then Exit;
  if Subtitles[Index].Text.IsEmpty then Exit;

  sl := TStringList.Create;
  try
    sl.SkipLastLineBreak := True;
    sl.Text := Subtitles[Index].Text;

    if (AMode = pwmUp) or (AMode = pwmDown) then
      words := sl[ALine].Split(' ')
    else if (AMode = pwmToPrevious) then
      words := sl[0].Split(' ')
    else
      words := sl[sl.Count-1].Split(' ');

    if Length(words) > 0 then
    begin
      pause := GetCorrectTime(AppOptions.Conventions.MinPause, AppOptions.Conventions.PauseInFrames);

      case AMode of
        pwmUp,
        pwmToPrevious : begin
                          if AMode = pwmUp then
                          begin
                            sl[ALine] := Copy(sl[ALine], words[0].Length + 2);

                            if ALine > 0 then
                              sl[ALine-1] := sl[ALine-1] + ' ' + words[0]
                            else
                              sl.Insert(0, words[0]);

                            SetSubtitleText(Index, sl.Text, smText, False, False, False);
                          end
                          else
                          begin
                            sl[0] := Copy(sl[0], words[0].Length + 2);

                            if Index > 0 then
                            begin
                              SetSubtitleText(Index-1, Subtitles[Index-1].Text + ' ' + words[0], smText, False, False, False);
                              SetSubtitleText(Index, sl.Text, smText, False, False, False);
                            end
                            else
                            begin
                              ti  := Range(Subtitles[Index].InitialTime - AppOptions.Conventions.NewSubtitleMs - Pause, 0, Subtitles[Index].InitialTime);
                              tf  := ti + AppOptions.Conventions.NewSubtitleMs;

                              SetSubtitleText(Index, sl.Text, smText, False, False, False);
                              InsertSubtitle(Index, ti, tf, words[0], '', False, False);
                            end;
                          end;
                        end;

        pwmDown,
        pwmToNext : begin
                      i := Length(words)-1;
                      sl[ALine] := Copy(sl[ALine], 1, sl[ALine].Length - (words[i].Length + 1));

                      if AMode = pwmDown then
                      begin
                        if ALine < sl.Count-1 then
                          sl[ALine+1] := words[i] + ' ' + sl[ALine+1]
                        else
                          sl.Add(words[i]);

                        SetSubtitleText(Index, sl.Text, smText, False, False, False);
                      end
                      else
                      begin
                        if Index < Subtitles.Count-1 then
                        begin
                          SetSubtitleText(Index+1, words[i] + ' ' + Subtitles[Index+1].Text, smText, False, False, False);
                          SetSubtitleText(Index, sl.Text, smText, False, False, False);
                        end
                        else
                        begin
                          ti  := Subtitles[Index].FinalTime + Pause;
                          tf  := ti + AppOptions.Conventions.NewSubtitleMs;

                          SetSubtitleText(Index, sl.Text, smText, False, False, False);
                          InsertSubtitle(Index+1, ti, tf, words[i], '', False, False);
                        end;
                      end;
                    end;
      end;

      UndoInstance.IncrementUndoGroup;
      SubtitleChanged(True, False);
      UpdateValues(True);
      DoAutoCheckErrors;
    end;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure MergeWithNext(const Index: Integer; const APrevious: Boolean = False);
begin
  if not Subtitles.ValidIndex(Index) or
    (APrevious and (Index <= 0)) or
    (not APrevious and (Index >= Subtitles.Count-1)) then
    Exit;

  if APrevious then
  begin
    SetSubtitleTexts(Index-1, Subtitles.Text[Index-1] + sLineBreak + Subtitles.Text[Index], Subtitles.Translation[Index-1] + sLineBreak + Subtitles.Translation[Index], False, False, False);
    SetSubtitleTime(Index-1, Subtitles[Index].FinalTime, TAG_CONTROL_FINALTIME, False, False);
  end
  else
  begin
    SetSubtitleTexts(Index+1, Subtitles[Index].Text + sLineBreak + Subtitles.Text[Index+1], Subtitles[Index].Translation + sLineBreak + Subtitles.Translation[Index+1], False, False, False);
    SetSubtitleTime(Index+1, Subtitles[Index].InitialTime, TAG_CONTROL_INITIALTIME, False, False);
  end;
  DeleteSubtitle(Index, False, False);

  UndoInstance.IncrementUndoGroup;
  SubtitleChanged(True, True);
  UpdateValues(True);
  DoAutoCheckErrors;
end;

// -----------------------------------------------------------------------------

function SetEndCueOneFrame(const AFinalTime: Integer; const ASubtract: Boolean = False): Integer;
begin
  if not ASubtract then
    Result := FramesToTime(TimeToFrames(AFinalTime, Workspace.FPS.OutputFPS)+1, Workspace.FPS.OutputFPS)
  else
    Result := FramesToTime(TimeToFrames(AFinalTime, Workspace.FPS.OutputFPS)-1, Workspace.FPS.OutputFPS);
end;

// -----------------------------------------------------------------------------

procedure ClearSubtitles(const AutoIncrementUndo: Boolean = True);
var
  i: Integer;
begin
  if Subtitles.Count = 0 then Exit;

  for i := Subtitles.Count-1 downto 0 do
  begin
    UndoInstance.AddUndo(utDeleteLine, i, Subtitles[i], False);
    Subtitles.Delete(i);
  end;

  if AutoIncrementUndo then
    UndoInstance.IncrementUndoGroup;

  frmMain.VST.RootNodeCount := Subtitles.Count;

  UpdateValues(True);
  SubtitleChanged(True, True);
end;

// -----------------------------------------------------------------------------

function CalcNewSubtitleFinalTime(const Index: Integer; const AInitialTime: Integer): Integer;
begin
  Result := AInitialTime + AppOptions.Conventions.MinDuration;
  if (Index >= 0) and (Index < Subtitles.Count) then
  begin
    if Result >= Subtitles[Index+1].InitialTime then
      Result := Subtitles[Index+1].InitialTime - GetDefPause;
  end;
end;

// -----------------------------------------------------------------------------

procedure SubtitleChanged(const AText, ATranslation: Boolean);
begin
  with SubtitleInfo do
  begin
    if AText        then Text.Changed        := True;
    if ATranslation then Translation.Changed := True;
  end;

  if MPVOptions.SubtitleHandleByMPV then
  begin
    if MPVSaveSubtitleTempTrack then
      MPVReloadSubtitleTempTrack;
  end;
end;

// -----------------------------------------------------------------------------

procedure SubtitleChangedReset(const ASubtitleMode: TSubtitleMode);
begin
  if ASubtitleMode = smText then
    SubtitleInfo.Text.Changed := False
  else
    SubtitleInfo.Translation.Changed := False;
end;

// -----------------------------------------------------------------------------

procedure SubtitleChangedReset;
begin
  SubtitleInfo.Text.Changed := False;
  SubtitleInfo.Translation.Changed := False;
end;

// -----------------------------------------------------------------------------

procedure SetSubtitleTimes(const Index: Integer; const AInitialTime, AFinalTime: Integer; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);
begin
  if (Subtitles.InitialTime[Index] = AInitialTime) and (Subtitles.FinalTime[Index] = AFinalTime) then
    Exit;

  UndoInstance.AddUndo(utSubtitleChange, Index, Subtitles[Index], AutoIncrementUndo);

  Subtitles.InitialTime[Index] := AInitialTime;
  Subtitles.FinalTime[Index]   := AFinalTime;

  if AUpdate then
  begin
    SubtitleChanged(True, True);
    if AppOptions.AutoCheckErrors then Subtitles.ItemPointer[Index]^.ErrorType := CheckErrors(Subtitles, Index, smText, AppOptions.CommonErrors - [etOCR], AppOptions.Conventions, [cmTimes]);
    UpdateCPSAndTexts(Index);
    if frmMain.VST.SelectedCount = 1 then frmMain.VST.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetSubtitleTime(const Index: Integer; const Time: Integer; const Tag: Integer; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);
begin
  UndoInstance.AddUndo(utSubtitleChange, Index, Subtitles[Index], AutoIncrementUndo);

  case Tag of
    TAG_CONTROL_INITIALTIME : Subtitles.InitialTime[Index] := Time;
    TAG_CONTROL_FINALTIME   : begin
                                Subtitles.FinalTime[Index] := Time;
                                if AppOptions.AutoCheckErrors and (Subtitles.ValidIndex(Index+1)) then
                                  CheckErrors(Subtitles, Index+1, smText, AppOptions.CommonErrors - [etOCR], AppOptions.Conventions, [cmTimes]);
                              end;
    TAG_CONTROL_DURATION    : Subtitles.Duration[Index] := Time;
    TAG_CONTROL_PAUSE       : Subtitles.Pause[Index]    := Time;
  end;

  case Tag of
    TAG_CONTROL_INITIALTIME, TAG_CONTROL_FINALTIME:
    begin
      frmMain.tedDuration.SetValueOnly(Subtitles.Duration[Index]);
      frmMain.tedPause.SetValueOnly(Subtitles.Pause[Index]);
    end;
  end;

  if AUpdate then
  begin
    SubtitleChanged(True, True);
    if AppOptions.AutoCheckErrors then Subtitles.ItemPointer[Index]^.ErrorType := CheckErrors(Subtitles, Index, smText, AppOptions.CommonErrors - [etOCR], AppOptions.Conventions, [cmTimes]);
    UpdateCPSAndTexts(Index);
    if frmMain.VST.SelectedCount = 1 then frmMain.VST.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetSubtitleText(const Index: Integer; const Text: String; const SubtitleMode: TSubtitleMode = smText; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True; const Resent: Boolean = True);
begin
  if ((SubtitleMode = smText) and (Subtitles.Text[Index] = Text)) or
    ((SubtitleMode = smTranslation) and (Subtitles.Translation[Index] = Text)) then
    Exit;

  if Resent then
    UndoInstance.AddUndoIfNotResent(utSubtitleChange, Index, Subtitles[Index], AutoIncrementUndo)
  else
    UndoInstance.AddUndo(utSubtitleChange, Index, Subtitles[Index], AutoIncrementUndo);

  if SubtitleMode = smText then
    Subtitles.Text[Index] := Text
  else
    Subtitles.Translation[Index] := Text;

  if AUpdate then
  begin
    SubtitleChanged(SubtitleMode = smText, SubtitleMode = smTranslation);
    if AppOptions.AutoCheckErrors then Subtitles.ItemPointer[Index]^.ErrorType := CheckErrors(Subtitles, Index, SubtitleMode, AppOptions.CommonErrors - [etOCR], AppOptions.Conventions, [cmTexts]);
    UpdateCPSAndTexts(Index);
    frmMain.VST.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetSubtitleTexts(const Index: Integer; const Text: String; const Translation: String; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True; const Resent: Boolean = True);
begin
  if (Subtitles.Text[Index] = Text) and (Subtitles.Translation[Index] = Translation) then
    Exit;

  if Resent then
    UndoInstance.AddUndoIfNotResent(utSubtitleChange, Index, Subtitles[Index], AutoIncrementUndo)
  else
    UndoInstance.AddUndo(utSubtitleChange, Index, Subtitles[Index], AutoIncrementUndo);

  Subtitles.Text[Index]        := Text;
  Subtitles.Translation[Index] := Translation;

  if AUpdate then
  begin
    SubtitleChanged(True, True);
    if AppOptions.AutoCheckErrors then
    begin
      Subtitles.ItemPointer[Index]^.ErrorType := CheckErrors(Subtitles, Index, smText, AppOptions.CommonErrors - [etOCR], AppOptions.Conventions, [cmTexts]);
      if Workspace.TranslatorMode then
        Subtitles.ItemPointer[Index]^.ErrorType += CheckErrors(Subtitles, Index, smTranslation, AppOptions.CommonErrors - [etOCR], AppOptions.Conventions, [cmTexts]);
    end;
    UpdateCPSAndTexts(Index);
    frmMain.VST.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetSubtitleValues(const Index: Integer; const AInitialTime, AFinalTime: Integer; const Text: String; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);
begin
  if (Subtitles.InitialTime[Index] = AInitialTime) and
     (Subtitles.FinalTime[Index] = AFinalTime) and
     (Subtitles.Text[Index] = Text) then
    Exit;

  UndoInstance.AddUndo(utSubtitleChange, Index, Subtitles[Index], AutoIncrementUndo);

  Subtitles.InitialTime[Index] := AInitialTime;
  Subtitles.FinalTime[Index]   := AFinalTime;
  Subtitles.Text[Index]        := Text;

  if AUpdate then
  begin
    if AppOptions.AutoCheckErrors then Subtitles.ItemPointer[Index]^.ErrorType := CheckErrors(Subtitles, Index, smText, AppOptions.CommonErrors - [etOCR], AppOptions.Conventions, [cmTexts, cmTimes]);
    UpdateCPSAndTexts(Index);
    frmMain.VST.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

function GetCorrectTime(const ATime: Integer; AIsFrames: Boolean): Integer;
begin
  if AIsFrames then
    Result := FramesToTime(ATime, Workspace.FPS.OutputFPS)
  else
    Result := ATime;
end;

// -----------------------------------------------------------------------------

function GetCorrectPause(const AIndex: Integer): Integer;
//var
//  it, ft: Integer;
begin
  if Workspace.WorkMode = wmTime then
    Result := Subtitles.Pause[AIndex]
  else
  begin
    Result := FramesToTime(Subtitles.PauseFrames[AIndex, Workspace.FPS.OutputFPS], Workspace.FPS.OutputFPS);
//    RoundFramesValue(Subtitles[AIndex+1].InitialTime, Subtitles[AIndex].FinalTime, Workspace.FPS.OutputFPS, it, ft);
//    Result := it-ft;
  end;
end;

// -----------------------------------------------------------------------------

function GetTimeStr(const Time: Integer; const Trim: Boolean = False): String;
begin
  if Workspace.WorkMode = wmTime then
    Result := TimeToString(Time, DefTimeFormat, Workspace.FPS.OutputFPS)
  else
    Result := TimeToString(Time, DefFramesFormat, Workspace.FPS.OutputFPS);

  if Trim then Result := TrimTimeString(Result);
end;

// -----------------------------------------------------------------------------

function GetInitialTimeStr(const Index: Integer; const Trim: Boolean = False): String;
var
  t: Integer;
begin
  if Subtitles.ValidIndex(Index) then
    t := Subtitles[Index].InitialTime
  else
    t := 0;

  Result := GetTimeStr(t, Trim)
end;

// -----------------------------------------------------------------------------

function GetFinalTimeStr(const Index: Integer; const Trim: Boolean = False): String;
var
  t: Integer;
begin
  if Subtitles.ValidIndex(Index) then
    t := Subtitles[Index].FinalTime
  else
    t := 0;

  Result := GetTimeStr(t, Trim);
end;

// -----------------------------------------------------------------------------

function GetDurationTimeStr(const Index: Integer; const Trim: Boolean = False): String;
var
  it, ft: Integer;
begin
  if Workspace.WorkMode = wmTime then
    Result := GetTimeStr(Subtitles.Duration[Index], Trim)
  else
  begin
    RoundFramesValue(Subtitles[Index].InitialTime, Subtitles[Index].FinalTime, Workspace.FPS.OutputFPS, it, ft);
    Result := GetTimeStr(ft-it, Trim)
  end;
end;

// -----------------------------------------------------------------------------

function GetPauseTimeStr(const Index: Integer; const Trim: Boolean = False): String;
begin
  Result := GetTimeStr(Subtitles.Pause[Index], Trim);
end;

// -----------------------------------------------------------------------------

function GetSubtitleText(const Index: Integer; const SubtitleMode: TSubtitleMode = smText): String;
begin
  if SubtitleMode = smText then
    Result := Subtitles[Index].Text
  else
    Result := Subtitles[Index].Translation;
end;

// -----------------------------------------------------------------------------

function GetMaxLinesOf(Text: String; const Separator: String = sLineBreak): Integer;
var
  l, PosEnter: Integer;
begin
  Result   := 0;
  Text     := RemoveTSTags(Text);
  PosEnter := Pos(sLineBreak, Text);
  if PosEnter > 0 then
  begin
    while PosEnter > 0 do
    begin
      l := UTF8Length(Copy(Text, 1, PosEnter-1));
      if Result < l then
        Result := l;

      Text     := Copy(Text, PosEnter + 2, UTF8Length(Text));
      PosEnter := Pos(sLineBreak, Text);
    end;
  end
  else
    Result := UTF8Length(Text);
end;

// -----------------------------------------------------------------------------

function GetLengthForEachLine(Text: String; const Separator: String = sLineBreak; const LastSeparator: String = sLineBreak): String;
var
  TotLen   : Integer;
  BreakLen : Integer;
  PosEnter : Integer;
begin
  Result   := '';
  Text     := RemoveTSTags(Text);
  BreakLen := Length(sLineBreak);
  TotLen   := UTF8Length(Text) - (StringCount(sLineBreak, Text) * BreakLen);
  PosEnter := Pos(sLineBreak, Text);
  if PosEnter > 0 then
  begin
    while PosEnter > 0 do
    begin
      Result   := Result + IntToStr(UTF8Length(Copy(Text, 1, PosEnter-1))) + Separator;
      Text     := Copy(Text, PosEnter + BreakLen, UTF8Length(Text));
      PosEnter := Pos(sLineBreak, Text);
    end;
    Result := Result + IntToStr(UTF8Length(Text));
    if LastSeparator <> '' then
      Result := Result + LastSeparator + IntToStr(TotLen);
  end
  else
    Result := IntToStr(UTF8Length(Text));
end;

// -----------------------------------------------------------------------------

function GetLengthForEachLineIntArray(Text: String; const Separator: String = sLineBreak; const LastSeparator: String = sLineBreak): TIntegerDynArray;
var
  sArray: TStringArray;
  i: Integer;
begin
  SetLength(Result, 0);
  sArray := GetLengthForEachLine(Text, Separator, LastSeparator).Split(sLineBreak, TStringSplitOptions.ExcludeEmpty);
  if Length(sArray) > 0 then
  begin
    SetLength(Result, Length(sArray));
    for i := 0 to High(sArray) do
      Result[i] := sArray[i].ToInteger;
  end;
end;

// -----------------------------------------------------------------------------

function GetTextLength(const Text: String): Integer;
begin
  Result := UTF8Length(RemoveTSTags(Text));
end;

// -----------------------------------------------------------------------------

procedure SelectSubtitleAndFocusMemo(const NextSibiling: Boolean; const WaveToo: Boolean = False);
begin
  with frmMain do
  begin
    if NextSibiling then
    begin
      if (VSTFocusedNode(VST) < Subtitles.Count) then VSTSelectNode(VST, VSTFocusedNode(VST)+1, True, True);
    end
    else
    begin
      if (VSTFocusedNode(VST) > 0) then VSTSelectNode(VST, VSTFocusedNode(VST)-1, True, True);
    end;

    if WaveToo then
      VSTDblClick(NIL)
    else
      FocusMemo;
  end;
end;

// -----------------------------------------------------------------------------

procedure GoToNextEntryAndPlay(const NextSibiling: Boolean = True);
var
  x: Integer;
begin
  with frmMain do
  begin
    SelectSubtitleAndFocusMemo(NextSibiling, True);
    x := VSTFocusedNode(VST);

    if x >= 0 then
    begin
      MPV.SetMediaPosInMs(Subtitles[x].InitialTime);
      MPVOptions.EndTime := Subtitles[x].FinalTime;

      if not MPV.IsPlaying then
        MPV.Resume(True);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure GoToCurrentEntryTime(const AInitialTime: Boolean = True);
var
  x, t: Integer;
begin
  with frmMain do
  begin
    x := VSTFocusedNode(VST);

    if x >= 0 then
    begin
      if AInitialTime then
        t := Subtitles[x].InitialTime
      else
        t := Subtitles[x].FinalTime;

      MPV.SetMediaPosInMs(t);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure PlayCurrentEntry;
var
  x: Integer;
begin
  with frmMain do
  begin
    x := VSTFocusedNode(VST);

    if x >= 0 then
    begin
      MPV.SetMediaPosInMs(Subtitles[x].InitialTime);
      MPVOptions.EndTime := Subtitles[x].FinalTime;

      if not MPV.IsPlaying then
        MPV.Resume(True);
    end;
  end;
end;

// -----------------------------------------------------------------------------

function GetSubtitleIndexAtTime(MSecs: Cardinal): Integer;
var
  i: Integer;
begin
  Result := -1;

  if Subtitles.Count > 0 then
  begin
    if not MPVOptions.SubtitleHandleByMPV and (Workspace.WorkMode = wmFrames) then
      MSecs += 1;

    for i := 0 to Subtitles.Count-1 do
      with Subtitles[i] do
      begin
        if (InitialTime <= MSecs) and (FinalTime > MSecs) then
        begin
          Result := i;
          SubtitleInfo.LastSubtitle.ShowIndex := Result;
          Break;
        end;
      end;
  end;
end;

// -----------------------------------------------------------------------------

function GetSubtitleTextAtTime(const MSecs: Cardinal): String;
var
  i: Integer;
  s: String;
begin
  Result := '';

  i := GetSubtitleIndexAtTime(MSecs);
  if i >= 0 then
    with Subtitles[i] do
    begin
      Result := Text;
      case MPVOptions.SubtitleToShow of
        TSubtitleMode.smText        : Result := Text;
        TSubtitleMode.smTranslation : Result := Translation;
      end;
      Result := ReplaceString(Result, sLineBreak, #10) + #10 + ' ';
      s := '';
      if Align <> shaNone then
      begin
        case Align of
          shaLeft : case VAlign of
                      svaCenter : s := '{\an4}';
                      svaTop    : s := '{\an7}';
                    else
                      s := '{\an1}';
                    end;
          shaCenter : case VAlign of
                        svaCenter : s := '{\an5}';
                        svaTop    : s := '{\an8}';
                      else
                        s := '{\an2}';
                      end;
          shaRight : case VAlign of
                       svaCenter : s := '{\an6}';
                       svaTop    : s := '{\an9}';
                     else
                       s := '{\an3}';
                     end;
        end;
      end
      else if VAlign <> svaBottom then
      begin
        case VAlign of
          svaCenter : s := '{\an5}';
          svaTop    : s := '{\an8}';
        else
          s := '{\an2}';
        end;
      end;

      if frmMain.actShowActorOnPreview.Checked and not Actor.IsEmpty then
        Result := '[' + Actor + '] ' + Result;

      if not s.IsEmpty then
        Result := s + Result + '{\an0}';
    end;
end;

// -----------------------------------------------------------------------------

procedure InsertMemoText(const AText: String; const AVSTLoop: Boolean = True);
var
  Memo : TUWMemo;
begin
  if not Assigned(Workspace.Transcription.Memo) then
  begin
    Memo := GetMemoFocused;
    if Memo <> NIL then
      Memo.SelText := AText
    else
      VSTDoLoop(frmMain.VST, @ApplyUnicodeChar, dlSelected, True, True);
  end
  else
  begin
    TranscriptionInsertText(AText);
  end;
end;

// -----------------------------------------------------------------------------

procedure SetTag(const Tag, TagFormat: String; const Len: Integer);
var
  Memo : TUWMemo;
  i, l : Integer;
begin
  Memo := GetMemoFocused;
  if Memo = NIL then Exit;

  i := Memo.SelStart;
  l := Memo.SelLength;
  if Memo.SelText <> '' then
    Memo.SelText := Format(TagFormat, [Tag, Memo.SelText, Tag])
  else
    Memo.Text := Format(TagFormat, [Tag, Memo.Text, Tag]);

  Memo.SelStart  := i+Len;
  Memo.SelLength := l;
end;

// -----------------------------------------------------------------------------

procedure SetTextTag(const Tag: String);
begin
  SetTag(Tag, '{\%s1}%s{\%s0}', 5);
end;

// -----------------------------------------------------------------------------

procedure SetTextCustomTag(const Tag: String);
begin
  SetTag(Tag, '%s%s%s', Tag.Length);
end;

// -----------------------------------------------------------------------------

procedure SetTextTagColor(const HexColor: String);
var
  Memo : TUWMemo;
  i    : Integer;
begin
  Memo := GetMemoFocused;
  if Memo = NIL then Exit;

  i := Memo.SelStart;
  if Memo.SelText <> '' then
    Memo.SelText := Format('{\%s&%s&}%s{\%s}', [tst_Color, HexColor, Memo.SelText, tst_Color])
  else
    Memo.Text := Format('{\%s&%s&}%s{\%s}', [tst_Color, HexColor, Memo.Text, tst_Color]);

  Memo.SelStart := i+12;
end;

// -----------------------------------------------------------------------------

procedure SetAlignTo(const AAlign: TSubtitleHAlign);
var
  p: PUWSubtitleItem;
begin
  with frmMain do
    if GetMemoFocused = NIL then
      VSTDoLoop(VST, @ApplyAlign, dlSelected, True, True)
    else
    begin
      p := Subtitles.ItemPointer[VSTFocusedNode(VST)];
      if Assigned(p) then
      begin
        p^.Align := AAlign;
        SubtitleChanged(True, True);
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure SetVAlignTo(const AVAlign: TSubtitleVAlign);
var
  p: PUWSubtitleItem;
begin
  with frmMain do
    if GetMemoFocused = NIL then
      VSTDoLoop(VST, @ApplyVAlign, dlSelected, True, True)
    else
    begin
      p := Subtitles.ItemPointer[VSTFocusedNode(VST)];
      if Assigned(p) then
      begin
        p^.VAlign := AVAlign;
        SubtitleChanged(True, True);
      end;
    end;
end;

// -----------------------------------------------------------------------------

function GetSubtitleMarkedCount: Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to Subtitles.Count-1 do
    if Subtitles[i].Marked then
      Inc(Result);
end;

// -----------------------------------------------------------------------------

end.

